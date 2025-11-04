# Individual Team Probability Visualizations

library(tidyverse)
library(here)
library(ggplot2)

source(here("scripts", "00_helper_functions.R"))

theme_set(theme_minimal(base_size = 12))

oi_colors <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
)

# Load Results

team_probs <- readRDS(here("results", "tables", "individual_team_probabilities.rds"))
team_matrix <- readRDS(here("results", "tables", "team_advancement_matrix.rds"))
mid_tier_advancement <- read_csv(
    here("results", "tables", "mid_tier_team_advancement.csv"),
    show_col_types = FALSE
)

dir_create_safe("results", "figures")


# Sweet 16 Probabilities for 8-12 Seeds

sweet16_data <- mid_tier_advancement %>%
    arrange(desc(`Sweet 16`)) %>%
    mutate(
        team_label = paste0(winner_name, "\n(", seed, "-seed, ", region, ")"),
        team_label = fct_reorder(team_label, `Sweet 16`)
    )

p1 <- ggplot(sweet16_data, aes(x = team_label, y = `Sweet 16` * 100, fill = factor(seed))) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", `Sweet 16` * 100)),
        hjust = -0.1, size = 3.5, fontface = "bold"
    ) +
    scale_fill_manual(
        values = c(
            "8" = oi_colors[1],
            "9" = oi_colors[2],
            "10" = oi_colors[3],
            "11" = oi_colors[5],
            "12" = oi_colors[6]
        ),
        name = "Seed"
    ) +
    coord_flip() +
    labs(
        title = "Probability of Reaching Sweet 16: 8-12 Seeds",
        subtitle = "Based on 5,000 tournament simulations",
        x = NULL,
        y = "Probability (%)",
        caption = "Note: In 2023 and 2024, zero 8-12 seeds reached the Sweet 16"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(limits = c(0, 75), breaks = seq(0, 70, 10))

ggsave(
    filename = here("results", "figures", "09_sweet16_mid_tier_probabilities.png"),
    plot = p1,
    width = 10,
    height = 6,
    dpi = 300
)

# Upset Potential Analysis - Show strength variability within seed groups

round_order <- c(
    "Round of 64", "Round of 32", "Sweet 16",
    "Elite 8", "Final Four", "Championship"
)

# Load team abilities to show within-seed variance (if not already loaded)
if (!exists("team_abilities")) {
    team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))
}

# Create upset potential visualization
upset_data <- team_abilities %>%
    filter(seed %in% 8:12) %>%
    group_by(seed) %>%
    summarise(
        mean_lambda = mean(lambda, na.rm = TRUE),
        sd_lambda = sd(lambda, na.rm = TRUE),
        min_lambda = min(lambda, na.rm = TRUE),
        max_lambda = max(lambda, na.rm = TRUE),
        n_teams = n(),
        .groups = "drop"
    ) %>%
    mutate(
        cv = sd_lambda / abs(mean_lambda),
        range_lambda = max_lambda - min_lambda
    )

p10 <- upset_data %>%
    ggplot(aes(x = factor(seed))) +
    geom_errorbar(
        aes(ymin = min_lambda, ymax = max_lambda),
        width = 0.4, linewidth = 1.2, color = oi_colors[6], alpha = 0.7
    ) +
    geom_point(aes(y = mean_lambda), size = 5, color = oi_colors[3], alpha = 0.9) +
    geom_errorbar(
        aes(ymin = mean_lambda - sd_lambda, ymax = mean_lambda + sd_lambda),
        width = 0.2, linewidth = 1.5, color = oi_colors[5], alpha = 0.8
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", alpha = 0.7) +
    labs(
        title = "Team Strength Variability Within 8-12 Seeds",
        subtitle = "Greater variability = higher upset potential",
        x = "Seed",
        y = "Team Strength (λ, z-score)",
        caption = "Large dot = mean strength • Thick bars = ±1 SD • Thin bars = full range (min-max)\nHigh variability within a seed indicates some teams are much stronger/weaker than others."
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.3)
    ) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5))

ggsave(
    filename = here("results", "figures", "10_upset_potential.png"),
    plot = p10,
    width = 10,
    height = 7,
    dpi = 400
)

# Key Matchup Analysis - Win probabilities for critical first round matchups

matchup_probs <- readRDS(here("data", "processed", "matchup_probabilities.rds"))

key_matchups <- matchup_probs %>%
    filter(higher_seed %in% 8:12 | lower_seed %in% 8:12) %>%
    mutate(
        mid_tier_seed = case_when(
            higher_seed %in% 8:12 ~ higher_seed,
            lower_seed %in% 8:12 ~ lower_seed,
            TRUE ~ NA_real_
        ),
        mid_tier_prob = case_when(
            higher_seed %in% 8:12 ~ avg_prob_higher_wins,
            lower_seed %in% 8:12 ~ avg_prob_lower_wins,
            TRUE ~ NA_real_
        ),
        is_favorable = mid_tier_prob > 0.5
    ) %>%
    filter(!is.na(mid_tier_seed))

p11 <- key_matchups %>%
    mutate(matchup = fct_reorder(matchup, mid_tier_prob)) %>%
    ggplot(aes(x = mid_tier_prob, y = matchup, fill = is_favorable)) +
    geom_col(alpha = 0.8) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray30", linewidth = 0.8) +
    geom_text(
        aes(label = scales::percent(mid_tier_prob, accuracy = 0.1)),
        hjust = ifelse(key_matchups$mid_tier_prob > 0.5, -0.1, 1.1),
        size = 3.5,
        fontface = "bold"
    ) +
    scale_fill_manual(
        values = c("TRUE" = oi_colors[3], "FALSE" = oi_colors[6]),
        labels = c("Underdog", "Favorite"),
        name = "8-12 Seed is:"
    ) +
    scale_x_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    labs(
        title = "Win Probability for 8-12 Seeds in First Round Matchups",
        subtitle = "Neutral-court probabilities based on Bradley-Terry model",
        x = "Win Probability (for 8-12 seed)",
        y = "Matchup",
        caption = "Shows typical NCAA tournament first round matchups. Dashed line = 50-50 matchup."
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.2),
        legend.position = "top"
    )

ggsave(
    filename = here("results", "figures", "11_key_matchups.png"),
    plot = p11,
    width = 10,
    height = 8,
    dpi = 400
)
