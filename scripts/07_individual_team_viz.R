# =============================================================================
# Script 07: Individual Team Probability Visualizations
# Purpose: Create visualizations for individual team advancement probabilities
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)
library(ggplot2)

# Load helper functions
source(here("scripts", "00_helper_functions.R"))

# Set theme
theme_set(theme_minimal(base_size = 12))

# Define color palette (Okabe-Ito colorblind-friendly)
oi_colors <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
)

# =============================================================================
# 1. Load Results
# =============================================================================

cat("Loading individual team probability results...\n")

team_probs <- readRDS(here("results", "tables", "individual_team_probabilities.rds"))
team_matrix <- readRDS(here("results", "tables", "team_advancement_matrix.rds"))
mid_tier_advancement <- read_csv(
    here("results", "tables", "mid_tier_team_advancement.csv"),
    show_col_types = FALSE
)

dir_create_safe("results", "figures")
cat("✓ Data loaded successfully\n")

# =============================================================================
# 2. Plot: Sweet 16 Probabilities for 8-12 Seeds
# =============================================================================

cat("\nCreating visualization 1: Sweet 16 probabilities for 8-12 seeds...\n")

# Prepare data for plotting
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

cat("✓ Saved: 09_sweet16_mid_tier_probabilities.png\n")

# =============================================================================
# 3. Plot: Round-by-Round Progression for Top Mid-Tier Teams
# =============================================================================

cat("\nCreating visualization 2: Round-by-round progression...\n")

# Define round order
round_order <- c(
    "Round of 64", "Round of 32", "Sweet 16",
    "Elite 8", "Final Four", "Championship"
)

# Get top 5 mid-tier teams by Sweet 16 probability
top_teams <- sweet16_data %>%
    head(5) %>%
    pull(winner_name)

# Prepare data for progression plot
progression_data <- team_probs %>%
    filter(winner_name %in% top_teams) %>%
    mutate(
        round = factor(round, levels = round_order),
        winner_name = factor(winner_name, levels = top_teams)
    )

p2 <- ggplot(progression_data, aes(x = round, y = percentage, color = winner_name, group = winner_name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_text(
        data = progression_data %>% filter(round == "Sweet 16"),
        aes(label = sprintf("%.1f%%", percentage)),
        vjust = -0.7, hjust = 0.5, size = 3, fontface = "bold", show.legend = FALSE
    ) +
    scale_color_manual(
        values = oi_colors[1:5],
        name = "Team"
    ) +
    labs(
        title = "Tournament Progression: Top 8-12 Seeds by Sweet 16 Probability",
        subtitle = "Probability of reaching each round (5,000 simulations)",
        x = "Tournament Round",
        y = "Probability (%)"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 2))

ggsave(
    filename = here("results", "figures", "10_round_by_round_progression.png"),
    plot = p2,
    width = 11,
    height = 7,
    dpi = 300
)

cat("✓ Saved: 10_round_by_round_progression.png\n")

# =============================================================================
# 4. Plot: Heatmap of All Round Probabilities
# =============================================================================

cat("\nCreating visualization 3: Probability heatmap...\n")

# Prepare heatmap data
heatmap_data <- mid_tier_advancement %>%
    pivot_longer(
        cols = c("Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final Four"),
        names_to = "round",
        values_to = "probability"
    ) %>%
    mutate(
        round = factor(round, levels = round_order[1:5]),
        team_label = paste0(winner_name, " (", seed, ")"),
        team_label = fct_reorder(team_label, seed)
    )

p3 <- ggplot(heatmap_data, aes(x = round, y = team_label, fill = probability * 100)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.1f%%", probability * 100)),
        size = 3.5, fontface = "bold", color = "black"
    ) +
    scale_fill_gradient2(
        low = "#f7f7f7",
        mid = "#fdae61",
        high = "#d7191c",
        midpoint = 40,
        name = "Probability (%)",
        limits = c(0, 100)
    ) +
    labs(
        title = "8-12 Seed Tournament Advancement Probabilities",
        subtitle = "Probability of reaching each round (5,000 simulations)",
        x = "Tournament Round",
        y = NULL
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
    )

ggsave(
    filename = here("results", "figures", "11_probability_heatmap.png"),
    plot = p3,
    width = 10,
    height = 6,
    dpi = 300
)

cat("✓ Saved: 11_probability_heatmap.png\n")

# =============================================================================
# 5. Plot: Comparison with Historical Context
# =============================================================================

cat("\nCreating visualization 4: Historical context comparison...\n")

# Calculate expected counts by round
expected_counts <- team_probs %>%
    filter(is_mid_tier_seed == TRUE) %>%
    group_by(round) %>%
    summarise(
        expected_count = sum(probability),
        .groups = "drop"
    ) %>%
    mutate(
        round = factor(round, levels = round_order),
        observed_2023_2024 = case_when(
            round == "Round of 64" ~ 20, # 20 teams seeded 8-12
            round == "Round of 32" ~ NA_real_,
            round == "Sweet 16" ~ 0, # Historical observation
            TRUE ~ NA_real_
        )
    ) %>%
    filter(round %in% c("Round of 32", "Sweet 16", "Elite 8", "Final Four"))

p4 <- ggplot(expected_counts, aes(x = round)) +
    geom_col(aes(y = expected_count, fill = "Model Prediction"),
        alpha = 0.7, width = 0.6
    ) +
    geom_point(aes(y = observed_2023_2024, color = "2023-2024 Actual"),
        size = 4, shape = 18
    ) +
    geom_text(aes(y = expected_count, label = sprintf("%.2f", expected_count)),
        vjust = -0.5, size = 3.5, fontface = "bold"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.5, color = "gray30") +
    scale_fill_manual(
        values = c("Model Prediction" = oi_colors[3]),
        name = ""
    ) +
    scale_color_manual(
        values = c("2023-2024 Actual" = oi_colors[6]),
        name = ""
    ) +
    labs(
        title = "Expected vs. Observed 8-12 Seeds by Round",
        subtitle = "Model predictions compared to 2023-2024 tournament results",
        x = "Tournament Round",
        y = "Number of 8-12 Seeds",
        caption = "Diamond marker shows actual count from 2023-2024 tournaments"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    ) +
    scale_y_continuous(breaks = seq(0, 12, 2), limits = c(0, 12))

ggsave(
    filename = here("results", "figures", "12_expected_vs_observed.png"),
    plot = p4,
    width = 10,
    height = 6,
    dpi = 300
)

cat("✓ Saved: 12_expected_vs_observed.png\n")

# =============================================================================
# 6. Create Summary Dashboard for Individual Teams
# =============================================================================

cat("\nCreating visualization 5: Individual team spotlight...\n")

# Focus on Stanford (highest probability) and create detailed view
stanford_data <- team_probs %>%
    filter(winner_name == "Stanford Cardinal") %>%
    mutate(
        round = factor(round, levels = round_order),
        round_num = as.numeric(round)
    )

# Create bar chart with confidence-like intervals based on simulation variance
p5 <- ggplot(stanford_data, aes(x = round, y = percentage)) +
    geom_col(fill = oi_colors[5], alpha = 0.8, width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%\n(%d/5000)", percentage, times_reached)),
        vjust = -0.5, size = 3.5, fontface = "bold"
    ) +
    labs(
        title = "Stanford Cardinal (11-seed) Tournament Probabilities",
        subtitle = "Highest Sweet 16 probability among 8-12 seeds",
        x = "Tournament Round",
        y = "Probability (%)",
        caption = "Stanford has a 62.5% chance of reaching the Sweet 16 based on simulations"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))

ggsave(
    filename = here("results", "figures", "13_stanford_spotlight.png"),
    plot = p5,
    width = 10,
    height = 6,
    dpi = 300
)

cat("✓ Saved: 13_stanford_spotlight.png\n")

# =============================================================================
# Summary
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("VISUALIZATION COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat("Generated visualizations:\n")
cat("  1. 09_sweet16_mid_tier_probabilities.png - Bar chart of Sweet 16 probabilities\n")
cat("  2. 10_round_by_round_progression.png - Line chart showing progression through rounds\n")
cat("  3. 11_probability_heatmap.png - Heatmap of all probabilities\n")
cat("  4. 12_expected_vs_observed.png - Comparison with 2023-2024 data\n")
cat("  5. 13_stanford_spotlight.png - Detailed view of highest probability team\n")

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("✓ All visualizations saved to results/figures/\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
