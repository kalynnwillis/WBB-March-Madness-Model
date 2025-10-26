# =============================================================================
# Script 05: Visualization
# Purpose: Create visualizations for Bradley-Terry model results and simulations
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)
library(ggplot2)
library(patchwork) # For combining plots

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

cat("Loading results for visualization...\n")

team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))
mid_tier_summary <- readRDS(here("results", "tables", "simulation_summary.rds"))
seed_performance <- readRDS(here("results", "tables", "seed_performance_sim.rds"))
first_round_summary <- readRDS(here("results", "tables", "first_round_summary.rds"))
second_round_summary <- readRDS(here("results", "tables", "second_round_summary.rds"))
deeper_summary <- readRDS(here("results", "tables", "deeper_runs_summary.rds"))
conditional_summary <- readRDS(here("results", "tables", "conditional_summary.rds"))
mid_tier_counts <- read_csv(here("results", "tables", "mid_tier_counts_by_sim.csv"),
    show_col_types = FALSE
)

cat("✓ Data loaded successfully\n")

# =============================================================================
# 2. Plot 1: Team Strength Distribution by Seed Category
# =============================================================================

cat("\nCreating Plot 1: Team strength distribution...\n")

p1 <- team_abilities %>%
    filter(!is.na(seed)) %>%
    mutate(
        seed_category = factor(
            seed_category,
            levels = c("1-4 seeds", "5-7 seeds", "8-12 seeds", "13-16 seeds")
        )
    ) %>%
    ggplot(aes(x = seed_category, y = lambda, fill = seed_category)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
    scale_fill_manual(values = oi_colors[c(1, 2, 3, 6)]) +
    labs(
        title = "Team Strength Distribution by Seed Category",
        subtitle = "Bradley-Terry Model Estimated Abilities (λ)",
        x = "Seed Category",
        y = "Estimated Team Strength (λ)",
        caption = "Higher λ indicates stronger team"
    ) +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold")
    )

ggsave(
    filename = here("results", "figures", "01_team_strength_by_seed.png"),
    plot = p1,
    width = 10,
    height = 6,
    dpi = 300
)

cat("✓ Saved: 01_team_strength_by_seed.png\n")

# =============================================================================
# 3. Plot 2: All Team Strengths with Confidence Intervals
# =============================================================================

cat("\nCreating Plot 2: All team strengths with confidence intervals...\n")

# Select teams for visualization (tournament teams only)
tournament_teams <- team_abilities %>%
    filter(!is.na(seed)) %>%
    arrange(lambda) %>%
    mutate(
        rank = row_number(),
        is_mid_tier = seed >= 8 & seed <= 12
    )

p2 <- tournament_teams %>%
    ggplot(aes(x = rank, y = lambda, color = is_mid_tier)) +
    geom_errorbar(
        aes(ymin = lambda - 2 * se, ymax = lambda + 2 * se),
        alpha = 0.5,
        width = 0
    ) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_color_manual(
        values = c("FALSE" = oi_colors[8], "TRUE" = oi_colors[6]),
        labels = c("Other seeds", "8-12 seeds")
    ) +
    labs(
        title = "Tournament Team Strengths with 95% Confidence Intervals",
        subtitle = "Teams ranked by estimated strength (λ)",
        x = "Rank (by team strength)",
        y = "Estimated Team Strength (λ)",
        color = "Seed Group",
        caption = "Error bars show ± 2 standard errors"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

ggsave(
    filename = here("results", "figures", "02_all_team_strengths.png"),
    plot = p2,
    width = 12,
    height = 7,
    dpi = 300
)

cat("✓ Saved: 02_all_team_strengths.png\n")

# =============================================================================
# 4. Plot 3: First Round Win Probabilities by Seed
# =============================================================================

cat("\nCreating Plot 3: First round win probabilities...\n")

p3 <- first_round_summary %>%
    ggplot(aes(x = factor(seed), y = avg_prob_win)) +
    geom_col(fill = oi_colors[3], alpha = 0.8) +
    geom_errorbar(
        aes(ymin = min_prob_win, ymax = max_prob_win),
        width = 0.3,
        color = oi_colors[5]
    ) +
    geom_text(
        aes(label = sprintf("%.1f%%", avg_prob_win * 100)),
        vjust = -0.5,
        size = 4,
        fontface = "bold"
    ) +
    scale_y_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1),
        expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
        title = "First Round Win Probability by Seed (8-12)",
        subtitle = "Probability of advancing to Round of 32",
        x = "Seed",
        y = "Win Probability",
        caption = "Error bars show range across teams"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold")
    )

ggsave(
    filename = here("results", "figures", "03_first_round_probabilities.png"),
    plot = p3,
    width = 10,
    height = 6,
    dpi = 300
)

cat("✓ Saved: 03_first_round_probabilities.png\n")

# =============================================================================
# 5. Plot 4: Expected Advancement by Round
# =============================================================================

cat("\nCreating Plot 4: Expected advancement by round...\n")

# Prepare data for plotting
advancement_data <- tibble(
    seed = rep(8:12, each = 4),
    round = rep(c("Round of 32", "Sweet 16", "Elite 8", "Final Four"), times = 5)
) %>%
    left_join(
        first_round_summary %>% select(seed, r32 = expected_wins),
        by = "seed"
    ) %>%
    left_join(
        second_round_summary %>% select(seed, sweet16 = expected_in_sweet16),
        by = "seed"
    ) %>%
    left_join(
        deeper_summary %>% select(seed,
            elite8 = expected_elite8,
            final4 = expected_final4
        ),
        by = "seed"
    ) %>%
    mutate(
        expected = case_when(
            round == "Round of 32" ~ r32,
            round == "Sweet 16" ~ sweet16,
            round == "Elite 8" ~ elite8,
            round == "Final Four" ~ final4
        ),
        round = factor(round, levels = c(
            "Round of 32", "Sweet 16",
            "Elite 8", "Final Four"
        ))
    )

p4 <- advancement_data %>%
    ggplot(aes(x = round, y = expected, color = factor(seed), group = seed)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(
        values = oi_colors[1:5],
        name = "Seed"
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
        title = "Expected Number of Teams Advancing by Round",
        subtitle = "8-12 seeds progression through tournament (analytical model)",
        x = "Tournament Round",
        y = "Expected Number of Teams",
        caption = "Based on Bradley-Terry model probabilities"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 1),
        legend.position = "right"
    )

ggsave(
    filename = here("results", "figures", "04_expected_advancement.png"),
    plot = p4,
    width = 12,
    height = 7,
    dpi = 300
)

cat("✓ Saved: 04_expected_advancement.png\n")

# =============================================================================
# 6. Plot 5: Simulation Results Distribution
# =============================================================================

cat("\nCreating Plot 5: Simulation results distribution...\n")

# Define round order
round_order <- c(
    "Round of 64", "Round of 32", "Sweet 16",
    "Elite 8", "Final Four", "Championship"
)

p5 <- mid_tier_counts %>%
    mutate(round = factor(round, levels = round_order)) %>%
    filter(round != "Round of 64") %>% # Exclude first round
    ggplot(aes(x = n_mid_tier, fill = round)) +
    geom_histogram(bins = 15, alpha = 0.7, color = "white") +
    facet_wrap(~round, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = rep(oi_colors[1:5], length.out = 6)) +
    labs(
        title = "Distribution of 8-12 Seeds by Round (5000 Simulations)",
        subtitle = "How many 8-12 seeds advance in each simulation?",
        x = "Number of 8-12 Seeds",
        y = "Frequency (# of simulations)",
        caption = "Based on Monte Carlo tournament simulation"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold")
    )

ggsave(
    filename = here("results", "figures", "05_simulation_distributions.png"),
    plot = p5,
    width = 12,
    height = 8,
    dpi = 300
)

cat("✓ Saved: 05_simulation_distributions.png\n")

# =============================================================================
# 7. Plot 6: Expected vs Simulated Results Comparison
# =============================================================================

cat("\nCreating Plot 6: Comparing analytical and simulation results...\n")

# Prepare comparison data
comparison_data <- tibble(
    round = c("Round of 32", "Sweet 16", "Elite 8", "Final Four"),
    analytical = c(
        sum(first_round_summary$expected_wins),
        sum(second_round_summary$expected_in_sweet16),
        sum(deeper_summary$expected_elite8),
        sum(deeper_summary$expected_final4)
    )
) %>%
    left_join(
        mid_tier_summary %>%
            filter(round %in% c("Round of 32", "Sweet 16", "Elite 8", "Final Four")) %>%
            select(round, simulated = mean_count, sim_sd = sd_count),
        by = "round"
    ) %>%
    pivot_longer(
        cols = c(analytical, simulated),
        names_to = "method",
        values_to = "expected_count"
    ) %>%
    mutate(
        round = factor(round, levels = c(
            "Round of 32", "Sweet 16",
            "Elite 8", "Final Four"
        )),
        method = factor(method,
            levels = c("analytical", "simulated"),
            labels = c("Analytical Model", "Monte Carlo Simulation")
        )
    )

p6 <- comparison_data %>%
    ggplot(aes(x = round, y = expected_count, fill = method)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_text(
        aes(label = sprintf("%.2f", expected_count)),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        size = 3.5
    ) +
    scale_fill_manual(
        values = c(
            "Analytical Model" = oi_colors[2],
            "Monte Carlo Simulation" = oi_colors[6]
        )
    ) +
    scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
        title = "Expected 8-12 Seeds Advancing: Model vs Simulation",
        subtitle = "Comparison of analytical predictions and Monte Carlo results",
        x = "Tournament Round",
        y = "Expected Number of 8-12 Seeds",
        fill = "Method"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 15, hjust = 1),
        legend.position = "bottom"
    )

ggsave(
    filename = here("results", "figures", "06_analytical_vs_simulation.png"),
    plot = p6,
    width = 12,
    height = 7,
    dpi = 300
)

cat("✓ Saved: 06_analytical_vs_simulation.png\n")

# =============================================================================
# 8. Plot 7: Conditional Probabilities
# =============================================================================

cat("\nCreating Plot 7: Conditional probabilities...\n")

conditional_long <- conditional_summary %>%
    select(seed, starts_with("avg_prob")) %>%
    pivot_longer(
        cols = starts_with("avg_prob"),
        names_to = "target_round",
        values_to = "probability"
    ) %>%
    mutate(
        target_round = str_remove(target_round, "avg_prob_") %>%
            str_remove("_given_s16") %>%
            str_to_title() %>%
            str_replace("elite8", "Elite 8") %>%
            str_replace("final4", "Final Four") %>%
            str_replace("finals", "Championship") %>%
            str_replace("champion", "Win Title"),
        target_round = factor(
            target_round,
            levels = c("Elite8", "Final4", "Finals", "Champion")
        )
    )

p7 <- conditional_long %>%
    ggplot(aes(x = factor(seed), y = probability, fill = factor(seed))) +
    geom_col(alpha = 0.8) +
    geom_text(
        aes(label = sprintf("%.1f%%", probability * 100)),
        vjust = -0.5,
        size = 3.5
    ) +
    facet_wrap(~target_round, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = oi_colors[1:5]) +
    scale_y_continuous(
        labels = scales::percent_format(),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
        title = "Conditional Advancement Probabilities for 8-12 Seeds",
        subtitle = "Given a team reaches the Sweet 16, probability of further advancement",
        x = "Seed",
        y = "Probability",
        caption = "Conditional on reaching Sweet 16"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold")
    )

ggsave(
    filename = here("results", "figures", "07_conditional_probabilities.png"),
    plot = p7,
    width = 12,
    height = 8,
    dpi = 300
)

cat("✓ Saved: 07_conditional_probabilities.png\n")

# =============================================================================
# 9. Plot 8: Seed Performance Heatmap
# =============================================================================

cat("\nCreating Plot 8: Seed performance heatmap...\n")

# Calculate percentage for each seed-round combination
heatmap_data <- seed_performance %>%
    mutate(
        pct_reached = pct_of_sims / 100,
        round = factor(round, levels = round_order)
    ) %>%
    filter(round != "Round of 64")

p8 <- heatmap_data %>%
    ggplot(aes(x = round, y = factor(winner_seed), fill = pct_reached)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(
        aes(label = sprintf("%.1f%%", pct_of_sims)),
        color = "white",
        fontface = "bold",
        size = 4
    ) +
    scale_fill_gradient(
        low = oi_colors[2],
        high = oi_colors[6],
        labels = scales::percent_format(),
        name = "% of\nSimulations"
    ) +
    labs(
        title = "Tournament Performance Heatmap: 8-12 Seeds",
        subtitle = "Percentage of simulations where each seed reached each round",
        x = "Tournament Round",
        y = "Seed"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1)
    )

ggsave(
    filename = here("results", "figures", "08_seed_performance_heatmap.png"),
    plot = p8,
    width = 12,
    height = 7,
    dpi = 300
)

cat("✓ Saved: 08_seed_performance_heatmap.png\n")

# =============================================================================
# 10. Create Summary Dashboard
# =============================================================================

cat("\nCreating combined summary dashboard...\n")

# Create simplified versions for dashboard
dash_p1 <- p3 +
    theme(
        text = element_text(size = 10),
        plot.title = element_text(size = 12)
    )

dash_p2 <- comparison_data %>%
    filter(method == "Monte Carlo Simulation") %>%
    ggplot(aes(x = round, y = expected_count, fill = round)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f", expected_count)),
        vjust = -0.5, size = 3
    ) +
    scale_fill_manual(values = oi_colors[1:4]) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
    labs(
        title = "Expected 8-12 Seeds by Round",
        x = "Round", y = "Expected Count"
    ) +
    theme(
        text = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1)
    )

dash_p3 <- heatmap_data %>%
    ggplot(aes(x = round, y = factor(winner_seed), fill = pct_reached)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f%%", pct_of_sims)),
        color = "white", fontface = "bold", size = 3
    ) +
    scale_fill_gradient(
        low = oi_colors[2], high = oi_colors[6],
        labels = scales::percent_format(), name = "% Sims"
    ) +
    labs(title = "Performance by Seed", x = "Round", y = "Seed") +
    theme(
        text = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.key.size = unit(0.4, "cm")
    )

dash_p4 <- team_abilities %>%
    filter(!is.na(seed)) %>%
    mutate(is_mid = seed >= 8 & seed <= 12) %>%
    ggplot(aes(x = seed_category, y = lambda, fill = is_mid)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(
        values = c("FALSE" = oi_colors[8], "TRUE" = oi_colors[6]),
        guide = "none"
    ) +
    labs(
        title = "Team Strength Distribution",
        x = "Seed Category", y = "λ"
    ) +
    theme(
        text = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1)
    )

# Combine into dashboard
dashboard <- (dash_p4 | dash_p1) / (dash_p2 | dash_p3) +
    plot_annotation(
        title = "Women's Basketball March Madness: 8-12 Seed Analysis",
        subtitle = "Bradley-Terry Model and Monte Carlo Simulation Results",
        theme = theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12)
        )
    )

ggsave(
    filename = here("results", "figures", "00_summary_dashboard.png"),
    plot = dashboard,
    width = 16,
    height = 10,
    dpi = 300
)

cat("✓ Saved: 00_summary_dashboard.png\n")

# =============================================================================
# 11. Generate Visualization Summary
# =============================================================================

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("VISUALIZATION SUMMARY\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n\n")

cat("Generated the following visualizations:\n\n")
cat("  00_summary_dashboard.png          - Combined 4-panel summary\n")
cat("  01_team_strength_by_seed.png      - Strength distributions\n")
cat("  02_all_team_strengths.png         - All teams with CIs\n")
cat("  03_first_round_probabilities.png  - Round 1 win probabilities\n")
cat("  04_expected_advancement.png       - Analytical progression\n")
cat("  05_simulation_distributions.png   - Simulation histograms\n")
cat("  06_analytical_vs_simulation.png   - Method comparison\n")
cat("  07_conditional_probabilities.png  - Conditional advancement\n")
cat("  08_seed_performance_heatmap.png   - Seed-round heatmap\n")

cat("\nAll figures saved to: results/figures/\n")

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("✓ Visualization complete!\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n")

# =============================================================================
# NOTES:
# - All plots use colorblind-friendly Okabe-Ito palette
# - High resolution (300 DPI) suitable for presentations/papers
# - Dashboard provides quick overview of key findings
# - Individual plots allow detailed examination of specific aspects
# =============================================================================
