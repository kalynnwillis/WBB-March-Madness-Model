# =============================================================================
# Script 05: Visualization
# Purpose: Create visualizations for Bradley-Terry model results and simulations
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)
library(ggplot2)
library(patchwork) # For combining plots

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

cat("Loading results for visualization...\n")

team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))
mid_tier_summary <- readRDS(here("results", "tables", "simulation_summary.rds"))
seed_performance <- readRDS(here("results", "tables", "seed_performance_sim.rds"))

first_round_summary <- read_csv(here("results", "tables", "first_round_summary.csv"), show_col_types = FALSE)
second_round_summary <- read_csv(here("results", "tables", "second_round_summary.csv"), show_col_types = FALSE)
deeper_summary <- read_csv(here("results", "tables", "deeper_runs_summary.csv"), show_col_types = FALSE)
conditional_summary <- read_csv(here("results", "tables", "conditional_summary.csv"), show_col_types = FALSE)
mid_tier_counts <- read_csv(here("results", "tables", "mid_tier_counts_by_sim.csv"), show_col_types = FALSE)

dir_create_safe("results", "figures")
cat("✓ Data loaded successfully\n")


# =============================================================================
# 2. Plot 1: Team Strength Distribution by Seed Category
# =============================================================================

cat("\nCreating Plot 1: Team strength distribution...\n")

# FIX 3: Add sample sizes and clip extreme outlier for better visual
annot <- team_abilities %>%
    filter(!is.na(seed_category)) %>%
    count(seed_category) %>%
    mutate(seed_category = factor(
        seed_category,
        levels = c("1-4 seeds", "5-7 seeds", "8-12 seeds", "13-16 seeds")
    ))

p1 <- team_abilities %>%
    filter(!is.na(seed)) %>%
    # Tame extreme values for cleaner visualization (cap SEs and lambdas)
    mutate(
        lambda = pmax(pmin(lambda, 8), -8), # Clamp to [-8, 8]
        se = pmin(se, 2), # Cap huge SEs
        seed_category = factor(
            seed_category,
            levels = c("1-4 seeds", "5-7 seeds", "8-12 seeds", "13-16 seeds")
        )
    ) %>%
    ggplot(aes(x = seed_category, y = lambda, fill = seed_category)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
    geom_text(
        data = annot,
        aes(x = seed_category, y = -Inf, label = paste0("n=", n)),
        vjust = -0.6, size = 3.5, inherit.aes = FALSE
    ) +
    coord_cartesian(ylim = c(-2, 8)) +
    scale_fill_manual(values = oi_colors[c(1, 2, 3, 6)]) +
    labs(
        title = "Team Strength Distribution by Seed Category",
        subtitle = "Bradley-Terry Model Estimated Abilities (λ)",
        x = "Seed Category",
        y = "Estimated Team Strength (λ)",
        caption = "Display clipped to [-2, 8] for clarity; extreme outliers excluded from view. Higher λ indicates stronger team."
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
# Filter out teams with huge SEs (indicates poor information/quasi-separation)
se_threshold <- quantile(team_abilities$se[!is.na(team_abilities$seed)], 0.95, na.rm = TRUE)

tournament_teams <- team_abilities %>%
    filter(!is.na(seed)) %>%
    filter(se <= se_threshold) %>% # Drop worst 5% SEs
    # Cap extreme values for cleaner visualization
    mutate(
        lambda = pmax(pmin(lambda, 6), -6), # Tighter clamp for viz
        se = pmin(se, 2)
    ) %>%
    arrange(lambda) %>%
    mutate(
        rank = row_number(),
        is_mid_tier = seed >= 8 & seed <= 12
    )

cat(sprintf(
    "Filtered to %d teams for visualization (excluded high-SE outliers)\n",
    nrow(tournament_teams)
))

# FIX 4: Add vertical guides between quartiles for easier reading
p2 <- tournament_teams %>%
    ggplot(aes(x = rank, y = lambda, color = is_mid_tier)) +
    geom_vline(
        xintercept = c(16, 32, 48),
        linetype = "dotted", color = "gray70", linewidth = 0.5
    ) +
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
        caption = "Error bars: ±2 SE; top 5% SE excluded; λ capped to ±6 for viz. Seeds are model-assigned from regular-season results."
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

# Plot 3: First Round Win Probabilities by Seed (use the summary directly)
cat("\nCreating Plot 3: First round win probabilities...\n")

# FIX 2: Use uniform ±10% error bars for clearer narrative
p3 <- first_round_summary %>%
    # Drop NAs and clip probabilities to [0, 1]
    drop_na(avg_prob_win) %>%
    mutate(
        avg_prob_win = pmin(pmax(avg_prob_win, 0), 1)
    ) %>%
    ggplot(aes(x = factor(seed), y = avg_prob_win)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60", linewidth = 0.5) +
    geom_col(fill = oi_colors[3], alpha = 0.85) +
    geom_errorbar(
        aes(ymin = pmax(avg_prob_win - 0.10, 0), ymax = pmin(avg_prob_win + 0.10, 1)),
        width = 0.3, color = oi_colors[5], linewidth = 0.8
    ) +
    geom_text(
        aes(label = scales::percent(avg_prob_win, accuracy = 0.1)),
        vjust = -0.5, size = 4, fontface = "bold"
    ) +
    scale_y_continuous(
        labels = scales::percent_format(),
        limits = c(0, 1),
        expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
        title = "First Round Win Probability by Seed (8–12)",
        subtitle = "Probability of advancing to Round of 32",
        x = "Seed", y = "Win Probability",
        caption = "Error bars show ±10% envelope. Seeds are model-assigned from regular-season results."
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold")
    )

ggsave(here("results", "figures", "03_first_round_probabilities.png"), p3, width = 10, height = 6, dpi = 300)
cat("✓ Saved: 03_first_round_probabilities.png\n")


# =============================================================================
# 5. Plot 4: Expected Advancement by Round
# =============================================================================

cat("\nCreating Plot 4: Expected advancement by round...\n")

# deeper_summary already has expected counts by seed (from Script 03)
# Join with first_round and second_round summaries to get all rounds
advancement_by_seed <- deeper_summary %>%
    left_join(
        first_round_summary %>% select(seed, expected_wins),
        by = "seed"
    ) %>%
    left_join(
        second_round_summary %>% select(seed, expected_in_sweet16),
        by = "seed"
    ) %>%
    select(
        seed,
        r32 = expected_wins,
        sweet16 = expected_in_sweet16,
        elite8 = expected_elite8,
        final4 = expected_final4
    )

# Prepare data for plotting
advancement_data <- advancement_by_seed %>%
    pivot_longer(
        cols = c(r32, sweet16, elite8, final4),
        names_to = "round",
        values_to = "expected"
    ) %>%
    # Drop NAs and replace remaining NAs with 0 (no teams expected)
    mutate(expected = if_else(is.na(expected), 0, expected)) %>%
    drop_na(seed, round) %>%
    mutate(
        round = case_when(
            round == "r32" ~ "Round of 32",
            round == "sweet16" ~ "Sweet 16",
            round == "elite8" ~ "Elite 8",
            round == "final4" ~ "Final Four"
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
        caption = "Based on Bradley-Terry model probabilities. Seeds are model-assigned from regular-season results."
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
        caption = "Monte Carlo simulation, D1-only schedule, neutral sites. Seeds are model-assigned from regular-season results."
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

# FIX 5: Add 95% simulation intervals from mid_tier_counts
summary_ci <- mid_tier_counts %>%
    filter(round %in% c("Round of 32", "Sweet 16", "Elite 8", "Final Four")) %>%
    group_by(round) %>%
    summarise(
        mean = mean(n_mid_tier, na.rm = TRUE),
        lo = quantile(n_mid_tier, 0.025, na.rm = TRUE),
        hi = quantile(n_mid_tier, 0.975, na.rm = TRUE),
        .groups = "drop"
    )

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
    left_join(summary_ci, by = "round") %>%
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
    geom_errorbar(
        data = comparison_data %>% filter(method == "Monte Carlo Simulation"),
        aes(ymin = lo, ymax = hi),
        position = position_dodge(width = 0.8),
        width = 0.25,
        color = "gray30"
    ) +
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
        trans = "sqrt",
        breaks = c(0, 1, 2, 3, 5, 7, 10),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
        title = "Expected 8-12 Seeds Advancing: Model vs Simulation",
        subtitle = "Monte Carlo (5,000 sims), D1-only schedule, neutral sites",
        x = "Tournament Round",
        y = "Expected Number of 8-12 Seeds",
        fill = "Method",
        caption = "Note: √-scale y-axis to emphasize later-round variation. Seeds are model-assigned from regular-season results."
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

# Plot 7: Conditional probabilities (column names end with _given_s16)
cat("\nCreating Plot 7: Conditional probabilities...\n")

conditional_long <- conditional_summary %>%
    pivot_longer(
        cols = c(
            avg_prob_elite8_given_s16, avg_prob_final4_given_s16,
            avg_prob_finals_given_s16, avg_prob_champion_given_s16
        ),
        names_to = "target_round", values_to = "probability"
    ) %>%
    mutate(
        target_round = case_when(
            target_round == "avg_prob_elite8_given_s16" ~ "Elite 8",
            target_round == "avg_prob_final4_given_s16" ~ "Final Four",
            target_round == "avg_prob_finals_given_s16" ~ "Championship",
            target_round == "avg_prob_champion_given_s16" ~ "Win Title"
        ),
        target_round = factor(target_round, levels = c("Elite 8", "Final Four", "Championship", "Win Title"))
    )

p7 <- conditional_long %>%
    ggplot(aes(x = factor(seed), y = probability, fill = factor(seed))) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), vjust = -0.5, size = 3.5) +
    facet_wrap(~target_round, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = oi_colors[1:5]) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
    labs(
        title = "Conditional Advancement Probabilities for 8–12 Seeds",
        subtitle = "Given a team reaches the Sweet 16",
        x = "Seed", y = "Probability",
        caption = "Seeds are model-assigned from regular-season results for demonstration; not NCAA committee seeds."
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold")
    )

ggsave(here("results", "figures", "07_conditional_probabilities.png"), p7, width = 12, height = 8, dpi = 300)
cat("✓ Saved: 07_conditional_probabilities.png\n")


# =============================================================================
# 9. Plot 8: Seed Performance Heatmap
# =============================================================================

cat("\nCreating Plot 8: Seed performance heatmap...\n")

# FIX 1: Convert to per-team probability (divide by 4) to avoid >100% confusion
heatmap_data <- seed_performance %>%
    mutate(
        per_team_prob = pct_of_sims / 4, # Convert to per-team probability (0-100%)
        round = factor(round, levels = round_order)
    ) %>%
    filter(round != "Round of 64")

p8 <- heatmap_data %>%
    ggplot(aes(x = round, y = factor(winner_seed), fill = per_team_prob)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(
        aes(
            label = sprintf("%.0f%%", per_team_prob),
            color = per_team_prob > 35
        ), # Dynamic text color with better threshold
        fontface = "bold",
        size = 4
    ) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "gray10"), guide = "none") +
    scale_fill_gradientn(
        colours = c("#08306B", "#3182BD", "#6BAED6", "#C6DBEF"), # Darker palette for contrast
        values = scales::rescale(c(0, 0.25, 0.5, 1)),
        limits = c(0, max(heatmap_data$per_team_prob)),
        name = "% Sims\n(per team)"
    ) +
    labs(
        title = "Tournament Performance Heatmap: 8-12 Seeds",
        subtitle = "Per-team probability of reaching each round",
        x = "Tournament Round",
        y = "Seed",
        caption = "Seeds are model-assigned from regular-season results for demonstration; not NCAA committee seeds."
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
    geom_col(alpha = 0.9, show.legend = FALSE) +
    geom_errorbar(
        aes(ymin = lo, ymax = hi),
        width = 0.25,
        color = "gray30"
    ) +
    geom_text(aes(label = sprintf("%.1f", expected_count)),
        vjust = -0.5, size = 3
    ) +
    scale_fill_manual(values = oi_colors[1:4]) +
    scale_y_continuous(
        trans = "sqrt",
        breaks = c(0, 1, 2, 3, 5, 7),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
        title = "Expected 8-12 Seeds by Round",
        subtitle = "Monte Carlo (5,000 sims)",
        x = "Round", y = "Expected Count"
    ) +
    theme(
        text = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9),
        axis.text.x = element_text(angle = 20, hjust = 1)
    )

dash_p3 <- heatmap_data %>%
    ggplot(aes(x = round, y = factor(winner_seed), fill = per_team_prob)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
        aes(
            label = sprintf("%.0f%%", per_team_prob),
            color = per_team_prob > 35
        ), # Dynamic text color with better threshold
        fontface = "bold", size = 3
    ) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "gray10"), guide = "none") +
    scale_fill_gradientn(
        colours = c("#08306B", "#3182BD", "#6BAED6", "#C6DBEF"), # Darker palette for contrast
        values = scales::rescale(c(0, 0.25, 0.5, 1)),
        limits = c(0, max(heatmap_data$per_team_prob)),
        name = "% Sims"
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
    mutate(
        lambda = pmax(pmin(lambda, 8), -8), # Clamp to [-8, 8]
        se = pmin(se, 2), # Cap huge SEs
        seed_category = factor(
            seed_category,
            levels = c("1-4 seeds", "5-7 seeds", "8-12 seeds", "13-16 seeds")
        )
    ) %>%
    ggplot(aes(x = seed_category, y = lambda, fill = seed_category)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
    coord_cartesian(ylim = c(-2, 8)) +
    scale_fill_manual(values = oi_colors[c(1, 2, 3, 6)], guide = "none") +
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
        subtitle = "D-I Schedule Only • Neutral Sites • 5,000 Sims",
        caption = "Seeds are model-assigned from regular-season results; not official NCAA seeds.",
        theme = theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(t = 10))
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

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("VISUALIZATION SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

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

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("✓ Visualization complete!\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

# =============================================================================
# NOTES:
# - All plots use colorblind-friendly Okabe-Ito palette
# - High resolution (300 DPI) suitable for presentations/papers
# - Dashboard provides quick overview of key findings
# - Individual plots allow detailed examination of specific aspects
# =============================================================================
