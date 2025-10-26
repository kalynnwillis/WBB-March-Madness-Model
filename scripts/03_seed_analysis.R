# =============================================================================
# Script 03: 8-12 Seed Analysis
# Purpose: Analyze advancement probabilities for mid-tier seeds
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)

# Load helper functions
source(here("scripts", "00_helper_functions.R"))

dir_create_safe("results", "tables")

# =============================================================================
# 1. Load Model Results
# =============================================================================

cat("Loading model results and data...\n")

bt_model <- readRDS(here("data", "processed", "bt_model.rds"))
team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))
win_probs <- readRDS(here("data", "processed", "win_probability_matrix.rds"))
tournament_seeds <- readRDS(here("data", "processed", "tournament_seeds.rds"))

cat("✓ Data loaded successfully\n")

# =============================================================================
# 2. Identify 8-12 Seed Teams and Their Opponents
# =============================================================================

cat("\nAnalyzing 8-12 seed teams...\n")

# Filter for 8-12 seeds
mid_tier_teams <- team_abilities %>%
    filter(seed >= 8 & seed <= 12) %>%
    arrange(seed, desc(lambda))

cat(sprintf("Found %d teams with 8-12 seeds\n", nrow(mid_tier_teams)))

# Display all 8-12 seed teams
cat("\n8-12 Seed Teams:\n")
print(mid_tier_teams %>% select(team, seed, region, lambda, se))

# =============================================================================
# 3. Calculate First Round Win Probabilities (Round of 64 -> Round of 32)
# =============================================================================

cat("\nCalculating first round win probabilities...\n")

# Define standard NCAA tournament first-round matchups
# 8 plays 9, 9 plays 8, 10 plays 7, 11 plays 6, 12 plays 5
matchup_map <- tibble(
    seed = c(8, 9, 10, 11, 12),
    opponent_seed = c(9, 8, 7, 6, 5),
    round = "Round of 64"
)

# For each 8-12 seed, calculate probability of beating their first-round opponent
first_round_analysis <- mid_tier_teams %>%
    left_join(matchup_map, by = "seed") %>%
    rowwise() %>%
    mutate(
        # Find the opponent's ability
        opponent_lambda = {
            opponent_team <- team_abilities %>%
                filter(seed == opponent_seed, region == region) %>%
                pull(lambda)
            if (length(opponent_team) > 0) opponent_team[1] else NA_real_
        },
        # Calculate win probability
        prob_win_round1 = if_else(
            !is.na(opponent_lambda),
            1 / (1 + exp(-(lambda - opponent_lambda))),
            NA_real_
        )
    ) %>%
    ungroup()

# Summary by seed - FIX 2: Use 10-90th percentile instead of min-max
first_round_summary <- first_round_analysis %>%
    group_by(seed) %>%
    summarise(
        n_teams = n(),
        avg_prob_win = mean(prob_win_round1, na.rm = TRUE),
        min_prob_win = min(prob_win_round1, na.rm = TRUE), # Keep for compatibility
        max_prob_win = max(prob_win_round1, na.rm = TRUE), # Keep for compatibility
        p10_prob_win = quantile(prob_win_round1, 0.10, na.rm = TRUE),
        p90_prob_win = quantile(prob_win_round1, 0.90, na.rm = TRUE),
        expected_wins = sum(prob_win_round1, na.rm = TRUE)
    )

cat("\nFirst Round Win Probabilities by Seed:\n")
print(first_round_summary)

# Expected number of 8-12 seeds advancing to Round of 32
expected_advance_r32 <- sum(first_round_summary$expected_wins)
cat(sprintf(
    "\nExpected # of 8-12 seeds advancing to Round of 32: %.2f\n",
    expected_advance_r32
))

# =============================================================================
# 4. Calculate Second Round Win Probabilities (Round of 32 -> Sweet 16)
# =============================================================================

cat("\nCalculating second round win probabilities...\n")

# Second round opponents are typically 1-4 seeds
# 8/9 winner plays 1, 5/12 winner plays 4, etc.
second_round_matchups <- tibble(
    seed = c(8, 9, 10, 11, 12),
    r2_opponent_seed = c(1, 1, 2, 3, 4)
)

second_round_analysis <- first_round_analysis %>%
    left_join(second_round_matchups, by = "seed") %>%
    rowwise() %>%
    mutate(
        # Find second round opponent's ability
        r2_opponent_lambda = {
            opponent_team <- team_abilities %>%
                filter(seed == r2_opponent_seed, region == region) %>%
                pull(lambda)
            if (length(opponent_team) > 0) opponent_team[1] else NA_real_
        },
        # Probability of winning second round (conditional on advancing)
        prob_win_round2_conditional = if_else(
            !is.na(r2_opponent_lambda),
            1 / (1 + exp(-(lambda - r2_opponent_lambda))),
            NA_real_
        ),
        # Probability of reaching Sweet 16 (winning both games)
        prob_reach_sweet16 = prob_win_round1 * prob_win_round2_conditional
    ) %>%
    ungroup()

# Summary by seed
second_round_summary <- second_round_analysis %>%
    group_by(seed) %>%
    summarise(
        n_teams = n(),
        avg_prob_win_r2_conditional = mean(prob_win_round2_conditional, na.rm = TRUE),
        avg_prob_reach_sweet16 = mean(prob_reach_sweet16, na.rm = TRUE),
        expected_in_sweet16 = sum(prob_reach_sweet16, na.rm = TRUE)
    )

cat("\nSecond Round Analysis by Seed:\n")
print(second_round_summary)

# Expected number of 8-12 seeds in Sweet 16
expected_in_sweet16 <- sum(second_round_summary$expected_in_sweet16)
cat(sprintf("\nExpected # of 8-12 seeds in Sweet 16: %.2f\n", expected_in_sweet16))

# =============================================================================
# 5. Answer Research Question 1: Expected Advancement Past Round 2
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("RESEARCH QUESTION 1: Expected 8-12 Seeds to Advance Past Round 2\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat(sprintf(
    "Expected 8-12 seeds to advance to Round of 32: %.2f\n",
    expected_advance_r32
))
cat(sprintf(
    "Expected 8-12 seeds to advance to Sweet 16: %.2f\n",
    expected_in_sweet16
))

cat("\nBy Seed:\n")
advancement_summary <- second_round_summary %>%
    left_join(first_round_summary %>% select(seed, expected_wins), by = "seed") %>%
    select(
        seed,
        n_teams,
        expected_r32 = expected_wins,
        expected_sweet16 = expected_in_sweet16
    )
print(advancement_summary)

# =============================================================================
# 6. Calculate Elite 8 and Final Four Probabilities
# =============================================================================

cat("\nCalculating Elite 8 and Final Four probabilities...\n")

# For teams that reach Sweet 16, they face other region winners
# Typically 1-4 seeds, but some variation
# We'll use average abilities of top seeds as approximation

# Get average abilities of top seeds by region
top_seed_abilities <- team_abilities %>%
    filter(seed <= 4) %>%
    group_by(region) %>%
    summarise(avg_top_lambda = mean(lambda, na.rm = TRUE))

# Calculate probabilities for deeper runs
deeper_runs <- second_round_analysis %>%
    left_join(
        top_seed_abilities %>% select(region, avg_top_lambda),
        by = "region"
    ) %>%
    mutate(
        # Sweet 16 opponent (typically 4/5 or 1 seed)
        prob_win_sweet16 = 1 / (1 + exp(-(lambda - avg_top_lambda))),
        prob_reach_elite8 = prob_reach_sweet16 * prob_win_sweet16,
        # Elite 8 opponent (typically 1 or 2 seed)
        prob_win_elite8 = 1 / (1 + exp(-(lambda - avg_top_lambda - 0.5))),
        prob_reach_final4 = prob_reach_elite8 * prob_win_elite8,
        # Final Four (facing another region's top team)
        prob_win_final4 = 1 / (1 + exp(-(lambda - avg_top_lambda - 0.5))),
        prob_reach_finals = prob_reach_final4 * prob_win_final4,
        # Championship game
        prob_win_championship = 1 / (1 + exp(-(lambda - avg_top_lambda - 0.5))),
        prob_win_title = prob_reach_finals * prob_win_championship
    )

# Summary by seed for deeper rounds
deeper_summary <- deeper_runs %>%
    group_by(seed) %>%
    summarise(
        expected_elite8 = sum(prob_reach_elite8, na.rm = TRUE),
        expected_final4 = sum(prob_reach_final4, na.rm = TRUE),
        expected_finals = sum(prob_reach_finals, na.rm = TRUE),
        expected_champions = sum(prob_win_title, na.rm = TRUE)
    )

cat("\nDeeper Tournament Run Probabilities:\n")
print(deeper_summary)

# =============================================================================
# 7. Conditional Probabilities (Given Advancement Past Round 2)
# NOTE: These are ANALYTICAL ESTIMATES based on heuristic opponent assumptions.
#       For robust conditional probabilities, refer to Script 04 simulation results.
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("RESEARCH QUESTION 2: Conditional Probabilities (Analytical Estimates)\n")
cat("⚠️  NOTE: Simulation-based conditionals (Script 04) are the primary results.\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat("Given that an 8-12 seed advances past Round 2 (reaches Sweet 16):\n\n")

# Calculate conditional probabilities
conditional_probs <- deeper_runs %>%
    mutate(
        # P(Elite 8 | Sweet 16)
        prob_elite8_given_sweet16 = prob_reach_elite8 / prob_reach_sweet16,
        # P(Final 4 | Sweet 16)
        prob_final4_given_sweet16 = prob_reach_final4 / prob_reach_sweet16,
        # P(Finals | Sweet 16)
        prob_finals_given_sweet16 = prob_reach_finals / prob_reach_sweet16,
        # P(Champion | Sweet 16)
        prob_champion_given_sweet16 = prob_win_title / prob_reach_sweet16
    )

conditional_summary <- conditional_probs %>%
    group_by(seed) %>%
    summarise(
        n_teams = n(),
        avg_prob_elite8_given_s16 = mean(prob_elite8_given_sweet16, na.rm = TRUE),
        avg_prob_final4_given_s16 = mean(prob_final4_given_sweet16, na.rm = TRUE),
        avg_prob_finals_given_s16 = mean(prob_finals_given_sweet16, na.rm = TRUE),
        avg_prob_champion_given_s16 = mean(prob_champion_given_sweet16, na.rm = TRUE)
    )

print(conditional_summary)

# Overall averages for 8-12 seeds
overall_conditional <- conditional_probs %>%
    summarise(
        prob_elite8_given_s16 = weighted.mean(prob_elite8_given_sweet16,
            prob_reach_sweet16,
            na.rm = TRUE
        ),
        prob_final4_given_s16 = weighted.mean(prob_final4_given_sweet16,
            prob_reach_sweet16,
            na.rm = TRUE
        ),
        prob_finals_given_s16 = weighted.mean(prob_finals_given_sweet16,
            prob_reach_sweet16,
            na.rm = TRUE
        ),
        prob_champion_given_s16 = weighted.mean(prob_champion_given_sweet16,
            prob_reach_sweet16,
            na.rm = TRUE
        )
    )

cat("\nOverall Conditional Probabilities for 8-12 Seeds:\n")
cat(sprintf(
    "  P(Elite 8 | Sweet 16) = %.2f%%\n",
    overall_conditional$prob_elite8_given_s16 * 100
))
cat(sprintf(
    "  P(Final 4 | Sweet 16) = %.2f%%\n",
    overall_conditional$prob_final4_given_s16 * 100
))
cat(sprintf(
    "  P(Finals | Sweet 16) = %.2f%%\n",
    overall_conditional$prob_finals_given_s16 * 100
))
cat(sprintf(
    "  P(Champion | Sweet 16) = %.2f%%\n",
    overall_conditional$prob_champion_given_s16 * 100
))

# =============================================================================
# 8. Compare to Higher Seeds
# =============================================================================

cat("\nComparison to other seed ranges...\n")

# Calculate similar metrics for all tournament seeds
all_seeds_summary <- team_abilities %>%
    filter(!is.na(seed)) %>%
    mutate(
        seed_group = case_when(
            seed <= 4 ~ "1-4 seeds",
            seed <= 7 ~ "5-7 seeds",
            seed <= 12 ~ "8-12 seeds",
            TRUE ~ "13-16 seeds"
        )
    ) %>%
    group_by(seed_group) %>%
    summarise(
        n_teams = n(),
        avg_lambda = mean(lambda),
        sd_lambda = sd(lambda)
    )

cat("\nAverage team strength by seed group:\n")
print(all_seeds_summary)

# =============================================================================
# 9. Save Analysis Results
# =============================================================================

cat("\nSaving analysis results...\n")

# Save detailed team-level results
saveRDS(first_round_analysis,
    file = here("results", "tables", "first_round_analysis.rds")
)
saveRDS(second_round_analysis,
    file = here("results", "tables", "second_round_analysis.rds")
)
saveRDS(deeper_runs,
    file = here("results", "tables", "deeper_runs_analysis.rds")
)
saveRDS(conditional_probs,
    file = here("results", "tables", "conditional_probabilities.rds")
)

# Save summary tables
write_csv(first_round_summary,
    file = here("results", "tables", "first_round_summary.csv")
)
write_csv(second_round_summary,
    file = here("results", "tables", "second_round_summary.csv")
)
write_csv(deeper_summary,
    file = here("results", "tables", "deeper_runs_summary.csv")
)
write_csv(conditional_summary,
    file = here("results", "tables", "conditional_summary.csv")
)
write_csv(advancement_summary,
    file = here("results", "tables", "advancement_summary.csv")
)

# Save key results
key_results <- list(
    expected_advance_r32 = expected_advance_r32,
    expected_in_sweet16 = expected_in_sweet16,
    overall_conditional = overall_conditional,
    first_round_summary = first_round_summary,
    second_round_summary = second_round_summary,
    deeper_summary = deeper_summary,
    conditional_summary = conditional_summary
)

saveRDS(key_results, file = here("results", "tables", "key_results.rds"))

# =============================================================================
# 10. Generate Final Summary Report
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("8-12 SEED ANALYSIS SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat("RESEARCH QUESTION 1:\n")
cat("How many 8-12 seeds should we expect to advance past Round 2?\n\n")
cat(sprintf(
    "Answer: We expect approximately %.1f of the 8-12 seeds to advance\n",
    expected_in_sweet16
))
cat(sprintf("        to the Sweet 16 (past Round 2).\n\n"))

cat("Breakdown by seed:\n")
print(advancement_summary)

cat("\n", paste(rep("-", 70), collapse = ""), "\n\n", sep = "")

cat("RESEARCH QUESTION 2:\n")
cat("Given that an 8-12 seed advanced past Round 2, what is the chance\n")
cat("they make it to the finals or win the championship?\n\n")

cat(sprintf("Answer: An 8-12 seed that reaches the Sweet 16 has:\n"))
cat(sprintf(
    "  - %.1f%% chance of reaching Elite 8\n",
    overall_conditional$prob_elite8_given_s16 * 100
))
cat(sprintf(
    "  - %.1f%% chance of reaching Final Four\n",
    overall_conditional$prob_final4_given_s16 * 100
))
cat(sprintf(
    "  - %.1f%% chance of reaching the Finals\n",
    overall_conditional$prob_finals_given_s16 * 100
))
cat(sprintf(
    "  - %.1f%% chance of winning the championship\n",
    overall_conditional$prob_champion_given_s16 * 100
))

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("✓ Seed analysis complete!\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

# =============================================================================
# NOTES:
# - Conditional probabilities assume team strength remains constant
# - Does not account for injuries, momentum, or other contextual factors
# - Opponent assumptions are based on typical bracket structure
# - May want to run sensitivity analysis with different opponent assumptions
# =============================================================================
