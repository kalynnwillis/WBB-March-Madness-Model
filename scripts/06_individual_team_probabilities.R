# =============================================================================
# Script 06: Individual Team Probabilities
# Purpose: Calculate the probability of specific teams reaching each round
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)

# Load helper functions
source(here("scripts", "00_helper_functions.R"))

dir_create_safe("results", "tables")

# =============================================================================
# 1. Load Simulation Results
# =============================================================================

cat("Loading simulation results...\n")

all_simulations <- readRDS(here("results", "tables", "all_simulations.rds"))
team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))

cat("✓ Data loaded successfully\n")

# =============================================================================
# 2. Calculate Individual Team Advancement Probabilities
# =============================================================================

cat("\nCalculating individual team advancement probabilities...\n")

# Define round order
round_order <- c(
    "Round of 64", "Round of 32", "Sweet 16",
    "Elite 8", "Final Four", "Championship"
)

# Count how many times each team reached each round
team_round_counts <- all_simulations %>%
    group_by(winner_name, round) %>%
    summarise(times_reached = n(), .groups = "drop")

# Get total number of simulations
n_sims <- length(unique(all_simulations$sim_id))

# Calculate probabilities for each team reaching each round
team_probabilities <- team_round_counts %>%
    mutate(
        probability = times_reached / n_sims,
        percentage = probability * 100
    ) %>%
    arrange(winner_name, match(round, round_order))

# Join with team abilities to get seed information
team_probabilities_full <- team_probabilities %>%
    left_join(
        team_abilities %>% select(team, seed, region, lambda, is_mid_tier_seed),
        by = c("winner_name" = "team")
    )

# =============================================================================
# 3. Focus on 8-12 Seed Teams
# =============================================================================

cat("\nAnalyzing 8-12 seed team probabilities...\n")

mid_tier_probabilities <- team_probabilities_full %>%
    filter(is_mid_tier_seed == TRUE) %>%
    arrange(seed, desc(probability))

# Get Sweet 16 probabilities for 8-12 seeds
sweet16_mid_tier <- mid_tier_probabilities %>%
    filter(round == "Sweet 16") %>%
    arrange(desc(probability))

cat("\nProbability of each 8-12 seed reaching Sweet 16:\n")
print(sweet16_mid_tier %>%
    select(winner_name, seed, region, probability, percentage, times_reached))

# =============================================================================
# 4. Create Comprehensive Team Report
# =============================================================================

cat("\nGenerating comprehensive team advancement report...\n")

# For each team, create a row with probabilities for all rounds
team_advancement_matrix <- team_probabilities_full %>%
    select(winner_name, round, probability, seed, region, is_mid_tier_seed) %>%
    pivot_wider(
        names_from = round,
        values_from = probability,
        values_fill = 0
    ) %>%
    # Ensure columns are in correct order
    select(
        winner_name, seed, region, is_mid_tier_seed,
        any_of(c("Round of 64", "Round of 32", "Sweet 16",
                 "Elite 8", "Final Four", "Championship"))
    )

# Filter for 8-12 seeds and sort by Sweet 16 probability
mid_tier_advancement <- team_advancement_matrix %>%
    filter(is_mid_tier_seed == TRUE) %>%
    arrange(desc(`Sweet 16`))

cat("\nTop 8-12 seeds by Sweet 16 probability:\n")
print(mid_tier_advancement %>%
    select(winner_name, seed, region, `Sweet 16`, `Elite 8`, `Final Four`) %>%
    head(10))

# =============================================================================
# 5. Statistical Summary by Seed
# =============================================================================

cat("\nCalculating statistical summary by seed...\n")

seed_summary <- team_probabilities_full %>%
    filter(is_mid_tier_seed == TRUE) %>%
    group_by(seed, round) %>%
    summarise(
        n_teams = n_distinct(winner_name),
        avg_probability = mean(probability),
        min_probability = min(probability),
        max_probability = max(probability),
        sd_probability = sd(probability),
        .groups = "drop"
    ) %>%
    arrange(seed, match(round, round_order))

cat("\nAverage advancement probabilities by seed:\n")
print(seed_summary %>%
    filter(round == "Sweet 16") %>%
    select(seed, n_teams, avg_probability, min_probability, max_probability))

# =============================================================================
# 6. Identify Most Likely Mid-Tier Sweet 16 Teams
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("MOST LIKELY 8-12 SEEDS TO REACH SWEET 16\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

top_sweet16_candidates <- sweet16_mid_tier %>%
    arrange(desc(probability)) %>%
    head(10)

for (i in 1:nrow(top_sweet16_candidates)) {
    team <- top_sweet16_candidates[i, ]
    cat(sprintf(
        "%d. %s (Seed %d, %s Region)\n",
        i, team$winner_name, team$seed, team$region
    ))
    cat(sprintf(
        "   Sweet 16 Probability: %.2f%% (%d / %d simulations)\n",
        team$percentage, team$times_reached, n_sims
    ))
    
    # Get probabilities for other rounds
    team_all_rounds <- team_probabilities_full %>%
        filter(winner_name == team$winner_name) %>%
        arrange(match(round, round_order))
    
    cat("   Round-by-round probabilities:\n")
    for (j in 1:nrow(team_all_rounds)) {
        cat(sprintf(
            "     - %s: %.2f%%\n",
            team_all_rounds$round[j],
            team_all_rounds$percentage[j]
        ))
    }
    cat("\n")
}

# =============================================================================
# 7. Compare to Historical Data (2023-2024)
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("HISTORICAL CONTEXT\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat("Historical Observation: In 2023 and 2024, NO 8-12 seeds made it to Sweet 16\n\n")

# Calculate probability of this happening
prob_no_mid_tier_sweet16 <- sweet16_mid_tier %>%
    summarise(
        total_expected = sum(probability),
        prob_zero = dpois(0, lambda = sum(probability))
    )

cat(sprintf(
    "Based on our model:\n"
))
cat(sprintf(
    "  - Expected # of 8-12 seeds in Sweet 16: %.2f\n",
    prob_no_mid_tier_sweet16$total_expected
))
cat(sprintf(
    "  - Probability of ZERO 8-12 seeds in Sweet 16: %.2f%%\n",
    prob_no_mid_tier_sweet16$prob_zero * 100
))
cat("\n")
cat("This suggests the 2023-2024 results were somewhat unusual but not impossible.\n")

# =============================================================================
# 8. Save Results
# =============================================================================

cat("\nSaving results...\n")

# Save individual team probabilities
saveRDS(team_probabilities_full,
    file = here("results", "tables", "individual_team_probabilities.rds")
)
write_csv(team_probabilities_full,
    file = here("results", "tables", "individual_team_probabilities.csv")
)

# Save advancement matrix
saveRDS(team_advancement_matrix,
    file = here("results", "tables", "team_advancement_matrix.rds")
)
write_csv(team_advancement_matrix,
    file = here("results", "tables", "team_advancement_matrix.csv")
)

# Save mid-tier specific results
write_csv(mid_tier_advancement,
    file = here("results", "tables", "mid_tier_team_advancement.csv")
)
write_csv(sweet16_mid_tier,
    file = here("results", "tables", "sweet16_mid_tier_probabilities.csv")
)

# Save seed-level summary
write_csv(seed_summary,
    file = here("results", "tables", "seed_advancement_summary.csv")
)

cat("✓ Results saved successfully\n")

# =============================================================================
# 9. Generate Summary Report
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("INDIVIDUAL TEAM PROBABILITY ANALYSIS COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat(sprintf("Total teams analyzed: %d\n", n_distinct(team_probabilities$winner_name)))
cat(sprintf("8-12 seed teams: %d\n", n_distinct(mid_tier_probabilities$winner_name)))
cat(sprintf("Simulations run: %d\n\n", n_sims))

cat("Key Findings:\n")
cat(sprintf(
    "1. Highest Sweet 16 probability (8-12 seed): %s (%.2f%%)\n",
    top_sweet16_candidates$winner_name[1],
    top_sweet16_candidates$percentage[1]
))
cat(sprintf(
    "2. Average Sweet 16 probability for 8-12 seeds: %.2f%%\n",
    mean(sweet16_mid_tier$percentage)
))
cat(sprintf(
    "3. Number of 8-12 seeds with >10%% Sweet 16 chance: %d\n",
    sum(sweet16_mid_tier$percentage > 10)
))

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("✓ Analysis complete!\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

# =============================================================================
# NOTES:
# - Probabilities are based on Monte Carlo simulation results
# - Teams that didn't appear in a round have 0% probability for that round
# - Probabilities reflect both team strength and bracket positioning
# - Results can be used to identify potential "Cinderella" teams
# =============================================================================

