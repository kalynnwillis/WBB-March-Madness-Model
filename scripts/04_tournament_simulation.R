# =============================================================================
# Script 04: Tournament Simulation
# Purpose: Monte Carlo simulation of March Madness tournament outcomes
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)

# =============================================================================
# 1. Load Model Results and Data
# =============================================================================

cat("Loading model results...\n")

team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))
win_probs <- readRDS(here("data", "processed", "win_probability_matrix.rds"))

cat("✓ Data loaded successfully\n")

# =============================================================================
# 2. Define Tournament Structure
# =============================================================================

cat("\nDefining tournament structure...\n")

# Standard NCAA Women's Tournament bracket
# 64 teams, 4 regions, single elimination

# Define the bracket structure with standard matchups
create_bracket_structure <- function() {
    # Each region has 16 teams seeded 1-16
    regions <- c("Portland", "Albany", "Spokane", "Wichita") # Example region names

    # Round 1 matchups (Round of 64)
    round1_matchups <- tibble(
        round = "Round of 64",
        matchup_id = 1:32,
        region = rep(regions, each = 8),
        higher_seed = rep(c(1, 2, 3, 4, 5, 6, 7, 8), times = 4),
        lower_seed = rep(c(16, 15, 14, 13, 12, 11, 10, 9), times = 4)
    )

    return(round1_matchups)
}

bracket_structure <- create_bracket_structure()

cat(sprintf("Created bracket with %d first-round games\n", nrow(bracket_structure)))

# =============================================================================
# 3. Helper Functions for Tournament Simulation
# =============================================================================

# Function to get team ability by seed and region
get_team_by_seed_region <- function(seed, region, team_data) {
    teams <- team_data %>%
        filter(seed == !!seed, region == !!region)

    if (nrow(teams) > 0) {
        return(teams[1, ])
    } else {
        # If exact match not found, use average for that seed
        avg_teams <- team_data %>%
            filter(seed == !!seed) %>%
            summarise(
                team = paste0("Seed_", seed, "_", region),
                lambda = mean(lambda, na.rm = TRUE),
                se = mean(se, na.rm = TRUE),
                seed = seed,
                region = region
            )
        if (nrow(avg_teams) > 0) {
            return(avg_teams[1, ])
        } else {
            return(NULL)
        }
    }
}

# Function to simulate a single game
simulate_game <- function(team1_lambda, team2_lambda, seed = NULL) {
    # Calculate probability that team1 wins
    prob_team1_wins <- 1 / (1 + exp(-(team1_lambda - team2_lambda)))

    # Simulate outcome (1 = team1 wins, 0 = team2 wins)
    outcome <- rbinom(n = 1, size = 1, prob = prob_team1_wins)

    return(outcome)
}

# Function to simulate entire tournament
simulate_tournament <- function(team_data, seed_val = NULL) {
    regions <- unique(bracket_structure$region)

    # Initialize results tracking
    all_results <- tibble()

    # ROUND 1: Round of 64 -> Round of 32
    round1_results <- bracket_structure %>%
        rowwise() %>%
        mutate(
            team1_info = list(get_team_by_seed_region(higher_seed, region, team_data)),
            team2_info = list(get_team_by_seed_region(lower_seed, region, team_data))
        ) %>%
        mutate(
            team1_lambda = {
                info <- team1_info[[1]]
                if (!is.null(info) && is.data.frame(info) && nrow(info) > 0) {
                    as.numeric(info$lambda[1])
                } else {
                    NA_real_
                }
            },
            team2_lambda = {
                info <- team2_info[[1]]
                if (!is.null(info) && is.data.frame(info) && nrow(info) > 0) {
                    as.numeric(info$lambda[1])
                } else {
                    NA_real_
                }
            },
            team1_name = {
                info <- team1_info[[1]]
                if (!is.null(info) && is.data.frame(info) && nrow(info) > 0) {
                    as.character(info$team[1])
                } else {
                    paste0("Team_", higher_seed)
                }
            },
            team2_name = {
                info <- team2_info[[1]]
                if (!is.null(info) && is.data.frame(info) && nrow(info) > 0) {
                    as.character(info$team[1])
                } else {
                    paste0("Team_", lower_seed)
                }
            },
            team1_seed = higher_seed,
            team2_seed = lower_seed,
            outcome = simulate_game(team1_lambda, team2_lambda, seed = seed_val),
            winner_seed = ifelse(outcome == 1, team1_seed, team2_seed),
            winner_lambda = ifelse(outcome == 1, team1_lambda, team2_lambda),
            winner_name = ifelse(outcome == 1, team1_name, team2_name)
        ) %>%
        ungroup() %>%
        select(
            round, region, matchup_id, team1_seed, team2_seed,
            winner_seed, winner_lambda, winner_name
        )

    all_results <- bind_rows(all_results, round1_results)

    # ROUND 2: Round of 32 -> Sweet 16
    round2_matchups <- round1_results %>%
        group_by(region) %>%
        mutate(
            r2_matchup = rep(1:4, each = 2),
            game_num = row_number()
        ) %>%
        group_by(region, r2_matchup) %>%
        filter(n() == 2) %>%
        summarise(
            round = "Round of 32",
            matchup_id = first(r2_matchup),
            team1_seed = first(winner_seed),
            team1_lambda = first(winner_lambda),
            team1_name = first(winner_name),
            team2_seed = last(winner_seed),
            team2_lambda = last(winner_lambda),
            team2_name = last(winner_name),
            .groups = "drop"
        ) %>%
        rowwise() %>%
        mutate(
            outcome = simulate_game(team1_lambda, team2_lambda, seed = seed_val),
            winner_seed = ifelse(outcome == 1, team1_seed, team2_seed),
            winner_lambda = ifelse(outcome == 1, team1_lambda, team2_lambda),
            winner_name = ifelse(outcome == 1, team1_name, team2_name)
        ) %>%
        ungroup() %>%
        select(
            round, region, matchup_id, team1_seed, team2_seed,
            winner_seed, winner_lambda, winner_name
        )

    all_results <- bind_rows(all_results, round2_matchups)

    # ROUND 3: Sweet 16 -> Elite 8
    round3_matchups <- round2_matchups %>%
        group_by(region) %>%
        mutate(
            r3_matchup = rep(1:2, each = 2),
            game_num = row_number()
        ) %>%
        group_by(region, r3_matchup) %>%
        filter(n() == 2) %>%
        summarise(
            round = "Sweet 16",
            matchup_id = first(r3_matchup),
            team1_seed = first(winner_seed),
            team1_lambda = first(winner_lambda),
            team1_name = first(winner_name),
            team2_seed = last(winner_seed),
            team2_lambda = last(winner_lambda),
            team2_name = last(winner_name),
            .groups = "drop"
        ) %>%
        rowwise() %>%
        mutate(
            outcome = simulate_game(team1_lambda, team2_lambda, seed = seed_val),
            winner_seed = ifelse(outcome == 1, team1_seed, team2_seed),
            winner_lambda = ifelse(outcome == 1, team1_lambda, team2_lambda),
            winner_name = ifelse(outcome == 1, team1_name, team2_name)
        ) %>%
        ungroup() %>%
        select(
            round, region, matchup_id, team1_seed, team2_seed,
            winner_seed, winner_lambda, winner_name
        )

    all_results <- bind_rows(all_results, round3_matchups)

    # ROUND 4: Elite 8 -> Final Four
    round4_matchups <- round3_matchups %>%
        group_by(region) %>%
        summarise(
            round = "Elite 8",
            matchup_id = 1,
            team1_seed = first(winner_seed),
            team1_lambda = first(winner_lambda),
            team1_name = first(winner_name),
            team2_seed = last(winner_seed),
            team2_lambda = last(winner_lambda),
            team2_name = last(winner_name),
            .groups = "drop"
        ) %>%
        rowwise() %>%
        mutate(
            outcome = simulate_game(team1_lambda, team2_lambda, seed = seed_val),
            winner_seed = ifelse(outcome == 1, team1_seed, team2_seed),
            winner_lambda = ifelse(outcome == 1, team1_lambda, team2_lambda),
            winner_name = ifelse(outcome == 1, team1_name, team2_name)
        ) %>%
        ungroup() %>%
        select(
            round, region, matchup_id, team1_seed, team2_seed,
            winner_seed, winner_lambda, winner_name
        )

    all_results <- bind_rows(all_results, round4_matchups)

    # ROUND 5: Final Four -> Finals
    final4_matchups <- round4_matchups %>%
        mutate(semifinal = rep(1:2, length.out = n())) %>%
        group_by(semifinal) %>%
        filter(n() == 2) %>%
        summarise(
            round = "Final Four",
            region = "National",
            matchup_id = first(semifinal),
            team1_seed = first(winner_seed),
            team1_lambda = first(winner_lambda),
            team1_name = first(winner_name),
            team2_seed = last(winner_seed),
            team2_lambda = last(winner_lambda),
            team2_name = last(winner_name),
            .groups = "drop"
        ) %>%
        rowwise() %>%
        mutate(
            outcome = simulate_game(team1_lambda, team2_lambda, seed = seed_val),
            winner_seed = ifelse(outcome == 1, team1_seed, team2_seed),
            winner_lambda = ifelse(outcome == 1, team1_lambda, team2_lambda),
            winner_name = ifelse(outcome == 1, team1_name, team2_name)
        ) %>%
        ungroup() %>%
        select(
            round, region, matchup_id, team1_seed, team2_seed,
            winner_seed, winner_lambda, winner_name
        )

    all_results <- bind_rows(all_results, final4_matchups)

    # ROUND 6: Championship
    championship <- final4_matchups %>%
        summarise(
            round = "Championship",
            region = "National",
            matchup_id = 1,
            team1_seed = first(winner_seed),
            team1_lambda = first(winner_lambda),
            team1_name = first(winner_name),
            team2_seed = last(winner_seed),
            team2_lambda = last(winner_lambda),
            team2_name = last(winner_name)
        ) %>%
        rowwise() %>%
        mutate(
            outcome = simulate_game(team1_lambda, team2_lambda, seed = seed_val),
            winner_seed = ifelse(outcome == 1, team1_seed, team2_seed),
            winner_lambda = ifelse(outcome == 1, team1_lambda, team2_lambda),
            winner_name = ifelse(outcome == 1, team1_name, team2_name)
        ) %>%
        ungroup() %>%
        select(
            round, region, matchup_id, team1_seed, team2_seed,
            winner_seed, winner_lambda, winner_name
        )

    all_results <- bind_rows(all_results, championship)

    return(all_results)
}

# =============================================================================
# 4. Run Monte Carlo Simulations
# =============================================================================

cat("\nRunning Monte Carlo simulations...\n")

# Set number of simulations
N_SIMS <- 5000

cat(sprintf("Simulating %d tournaments...\n", N_SIMS))

# Initialize storage for simulation results
simulation_results <- list()

# Run simulations with progress updates
for (sim in 1:N_SIMS) {
    if (sim %% 500 == 0) {
        cat(sprintf("  Completed %d/%d simulations...\n", sim, N_SIMS))
    }

    set.seed(479 + sim)

    # Run one tournament simulation
    tournament_result <- simulate_tournament(team_abilities, seed_val = 479 + sim)

    # Add simulation ID
    tournament_result$sim_id <- sim

    simulation_results[[sim]] <- tournament_result
}

# Combine all simulations
all_simulations <- bind_rows(simulation_results)

cat(sprintf("✓ Completed %d tournament simulations\n", N_SIMS))

# =============================================================================
# 5. Analyze 8-12 Seed Performance Across Simulations
# =============================================================================

cat("\nAnalyzing 8-12 seed performance across simulations...\n")

# Function to count 8-12 seeds by round
count_mid_tier_by_round <- function(sim_data) {
    sim_data %>%
        filter(winner_seed >= 8 & winner_seed <= 12) %>%
        group_by(round) %>%
        summarise(n_mid_tier = n(), .groups = "drop")
}

# Count 8-12 seeds in each round for each simulation
mid_tier_counts <- all_simulations %>%
    group_by(sim_id) %>%
    group_modify(~ count_mid_tier_by_round(.x)) %>%
    ungroup()

# Define round order
round_order <- c(
    "Round of 64", "Round of 32", "Sweet 16",
    "Elite 8", "Final Four", "Championship"
)

mid_tier_counts <- mid_tier_counts %>%
    mutate(round = factor(round, levels = round_order))

# Summary statistics by round
mid_tier_summary <- mid_tier_counts %>%
    group_by(round) %>%
    summarise(
        mean_count = mean(n_mid_tier),
        median_count = median(n_mid_tier),
        sd_count = sd(n_mid_tier),
        min_count = min(n_mid_tier),
        max_count = max(n_mid_tier),
        q25 = quantile(n_mid_tier, 0.25),
        q75 = quantile(n_mid_tier, 0.75)
    )

cat("\n8-12 Seed Advancement by Round:\n")
print(mid_tier_summary)

# =============================================================================
# 6. Answer Research Question 1: Expected Advancement
# =============================================================================

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("RESEARCH QUESTION 1: Expected 8-12 Seeds by Round\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n\n")

# Round of 32 (after Round 1)
r32_count <- mid_tier_counts %>%
    filter(round == "Round of 32") %>%
    pull(n_mid_tier)

cat(sprintf("Round of 32 (after Round 1):\n"))
cat(sprintf("  Expected: %.2f 8-12 seeds\n", mean(r32_count)))
cat(sprintf(
    "  95%% CI: [%.2f, %.2f]\n",
    quantile(r32_count, 0.025), quantile(r32_count, 0.975)
))

# Sweet 16 (after Round 2)
s16_count <- mid_tier_counts %>%
    filter(round == "Sweet 16") %>%
    pull(n_mid_tier)

cat(sprintf("\nSweet 16 (after Round 2):\n"))
cat(sprintf("  Expected: %.2f 8-12 seeds\n", mean(s16_count)))
cat(sprintf(
    "  95%% CI: [%.2f, %.2f]\n",
    quantile(s16_count, 0.025), quantile(s16_count, 0.975)
))

# Elite 8
e8_count <- mid_tier_counts %>%
    filter(round == "Elite 8") %>%
    pull(n_mid_tier)

cat(sprintf("\nElite 8:\n"))
cat(sprintf("  Expected: %.2f 8-12 seeds\n", mean(e8_count)))
cat(sprintf(
    "  95%% CI: [%.2f, %.2f]\n",
    quantile(e8_count, 0.025), quantile(e8_count, 0.975)
))

# Final Four
f4_count <- mid_tier_counts %>%
    filter(round == "Final Four") %>%
    pull(n_mid_tier)

cat(sprintf("\nFinal Four:\n"))
cat(sprintf("  Expected: %.2f 8-12 seeds\n", mean(f4_count)))
cat(sprintf(
    "  95%% CI: [%.2f, %.2f]\n",
    quantile(f4_count, 0.025), quantile(f4_count, 0.975)
))

# Championship
champ_count <- mid_tier_counts %>%
    filter(round == "Championship") %>%
    pull(n_mid_tier)

cat(sprintf("\nChampionship:\n"))
cat(sprintf("  Expected: %.2f 8-12 seeds\n", mean(champ_count)))
cat(sprintf(
    "  Probability at least one: %.2f%%\n",
    mean(champ_count > 0) * 100
))

# =============================================================================
# 7. Conditional Probabilities from Simulation
# =============================================================================

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("RESEARCH QUESTION 2: Conditional Probabilities (from simulation)\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n\n")

# For each simulation, track if 8-12 seed reached each round
progression_tracking <- all_simulations %>%
    filter(winner_seed >= 8 & winner_seed <= 12) %>%
    select(sim_id, round, winner_seed, winner_name) %>%
    distinct()

# Create indicators for reaching each round
round_progression <- progression_tracking %>%
    mutate(round = factor(round, levels = round_order)) %>%
    complete(sim_id, round, fill = list(reached = FALSE)) %>%
    group_by(sim_id, round) %>%
    summarise(reached = n() > 0, .groups = "drop") %>%
    pivot_wider(names_from = round, values_from = reached, values_fill = FALSE)

# Calculate conditional probabilities
# P(Round B | Round A) = P(reached B and A) / P(reached A)
n_reached_sweet16 <- sum(round_progression$`Sweet 16`)
n_reached_elite8 <- sum(round_progression$`Elite 8`)
n_reached_final4 <- sum(round_progression$`Final Four`)
n_reached_champ <- sum(round_progression$Championship)

cat("Given that an 8-12 seed reaches the Sweet 16:\n\n")
cat(sprintf(
    "  P(Elite 8 | Sweet 16) = %.2f%%\n",
    (n_reached_elite8 / n_reached_sweet16) * 100
))
cat(sprintf(
    "  P(Final Four | Sweet 16) = %.2f%%\n",
    (n_reached_final4 / n_reached_sweet16) * 100
))
cat(sprintf(
    "  P(Championship | Sweet 16) = %.2f%%\n",
    (n_reached_champ / n_reached_sweet16) * 100
))

# Championship winners
championship_winners <- all_simulations %>%
    filter(round == "Championship") %>%
    select(sim_id, winner_seed, winner_name)

mid_tier_championships <- championship_winners %>%
    filter(winner_seed >= 8 & winner_seed <= 12)

cat(sprintf("\nOut of %d simulations:\n", N_SIMS))
cat(sprintf(
    "  8-12 seed won championship: %d times (%.2f%%)\n",
    nrow(mid_tier_championships),
    (nrow(mid_tier_championships) / N_SIMS) * 100
))

# =============================================================================
# 8. Analyze Performance by Individual Seed
# =============================================================================

cat("\nAnalyzing performance by individual seed (8-12)...\n")

seed_performance <- all_simulations %>%
    filter(winner_seed >= 8 & winner_seed <= 12) %>%
    mutate(round = factor(round, levels = round_order)) %>%
    group_by(winner_seed, round) %>%
    summarise(n_reached = n(), .groups = "drop") %>%
    mutate(
        pct_of_sims = (n_reached / N_SIMS) * 100,
        # Approximate number per seed (4 teams of each seed in tournament)
        expected_per_seed = n_reached / (N_SIMS * 4)
    )

cat("\nRound advancement by seed:\n")
print(seed_performance %>%
    select(winner_seed, round, n_reached, pct_of_sims))

# =============================================================================
# 9. Save Simulation Results
# =============================================================================

cat("\nSaving simulation results...\n")

# Save all simulations (large file)
saveRDS(all_simulations, file = here("results", "tables", "all_simulations.rds"))

# Save summary statistics
saveRDS(mid_tier_summary, file = here("results", "tables", "simulation_summary.rds"))
write_csv(mid_tier_summary, file = here("results", "tables", "simulation_summary.csv"))

# Save seed-specific performance
saveRDS(seed_performance, file = here("results", "tables", "seed_performance_sim.rds"))
write_csv(seed_performance, file = here("results", "tables", "seed_performance_sim.csv"))

# Save championship winners
write_csv(championship_winners,
    file = here("results", "tables", "championship_winners.csv")
)

# Save mid-tier counts by simulation
write_csv(mid_tier_counts,
    file = here("results", "tables", "mid_tier_counts_by_sim.csv")
)

# =============================================================================
# 10. Generate Simulation Summary
# =============================================================================

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("TOURNAMENT SIMULATION SUMMARY\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n\n")

cat(sprintf("Number of simulations: %d\n", N_SIMS))
cat(sprintf("Total games simulated: %d\n", nrow(all_simulations)))

cat("\nKey Findings:\n")
cat(sprintf("1. Expected 8-12 seeds in Sweet 16: %.2f\n", mean(s16_count)))
cat(sprintf("2. Expected 8-12 seeds in Final Four: %.2f\n", mean(f4_count)))
cat(sprintf(
    "3. Probability 8-12 seed wins title: %.2f%%\n",
    (nrow(mid_tier_championships) / N_SIMS) * 100
))

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("✓ Tournament simulation complete!\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n")

# =============================================================================
# NOTES:
# - Simulations assume team strength constant throughout tournament
# - Does not model injuries, fatigue, or momentum effects
# - Bracket structure simplified - actual brackets have specific regional placements
# - Could extend to include uncertainty in lambda estimates
# =============================================================================
