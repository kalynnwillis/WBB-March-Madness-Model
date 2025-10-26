# =============================================================================
# Script 04: Tournament Simulation
# Purpose: Monte Carlo simulation of March Madness tournament outcomes
#
# PERFORMANCE OPTIMIZATIONS APPLIED:
# - Fast matrix-based lookups (no dplyr in hot path)
# - Vectorized simulation functions (no rowwise operations)
# - Parallel execution using mclapply (multi-core)
# - Expected speedup: 10-30x faster than original implementation
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)

# Load helper functions
source(here("scripts", "00_helper_functions.R"))

dir_create_safe("results", "tables")

# =============================================================================
# 1. Load Model Results and Data
# =============================================================================

cat("Loading model results...\n")

team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))
win_probs <- readRDS(here("data", "processed", "win_probability_matrix.rds"))

cat("✓ Data loaded successfully\n")

# =============================================================================
# 1b. Build Fast Lookup Tables
# =============================================================================

cat("Building fast lookup tables...\n")

# ---- FAST LOOKUPS ----
# We only need (seed, region) -> (team, lambda). Build tiny matrices.
regions <- sort(unique(team_abilities$region))
if (length(regions) != 4) message("Note: found ", length(regions), " regions; using whatever is present.")

lambda_mat <- matrix(NA_real_,
    nrow = length(regions), ncol = 16,
    dimnames = list(regions, as.character(1:16))
)
team_mat <- matrix(NA_character_,
    nrow = length(regions), ncol = 16,
    dimnames = list(regions, as.character(1:16))
)

# Fill from your seeded bracket (one team per seed/region if present)
tmp <- team_abilities %>%
    select(region, seed, team, lambda) %>%
    arrange(region, seed)

for (i in seq_len(nrow(tmp))) {
    rr <- as.character(tmp$region[i])
    ss <- as.character(tmp$seed[i])
    if (ss %in% colnames(lambda_mat) && rr %in% rownames(lambda_mat) && is.na(lambda_mat[rr, ss])) {
        lambda_mat[rr, ss] <- tmp$lambda[i]
        team_mat[rr, ss] <- tmp$team[i]
    }
}

# Fallbacks for any missing (seed,region): use seed-wise average across regions
seed_means <- tapply(tmp$lambda, tmp$seed, mean, na.rm = TRUE)
for (rr in rownames(lambda_mat)) {
    for (ss in colnames(lambda_mat)) {
        if (is.na(lambda_mat[rr, ss])) {
            lambda_mat[rr, ss] <- seed_means[[ss]]
            team_mat[rr, ss] <- paste0("Seed_", ss, "_", rr)
        }
    }
}

# Small helpers (no dplyr):
get_lambda_fast <- function(seed, region) lambda_mat[region, as.character(seed)]
get_team_fast <- function(seed, region) team_mat[region, as.character(seed)]
inv_logit <- function(x) 1 / (1 + exp(-x))

cat("✓ Fast lookup tables built\n")

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
# 3. Fast Vectorized Tournament Simulation Functions
# =============================================================================

# Simulate a single game (vectorized-safe)
simulate_game <- function(team1_lambda, team2_lambda) {
    p <- inv_logit(team1_lambda - team2_lambda)
    # Clip to avoid numeric 0/1 (prevents warnings and stabilizes summaries)
    p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
    rbinom(length(p), size = 1, prob = p)
}

# Simulate an entire tournament for ONE simulation, using only base vectors
simulate_tournament_fast <- function(seed_val = NULL) {
    if (!is.null(seed_val)) set.seed(seed_val)

    # Round 1 pairings are fixed per region
    hi <- c(1, 2, 3, 4, 5, 6, 7, 8)
    lo <- c(16, 15, 14, 13, 12, 11, 10, 9)

    out_rows <- list()

    # Do each region independently
    for (rr in rownames(lambda_mat)) {
        # ----- Round of 64 -----
        team1_l <- get_lambda_fast(hi, rr)
        team2_l <- get_lambda_fast(lo, rr)
        team1_n <- get_team_fast(hi, rr)
        team2_n <- get_team_fast(lo, rr)

        win1 <- simulate_game(team1_l, team2_l) # length 8
        r64_w_seed <- ifelse(win1 == 1, hi, lo)
        r64_w_l <- ifelse(win1 == 1, team1_l, team2_l)
        r64_w_n <- ifelse(win1 == 1, team1_n, team2_n)

        out_rows[[length(out_rows) + 1]] <- tibble(
            round = "Round of 64", region = rr, matchup_id = seq_along(hi),
            team1_seed = hi, team2_seed = lo,
            winner_seed = r64_w_seed, winner_lambda = r64_w_l, winner_name = r64_w_n
        )

        # ----- Round of 32 -----
        idx32 <- matrix(r64_w_seed, nrow = 2, byrow = TRUE) # (1v16 vs 8v9), etc.
        lam32 <- matrix(r64_w_l, nrow = 2, byrow = TRUE)
        nam32 <- matrix(r64_w_n, nrow = 2, byrow = TRUE)

        win2 <- simulate_game(lam32[1, ], lam32[2, ])
        r32_w_seed <- ifelse(win2 == 1, idx32[1, ], idx32[2, ])
        r32_w_l <- ifelse(win2 == 1, lam32[1, ], lam32[2, ])
        r32_w_n <- ifelse(win2 == 1, nam32[1, ], nam32[2, ])

        out_rows[[length(out_rows) + 1]] <- tibble(
            round = "Round of 32", region = rr, matchup_id = 1:4,
            team1_seed = idx32[1, ], team2_seed = idx32[2, ],
            winner_seed = r32_w_seed, winner_lambda = r32_w_l, winner_name = r32_w_n
        )

        # ----- Sweet 16 -----
        idx16 <- matrix(r32_w_seed, nrow = 2, byrow = TRUE)
        lam16 <- matrix(r32_w_l, nrow = 2, byrow = TRUE)
        nam16 <- matrix(r32_w_n, nrow = 2, byrow = TRUE)

        win3 <- simulate_game(lam16[1, ], lam16[2, ])
        r16_w_seed <- ifelse(win3 == 1, idx16[1, ], idx16[2, ])
        r16_w_l <- ifelse(win3 == 1, lam16[1, ], lam16[2, ])
        r16_w_n <- ifelse(win3 == 1, nam16[1, ], nam16[2, ])

        out_rows[[length(out_rows) + 1]] <- tibble(
            round = "Sweet 16", region = rr, matchup_id = 1:2,
            team1_seed = idx16[1, ], team2_seed = idx16[2, ],
            winner_seed = r16_w_seed, winner_lambda = r16_w_l, winner_name = r16_w_n
        )

        # ----- Elite 8 -----
        idx8 <- c(r16_w_seed[1], r16_w_seed[2])
        lam8 <- c(r16_w_l[1], r16_w_l[2])
        nam8 <- c(r16_w_n[1], r16_w_n[2])

        win4 <- simulate_game(lam8[1], lam8[2])
        e8_w_seed <- ifelse(win4 == 1, idx8[1], idx8[2])
        e8_w_l <- ifelse(win4 == 1, lam8[1], lam8[2])
        e8_w_n <- ifelse(win4 == 1, nam8[1], nam8[2])

        out_rows[[length(out_rows) + 1]] <- tibble(
            round = "Elite 8", region = rr, matchup_id = 1L,
            team1_seed = idx8[1], team2_seed = idx8[2],
            winner_seed = e8_w_seed, winner_lambda = e8_w_l, winner_name = e8_w_n
        )
    }

    # Final Four (National semis) — pair region winners [1vs2], [3vs4]
    e8 <- bind_rows(out_rows) %>%
        filter(round == "Elite 8") %>%
        arrange(region)
    semi_pairs <- split(e8, rep(1:2, each = 2)) # 2 games

    ff_rows <- lapply(seq_along(semi_pairs), function(k) {
        g <- semi_pairs[[k]]
        lam <- g$winner_lambda
        nam <- g$winner_name
        sed <- g$winner_seed
        win <- simulate_game(lam[1], lam[2])
        tibble(
            round = "Final Four", region = "National", matchup_id = k,
            team1_seed = sed[1], team2_seed = sed[2],
            winner_seed = ifelse(win == 1, sed[1], sed[2]),
            winner_lambda = ifelse(win == 1, lam[1], lam[2]),
            winner_name = ifelse(win == 1, nam[1], nam[2])
        )
    })

    # Championship
    ff <- bind_rows(ff_rows) %>% arrange(matchup_id)
    lamc <- ff$winner_lambda
    namc <- ff$winner_name
    sedc <- ff$winner_seed
    wfin <- simulate_game(lamc[1], lamc[2])
    champ <- tibble(
        round = "Championship", region = "National", matchup_id = 1L,
        team1_seed = sedc[1], team2_seed = sedc[2],
        winner_seed = ifelse(wfin == 1, sedc[1], sedc[2]),
        winner_lambda = ifelse(wfin == 1, lamc[1], lamc[2]),
        winner_name = ifelse(wfin == 1, namc[1], namc[2])
    )

    bind_rows(out_rows, ff_rows, list(champ)) %>% bind_rows()
}

# =============================================================================
# 4. Run Monte Carlo Simulations (FAST & PARALLEL)
# =============================================================================

library(parallel)

N_SIMS <- 5000
N_CORES <- max(1, detectCores() - 1)

cat(sprintf("\nSimulating %d tournaments using %d cores...\n", N_SIMS, N_CORES))

# Each worker returns the same tibble shape you already use
simulation_results <- mclapply(
    X = 1:N_SIMS,
    FUN = function(sim) {
        simulate_tournament_fast(seed_val = 479 + sim) %>%
            mutate(sim_id = sim)
    },
    mc.cores = N_CORES,
    mc.preschedule = TRUE
)

all_simulations <- bind_rows(simulation_results)

# Clean up memory
rm(simulation_results)
gc(verbose = FALSE)

cat(sprintf("✓ Completed %d tournament simulations\n", N_SIMS))

# =============================================================================
# 5. Analyze 8-12 Seed Performance Across Simulations
# =============================================================================

cat("\nAnalyzing 8-12 seed performance across simulations...\n")

# Define round order (explicit for complete())
round_order <- c(
    "Round of 64", "Round of 32", "Sweet 16",
    "Elite 8", "Final Four", "Championship"
)

# Count 8-12 seeds in each round for each simulation
# Force all rounds into result with complete() to avoid NaN from missing rounds
mid_tier_counts <- all_simulations %>%
    mutate(round = factor(round, levels = round_order)) %>%
    group_by(sim_id, round) %>%
    summarise(n_mid_tier = sum(winner_seed >= 8 & winner_seed <= 12), .groups = "drop") %>%
    tidyr::complete(
        sim_id,
        round = factor(round_order, levels = round_order),
        fill = list(n_mid_tier = 0L)
    )

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

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("RESEARCH QUESTION 1: Expected 8-12 Seeds by Round\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

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

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("RESEARCH QUESTION 2: Conditional Probabilities (from simulation)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

# For each simulation, track if 8-12 seed reached each round
# FIX: Create hit indicator BEFORE complete() to avoid 100% bug
progression_tracking <- all_simulations %>%
    filter(winner_seed >= 8 & winner_seed <= 12) %>%
    transmute(sim_id, round = factor(round, levels = round_order), hit = TRUE)

# Complete grid with hit = FALSE for rounds not reached
round_progression <- progression_tracking %>%
    tidyr::complete(
        sim_id,
        round = factor(round_order, levels = round_order),
        fill = list(hit = FALSE)
    ) %>%
    group_by(sim_id, round) %>%
    summarise(reached = any(hit), .groups = "drop") %>%
    pivot_wider(names_from = round, values_from = reached, values_fill = FALSE)

# Calculate conditional probabilities
# P(Round B | Round A) = P(reached B and A) / P(reached A)
n_reached_sweet16 <- sum(round_progression$`Sweet 16`, na.rm = TRUE)
n_reached_elite8 <- sum(round_progression$`Elite 8`, na.rm = TRUE)
n_reached_final4 <- sum(round_progression$`Final Four`, na.rm = TRUE)
n_reached_champ <- sum(round_progression$Championship, na.rm = TRUE)

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

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("TOURNAMENT SIMULATION SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat(sprintf("Number of simulations: %d\n", N_SIMS))
cat(sprintf("Total games simulated: %d\n", nrow(all_simulations)))

cat("\nKey Findings:\n")
cat(sprintf("1. Expected 8-12 seeds in Sweet 16: %.2f\n", mean(s16_count)))
cat(sprintf("2. Expected 8-12 seeds in Final Four: %.2f\n", mean(f4_count)))
cat(sprintf(
    "3. Probability 8-12 seed wins title: %.2f%%\n",
    (nrow(mid_tier_championships) / N_SIMS) * 100
))

cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("✓ Tournament simulation complete!\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

# =============================================================================
# NOTES:
# - Simulations assume team strength constant throughout tournament
# - Does not model injuries, fatigue, or momentum effects
# - Bracket structure simplified - actual brackets have specific regional placements
# - Could extend to include uncertainty in lambda estimates
# =============================================================================
