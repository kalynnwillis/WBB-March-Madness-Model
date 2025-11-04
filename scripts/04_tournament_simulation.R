# =============================================================================
# Script 04: Tournament Simulation
# Purpose: Monte Carlo simulation of March Madness tournament outcomes
#

library(tidyverse)
library(here)

source(here("scripts", "00_helper_functions.R"))

dir_create_safe("results", "tables")

team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))
win_probs <- readRDS(here("data", "processed", "win_probability_matrix.rds"))

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

seed_means <- tapply(tmp$lambda, tmp$seed, mean, na.rm = TRUE)
for (rr in rownames(lambda_mat)) {
    for (ss in colnames(lambda_mat)) {
        if (is.na(lambda_mat[rr, ss])) {
            lambda_mat[rr, ss] <- seed_means[[ss]]
            team_mat[rr, ss] <- paste0("Seed_", ss, "_", rr)
        }
    }
}

get_lambda_fast <- function(seed, region) lambda_mat[region, as.character(seed)]
get_team_fast <- function(seed, region) team_mat[region, as.character(seed)]
inv_logit <- function(x) 1 / (1 + exp(-x))


# Define Tournament Structure

# Standard NCAA Women's Tournament bracket
# 64 teams, 4 regions, single elimination

create_bracket_structure <- function() {
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


# Fast Vectorized Tournament Simulation Functions

# Simulate a single game
simulate_game <- function(team1_lambda, team2_lambda) {
    p <- inv_logit(team1_lambda - team2_lambda)
    p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
    rbinom(length(p), size = 1, prob = p)
}

# Simulate an entire tournament for ONE simulation, using only base vectors
simulate_tournament_fast <- function(seed_val = NULL) {
    if (!is.null(seed_val)) set.seed(seed_val)

    hi <- c(1, 2, 3, 4, 5, 6, 7, 8)
    lo <- c(16, 15, 14, 13, 12, 11, 10, 9)

    out_rows <- list()

    # Do each region independently
    for (rr in rownames(lambda_mat)) {
        team1_l <- get_lambda_fast(hi, rr)
        team2_l <- get_lambda_fast(lo, rr)
        team1_n <- get_team_fast(hi, rr)
        team2_n <- get_team_fast(lo, rr)

        win1 <- simulate_game(team1_l, team2_l)
        r64_w_seed <- ifelse(win1 == 1, hi, lo)
        r64_w_l <- ifelse(win1 == 1, team1_l, team2_l)
        r64_w_n <- ifelse(win1 == 1, team1_n, team2_n)

        out_rows[[length(out_rows) + 1]] <- tibble(
            round = "Round of 64", region = rr, matchup_id = seq_along(hi),
            team1_seed = hi, team2_seed = lo,
            winner_seed = r64_w_seed, winner_lambda = r64_w_l, winner_name = r64_w_n
        )

        idx32 <- matrix(r64_w_seed, nrow = 2, byrow = TRUE)
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

# Run Monte Carlo Simulations

library(parallel)

N_SIMS <- 5000
N_CORES <- tryCatch(
    {
        cores <- detectCores()
        if (is.na(cores) || cores < 1) 1 else max(1, cores - 1)
    },
    error = function(e) 1
)

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

rm(simulation_results)
gc(verbose = FALSE)

#  Analyze 8-12 Seed Performance Across Simulations

round_order <- c(
    "Round of 64", "Round of 32", "Sweet 16",
    "Elite 8", "Final Four", "Championship"
)

# Count 8-12 seeds in each round for each simulation
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

# Answer Research Question 1: Expected Advancement

# Round of 32 (after Round 1)
r32_count <- mid_tier_counts %>%
    filter(round == "Round of 32") %>%
    pull(n_mid_tier)

# Sweet 16 (after Round 2)
s16_count <- mid_tier_counts %>%
    filter(round == "Sweet 16") %>%
    pull(n_mid_tier)

# Elite 8
e8_count <- mid_tier_counts %>%
    filter(round == "Elite 8") %>%
    pull(n_mid_tier)

# Final Four
f4_count <- mid_tier_counts %>%
    filter(round == "Final Four") %>%
    pull(n_mid_tier)

# Championship
champ_count <- mid_tier_counts %>%
    filter(round == "Championship") %>%
    pull(n_mid_tier)

# Conditional Probabilities from Simulation

# For each simulation, track if 8-12 seed reached each round
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


# Championship winners
championship_winners <- all_simulations %>%
    filter(round == "Championship") %>%
    select(sim_id, winner_seed, winner_name)

mid_tier_championships <- championship_winners %>%
    filter(winner_seed >= 8 & winner_seed <= 12)

# Analyze Performance by Individual Seed

seed_performance <- all_simulations %>%
    filter(winner_seed >= 8 & winner_seed <= 12) %>%
    mutate(round = factor(round, levels = round_order)) %>%
    group_by(winner_seed, round) %>%
    summarise(n_reached = n(), .groups = "drop") %>%
    mutate(
        pct_of_sims = (n_reached / N_SIMS) * 100,
        expected_per_seed = n_reached / (N_SIMS * 4)
    )


# Save Simulation Results

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
