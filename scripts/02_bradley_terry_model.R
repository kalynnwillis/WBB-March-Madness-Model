# =============================================================================
# Script 02: Bradley-Terry Model Fitting (STREAMLINED)
# Purpose: Fit Bradley-Terry models to estimate team strengths
# =============================================================================

library(tidyverse)
library(here)

if (!require("BradleyTerry2")) {
    install.packages("BradleyTerry2")
    library(BradleyTerry2)
}


# =============================================================================
# Load Data
# =============================================================================

bt_data <- readRDS(here("data", "processed", "bt_data.rds"))
tournament_seeds <- readRDS(here("data", "processed", "tournament_seeds.rds"))

# =============================================================================
# Fit Models by Season
# =============================================================================

seasons <- sort(unique(bt_data$season))

all_season_abilities <- list()

for (season_year in seasons) {
    bt_season <- bt_data %>% filter(season == season_year)

    # Fit Bradley-Terry model
    bt_model_season <- BradleyTerry2::BTm(
        outcome = cbind(home.wins, away.wins),
        player1 = home.team,
        player2 = away.team,
        formula = ~ team + home_adv_bar,
        id = "team",
        contrasts = list(team = "contr.sum"),
        data = bt_season
    )

    # Check convergence
    if (!bt_model_season$converged) {
        stop(sprintf(
            "Model FAILED TO CONVERGE for %d season after %d iterations.",
            season_year, bt_model_season$iter
        ))
    }

    # Extract team abilities
    abilities_season <- BradleyTerry2::BTabilities(bt_model_season)
    abilities_df <- as.data.frame(abilities_season) %>%
        rownames_to_column(var = "team") %>%
        as_tibble() %>%
        rename(lambda = ability, se = s.e.) %>%
        mutate(season = season_year)

    all_season_abilities[[as.character(season_year)]] <- abilities_df
}

# =============================================================================
# Aggregate Across Seasons
# =============================================================================

all_abilities <- bind_rows(all_season_abilities)

# Z-score within season
all_abilities_scaled <- all_abilities %>%
    group_by(season) %>%
    mutate(
        season_sd = sd(lambda, na.rm = TRUE),
        season_mean = mean(lambda, na.rm = TRUE),
        lambda_z = (lambda - season_mean) / season_sd,
        se_z = se / season_sd,
        se_z = pmax(se_z, 1e-6)
    ) %>%
    ungroup()

# Aggregate using inverse-variance weighting
team_abilities_df <- all_abilities_scaled %>%
    group_by(team) %>%
    summarise(
        lambda = weighted.mean(lambda_z, w = 1 / (se_z^2), na.rm = TRUE),
        se = sqrt(1 / sum(1 / (se_z^2), na.rm = TRUE)),
        n_seasons = n(),
        seasons_played = paste(season, collapse = ", "),
        .groups = "drop"
    ) %>%
    arrange(desc(lambda))

# =============================================================================
# Compute Win Probability Matrix (Neutral Site)
# =============================================================================

lambda_vec <- setNames(team_abilities_df$lambda, team_abilities_df$team)
lambda_diff <- outer(X = lambda_vec, Y = lambda_vec, FUN = "-")
win_probs <- 1 / (1 + exp(-1 * lambda_diff))
win_probs <- pmin(pmax(win_probs, 1e-6), 1 - 1e-6)
diag(win_probs) <- NA

# =============================================================================
# Add Tournament Seeding
# =============================================================================

team_abilities_with_seeds <- team_abilities_df %>%
    left_join(
        tournament_seeds %>% select(team, seed, region),
        by = "team"
    ) %>%
    mutate(
        is_mid_tier_seed = !is.na(seed) & seed >= 8 & seed <= 12,
        seed_category = case_when(
            is.na(seed) ~ "Non-tournament",
            seed <= 4 ~ "1-4 seeds",
            seed <= 7 ~ "5-7 seeds",
            seed <= 12 ~ "8-12 seeds",
            TRUE ~ "13-16 seeds"
        )
    )

# Show top seeds
top_seeds <- team_abilities_with_seeds %>%
    filter(seed == 1) %>%
    select(team, lambda, seed, region) %>%
    arrange(region)

# =============================================================================
# Summary Statistics
# =============================================================================

seed_summary <- team_abilities_with_seeds %>%
    filter(!is.na(seed)) %>%
    group_by(seed_category) %>%
    summarise(
        n_teams = n(),
        mean_lambda = mean(lambda),
        median_lambda = median(lambda),
        sd_lambda = sd(lambda),
        .groups = "drop"
    ) %>%
    arrange(desc(mean_lambda))

print(seed_summary)

mid_tier <- team_abilities_with_seeds %>%
    filter(is_mid_tier_seed) %>%
    select(team, seed, lambda, se) %>%
    arrange(seed, desc(lambda))

print(mid_tier, n = 20)

# =============================================================================
# Save Results
# =============================================================================

bt_model <- bt_model_season

saveRDS(bt_model, here("data", "processed", "bt_model.rds"))
saveRDS(all_abilities, here("data", "processed", "team_abilities_by_season.rds"))
saveRDS(team_abilities_df, here("data", "processed", "team_abilities.rds"))
saveRDS(team_abilities_with_seeds, here("data", "processed", "team_abilities_with_seeds.rds"))
saveRDS(win_probs, here("data", "processed", "win_probability_matrix.rds"))

write_csv(team_abilities_df, here("data", "processed", "team_abilities.csv"))
write_csv(team_abilities_with_seeds, here("data", "processed", "team_abilities_with_seeds.csv"))
