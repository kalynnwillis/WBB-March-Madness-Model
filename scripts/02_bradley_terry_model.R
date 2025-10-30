# =============================================================================
# Script 02: Bradley-Terry Model Fitting
# Purpose: Fit Bradley-Terry models to estimate team strengths
# =============================================================================

library(tidyverse)
library(here)

if (!require("BradleyTerry2")) {
    install.packages("BradleyTerry2")
    library(BradleyTerry2)
}

bt_data <- readRDS(here("data", "processed", "bt_data.rds"))
games_cleaned <- readRDS(here("data", "processed", "games_cleaned.rds"))
tournament_seeds <- readRDS(here("data", "processed", "tournament_seeds.rds"))

# Fit separate models for each season
seasons <- sort(unique(bt_data$season))
cat(sprintf(
    "Fitting separate Bradley-Terry models for %d seasons: %s\n",
    length(seasons), paste(seasons, collapse = ", ")
))

all_season_abilities <- list()

for (season_year in seasons) {
    cat(sprintf("\n=== Fitting model for %d season ===\n", season_year))

    # Filter data for this season
    bt_season <- bt_data %>% filter(season == season_year)

    # Get unique teams for this season
    season_teams <- sort(unique(c(
        as.character(bt_season$home.team),
        as.character(bt_season$away.team)
    )))

    cat(sprintf(
        "  Teams: %d | Matchups: %d\n",
        length(season_teams), nrow(bt_season)
    ))

    # Fit Bradley-Terry model for this season
    bt_model_season <- BradleyTerry2::BTm(
        outcome = cbind(home.wins, away.wins),
        player1 = home.team,
        player2 = away.team,
        formula = ~ team + home_adv_bar,
        id = "team",
        contrasts = list(team = "contr.sum"),
        data = bt_season
    )

    # Extract abilities
    abilities_season <- BradleyTerry2::BTabilities(bt_model_season)
    abilities_df <- as.data.frame(abilities_season) %>%
        rownames_to_column(var = "team") %>%
        as_tibble() %>%
        rename(lambda = ability, se = s.e.) %>%
        mutate(season = season_year)

    all_season_abilities[[as.character(season_year)]] <- abilities_df

    cat(sprintf(
        "  Converged: %s | Iterations: %d\n",
        bt_model_season$converged, bt_model_season$iter
    ))

    # FIXED: Add convergence checks
    if (!bt_model_season$converged) {
        stop(sprintf(
            "Bradley-Terry model FAILED TO CONVERGE for %d season after %d iterations.\n%s",
            season_year, bt_model_season$iter,
            "Check for separation issues or reduce model complexity."
        ))
    }

    if (bt_model_season$iter > 50) {
        warning(sprintf(
            "Model for %d season took %d iterations (>50). May indicate convergence issues.",
            season_year, bt_model_season$iter
        ))
    }
}

# Combine all seasons' abilities
all_abilities <- bind_rows(all_season_abilities)

cat(sprintf("\n=== Combined results ===\n"))
cat(sprintf("Total team-season observations: %d\n", nrow(all_abilities)))

# Z-score within season to put all seasons on same scale (CRITICAL FIX)
# Each season's BT is identified up to a shift - need to standardize first
all_abilities_scaled <- all_abilities %>%
    group_by(season) %>%
    mutate(
        season_sd = sd(lambda, na.rm = TRUE),
        season_mean = mean(lambda, na.rm = TRUE),
        lambda_z = (lambda - season_mean) / season_sd,
        se_z = se / season_sd, # FIXED: Scale SE by same factor as lambda
        se_z = pmax(se_z, 1e-6) # Prevent division by zero
    ) %>%
    ungroup()

# Aggregate z-scored abilities across seasons using inverse-variance weighting
team_abilities_df <- all_abilities_scaled %>%
    group_by(team) %>%
    summarise(
        lambda = weighted.mean(lambda_z, w = 1 / (se_z^2), na.rm = TRUE),
        se = sqrt(1 / sum(1 / (se_z^2), na.rm = TRUE)), # Combined SE
        n_seasons = n(),
        seasons_played = paste(season, collapse = ", "),
        .groups = "drop"
    ) %>%
    arrange(desc(lambda))

cat(sprintf("Unique teams across all seasons: %d\n", nrow(team_abilities_df)))
cat(sprintf(
    "Teams appearing in all %d seasons: %d\n",
    length(seasons),
    sum(team_abilities_df$n_seasons == length(seasons))
))

# Season coverage sanity check
cat("\n=== Season Coverage ===\n")
season_coverage <- all_abilities %>%
    group_by(season) %>%
    summarise(
        n_teams = n(),
        min_games = NA, # Will need to compute from bt_data
        med_games = NA,
        .groups = "drop"
    )

# Compute games per team per season from bt_data
games_per_team_season <- bt_data %>%
    group_by(season, team = home.team) %>%
    summarise(n_home = sum(home.wins + away.wins), .groups = "drop") %>%
    full_join(
        bt_data %>%
            group_by(season, team = away.team) %>%
            summarise(n_away = sum(home.wins + away.wins), .groups = "drop"),
        by = c("season", "team")
    ) %>%
    mutate(
        n_home = replace_na(n_home, 0),
        n_away = replace_na(n_away, 0),
        total_games = n_home + n_away
    ) %>%
    group_by(season) %>%
    summarise(
        min_games = min(total_games, na.rm = TRUE),
        med_games = median(total_games, na.rm = TRUE),
        .groups = "drop"
    )

season_coverage <- season_coverage %>%
    left_join(games_per_team_season, by = "season")

print(season_coverage)

# Extract home advantage coefficient from ALL seasons (meta-estimate)
# Don't just use last season - get weighted average across all seasons
beta_home_df <- map_dfr(seasons, function(y) {
    m <- tryCatch(
        {
            BradleyTerry2::BTm(
                outcome = cbind(home.wins, away.wins),
                player1 = home.team,
                player2 = away.team,
                formula = ~ team + home_adv_bar,
                id = "team",
                contrasts = list(team = "contr.sum"),
                data = filter(bt_data, season == y)
            )
        },
        error = function(e) NULL
    )

    if (is.null(m)) {
        return(tibble(season = y, beta_home = 0, se = Inf))
    }

    co <- tryCatch(summary(m)$coefficients, error = function(e) NULL)
    if (!is.null(co) && "home_adv_bar" %in% rownames(co) && !is.na(co["home_adv_bar", "Estimate"])) {
        tibble(
            season = y,
            beta_home = co["home_adv_bar", "Estimate"],
            se = co["home_adv_bar", "Std. Error"]
        )
    } else {
        tibble(season = y, beta_home = 0, se = Inf)
    }
})

# Weighted mean of home advantage across seasons
# NOTE: Pair-level aggregation causes home_adv_bar to be near-constant or singular
# Result: beta_home = 0 or NA. Tournament is neutral-site anyway.
beta_home <- with(beta_home_df, {
    valid_idx <- is.finite(se) & se < 1e6
    if (sum(valid_idx) > 0) {
        weighted.mean(beta_home[valid_idx], w = 1 / se[valid_idx]^2, na.rm = TRUE)
    } else {
        0
    }
})
if (is.na(beta_home) || is.nan(beta_home) || !is.finite(beta_home)) beta_home <- 0

cat(sprintf("\n=== Home Advantage Estimate ===\n"))
cat(sprintf("Home advantage (log-odds): %.4f\n", beta_home))
cat("NOTE: Pair-level aggregation prevents proper home advantage inference.\n")
cat("      beta_home = 0 used (tournament is neutral-site).\n")
cat("      For accurate home advantage, refit using game-level BTM.\n")
print(beta_home_df)

# Keep last model for reference only
bt_model <- bt_model_season

# Save all season-specific abilities for potential future analysis
saveRDS(all_abilities, file = here("data", "processed", "team_abilities_by_season.rds"))

# Compute Win Probabilities Matrix (Neutral Site)

# Extract lambda values as a named vector
lambda_vec <- setNames(team_abilities_df$lambda, team_abilities_df$team)

# Compute all pairwise differences (log-odds scale)
lambda_diff <- outer(
    X = lambda_vec,
    Y = lambda_vec,
    FUN = "-"
)

# Transform to probability scale using inverse logit
win_probs_raw <- 1 / (1 + exp(-1 * lambda_diff))
win_probs <- pmin(pmax(win_probs_raw, 1e-6), 1 - 1e-6)
diag(win_probs) <- NA # Teams don't play themselves


# Add Tournament Seeding Information to Team Abilities

# Load tournament seeds (from Script 01 - either real NCAA or model-based)
tournament_seeds_from_01 <- readRDS(here("data", "processed", "tournament_seeds.rds"))

cat("\nUsing tournament seeds from Script 01\n")
cat(sprintf("  %d teams seeded\n", nrow(tournament_seeds_from_01)))

# Join team abilities with seeds
team_abilities_with_seeds <- team_abilities_df %>%
    left_join(
        tournament_seeds_from_01 %>% select(team, seed, region),
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

# Sanity checks
n_seeded <- sum(!is.na(team_abilities_with_seeds$seed))
cat(sprintf("✓ %d teams have tournament seeds assigned\n", n_seeded))

if (n_seeded > 0) {
    top_seeds <- team_abilities_with_seeds %>%
        filter(seed == 1) %>%
        select(team, lambda, seed, region)
    cat("Top seeds (seed=1) by region:\n")
    print(top_seeds)
}

seed_ability_summary <- team_abilities_with_seeds %>%
    filter(!is.na(seed)) %>%
    group_by(seed_category) %>%
    summarise(
        n_teams = n(),
        mean_lambda = mean(lambda),
        median_lambda = median(lambda),
        sd_lambda = sd(lambda),
        min_lambda = min(lambda),
        max_lambda = max(lambda)
    )


# Compute Expected Win Probabilities by Seed Matchups

# Define common first-round matchups
first_round_matchups <- tibble(
    higher_seed = c(1, 2, 3, 4, 5, 6, 7, 8),
    lower_seed = c(16, 15, 14, 13, 12, 11, 10, 9)
)

# Calculate average win probability for each matchup type
matchup_probs <- first_round_matchups %>%
    rowwise() %>%
    mutate(
        # Get teams with these seeds
        higher_teams = list(team_abilities_with_seeds %>%
            filter(seed == higher_seed) %>%
            pull(lambda)),
        lower_teams = list(team_abilities_with_seeds %>%
            filter(seed == lower_seed) %>%
            pull(lambda)),
        # Calculate average probability
        avg_prob_higher_wins = mean(
            sapply(higher_teams[[1]], function(h) {
                mean(sapply(lower_teams[[1]], function(l) {
                    1 / (1 + exp(-(h - l)))
                }))
            })
        )
    ) %>%
    select(higher_seed, lower_seed, avg_prob_higher_wins) %>%
    mutate(
        matchup = sprintf("%d vs %d", higher_seed, lower_seed),
        avg_prob_lower_wins = 1 - avg_prob_higher_wins
    )


# Extract model summary and home advantage coefficient
model_summary <- summary(bt_model)

# Extract home advantage coefficient (log-odds scale)
beta_home <- tryCatch(
    {
        coef(bt_model)["home_adv_bar"]
    },
    error = function(e) 0
)

if (is.na(beta_home)) beta_home <- 0

cat("\n=== Model Fit Statistics ===\n")
cat("Model convergence: ", bt_model$converged, "\n")
cat("Number of iterations: ", bt_model$iter, "\n")
cat(sprintf("Home advantage coefficient (log-odds): %.4f\n", beta_home))
if (beta_home != 0) {
    # Convert to probability scale for typical matchup
    prob_home_equal <- plogis(beta_home) # P(home wins | equal teams)
    cat(sprintf("  → For equal teams, home wins %.1f%% of the time\n", 100 * prob_home_equal))
}

# Predict outcomes for actual games and compare (USING HOME ADVANTAGE)
predictions <- games_cleaned %>%
    filter(
        home_team %in% team_abilities_df$team,
        away_team %in% team_abilities_df$team
    ) %>%
    mutate(
        home_lambda = team_abilities_df$lambda[match(home_team, team_abilities_df$team)],
        away_lambda = team_abilities_df$lambda[match(away_team, team_abilities_df$team)],
        # Include home advantage: logit(p) = lambda_home - lambda_away + beta_home * (1 - neutral)
        pred_prob_home_wins = plogis((home_lambda - away_lambda) + beta_home * (1 - neutral_site)),
        pred_home_wins = as.integer(pred_prob_home_wins > 0.5),
        correct_prediction = (pred_home_wins == home_winner)
    )

# Calculate prediction accuracy
accuracy <- mean(predictions$correct_prediction, na.rm = TRUE)

# Calculate log-loss (cross-entropy loss) with proper clamping
p_clamped <- pmax(pmin(predictions$pred_prob_home_wins, 1 - 1e-12), 1e-12)
log_loss <- -mean(
    predictions$home_winner * log(p_clamped) +
        (1 - predictions$home_winner) * log(1 - p_clamped),
    na.rm = TRUE
)

# FIXED: Add warning about in-sample bias
cat(sprintf("\nTraining Set Performance (OPTIMISTICALLY BIASED - in-sample):\n"))
cat(sprintf("  Accuracy: %.2f%%\n", 100 * accuracy))
cat(sprintf("  Log-loss: %.4f\n", log_loss))
cat("\nNOTE: These metrics are calculated on training data and will be optimistically biased.\n")
cat("      For unbiased performance estimates, see Script 08 (model_validation.R)\n")

# Save Model Results



# Save the fitted model object
saveRDS(bt_model, file = here("data", "processed", "bt_model.rds"))

# Save team abilities
saveRDS(team_abilities_df, file = here("data", "processed", "team_abilities.rds"))
write_csv(team_abilities_df, file = here("data", "processed", "team_abilities.csv"))

# Save team abilities with seeds
saveRDS(team_abilities_with_seeds,
    file = here("data", "processed", "team_abilities_with_seeds.rds")
)
write_csv(team_abilities_with_seeds,
    file = here("data", "processed", "team_abilities_with_seeds.csv")
)

# Save win probability matrix
saveRDS(win_probs, file = here("data", "processed", "win_probability_matrix.rds"))

# Save predictions with accuracy
saveRDS(predictions, file = here("data", "processed", "model_predictions.rds"))

# Save matchup probabilities
saveRDS(matchup_probs, file = here("data", "processed", "matchup_probabilities.rds"))
write_csv(matchup_probs, file = here("data", "processed", "matchup_probabilities.csv"))

# Note: tournament_seeds.rds is created by Script 01, not overwritten here

# Generate Model Summary Report

print(team_abilities_df %>% head(5))

print(team_abilities_with_seeds %>%
    filter(is_mid_tier_seed) %>%
    select(team, seed, lambda, se) %>%
    arrange(seed, desc(lambda)))
