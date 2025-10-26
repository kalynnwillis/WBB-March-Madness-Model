# =============================================================================
# Script 02: Bradley-Terry Model Fitting
# Purpose: Fit Bradley-Terry models to estimate team strengths
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)

# Install BradleyTerry2 if not already installed
if (!require("BradleyTerry2")) {
    devtools::install_github("hturner/BradleyTerry2")
    library(BradleyTerry2)
}

# =============================================================================
# 1. Load Processed Data
# =============================================================================

cat("Loading processed data...\n")

bt_data <- readRDS(here("data", "processed", "bt_data.rds"))
games_cleaned <- readRDS(here("data", "processed", "games_cleaned.rds"))
tournament_seeds <- readRDS(here("data", "processed", "tournament_seeds.rds"))

cat(sprintf("Loaded %d unique matchups\n", nrow(bt_data)))
cat(sprintf("Loaded %d games\n", nrow(games_cleaned)))
cat(sprintf("Loaded %d tournament teams\n", nrow(tournament_seeds)))

# =============================================================================
# 2. Fit Basic Bradley-Terry Model
# =============================================================================

cat("\nFitting Bradley-Terry model...\n")

# Get unique teams for factor levels
unique_teams <- sort(unique(c(as.character(bt_data$home.team), as.character(bt_data$away.team))))

# Select a reference team (typically choose a mid-level team)
# We'll use the first team alphabetically as default
reference_team <- as.character(unique_teams[1])
cat(sprintf("Using reference team: %s\n", reference_team))

# Fit the model
bt_model <- BradleyTerry2::BTm(
    outcome = cbind(home.wins, away.wins),
    player1 = home.team,
    player2 = away.team,
    refcat = reference_team,
    data = bt_data
)

cat("✓ Model fitted successfully\n")

# =============================================================================
# 3. Extract Team Abilities (Lambda Values)
# =============================================================================

cat("\nExtracting team abilities...\n")

# Get estimated abilities and standard errors
team_abilities <- BradleyTerry2::BTabilities(bt_model)

# Convert to data frame for easier manipulation
team_abilities_df <- as.data.frame(team_abilities) %>%
    rownames_to_column(var = "team") %>%
    as_tibble() %>%
    rename(lambda = ability, se = s.e.) %>%
    arrange(desc(lambda))

cat(sprintf("Estimated abilities for %d teams\n", nrow(team_abilities_df)))

# Display top 10 teams
cat("\nTop 10 strongest teams (by estimated lambda):\n")
print(team_abilities_df %>% head(10), n = 10)

# Display bottom 10 teams
cat("\nBottom 10 teams (by estimated lambda):\n")
print(team_abilities_df %>% tail(10), n = 10)

# =============================================================================
# 4. Compute Win Probabilities Matrix
# =============================================================================

cat("\nComputing pairwise win probabilities...\n")

# Extract lambda values as a named vector
lambda_vec <- setNames(team_abilities_df$lambda, team_abilities_df$team)

# Compute all pairwise differences (log-odds scale)
lambda_diff <- outer(
    X = lambda_vec,
    Y = lambda_vec,
    FUN = "-"
)

# Transform to probability scale using inverse logit
win_probs <- 1 / (1 + exp(-1 * lambda_diff))
diag(win_probs) <- NA # Teams don't play themselves

cat("✓ Win probability matrix computed\n")

# =============================================================================
# 5. Add Tournament Seeding Information to Team Abilities
# =============================================================================

cat("\nMerging tournament seeding information...\n")

# Join team abilities with tournament seeds
team_abilities_with_seeds <- team_abilities_df %>%
    left_join(
        tournament_seeds %>% select(team, seed, region),
        by = "team"
    ) %>%
    mutate(
        # Identify 8-12 seed teams
        is_mid_tier_seed = !is.na(seed) & seed >= 8 & seed <= 12,
        # Categorize all teams by seed range
        seed_category = case_when(
            is.na(seed) ~ "Non-tournament",
            seed <= 4 ~ "1-4 seeds",
            seed <= 7 ~ "5-7 seeds",
            seed <= 12 ~ "8-12 seeds",
            TRUE ~ "13-16 seeds"
        )
    )

# Summary statistics by seed category
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

cat("\nTeam abilities by seed category:\n")
print(seed_ability_summary)

# =============================================================================
# 6. Compute Expected Win Probabilities by Seed Matchups
# =============================================================================

cat("\nComputing win probabilities for common seed matchups...\n")

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

cat("\nExpected first-round win probabilities:\n")
print(matchup_probs)

# =============================================================================
# 7. Model Diagnostics and Validation
# =============================================================================

cat("\nPerforming model diagnostics...\n")

# Extract model summary
model_summary <- summary(bt_model)

# Calculate overall model fit statistics
cat("\nModel convergence: ", bt_model$converged, "\n")
cat("Number of iterations: ", bt_model$iter, "\n")

# Predict outcomes for actual games and compare
predictions <- games_cleaned %>%
    filter(
        home_team %in% team_abilities_df$team,
        away_team %in% team_abilities_df$team
    ) %>%
    rowwise() %>%
    mutate(
        home_lambda = team_abilities_df$lambda[team_abilities_df$team == home_team],
        away_lambda = team_abilities_df$lambda[team_abilities_df$team == away_team],
        pred_prob_home_wins = 1 / (1 + exp(-(home_lambda - away_lambda))),
        pred_home_wins = if_else(pred_prob_home_wins > 0.5, 1, 0),
        correct_prediction = (pred_home_wins == home_winner)
    ) %>%
    ungroup()

# Calculate prediction accuracy
accuracy <- mean(predictions$correct_prediction, na.rm = TRUE)
cat(sprintf("\nPrediction accuracy on training data: %.2f%%\n", accuracy * 100))

# Calculate log-loss (Brier score)
log_loss <- -mean(
    predictions$home_winner * log(predictions$pred_prob_home_wins) +
        (1 - predictions$home_winner) * log(1 - predictions$pred_prob_home_wins),
    na.rm = TRUE
)
cat(sprintf("Log-loss: %.4f\n", log_loss))

# =============================================================================
# 8. Save Model Results
# =============================================================================

cat("\nSaving model results...\n")

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

# =============================================================================
# 9. Generate Model Summary Report
# =============================================================================

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("BRADLEY-TERRY MODEL SUMMARY\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n\n")

cat(sprintf("Reference team: %s\n", reference_team))
cat(sprintf("Teams in model: %d\n", nrow(team_abilities_df)))
cat(sprintf("Model converged: %s\n", bt_model$converged))
cat(sprintf("Prediction accuracy: %.2f%%\n", accuracy * 100))
cat(sprintf("Log-loss: %.4f\n", log_loss))

cat("\nStrongest teams:\n")
print(team_abilities_df %>% head(5))

cat("\n8-12 seed teams in dataset:\n")
print(team_abilities_with_seeds %>%
    filter(is_mid_tier_seed) %>%
    select(team, seed, lambda, se) %>%
    arrange(seed, desc(lambda)))

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("✓ Bradley-Terry model fitting complete!\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n")

# =============================================================================
# NOTES:
# - Consider fitting separate models by season if team compositions change
# - Could add home court advantage term in extended model
# - May want to validate on held-out test set
# - Consider robust standard errors for inference
# =============================================================================
