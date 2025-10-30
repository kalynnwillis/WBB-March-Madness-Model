# =============================================================================
# Script 08: Model Validation
# Purpose: Holdout validation, calibration plots, and historical NCAA seed comparison
# =============================================================================

library(tidyverse)
library(here)
library(BradleyTerry2)
library(ggplot2)
library(patchwork)

source(here("scripts", "00_helper_functions.R"))

set.seed(479)

# =============================================================================
# 1. HOLDOUT VALIDATION: Train on 2019-2023, Test on 2024
# =============================================================================

cat(strrep("=", 60), "\n")
cat("HOLDOUT VALIDATION: 2024 as Test Set\n")
cat(strrep("=", 60), "\n\n")

# Load full data
games_cleaned <- readRDS(here("data", "processed", "games_cleaned.rds"))

# Split: Train = 2019, 2021-2023; Test = 2024
train_seasons <- c(2019, 2021, 2022, 2023)
test_season <- 2024

games_train <- games_cleaned %>% filter(season %in% train_seasons)
games_test <- games_cleaned %>% filter(season == test_season)

cat(sprintf(
    "Training: %d seasons (%s) - %d games\n",
    length(train_seasons), paste(train_seasons, collapse = ", "),
    nrow(games_train)
))
cat(sprintf("Testing: 1 season (%d) - %d games\n\n", test_season, nrow(games_test)))

# Build training BT data
bt_train <- games_train %>%
    group_by(season, home_team, away_team) %>%
    summarise(
        home.wins = sum(home_winner),
        away.wins = n() - sum(home_winner),
        total_games = n(),
        home_adv_bar = mean(1 - neutral_site),
        .groups = "drop"
    ) %>%
    mutate(
        home.team = factor(home_team),
        away.team = factor(away_team)
    )

# Fit separate models per training season and aggregate
train_seasons_used <- sort(unique(bt_train$season))
all_train_abilities <- list()

for (season_year in train_seasons_used) {
    cat(sprintf("Fitting model for %d season...\n", season_year))

    bt_season <- bt_train %>% filter(season == season_year)

    m <- BradleyTerry2::BTm(
        outcome = cbind(home.wins, away.wins),
        player1 = home.team,
        player2 = away.team,
        formula = ~ team + home_adv_bar,
        id = "team",
        contrasts = list(team = "contr.sum"),
        data = bt_season
    )

    abilities <- BradleyTerry2::BTabilities(m) %>%
        as.data.frame() %>%
        rownames_to_column(var = "team") %>%
        rename(lambda = ability, se = s.e.) %>%
        mutate(season = season_year)

    all_train_abilities[[as.character(season_year)]] <- abilities
}

# Z-score and aggregate
all_train_combined <- bind_rows(all_train_abilities)

train_abilities_scaled <- all_train_combined %>%
    group_by(season) %>%
    mutate(
        season_sd = sd(lambda, na.rm = TRUE),
        season_mean = mean(lambda, na.rm = TRUE),
        lambda_z = (lambda - season_mean) / season_sd,
        se_z = se / season_sd, # FIXED: Scale SE by same factor as lambda
        se_z = pmax(se_z, 1e-6)
    ) %>%
    ungroup()

train_abilities_final <- train_abilities_scaled %>%
    group_by(team) %>%
    summarise(
        lambda = weighted.mean(lambda_z, w = 1 / se_z^2, na.rm = TRUE),
        se = sqrt(1 / sum(1 / se_z^2, na.rm = TRUE)),
        n_seasons = n(),
        .groups = "drop"
    )

cat(sprintf("\nTrained on %d unique teams\n", nrow(train_abilities_final)))

# Extract home advantage from training data
beta_home_train <- 0 # Default (neutral sites)

# Test set predictions
test_predictions <- games_test %>%
    filter(
        home_team %in% train_abilities_final$team,
        away_team %in% train_abilities_final$team
    ) %>%
    left_join(train_abilities_final %>% select(team, lambda),
        by = c("home_team" = "team")
    ) %>%
    rename(home_lambda = lambda) %>%
    left_join(train_abilities_final %>% select(team, lambda),
        by = c("away_team" = "team")
    ) %>%
    rename(away_lambda = lambda) %>%
    mutate(
        pred_prob = plogis((home_lambda - away_lambda) + beta_home_train * (1 - neutral_site)),
        pred_prob = pmax(pmin(pred_prob, 1 - 1e-12), 1e-12),
        pred_winner = as.integer(pred_prob > 0.5),
        correct = (pred_winner == home_winner)
    )

# Holdout performance
holdout_accuracy <- mean(test_predictions$correct, na.rm = TRUE)
holdout_logloss <- -mean(
    test_predictions$home_winner * log(test_predictions$pred_prob) +
        (1 - test_predictions$home_winner) * log(1 - test_predictions$pred_prob),
    na.rm = TRUE
)

cat(sprintf("\n=== Holdout Performance (2024) ===\n"))
cat(sprintf("Test games: %d\n", nrow(test_predictions)))
cat(sprintf("Accuracy: %.2f%%\n", holdout_accuracy * 100))
cat(sprintf("Log-loss: %.4f\n\n", holdout_logloss))

# =============================================================================
# 2. CALIBRATION PLOTS: Predicted Probability vs. Actual Win Rate
# =============================================================================

cat(strrep("=", 60), "\n")
cat("CALIBRATION ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

# Bin predictions and compute actual win rates
calibration_data <- test_predictions %>%
    mutate(
        prob_bin = cut(pred_prob,
            breaks = seq(0, 1, by = 0.1),
            include.lowest = TRUE,
            labels = paste0(seq(5, 95, by = 10), "%")
        )
    ) %>%
    group_by(prob_bin) %>%
    summarise(
        mean_pred_prob = mean(pred_prob, na.rm = TRUE),
        actual_win_rate = mean(home_winner, na.rm = TRUE),
        n_games = n(),
        se = sqrt(actual_win_rate * (1 - actual_win_rate) / n_games),
        .groups = "drop"
    ) %>%
    filter(n_games >= 10) # Require at least 10 games per bin

cat("Calibration bins with ≥10 games:\n")
print(calibration_data %>% select(prob_bin, mean_pred_prob, actual_win_rate, n_games))

# Calibration plot
p_calibration <- calibration_data %>%
    ggplot(aes(x = mean_pred_prob, y = actual_win_rate)) +
    geom_abline(
        intercept = 0, slope = 1, linetype = "dashed",
        color = "gray40", linewidth = 1
    ) +
    geom_point(aes(size = n_games), alpha = 0.7, color = "#0072B2") +
    geom_errorbar(
        aes(
            ymin = actual_win_rate - 1.96 * se,
            ymax = actual_win_rate + 1.96 * se
        ),
        width = 0.02, color = "#0072B2", alpha = 0.7
    ) +
    geom_smooth(
        method = "loess", se = TRUE, color = "#D55E00",
        fill = "#D55E00", alpha = 0.2
    ) +
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_size_continuous(range = c(3, 10), name = "# Games") +
    labs(
        title = "Model Calibration: 2024 Holdout Test Set",
        subtitle = "Trained on 2019, 2021-2023 • Tested on 2024",
        x = "Predicted Win Probability (Home Team)",
        y = "Actual Win Rate (Home Team)",
        caption = "Dashed line = perfect calibration. Points sized by # of games per bin. Error bars = 95% CI."
    ) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
    )

ggsave(
    filename = here("results", "figures", "14_calibration_plot.png"),
    plot = p_calibration,
    width = 10,
    height = 8,
    dpi = 400
)

cat("\n✓ Calibration plot saved\n\n")

# Calibration metrics
cal_mse <- mean((calibration_data$mean_pred_prob - calibration_data$actual_win_rate)^2, na.rm = TRUE)
cal_mae <- mean(abs(calibration_data$mean_pred_prob - calibration_data$actual_win_rate), na.rm = TRUE)

cat(sprintf("Calibration MSE: %.4f\n", cal_mse))
cat(sprintf("Calibration MAE: %.4f\n\n", cal_mae))

# =============================================================================
# 3. COMPARE TO HISTORICAL NCAA SEEDS (if available)
# =============================================================================

cat(strrep("=", 60), "\n")
cat("HISTORICAL NCAA SEED COMPARISON\n")
cat(strrep("=", 60), "\n\n")

# Placeholder for NCAA seed data - user needs to provide this
# Expected format: team, season, ncaa_seed, region

# For now, create a template for future use
ncaa_seed_template <- tibble(
    season = integer(),
    team = character(),
    ncaa_seed = integer(),
    region = character()
)

# Check if user has provided NCAA seed data
ncaa_seed_file <- here("data", "raw", "ncaa_seeds_historical.csv")

if (file.exists(ncaa_seed_file)) {
    cat("✓ Found historical NCAA seed data\n")

    ncaa_seeds <- read_csv(ncaa_seed_file, show_col_types = FALSE)

    # Compare model seeds to NCAA seeds for overlapping teams/seasons
    # Load model-based seeds
    team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))

    seed_comparison <- team_abilities %>%
        filter(!is.na(seed)) %>%
        inner_join(ncaa_seeds, by = c("team")) %>%
        mutate(
            seed_diff = seed - ncaa_seed,
            seed_diff_abs = abs(seed_diff)
        )

    cat(sprintf("Comparing %d teams with both model and NCAA seeds\n\n", nrow(seed_comparison)))

    # Summary statistics
    cat("Seed Difference Summary:\n")
    print(summary(seed_comparison$seed_diff))

    cat(sprintf("\nMean Absolute Seed Difference: %.2f\n", mean(seed_comparison$seed_diff_abs)))
    cat(sprintf("Median Absolute Seed Difference: %.2f\n\n", median(seed_comparison$seed_diff_abs)))

    # Seed comparison plot
    p_seed_compare <- seed_comparison %>%
        ggplot(aes(x = ncaa_seed, y = seed)) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
        geom_point(alpha = 0.5, size = 3, color = "#0072B2") +
        geom_smooth(method = "lm", se = TRUE, color = "#D55E00") +
        scale_x_continuous(breaks = 1:16) +
        scale_y_continuous(breaks = 1:16) +
        coord_fixed() +
        labs(
            title = "Model Seeds vs. NCAA Committee Seeds",
            subtitle = "Based on Bradley-Terry λ values",
            x = "NCAA Committee Seed",
            y = "Model-Assigned Seed (λ-based)",
            caption = "Dashed line = perfect agreement. Lower seeds (1-4) are stronger."
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11),
            axis.title = element_text(face = "bold")
        )

    ggsave(
        filename = here("results", "figures", "15_seed_comparison.png"),
        plot = p_seed_compare,
        width = 10,
        height = 10,
        dpi = 400
    )

    cat("✓ Seed comparison plot saved\n")
} else {
    cat("⚠️  No historical NCAA seed data found\n")
    cat(sprintf("   Expected file: %s\n", ncaa_seed_file))
    cat("\nTo enable NCAA seed comparison:\n")
    cat("1. Create a CSV file with columns: season, team, ncaa_seed, region\n")
    cat("2. Use exact team names matching your model data\n")
    cat("3. Save to data/raw/ncaa_seeds_historical.csv\n\n")

    # Create template file
    dir_create_safe("data", "raw")
    write_csv(
        tibble(
            season = c(2024, 2024, 2023, 2023),
            team = c(
                "South Carolina Gamecocks", "Iowa Hawkeyes",
                "South Carolina Gamecocks", "Virginia Tech Hokies"
            ),
            ncaa_seed = c(1, 1, 1, 3),
            region = c("Portland", "Albany", "Greenville", "Seattle")
        ),
        here("data", "raw", "ncaa_seeds_template.csv")
    )

    cat("✓ Created template file: data/raw/ncaa_seeds_template.csv\n\n")
}

# =============================================================================
# Save validation results
# =============================================================================

validation_results <- list(
    holdout_accuracy = holdout_accuracy,
    holdout_logloss = holdout_logloss,
    calibration_data = calibration_data,
    calibration_mse = cal_mse,
    calibration_mae = cal_mae,
    test_predictions = test_predictions,
    train_test_split = list(
        train_seasons = train_seasons,
        test_season = test_season,
        n_train_games = nrow(games_train),
        n_test_games = nrow(games_test)
    )
)

saveRDS(validation_results, here("results", "tables", "validation_results.rds"))
cat("\n✓ Validation results saved\n")

# Create summary table
validation_summary <- tibble(
    Metric = c(
        "Training Seasons",
        "Test Season",
        "Training Games",
        "Test Games",
        "Test Accuracy",
        "Test Log-Loss",
        "Calibration MSE",
        "Calibration MAE"
    ),
    Value = c(
        paste(train_seasons, collapse = ", "),
        as.character(test_season),
        as.character(nrow(games_train)),
        as.character(nrow(test_predictions)),
        sprintf("%.2f%%", holdout_accuracy * 100),
        sprintf("%.4f", holdout_logloss),
        sprintf("%.4f", cal_mse),
        sprintf("%.4f", cal_mae)
    )
)

write_csv(validation_summary, here("results", "tables", "validation_summary.csv"))

cat("\n", strrep("=", 60), "\n")
cat("VALIDATION COMPLETE\n")
cat(strrep("=", 60), "\n\n")

print(validation_summary)
