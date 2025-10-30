# =============================================================================
# Script 02a: Bradley-Terry Model - GAME LEVEL (Proper Home Advantage)
# Purpose: Fit BT model at game level (not pair level) to properly estimate home advantage
# =============================================================================

library(tidyverse)
library(here)

if (!require("BradleyTerry2")) {
    install.packages("BradleyTerry2")
    library(BradleyTerry2)
}

games_cleaned <- readRDS(here("data", "processed", "games_cleaned.rds"))

cat("\n")
cat("================================================================================\n")
cat("  GAME-LEVEL Bradley-Terry Model (Proper Home Advantage Estimation)\n")
cat("================================================================================\n")
cat("\n")

# Fit game-level models per season (proper home advantage)
seasons <- sort(unique(games_cleaned$season))
all_season_abilities <- list()
beta_home_by_season <- tibble()

for (season_year in seasons) {
    cat(sprintf("\n=== Fitting GAME-LEVEL model for %d ===\n", season_year))

    games_season <- games_cleaned %>%
        filter(season == season_year) %>%
        mutate(
            # Convert to 1-game format for BTM
            home.wins = as.integer(home_winner),
            away.wins = as.integer(1 - home_winner),
            home_advantage = 1 - neutral_site # 1 = home game, 0 = neutral
        )

    cat(sprintf(
        "  Games: %d | Teams: %d\n",
        nrow(games_season),
        length(unique(c(games_season$home_team, games_season$away_team)))
    ))

    # Ensure factors have all teams
    all_teams <- sort(unique(c(games_season$home_team, games_season$away_team)))
    games_season <- games_season %>%
        mutate(
            home.team = factor(home_team, levels = all_teams),
            away.team = factor(away_team, levels = all_teams)
        )

    # Fit game-level BTM
    bt_model_season <- BTm(
        outcome = cbind(home.wins, away.wins),
        player1 = home.team,
        player2 = away.team,
        formula = ~ team + home_advantage, # Game-level covariate
        id = "team",
        contrasts = list(team = "contr.sum"),
        data = games_season
    )

    # Check convergence
    if (!bt_model_season$converged) {
        warning(sprintf("Model did not converge for season %d", season_year))
    } else if (bt_model_season$iter > 50) {
        warning(sprintf(
            "Model for %d took %d iterations (>50). May indicate convergence issues.",
            season_year, bt_model_season$iter
        ))
    }

    cat(sprintf(
        "  Converged: %s | Iterations: %d\n",
        bt_model_season$converged, bt_model_season$iter
    ))

    # Extract abilities
    abilities <- BTabilities(bt_model_season) %>%
        as.data.frame() %>%
        rownames_to_column(var = "team") %>%
        rename(lambda = ability, se = s.e.) %>%
        mutate(season = season_year)

    all_season_abilities[[as.character(season_year)]] <- abilities

    # Extract home advantage coefficient
    coef_summary <- summary(bt_model_season)$coefficients
    if ("home_advantage" %in% rownames(coef_summary)) {
        beta_home_by_season <- bind_rows(
            beta_home_by_season,
            tibble(
                season = season_year,
                beta_home = coef_summary["home_advantage", "Estimate"],
                se = coef_summary["home_advantage", "Std. Error"],
                z_value = coef_summary["home_advantage", "z value"],
                p_value = coef_summary["home_advantage", "Pr(>|z|)"]
            )
        )
        cat(sprintf(
            "  Home advantage: %.4f (SE: %.4f, p-value: %.4f)\n",
            coef_summary["home_advantage", "Estimate"],
            coef_summary["home_advantage", "Std. Error"],
            coef_summary["home_advantage", "Pr(>|z|)"]
        ))
    } else {
        warning("Home advantage coefficient not found in model")
    }
}

# Meta-analysis of home advantage across seasons
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("HOME ADVANTAGE ACROSS SEASONS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

print(beta_home_by_season)

# Compute weighted average home advantage (inverse-variance weighting)
if (nrow(beta_home_by_season) > 0) {
    beta_home_meta <- with(beta_home_by_season, {
        weighted.mean(beta_home, w = 1 / se^2)
    })

    se_meta <- sqrt(1 / sum(1 / beta_home_by_season$se^2))

    cat(sprintf("\nMeta-estimate of home advantage: %.4f (SE: %.4f)\n", beta_home_meta, se_meta))
    cat(sprintf("  → Home win prob (equal teams): %.1f%%\n", plogis(beta_home_meta) * 100))
    cat(sprintf(
        "  → 95%% CI: [%.4f, %.4f]\n",
        beta_home_meta - 1.96 * se_meta,
        beta_home_meta + 1.96 * se_meta
    ))
} else {
    beta_home_meta <- 0
    warning("No valid home advantage estimates found. Using beta_home = 0")
}

# Z-score within season to put all seasons on same scale
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("AGGREGATING TEAM ABILITIES ACROSS SEASONS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

all_abilities <- bind_rows(all_season_abilities)

# Z-score within season (CRITICAL: standardize before aggregating)
all_abilities_scaled <- all_abilities %>%
    group_by(season) %>%
    mutate(
        season_sd = sd(lambda, na.rm = TRUE),
        season_mean = mean(lambda, na.rm = TRUE),
        lambda_z = (lambda - season_mean) / season_sd,
        se_z = se / season_sd, # Scale SE too!
        se_z = pmax(se_z, 1e-6) # Prevent division by zero
    ) %>%
    ungroup()

# Aggregate z-scored abilities using inverse-variance weighting
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

cat(sprintf("Unique teams across all seasons: %d\n", nrow(team_abilities_df)))
cat(sprintf(
    "Teams appearing in all %d seasons: %d\n",
    length(seasons),
    sum(team_abilities_df$n_seasons == length(seasons))
))

# Summary stats
cat("\nTeam ability summary:\n")
cat(sprintf("  Mean λ: %.4f (should be ~0 due to z-scoring)\n", mean(team_abilities_df$lambda)))
cat(sprintf("  SD λ: %.4f\n", sd(team_abilities_df$lambda)))
cat(sprintf("  Range: [%.2f, %.2f]\n", min(team_abilities_df$lambda), max(team_abilities_df$lambda)))
cat(sprintf("  Median SE: %.4f\n", median(team_abilities_df$se)))

# Save results
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

saveRDS(beta_home_meta, here("data", "processed", "home_advantage_meta.rds"))
saveRDS(beta_home_by_season, here("data", "processed", "home_advantage_by_season.rds"))
write_csv(beta_home_by_season, here("data", "processed", "home_advantage_by_season.csv"))

saveRDS(all_abilities_scaled, here("data", "processed", "team_abilities_by_season_gamelevel.rds"))
saveRDS(team_abilities_df, here("data", "processed", "team_abilities_gamelevel.rds"))
write_csv(team_abilities_df, here("data", "processed", "team_abilities_gamelevel.csv"))

cat("✓ Saved game-level model results:\n")
cat("  - home_advantage_meta.rds (single value)\n")
cat("  - home_advantage_by_season.rds/csv (by season)\n")
cat("  - team_abilities_gamelevel.rds/csv (aggregated abilities)\n")
cat("\n")
cat("NOTE: These game-level estimates should be used instead of pair-level estimates\n")
cat("      for accurate home advantage inference and unbiased predictions.\n")
cat("\n")
