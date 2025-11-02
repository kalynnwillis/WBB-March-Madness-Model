# =============================================================================
# Script 02b: Bradley-Terry Model with Offensive/Defensive Covariates
# Purpose: Fit BT model including team-specific offensive/defensive ratings
# =============================================================================

library(tidyverse)
library(here)

if (!require("BradleyTerry2")) {
    install.packages("BradleyTerry2")
    library(BradleyTerry2)
}

cat("\n")
cat("================================================================================\n")
cat("  Bradley-Terry Model with Offensive/Defensive Covariates\n")
cat("================================================================================\n")
cat("\n")

# Load data
games_cleaned <- readRDS(here("data", "processed", "games_cleaned.rds"))
team_off_def <- readRDS(here("data", "processed", "team_off_def_by_season.rds"))
tournament_seeds <- readRDS(here("data", "processed", "tournament_seeds.rds"))

# Load or compute MOV ratings
mov_file <- here("data", "processed", "team_mov_ratings_by_season.rds")
if (!file.exists(mov_file)) {
    cat("Computing MOV ratings...\n")
    mov_ratings <- games_cleaned %>%
        mutate(
            home_mov = ifelse(home_winner == 1, home_score - away_score, 0),
            away_mov = ifelse(home_winner == 0, away_score - home_score, 0)
        ) %>%
        group_by(season) %>%
        {
            bind_rows(
                group_by(., season, team = home_team) %>%
                    summarise(
                        total_mov = sum(home_score - away_score),
                        avg_mov = mean(home_score - away_score),
                        .groups = "drop"
                    ),
                group_by(., season, team = away_team) %>%
                    summarise(
                        total_mov = sum(away_score - home_score),
                        avg_mov = mean(away_score - home_score),
                        .groups = "drop"
                    )
            )
        } %>%
        group_by(season, team) %>%
        summarise(
            total_mov = sum(total_mov, na.rm = TRUE),
            avg_mov = mean(avg_mov, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        group_by(season) %>%
        mutate(avg_mov_z = scale(avg_mov)[, 1]) %>%
        ungroup()

    saveRDS(mov_ratings, mov_file)
    write_csv(mov_ratings, here("data", "processed", "team_mov_ratings_by_season.csv"))
} else {
    mov_ratings <- readRDS(mov_file)
}

cat(sprintf(
    "Loaded %d games, %d teams with offensive/defensive ratings, %d team-seasons with MOV\n",
    nrow(games_cleaned), nrow(team_off_def), nrow(mov_ratings)
))

# Fit game-level models per season with offensive/defensive covariates
seasons <- sort(unique(games_cleaned$season))
all_season_abilities <- list()
beta_home_by_season <- tibble()
coef_summary_all <- list()

for (season_year in seasons) {
    cat(sprintf("\n=== Fitting model with covariates for %d ===\n", season_year))

    # Get this season's games
    games_season <- games_cleaned %>%
        filter(season == season_year) %>%
        mutate(
            home.wins = as.integer(home_winner),
            away.wins = as.integer(1 - home_winner),
            home_advantage = 1 - neutral_site,
            # Margin of victory (always positive)
            margin_of_victory = abs(home_score - away_score),
            # Scaled MOV (z-score within season for standardization)
            mov_scaled = scale(margin_of_victory)[, 1],
            # Log-transformed MOV to reduce impact of blowouts
            log_mov = log(margin_of_victory + 1)
        )

    # Get this season's offensive/defensive ratings
    off_def_season <- team_off_def %>%
        filter(season == season_year) %>%
        select(team, off_rating_z, def_rating_z, net_rating_z)

    # Get this season's MOV ratings
    mov_season <- mov_ratings %>%
        filter(season == season_year) %>%
        select(team, avg_mov_z)

    # Join offensive/defensive ratings to games
    games_season <- games_season %>%
        left_join(off_def_season %>% rename_with(~ paste0("home_", .), -team),
            by = c("home_team" = "team")
        ) %>%
        left_join(off_def_season %>% rename_with(~ paste0("away_", .), -team),
            by = c("away_team" = "team")
        ) %>%
        left_join(mov_season %>% rename(home_mov_z = avg_mov_z),
            by = c("home_team" = "team")
        ) %>%
        left_join(mov_season %>% rename(away_mov_z = avg_mov_z),
            by = c("away_team" = "team")
        )

    # Calculate offensive and defensive differentials for the matchup
    games_season <- games_season %>%
        mutate(
            # Offensive advantage: home offense vs away defense
            off_diff = home_off_rating_z - away_def_rating_z,
            # Defensive advantage: home defense vs away offense
            def_diff = away_off_rating_z - home_def_rating_z,
            # Net rating difference
            net_diff = home_net_rating_z - away_net_rating_z,
            # MOV difference (reflects dominance)
            mov_diff = home_mov_z - away_mov_z
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

    # ===== MODEL 1: Base model (team + home advantage only) =====
    bt_model_base <- BTm(
        outcome = cbind(home.wins, away.wins),
        player1 = home.team,
        player2 = away.team,
        formula = ~ team + home_advantage,
        id = "team",
        contrasts = list(team = "contr.sum"),
        data = games_season
    )

    # ===== MODEL 2: With net rating covariate =====
    bt_model_net <- tryCatch(
        {
            BTm(
                outcome = cbind(home.wins, away.wins),
                player1 = home.team,
                player2 = away.team,
                formula = ~ team + home_advantage + net_diff,
                id = "team",
                contrasts = list(team = "contr.sum"),
                data = games_season
            )
        },
        error = function(e) {
            warning(sprintf("Net rating model failed for %d: %s", season_year, e$message))
            NULL
        }
    )

    # ===== MODEL 3: With separate offensive/defensive differentials =====
    bt_model_offdef <- tryCatch(
        {
            BTm(
                outcome = cbind(home.wins, away.wins),
                player1 = home.team,
                player2 = away.team,
                formula = ~ team + home_advantage + off_diff + def_diff,
                id = "team",
                contrasts = list(team = "contr.sum"),
                data = games_season
            )
        },
        error = function(e) {
            warning(sprintf("Off/Def model failed for %d: %s", season_year, e$message))
            NULL
        }
    )

    # ===== MODEL 4: With MOV differential (team-level covariate) =====
    bt_model_mov <- tryCatch(
        {
            BTm(
                outcome = cbind(home.wins, away.wins),
                player1 = home.team,
                player2 = away.team,
                formula = ~ team + home_advantage + mov_diff,
                id = "team",
                contrasts = list(team = "contr.sum"),
                data = games_season
            )
        },
        error = function(e) {
            warning(sprintf("MOV model failed for %d: %s", season_year, e$message))
            NULL
        }
    )

    # ===== MODEL 5: Combined model (net rating + MOV) =====
    bt_model_combined <- tryCatch(
        {
            BTm(
                outcome = cbind(home.wins, away.wins),
                player1 = home.team,
                player2 = away.team,
                formula = ~ team + home_advantage + net_diff + mov_diff,
                id = "team",
                contrasts = list(team = "contr.sum"),
                data = games_season
            )
        },
        error = function(e) {
            warning(sprintf("Combined model failed for %d: %s", season_year, e$message))
            NULL
        }
    )

    # Check convergence for all models
    cat(sprintf(
        "  Base model - Converged: %s | Iterations: %d\n",
        bt_model_base$converged, bt_model_base$iter
    ))

    if (!is.null(bt_model_net)) {
        cat(sprintf(
            "  Net rating model - Converged: %s | Iterations: %d\n",
            bt_model_net$converged, bt_model_net$iter
        ))
    }

    if (!is.null(bt_model_offdef)) {
        cat(sprintf(
            "  Off/Def model - Converged: %s | Iterations: %d\n",
            bt_model_offdef$converged, bt_model_offdef$iter
        ))
    }

    if (!is.null(bt_model_mov)) {
        cat(sprintf(
            "  MOV model - Converged: %s | Iterations: %d\n",
            bt_model_mov$converged, bt_model_mov$iter
        ))
    }

    if (!is.null(bt_model_combined)) {
        cat(sprintf(
            "  Combined model - Converged: %s | Iterations: %d\n",
            bt_model_combined$converged, bt_model_combined$iter
        ))
    }

    # Select best model based on AIC (computed below)
    # For now, use base model (will select best by AIC later)
    best_model <- bt_model_base

    abilities <- BTabilities(best_model) %>%
        as.data.frame() %>%
        rownames_to_column(var = "team") %>%
        rename(lambda = ability, se = s.e.) %>%
        mutate(season = season_year)

    all_season_abilities[[as.character(season_year)]] <- abilities

    # Extract coefficients
    coef_summary <- summary(best_model)$coefficients
    coef_summary_all[[as.character(season_year)]] <- coef_summary

    # Extract home advantage
    if ("home_advantage" %in% rownames(coef_summary)) {
        beta_home_by_season <- bind_rows(
            beta_home_by_season,
            tibble(
                season = season_year,
                beta_home = coef_summary["home_advantage", "Estimate"],
                se = coef_summary["home_advantage", "Std. Error"]
            )
        )
        cat(sprintf(
            "  Home advantage: %.4f (SE: %.4f)\n",
            coef_summary["home_advantage", "Estimate"],
            coef_summary["home_advantage", "Std. Error"]
        ))
    }

    # Print covariate effects if present
    if ("net_diff" %in% rownames(coef_summary)) {
        cat(sprintf(
            "  Net rating effect: %.4f (SE: %.4f) ***\n",
            coef_summary["net_diff", "Estimate"],
            coef_summary["net_diff", "Std. Error"]
        ))
    }

    if ("off_diff" %in% rownames(coef_summary) && "def_diff" %in% rownames(coef_summary)) {
        cat(sprintf(
            "  Offensive differential effect: %.4f (SE: %.4f)\n",
            coef_summary["off_diff", "Estimate"],
            coef_summary["off_diff", "Std. Error"]
        ))
        cat(sprintf(
            "  Defensive differential effect: %.4f (SE: %.4f)\n",
            coef_summary["def_diff", "Estimate"],
            coef_summary["def_diff", "Std. Error"]
        ))
    }

    # Model comparison (AIC)
    aic_base <- AIC(bt_model_base)
    aic_net <- if (!is.null(bt_model_net)) AIC(bt_model_net) else NA
    aic_offdef <- if (!is.null(bt_model_offdef)) AIC(bt_model_offdef) else NA
    aic_mov <- if (!is.null(bt_model_mov)) AIC(bt_model_mov) else NA
    aic_combined <- if (!is.null(bt_model_combined)) AIC(bt_model_combined) else NA

    cat(sprintf("\n  Model Comparison (AIC, lower is better):\n"))
    cat(sprintf("    Base model: %.1f\n", aic_base))
    if (!is.na(aic_net)) {
        cat(sprintf(
            "    Net rating model: %.1f (Δ = %.1f)\n",
            aic_net, aic_net - aic_base
        ))
    }
    if (!is.na(aic_offdef)) {
        cat(sprintf(
            "    Off/Def model: %.1f (Δ = %.1f)\n",
            aic_offdef, aic_offdef - aic_base
        ))
    }
    if (!is.na(aic_mov)) {
        cat(sprintf(
            "    MOV model: %.1f (Δ = %.1f) %s\n",
            aic_mov, aic_mov - aic_base,
            if (aic_mov < aic_base) "***BETTER***" else ""
        ))
    }
    if (!is.na(aic_combined)) {
        cat(sprintf(
            "    Combined model: %.1f (Δ = %.1f) %s\n",
            aic_combined, aic_combined - aic_base,
            if (aic_combined < aic_base) "***BETTER***" else ""
        ))
    }

    # Identify and SELECT best model by AIC
    all_aics <- c(
        base = aic_base, net = aic_net, offdef = aic_offdef,
        mov = aic_mov, combined = aic_combined
    )
    all_models <- list(
        base = bt_model_base, net = bt_model_net, offdef = bt_model_offdef,
        mov = bt_model_mov, combined = bt_model_combined
    )
    all_aics <- all_aics[!is.na(all_aics)]
    best_aic_name <- names(which.min(all_aics))
    best_model <- all_models[[best_aic_name]]
    cat(sprintf("    → Best model by AIC: %s (USING FOR ABILITIES)\n", best_aic_name))
}

# ===== AGGREGATE RESULTS ACROSS SEASONS =====
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("AGGREGATING TEAM ABILITIES ACROSS SEASONS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

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

cat(sprintf("Unique teams: %d\n", nrow(team_abilities_df)))

# Join with offensive/defensive ratings
team_off_def_agg <- readRDS(here("data", "processed", "team_off_def_aggregated.rds"))

team_abilities_with_ratings <- team_abilities_df %>%
    left_join(
        team_off_def_agg %>% select(-n_seasons), # Remove n_seasons to avoid duplicate column
        by = "team"
    ) %>%
    select(
        team, lambda, se, n_seasons, seasons_played, offensive_rating, defensive_rating,
        net_rating, off_rating_z, def_rating_z, net_rating_z
    )

# Add tournament seeds
team_abilities_with_seeds <- team_abilities_with_ratings %>%
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

# Meta-analysis of home advantage
if (nrow(beta_home_by_season) > 0) {
    beta_home_meta <- with(beta_home_by_season, {
        weighted.mean(beta_home, w = 1 / se^2)
    })
    se_meta <- sqrt(1 / sum(1 / beta_home_by_season$se^2))

    cat(sprintf(
        "\nMeta-estimate of home advantage: %.4f (SE: %.4f)\n",
        beta_home_meta, se_meta
    ))
} else {
    beta_home_meta <- 0
}

# Compute win probability matrix
lambda_vec <- setNames(team_abilities_df$lambda, team_abilities_df$team)
lambda_diff <- outer(X = lambda_vec, Y = lambda_vec, FUN = "-")
win_probs <- pmin(pmax(1 / (1 + exp(-1 * lambda_diff)), 1e-6), 1 - 1e-6)
diag(win_probs) <- NA

# ===== SAVE RESULTS =====
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

saveRDS(beta_home_meta, here("data", "processed", "home_advantage_meta_cov.rds"))
saveRDS(beta_home_by_season, here("data", "processed", "home_advantage_by_season_cov.rds"))
saveRDS(all_abilities_scaled, here("data", "processed", "team_abilities_by_season_cov.rds"))
saveRDS(team_abilities_df, here("data", "processed", "team_abilities_cov.rds"))
saveRDS(team_abilities_with_ratings, here("data", "processed", "team_abilities_with_ratings.rds"))
saveRDS(team_abilities_with_seeds, here("data", "processed", "team_abilities_with_seeds_cov.rds"))
saveRDS(win_probs, here("data", "processed", "win_probability_matrix_cov.rds"))

write_csv(team_abilities_df, here("data", "processed", "team_abilities_cov.csv"))
write_csv(team_abilities_with_ratings, here("data", "processed", "team_abilities_with_ratings.csv"))
write_csv(team_abilities_with_seeds, here("data", "processed", "team_abilities_with_seeds_cov.csv"))

cat("✓ Saved model results with offensive/defensive covariates:\n")
cat("  - team_abilities_cov.rds/csv\n")
cat("  - team_abilities_with_ratings.rds/csv\n")
cat("  - team_abilities_with_seeds_cov.rds/csv\n")
cat("  - win_probability_matrix_cov.rds\n")

# ===== SUMMARY STATISTICS =====
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("COVARIATE EFFECTS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

# Aggregate covariate effects across seasons
covariate_effects <- map_dfr(names(coef_summary_all), function(s) {
    coefs <- coef_summary_all[[s]]
    tibble(
        season = as.integer(s),
        net_diff_coef = if ("net_diff" %in% rownames(coefs)) coefs["net_diff", "Estimate"] else NA,
        net_diff_se = if ("net_diff" %in% rownames(coefs)) coefs["net_diff", "Std. Error"] else NA,
        off_diff_coef = if ("off_diff" %in% rownames(coefs)) coefs["off_diff", "Estimate"] else NA,
        off_diff_se = if ("off_diff" %in% rownames(coefs)) coefs["off_diff", "Std. Error"] else NA,
        def_diff_coef = if ("def_diff" %in% rownames(coefs)) coefs["def_diff", "Estimate"] else NA,
        def_diff_se = if ("def_diff" %in% rownames(coefs)) coefs["def_diff", "Std. Error"] else NA
    )
})

saveRDS(covariate_effects, here("data", "processed", "covariate_effects_summary.rds"))
write_csv(covariate_effects, here("data", "processed", "covariate_effects_summary.csv"))

cat("Covariate effects across seasons:\n")
print(covariate_effects)

# Calculate average effects (weighted by inverse variance)
if (any(!is.na(covariate_effects$net_diff_coef))) {
    valid_net <- !is.na(covariate_effects$net_diff_coef) & !is.na(covariate_effects$net_diff_se)
    if (sum(valid_net) > 0) {
        avg_net_effect <- weighted.mean(
            covariate_effects$net_diff_coef[valid_net],
            w = 1 / covariate_effects$net_diff_se[valid_net]^2
        )
        cat(sprintf("\nAverage net rating effect: %.4f\n", avg_net_effect))
        cat("  → +1 SD in net rating increases log-odds of winning by %.4f\n", avg_net_effect)
        cat(sprintf(
            "  → Equivalent to %.1f%% increase in win probability (at 50-50 matchup)\n",
            100 * (plogis(avg_net_effect) - 0.5)
        ))
    }
}

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Top 10 Teams by Lambda (with offensive/defensive context):\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(team_abilities_with_ratings %>%
    select(team, lambda, offensive_rating, defensive_rating, net_rating) %>%
    head(10))

cat("\n✓ Enhanced Bradley-Terry model with covariates completed successfully!\n\n")
