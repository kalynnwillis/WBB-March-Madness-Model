# =============================================================================
# Script 01: Data Collection (UPDATED - Fixed for wncaahoopR)
# Purpose: Collect and prepare Women's NCAA Basketball data
# =============================================================================

# Load required libraries
library(tidyverse)
library(here)

# =============================================================================
# IMPORTANT NOTE ABOUT DATA COLLECTION
# =============================================================================
# The wncaahoopR package functions may vary. Here are alternative approaches:
#
# APPROACH 1: Use wehoop package (recommended alternative)
# install.packages("wehoop")
# library(wehoop)
#
# APPROACH 2: Use hoopR package
# install.packages("hoopR")
# library(hoopR)
#
# APPROACH 3: Manual CSV import (if you have data files)
# =============================================================================

cat("Setting up data collection...\n")

# Try to use wehoop package as primary data source
use_wehoop <- FALSE
use_hoopR <- FALSE
use_wncaahoop <- FALSE

# Check what's available
if (requireNamespace("wehoop", quietly = TRUE)) {
    library(wehoop)
    use_wehoop <- TRUE
    cat("✓ Using wehoop package for data collection\n")
} else if (requireNamespace("hoopR", quietly = TRUE)) {
    library(hoopR)
    use_hoopR <- TRUE
    cat("✓ Using hoopR package for data collection\n")
} else if (requireNamespace("wncaahoopR", quietly = TRUE)) {
    library(wncaahoopR)
    use_wncaahoop <- TRUE
    cat("✓ Using wncaahoopR package for data collection\n")
} else {
    cat("⚠ No basketball data packages found.\n")
    cat("  Installing wehoop package...\n")
    install.packages("wehoop")
    library(wehoop)
    use_wehoop <- TRUE
}

# =============================================================================
# 1. Collect Game Data
# =============================================================================

SEASONS <- c(2023, 2024) # March Madness years

cat("\nCollecting Women's Basketball game data...\n")

all_games <- list()

# =============================================================================
# OPTION 1: Using wehoop package
# =============================================================================
if (use_wehoop) {
    cat("Using wehoop package to collect data...\n")

    for (season in SEASONS) {
        cat(sprintf("Fetching data for %d season...\n", season))

        tryCatch(
            {
                # Load WBB schedule data using wehoop
                season_games <- wehoop::load_wbb_schedule(seasons = season)

                # Standardize column names
                season_games <- season_games %>%
                    mutate(
                        season = season,
                        game_date = as.Date(date),
                        home_team = home_team_name,
                        away_team = away_team_name,
                        home_score = home_score,
                        away_score = away_score,
                        game_type = case_when(
                            !is.na(tournament_type) ~ tournament_type,
                            TRUE ~ "Regular Season"
                        )
                    ) %>%
                    select(
                        season, game_date, home_team, away_team,
                        home_score, away_score, game_type
                    )

                all_games[[as.character(season)]] <- season_games

                cat(sprintf("  ✓ Successfully collected %d games\n", nrow(season_games)))
            },
            error = function(e) {
                cat(sprintf(
                    "  ✗ Error collecting data for season %d: %s\n",
                    season, e$message
                ))
            }
        )
    }
}

# =============================================================================
# OPTION 2: Using hoopR package
# =============================================================================
if (use_hoopR && length(all_games) == 0) {
    cat("Using hoopR package to collect data...\n")

    for (season in SEASONS) {
        cat(sprintf("Fetching data for %d season...\n", season))

        tryCatch(
            {
                # Load WBB schedule data using hoopR
                season_games <- hoopR::load_wbb_schedule(seasons = season)

                # Standardize column names
                season_games <- season_games %>%
                    mutate(
                        season = season,
                        game_date = as.Date(date),
                        home_team = home_team_name,
                        away_team = away_team_name,
                        home_score = as.numeric(home_score),
                        away_score = as.numeric(away_score),
                        game_type = "Regular Season"
                    ) %>%
                    select(
                        season, game_date, home_team, away_team,
                        home_score, away_score, game_type
                    )

                all_games[[as.character(season)]] <- season_games

                cat(sprintf("  ✓ Successfully collected %d games\n", nrow(season_games)))
            },
            error = function(e) {
                cat(sprintf(
                    "  ✗ Error collecting data for season %d: %s\n",
                    season, e$message
                ))
            }
        )
    }
}

# =============================================================================
# OPTION 3: Use sample/simulated data for testing
# =============================================================================
if (length(all_games) == 0) {
    cat("\n⚠ Could not collect real data. Creating sample dataset for testing...\n")
    cat("  NOTE: Replace this with real data for your actual analysis!\n\n")

    # Create sample teams
    teams <- c(
        "South Carolina", "Stanford", "UConn", "Notre Dame",
        "Louisville", "Baylor", "Indiana", "Iowa",
        "Maryland", "NC State", "Texas", "Tennessee",
        "Ohio State", "Duke", "Virginia Tech", "Oregon"
    )

    # Create sample games
    set.seed(479)
    n_games <- 200

    sample_games <- tibble(
        season = sample(SEASONS, n_games, replace = TRUE),
        game_date = as.Date("2023-11-01") + sample(1:120, n_games, replace = TRUE),
        home_team = sample(teams, n_games, replace = TRUE),
        away_team = sample(teams, n_games, replace = TRUE),
        home_score = sample(55:95, n_games, replace = TRUE),
        away_score = sample(50:90, n_games, replace = TRUE),
        game_type = "Regular Season"
    ) %>%
        filter(home_team != away_team) # Remove games where team plays itself

    all_games[["sample"]] <- sample_games

    cat(sprintf("  Created %d sample games for testing\n", nrow(sample_games)))
}

# =============================================================================
# 2. Combine and Clean Data
# =============================================================================

# Combine all seasons
if (length(all_games) > 0) {
    games_combined <- bind_rows(all_games)
    cat(sprintf("\nTotal games collected: %d\n", nrow(games_combined)))
} else {
    stop("No data collected. Please install wehoop or hoopR package, or provide CSV files.")
}

cat("\nCleaning and preparing data...\n")

# Filter and clean the data
games_cleaned <- games_combined %>%
    # Remove exhibition games and non-D1 games
    filter(!grepl("Exhibition", game_type, ignore.case = TRUE)) %>%
    # Remove games with missing scores
    filter(!is.na(home_score) & !is.na(away_score)) %>%
    # Convert scores to numeric
    mutate(
        home_score = as.numeric(home_score),
        away_score = as.numeric(away_score)
    ) %>%
    # Remove ties (if any exist in women's basketball)
    filter(home_score != away_score) %>%
    # Create winner indicator
    mutate(
        home_winner = if_else(home_score > away_score, 1, 0),
        away_winner = 1 - home_winner,
        # Extract game date information
        game_month = lubridate::month(game_date),
        game_year = lubridate::year(game_date)
    ) %>%
    # Filter for regular season only (before March Madness starts)
    # Regular season typically ends in early March
    filter(game_month < 3 | (game_month == 3 & lubridate::day(game_date) < 15))

cat(sprintf("Regular season games after cleaning: %d\n", nrow(games_cleaned)))

# =============================================================================
# 3. Create Tournament Seedings (Sample Data)
# =============================================================================

cat("\nCreating tournament bracket information...\n")
cat("NOTE: Using sample seedings. Replace with real tournament data.\n")

# Get unique teams
unique_teams <- sort(unique(c(games_cleaned$home_team, games_cleaned$away_team)))

# Create sample tournament seeds for top teams based on game performance
team_performance <- games_cleaned %>%
    mutate(team = home_team, wins = home_winner) %>%
    bind_rows(
        games_cleaned %>% mutate(team = away_team, wins = away_winner)
    ) %>%
    group_by(team) %>%
    summarise(
        games = n(),
        wins = sum(wins),
        win_pct = wins / games
    ) %>%
    arrange(desc(win_pct)) %>%
    head(64) # Top 64 teams make tournament

# Assign seeds
# Get actual number of teams
n_teams <- nrow(team_performance)

tournament_seeds <- team_performance %>%
    mutate(
        # Assign seeds based on actual number of teams
        seed = rep(1:16, length.out = n_teams),
        region = rep(c("Portland", "Albany", "Spokane", "Wichita"), length.out = n_teams),
        season = max(SEASONS)
    ) %>%
    select(season, team, seed, region)

cat(sprintf("Tournament teams created: %d\n", nrow(tournament_seeds)))

# =============================================================================
# 4. Create Bradley-Terry Model Data Structure
# =============================================================================

cat("\nPreparing data for Bradley-Terry model...\n")

# Get unique teams
unique_teams <- sort(unique(c(games_cleaned$home_team, games_cleaned$away_team)))
cat(sprintf("Unique teams in dataset: %d\n", length(unique_teams)))

# Prepare data in Bradley-Terry format
bt_data <- games_cleaned %>%
    rename(home.team = home_team, away.team = away_team) %>%
    group_by(season, home.team, away.team) %>%
    summarise(
        home.wins = sum(home_winner),
        away.wins = sum(away_winner),
        total_games = n(),
        .groups = "drop"
    ) %>%
    # Convert teams to factors with consistent levels
    mutate(
        home.team = factor(home.team, levels = unique_teams),
        away.team = factor(away.team, levels = unique_teams)
    )

cat(sprintf("Bradley-Terry data structure created: %d unique matchups\n", nrow(bt_data)))

# =============================================================================
# 5. Identify 8-12 Seed Teams
# =============================================================================

cat("\nIdentifying 8-12 seed teams...\n")

mid_tier_seeds <- tournament_seeds %>%
    filter(seed >= 8 & seed <= 12) %>%
    mutate(seed_category = "8-12 seeds")

cat(sprintf("Teams with 8-12 seeds: %d\n", nrow(mid_tier_seeds)))

# Summary by seed
seed_summary <- mid_tier_seeds %>%
    group_by(seed) %>%
    summarise(count = n()) %>%
    arrange(seed)

print(seed_summary)

# =============================================================================
# 6. Save Processed Data
# =============================================================================

cat("\nSaving processed data...\n")

# Create directories if they don't exist
dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

# Save raw combined data
saveRDS(games_combined, file = here("data", "raw", "games_raw.rds"))
write_csv(games_combined, file = here("data", "raw", "games_raw.csv"))

# Save cleaned regular season data
saveRDS(games_cleaned, file = here("data", "processed", "games_cleaned.rds"))
write_csv(games_cleaned, file = here("data", "processed", "games_cleaned.csv"))

# Save Bradley-Terry formatted data
saveRDS(bt_data, file = here("data", "processed", "bt_data.rds"))
write_csv(bt_data, file = here("data", "processed", "bt_data.csv"))

# Save tournament seeding information
saveRDS(tournament_seeds, file = here("data", "processed", "tournament_seeds.rds"))
write_csv(tournament_seeds, file = here("data", "processed", "tournament_seeds.csv"))

# Save mid-tier seed teams
saveRDS(mid_tier_seeds, file = here("data", "processed", "mid_tier_seeds.rds"))
write_csv(mid_tier_seeds, file = here("data", "processed", "mid_tier_seeds.csv"))

# =============================================================================
# 7. Generate Data Summary Report
# =============================================================================

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("DATA COLLECTION SUMMARY\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n\n")

cat(sprintf("Seasons analyzed: %s\n", paste(SEASONS, collapse = ", ")))
cat(sprintf("Total games collected: %d\n", nrow(games_combined)))
cat(sprintf("Regular season games: %d\n", nrow(games_cleaned)))
cat(sprintf("Unique teams: %d\n", length(unique_teams)))
cat(sprintf("Unique matchups: %d\n", nrow(bt_data)))
cat(sprintf("Tournament teams: %d\n", nrow(tournament_seeds)))
cat(sprintf("8-12 seed teams: %d\n", nrow(mid_tier_seeds)))

# Games per season
games_by_season <- games_cleaned %>%
    group_by(season) %>%
    summarise(games = n())

cat("\nGames by season:\n")
print(games_by_season)

cat("\n" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("✓ Data collection complete!\n")
cat(paste(rep("=", 70), collapse = "") %+% "\n")

# =============================================================================
# INSTRUCTIONS FOR REAL DATA:
# =============================================================================
# To use real data instead of sample data:
# 1. Install wehoop: install.packages("wehoop")
# 2. Or manually download CSV files from:
#    - NCAA.com women's basketball statistics
#    - Her Hoop Stats
#    - ESPN women's college basketball data
# 3. Place CSV files in data/raw/ folder
# 4. Modify this script to read from your CSV files
# =============================================================================
