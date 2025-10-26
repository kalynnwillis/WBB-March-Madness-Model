# =============================================================================
# Script 01: Data Collection (wehoop)
# Purpose: Pull WBB schedules/results with wehoop, clean to regular-season
# Outputs:
#   data/processed/games_cleaned.{csv,rds}
#   data/processed/bt_data.{csv,rds}
#   data/processed/tournament_seeds.{csv,rds}
#   data/processed/mid_tier_seeds.{csv,rds}
# =============================================================================

suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(here)
})

# -------- Config --------
SEASONS <- c(2023, 2024) # seasons you want in the BT fit
set.seed(479)

# -------- Ensure wehoop --------
if (!requireNamespace("wehoop", quietly = TRUE)) {
    install.packages("wehoop")
}
library(wehoop)

# -------- Helper to pick first matching column name --------
pick <- function(cands, nm) {
    hit <- intersect(cands, nm)
    if (length(hit)) hit[1] else NA_character_
}

cat("================================================================================\n")
cat("STEP 1/5: Data Collection (wehoop)\n")
cat("================================================================================\n\n")

cat("Loading WBB schedules/results with wehoop ...\n")
raw <- wehoop::load_wbb_schedule(seasons = SEASONS)
stopifnot(is.data.frame(raw), nrow(raw) > 0)

nm <- names(raw)
cat("Columns returned by wehoop:\n")
print(nm)

pick_first <- function(cands, nm) {
    hit <- intersect(cands, nm)
    if (length(hit)) hit[1] else NA_character_
}

# Dates / IDs / season
date_col <- pick_first(c("game_date", "start_date", "date"), nm)
season_col <- pick_first(c("season", "season_year"), nm)
id_col <- pick_first(c("game_id", "id", "espn_game_id"), nm)

# ✅ Team name columns (schema you showed: home_* / away_*)
home_team_col <- pick_first(c(
    "home_display_name", "home_name", "home_location", "home_short_display_name",
    # older schema fallbacks:
    "home_team", "home_team_name", "home_team_display_name", "home_team_short_display_name"
), nm)

away_team_col <- pick_first(c(
    "away_display_name", "away_name", "away_location", "away_short_display_name",
    # older schema fallbacks:
    "away_team", "away_team_name", "away_team_display_name", "away_team_short_display_name"
), nm)

# Scores
home_score_col <- pick_first(c("home_score", "home_points", "home_team_score"), nm)
away_score_col <- pick_first(c("away_score", "away_points", "away_team_score"), nm)

# Neutral + type
neutral_col <- pick_first(c("neutral_site", "neutral", "is_neutral_site", "site_neutral"), nm)
type_col <- pick_first(c("season_type", "game_type", "tournament_type", "conference_competition"), nm)

cat("\nColumn choices:\n")
print(list(
    date_col = date_col, season_col = season_col, id_col = id_col,
    home_team_col = home_team_col, away_team_col = away_team_col,
    home_score_col = home_score_col, away_score_col = away_score_col,
    neutral_col = neutral_col, type_col = type_col
))

# Fail only if truly critical fields are missing
must_have <- c(date_col, season_col, home_team_col, away_team_col)
if (any(is.na(must_have))) {
    stop("Critical columns missing after remapping. Ping me with the printed names(raw).")
}

# Build unified frame (carry season_type/neutral along)
games_combined <- raw |>
    transmute(
        game_id = if (!is.na(id_col)) as.character(.data[[id_col]]) else NA_character_,
        season = suppressWarnings(as.integer(.data[[season_col]])),
        game_date = suppressWarnings(lubridate::ymd(.data[[date_col]])),
        home_team = as.character(.data[[home_team_col]]),
        away_team = as.character(.data[[away_team_col]]),
        home_score = if (!is.na(home_score_col)) suppressWarnings(as.integer(.data[[home_score_col]])) else NA_integer_,
        away_score = if (!is.na(away_score_col)) suppressWarnings(as.integer(.data[[away_score_col]])) else NA_integer_,
        neutral_site = if (!is.na(neutral_col)) {
            val <- .data[[neutral_col]]
            as.integer(val %in% c(TRUE, 1, "1", "TRUE", "True", "true", "Neutral", "NEUTRAL"))
        } else {
            0L
        },
        season_type = if (!is.na(type_col)) .data[[type_col]] else NA, # keep original season_type
        game_type_raw = if (!is.na(type_col)) as.character(.data[[type_col]]) else NA_character_
    ) |>
    distinct(game_id, .keep_all = TRUE)

cat(sprintf("\nTotal rows pulled: %d\n", nrow(games_combined)))
cat("Sample (names only):\n")
print(head(games_combined |> select(season, game_date, home_team, away_team, home_score, away_score, neutral_site, season_type)))

# --- Regular-season filter ---
# If season_type is numeric per ESPN (1=preseason, 2=regular, 3=postseason), keep == 2.
use_numeric_regular <- is.numeric(games_combined$season_type) && any(games_combined$season_type == 2, na.rm = TRUE)

if (use_numeric_regular) {
    cat("Detected numeric season_type; filtering to season_type == 2 (regular season)\n")
    games_cleaned <- games_combined |>
        filter(season_type == 2)
} else {
    cat("No reliable numeric season_type; using date cutoff (~Mar 15) for regular season\n")
    games_cleaned <- games_combined |>
        filter(lubridate::month(game_date) < 3 | (lubridate::month(game_date) == 3 & lubridate::day(game_date) < 15))
}
# ---- Division I filter (use wehoop, not hoopR) ----
if (!requireNamespace("wehoop", quietly = TRUE)) install.packages("wehoop")
library(wehoop)

# helper to pick a column if schemas differ
pick <- function(cands, nm) {
    hit <- intersect(cands, nm)
    if (length(hit)) hit[1] else NA_character_
}
normalize_name <- function(x) stringr::str_squish(stringr::str_to_lower(x))

# pull team metadata from wehoop and auto-map the name column
wbb_teams <- wehoop::espn_wbb_teams()
nm_t <- names(wbb_teams)

name_col <- pick(c("display_name", "short_display_name", "name", "team", "school"), nm_t)
class_col <- pick(c("classification", "org", "org_type"), nm_t)
div_col <- pick(c("division", "division_name", "division_short"), nm_t)

# conservative DI filter:
wbb_di <- wbb_teams |>
    dplyr::mutate(
        .team_name = if (!is.na(name_col)) .data[[name_col]] else NA_character_,
        .class     = if (!is.na(class_col)) .data[[class_col]] else NA_character_,
        .div       = if (!is.na(div_col)) .data[[div_col]] else NA_character_
    ) |>
    # keep rows that look like NCAA Division I
    dplyr::filter(
        # classification sometimes "ncaa" or "NCAA"
        is.na(.class) | stringr::str_detect(tolower(.class), "ncaa"),
        # division sometimes "1", "I", "Division I", or missing; keep when matches I/1
        is.na(.div) | stringr::str_detect(tolower(.div), "^(1|i|division i)\\b")
    ) |>
    dplyr::transmute(
        di_team = .team_name,
        di_key = normalize_name(.team_name)
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(di_key), di_key != "")

# build keys on schedule names and keep only DI vs DI
games_combined <- games_combined |>
    dplyr::mutate(
        home_key = normalize_name(home_team),
        away_key = normalize_name(away_team)
    ) |>
    dplyr::left_join(wbb_di, by = c("home_key" = "di_key")) |>
    dplyr::rename(home_di_team = di_team) |>
    dplyr::left_join(wbb_di, by = c("away_key" = "di_key")) |>
    dplyr::rename(away_di_team = di_team) |>
    dplyr::filter(!is.na(home_di_team), !is.na(away_di_team)) |>
    dplyr::mutate(
        home_team = home_di_team,
        away_team = away_di_team
    ) |>
    dplyr::select(-home_di_team, -away_di_team, -home_key, -away_key)

# Completed games with valid scores, no ties
games_cleaned <- games_cleaned |>
    filter(!is.na(home_score), !is.na(away_score)) |>
    filter(home_score >= 0, away_score >= 0) |>
    filter(!is.na(game_date)) |>
    filter(home_score != away_score) |>
    mutate(
        home_winner = as.integer(home_score > away_score),
        away_winner = 1L - home_winner
    )

if (nrow(games_cleaned) == 0) stop("No regular-season completed games after filtering.")

cat(sprintf(
    "After basic filters: %d games, %d teams\n",
    nrow(games_cleaned),
    length(unique(c(games_cleaned$home_team, games_cleaned$away_team)))
))

# ---- Keep only largest connected component (avoid separation) ----
if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
library(igraph)

cat("Finding largest connected component...\n")
g <- graph_from_data_frame(
    games_cleaned |> distinct(home_team, away_team),
    directed = FALSE
)
comps <- components(g)
largest_comp <- which.max(comps$csize)
teams_in_cc <- names(comps$membership[comps$membership == largest_comp])

games_cleaned <- games_cleaned |>
    filter(home_team %in% teams_in_cc, away_team %in% teams_in_cc)

cat(sprintf(
    "After connected component filter: %d games, %d teams\n",
    nrow(games_cleaned),
    length(unique(c(games_cleaned$home_team, games_cleaned$away_team)))
))

# ---- Require minimum games to reduce low-information teams ----
cat("Filtering teams with < 8 games...\n")
team_game_counts <- bind_rows(
    games_cleaned |> count(team = home_team, name = "n"),
    games_cleaned |> count(team = away_team, name = "n")
) |>
    group_by(team) |>
    summarise(total_games = sum(n), .groups = "drop")

eligible_teams <- team_game_counts |>
    filter(total_games >= 8) |>
    pull(team)

games_cleaned <- games_cleaned |>
    filter(home_team %in% eligible_teams, away_team %in% eligible_teams)

cat(sprintf(
    "After min-games filter: %d games, %d teams\n",
    nrow(games_cleaned),
    length(unique(c(games_cleaned$home_team, games_cleaned$away_team)))
))

if (nrow(games_cleaned) == 0) stop("No games remain after filtering!")


# -------- Build Bradley–Terry pair table (no model fit here) --------
unique_teams <- sort(unique(c(games_cleaned$home_team, games_cleaned$away_team)))

bt_data <- games_cleaned |>
    rename(home.team = home_team, away.team = away_team) |>
    group_by(season, home.team, away.team) |>
    summarise(
        home.wins = sum(home_winner),
        away.wins = sum(away_winner),
        total_games = dplyr::n(),
        # mean home-advantage indicator for this pair (non-neutral fraction)
        home_adv_bar = mean(1L - neutral_site),
        .groups = "drop"
    ) |>
    mutate(
        home.team = factor(home.team, levels = unique_teams),
        away.team = factor(away.team, levels = unique_teams)
    )

cat(sprintf("Regular-season games kept: %d\n", nrow(games_cleaned)))
cat(sprintf(
    "Unique teams: %d | Unique matchups: %d\n",
    length(unique_teams), nrow(bt_data)
))

# -------- Seed a bracket (PLACEHOLDER from performance) --------
# ⚠️ WARNING: These seeds are synthetic (based on win %) for demonstration only!
# Real seeds should come from actual NCAA tournament bracket.
# Until you have real seeds, seed-specific claims are illustrative only.
cat("\n⚠️  Creating PLACEHOLDER bracket from win percentage (not real seeds)\n")
team_perf <- games_cleaned |>
    transmute(team = home_team, win = home_winner) |>
    bind_rows(games_cleaned |>
        transmute(team = away_team, win = away_winner)) |>
    group_by(team) |>
    summarise(games = n(), wins = sum(win), win_pct = wins / games, .groups = "drop") |>
    arrange(desc(win_pct)) |>
    slice_head(n = 64)

if (nrow(team_perf) < 64) {
    warning("Fewer than 64 teams available; seeding will be shorter than a full bracket.")
}

tournament_seeds <- team_perf |>
    mutate(
        seed = rep(1:16, length.out = n()),
        region = rep(c("Portland", "Albany", "Spokane", "Wichita"), length.out = n()),
        season = max(SEASONS)
    ) |>
    select(season, team, seed, region)

mid_tier_seeds <- tournament_seeds |>
    filter(seed >= 8, seed <= 12) |>
    mutate(seed_category = "8-12 seeds")

cat(sprintf(
    "Tournament teams (placeholder): %d | 8–12 seeds: %d\n",
    nrow(tournament_seeds), nrow(mid_tier_seeds)
))

# -------- Save outputs --------
dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

# Full raw (mapped) — optional
readr::write_csv(games_combined, here("data", "raw", "games_raw.csv"))
saveRDS(games_combined, here("data", "raw", "games_raw.rds"))

# Cleaned regular season
readr::write_csv(games_cleaned, here("data", "processed", "games_cleaned.csv"))
saveRDS(games_cleaned, here("data", "processed", "games_cleaned.rds"))

# BT pair table
readr::write_csv(bt_data, here("data", "processed", "bt_data.csv"))
saveRDS(bt_data, here("data", "processed", "bt_data.rds"))

# Bracket placeholder + mid-tier slice
readr::write_csv(tournament_seeds, here("data", "processed", "tournament_seeds.csv"))
saveRDS(tournament_seeds, here("data", "processed", "tournament_seeds.rds"))

readr::write_csv(mid_tier_seeds, here("data", "processed", "mid_tier_seeds.csv"))
saveRDS(mid_tier_seeds, here("data", "processed", "mid_tier_seeds.rds"))

# -------- Summary --------
bar <- paste(rep("=", 70), collapse = "")
cat("\n", bar, "\nDATA COLLECTION SUMMARY\n", bar, "\n", sep = "")
cat(sprintf("Seasons: %s\n", paste(SEASONS, collapse = ", ")))
cat(sprintf("Total games pulled: %d\n", nrow(games_combined)))
cat(sprintf("Regular-season games kept: %d\n", nrow(games_cleaned)))
cat(sprintf(
    "Unique teams: %d | Unique matchups: %d\n",
    length(unique_teams), nrow(bt_data)
))
cat(sprintf(
    "Tournament teams (placeholder): %d | 8–12 seeds: %d\n",
    nrow(tournament_seeds), nrow(mid_tier_seeds)
))
cat(bar, "\n✓ Data collection complete!\n", bar, "\n", sep = "")
