suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(here)
})


SEASONS <- c(2019, 2021, 2022, 2023, 2024)
set.seed(479)


if (!requireNamespace("wehoop", quietly = TRUE)) {
    install.packages("wehoop")
}
library(wehoop)

pick_first <- function(cands, nm) {
    hit <- intersect(cands, nm)
    if (length(hit)) hit[1] else NA_character_
}

raw <- wehoop::load_wbb_schedule(seasons = SEASONS)
stopifnot(is.data.frame(raw), nrow(raw) > 0)

nm <- names(raw)
cat("Columns returned by wehoop:\n")
print(nm)

date_col <- pick_first(c("game_date", "start_date", "date"), nm)
season_col <- pick_first(c("season", "season_year"), nm)
id_col <- pick_first(c("game_id", "id", "espn_game_id"), nm)


home_team_col <- pick_first(c(
    "home_display_name", "home_name", "home_location", "home_short_display_name",
    "home_team", "home_team_name", "home_team_display_name", "home_team_short_display_name"
), nm)

away_team_col <- pick_first(c(
    "away_display_name", "away_name", "away_location", "away_short_display_name",
    "away_team", "away_team_name", "away_team_display_name", "away_team_short_display_name"
), nm)


home_score_col <- pick_first(c("home_score", "home_points", "home_team_score"), nm)
away_score_col <- pick_first(c("away_score", "away_points", "away_team_score"), nm)


neutral_col <- pick_first(c("neutral_site", "neutral", "is_neutral_site", "site_neutral"), nm)
type_col <- pick_first(c("season_type", "game_type", "tournament_type", "conference_competition"), nm)


must_have <- c(date_col, season_col, home_team_col, away_team_col)
if (any(is.na(must_have))) {
    stop("Critical columns missing after remapping. Ping me with the printed names(raw).")
}

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


print(head(games_combined |> select(season, game_date, home_team, away_team, home_score, away_score, neutral_site, season_type)))

# ====== D-I NORMALIZATION (BEFORE ANY FILTERING) ======
normalize_name <- function(x) stringr::str_squish(stringr::str_to_lower(x))

n_games_before_d1 <- nrow(games_combined)

d1_teams <- tryCatch(
    suppressMessages(purrr::map_dfr(SEASONS, wehoop::espn_wbb_teams)),
    error = function(e) NULL
)

if (is.null(d1_teams) || !"team_id" %in% names(d1_teams)) {
    cat("⚠️  teams endpoint unavailable or schema changed; using name-normalized + connectivity filter only\n")
    games_norm <- games_combined
} else {
    cat("✓ Successfully pulled D1 teams metadata\n")
    nm_t <- names(d1_teams)

    name_col <- pick_first(c("display_name", "short_display_name", "name", "team", "school"), nm_t)
    class_col <- pick_first(c("classification", "org", "org_type"), nm_t)
    div_col <- pick_first(c("division", "division_name", "division_short"), nm_t)

    wbb_di <- d1_teams |>
        dplyr::mutate(
            .team_name = if (!is.na(name_col)) .data[[name_col]] else NA_character_,
            .class     = if (!is.na(class_col)) .data[[class_col]] else NA_character_,
            .div       = if (!is.na(div_col)) .data[[div_col]] else NA_character_
        ) |>
        dplyr::filter(
            is.na(.class) | stringr::str_detect(tolower(.class), "ncaa"),
            is.na(.div) | stringr::str_detect(tolower(.div), "^(1|i|division i)\\b")
        ) |>
        dplyr::transmute(
            di_team = .team_name,
            di_key = normalize_name(.team_name)
        ) |>
        dplyr::distinct() |>
        dplyr::filter(!is.na(di_key), di_key != "")

    # Apply D-I normalization and filtering
    games_norm <- games_combined |>
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

    n_games_after_d1 <- nrow(games_norm)
    cat(sprintf(
        "D-I filter: %d games → %d games (%.1f%% kept)\n",
        n_games_before_d1, n_games_after_d1,
        100 * n_games_after_d1 / n_games_before_d1
    ))
}

# ====== REGULAR-SEASON FILTER  ======
stype <- suppressWarnings(as.integer(games_norm$season_type))

if (!all(is.na(stype)) && any(stype == 2, na.rm = TRUE)) {
    # Numeric season_type: 2 = regular season
    cat("Using numeric season_type filter (2 = regular season)\n")
    games_cleaned <- games_norm |> filter(stype == 2)
} else {
    cat("Using text/date-based season filter\n")
    stxt <- tolower(as.character(games_norm$season_type))
    stxt[is.na(stxt)] <- ""

    games_cleaned <- games_norm |>
        filter(
            stringr::str_detect(stxt, "regular|season\\b") |
                (!stringr::str_detect(stxt, "post|tourn|conf|champ|playoff") &
                    lubridate::month(game_date) <= 2)
        )
}

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


if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
library(igraph)

g <- graph_from_data_frame(
    games_cleaned |> distinct(home_team, away_team),
    directed = FALSE
)
comps <- components(g)
largest_comp <- which.max(comps$csize)
teams_in_cc <- names(comps$membership[comps$membership == largest_comp])

games_cleaned <- games_cleaned |>
    filter(home_team %in% teams_in_cc, away_team %in% teams_in_cc)


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

season_summary <- games_cleaned %>%
    group_by(season) %>%
    summarise(
        n_games = n(),
        n_teams = length(unique(c(home_team, away_team))),
        .groups = "drop"
    )
print(season_summary)

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
        home_adv_bar = mean(1L - neutral_site),
        .groups = "drop"
    ) |>
    mutate(
        home.team = factor(home.team, levels = unique_teams),
        away.team = factor(away.team, levels = unique_teams)
    )


# ====== TOURNAMENT SEEDS: Real NCAA Data vs Model-Based ======
ncaa_seed_file <- here("data", "raw", "ncaa_seeds_historical.csv")

if (file.exists(ncaa_seed_file)) {
    cat("\n✓ Loading REAL NCAA tournament seeds from file\n")

    tournament_seeds <- read_csv(ncaa_seed_file, show_col_types = FALSE) |>
        filter(season == max(SEASONS)) |> # Use most recent season
        select(team, seed = ncaa_seed, region) |>
        mutate(
            team = str_trim(team),
            season = max(SEASONS)
        )

    available_teams <- unique(c(games_cleaned$home_team, games_cleaned$away_team))
    missing_teams <- tournament_seeds |>
        filter(!team %in% available_teams) |>
        pull(team)


    tournament_seeds <- tournament_seeds |>
        filter(team %in% available_teams)

    team_perf <- games_cleaned |>
        transmute(team = home_team, win = home_winner, season = season) |>
        bind_rows(games_cleaned |>
            transmute(team = away_team, win = away_winner, season = season)) |>
        group_by(team) |>
        summarise(
            games = n(),
            wins = sum(win),
            win_pct = wins / games,
            n_seasons = n_distinct(season),
            .groups = "drop"
        ) |>
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
}

mid_tier_seeds <- tournament_seeds |>
    filter(seed >= 8, seed <= 12) |>
    mutate(seed_category = "8-12 seeds")

seed_type <- if (file.exists(ncaa_seed_file)) "NCAA committee seeds" else "model-based ranks"

dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(games_combined, here("data", "raw", "games_raw.csv"))
saveRDS(games_combined, here("data", "raw", "games_raw.rds"))

readr::write_csv(games_cleaned, here("data", "processed", "games_cleaned.csv"))
saveRDS(games_cleaned, here("data", "processed", "games_cleaned.rds"))

readr::write_csv(bt_data, here("data", "processed", "bt_data.csv"))
saveRDS(bt_data, here("data", "processed", "bt_data.rds"))

readr::write_csv(tournament_seeds, here("data", "processed", "tournament_seeds.csv"))
saveRDS(tournament_seeds, here("data", "processed", "tournament_seeds.rds"))

readr::write_csv(mid_tier_seeds, here("data", "processed", "mid_tier_seeds.csv"))
saveRDS(mid_tier_seeds, here("data", "processed", "mid_tier_seeds.rds"))
