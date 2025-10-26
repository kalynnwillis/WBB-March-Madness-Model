# Script 01 (wncaahoopR-only): build a game-results table from PBP
suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
})

# install.packages("devtools"); devtools::install_github("snestler/wncaahoopR")
library(wncaahoopR)

# 1) Pick a date window spanning regular season (adjust years as needed)
dates <- seq.Date(as.Date("2024-11-01"), as.Date("2025-03-15"), by = "day")

safe_sched <- purrr::possibly(w_get_master_schedule, otherwise = NULL)
sched <- purrr::map_dfr(dates, ~ {
    df <- safe_sched(format(.x, "%Y-%m-%d"))
    if (is.null(df)) tibble() else df |> mutate(game_date = .x)
})

# Expect a column with ESPN game IDs (commonly "game_id"); be defensive:
id_col <- intersect(names(sched), c("game_id", "gameid", "gameId"))[1]
home_col <- intersect(names(sched), c("home_team", "home", "homeTeam"))[1]
away_col <- intersect(names(sched), c("away_team", "away", "awayTeam"))[1]

stopifnot(!is.na(id_col), !is.na(home_col), !is.na(away_col))
game_ids <- sched[[id_col]] |>
    unique() |>
    na.omit()

# 2) Pull PBP for all games (this can take time; throttle if ESPN rate-limits)
safe_pbp <- purrr::safely(w_get_pbp_game, quiet = TRUE)
pbp_list <- purrr::map(game_ids, ~ safe_pbp(.x))

# 3) Extract winners from each PBP tibble
extract_result <- function(gid, res) {
    if (!is.null(res$error) || is.null(res$result) || nrow(res$result) == 0) {
        return(tibble(game_id = gid, ok = FALSE))
    }
    df <- res$result
    # Try to find team names and evolving scores; adapt to column names
    hcol <- intersect(names(df), c("home_team", "home", "homeTeam"))[1]
    acol <- intersect(names(df), c("away_team", "away", "awayTeam"))[1]
    hscol <- intersect(names(df), c("home_score", "homeScore", "home_points"))[1]
    ascol <- intersect(names(df), c("away_score", "awayScore", "away_points"))[1]

    # Last non-NA score row is the final
    df2 <- df |> filter(!is.na(.data[[hscol]]), !is.na(.data[[ascol]]))
    if (nrow(df2) == 0) {
        return(tibble(game_id = gid, ok = FALSE))
    }
    last <- df2 |> slice_tail(n = 1)

    tibble(
        game_id = gid,
        game_date = suppressWarnings(as.Date(last$game_date %||% NA)),
        home_team = as.character(last[[hcol]]),
        away_team = as.character(last[[acol]]),
        home_score = as.integer(last[[hscol]]),
        away_score = as.integer(last[[ascol]]),
        home_winner = as.integer(last[[hscol]] > last[[ascol]]),
        away_winner = as.integer(last[[ascol]] > last[[hscol]]),
        ok = TRUE
    )
}

games_from_pbp <- purrr::map2_dfr(game_ids, pbp_list, ~ extract_result(.x, .y))

# 4) Clean and keep regular-season-ish games before Selection Sunday
games_cleaned <- games_from_pbp |>
    filter(ok, !is.na(home_score), !is.na(away_score)) |>
    mutate(
        game_date = coalesce(
            game_date,
            sched$game_date[match(game_id, sched[[id_col]])]
        )
    ) |>
    filter(!is.na(game_date)) |>
    filter(month(game_date) < 3 | (month(game_date) == 3 & day(game_date) < 15))

# Save for your BT script
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(games_cleaned, "data/processed/games_cleaned.csv")
saveRDS(games_cleaned, "data/processed/games_cleaned.rds")

cat(glue::glue("OK: {nrow(games_cleaned)} games with final scores\n"))
