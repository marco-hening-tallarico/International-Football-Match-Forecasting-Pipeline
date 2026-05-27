fd_uk <- readr::read_csv(
    "data/processed/football_data_uk_matches.csv",
    show_col_types = FALSE
)

dplyr::glimpse(fd_uk)

fd_uk |>
    dplyr::count(source_league_code, sort = TRUE)

fd_uk |>
    dplyr::summarise(
        rows = dplyr::n(),
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE),
        competitions = dplyr::n_distinct(competition),
        teams = dplyr::n_distinct(c(home_team, away_team)),
        missing_scores = sum(is.na(home_score) | is.na(away_score)),
        missing_dates = sum(is.na(date))
    )

fd_uk |>
    dplyr::count(match_result)


fd_uk |>
    dplyr::summarise(
        rows = dplyr::n(),
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE),
        leagues = dplyr::n_distinct(source_league_code),
        seasons = dplyr::n_distinct(source_season_code),
        home_teams = dplyr::n_distinct(home_team),
        away_teams = dplyr::n_distinct(away_team),
        missing_dates = sum(is.na(date)),
        missing_home_teams = sum(is.na(home_team) | home_team == ""),
        missing_away_teams = sum(is.na(away_team) | away_team == ""),
        missing_scores = sum(is.na(home_score) | is.na(away_score))
    )


fd_uk |>
    dplyr::count(match_result)

fd_uk |>
    dplyr::count(source_league_code, sort = TRUE)

fd_uk |>
    dplyr::summarise(
        bad_home_win = sum(home_win != as.integer(match_result == "home_win")),
        bad_draw = sum(draw != as.integer(match_result == "draw")),
        bad_away_win = sum(away_win != as.integer(match_result == "away_win")),
        bad_goal_difference = sum(goal_difference != home_score - away_score),
        bad_total_goals = sum(total_goals != home_score + away_score)
    )