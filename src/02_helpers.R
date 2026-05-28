# ============================================================
# 02_helpers.R
# Reusable helper functions for downloads, reading files,
# and tracking data provenance
# ============================================================

safe_download <- function(url, destfile, overwrite = FALSE) {
    dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

    if (file.exists(destfile) && !overwrite) {
        message("Already exists, skipping: ", destfile)
        return(TRUE)
    }

    message("Downloading: ", url)

    ok <- tryCatch(
        {
            download.file(url, destfile, mode = "wb", quiet = TRUE)
            TRUE
        },
        error = function(e) {
            warning("Download failed: ", url, "\nReason: ", conditionMessage(e))
            FALSE
        }
    )

    return(ok)
}


safe_read_json <- function(path) {
    tryCatch(
        jsonlite::fromJSON(path, flatten = TRUE),
        error = function(e) {
            warning("Could not read JSON: ", path, "\nReason: ", conditionMessage(e))
            NULL
        }
    )
}


safe_read_csv <- function(path) {
    tryCatch(
        readr::read_csv(path, show_col_types = FALSE),
        error = function(e) {
            warning("Could not read CSV: ", path, "\nReason: ", conditionMessage(e))
            NULL
        }
    )
}


processed_csv_col_types <- function(path) {
    table_name <- basename(path)

    common_match_cols <- list(
        source = readr::col_character(),
        raw_file = readr::col_character(),
        source_match_id = readr::col_character(),
        date = readr::col_date(),
        season = readr::col_character(),
        competition = readr::col_character(),
        home_team = readr::col_character(),
        away_team = readr::col_character(),
        home_score = readr::col_integer(),
        away_score = readr::col_integer(),
        match_result = readr::col_character(),
        result_class = readr::col_integer(),
        home_win = readr::col_integer(),
        draw = readr::col_integer(),
        away_win = readr::col_integer(),
        goal_difference = readr::col_integer(),
        total_goals = readr::col_integer(),
        neutral = readr::col_logical()
    )

    if (identical(table_name, "statsbomb_competitions.csv")) {
        return(readr::cols(
            competition_id = readr::col_integer(),
            season_id = readr::col_integer(),
            country_name = readr::col_character(),
            competition_name = readr::col_character(),
            competition_gender = readr::col_character(),
            competition_youth = readr::col_logical(),
            competition_international = readr::col_logical(),
            season_name = readr::col_character(),
            match_updated = readr::col_character(),
            match_updated_360 = readr::col_character(),
            match_available_360 = readr::col_character(),
            match_available = readr::col_character()
        ))
    }

    if (identical(table_name, "statsbomb_matches.csv")) {
        return(do.call(readr::cols, c(common_match_cols, list(
            competition_id = readr::col_integer(),
            season_id = readr::col_integer(),
            match_week = readr::col_integer(),
            competition_country = readr::col_character(),
            stadium = readr::col_character(),
            data_version = readr::col_character(),
            shot_fidelity_version = readr::col_character(),
            xy_fidelity_version = readr::col_character()
        ))))
    }

    football_data_uk_match_stats <- list(
        half_time_home_score = readr::col_double(),
        half_time_away_score = readr::col_double(),
        home_shots = readr::col_double(),
        away_shots = readr::col_double(),
        home_shots_on_target = readr::col_double(),
        away_shots_on_target = readr::col_double(),
        home_corners = readr::col_double(),
        away_corners = readr::col_double(),
        home_fouls = readr::col_double(),
        away_fouls = readr::col_double(),
        home_yellow_cards = readr::col_double(),
        away_yellow_cards = readr::col_double(),
        home_red_cards = readr::col_double(),
        away_red_cards = readr::col_double(),
        source_league_code = readr::col_character(),
        source_season_code = readr::col_character(),
        full_time_result = readr::col_character(),
        half_time_result = readr::col_character()
    )

    if (identical(table_name, "football_data_uk_matches.csv") ||
        identical(table_name, "football_data_uk_matches_core.csv")) {
        return(do.call(readr::cols, c(common_match_cols, football_data_uk_match_stats)))
    }

    if (identical(table_name, "football_data_uk_odds_wide.csv")) {
        return(readr::cols(
            source_match_id = readr::col_character(),
            raw_file = readr::col_character(),
            source_league_code = readr::col_character(),
            source_season_code = readr::col_character(),
            date = readr::col_date(),
            home_team = readr::col_character(),
            away_team = readr::col_character(),
            avg_home_odds = readr::col_double(),
            avg_draw_odds = readr::col_double(),
            avg_away_odds = readr::col_double(),
            max_home_odds = readr::col_double(),
            max_draw_odds = readr::col_double(),
            max_away_odds = readr::col_double(),
            closing_home_odds = readr::col_double(),
            closing_draw_odds = readr::col_double(),
            closing_away_odds = readr::col_double(),
            home_implied_prob = readr::col_double(),
            draw_implied_prob = readr::col_double(),
            away_implied_prob = readr::col_double(),
            market_overround = readr::col_double(),
            .default = readr::col_guess()
        ))
    }

    if (identical(table_name, "international_results.csv")) {
        return(do.call(readr::cols, c(common_match_cols, list(
            tournament = readr::col_character(),
            city = readr::col_character(),
            country = readr::col_character()
        ))))
    }

    readr::cols()
}


read_processed_csv <- function(path) {
    readr::read_csv(
        path,
        col_types = processed_csv_col_types(path),
        show_col_types = FALSE
    )
}


create_empty_manifest <- function() {
    tibble::tibble(
        source = character(),
        dataset = character(),
        url = character(),
        local_path = character(),
        downloaded_at = character(),
        notes = character()
    )
}


read_or_create_manifest <- function(meta_dir) {
    manifest_path <- file.path(meta_dir, "source_manifest.csv")

    if (file.exists(manifest_path)) {
        readr::read_csv(
            manifest_path,
            col_types = readr::cols(.default = readr::col_character())
        )
    } else {
        create_empty_manifest()
    }
}


add_manifest_record <- function(source, dataset, url, local_path, notes = NA_character_) {
    tibble::tibble(
        source = source,
        dataset = dataset,
        url = as.character(url),
        local_path = as.character(local_path),
        downloaded_at = as.character(Sys.time()),
        notes = notes
    )
}


write_source_manifest <- function(records, meta_dir) {
    out <- file.path(meta_dir, "source_manifest.csv")

    records_clean <- records |>
        dplyr::distinct(source, dataset, url, local_path, .keep_all = TRUE) |>
        dplyr::arrange(source, dataset, local_path)

    readr::write_csv(records_clean, out)

    message("Wrote manifest: ", out)
    invisible(out)
}


parse_fd_uk_date <- function(x) {
    lubridate::parse_date_time(
        x,
        orders = c("dmy", "dmY", "ymd", "Ymd"),
        quiet = TRUE
    ) |>
        as.Date()
}


safe_integer <- function(x) {
    suppressWarnings(as.integer(x))
}


message("Helper functions loaded.")
