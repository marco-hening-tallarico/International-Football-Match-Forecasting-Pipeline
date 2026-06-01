# 02_helpers.R
#
# Shared helpers for downloads, CSV/JSON reads, manifest logging, and
# team-name normalization. No standalone outputs.

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


CHRONOLOGICAL_TEST_SPLIT_DATE <- as.Date("2018-01-01")
DEFAULT_VALIDATION_FRACTION <- 0.20

# Explicit FIFA / results names -> World Football Elo team labels.
# Applied only when the Elo team exists in cleaned ratings (script 16).
STANDARD_RESULT_TO_ELO_TEAM_MAPPINGS <- c(
    "China PR" = "China",
    "Czech Republic" = "Czechia",
    "Czechoslovakia" = "Czechia",
    "Republic of Ireland" = "Ireland",
    "Timor-Leste" = "East Timor",
    "German DR" = "Germany",
    "South Yemen" = "Yemen",
    "Yemen DPR" = "Yemen",
    "Vietnam Republic" = "Vietnam",
    "Yugoslavia" = "Serbia"
)


make_team_clean <- function(team_name) {
    team_name |>
        as.character() |>
        stringr::str_squish() |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("[^a-z0-9]+", "_") |>
        stringr::str_replace_all("^_|_$", "")
}


load_team_name_crosswalk <- function(
    crosswalk_path = file.path(META_DIR, "team_name_crosswalk.csv")
) {
    optional_cols <- c("elo_team", "elo_team_clean")
    base_cols <- c(
        "source",
        "raw_team",
        "team_clean",
        "canonical_team",
        "notes"
    )

    if (!file.exists(crosswalk_path)) {
        empty <- tibble::tibble(
            source = character(),
            raw_team = character(),
            team_clean = character(),
            canonical_team = character(),
            notes = character()
        )
        empty$elo_team <- character()
        empty$elo_team_clean <- character()
        return(empty)
    }

    crosswalk <- readr::read_csv(crosswalk_path, show_col_types = FALSE)
    missing_base <- setdiff(base_cols, names(crosswalk))

    if (length(missing_base) > 0L) {
        stop(
            "team_name_crosswalk.csv is missing columns: ",
            paste(missing_base, collapse = ", "),
            call. = FALSE
        )
    }

    for (col_name in optional_cols) {
        if (!col_name %in% names(crosswalk)) {
            crosswalk[[col_name]] <- NA_character_
        }
    }

    crosswalk
}


apply_standard_result_to_elo_mappings <- function(
    crosswalk,
    available_elo_teams
) {
    if (length(available_elo_teams) == 0L) {
        return(crosswalk)
    }

    available_elo_clean <- make_team_clean(available_elo_teams)
    names(available_elo_clean) <- available_elo_teams

    for (result_team in names(STANDARD_RESULT_TO_ELO_TEAM_MAPPINGS)) {
        elo_team <- STANDARD_RESULT_TO_ELO_TEAM_MAPPINGS[[result_team]]
        elo_team_clean <- make_team_clean(elo_team)

        if (!elo_team_clean %in% available_elo_clean) {
            next
        }

        row_idx <- which(crosswalk$raw_team == result_team)
        mapping_notes <- paste0(
            "mapped_to_elo:",
            elo_team
        )

        if (length(row_idx) == 0L) {
            crosswalk <- dplyr::bind_rows(
                crosswalk,
                tibble::tibble(
                    source = "international_results",
                    raw_team = result_team,
                    team_clean = make_team_clean(result_team),
                    canonical_team = result_team,
                    notes = mapping_notes,
                    elo_team = elo_team,
                    elo_team_clean = elo_team_clean
                )
            )
        } else {
            crosswalk$elo_team[row_idx] <- elo_team
            crosswalk$elo_team_clean[row_idx] <- elo_team_clean
            for (idx in row_idx) {
                old_note <- crosswalk$notes[idx]
                if (
                    is.na(old_note) ||
                        old_note == "" ||
                        old_note == "auto_initialized"
                ) {
                    crosswalk$notes[idx] <- mapping_notes
                }
            }
        }
    }

    crosswalk |>
        dplyr::distinct(.data$source, .data$raw_team, .keep_all = TRUE) |>
        dplyr::arrange(.data$source, .data$team_clean, .data$raw_team)
}


build_result_to_elo_clean_lookup <- function(crosswalk) {
    explicit <- crosswalk |>
        dplyr::filter(
            !is.na(.data$raw_team),
            .data$raw_team != "",
            !is.na(.data$elo_team_clean),
            .data$elo_team_clean != ""
        ) |>
        dplyr::transmute(
            raw_team = .data$raw_team,
            elo_team_clean = .data$elo_team_clean
        ) |>
        dplyr::distinct(.data$raw_team, .keep_all = TRUE)

    stats::setNames(explicit$elo_team_clean, explicit$raw_team)
}


resolve_elo_team_clean <- function(
    team_name,
    crosswalk_lookup = character(),
    available_elo_team_clean = character()
) {
    team_name <- as.character(team_name)
    result_team_clean <- make_team_clean(team_name)
    elo_lookup_team_clean <- result_team_clean

    direct_hit <- result_team_clean %in% available_elo_team_clean
    elo_lookup_team_clean[direct_hit] <- result_team_clean[direct_hit]

    if (length(crosswalk_lookup) > 0L) {
        crosswalk_hit <- team_name %in% names(crosswalk_lookup)
        elo_lookup_team_clean[crosswalk_hit] <- unname(
            crosswalk_lookup[team_name[crosswalk_hit]]
        )
    }

    elo_lookup_team_clean
}


assign_data_split_modeling <- function(
    modeling_table,
    validation_fraction = DEFAULT_VALIDATION_FRACTION
) {
    if (!"data_split" %in% names(modeling_table)) {
        stop(
            "modeling_table must include data_split before assigning data_split_modeling.",
            call. = FALSE
        )
    }

    train_rows <- modeling_table |>
        dplyr::filter(.data$data_split == "train") |>
        dplyr::arrange(.data$date)

    validation_start_index <- floor(nrow(train_rows) * (1 - validation_fraction)) + 1L
    validation_ids <- character()
    train_modeling_ids <- character()

    if (nrow(train_rows) > 0L) {
        if (validation_start_index <= nrow(train_rows)) {
            validation_ids <- train_rows$source_match_id[
                validation_start_index:nrow(train_rows)
            ]
            train_modeling_ids <- train_rows$source_match_id[
                seq_len(validation_start_index - 1L)
            ]
        } else {
            train_modeling_ids <- train_rows$source_match_id
        }
    }

    modeling_table |>
        dplyr::mutate(
            data_split_modeling = dplyr::case_when(
                .data$data_split == "test" ~ "test",
                .data$source_match_id %in% validation_ids ~ "validation",
                .data$source_match_id %in% train_modeling_ids ~ "train",
                .data$data_split == "train" ~ "train",
                TRUE ~ NA_character_
            )
        )
}


make_chronological_modeling_splits <- function(
    modeling_df,
    validation_fraction = DEFAULT_VALIDATION_FRACTION
) {
    if ("data_split_modeling" %in% names(modeling_df)) {
        train <- modeling_df |>
            dplyr::filter(.data$data_split_modeling == "train") |>
            dplyr::arrange(.data$date)
        validation <- modeling_df |>
            dplyr::filter(.data$data_split_modeling == "validation") |>
            dplyr::arrange(.data$date)
        test <- modeling_df |>
            dplyr::filter(.data$data_split_modeling == "test") |>
            dplyr::arrange(.data$date)
    } else {
        train_all <- modeling_df |>
            dplyr::filter(.data$data_split == "train") |>
            dplyr::arrange(.data$date)
        test <- modeling_df |>
            dplyr::filter(.data$data_split == "test") |>
            dplyr::arrange(.data$date)

        if (nrow(train_all) == 0L || nrow(test) == 0L) {
            stop("Train or test split is empty after filtering.", call. = FALSE)
        }

        validation_start_index <- floor(nrow(train_all) * (1 - validation_fraction)) + 1L

        train <- train_all[seq_len(validation_start_index - 1L), ]
        validation <- train_all[validation_start_index:nrow(train_all), ]
    }

    if (nrow(train) == 0L || nrow(validation) == 0L || nrow(test) == 0L) {
        stop("Train, validation, or test split is empty.", call. = FALSE)
    }

    list(
        train = train,
        validation = validation,
        test = test
    )
}


message("Helper functions loaded.")
