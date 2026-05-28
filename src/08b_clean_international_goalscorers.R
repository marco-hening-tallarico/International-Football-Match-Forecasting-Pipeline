# ============================================================
# 08b_clean_international_goalscorers.R
# Clean martj42/international_results goalscorers into one table
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

goalscorers_raw_path <- file.path(
    RAW_DIR,
    "international_results",
    "goalscorers.csv"
)
international_results_path <- file.path(
    PROCESSED_DIR,
    "international_results.csv"
)

if (!file.exists(goalscorers_raw_path)) {
    stop(
        "Missing international goalscorers file. ",
        "Run 07_download_international_results.R first."
    )
}

if (!file.exists(international_results_path)) {
    stop(
        "Missing processed international_results.csv. ",
        "Run 08_clean_international_results.R first."
    )
}

make_id_part <- function(x) {
    x |>
        as.character() |>
        stringr::str_squish() |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("[^a-z0-9]+", "_") |>
        stringr::str_replace_all("^_|_$", "")
}

build_source_match_key <- function(date, home_team, away_team) {
    paste(
        make_id_part(date),
        make_id_part(home_team),
        make_id_part(away_team),
        sep = "_"
    )
}

parse_logical_field <- function(x) {
    if (is.logical(x)) {
        return(x)
    }

    normalized <- stringr::str_to_lower(stringr::str_squish(as.character(x)))

    dplyr::case_when(
        normalized %in% c("true", "t", "1", "yes", "y") ~ TRUE,
        normalized %in% c("false", "f", "0", "no", "n") ~ FALSE,
        is.na(normalized) | normalized == "" ~ NA,
        TRUE ~ NA
    )
}

goalscorers_raw <- readr::read_csv(
    goalscorers_raw_path,
    show_col_types = FALSE
) |>
    janitor::clean_names()

required_raw_cols <- c(
    "date",
    "home_team",
    "away_team",
    "team",
    "scorer",
    "minute",
    "own_goal",
    "penalty"
)

missing_required <- setdiff(required_raw_cols, names(goalscorers_raw))

if (length(missing_required) > 0) {
    stop(
        "International goalscorers file is missing required columns: ",
        paste(missing_required, collapse = ", ")
    )
}

international_results <- readr::read_csv(
    international_results_path,
    show_col_types = FALSE
) |>
    janitor::clean_names() |>
    dplyr::transmute(
        date = suppressWarnings(as.Date(date)),
        home_team = stringr::str_squish(as.character(home_team)),
        away_team = stringr::str_squish(as.character(away_team))
    ) |>
    dplyr::distinct()

goalscorers_normalized <- goalscorers_raw |>
    dplyr::mutate(
        source = as.character("international_results"),
        raw_file = as.character(goalscorers_raw_path),
        date = suppressWarnings(as.Date(date)),
        home_team = stringr::str_squish(as.character(home_team)),
        away_team = stringr::str_squish(as.character(away_team)),
        team = stringr::str_squish(as.character(team)),
        scorer = stringr::str_squish(as.character(scorer)),
        minute_raw = minute,
        minute = safe_integer(minute),
        own_goal = parse_logical_field(own_goal),
        penalty = parse_logical_field(penalty)
    )

bad_identity <- goalscorers_normalized |>
    dplyr::filter(
        is.na(date) |
            is.na(home_team) | home_team == "" |
            is.na(away_team) | away_team == ""
    )

if (nrow(bad_identity) > 0) {
    stop(
        "International goalscorers has missing required identity values. ",
        "Bad rows: ",
        nrow(bad_identity)
    )
}

goalscorers_with_keys <- goalscorers_normalized |>
    dplyr::mutate(
        source_match_key = build_source_match_key(date, home_team, away_team),
        notes = dplyr::case_when(
            is.na(minute) & !is.na(minute_raw) ~ "minute_not_integer",
            TRUE ~ NA_character_
        )
    ) |>
    dplyr::group_by(source_match_key) |>
    dplyr::mutate(
        source_goal_id = paste0(
            source_match_key,
            "_goal_",
            sprintf("%04d", dplyr::row_number())
        )
    ) |>
    dplyr::ungroup()

results_match_lookup <- international_results |>
    dplyr::mutate(match_found_in_results = TRUE)

goalscorers_joined <- goalscorers_with_keys |>
    dplyr::left_join(
        results_match_lookup,
        by = c("date", "home_team", "away_team")
    ) |>
    dplyr::mutate(
        match_found_in_results = dplyr::coalesce(match_found_in_results, FALSE)
    )

international_goalscorers <- goalscorers_joined |>
    dplyr::select(
        source,
        raw_file,
        source_goal_id,
        source_match_key,
        date,
        home_team,
        away_team,
        team,
        scorer,
        minute,
        own_goal,
        penalty,
        match_found_in_results,
        notes
    )

if (nrow(international_goalscorers) == 0L) {
    stop(
        "Processed international_goalscorers.csv would be empty ",
        "although raw goalscorers input exists."
    )
}

unmatched_matches <- international_goalscorers |>
    dplyr::filter(!match_found_in_results) |>
    dplyr::distinct(date, home_team, away_team, source_match_key) |>
    dplyr::arrange(date, home_team, away_team)

cleaning_summary <- tibble::tibble(
    metric = c(
        "raw_rows",
        "processed_rows",
        "distinct_matches_in_raw",
        "distinct_matches_matched_in_results",
        "distinct_matches_unmatched_in_results",
        "rows_unmatched_in_results",
        "rows_with_non_integer_minute",
        "rows_with_missing_own_goal",
        "rows_with_missing_penalty"
    ),
    value = c(
        nrow(goalscorers_raw),
        nrow(international_goalscorers),
        dplyr::n_distinct(
            goalscorers_with_keys$source_match_key
        ),
        dplyr::n_distinct(
            international_goalscorers$source_match_key[
                international_goalscorers$match_found_in_results
            ]
        ),
        nrow(unmatched_matches),
        sum(!international_goalscorers$match_found_in_results),
        sum(international_goalscorers$notes == "minute_not_integer", na.rm = TRUE),
        sum(is.na(international_goalscorers$own_goal)),
        sum(is.na(international_goalscorers$penalty))
    )
)

goalscorers_out <- file.path(PROCESSED_DIR, "international_goalscorers.csv")
summary_out <- file.path(
    VALIDATION_DIR,
    "international_goalscorers_cleaning_summary.csv"
)
unmatched_out <- file.path(
    VALIDATION_DIR,
    "international_goalscorers_unmatched_matches.csv"
)

readr::write_csv(international_goalscorers, goalscorers_out)
readr::write_csv(cleaning_summary, summary_out)
readr::write_csv(unmatched_matches, unmatched_out)

message("Done.")
message("International goalscorers cleaned rows: ", nrow(international_goalscorers))
message("Unmatched match keys: ", nrow(unmatched_matches))
message("Output: ", goalscorers_out)
message("Cleaning summary: ", summary_out)
message("Unmatched matches: ", unmatched_out)
