# 08c_clean_international_shootouts.R
#
# Cleans martj42/international_results penalty-shootout records into one
# table for joining onto match results.
#
# Reads: data/raw/international_results/shootouts.csv
#
# Writes: data/processed/international_shootouts.csv

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

shootouts_raw_path <- file.path(
    RAW_DIR,
    "international_results",
    "shootouts.csv"
)
international_results_path <- file.path(
    PROCESSED_DIR,
    "international_results.csv"
)

if (!file.exists(shootouts_raw_path)) {
    stop(
        "Missing international shootouts file. ",
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

shootouts_raw <- readr::read_csv(
    shootouts_raw_path,
    show_col_types = FALSE
) |>
    janitor::clean_names()

required_raw_cols <- c(
    "date",
    "home_team",
    "away_team",
    "winner",
    "first_shooter"
)

missing_required <- setdiff(required_raw_cols, names(shootouts_raw))

if (length(missing_required) > 0) {
    stop(
        "International shootouts file is missing required columns: ",
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

shootouts_normalized <- shootouts_raw |>
    dplyr::mutate(
        source = as.character("international_results"),
        raw_file = as.character(shootouts_raw_path),
        date = suppressWarnings(as.Date(date)),
        home_team = stringr::str_squish(as.character(home_team)),
        away_team = stringr::str_squish(as.character(away_team)),
        winner = stringr::str_squish(as.character(winner)),
        first_shooter = stringr::str_squish(as.character(first_shooter)),
        first_shooter = dplyr::if_else(
            first_shooter == "",
            NA_character_,
            first_shooter
        )
    )

bad_identity <- shootouts_normalized |>
    dplyr::filter(
        is.na(date) |
            is.na(home_team) | home_team == "" |
            is.na(away_team) | away_team == ""
    )

if (nrow(bad_identity) > 0) {
    stop(
        "International shootouts has missing required identity values. ",
        "Bad rows: ",
        nrow(bad_identity)
    )
}

shootouts_with_keys <- shootouts_normalized |>
    dplyr::mutate(
        source_match_key = build_source_match_key(date, home_team, away_team),
        notes = NA_character_
    ) |>
    dplyr::group_by(source_match_key) |>
    dplyr::mutate(
        source_shootout_id = paste0(
            source_match_key,
            "_shootout_",
            sprintf("%03d", dplyr::row_number())
        )
    ) |>
    dplyr::ungroup()

results_match_lookup <- international_results |>
    dplyr::mutate(match_found_in_results = TRUE)

shootouts_joined <- shootouts_with_keys |>
    dplyr::left_join(
        results_match_lookup,
        by = c("date", "home_team", "away_team")
    ) |>
    dplyr::mutate(
        match_found_in_results = dplyr::coalesce(match_found_in_results, FALSE)
    )

international_shootouts <- shootouts_joined |>
    dplyr::select(
        source,
        raw_file,
        source_shootout_id,
        source_match_key,
        date,
        home_team,
        away_team,
        winner,
        first_shooter,
        match_found_in_results,
        notes
    )

if (nrow(international_shootouts) == 0L) {
    stop(
        "Processed international_shootouts.csv would be empty ",
        "although raw shootouts input exists."
    )
}

unmatched_matches <- international_shootouts |>
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
        "rows_with_missing_winner",
        "rows_with_missing_first_shooter"
    ),
    value = c(
        nrow(shootouts_raw),
        nrow(international_shootouts),
        dplyr::n_distinct(shootouts_with_keys$source_match_key),
        dplyr::n_distinct(
            international_shootouts$source_match_key[
                international_shootouts$match_found_in_results
            ]
        ),
        nrow(unmatched_matches),
        sum(!international_shootouts$match_found_in_results),
        sum(
            is.na(international_shootouts$winner) |
                international_shootouts$winner == ""
        ),
        sum(is.na(international_shootouts$first_shooter))
    )
)

shootouts_out <- file.path(PROCESSED_DIR, "international_shootouts.csv")
summary_out <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_shootouts_cleaning_summary.csv"
)
unmatched_out <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_shootouts_unmatched_matches.csv"
)

readr::write_csv(international_shootouts, shootouts_out)
readr::write_csv(cleaning_summary, summary_out)
readr::write_csv(unmatched_matches, unmatched_out)

message("Done.")
message("International shootouts cleaned rows: ", nrow(international_shootouts))
message("Unmatched match keys: ", nrow(unmatched_matches))
message("Output: ", shootouts_out)
message("Cleaning summary: ", summary_out)
message("Unmatched matches: ", unmatched_out)
