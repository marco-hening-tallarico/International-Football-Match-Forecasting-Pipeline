# ============================================================
# validation.R
# Hard validation checks for the reproducible data pipeline
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

fail <- function(message) {
    stop(message, call. = FALSE)
}

assert_true <- function(condition, message) {
    if (!isTRUE(condition)) {
        fail(message)
    }
}

assert_files_exist <- function(paths, label) {
    missing <- paths[!file.exists(paths)]

    if (length(missing) > 0) {
        fail(paste0(
            label,
            " missing expected files:\n",
            paste(missing, collapse = "\n")
        ))
    }
}

assert_count <- function(actual, expected, label) {
    if (!identical(as.integer(actual), as.integer(expected))) {
        fail(paste0(
            label,
            " count changed. Expected ",
            expected,
            " based on the current project snapshot, found ",
            actual,
            ". If this is an intentional data refresh, update the expected count and inspect downstream outputs."
        ))
    }
}

normalize_existing_path <- function(path) {
    normalizePath(path, winslash = "/", mustWork = TRUE)
}

normalize_possible_path <- function(path) {
    path <- as.character(path)
    candidate <- ifelse(fs::is_absolute_path(path), path, file.path(PROJECT_ROOT, path))
    normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

expected_processed_files <- file.path(
    PROCESSED_DIR,
    c(
        "football_data_uk_matches.csv",
        "statsbomb_competitions.csv",
        "statsbomb_matches.csv",
        "international_results.csv"
    )
)

assert_files_exist(expected_processed_files, "Processed data")

football_data_raw_files <- fs::dir_ls(
    file.path(RAW_DIR, "football_data_uk"),
    recurse = TRUE,
    glob = "*.csv",
    fail = FALSE
)

statsbomb_match_raw_files <- fs::dir_ls(
    file.path(RAW_DIR, "statsbomb_open", "matches"),
    glob = "*.json",
    fail = FALSE
)

international_raw_files <- file.path(
    RAW_DIR,
    "international_results",
    c("results.csv", "shootouts.csv", "goalscorers.csv")
)

assert_count(
    length(football_data_raw_files),
    464L,
    "football-data.co.uk raw CSV"
)

assert_count(
    length(statsbomb_match_raw_files),
    80L,
    "StatsBomb match JSON"
)

assert_files_exist(
    international_raw_files,
    "international_results raw data"
)

validate_match_table <- function(path, label) {
    dat <- readr::read_csv(path, show_col_types = FALSE)

    required_cols <- c(
        "source",
        "raw_file",
        "source_match_id",
        "date",
        "season",
        "competition",
        "home_team",
        "away_team",
        "home_score",
        "away_score",
        "match_result",
        "result_class",
        "home_win",
        "draw",
        "away_win",
        "goal_difference",
        "total_goals",
        "neutral"
    )

    missing_cols <- setdiff(required_cols, names(dat))

    if (length(missing_cols) > 0) {
        fail(paste0(
            label,
            " is missing required columns: ",
            paste(missing_cols, collapse = ", ")
        ))
    }

    date_values <- suppressWarnings(as.Date(dat$date))
    home_score <- suppressWarnings(as.integer(dat$home_score))
    away_score <- suppressWarnings(as.integer(dat$away_score))
    result_class <- suppressWarnings(as.integer(dat$result_class))
    home_win <- suppressWarnings(as.integer(dat$home_win))
    draw <- suppressWarnings(as.integer(dat$draw))
    away_win <- suppressWarnings(as.integer(dat$away_win))
    goal_difference <- suppressWarnings(as.integer(dat$goal_difference))
    total_goals <- suppressWarnings(as.integer(dat$total_goals))

    checks <- c(
        missing_dates = sum(is.na(date_values)),
        missing_home_team = sum(is.na(dat$home_team) | dat$home_team == ""),
        missing_away_team = sum(is.na(dat$away_team) | dat$away_team == ""),
        missing_home_score = sum(is.na(home_score)),
        missing_away_score = sum(is.na(away_score)),
        missing_match_result = sum(is.na(dat$match_result) | dat$match_result == ""),
        missing_result_class = sum(is.na(result_class)),
        missing_home_win = sum(is.na(home_win)),
        missing_draw = sum(is.na(draw)),
        missing_away_win = sum(is.na(away_win))
    )

    failed_missing <- checks[checks > 0]

    if (length(failed_missing) > 0) {
        fail(paste0(
            label,
            " has missing required values:\n",
            paste(names(failed_missing), failed_missing, sep = ": ", collapse = "\n")
        ))
    }

    expected_match_result <- dplyr::case_when(
        home_score > away_score ~ "H",
        home_score == away_score ~ "D",
        home_score < away_score ~ "A",
        TRUE ~ NA_character_
    )

    expected_result_class <- dplyr::case_when(
        expected_match_result == "H" ~ 1L,
        expected_match_result == "D" ~ 0L,
        expected_match_result == "A" ~ -1L,
        TRUE ~ NA_integer_
    )

    bad_goal_difference <- sum(goal_difference != home_score - away_score)
    bad_total_goals <- sum(total_goals != home_score + away_score)
    bad_indicator_sum <- sum(home_win + draw + away_win != 1L)
    bad_home_win <- sum(home_win != as.integer(expected_match_result == "H"))
    bad_draw <- sum(draw != as.integer(expected_match_result == "D"))
    bad_away_win <- sum(away_win != as.integer(expected_match_result == "A"))
    bad_match_result <- sum(dat$match_result != expected_match_result)
    bad_result_class <- sum(result_class != expected_result_class)

    derived_failures <- c(
        bad_goal_difference = bad_goal_difference,
        bad_total_goals = bad_total_goals,
        bad_indicator_sum = bad_indicator_sum,
        bad_home_win = bad_home_win,
        bad_draw = bad_draw,
        bad_away_win = bad_away_win,
        bad_match_result = bad_match_result,
        bad_result_class = bad_result_class
    )

    failed_derived <- derived_failures[derived_failures > 0]

    if (length(failed_derived) > 0) {
        fail(paste0(
            label,
            " has invalid derived fields:\n",
            paste(names(failed_derived), failed_derived, sep = ": ", collapse = "\n")
        ))
    }

    invisible(TRUE)
}

football_data_uk_matches <- readr::read_csv(
    file.path(PROCESSED_DIR, "football_data_uk_matches.csv"),
    show_col_types = FALSE
)

statsbomb_matches <- readr::read_csv(
    file.path(PROCESSED_DIR, "statsbomb_matches.csv"),
    show_col_types = FALSE
)

international_results <- readr::read_csv(
    file.path(PROCESSED_DIR, "international_results.csv"),
    show_col_types = FALSE
)

validate_match_table(
    file.path(PROCESSED_DIR, "football_data_uk_matches.csv"),
    "football_data_uk_matches.csv"
)

validate_match_table(
    file.path(PROCESSED_DIR, "statsbomb_matches.csv"),
    "statsbomb_matches.csv"
)

validate_match_table(
    file.path(PROCESSED_DIR, "international_results.csv"),
    "international_results.csv"
)

expected_football_data_skipped <- file.path(
    RAW_DIR,
    "football_data_uk",
    c(
        "9394/F2.csv",
        "9394/I2.csv",
        "9495/F2.csv",
        "9495/I2.csv",
        "9596/F2.csv",
        "9596/I2.csv",
        "9697/I2.csv"
    )
)

# These legacy football-data.co.uk files are present in raw data but do not have
# the required match-result columns. They are intentionally excluded from the
# processed match table, and this list should change only after inspecting the
# raw files and cleaner behavior.
football_data_represented <- normalize_possible_path(unique(football_data_uk_matches$raw_file))
football_data_raw_norm <- normalize_existing_path(football_data_raw_files)
football_data_skipped_norm <- normalize_existing_path(expected_football_data_skipped)
football_data_missing_from_processed <- setdiff(
    football_data_raw_norm,
    c(football_data_represented, football_data_skipped_norm)
)
unexpected_football_data_skips <- setdiff(
    football_data_skipped_norm,
    setdiff(football_data_raw_norm, football_data_represented)
)

if (length(football_data_missing_from_processed) > 0) {
    fail(paste0(
        "football_data_uk_matches.csv does not represent these raw files, ",
        "and they are not configured skipped-file exceptions:\n",
        paste(football_data_missing_from_processed, collapse = "\n")
    ))
}

if (length(unexpected_football_data_skips) > 0) {
    fail(paste0(
        "Configured football-data skipped-file exceptions are now represented ",
        "or absent; inspect and update validation:\n",
        paste(unexpected_football_data_skips, collapse = "\n")
    ))
}

statsbomb_represented <- normalize_possible_path(unique(statsbomb_matches$raw_file))
statsbomb_raw_norm <- normalize_existing_path(statsbomb_match_raw_files)
statsbomb_missing_from_processed <- setdiff(statsbomb_raw_norm, statsbomb_represented)

if (length(statsbomb_missing_from_processed) > 0) {
    fail(paste0(
        "statsbomb_matches.csv does not represent these raw match files:\n",
        paste(statsbomb_missing_from_processed, collapse = "\n")
    ))
}

international_results_raw_norm <- normalize_existing_path(
    file.path(RAW_DIR, "international_results", "results.csv")
)
international_represented <- normalize_possible_path(unique(international_results$raw_file))

if (!international_results_raw_norm %in% international_represented) {
    fail("international_results.csv does not represent raw international_results/results.csv")
}

manifest_path <- file.path(META_DIR, "source_manifest.csv")

assert_true(
    file.exists(manifest_path),
    "Missing source manifest: data/metadata/source_manifest.csv"
)

source_manifest <- readr::read_csv(
    manifest_path,
    show_col_types = FALSE
)

required_manifest_cols <- c(
    "source",
    "dataset",
    "url",
    "local_path",
    "downloaded_at",
    "notes"
)

missing_manifest_cols <- setdiff(required_manifest_cols, names(source_manifest))

if (length(missing_manifest_cols) > 0) {
    fail(paste0(
        "source_manifest.csv is missing required columns: ",
        paste(missing_manifest_cols, collapse = ", ")
    ))
}

manifest_paths <- normalize_possible_path(source_manifest$local_path)
missing_manifest_paths <- source_manifest$local_path[!file.exists(manifest_paths)]

if (length(missing_manifest_paths) > 0) {
    fail(paste0(
        "source_manifest.csv contains local_path values that do not exist:\n",
        paste(missing_manifest_paths, collapse = "\n")
    ))
}

expected_manifest_files <- c(
    file.path(RAW_DIR, "statsbomb_open", "competitions.json"),
    football_data_raw_files,
    statsbomb_match_raw_files,
    international_raw_files
)

# These are all downloaded source files for this pipeline milestone. There are
# no intentional manifest exceptions at present; a future exception should name
# the file pattern and reason here, then keep this failure message explicit.
expected_manifest_norm <- normalize_existing_path(expected_manifest_files)
manifest_norm <- normalize_existing_path(manifest_paths)
missing_from_manifest <- setdiff(expected_manifest_norm, manifest_norm)

if (length(missing_from_manifest) > 0) {
    fail(paste0(
        "Downloaded raw files are missing from source_manifest.csv. ",
        "There are no intentional exceptions configured.\n",
        paste(missing_from_manifest, collapse = "\n")
    ))
}

message("Validation passed.")
message("Processed files checked: ", length(expected_processed_files))
message("football-data.co.uk raw CSV files: ", length(football_data_raw_files))
message("StatsBomb match JSON files: ", length(statsbomb_match_raw_files))
message("international_results raw files: ", length(international_raw_files))
