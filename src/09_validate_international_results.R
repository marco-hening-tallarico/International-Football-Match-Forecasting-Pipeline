# 09_validate_international_results.R
#
# Validates cleaned international match results: schema, keys, score logic,
# date ranges, and a set of warning-level duplicate or extreme-score checks.
#
# Reads:
# - data/processed/international_results.csv
# - data/raw/international_results/results.csv (row-count cross-check)
#
# Writes:
# - data/validation/processed_data/international_results_validation_*.csv

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

fail <- function(message) {
    stop(message, call. = FALSE)
}

validation_started_at <- Sys.time()

processed_path <- file.path(PROCESSED_DIR, "international_results.csv")
raw_results_path <- file.path(RAW_DIR, "international_results", "results.csv")

if (!file.exists(processed_path)) {
    fail("Missing processed international_results.csv. Run src/08_clean_international_results.R first.")
}

if (!file.exists(raw_results_path)) {
    fail("Missing raw international results.csv. Run src/07_download_international_results.R first.")
}

international_results <- read_processed_csv(processed_path) |>
    dplyr::mutate(validation_row_id = dplyr::row_number())

raw_results <- readr::read_csv(raw_results_path, show_col_types = FALSE) |>
    janitor::clean_names()

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
    "neutral",
    "tournament",
    "city",
    "country"
)

required_raw_cols <- c(
    "date",
    "home_team",
    "away_team",
    "home_score",
    "away_score",
    "tournament",
    "city",
    "country",
    "neutral"
)

check_rows <- list()
example_rows <- list()

add_check <- function(check_name,
                      severity,
                      rows_affected,
                      details,
                      status = NULL) {
    if (is.null(status)) {
        status <- dplyr::case_when(
            rows_affected == 0L ~ "pass",
            severity == "error" ~ "fail",
            TRUE ~ "review"
        )
    }

    check_rows[[length(check_rows) + 1L]] <<- tibble::tibble(
        check_name = check_name,
        severity = severity,
        status = status,
        rows_affected = as.integer(rows_affected),
        details = details
    )
}

add_examples <- function(issue_type, dat) {
    if (nrow(dat) == 0L) {
        return(invisible(NULL))
    }

    example_cols <- c(
        "validation_row_id",
        "source_match_id",
        "date",
        "home_team",
        "away_team",
        "home_score",
        "away_score",
        "tournament",
        "city",
        "country",
        "neutral",
        "total_goals",
        "goal_difference"
    )

    example_rows[[length(example_rows) + 1L]] <<- dat |>
        dplyr::select(dplyr::any_of(example_cols)) |>
        dplyr::mutate(validation_issue = issue_type, .before = 1L) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    invisible(NULL)
}

format_n <- function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

normalize_possible_path <- function(path) {
    path <- as.character(path)
    candidate <- ifelse(fs::is_absolute_path(path), path, file.path(PROJECT_ROOT, path))
    normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

missing_cols <- setdiff(required_cols, names(international_results))
unexpected_cols <- setdiff(names(international_results), c(required_cols, "validation_row_id"))
missing_raw_cols <- setdiff(required_raw_cols, names(raw_results))

add_check(
    "processed_schema_required_columns",
    "error",
    length(missing_cols),
    if (length(missing_cols) == 0L) {
        "All required processed columns are present."
    } else {
        paste("Missing:", paste(missing_cols, collapse = ", "))
    }
)

add_check(
    "processed_schema_unexpected_columns",
    "warning",
    length(unexpected_cols),
    if (length(unexpected_cols) == 0L) {
        "No unexpected processed columns are present."
    } else {
        paste("Unexpected:", paste(unexpected_cols, collapse = ", "))
    }
)

add_check(
    "raw_schema_required_columns",
    "error",
    length(missing_raw_cols),
    if (length(missing_raw_cols) == 0L) {
        "All required raw columns are present."
    } else {
        paste("Missing:", paste(missing_raw_cols, collapse = ", "))
    }
)

if (length(missing_cols) > 0L || length(missing_raw_cols) > 0L) {
    checks <- dplyr::bind_rows(check_rows)
    readr::write_csv(checks, file.path(VALIDATION_PROCESSED_DIR, "international_results_validation_checks.csv"))
    fail("International results validation could not continue because required columns are missing.")
}

blank_or_missing <- function(x) {
    is.na(x) | stringr::str_squish(as.character(x)) == ""
}

required_non_missing <- c(
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
    "neutral",
    "tournament",
    "city",
    "country"
)

missing_required_values <- purrr::map_int(
    required_non_missing,
    function(col) sum(blank_or_missing(international_results[[col]]))
)

add_check(
    "required_values_not_missing",
    "error",
    sum(missing_required_values),
    if (sum(missing_required_values) == 0L) {
        "No missing required values."
    } else {
        paste(
            names(missing_required_values)[missing_required_values > 0L],
            missing_required_values[missing_required_values > 0L],
            sep = ": ",
            collapse = "; "
        )
    }
)

source_mismatch <- international_results |>
    dplyr::filter(source != "international_results")

add_check(
    "source_constant",
    "error",
    nrow(source_mismatch),
    "All rows should identify the international_results source."
)
add_examples("source_constant", source_mismatch)

raw_file_mismatch <- international_results |>
    dplyr::filter(normalize_possible_path(raw_file) != normalize_possible_path(raw_results_path))

add_check(
    "raw_file_points_to_results_csv",
    "error",
    nrow(raw_file_mismatch),
    "Processed lineage should point to raw international_results/results.csv."
)
add_examples("raw_file_points_to_results_csv", raw_file_mismatch)

source_id_dupes <- international_results |>
    dplyr::count(source_match_id, name = "duplicate_count") |>
    dplyr::filter(duplicate_count > 1L)

add_check(
    "source_match_id_unique",
    "error",
    nrow(source_id_dupes),
    "source_match_id should uniquely identify processed international matches."
)

exact_duplicate_rows <- international_results |>
    dplyr::count(
        date,
        home_team,
        away_team,
        home_score,
        away_score,
        tournament,
        city,
        country,
        neutral,
        name = "duplicate_count"
    ) |>
    dplyr::filter(duplicate_count > 1L)

add_check(
    "exact_match_rows_unique",
    "error",
    nrow(exact_duplicate_rows),
    "No exact duplicate match-result rows should exist."
)

same_fixture_key <- international_results |>
    dplyr::count(date, home_team, away_team, tournament, name = "fixture_count") |>
    dplyr::filter(fixture_count > 1L) |>
    dplyr::inner_join(
        international_results,
        by = c("date", "home_team", "away_team", "tournament")
    ) |>
    dplyr::arrange(date, home_team, away_team, tournament)

add_check(
    "same_date_team_tournament_repeats",
    "warning",
    nrow(dplyr::distinct(same_fixture_key, date, home_team, away_team, tournament)),
    "Repeated date/home/away/tournament keys are kept but should be inspected."
)
add_examples("same_date_team_tournament_repeats", same_fixture_key)

team_same_day <- international_results |>
    dplyr::select(validation_row_id, source_match_id, date, home_team, away_team) |>
    tidyr::pivot_longer(
        cols = c(home_team, away_team),
        names_to = "side",
        values_to = "team"
    ) |>
    dplyr::count(date, team, name = "matches_on_date") |>
    dplyr::filter(matches_on_date > 1L)

team_same_day_examples <- international_results |>
    dplyr::semi_join(team_same_day, by = c("date", "home_team" = "team")) |>
    dplyr::bind_rows(
        international_results |>
            dplyr::semi_join(team_same_day, by = c("date", "away_team" = "team"))
    ) |>
    dplyr::distinct(validation_row_id, .keep_all = TRUE) |>
    dplyr::arrange(date, home_team, away_team)

add_check(
    "same_team_multiple_matches_same_date",
    "warning",
    nrow(team_same_day),
    "Teams appearing in multiple matches on the same date should be reviewed."
)
add_examples("same_team_multiple_matches_same_date", team_same_day_examples)

score_domain_bad <- international_results |>
    dplyr::filter(
        is.na(home_score) |
            is.na(away_score) |
            home_score < 0L |
            away_score < 0L |
            home_score != floor(home_score) |
            away_score != floor(away_score)
    )

add_check(
    "score_domain_nonnegative_integers",
    "error",
    nrow(score_domain_bad),
    "Scores should be present, integer-valued, and non-negative."
)
add_examples("score_domain_nonnegative_integers", score_domain_bad)

home_equals_away <- international_results |>
    dplyr::filter(home_team == away_team)

add_check(
    "home_team_differs_from_away_team",
    "error",
    nrow(home_equals_away),
    "A team should not play itself in one match row."
)
add_examples("home_team_differs_from_away_team", home_equals_away)

date_domain_bad <- international_results |>
    dplyr::filter(is.na(date) | date > Sys.Date())

add_check(
    "date_domain_valid_and_not_future",
    "error",
    nrow(date_domain_bad),
    paste("Match dates should parse and should not be after", as.character(Sys.Date()))
)
add_examples("date_domain_valid_and_not_future", date_domain_bad)

season_bad <- international_results |>
    dplyr::filter(season != as.character(lubridate::year(date)))

add_check(
    "season_equals_match_year",
    "error",
    nrow(season_bad),
    "season should equal the calendar year of date for international results."
)
add_examples("season_equals_match_year", season_bad)

competition_bad <- international_results |>
    dplyr::filter(competition != tournament)

add_check(
    "competition_matches_tournament",
    "error",
    nrow(competition_bad),
    "competition should mirror tournament for common match-table compatibility."
)
add_examples("competition_matches_tournament", competition_bad)

expected_match_result <- dplyr::case_when(
    international_results$home_score > international_results$away_score ~ "H",
    international_results$home_score == international_results$away_score ~ "D",
    international_results$home_score < international_results$away_score ~ "A",
    TRUE ~ NA_character_
)

expected_result_class <- dplyr::case_when(
    expected_match_result == "H" ~ 1L,
    expected_match_result == "D" ~ 0L,
    expected_match_result == "A" ~ -1L,
    TRUE ~ NA_integer_
)

derived_bad <- international_results |>
    dplyr::filter(
        !(match_result %in% c("H", "D", "A")) |
            result_class != expected_result_class |
            home_win != as.integer(expected_match_result == "H") |
            draw != as.integer(expected_match_result == "D") |
            away_win != as.integer(expected_match_result == "A") |
            home_win + draw + away_win != 1L |
            goal_difference != home_score - away_score |
            total_goals != home_score + away_score
    )

add_check(
    "derived_result_fields_consistent",
    "error",
    nrow(derived_bad),
    "Outcome, indicator, goal-difference, and total-goals fields should match the final score."
)
add_examples("derived_result_fields_consistent", derived_bad)

string_cols <- c("home_team", "away_team", "tournament", "city", "country")
string_whitespace_bad <- international_results |>
    dplyr::filter(
        dplyr::if_any(
            dplyr::all_of(string_cols),
            function(x) stringr::str_squish(x) != x
        )
    )

add_check(
    "text_fields_are_squished",
    "error",
    nrow(string_whitespace_bad),
    "Core text fields should not contain leading, trailing, or repeated internal whitespace."
)
add_examples("text_fields_are_squished", string_whitespace_bad)

neutral_bad <- international_results |>
    dplyr::filter(is.na(neutral))

add_check(
    "neutral_is_populated",
    "error",
    nrow(neutral_bad),
    "The international source supplies neutral-site flags, so processed neutral should not be missing."
)
add_examples("neutral_is_populated", neutral_bad)

extreme_scores <- international_results |>
    dplyr::filter(total_goals > 20L | abs(goal_difference) > 15L) |>
    dplyr::arrange(dplyr::desc(total_goals), dplyr::desc(abs(goal_difference)))

add_check(
    "extreme_scorelines_for_review",
    "warning",
    nrow(extreme_scores),
    "Extreme scorelines are plausible in historical international data but should remain visible in QA."
)
add_examples("extreme_scorelines_for_review", extreme_scores)

make_id_part <- function(x) {
    x |>
        as.character() |>
        stringr::str_squish() |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("[^a-z0-9]+", "_") |>
        stringr::str_replace_all("^_|_$", "")
}

raw_normalized <- raw_results |>
    dplyr::transmute(
        source = as.character("international_results"),
        raw_file = as.character(raw_results_path),
        date = suppressWarnings(as.Date(date)),
        season = as.character(lubridate::year(date)),
        competition = stringr::str_squish(as.character(tournament)),
        home_team = stringr::str_squish(as.character(home_team)),
        away_team = stringr::str_squish(as.character(away_team)),
        home_score = suppressWarnings(as.integer(home_score)),
        away_score = suppressWarnings(as.integer(away_score)),
        tournament = stringr::str_squish(as.character(tournament)),
        city = stringr::str_squish(as.character(city)),
        country = stringr::str_squish(as.character(country)),
        neutral = as.logical(neutral)
    )

raw_complete <- raw_normalized |>
    dplyr::filter(!is.na(home_score), !is.na(away_score))

raw_incomplete <- raw_normalized |>
    dplyr::filter(is.na(home_score) | is.na(away_score))

raw_expected <- raw_complete |>
    dplyr::mutate(
        source_match_id_base = paste(
            make_id_part(date),
            make_id_part(home_team),
            make_id_part(away_team),
            make_id_part(tournament),
            sep = "_"
        )
    ) |>
    dplyr::group_by(source_match_id_base) |>
    dplyr::mutate(
        source_match_id = as.character(if (dplyr::n() == 1L) {
            source_match_id_base
        } else {
            paste0(source_match_id_base, "_", dplyr::row_number())
        })
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-source_match_id_base) |>
    dplyr::mutate(
        match_result = dplyr::case_when(
            home_score > away_score ~ "H",
            home_score == away_score ~ "D",
            home_score < away_score ~ "A",
            TRUE ~ NA_character_
        ),
        result_class = dplyr::case_when(
            match_result == "H" ~ 1L,
            match_result == "D" ~ 0L,
            match_result == "A" ~ -1L,
            TRUE ~ NA_integer_
        ),
        home_win = as.integer(match_result == "H"),
        draw = as.integer(match_result == "D"),
        away_win = as.integer(match_result == "A"),
        goal_difference = home_score - away_score,
        total_goals = home_score + away_score
    ) |>
    dplyr::select(dplyr::all_of(required_cols))

processed_compare <- international_results |>
    dplyr::select(dplyr::all_of(required_cols))

raw_missing_from_processed <- raw_expected |>
    dplyr::anti_join(processed_compare, by = required_cols)

processed_missing_from_raw <- processed_compare |>
    dplyr::anti_join(raw_expected, by = required_cols)

add_check(
    "raw_complete_rows_match_processed_rows",
    "error",
    abs(nrow(raw_expected) - nrow(processed_compare)),
    paste0(
        "Complete raw rows: ",
        format_n(nrow(raw_expected)),
        "; processed rows: ",
        format_n(nrow(processed_compare)),
        "."
    )
)

add_check(
    "processed_rows_reconcile_to_raw",
    "error",
    nrow(raw_missing_from_processed) + nrow(processed_missing_from_raw),
    "Processed rows should reconcile exactly to complete scored rows from raw results.csv."
)

add_check(
    "raw_incomplete_fixtures_excluded",
    "info",
    nrow(raw_incomplete),
    "Raw rows without final scores are intentionally excluded from processed international_results.csv.",
    status = "recorded"
)

check_output_path <- file.path(VALIDATION_PROCESSED_DIR, "international_results_validation_checks.csv")
summary_output_path <- file.path(VALIDATION_PROCESSED_DIR, "international_results_validation_summary.csv")
examples_output_path <- file.path(VALIDATION_PROCESSED_DIR, "international_results_validation_examples.csv")

checks <- dplyr::bind_rows(check_rows) |>
    dplyr::arrange(
        factor(severity, levels = c("error", "warning", "info")),
        factor(status, levels = c("fail", "review", "recorded", "pass")),
        check_name
    )

examples <- dplyr::bind_rows(example_rows)

if (is.null(examples) || nrow(examples) == 0L) {
    examples <- tibble::tibble(
        validation_issue = character(),
        validation_row_id = character(),
        source_match_id = character(),
        date = character(),
        home_team = character(),
        away_team = character(),
        home_score = character(),
        away_score = character(),
        tournament = character(),
        city = character(),
        country = character(),
        neutral = character(),
        total_goals = character(),
        goal_difference = character()
    )
}

summary_rows <- tibble::tibble(
    metric = c(
        "validation_run_at",
        "processed_file",
        "raw_file",
        "processed_rows",
        "processed_columns",
        "raw_complete_scored_rows",
        "raw_incomplete_unscored_rows",
        "date_min",
        "date_max",
        "teams",
        "tournaments",
        "countries",
        "cities",
        "neutral_true",
        "neutral_false",
        "home_wins",
        "draws",
        "away_wins",
        "max_total_goals",
        "max_absolute_goal_difference",
        "error_checks_failed",
        "warning_checks_for_review"
    ),
    value = c(
        format(validation_started_at, "%Y-%m-%d %H:%M:%S %Z"),
        processed_path,
        raw_results_path,
        as.character(nrow(international_results)),
        as.character(length(required_cols)),
        as.character(nrow(raw_complete)),
        as.character(nrow(raw_incomplete)),
        as.character(min(international_results$date, na.rm = TRUE)),
        as.character(max(international_results$date, na.rm = TRUE)),
        as.character(dplyr::n_distinct(c(international_results$home_team, international_results$away_team))),
        as.character(dplyr::n_distinct(international_results$tournament)),
        as.character(dplyr::n_distinct(international_results$country)),
        as.character(dplyr::n_distinct(international_results$city)),
        as.character(sum(international_results$neutral, na.rm = TRUE)),
        as.character(sum(!international_results$neutral, na.rm = TRUE)),
        as.character(sum(international_results$match_result == "H")),
        as.character(sum(international_results$match_result == "D")),
        as.character(sum(international_results$match_result == "A")),
        as.character(max(international_results$total_goals, na.rm = TRUE)),
        as.character(max(abs(international_results$goal_difference), na.rm = TRUE)),
        as.character(sum(checks$severity == "error" & checks$status == "fail")),
        as.character(sum(checks$severity == "warning" & checks$status == "review"))
    )
)

readr::write_csv(checks, check_output_path)
readr::write_csv(summary_rows, summary_output_path)
readr::write_csv(examples, examples_output_path)

failed_errors <- checks |>
    dplyr::filter(severity == "error", status == "fail")

message("International results validation reports written:")
message("- ", check_output_path)
message("- ", summary_output_path)
message("- ", examples_output_path)

if (nrow(failed_errors) > 0L) {
    fail(paste0(
        "International results validation failed error checks:\n",
        paste(failed_errors$check_name, collapse = "\n")
    ))
}

message("International results validation passed error checks.")
message("Warnings for review: ", sum(checks$severity == "warning" & checks$status == "review"))
