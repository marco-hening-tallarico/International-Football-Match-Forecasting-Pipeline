# 30b_validate_engineered_features.R
#
# Checks the engineered international modeling table before model training.
# Main risk is leakage from form or goalscorer features, so derived columns are
# checked for numeric types, internal consistency, and pre-match-only inputs.
#
# Reads:
# - data/processed/international_modeling_table_with_form_and_goalscorers.csv
# - data/processed/international_results.csv
# - data/processed/international_goalscorers.csv
# - data/processed/international_modeling_table_with_form.csv, if present
#
# Writes:
# - data/validation/engineered_features/
# - reports/figures/engineered_features/

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

fail <- function(message) {
    stop(message, call. = FALSE)
}

MODELING_TABLE_PATH <- file.path(
    PROCESSED_DIR,
    "international_modeling_table_with_form_and_goalscorers.csv"
)
RESULTS_PATH <- file.path(PROCESSED_DIR, "international_results.csv")
GOALSCORERS_PATH <- file.path(PROCESSED_DIR, "international_goalscorers.csv")
FORM_TABLE_PATH <- file.path(
    PROCESSED_DIR,
    "international_modeling_table_with_form.csv"
)

OUTPUT_DIR <- file.path(VALIDATION_DIR, "engineered_features")
GRAPHS_DIR <- file.path(REPORTS_FIGURES_DIR, "engineered_features")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(GRAPHS_DIR, recursive = TRUE, showWarnings = FALSE)

SCHEMA_PATH <- file.path(OUTPUT_DIR, "engineered_feature_schema_check.csv")
MISSINGNESS_PATH <- file.path(
    OUTPUT_DIR,
    "engineered_feature_missingness_summary.csv"
)
RANGE_PATH <- file.path(OUTPUT_DIR, "engineered_feature_range_checks.csv")
DIFF_PATH <- file.path(OUTPUT_DIR, "engineered_feature_diff_checks.csv")
LEAKAGE_PATH <- file.path(OUTPUT_DIR, "goalscorer_feature_leakage_audit.csv")
RECOMPUTE_PATH <- file.path(
    OUTPUT_DIR,
    "goalscorer_feature_manual_recompute_check.csv"
)
EXAMPLES_PATH <- file.path(OUTPUT_DIR, "goalscorer_feature_manual_examples.csv")
NOTES_PATH <- file.path(OUTPUT_DIR, "engineered_feature_validation_notes.md")
DIST_PNG_PATH <- file.path(GRAPHS_DIR, "engineered_feature_distributions.png")
DIST_PDF_PATH <- file.path(GRAPHS_DIR, "engineered_feature_distributions.pdf")

SAMPLE_MATCH_COUNT <- 25L
LAST_10_MATCH_WINDOW <- 10L
ROLLING_DAYS_WINDOW <- 365L
FLOAT_TOLERANCE <- 1e-6

HOME_FORM_COLUMNS <- c(
    "home_prior_matches",
    "home_points_per_match_last_5",
    "home_goal_diff_per_match_last_5",
    "home_goals_for_per_match_last_5",
    "home_goals_against_per_match_last_5",
    "home_draw_rate_last_5",
    "home_points_per_match_last_10",
    "home_goal_diff_per_match_last_10",
    "home_goals_for_per_match_last_10",
    "home_goals_against_per_match_last_10",
    "home_draw_rate_last_10"
)

AWAY_FORM_COLUMNS <- c(
    "away_prior_matches",
    "away_points_per_match_last_5",
    "away_goal_diff_per_match_last_5",
    "away_goals_for_per_match_last_5",
    "away_goals_against_per_match_last_5",
    "away_draw_rate_last_5",
    "away_points_per_match_last_10",
    "away_goal_diff_per_match_last_10",
    "away_goals_for_per_match_last_10",
    "away_goals_against_per_match_last_10",
    "away_draw_rate_last_10"
)

COMBINED_FORM_COLUMNS <- c(
    "form_points_diff_last_5",
    "form_goal_diff_diff_last_5",
    "form_goals_for_diff_last_5",
    "form_goals_against_diff_last_5",
    "form_draw_rate_mean_last_5",
    "form_draw_rate_diff_last_5",
    "form_points_diff_last_10",
    "form_goal_diff_diff_last_10",
    "form_goals_for_diff_last_10",
    "form_goals_against_diff_last_10",
    "form_draw_rate_mean_last_10",
    "form_draw_rate_diff_last_10"
)

HOME_GOALSCORER_COLUMNS <- c(
    "home_unique_scorers_last_10",
    "home_goals_by_top_scorer_last_10",
    "home_non_penalty_goals_last_10",
    "home_penalty_goals_last_10",
    "home_unique_scorers_365d",
    "home_goals_by_top_scorer_365d",
    "home_non_penalty_goals_365d",
    "home_penalty_goals_365d",
    "home_avg_goal_minute_365d"
)

AWAY_GOALSCORER_COLUMNS <- c(
    "away_unique_scorers_last_10",
    "away_goals_by_top_scorer_last_10",
    "away_non_penalty_goals_last_10",
    "away_penalty_goals_last_10",
    "away_unique_scorers_365d",
    "away_goals_by_top_scorer_365d",
    "away_non_penalty_goals_365d",
    "away_penalty_goals_365d",
    "away_avg_goal_minute_365d"
)

DIFF_GOALSCORER_COLUMNS <- c(
    "unique_scorers_diff_last_10",
    "top_scorer_goals_diff_last_10",
    "non_penalty_goals_diff_last_10",
    "penalty_goals_diff_last_10",
    "unique_scorers_diff_365d",
    "top_scorer_goals_diff_365d",
    "non_penalty_goals_diff_365d",
    "penalty_goals_diff_365d",
    "avg_goal_minute_diff_365d"
)

ENGINEERED_FEATURE_COLUMNS <- c(
    HOME_FORM_COLUMNS,
    AWAY_FORM_COLUMNS,
    COMBINED_FORM_COLUMNS,
    HOME_GOALSCORER_COLUMNS,
    AWAY_GOALSCORER_COLUMNS,
    DIFF_GOALSCORER_COLUMNS
)

DIFF_FEATURE_MAPPINGS <- tibble::tribble(
    ~diff_feature, ~home_feature, ~away_feature,
    "form_points_diff_last_5", "home_points_per_match_last_5", "away_points_per_match_last_5",
    "form_goal_diff_diff_last_5", "home_goal_diff_per_match_last_5", "away_goal_diff_per_match_last_5",
    "form_goals_for_diff_last_5", "home_goals_for_per_match_last_5", "away_goals_for_per_match_last_5",
    "form_goals_against_diff_last_5", "home_goals_against_per_match_last_5", "away_goals_against_per_match_last_5",
    "form_draw_rate_diff_last_5", "home_draw_rate_last_5", "away_draw_rate_last_5",
    "form_points_diff_last_10", "home_points_per_match_last_10", "away_points_per_match_last_10",
    "form_goal_diff_diff_last_10", "home_goal_diff_per_match_last_10", "away_goal_diff_per_match_last_10",
    "form_goals_for_diff_last_10", "home_goals_for_per_match_last_10", "away_goals_for_per_match_last_10",
    "form_goals_against_diff_last_10", "home_goals_against_per_match_last_10", "away_goals_against_per_match_last_10",
    "form_draw_rate_diff_last_10", "home_draw_rate_last_10", "away_draw_rate_last_10",
    "unique_scorers_diff_last_10", "home_unique_scorers_last_10", "away_unique_scorers_last_10",
    "top_scorer_goals_diff_last_10", "home_goals_by_top_scorer_last_10", "away_goals_by_top_scorer_last_10",
    "non_penalty_goals_diff_last_10", "home_non_penalty_goals_last_10", "away_non_penalty_goals_last_10",
    "penalty_goals_diff_last_10", "home_penalty_goals_last_10", "away_penalty_goals_last_10",
    "unique_scorers_diff_365d", "home_unique_scorers_365d", "away_unique_scorers_365d",
    "top_scorer_goals_diff_365d", "home_goals_by_top_scorer_365d", "away_goals_by_top_scorer_365d",
    "non_penalty_goals_diff_365d", "home_non_penalty_goals_365d", "away_non_penalty_goals_365d",
    "penalty_goals_diff_365d", "home_penalty_goals_365d", "away_penalty_goals_365d",
    "avg_goal_minute_diff_365d", "home_avg_goal_minute_365d", "away_avg_goal_minute_365d"
)

GOALSCORER_RECOMPUTE_FEATURES <- tibble::tribble(
    ~feature_suffix, ~stored_home, ~stored_away,
    "unique_scorers_last_10", "home_unique_scorers_last_10", "away_unique_scorers_last_10",
    "goals_by_top_scorer_last_10", "home_goals_by_top_scorer_last_10", "away_goals_by_top_scorer_last_10",
    "non_penalty_goals_last_10", "home_non_penalty_goals_last_10", "away_non_penalty_goals_last_10",
    "penalty_goals_last_10", "home_penalty_goals_last_10", "away_penalty_goals_last_10",
    "unique_scorers_365d", "home_unique_scorers_365d", "away_unique_scorers_365d",
    "goals_by_top_scorer_365d", "home_goals_by_top_scorer_365d", "away_goals_by_top_scorer_365d",
    "non_penalty_goals_365d", "home_non_penalty_goals_365d", "away_non_penalty_goals_365d",
    "penalty_goals_365d", "home_penalty_goals_365d", "away_penalty_goals_365d"
)

make_id_part <- function(x) {
    x |>
        as.character() |>
        stringr::str_squish() |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("[^a-z0-9]+", "_") |>
        stringr::str_replace_all("^_|_$", "")
}

build_source_match_key <- function(match_date, home_team, away_team) {
    paste(
        make_id_part(match_date),
        make_id_part(home_team),
        make_id_part(away_team),
        sep = "_"
    )
}

aggregate_attacking_goal_features <- function(goal_rows, include_avg_minute = FALSE) {
    if (nrow(goal_rows) == 0L) {
        return(list(
            unique_scorers = 0,
            goals_by_top_scorer = 0,
            non_penalty_goals = 0,
            penalty_goals = 0,
            avg_goal_minute = NA_real_
        ))
    }

    attacking_goals <- goal_rows |>
        dplyr::filter(!isTRUE(.data$own_goal))

    if (nrow(attacking_goals) == 0L) {
        return(list(
            unique_scorers = 0,
            goals_by_top_scorer = 0,
            non_penalty_goals = 0,
            penalty_goals = 0,
            avg_goal_minute = NA_real_
        ))
    }

    scorer_goal_counts <- attacking_goals |>
        dplyr::count(.data$scorer, name = "goal_count")

    non_penalty_goals <- sum(!isTRUE(attacking_goals$penalty))
    penalty_goals <- sum(isTRUE(attacking_goals$penalty))

    avg_goal_minute <- NA_real_
    if (isTRUE(include_avg_minute)) {
        minute_values <- attacking_goals$minute
        if (any(!is.na(minute_values))) {
            avg_goal_minute <- mean(minute_values, na.rm = TRUE)
        }
    }

    list(
        unique_scorers = dplyr::n_distinct(attacking_goals$scorer),
        goals_by_top_scorer = max(scorer_goal_counts$goal_count),
        non_penalty_goals = non_penalty_goals,
        penalty_goals = penalty_goals,
        avg_goal_minute = avg_goal_minute
    )
}

recompute_team_goalscorer_features <- function(
    team_name,
    match_date,
    team_match_history,
    team_goal_history
) {
    prior_matches <- team_match_history |>
        dplyr::filter(.data$date < match_date)

    prior_goals <- team_goal_history |>
        dplyr::filter(.data$goal_date < match_date)

    window_start_date <- match_date - ROLLING_DAYS_WINDOW

    if (nrow(prior_matches) == 0L) {
        return(list(
            unique_scorers_last_10 = NA_real_,
            goals_by_top_scorer_last_10 = NA_real_,
            non_penalty_goals_last_10 = NA_real_,
            penalty_goals_last_10 = NA_real_,
            unique_scorers_365d = NA_real_,
            goals_by_top_scorer_365d = NA_real_,
            non_penalty_goals_365d = NA_real_,
            penalty_goals_365d = NA_real_,
            prior_match_keys_last_10 = character(),
            prior_goals_last_10 = team_goal_history[0L, , drop = FALSE],
            prior_goals_365d = team_goal_history[0L, , drop = FALSE]
        ))
    }

    last_10_match_keys <- prior_matches |>
        dplyr::arrange(.data$date, .data$source_match_id) |>
        dplyr::slice_tail(n = LAST_10_MATCH_WINDOW) |>
        dplyr::pull(.data$source_match_key)

    goals_last_10 <- prior_goals |>
        dplyr::filter(.data$source_match_key %in% last_10_match_keys)

    goals_365d <- prior_goals |>
        dplyr::filter(.data$goal_date >= window_start_date)

    last_10_stats <- aggregate_attacking_goal_features(
        goals_last_10,
        include_avg_minute = FALSE
    )
    stats_365d <- aggregate_attacking_goal_features(
        goals_365d,
        include_avg_minute = FALSE
    )

    list(
        unique_scorers_last_10 = last_10_stats$unique_scorers,
        goals_by_top_scorer_last_10 = last_10_stats$goals_by_top_scorer,
        non_penalty_goals_last_10 = last_10_stats$non_penalty_goals,
        penalty_goals_last_10 = last_10_stats$penalty_goals,
        unique_scorers_365d = stats_365d$unique_scorers,
        goals_by_top_scorer_365d = stats_365d$goals_by_top_scorer,
        non_penalty_goals_365d = stats_365d$non_penalty_goals,
        penalty_goals_365d = stats_365d$penalty_goals,
        prior_match_keys_last_10 = last_10_match_keys,
        prior_goals_last_10 = goals_last_10,
        prior_goals_365d = goals_365d
    )
}

collect_goal_rows_for_team_match <- function(
    team_name,
    match_date,
    team_match_history,
    team_goal_history
) {
    recomputed <- recompute_team_goalscorer_features(
        team_name = team_name,
        match_date = match_date,
        team_match_history = team_match_history,
        team_goal_history = team_goal_history
    )

    dplyr::bind_rows(
        recomputed$prior_goals_last_10 |>
            dplyr::mutate(window = "last_10"),
        recomputed$prior_goals_365d |>
            dplyr::mutate(window = "365d")
    ) |>
        dplyr::distinct()
}

values_equal <- function(stored_value, recomputed_value, tolerance = FLOAT_TOLERANCE) {
    if (is.na(stored_value) && is.na(recomputed_value)) {
        return(TRUE)
    }
    if (is.na(stored_value) || is.na(recomputed_value)) {
        return(FALSE)
    }
    abs(stored_value - recomputed_value) <= tolerance
}

message("============================================================")
message("Validating engineered features")
message("============================================================")

required_inputs <- c(
    MODELING_TABLE_PATH,
    RESULTS_PATH,
    GOALSCORERS_PATH
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0L) {
    fail(paste(
        "Missing required input file(s):",
        paste(missing_inputs, collapse = ", ")
    ))
}

message("Reading modeling table: ", MODELING_TABLE_PATH)
modeling_table <- readr::read_csv(
    MODELING_TABLE_PATH,
    show_col_types = FALSE,
    progress = FALSE
) |>
    dplyr::mutate(date = as.Date(.data$date))

input_row_count <- nrow(modeling_table)

message("Reading goalscorers: ", GOALSCORERS_PATH)
goalscorers_table <- readr::read_csv(
    GOALSCORERS_PATH,
    show_col_types = FALSE,
    progress = FALSE
)

goalscorers_prepared <- goalscorers_table |>
    dplyr::transmute(
        goal_date = as.Date(.data$date),
        home_team = stringr::str_squish(as.character(.data$home_team)),
        away_team = stringr::str_squish(as.character(.data$away_team)),
        team = stringr::str_squish(as.character(.data$team)),
        scorer = stringr::str_squish(as.character(.data$scorer)),
        minute = suppressWarnings(as.numeric(.data$minute)),
        own_goal = as.logical(.data$own_goal),
        penalty = as.logical(.data$penalty),
        source_match_key = build_source_match_key(
            .data$date,
            .data$home_team,
            .data$away_team
        )
    ) |>
    dplyr::filter(
        !is.na(.data$goal_date),
        !is.na(.data$team),
        .data$team != "",
        !is.na(.data$scorer),
        .data$scorer != ""
    )

modeling_table <- modeling_table |>
    dplyr::mutate(
        source_match_key = build_source_match_key(
            .data$date,
            .data$home_team,
            .data$away_team
        )
    )

home_team_matches <- modeling_table |>
    dplyr::transmute(
        team = .data$home_team,
        opponent = .data$away_team,
        date = .data$date,
        source_match_id = .data$source_match_id,
        source_match_key = .data$source_match_key,
        venue_role = "home"
    )

away_team_matches <- modeling_table |>
    dplyr::transmute(
        team = .data$away_team,
        opponent = .data$home_team,
        date = .data$date,
        source_match_id = .data$source_match_id,
        source_match_key = .data$source_match_key,
        venue_role = "away"
    )

team_match_history <- dplyr::bind_rows(home_team_matches, away_team_matches) |>
    dplyr::arrange(.data$team, .data$date, .data$source_match_id, .data$venue_role)

check_results <- list()

add_check_result <- function(check_name, passed, details = NA_character_) {
    check_results[[length(check_results) + 1L]] <<- tibble::tibble(
        check_name = check_name,
        passed = passed,
        details = details
    )
}

message("Running schema checks...")
schema_check <- purrr::map_dfr(
    ENGINEERED_FEATURE_COLUMNS,
    function(feature_name) {
        column_exists <- feature_name %in% names(modeling_table)
        actual_class <- if (column_exists) {
            paste(class(modeling_table[[feature_name]]), collapse = "|")
        } else {
            NA_character_
        }
        is_numeric <- column_exists &&
            is.numeric(modeling_table[[feature_name]])
        passed <- column_exists && is_numeric

        tibble::tibble(
            feature = feature_name,
            column_exists = column_exists,
            actual_class = actual_class,
            expected_class = "numeric",
            is_numeric = is_numeric,
            passed = passed
        )
    }
)

readr::write_csv(schema_check, SCHEMA_PATH)
add_check_result(
    "schema_check",
    all(schema_check$passed, na.rm = TRUE),
    paste(sum(!schema_check$passed), "schema failures")
)

message("Running missingness checks...")
missingness_summary <- purrr::map_dfr(
    ENGINEERED_FEATURE_COLUMNS,
    function(feature_name) {
        if (!feature_name %in% names(modeling_table)) {
            return(tibble::tibble(
                feature = feature_name,
                n_missing = NA_integer_,
                pct_missing = NA_real_,
                n_zero = NA_integer_,
                pct_zero = NA_real_,
                min = NA_real_,
                p01 = NA_real_,
                median = NA_real_,
                mean = NA_real_,
                p99 = NA_real_,
                max = NA_real_
            ))
        }

        feature_values <- modeling_table[[feature_name]]
        non_missing_values <- feature_values[!is.na(feature_values)]
        n_rows <- length(feature_values)
        n_missing <- sum(is.na(feature_values))
        n_zero <- sum(feature_values == 0, na.rm = TRUE)

        tibble::tibble(
            feature = feature_name,
            n_missing = n_missing,
            pct_missing = 100 * n_missing / n_rows,
            n_zero = n_zero,
            pct_zero = 100 * n_zero / n_rows,
            min = if (length(non_missing_values) > 0L) min(non_missing_values) else NA_real_,
            p01 = if (length(non_missing_values) > 0L) {
                as.numeric(stats::quantile(non_missing_values, probs = 0.01))
            } else {
                NA_real_
            },
            median = if (length(non_missing_values) > 0L) {
                stats::median(non_missing_values)
            } else {
                NA_real_
            },
            mean = if (length(non_missing_values) > 0L) {
                mean(non_missing_values)
            } else {
                NA_real_
            },
            p99 = if (length(non_missing_values) > 0L) {
                as.numeric(stats::quantile(non_missing_values, probs = 0.99))
            } else {
                NA_real_
            },
            max = if (length(non_missing_values) > 0L) max(non_missing_values) else NA_real_
        )
    }
)

readr::write_csv(missingness_summary, MISSINGNESS_PATH)
add_check_result("missingness_summary", TRUE, "Descriptive check only")

message("Running range and invariant checks...")
points_per_match_features <- c(
    "home_points_per_match_last_5",
    "home_points_per_match_last_10",
    "away_points_per_match_last_5",
    "away_points_per_match_last_10"
)
rate_features <- c(
    "home_draw_rate_last_5",
    "home_draw_rate_last_10",
    "away_draw_rate_last_5",
    "away_draw_rate_last_10"
)
unique_scorer_features <- c(
    "home_unique_scorers_last_10",
    "away_unique_scorers_last_10",
    "home_unique_scorers_365d",
    "away_unique_scorers_365d"
)
penalty_goal_features <- c(
    "home_penalty_goals_last_10",
    "away_penalty_goals_last_10",
    "home_penalty_goals_365d",
    "away_penalty_goals_365d"
)
non_penalty_goal_features <- c(
    "home_non_penalty_goals_last_10",
    "away_non_penalty_goals_last_10",
    "home_non_penalty_goals_365d",
    "away_non_penalty_goals_365d"
)
prior_match_features <- c("home_prior_matches", "away_prior_matches")
avg_minute_features <- c(
    "home_avg_goal_minute_365d",
    "away_avg_goal_minute_365d"
)

count_range_violations <- function(feature_name, violation_mask) {
    if (!feature_name %in% names(modeling_table)) {
        return(NA_integer_)
    }
    sum(violation_mask, na.rm = TRUE)
}

range_checks <- dplyr::bind_rows(
    purrr::map_dfr(points_per_match_features, function(feature_name) {
        values <- modeling_table[[feature_name]]
        violations <- !is.na(values) & (values < 0 | values > 3)
        tibble::tibble(
            check_name = "points_per_match_between_0_and_3",
            feature = feature_name,
            n_violations = count_range_violations(feature_name, violations),
            passed = is.na(count_range_violations(feature_name, violations)) ||
                count_range_violations(feature_name, violations) == 0L
        )
    }),
    purrr::map_dfr(rate_features, function(feature_name) {
        values <- modeling_table[[feature_name]]
        violations <- !is.na(values) & (values < 0 | values > 1)
        tibble::tibble(
            check_name = "rate_between_0_and_1",
            feature = feature_name,
            n_violations = count_range_violations(feature_name, violations),
            passed = is.na(count_range_violations(feature_name, violations)) ||
                count_range_violations(feature_name, violations) == 0L
        )
    }),
    purrr::map_dfr(unique_scorer_features, function(feature_name) {
        values <- modeling_table[[feature_name]]
        violations <- !is.na(values) & values < 0
        tibble::tibble(
            check_name = "unique_scorer_count_non_negative",
            feature = feature_name,
            n_violations = count_range_violations(feature_name, violations),
            passed = is.na(count_range_violations(feature_name, violations)) ||
                count_range_violations(feature_name, violations) == 0L
        )
    }),
    purrr::map_dfr(penalty_goal_features, function(feature_name) {
        values <- modeling_table[[feature_name]]
        violations <- !is.na(values) & values < 0
        tibble::tibble(
            check_name = "penalty_goal_count_non_negative",
            feature = feature_name,
            n_violations = count_range_violations(feature_name, violations),
            passed = is.na(count_range_violations(feature_name, violations)) ||
                count_range_violations(feature_name, violations) == 0L
        )
    }),
    purrr::map_dfr(non_penalty_goal_features, function(feature_name) {
        values <- modeling_table[[feature_name]]
        violations <- !is.na(values) & values < 0
        tibble::tibble(
            check_name = "non_penalty_goal_count_non_negative",
            feature = feature_name,
            n_violations = count_range_violations(feature_name, violations),
            passed = is.na(count_range_violations(feature_name, violations)) ||
                count_range_violations(feature_name, violations) == 0L
        )
    }),
    purrr::map_dfr(prior_match_features, function(feature_name) {
        values <- modeling_table[[feature_name]]
        violations <- !is.na(values) & values < 0
        tibble::tibble(
            check_name = "prior_match_count_non_negative",
            feature = feature_name,
            n_violations = count_range_violations(feature_name, violations),
            passed = is.na(count_range_violations(feature_name, violations)) ||
                count_range_violations(feature_name, violations) == 0L
        )
    }),
    purrr::map_dfr(avg_minute_features, function(feature_name) {
        values <- modeling_table[[feature_name]]
        violations <- !is.na(values) & (values < 0 | values > 130)
        tibble::tibble(
            check_name = "avg_goal_minute_between_0_and_130",
            feature = feature_name,
            n_violations = count_range_violations(feature_name, violations),
            passed = is.na(count_range_violations(feature_name, violations)) ||
                count_range_violations(feature_name, violations) == 0L
        )
    })
)

readr::write_csv(range_checks, RANGE_PATH)
add_check_result(
    "range_checks",
    all(range_checks$passed, na.rm = TRUE),
    paste(sum(!range_checks$passed, na.rm = TRUE), "range failures")
)

message("Running difference-feature checks...")
diff_checks <- purrr::map_dfr(
    seq_len(nrow(DIFF_FEATURE_MAPPINGS)),
    function(row_index) {
        mapping <- DIFF_FEATURE_MAPPINGS[row_index, ]
        diff_feature <- mapping$diff_feature
        home_feature <- mapping$home_feature
        away_feature <- mapping$away_feature

        if (!all(c(diff_feature, home_feature, away_feature) %in% names(modeling_table))) {
            return(tibble::tibble(
                diff_feature = diff_feature,
                home_feature = home_feature,
                away_feature = away_feature,
                n_rows_checked = 0L,
                n_mismatches = NA_integer_,
                passed = FALSE
            ))
        }

        expected_diff <- modeling_table[[home_feature]] - modeling_table[[away_feature]]
        actual_diff <- modeling_table[[diff_feature]]
        comparable_rows <- !is.na(expected_diff) | !is.na(actual_diff)
        mismatches <- comparable_rows &
            !mapply(values_equal, actual_diff, expected_diff)

        tibble::tibble(
            diff_feature = diff_feature,
            home_feature = home_feature,
            away_feature = away_feature,
            n_rows_checked = sum(comparable_rows),
            n_mismatches = sum(mismatches, na.rm = TRUE),
            passed = sum(mismatches, na.rm = TRUE) == 0L
        )
    }
)

readr::write_csv(diff_checks, DIFF_PATH)
add_check_result(
    "diff_checks",
    all(diff_checks$passed, na.rm = TRUE),
    paste(sum(!diff_checks$passed, na.rm = TRUE), "diff failures")
)

message("Sampling matches for goalscorer audits...")
available_goalscorer_features <- intersect(
    c(HOME_GOALSCORER_COLUMNS, AWAY_GOALSCORER_COLUMNS),
    names(modeling_table)
)

sample_pool <- modeling_table |>
    dplyr::filter(
        dplyr::if_any(
            dplyr::all_of(available_goalscorer_features),
            ~ !is.na(.x)
        )
    )

if (nrow(sample_pool) == 0L) {
    sample_pool <- modeling_table
}

sample_size <- min(SAMPLE_MATCH_COUNT, nrow(sample_pool))
sampled_matches <- sample_pool |>
    dplyr::slice_sample(n = sample_size)

message("Running goalscorer leakage audit...")
leakage_audit_rows <- purrr::map_dfr(
    seq_len(nrow(sampled_matches)),
    function(row_index) {
        match_row <- sampled_matches[row_index, , drop = FALSE]
        match_date <- match_row$date[[1L]]
        source_match_id <- match_row$source_match_id[[1L]]

        purrr::map_dfr(
            c("home", "away"),
            function(team_side) {
                team_name <- if (team_side == "home") {
                    match_row$home_team[[1L]]
                } else {
                    match_row$away_team[[1L]]
                }

                team_history <- team_match_history |>
                    dplyr::filter(.data$team == team_name)

                team_goals <- goalscorers_prepared |>
                    dplyr::filter(.data$team == team_name)

                goals_used <- collect_goal_rows_for_team_match(
                    team_name = team_name,
                    match_date = match_date,
                    team_match_history = team_history,
                    team_goal_history = team_goals
                )

                n_goals_used <- nrow(goals_used)
                n_future_goals <- if (n_goals_used == 0L) {
                    0L
                } else {
                    sum(goals_used$goal_date >= match_date, na.rm = TRUE)
                }
                max_goal_date_used <- if (n_goals_used == 0L) {
                    as.Date(NA)
                } else {
                    max(goals_used$goal_date, na.rm = TRUE)
                }

                tibble::tibble(
                    source_match_id = source_match_id,
                    date = match_date,
                    home_team = match_row$home_team[[1L]],
                    away_team = match_row$away_team[[1L]],
                    team_side = team_side,
                    team = team_name,
                    n_goals_used = n_goals_used,
                    n_future_goals = n_future_goals,
                    max_goal_date_used = max_goal_date_used,
                    passed = n_future_goals == 0L
                )
            }
        )
    }
)

readr::write_csv(leakage_audit_rows, LEAKAGE_PATH)
add_check_result(
    "goalscorer_leakage_audit",
    all(leakage_audit_rows$passed, na.rm = TRUE),
    paste(sum(!leakage_audit_rows$passed, na.rm = TRUE), "leakage failures")
)

message("Running manual goalscorer recomputation checks...")
manual_recompute_rows <- purrr::map_dfr(
    seq_len(nrow(sampled_matches)),
    function(row_index) {
        match_row <- sampled_matches[row_index, , drop = FALSE]
        match_date <- match_row$date[[1L]]

        purrr::map_dfr(
            c("home", "away"),
            function(team_side) {
                team_name <- if (team_side == "home") {
                    match_row$home_team[[1L]]
                } else {
                    match_row$away_team[[1L]]
                }

                team_history <- team_match_history |>
                    dplyr::filter(.data$team == team_name)

                team_goals <- goalscorers_prepared |>
                    dplyr::filter(.data$team == team_name)

                recomputed <- recompute_team_goalscorer_features(
                    team_name = team_name,
                    match_date = match_date,
                    team_match_history = team_history,
                    team_goal_history = team_goals
                )

                purrr::map_dfr(
                    seq_len(nrow(GOALSCORER_RECOMPUTE_FEATURES)),
                    function(feature_index) {
                        feature_row <- GOALSCORER_RECOMPUTE_FEATURES[feature_index, ]
                        stored_column <- if (team_side == "home") {
                            feature_row$stored_home[[1L]]
                        } else {
                            feature_row$stored_away[[1L]]
                        }
                        feature_suffix <- feature_row$feature_suffix[[1L]]
                        stored_value <- match_row[[stored_column]][[1L]]
                        recomputed_value <- recomputed[[feature_suffix]]

                        tibble::tibble(
                            source_match_id = match_row$source_match_id[[1L]],
                            date = match_date,
                            home_team = match_row$home_team[[1L]],
                            away_team = match_row$away_team[[1L]],
                            team_side = team_side,
                            feature = stored_column,
                            stored_value = stored_value,
                            recomputed_value = recomputed_value,
                            difference = stored_value - recomputed_value,
                            passed = values_equal(stored_value, recomputed_value)
                        )
                    }
                )
            }
        )
    }
)

readr::write_csv(manual_recompute_rows, RECOMPUTE_PATH)
add_check_result(
    "goalscorer_manual_recompute",
    all(manual_recompute_rows$passed, na.rm = TRUE),
    paste(sum(!manual_recompute_rows$passed, na.rm = TRUE), "recompute failures")
)

message("Building manual recomputation examples...")
manual_examples <- purrr::map_dfr(
    seq_len(nrow(sampled_matches)),
    function(row_index) {
        match_row <- sampled_matches[row_index, , drop = FALSE]
        match_date <- match_row$date[[1L]]

        purrr::map_dfr(
            c("home", "away"),
            function(team_side) {
                team_name <- if (team_side == "home") {
                    match_row$home_team[[1L]]
                } else {
                    match_row$away_team[[1L]]
                }

                team_history <- team_match_history |>
                    dplyr::filter(.data$team == team_name)

                team_goals <- goalscorers_prepared |>
                    dplyr::filter(.data$team == team_name)

                recomputed <- recompute_team_goalscorer_features(
                    team_name = team_name,
                    match_date = match_date,
                    team_match_history = team_history,
                    team_goal_history = team_goals
                )

                prior_matches <- team_history |>
                    dplyr::filter(.data$date < match_date) |>
                    dplyr::arrange(.data$date, .data$source_match_id) |>
                    dplyr::slice_tail(n = LAST_10_MATCH_WINDOW) |>
                    dplyr::mutate(example_type = "prior_match")

                prior_goals_last_10 <- recomputed$prior_goals_last_10 |>
                    dplyr::mutate(example_type = "goal_last_10")

                prior_goals_365d <- recomputed$prior_goals_365d |>
                    dplyr::mutate(example_type = "goal_365d")

                dplyr::bind_rows(
                    prior_matches |>
                        dplyr::transmute(
                            source_match_id = match_row$source_match_id[[1L]],
                            date = match_date,
                            home_team = match_row$home_team[[1L]],
                            away_team = match_row$away_team[[1L]],
                            team_side = team_side,
                            team = team_name,
                            example_type = .data$example_type,
                            prior_match_date = .data$date,
                            prior_match_key = .data$source_match_key,
                            goal_date = as.Date(NA),
                            scorer = NA_character_,
                            minute = NA_real_,
                            own_goal = NA,
                            penalty = NA
                        ),
                    prior_goals_last_10 |>
                        dplyr::transmute(
                            source_match_id = match_row$source_match_id[[1L]],
                            date = match_date,
                            home_team = match_row$home_team[[1L]],
                            away_team = match_row$away_team[[1L]],
                            team_side = team_side,
                            team = team_name,
                            example_type = .data$example_type,
                            prior_match_date = as.Date(NA),
                            prior_match_key = .data$source_match_key,
                            goal_date = .data$goal_date,
                            scorer = .data$scorer,
                            minute = .data$minute,
                            own_goal = .data$own_goal,
                            penalty = .data$penalty
                        ),
                    prior_goals_365d |>
                        dplyr::transmute(
                            source_match_id = match_row$source_match_id[[1L]],
                            date = match_date,
                            home_team = match_row$home_team[[1L]],
                            away_team = match_row$away_team[[1L]],
                            team_side = team_side,
                            team = team_name,
                            example_type = .data$example_type,
                            prior_match_date = as.Date(NA),
                            prior_match_key = .data$source_match_key,
                            goal_date = .data$goal_date,
                            scorer = .data$scorer,
                            minute = .data$minute,
                            own_goal = .data$own_goal,
                            penalty = .data$penalty
                        )
                )
            }
        )
    }
)

readr::write_csv(manual_examples, EXAMPLES_PATH)
add_check_result("goalscorer_manual_examples", TRUE, "Example table written")

message("Creating distribution plots...")
existing_features <- intersect(
    ENGINEERED_FEATURE_COLUMNS,
    names(modeling_table)
)
feature_chunks <- split(
    existing_features,
    ceiling(seq_along(existing_features) / 9)
)

plot_chunk <- function(feature_names, title_suffix) {
    plot_data <- modeling_table |>
        dplyr::select(dplyr::all_of(feature_names)) |>
        tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "feature",
            values_to = "value"
        )

    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$value)) +
        ggplot2::geom_histogram(
            bins = 40,
            fill = "#2C7FB8",
            color = "white",
            na.rm = TRUE
        ) +
        ggplot2::facet_wrap(
            ~feature,
            scales = "free",
            ncol = 3
        ) +
        ggplot2::labs(
            title = paste("Engineered feature distributions", title_suffix),
            x = "Feature value",
            y = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 10)
}

grDevices::pdf(DIST_PDF_PATH, width = 12, height = 9)
purrr::walk2(
    feature_chunks,
    seq_along(feature_chunks),
    function(chunk_features, chunk_index) {
        print(plot_chunk(chunk_features, paste("(page", chunk_index, ")")))
    }
)
grDevices::dev.off()

png_features <- existing_features[seq_len(min(12L, length(existing_features)))]
ggplot2::ggsave(
    filename = DIST_PNG_PATH,
    plot = plot_chunk(png_features, "(overview)"),
    width = 12,
    height = 10,
    dpi = 150
)

add_check_result("distribution_plots", TRUE, "Plots written")

checks_summary <- dplyr::bind_rows(check_results)
checks_passed <- sum(checks_summary$passed, na.rm = TRUE)
checks_failed <- sum(!checks_summary$passed, na.rm = TRUE)
total_checks <- nrow(checks_summary)
table_safe <- checks_failed == 0L &&
    all(schema_check$passed, na.rm = TRUE) &&
    all(range_checks$passed, na.rm = TRUE) &&
    all(diff_checks$passed, na.rm = TRUE) &&
    all(leakage_audit_rows$passed, na.rm = TRUE) &&
    all(manual_recompute_rows$passed, na.rm = TRUE)

top_missingness <- missingness_summary |>
    dplyr::filter(!is.na(.data$pct_missing)) |>
    dplyr::arrange(dplyr::desc(.data$pct_missing)) |>
    dplyr::slice_head(n = 5)

range_violations <- range_checks |>
    dplyr::filter(!.data$passed)

leakage_violations <- leakage_audit_rows |>
    dplyr::filter(!.data$passed)

recompute_mismatches <- manual_recompute_rows |>
    dplyr::filter(!.data$passed)

notes_lines <- c(
    "# Engineered feature validation notes",
    "",
    paste0("- Validation run: ", Sys.time()),
    paste0("- Input table: ", MODELING_TABLE_PATH),
    paste0("- Input row count: ", input_row_count),
    paste0("- Engineered features checked: ", length(ENGINEERED_FEATURE_COLUMNS)),
    paste0("- Validation checks passed: ", checks_passed, " / ", total_checks),
    paste0("- Validation checks failed: ", checks_failed),
    paste0(
        "- Modeling table safe for modeling: ",
        if (table_safe) "yes" else "no"
    ),
    "",
    "## Largest missingness features",
    if (nrow(top_missingness) == 0L) {
        "- None"
    } else {
        paste0(
            "- ",
            top_missingness$feature,
            ": ",
            round(top_missingness$pct_missing, 2),
            "% missing"
        )
    },
    "",
    "## Range violations",
    if (nrow(range_violations) == 0L) {
        "- None"
    } else {
        paste0(
            "- ",
            range_violations$check_name,
            " / ",
            range_violations$feature,
            ": ",
            range_violations$n_violations,
            " violations"
        )
    },
    "",
    "## Leakage violations",
    if (nrow(leakage_violations) == 0L) {
        "- None in sampled goalscorer audit"
    } else {
        paste0(
            "- ",
            leakage_violations$source_match_id,
            " (",
            leakage_violations$team_side,
            "): ",
            leakage_violations$n_future_goals,
            " future goals"
        )
    },
    "",
    "## Manual recomputation mismatches",
    if (nrow(recompute_mismatches) == 0L) {
        "- None in sampled goalscorer audit"
    } else {
        paste0(
            "- ",
            recompute_mismatches$source_match_id,
            " / ",
            recompute_mismatches$feature,
            ": stored=",
            recompute_mismatches$stored_value,
            ", recomputed=",
            recompute_mismatches$recomputed_value
        )
    },
    "",
    "## Caveats",
    "- Goalscorer leakage and recomputation checks use a random sample of 25 matches.",
    "- form_draw_rate_mean_* features are means, not home-minus-away differences.",
    "- Early-career teams may legitimately have NA form or goalscorer history.",
    "- Full feature distributions are in the multi-page PDF; PNG shows an overview subset.",
    if (file.exists(FORM_TABLE_PATH)) {
        paste0("- Form-only table available at: ", FORM_TABLE_PATH)
    } else {
        "- Form-only intermediate table was not found (optional input)."
    }
)

writeLines(notes_lines, NOTES_PATH)
add_check_result("validation_notes", TRUE, "Notes written")

message("============================================================")
message("Engineered feature validation summary")
message("Total checks: ", total_checks)
message("Passed checks: ", checks_passed)
message("Failed checks: ", checks_failed)
message(
    "Engineered table safe for modeling: ",
    if (table_safe) "YES" else "NO"
)
message("Output directory: ", OUTPUT_DIR)
message("Graph directory: ", GRAPHS_DIR)
message("============================================================")

if (checks_failed > 0L) {
    warning(
        "One or more engineered feature validation checks failed. ",
        "Review outputs in ",
        OUTPUT_DIR,
        call. = FALSE
    )
}
