# 27_build_lagged_team_form_features.R
#
# Adds strictly lagged team-form columns to the international modeling table.
# Each row uses only results from matches before that fixture date.
#
# Reads: data/processed/international_modeling_table.csv
#
# Writes:
# - data/processed/international_modeling_table_with_form.csv
# - data/validation/engineered_features/lagged_form_*.csv

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

INPUT_PATH <- file.path(PROCESSED_DIR, "international_modeling_table.csv")
OUTPUT_PATH <- file.path(
    PROCESSED_DIR,
    "international_modeling_table_with_form.csv"
)
SUMMARY_PATH <- file.path(VALIDATION_ENGINEERED_DIR, "lagged_form_feature_summary.csv")
MISSINGNESS_PATH <- file.path(VALIDATION_ENGINEERED_DIR, "lagged_form_missingness.csv")
LEAKAGE_PATH <- file.path(VALIDATION_ENGINEERED_DIR, "lagged_form_leakage_check.csv")

REQUIRED_COLUMNS <- c(
    "source_match_id",
    "date",
    "home_team_clean",
    "away_team_clean",
    "home_score",
    "away_score",
    "match_result"
)

LAST_5_WINDOW <- 5L
LAST_10_WINDOW <- 10L

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

FORM_FEATURE_COLUMNS <- c(
    HOME_FORM_COLUMNS,
    AWAY_FORM_COLUMNS,
    COMBINED_FORM_COLUMNS
)

LAST_5_FEATURE_COLUMNS <- c(
    HOME_FORM_COLUMNS[grepl("_last_5$", HOME_FORM_COLUMNS)],
    AWAY_FORM_COLUMNS[grepl("_last_5$", AWAY_FORM_COLUMNS)],
    COMBINED_FORM_COLUMNS[grepl("_last_5$", COMBINED_FORM_COLUMNS)]
)

LAST_10_FEATURE_COLUMNS <- c(
    HOME_FORM_COLUMNS[grepl("_last_10$", HOME_FORM_COLUMNS)],
    AWAY_FORM_COLUMNS[grepl("_last_10$", AWAY_FORM_COLUMNS)],
    COMBINED_FORM_COLUMNS[grepl("_last_10$", COMBINED_FORM_COLUMNS)]
)

compute_window_means <- function(prior_history, window_size) {
    if (nrow(prior_history) == 0L) {
        return(list(
            points_per_match = NA_real_,
            goal_diff_per_match = NA_real_,
            goals_for_per_match = NA_real_,
            goals_against_per_match = NA_real_,
            draw_rate = NA_real_
        ))
    }

    window_rows <- utils::tail(prior_history, window_size)

    list(
        points_per_match = mean(window_rows$points),
        goal_diff_per_match = mean(window_rows$goal_diff),
        goals_for_per_match = mean(window_rows$goals_for),
        goals_against_per_match = mean(window_rows$goals_against),
        draw_rate = mean(window_rows$is_draw)
    )
}

compute_team_form_features <- function(team_match_history) {
    team_match_history <- team_match_history |>
        dplyr::arrange(.data$date, .data$source_match_id)

    n_rows <- nrow(team_match_history)
    prior_matches <- integer(n_rows)
    points_per_match_last_5 <- rep(NA_real_, n_rows)
    goal_diff_per_match_last_5 <- rep(NA_real_, n_rows)
    goals_for_per_match_last_5 <- rep(NA_real_, n_rows)
    goals_against_per_match_last_5 <- rep(NA_real_, n_rows)
    draw_rate_last_5 <- rep(NA_real_, n_rows)
    points_per_match_last_10 <- rep(NA_real_, n_rows)
    goal_diff_per_match_last_10 <- rep(NA_real_, n_rows)
    goals_for_per_match_last_10 <- rep(NA_real_, n_rows)
    goals_against_per_match_last_10 <- rep(NA_real_, n_rows)
    draw_rate_last_10 <- rep(NA_real_, n_rows)
    max_prior_date_used <- as.Date(rep(NA, n_rows))

    completed_history <- team_match_history[0L, , drop = FALSE]

    for (row_index in seq_len(n_rows)) {
        current_row <- team_match_history[row_index, , drop = FALSE]
        current_date <- current_row$date[[1L]]

        prior_history <- completed_history |>
            dplyr::filter(.data$date < current_date)

        prior_matches[[row_index]] <- nrow(prior_history)

        if (nrow(prior_history) > 0L) {
            max_prior_date_used[[row_index]] <- max(prior_history$date)
        }

        last_5_stats <- compute_window_means(prior_history, LAST_5_WINDOW)
        last_10_stats <- compute_window_means(prior_history, LAST_10_WINDOW)

        points_per_match_last_5[[row_index]] <- last_5_stats$points_per_match
        goal_diff_per_match_last_5[[row_index]] <- last_5_stats$goal_diff_per_match
        goals_for_per_match_last_5[[row_index]] <- last_5_stats$goals_for_per_match
        goals_against_per_match_last_5[[row_index]] <- last_5_stats$goals_against_per_match
        draw_rate_last_5[[row_index]] <- last_5_stats$draw_rate

        points_per_match_last_10[[row_index]] <- last_10_stats$points_per_match
        goal_diff_per_match_last_10[[row_index]] <- last_10_stats$goal_diff_per_match
        goals_for_per_match_last_10[[row_index]] <- last_10_stats$goals_for_per_match
        goals_against_per_match_last_10[[row_index]] <- last_10_stats$goals_against_per_match
        draw_rate_last_10[[row_index]] <- last_10_stats$draw_rate

        completed_history <- dplyr::bind_rows(completed_history, current_row)
    }

    team_match_history |>
        dplyr::mutate(
            prior_matches = prior_matches,
            points_per_match_last_5 = points_per_match_last_5,
            goal_diff_per_match_last_5 = goal_diff_per_match_last_5,
            goals_for_per_match_last_5 = goals_for_per_match_last_5,
            goals_against_per_match_last_5 = goals_against_per_match_last_5,
            draw_rate_last_5 = draw_rate_last_5,
            points_per_match_last_10 = points_per_match_last_10,
            goal_diff_per_match_last_10 = goal_diff_per_match_last_10,
            goals_for_per_match_last_10 = goals_for_per_match_last_10,
            goals_against_per_match_last_10 = goals_against_per_match_last_10,
            draw_rate_last_10 = draw_rate_last_10,
            max_prior_date_used = max_prior_date_used
        )
}

message("============================================================")
message("Building lagged team-form features")
message("============================================================")

if (!file.exists(INPUT_PATH)) {
    stop("Input file not found: ", INPUT_PATH, call. = FALSE)
}

message("Reading input: ", INPUT_PATH)
match_table <- readr::read_csv(
    INPUT_PATH,
    show_col_types = FALSE,
    progress = FALSE
)
input_row_count <- nrow(match_table)

missing_required_columns <- setdiff(REQUIRED_COLUMNS, names(match_table))
if (length(missing_required_columns) > 0L) {
    stop(
        "Input is missing required columns: ",
        paste(missing_required_columns, collapse = ", "),
        call. = FALSE
    )
}

if (anyDuplicated(match_table$source_match_id) > 0L) {
    stop(
        "Input source_match_id is duplicated (",
        sum(duplicated(match_table$source_match_id)),
        " duplicates).",
        call. = FALSE
    )
}

message("Preparing team-match history (two rows per match)...")
match_table <- match_table |>
    dplyr::mutate(
        date = as.Date(.data$date),
        home_score = as.integer(.data$home_score),
        away_score = as.integer(.data$away_score)
    )

home_team_matches <- match_table |>
    dplyr::transmute(
        team = .data$home_team_clean,
        opponent = .data$away_team_clean,
        date = .data$date,
        source_match_id = .data$source_match_id,
        venue_role = "home",
        goals_for = .data$home_score,
        goals_against = .data$away_score,
        goal_diff = .data$home_score - .data$away_score,
        result = dplyr::case_when(
            .data$match_result == "H" ~ "W",
            .data$match_result == "D" ~ "D",
            .data$match_result == "A" ~ "L",
            TRUE ~ NA_character_
        ),
        points = dplyr::case_when(
            .data$match_result == "H" ~ 3L,
            .data$match_result == "D" ~ 1L,
            .data$match_result == "A" ~ 0L,
            TRUE ~ NA_integer_
        ),
        is_draw = as.integer(.data$match_result == "D")
    )

away_team_matches <- match_table |>
    dplyr::transmute(
        team = .data$away_team_clean,
        opponent = .data$home_team_clean,
        date = .data$date,
        source_match_id = .data$source_match_id,
        venue_role = "away",
        goals_for = .data$away_score,
        goals_against = .data$home_score,
        goal_diff = .data$away_score - .data$home_score,
        result = dplyr::case_when(
            .data$match_result == "H" ~ "L",
            .data$match_result == "D" ~ "D",
            .data$match_result == "A" ~ "W",
            TRUE ~ NA_character_
        ),
        points = dplyr::case_when(
            .data$match_result == "H" ~ 0L,
            .data$match_result == "D" ~ 1L,
            .data$match_result == "A" ~ 3L,
            TRUE ~ NA_integer_
        ),
        is_draw = as.integer(.data$match_result == "D")
    )

team_match_history <- dplyr::bind_rows(home_team_matches, away_team_matches) |>
    dplyr::arrange(.data$team, .data$date, .data$source_match_id, .data$venue_role)

team_ids <- sort(unique(team_match_history$team))
message(
    "Computing lagged form for ",
    length(team_ids),
    " teams..."
)

team_form_features <- purrr::map_dfr(
    team_ids,
    function(team_id) {
        team_rows <- team_match_history |>
            dplyr::filter(.data$team == team_id)

        compute_team_form_features(team_rows)
    }
)

message("Joining home-team form features back to match table...")
home_form <- team_form_features |>
    dplyr::filter(.data$venue_role == "home") |>
    dplyr::transmute(
        source_match_id = .data$source_match_id,
        home_prior_matches = .data$prior_matches,
        home_points_per_match_last_5 = .data$points_per_match_last_5,
        home_goal_diff_per_match_last_5 = .data$goal_diff_per_match_last_5,
        home_goals_for_per_match_last_5 = .data$goals_for_per_match_last_5,
        home_goals_against_per_match_last_5 = .data$goals_against_per_match_last_5,
        home_draw_rate_last_5 = .data$draw_rate_last_5,
        home_points_per_match_last_10 = .data$points_per_match_last_10,
        home_goal_diff_per_match_last_10 = .data$goal_diff_per_match_last_10,
        home_goals_for_per_match_last_10 = .data$goals_for_per_match_last_10,
        home_goals_against_per_match_last_10 = .data$goals_against_per_match_last_10,
        home_draw_rate_last_10 = .data$draw_rate_last_10,
        home_max_prior_date_used = .data$max_prior_date_used
    )

message("Joining away-team form features back to match table...")
away_form <- team_form_features |>
    dplyr::filter(.data$venue_role == "away") |>
    dplyr::transmute(
        source_match_id = .data$source_match_id,
        away_prior_matches = .data$prior_matches,
        away_points_per_match_last_5 = .data$points_per_match_last_5,
        away_goal_diff_per_match_last_5 = .data$goal_diff_per_match_last_5,
        away_goals_for_per_match_last_5 = .data$goals_for_per_match_last_5,
        away_goals_against_per_match_last_5 = .data$goals_against_per_match_last_5,
        away_draw_rate_last_5 = .data$draw_rate_last_5,
        away_points_per_match_last_10 = .data$points_per_match_last_10,
        away_goal_diff_per_match_last_10 = .data$goal_diff_per_match_last_10,
        away_goals_for_per_match_last_10 = .data$goals_for_per_match_last_10,
        away_goals_against_per_match_last_10 = .data$goals_against_per_match_last_10,
        away_draw_rate_last_10 = .data$draw_rate_last_10,
        away_max_prior_date_used = .data$max_prior_date_used
    )

output_table <- match_table |>
    dplyr::left_join(home_form, by = "source_match_id") |>
    dplyr::left_join(away_form, by = "source_match_id") |>
    dplyr::mutate(
        form_points_diff_last_5 = .data$home_points_per_match_last_5 -
            .data$away_points_per_match_last_5,
        form_goal_diff_diff_last_5 = .data$home_goal_diff_per_match_last_5 -
            .data$away_goal_diff_per_match_last_5,
        form_goals_for_diff_last_5 = .data$home_goals_for_per_match_last_5 -
            .data$away_goals_for_per_match_last_5,
        form_goals_against_diff_last_5 = .data$home_goals_against_per_match_last_5 -
            .data$away_goals_against_per_match_last_5,
        form_draw_rate_mean_last_5 = (
            .data$home_draw_rate_last_5 + .data$away_draw_rate_last_5
        ) / 2,
        form_draw_rate_diff_last_5 = .data$home_draw_rate_last_5 -
            .data$away_draw_rate_last_5,
        form_points_diff_last_10 = .data$home_points_per_match_last_10 -
            .data$away_points_per_match_last_10,
        form_goal_diff_diff_last_10 = .data$home_goal_diff_per_match_last_10 -
            .data$away_goal_diff_per_match_last_10,
        form_goals_for_diff_last_10 = .data$home_goals_for_per_match_last_10 -
            .data$away_goals_for_per_match_last_10,
        form_goals_against_diff_last_10 = .data$home_goals_against_per_match_last_10 -
            .data$away_goals_against_per_match_last_10,
        form_draw_rate_mean_last_10 = (
            .data$home_draw_rate_last_10 + .data$away_draw_rate_last_10
        ) / 2,
        form_draw_rate_diff_last_10 = .data$home_draw_rate_last_10 -
            .data$away_draw_rate_last_10
    ) |>
    dplyr::select(-dplyr::any_of(c(
        "home_max_prior_date_used",
        "away_max_prior_date_used"
    )))

output_row_count <- nrow(output_table)

if (output_row_count != input_row_count) {
    stop(
        "Output row count (",
        output_row_count,
        ") differs from input row count (",
        input_row_count,
        ").",
        call. = FALSE
    )
}

if (anyDuplicated(output_table$source_match_id) > 0L) {
    stop(
        "Output source_match_id is duplicated (",
        sum(duplicated(output_table$source_match_id)),
        " duplicates).",
        call. = FALSE
    )
}

message("Running leakage checks...")
leakage_check <- output_table |>
    dplyr::left_join(
        home_form |>
            dplyr::select(
                source_match_id,
                home_max_prior_date_used
            ),
        by = "source_match_id"
    ) |>
    dplyr::left_join(
        away_form |>
            dplyr::select(
                source_match_id,
                away_max_prior_date_used
            ),
        by = "source_match_id"
    ) |>
    dplyr::mutate(
        home_leakage_violation = !is.na(.data$home_max_prior_date_used) &
            .data$home_max_prior_date_used >= .data$date,
        away_leakage_violation = !is.na(.data$away_max_prior_date_used) &
            .data$away_max_prior_date_used >= .data$date,
        any_leakage_violation = .data$home_leakage_violation |
            .data$away_leakage_violation
    )

leakage_summary <- tibble::tibble(
    metric = c(
        "rows_checked",
        "home_leakage_violations",
        "away_leakage_violations",
        "any_leakage_violations",
        "max_home_prior_date_used",
        "max_away_prior_date_used"
    ),
    value = c(
        nrow(leakage_check),
        sum(leakage_check$home_leakage_violation, na.rm = TRUE),
        sum(leakage_check$away_leakage_violation, na.rm = TRUE),
        sum(leakage_check$any_leakage_violation, na.rm = TRUE),
        as.character(max(leakage_check$home_max_prior_date_used, na.rm = TRUE)),
        as.character(max(leakage_check$away_max_prior_date_used, na.rm = TRUE))
    )
)

readr::write_csv(leakage_summary, LEAKAGE_PATH)

if (any(leakage_check$any_leakage_violation, na.rm = TRUE)) {
    stop(
        "Leakage check failed: at least one row uses prior history with date ",
        "not strictly less than the current match date. See ",
        LEAKAGE_PATH,
        call. = FALSE
    )
}

rows_with_5_for_both <- output_table |>
    dplyr::filter(
        .data$home_prior_matches >= LAST_5_WINDOW,
        .data$away_prior_matches >= LAST_5_WINDOW
    ) |>
    nrow()

rows_with_10_for_both <- output_table |>
    dplyr::filter(
        .data$home_prior_matches >= LAST_10_WINDOW,
        .data$away_prior_matches >= LAST_10_WINDOW
    ) |>
    nrow()

message(
    "Rows with at least 5 prior matches for both teams: ",
    rows_with_5_for_both
)
message(
    "Rows with at least 10 prior matches for both teams: ",
    rows_with_10_for_both
)

complete_last_5_rows <- output_table |>
    dplyr::filter(
        dplyr::if_all(
            dplyr::all_of(LAST_5_FEATURE_COLUMNS),
            ~ !is.na(.x)
        )
    ) |>
    nrow()

complete_last_10_rows <- output_table |>
    dplyr::filter(
        dplyr::if_all(
            dplyr::all_of(LAST_10_FEATURE_COLUMNS),
            ~ !is.na(.x)
        )
    ) |>
    nrow()

percent_complete_last_5 <- 100 * complete_last_5_rows / input_row_count
percent_complete_last_10 <- 100 * complete_last_10_rows / input_row_count

missingness_table <- purrr::map_dfr(
    FORM_FEATURE_COLUMNS,
    function(feature_name) {
        feature_values <- output_table[[feature_name]]
        missing_count <- sum(is.na(feature_values))
        tibble::tibble(
            feature = feature_name,
            missing_count = missing_count,
            missing_rate = missing_count / input_row_count
        )
    }
)

feature_summary <- tibble::tibble(
    metric = c(
        "input_rows",
        "output_rows",
        "number_of_teams",
        "min_date",
        "max_date",
        "added_form_features",
        "rows_with_5_prior_matches_both_teams",
        "rows_with_10_prior_matches_both_teams",
        "percent_rows_complete_last_5_form",
        "percent_rows_complete_last_10_form",
        "total_missing_values_all_form_features",
        "overall_missing_rate_all_form_features"
    ),
    value = c(
        input_row_count,
        output_row_count,
        length(team_ids),
        as.character(min(output_table$date, na.rm = TRUE)),
        as.character(max(output_table$date, na.rm = TRUE)),
        length(FORM_FEATURE_COLUMNS),
        rows_with_5_for_both,
        rows_with_10_for_both,
        percent_complete_last_5,
        percent_complete_last_10,
        sum(missingness_table$missing_count),
        mean(missingness_table$missing_rate)
    )
)

message("Writing validation outputs...")
readr::write_csv(feature_summary, SUMMARY_PATH)
readr::write_csv(missingness_table, MISSINGNESS_PATH)

message("Writing output table: ", OUTPUT_PATH)
readr::write_csv(output_table, OUTPUT_PATH)

message("============================================================")
message("Lagged team-form feature build complete")
message("Input rows: ", input_row_count)
message("Output rows: ", output_row_count)
message("Added form features: ", length(FORM_FEATURE_COLUMNS))
message(
    "Percent of rows with complete last-5 form features: ",
    round(percent_complete_last_5, 2),
    "%"
)
message(
    "Percent of rows with complete last-10 form features: ",
    round(percent_complete_last_10, 2),
    "%"
)
message("Output file path: ", OUTPUT_PATH)
message("============================================================")
