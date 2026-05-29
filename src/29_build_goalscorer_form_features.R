# 29_build_goalscorer_form_features.R
#
# Adds pre-match goalscorer and attacking-depth aggregates to the form-enabled
# modeling table. Goal counts and minutes must come from goals before the
# fixture date.
#
# Reads:
# - data/processed/international_modeling_table_with_form.csv
#   (falls back to international_modeling_table.csv)
# - data/processed/international_goalscorers.csv
#
# Writes:
# - data/processed/international_modeling_table_with_form_and_goalscorers.csv
# - data/validation/engineered_features/goalscorer_form_*.csv

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

INPUT_WITH_FORM_PATH <- file.path(
    PROCESSED_DIR,
    "international_modeling_table_with_form.csv"
)
INPUT_BASE_PATH <- file.path(
    PROCESSED_DIR,
    "international_modeling_table.csv"
)
GOALSCORERS_PATH <- file.path(PROCESSED_DIR, "international_goalscorers.csv")
OUTPUT_PATH <- file.path(
    PROCESSED_DIR,
    "international_modeling_table_with_form_and_goalscorers.csv"
)
SUMMARY_PATH <- file.path(
    VALIDATION_ENGINEERED_DIR,
    "goalscorer_form_feature_summary.csv"
)
MISSINGNESS_PATH <- file.path(
    VALIDATION_ENGINEERED_DIR,
    "goalscorer_form_feature_missingness.csv"
)
EXAMPLES_PATH <- file.path(
    VALIDATION_ENGINEERED_DIR,
    "goalscorer_form_feature_examples.csv"
)
LEAKAGE_AUDIT_PATH <- file.path(
    VALIDATION_ENGINEERED_DIR,
    "goalscorer_form_leakage_audit.csv"
)

MODELING_REQUIRED_COLUMNS <- c(
    "date",
    "home_team",
    "away_team",
    "result_class"
)

LAST_10_MATCH_WINDOW <- 10L
ROLLING_DAYS_WINDOW <- 365L
EXAMPLE_ROW_COUNT <- 25L
LEAKAGE_AUDIT_ROW_COUNT <- 25L

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

GOALSCORER_FEATURE_COLUMNS <- c(
    HOME_GOALSCORER_COLUMNS,
    AWAY_GOALSCORER_COLUMNS,
    DIFF_GOALSCORER_COLUMNS
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

resolve_first_column <- function(
    data_frame,
    candidate_names,
    label
) {
    matched <- intersect(candidate_names, names(data_frame))
    if (length(matched) == 0L) {
        return(NULL)
    }
    matched[[1L]]
}

zero_attacking_goal_features <- function(include_avg_minute) {
    list(
        unique_scorers = 0,
        goals_by_top_scorer = 0,
        non_penalty_goals = 0,
        penalty_goals = 0,
        avg_goal_minute = NA_real_
    )
}

aggregate_attacking_goal_features <- function(
    goal_rows,
    include_avg_minute
) {
    if (nrow(goal_rows) == 0L) {
        return(zero_attacking_goal_features(include_avg_minute))
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

compute_team_goalscorer_features <- function(
    team_match_history,
    team_goal_history,
    include_avg_minute
) {
    team_match_history <- team_match_history |>
        dplyr::arrange(.data$date, .data$source_match_id, .data$venue_role)

    n_rows <- nrow(team_match_history)
    unique_scorers_last_10 <- rep(NA_real_, n_rows)
    goals_by_top_scorer_last_10 <- rep(NA_real_, n_rows)
    non_penalty_goals_last_10 <- rep(NA_real_, n_rows)
    penalty_goals_last_10 <- rep(NA_real_, n_rows)
    unique_scorers_365d <- rep(NA_real_, n_rows)
    goals_by_top_scorer_365d <- rep(NA_real_, n_rows)
    non_penalty_goals_365d <- rep(NA_real_, n_rows)
    penalty_goals_365d <- rep(NA_real_, n_rows)
    avg_goal_minute_365d <- rep(NA_real_, n_rows)
    prior_goal_rows <- rep(NA_integer_, n_rows)
    max_prior_goal_date_used <- as.Date(rep(NA, n_rows))

    completed_matches <- team_match_history[0L, , drop = FALSE]

    for (row_index in seq_len(n_rows)) {
        current_row <- team_match_history[row_index, , drop = FALSE]
        current_date <- current_row$date[[1L]]
        window_start_date <- current_date - ROLLING_DAYS_WINDOW

        prior_matches <- completed_matches |>
            dplyr::filter(.data$date < current_date)

        prior_goals <- team_goal_history |>
            dplyr::filter(.data$goal_date < current_date)

        prior_goal_rows[[row_index]] <- nrow(prior_goals)
        has_prior_matches <- nrow(prior_matches) > 0L

        if (nrow(prior_goals) > 0L) {
            max_prior_goal_date_used[[row_index]] <- max(prior_goals$goal_date)
        }

        if (has_prior_matches) {
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
                include_avg_minute = include_avg_minute
            )
        } else {
            last_10_stats <- list(
                unique_scorers = NA_real_,
                goals_by_top_scorer = NA_real_,
                non_penalty_goals = NA_real_,
                penalty_goals = NA_real_
            )
            stats_365d <- list(
                unique_scorers = NA_real_,
                goals_by_top_scorer = NA_real_,
                non_penalty_goals = NA_real_,
                penalty_goals = NA_real_,
                avg_goal_minute = NA_real_
            )
        }

        unique_scorers_last_10[[row_index]] <- last_10_stats$unique_scorers
        goals_by_top_scorer_last_10[[row_index]] <- last_10_stats$goals_by_top_scorer
        non_penalty_goals_last_10[[row_index]] <- last_10_stats$non_penalty_goals
        penalty_goals_last_10[[row_index]] <- last_10_stats$penalty_goals

        unique_scorers_365d[[row_index]] <- stats_365d$unique_scorers
        goals_by_top_scorer_365d[[row_index]] <- stats_365d$goals_by_top_scorer
        non_penalty_goals_365d[[row_index]] <- stats_365d$non_penalty_goals
        penalty_goals_365d[[row_index]] <- stats_365d$penalty_goals
        avg_goal_minute_365d[[row_index]] <- stats_365d$avg_goal_minute

        completed_matches <- dplyr::bind_rows(completed_matches, current_row)
    }

    team_match_history |>
        dplyr::mutate(
            prior_goal_rows = prior_goal_rows,
            unique_scorers_last_10 = unique_scorers_last_10,
            goals_by_top_scorer_last_10 = goals_by_top_scorer_last_10,
            non_penalty_goals_last_10 = non_penalty_goals_last_10,
            penalty_goals_last_10 = penalty_goals_last_10,
            unique_scorers_365d = unique_scorers_365d,
            goals_by_top_scorer_365d = goals_by_top_scorer_365d,
            non_penalty_goals_365d = non_penalty_goals_365d,
            penalty_goals_365d = penalty_goals_365d,
            avg_goal_minute_365d = avg_goal_minute_365d,
            max_prior_goal_date_used = max_prior_goal_date_used
        )
}

message("============================================================")
message("Building goalscorer form features")
message("============================================================")

if (file.exists(INPUT_WITH_FORM_PATH)) {
    input_path <- INPUT_WITH_FORM_PATH
} else {
    input_path <- INPUT_BASE_PATH
    warning(
    "Input with form not found at ",
    INPUT_WITH_FORM_PATH,
    ". Falling back to ",
    INPUT_BASE_PATH,
    ".",
    call. = FALSE
  )
}

if (!file.exists(input_path)) {
    stop("Modeling table input not found: ", input_path, call. = FALSE)
}

if (!file.exists(GOALSCORERS_PATH)) {
    stop("Goalscorers input not found: ", GOALSCORERS_PATH, call. = FALSE)
}

message("Reading modeling table: ", input_path)
match_table <- readr::read_csv(
    input_path,
    show_col_types = FALSE,
    progress = FALSE
)
input_row_count <- nrow(match_table)

missing_modeling_columns <- setdiff(
    MODELING_REQUIRED_COLUMNS,
    names(match_table)
)
if (length(missing_modeling_columns) > 0L) {
    stop(
        "Modeling table is missing required columns: ",
        paste(missing_modeling_columns, collapse = ", "),
        call. = FALSE
    )
}

if (!"source_match_id" %in% names(match_table)) {
    stop(
        "Modeling table is missing source_match_id, which is required to join ",
        "features back to matches.",
        call. = FALSE
    )
}

if (anyDuplicated(match_table$source_match_id) > 0L) {
    stop(
        "Modeling table source_match_id is duplicated (",
        sum(duplicated(match_table$source_match_id)),
        " duplicates).",
        call. = FALSE
    )
}

message("Reading goalscorers: ", GOALSCORERS_PATH)
goalscorers_table <- readr::read_csv(
    GOALSCORERS_PATH,
    show_col_types = FALSE,
    progress = FALSE
)

skipped_features <- tibble::tibble(
    feature = character(),
    reason = character()
)

goal_date_column <- resolve_first_column(
    goalscorers_table,
    c("date", "match_date", "goal_date"),
    "goal date"
)
home_team_column <- resolve_first_column(
    goalscorers_table,
    c("home_team", "home"),
    "home team"
)
away_team_column <- resolve_first_column(
    goalscorers_table,
    c("away_team", "away"),
    "away team"
)
scoring_team_column <- resolve_first_column(
    goalscorers_table,
    c("team", "scoring_team"),
    "scoring team"
)
scorer_column <- resolve_first_column(
    goalscorers_table,
    c("scorer", "player", "goal_scorer"),
    "scorer"
)
minute_column <- resolve_first_column(
    goalscorers_table,
    c("minute", "goal_minute"),
    "minute"
)
own_goal_column <- resolve_first_column(
    goalscorers_table,
    c("own_goal", "is_own_goal"),
    "own goal"
)
penalty_column <- resolve_first_column(
    goalscorers_table,
    c("penalty", "is_penalty"),
    "penalty"
)

required_goalscorer_identity <- c(
    goal_date_column,
    home_team_column,
    away_team_column,
    scoring_team_column,
    scorer_column
)
if (any(is.null(required_goalscorer_identity))) {
    stop(
        "Goalscorers table is missing required identity columns. ",
        "Available columns: ",
        paste(names(goalscorers_table), collapse = ", "),
        call. = FALSE
    )
}

has_minute_column <- !is.null(minute_column)
has_own_goal_column <- !is.null(own_goal_column)
has_penalty_column <- !is.null(penalty_column)

if (!has_minute_column) {
    skipped_features <- dplyr::bind_rows(
        skipped_features,
        tibble::tibble(
            feature = "home_avg_goal_minute_365d",
            reason = "minute column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "away_avg_goal_minute_365d",
            reason = "minute column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "avg_goal_minute_diff_365d",
            reason = "minute column unavailable in goalscorers input"
        )
    )
}

if (!has_own_goal_column) {
    warning(
        "own_goal column unavailable; own goals cannot be excluded explicitly.",
        call. = FALSE
    )
}

if (!has_penalty_column) {
    skipped_features <- dplyr::bind_rows(
        skipped_features,
        tibble::tibble(
            feature = "home_non_penalty_goals_last_10",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "away_non_penalty_goals_last_10",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "home_penalty_goals_last_10",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "away_penalty_goals_last_10",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "non_penalty_goals_diff_last_10",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "penalty_goals_diff_last_10",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "home_non_penalty_goals_365d",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "away_non_penalty_goals_365d",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "home_penalty_goals_365d",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "away_penalty_goals_365d",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "non_penalty_goals_diff_365d",
            reason = "penalty column unavailable in goalscorers input"
        ),
        tibble::tibble(
            feature = "penalty_goals_diff_365d",
            reason = "penalty column unavailable in goalscorers input"
        )
    )
}

goalscorers_prepared <- goalscorers_table |>
    dplyr::transmute(
        goal_date = as.Date(.data[[goal_date_column]]),
        home_team = stringr::str_squish(as.character(.data[[home_team_column]])),
        away_team = stringr::str_squish(as.character(.data[[away_team_column]])),
        team = stringr::str_squish(as.character(.data[[scoring_team_column]])),
        scorer = stringr::str_squish(as.character(.data[[scorer_column]])),
        minute = if (has_minute_column) {
            suppressWarnings(as.numeric(.data[[minute_column]]))
        } else {
            NA_real_
        },
        own_goal = if (has_own_goal_column) {
            as.logical(.data[[own_goal_column]])
        } else {
            FALSE
        },
        penalty = if (has_penalty_column) {
            as.logical(.data[[penalty_column]])
        } else {
            NA
        },
        source_match_key = build_source_match_key(
            .data[[goal_date_column]],
            .data[[home_team_column]],
            .data[[away_team_column]]
        )
    ) |>
    dplyr::filter(
        !is.na(.data$goal_date),
        !is.na(.data$team),
        .data$team != "",
        !is.na(.data$scorer),
        .data$scorer != ""
    )

message("Preparing team-match history from modeling table...")
match_table <- match_table |>
    dplyr::mutate(
        date = as.Date(.data$date),
        source_match_key = build_source_match_key(
            .data$date,
            .data$home_team,
            .data$away_team
        )
    )

home_team_matches <- match_table |>
    dplyr::transmute(
        team = .data$home_team,
        opponent = .data$away_team,
        date = .data$date,
        source_match_id = .data$source_match_id,
        source_match_key = .data$source_match_key,
        venue_role = "home"
    )

away_team_matches <- match_table |>
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

team_ids <- sort(unique(team_match_history$team))
message(
    "Computing goalscorer form for ",
    length(team_ids),
    " teams..."
)

team_goalscorer_features <- purrr::map_dfr(
    team_ids,
    function(team_id) {
        team_matches <- team_match_history |>
            dplyr::filter(.data$team == team_id)

        team_goals <- goalscorers_prepared |>
            dplyr::filter(.data$team == team_id) |>
            dplyr::arrange(.data$goal_date, .data$source_match_key, .data$scorer)

        compute_team_goalscorer_features(
            team_matches,
            team_goals,
            include_avg_minute = has_minute_column
        )
    }
)

message("Joining home goalscorer features back to match table...")
home_goalscorer_features <- team_goalscorer_features |>
    dplyr::filter(.data$venue_role == "home") |>
    dplyr::transmute(
        source_match_id = .data$source_match_id,
        home_prior_goal_rows = .data$prior_goal_rows,
        home_unique_scorers_last_10 = .data$unique_scorers_last_10,
        home_goals_by_top_scorer_last_10 = .data$goals_by_top_scorer_last_10,
        home_non_penalty_goals_last_10 = .data$non_penalty_goals_last_10,
        home_penalty_goals_last_10 = .data$penalty_goals_last_10,
        home_unique_scorers_365d = .data$unique_scorers_365d,
        home_goals_by_top_scorer_365d = .data$goals_by_top_scorer_365d,
        home_non_penalty_goals_365d = .data$non_penalty_goals_365d,
        home_penalty_goals_365d = .data$penalty_goals_365d,
        home_avg_goal_minute_365d = .data$avg_goal_minute_365d,
        home_max_prior_goal_date_used = .data$max_prior_goal_date_used
    )

message("Joining away goalscorer features back to match table...")
away_goalscorer_features <- team_goalscorer_features |>
    dplyr::filter(.data$venue_role == "away") |>
    dplyr::transmute(
        source_match_id = .data$source_match_id,
        away_prior_goal_rows = .data$prior_goal_rows,
        away_unique_scorers_last_10 = .data$unique_scorers_last_10,
        away_goals_by_top_scorer_last_10 = .data$goals_by_top_scorer_last_10,
        away_non_penalty_goals_last_10 = .data$non_penalty_goals_last_10,
        away_penalty_goals_last_10 = .data$penalty_goals_last_10,
        away_unique_scorers_365d = .data$unique_scorers_365d,
        away_goals_by_top_scorer_365d = .data$goals_by_top_scorer_365d,
        away_non_penalty_goals_365d = .data$non_penalty_goals_365d,
        away_penalty_goals_365d = .data$penalty_goals_365d,
        away_avg_goal_minute_365d = .data$avg_goal_minute_365d,
        away_max_prior_goal_date_used = .data$max_prior_goal_date_used
    )

output_table <- match_table |>
    dplyr::left_join(home_goalscorer_features, by = "source_match_id") |>
    dplyr::left_join(away_goalscorer_features, by = "source_match_id") |>
    dplyr::mutate(
        unique_scorers_diff_last_10 = .data$home_unique_scorers_last_10 -
            .data$away_unique_scorers_last_10,
        top_scorer_goals_diff_last_10 = .data$home_goals_by_top_scorer_last_10 -
            .data$away_goals_by_top_scorer_last_10,
        non_penalty_goals_diff_last_10 = .data$home_non_penalty_goals_last_10 -
            .data$away_non_penalty_goals_last_10,
        penalty_goals_diff_last_10 = .data$home_penalty_goals_last_10 -
            .data$away_penalty_goals_last_10,
        unique_scorers_diff_365d = .data$home_unique_scorers_365d -
            .data$away_unique_scorers_365d,
        top_scorer_goals_diff_365d = .data$home_goals_by_top_scorer_365d -
            .data$away_goals_by_top_scorer_365d,
        non_penalty_goals_diff_365d = .data$home_non_penalty_goals_365d -
            .data$away_non_penalty_goals_365d,
        penalty_goals_diff_365d = .data$home_penalty_goals_365d -
            .data$away_penalty_goals_365d,
        avg_goal_minute_diff_365d = if (has_minute_column) {
            .data$home_avg_goal_minute_365d - .data$away_avg_goal_minute_365d
        } else {
            NA_real_
        }
    )

if (!has_minute_column) {
    output_table <- output_table |>
        dplyr::mutate(
            home_avg_goal_minute_365d = NA_real_,
            away_avg_goal_minute_365d = NA_real_,
            avg_goal_minute_diff_365d = NA_real_
        )
}

if (!has_penalty_column) {
    penalty_related_columns <- c(
        "home_non_penalty_goals_last_10",
        "away_non_penalty_goals_last_10",
        "home_penalty_goals_last_10",
        "away_penalty_goals_last_10",
        "non_penalty_goals_diff_last_10",
        "penalty_goals_diff_last_10",
        "home_non_penalty_goals_365d",
        "away_non_penalty_goals_365d",
        "home_penalty_goals_365d",
        "away_penalty_goals_365d",
        "non_penalty_goals_diff_365d",
        "penalty_goals_diff_365d"
    )
    output_table <- output_table |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(penalty_related_columns),
                ~ NA_real_
            )
        )
}

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

active_feature_columns <- GOALSCORER_FEATURE_COLUMNS
if (!has_minute_column) {
    active_feature_columns <- setdiff(
        active_feature_columns,
        c(
            "home_avg_goal_minute_365d",
            "away_avg_goal_minute_365d",
            "avg_goal_minute_diff_365d"
        )
    )
}
if (!has_penalty_column) {
    active_feature_columns <- setdiff(
        active_feature_columns,
        c(
            "home_non_penalty_goals_last_10",
            "away_non_penalty_goals_last_10",
            "home_penalty_goals_last_10",
            "away_penalty_goals_last_10",
            "non_penalty_goals_diff_last_10",
            "penalty_goals_diff_last_10",
            "home_non_penalty_goals_365d",
            "away_non_penalty_goals_365d",
            "home_penalty_goals_365d",
            "away_penalty_goals_365d",
            "non_penalty_goals_diff_365d",
            "penalty_goals_diff_365d"
        )
    )
}

message("Building validation tables...")
missingness_table <- purrr::map_dfr(
    active_feature_columns,
    function(feature_name) {
        feature_values <- output_table[[feature_name]]
        missing_count <- sum(is.na(feature_values))
        tibble::tibble(
            feature = feature_name,
            missing_count = missing_count,
            missing_percent = 100 * missing_count / input_row_count
        )
    }
)

matches_without_home_goal_history <- sum(
    output_table$home_prior_goal_rows == 0L,
    na.rm = TRUE
)
matches_without_away_goal_history <- sum(
    output_table$away_prior_goal_rows == 0L,
    na.rm = TRUE
)

future_goal_leakage_violations <- output_table |>
    dplyr::transmute(
        home_violation = !is.na(.data$home_max_prior_goal_date_used) &
            .data$home_max_prior_goal_date_used >= .data$date,
        away_violation = !is.na(.data$away_max_prior_goal_date_used) &
            .data$away_max_prior_goal_date_used >= .data$date
    )

any_future_goal_leakage <- any(
    future_goal_leakage_violations$home_violation,
    na.rm = TRUE
) ||
    any(future_goal_leakage_violations$away_violation, na.rm = TRUE)

set.seed(2026)
audit_match_ids <- output_table |>
    dplyr::filter(
        dplyr::if_any(
            dplyr::all_of(active_feature_columns),
            ~ !is.na(.x)
        )
    ) |>
    dplyr::slice_sample(n = LEAKAGE_AUDIT_ROW_COUNT) |>
    dplyr::pull(.data$source_match_id)

leakage_audit <- output_table |>
    dplyr::filter(.data$source_match_id %in% audit_match_ids) |>
    dplyr::transmute(
        source_match_id = .data$source_match_id,
        date = .data$date,
        home_team = .data$home_team,
        away_team = .data$away_team,
        result_class = .data$result_class,
        home_max_prior_goal_date_used = .data$home_max_prior_goal_date_used,
        away_max_prior_goal_date_used = .data$away_max_prior_goal_date_used,
        home_leakage_violation = !is.na(.data$home_max_prior_goal_date_used) &
            .data$home_max_prior_goal_date_used >= .data$date,
        away_leakage_violation = !is.na(.data$away_max_prior_goal_date_used) &
            .data$away_max_prior_goal_date_used >= .data$date,
        home_goals_used_count = purrr::map2_int(
            .data$home_team,
            .data$date,
            function(team_name, match_date) {
                sum(
                    goalscorers_prepared$team == team_name &
                        goalscorers_prepared$goal_date < match_date,
                    na.rm = TRUE
                )
            }
        ),
        away_goals_used_count = purrr::map2_int(
            .data$away_team,
            .data$date,
            function(team_name, match_date) {
                sum(
                    goalscorers_prepared$team == team_name &
                        goalscorers_prepared$goal_date < match_date,
                    na.rm = TRUE
                )
            }
        ),
        home_latest_goal_date_strictly_before_match = purrr::map2(
            .data$home_team,
            .data$date,
            function(team_name, match_date) {
                team_goal_dates <- goalscorers_prepared$goal_date[
                    goalscorers_prepared$team == team_name &
                        goalscorers_prepared$goal_date < match_date
                ]
                if (length(team_goal_dates) == 0L) {
                    return(as.Date(NA))
                }
                max(team_goal_dates)
            }
        ) |>
            unlist() |>
            as.Date(origin = "1970-01-01"),
        away_latest_goal_date_strictly_before_match = purrr::map2(
            .data$away_team,
            .data$date,
            function(team_name, match_date) {
                team_goal_dates <- goalscorers_prepared$goal_date[
                    goalscorers_prepared$team == team_name &
                        goalscorers_prepared$goal_date < match_date
                ]
                if (length(team_goal_dates) == 0L) {
                    return(as.Date(NA))
                }
                max(team_goal_dates)
            }
        ) |>
            unlist() |>
            as.Date(origin = "1970-01-01"),
        audit_passed = !.data$home_leakage_violation & !.data$away_leakage_violation
    )

if (any(!leakage_audit$audit_passed, na.rm = TRUE)) {
    stop(
        "Leakage audit failed for at least one sampled match. See ",
        LEAKAGE_AUDIT_PATH,
        call. = FALSE
    )
}

example_rows <- output_table |>
    dplyr::arrange(.data$date, .data$source_match_id) |>
    dplyr::slice_head(n = EXAMPLE_ROW_COUNT) |>
    dplyr::select(
        date,
        home_team,
        away_team,
        result_class,
        dplyr::all_of(active_feature_columns)
    )

feature_summary <- tibble::tibble(
    metric = c(
        "input_rows",
        "output_rows",
        "added_feature_count",
        "added_feature_names",
        "input_path",
        "goalscorers_path",
        "min_date",
        "max_date",
        "matches_without_home_prior_goalscorer_history",
        "matches_without_away_prior_goalscorer_history",
        "future_goal_leakage_violations",
        "no_future_goals_used_confirmation",
        "goalscorer_rows_used",
        "minute_column_used",
        "penalty_column_used",
        "own_goal_column_used"
    ),
    value = c(
        input_row_count,
        output_row_count,
        length(active_feature_columns),
        paste(active_feature_columns, collapse = "; "),
        input_path,
        GOALSCORERS_PATH,
        as.character(min(output_table$date, na.rm = TRUE)),
        as.character(max(output_table$date, na.rm = TRUE)),
        matches_without_home_goal_history,
        matches_without_away_goal_history,
        sum(
            future_goal_leakage_violations$home_violation,
            future_goal_leakage_violations$away_violation,
            na.rm = TRUE
        ),
        as.character(!any_future_goal_leakage),
        nrow(goalscorers_prepared),
        as.character(has_minute_column),
        as.character(has_penalty_column),
        as.character(has_own_goal_column)
    )
)

if (any_future_goal_leakage) {
    stop(
        "Leakage check failed: at least one row uses goals with date ",
        "not strictly less than the current match date.",
        call. = FALSE
    )
}

output_table <- output_table |>
    dplyr::select(-dplyr::any_of(c(
        "home_prior_goal_rows",
        "away_prior_goal_rows",
        "home_max_prior_goal_date_used",
        "away_max_prior_goal_date_used",
        "source_match_key"
    )))

message("Writing validation outputs...")
readr::write_csv(feature_summary, SUMMARY_PATH)
readr::write_csv(missingness_table, MISSINGNESS_PATH)
readr::write_csv(example_rows, EXAMPLES_PATH)
readr::write_csv(leakage_audit, LEAKAGE_AUDIT_PATH)

if (nrow(skipped_features) > 0L) {
    readr::write_csv(
        skipped_features,
        file.path(VALIDATION_DIR, "goalscorer_form_skipped_features.csv")
    )
}

message("Writing output table: ", OUTPUT_PATH)
readr::write_csv(output_table, OUTPUT_PATH)

message("============================================================")
message("Goalscorer form feature build complete")
message("Input rows: ", input_row_count)
message("Output rows: ", output_row_count)
message("Added goalscorer features: ", length(active_feature_columns))
message(
    "Matches without home prior goalscorer history: ",
    matches_without_home_goal_history
)
message(
    "Matches without away prior goalscorer history: ",
    matches_without_away_goal_history
)
message("Output file path: ", OUTPUT_PATH)
message("============================================================")
