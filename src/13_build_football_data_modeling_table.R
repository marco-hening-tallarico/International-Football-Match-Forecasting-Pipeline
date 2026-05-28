# ============================================================
# 13_build_football_data_modeling_table.R
# Join football-data.co.uk core matches with wide odds for modeling
#
# Inputs:
#   data/processed/football_data_uk_matches_core.csv
#   data/processed/football_data_uk_odds_wide.csv
#
# Outputs:
#   data/processed/football_data_uk_modeling_table.csv
#   data/validation/football_data_uk_modeling_table_validation.csv
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

CHRONOLOGICAL_SPLIT_DATE <- as.Date("2018-01-01")
MARKET_LOG_LOSS_EPSILON <- 1e-15

MATCH_IDENTITY_COLS <- c(
    "source",
    "raw_file",
    "source_match_id",
    "source_league_code",
    "source_season_code",
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

OPTIONAL_POSTMATCH_COLS <- c(
    "half_time_home_score",
    "half_time_away_score",
    "half_time_result",
    "home_shots",
    "away_shots",
    "home_shots_on_target",
    "away_shots_on_target",
    "home_corners",
    "away_corners",
    "home_fouls",
    "away_fouls",
    "home_yellow_cards",
    "away_yellow_cards",
    "home_red_cards",
    "away_red_cards"
)

MARKET_SUMMARY_COLS <- c(
    "avg_home_odds",
    "avg_draw_odds",
    "avg_away_odds",
    "max_home_odds",
    "max_draw_odds",
    "max_away_odds",
    "closing_home_odds",
    "closing_draw_odds",
    "closing_away_odds",
    "home_implied_prob",
    "draw_implied_prob",
    "away_implied_prob",
    "market_overround"
)

ODDS_JOIN_IDENTIFIER_COLS <- c(
    "raw_file",
    "source_league_code",
    "source_season_code",
    "date",
    "home_team",
    "away_team"
)

MODELING_FLAG_COLS <- c(
    "has_closing_odds",
    "has_market_probs",
    "is_modeling_candidate"
)

MARKET_PREDICTION_COLS <- c(
    "market_predicted_class",
    "market_predicted_result",
    "market_predicted_prob",
    "market_prob_actual_result",
    "market_log_loss"
)

core_path <- file.path(PROCESSED_DIR, "football_data_uk_matches_core.csv")
odds_path <- file.path(PROCESSED_DIR, "football_data_uk_odds_wide.csv")
output_path <- file.path(PROCESSED_DIR, "football_data_uk_modeling_table.csv")
validation_path <- file.path(
    VALIDATION_DIR,
    "football_data_uk_modeling_table_validation.csv"
)

if (!file.exists(core_path)) {
    stop(
        "Missing football_data_uk_matches_core.csv. ",
        "Run src/06_clean_football_data_uk.R first."
    )
}

if (!file.exists(odds_path)) {
    stop(
        "Missing football_data_uk_odds_wide.csv locally. ",
        "Run src/06_clean_football_data_uk.R first to generate it."
    )
}

football_data_uk_matches_core <- readr::read_csv(
    core_path,
    col_types = processed_csv_col_types(core_path),
    show_col_types = FALSE
)

football_data_uk_odds_wide <- readr::read_csv(
    odds_path,
    col_types = processed_csv_col_types(odds_path),
    show_col_types = FALSE
)

core_rows <- nrow(football_data_uk_matches_core)
odds_rows <- nrow(football_data_uk_odds_wide)

odds_for_join <- football_data_uk_odds_wide |>
    dplyr::select(-dplyr::any_of(ODDS_JOIN_IDENTIFIER_COLS))

football_data_uk_modeling_table <- football_data_uk_matches_core |>
    dplyr::left_join(
        odds_for_join,
        by = "source_match_id",
        relationship = "many-to-one"
    )

football_data_uk_modeling_table <- football_data_uk_modeling_table |>
    dplyr::mutate(
        has_closing_odds = !is.na(.data$closing_home_odds) &
            !is.na(.data$closing_draw_odds) &
            !is.na(.data$closing_away_odds) &
            .data$closing_home_odds > 0 &
            .data$closing_draw_odds > 0 &
            .data$closing_away_odds > 0,
        has_market_probs = !is.na(.data$home_implied_prob) &
            !is.na(.data$draw_implied_prob) &
            !is.na(.data$away_implied_prob) &
            !is.na(.data$market_overround) &
            .data$home_implied_prob > 0 &
            .data$draw_implied_prob > 0 &
            .data$away_implied_prob > 0 &
            .data$market_overround > 0,
        is_modeling_candidate = !is.na(.data$result_class) &
            .data$has_closing_odds &
            .data$has_market_probs &
            !is.na(.data$date) &
            !is.na(.data$home_team) &
            !is.na(.data$away_team),
        market_predicted_class = dplyr::if_else(
            .data$has_market_probs,
            dplyr::case_when(
                .data$home_implied_prob >= .data$draw_implied_prob &
                    .data$home_implied_prob >= .data$away_implied_prob ~ 1L,
                .data$draw_implied_prob >= .data$away_implied_prob ~ 0L,
                TRUE ~ -1L
            ),
            NA_integer_
        ),
        market_predicted_result = dplyr::case_when(
            .data$market_predicted_class == 1L ~ "H",
            .data$market_predicted_class == 0L ~ "D",
            .data$market_predicted_class == -1L ~ "A",
            TRUE ~ NA_character_
        ),
        market_predicted_prob = dplyr::if_else(
            .data$has_market_probs,
            pmax(
                .data$home_implied_prob,
                .data$draw_implied_prob,
                .data$away_implied_prob
            ),
            NA_real_
        ),
        market_prob_actual_result = dplyr::case_when(
            .data$result_class == 1L ~ .data$home_implied_prob,
            .data$result_class == 0L ~ .data$draw_implied_prob,
            .data$result_class == -1L ~ .data$away_implied_prob,
            TRUE ~ NA_real_
        ),
        market_log_loss = dplyr::if_else(
            .data$is_modeling_candidate,
            -log(pmax(.data$market_prob_actual_result, MARKET_LOG_LOSS_EPSILON)),
            NA_real_
        ),
        data_split = dplyr::case_when(
            is.na(.data$date) ~ NA_character_,
            .data$date < CHRONOLOGICAL_SPLIT_DATE ~ "train",
            TRUE ~ "test"
        )
    )

present_match_identity_cols <- intersect(
    MATCH_IDENTITY_COLS,
    names(football_data_uk_modeling_table)
)
present_optional_postmatch_cols <- intersect(
    OPTIONAL_POSTMATCH_COLS,
    names(football_data_uk_modeling_table)
)
present_market_summary_cols <- intersect(
    MARKET_SUMMARY_COLS,
    names(football_data_uk_modeling_table)
)

bookmaker_odds_cols <- setdiff(
    names(football_data_uk_odds_wide),
    c("source_match_id", ODDS_JOIN_IDENTIFIER_COLS, MARKET_SUMMARY_COLS)
)
present_bookmaker_odds_cols <- intersect(
    bookmaker_odds_cols,
    names(football_data_uk_modeling_table)
)

football_data_uk_modeling_table <- football_data_uk_modeling_table |>
    dplyr::select(
        dplyr::all_of(present_match_identity_cols),
        dplyr::all_of(present_optional_postmatch_cols),
        dplyr::all_of(present_market_summary_cols),
        dplyr::all_of(present_bookmaker_odds_cols),
        dplyr::all_of(MODELING_FLAG_COLS),
        dplyr::all_of(MARKET_PREDICTION_COLS),
        data_split
    )

output_rows <- nrow(football_data_uk_modeling_table)

if (output_rows == 0L) {
    stop("football_data_uk_modeling_table has 0 rows after join.")
}

if (any(is.na(football_data_uk_modeling_table$source_match_id))) {
    stop("football_data_uk_modeling_table has rows with missing source_match_id.")
}

if (any(duplicated(football_data_uk_modeling_table$source_match_id))) {
    stop("football_data_uk_modeling_table has duplicate source_match_id values.")
}

valid_result_classes <- c(-1L, 0L, 1L)
non_missing_result_class <- football_data_uk_modeling_table$result_class[
    !is.na(football_data_uk_modeling_table$result_class)
]

if (length(non_missing_result_class) > 0L &&
        any(!non_missing_result_class %in% valid_result_classes)) {
    stop(
        "football_data_uk_modeling_table result_class contains values ",
        "other than -1, 0, and 1."
    )
}

scores_present <- !is.na(football_data_uk_modeling_table$home_score) &
    !is.na(football_data_uk_modeling_table$away_score)

if (any(
    scores_present &
        football_data_uk_modeling_table$total_goals !=
            football_data_uk_modeling_table$home_score +
                football_data_uk_modeling_table$away_score,
    na.rm = TRUE
)) {
    stop(
        "football_data_uk_modeling_table has rows where total_goals ",
        "does not equal home_score + away_score."
    )
}

if (any(
    scores_present &
        football_data_uk_modeling_table$goal_difference !=
            football_data_uk_modeling_table$home_score -
                football_data_uk_modeling_table$away_score,
    na.rm = TRUE
)) {
    stop(
        "football_data_uk_modeling_table has rows where goal_difference ",
        "does not equal home_score - away_score."
    )
}

odds_match_ids <- unique(football_data_uk_odds_wide$source_match_id)
modeling_candidate_rows <- sum(
    football_data_uk_modeling_table$is_modeling_candidate,
    na.rm = TRUE
)
train_rows <- sum(
    football_data_uk_modeling_table$data_split == "train",
    na.rm = TRUE
)
test_rows <- sum(
    football_data_uk_modeling_table$data_split == "test",
    na.rm = TRUE
)

all_teams <- c(
    football_data_uk_modeling_table$home_team,
    football_data_uk_modeling_table$away_team
)

modeling_log_loss <- football_data_uk_modeling_table$market_log_loss[
    football_data_uk_modeling_table$is_modeling_candidate &
        is.finite(football_data_uk_modeling_table$market_log_loss)
]

validation_summary <- tibble::tibble(
    core_rows = core_rows,
    odds_rows = odds_rows,
    output_rows = output_rows,
    modeling_candidate_rows = modeling_candidate_rows,
    train_rows = train_rows,
    test_rows = test_rows,
    missing_source_match_id_rows = sum(
        is.na(football_data_uk_modeling_table$source_match_id)
    ),
    duplicate_source_match_id_rows = sum(
        duplicated(football_data_uk_modeling_table$source_match_id)
    ),
    rows_without_odds = sum(
        !football_data_uk_modeling_table$source_match_id %in% odds_match_ids
    ),
    rows_without_closing_odds = sum(
        !football_data_uk_modeling_table$has_closing_odds,
        na.rm = TRUE
    ),
    rows_without_market_probs = sum(
        !football_data_uk_modeling_table$has_market_probs,
        na.rm = TRUE
    ),
    missing_result_class_rows = sum(
        is.na(football_data_uk_modeling_table$result_class)
    ),
    min_date = as.character(
        min(football_data_uk_modeling_table$date, na.rm = TRUE)
    ),
    max_date = as.character(
        max(football_data_uk_modeling_table$date, na.rm = TRUE)
    ),
    n_leagues = dplyr::n_distinct(
        football_data_uk_modeling_table$source_league_code
    ),
    n_seasons = dplyr::n_distinct(football_data_uk_modeling_table$season),
    n_teams = dplyr::n_distinct(all_teams),
    mean_market_log_loss = if (length(modeling_log_loss) > 0L) {
        mean(modeling_log_loss)
    } else {
        NA_real_
    },
    notes = paste(
        "Joined football_data_uk_matches_core with football_data_uk_odds_wide",
        "on source_match_id; one row per core match.",
        "Chronological split: train before 2018-01-01, test from 2018-01-01 onward.",
        "Market baseline uses unnormalized implied probabilities from odds table.",
        sep = " "
    )
)

readr::write_csv(football_data_uk_modeling_table, output_path)
readr::write_csv(validation_summary, validation_path)

message("football-data.co.uk modeling table rows: ", output_rows)
message("Modeling candidates: ", modeling_candidate_rows)
message("Wrote: ", output_path)
message("Wrote: ", validation_path)
