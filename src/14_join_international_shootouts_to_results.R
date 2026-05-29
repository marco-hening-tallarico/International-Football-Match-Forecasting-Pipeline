# 14_join_international_shootouts_to_results.R
#
# Left-joins processed shootout outcomes onto international match results
# so knockout matches retain penalty-winner information without changing
# regulation-time scores.
#
# Reads:
# - data/processed/international_results.csv
# - data/processed/international_shootouts.csv
#
# Writes:
# - data/processed/international_results_with_shootouts.csv
# - data/validation/processed_data/international_results_shootout_*.csv

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

RESULTS_PATH <- file.path(PROCESSED_DIR, "international_results.csv")
SHOOTOUTS_PATH <- file.path(PROCESSED_DIR, "international_shootouts.csv")
OUTPUT_PATH <- file.path(
    PROCESSED_DIR,
    "international_results_with_shootouts.csv"
)
SUMMARY_PATH <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_results_shootout_join_summary.csv"
)
UNMATCHED_PATH <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_results_shootout_unmatched.csv"
)

FALLBACK_JOIN_COLS <- c("date", "home_team", "away_team", "tournament")

if (!file.exists(RESULTS_PATH)) {
    stop(
        "Missing international_results.csv. ",
        "Run src/08_clean_international_results.R first."
    )
}

if (!file.exists(SHOOTOUTS_PATH)) {
    stop(
        "Missing international_shootouts.csv. ",
        "Run src/08c_clean_international_shootouts.R first."
    )
}

international_results <- read_processed_csv(RESULTS_PATH)
international_shootouts <- safe_read_csv(SHOOTOUTS_PATH)

if (is.null(international_shootouts)) {
    stop("Could not read international_shootouts.csv.")
}

results_row_count <- nrow(international_results)
shootouts_row_count <- nrow(international_shootouts)

required_results_cols <- c(
    "source_match_id",
    "date",
    "home_team",
    "away_team",
    "tournament",
    "match_result",
    "result_class"
)
required_shootouts_cols <- c(
    "date",
    "home_team",
    "away_team",
    "winner"
)

missing_results_cols <- setdiff(
    required_results_cols,
    names(international_results)
)
missing_shootouts_cols <- setdiff(
    required_shootouts_cols,
    names(international_shootouts)
)

if (length(missing_results_cols) > 0L) {
    stop(
        "international_results.csv is missing required columns: ",
        paste(missing_results_cols, collapse = ", ")
    )
}

if (length(missing_shootouts_cols) > 0L) {
    stop(
        "international_shootouts.csv is missing required columns: ",
        paste(missing_shootouts_cols, collapse = ", ")
    )
}

has_source_match_id_in_both <- "source_match_id" %in% names(international_shootouts)

if (has_source_match_id_in_both) {
    join_cols <- "source_match_id"
    join_method <- "source_match_id"
    international_shootouts_for_join <- international_shootouts |>
        dplyr::transmute(
            source_match_id = as.character(.data$source_match_id),
            shootout_winner = stringr::str_squish(as.character(.data$winner))
        )
} else {
    join_cols <- FALLBACK_JOIN_COLS
    join_method <- paste(FALLBACK_JOIN_COLS, collapse = ", ")

    tournament_lookup <- international_results |>
        dplyr::group_by(
            .data$date,
            .data$home_team,
            .data$away_team
        ) |>
        dplyr::summarise(
            tournament = if (dplyr::n_distinct(.data$tournament) == 1L) {
                dplyr::first(.data$tournament)
            } else {
                NA_character_
            },
            .groups = "drop"
        )

    international_shootouts_for_join <- international_shootouts |>
        dplyr::left_join(
            tournament_lookup,
            by = c("date", "home_team", "away_team")
        ) |>
        dplyr::transmute(
            date = .data$date,
            home_team = .data$home_team,
            away_team = .data$away_team,
            tournament = .data$tournament,
            shootout_winner = stringr::str_squish(as.character(.data$winner))
        )
}

if (anyDuplicated(international_shootouts_for_join[, join_cols, drop = FALSE]) > 0L) {
    stop(
        "international_shootouts has duplicate join keys on ",
        join_method,
        "."
    )
}

international_results_with_shootouts <- international_results |>
    dplyr::left_join(
        international_shootouts_for_join |>
            dplyr::select(dplyr::all_of(c(join_cols, "shootout_winner"))),
        by = join_cols,
        relationship = "many-to-one"
    ) |>
    dplyr::mutate(
        shootout_played = !is.na(.data$shootout_winner) &
            .data$shootout_winner != "",
        shootout_winner = dplyr::if_else(
            .data$shootout_played,
            .data$shootout_winner,
            NA_character_
        ),
        home_won_shootout = dplyr::if_else(
            .data$shootout_played,
            as.integer(.data$shootout_winner == .data$home_team),
            0L
        ),
        away_won_shootout = dplyr::if_else(
            .data$shootout_played,
            as.integer(.data$shootout_winner == .data$away_team),
            0L
        )
    )

output_row_count <- nrow(international_results_with_shootouts)
shootout_played_count <- sum(
    international_results_with_shootouts$shootout_played,
    na.rm = TRUE
)
matched_shootout_rows <- international_shootouts_for_join |>
    dplyr::inner_join(
        international_results |>
            dplyr::select(dplyr::all_of(join_cols)),
        by = join_cols
    ) |>
    nrow()

duplicate_source_match_id_rows <- international_results_with_shootouts |>
    dplyr::count(.data$source_match_id, name = "duplicate_count") |>
    dplyr::filter(.data$duplicate_count > 1L) |>
    nrow()

invalid_shootout_winners <- international_results_with_shootouts |>
    dplyr::filter(
        .data$shootout_played,
        .data$home_won_shootout + .data$away_won_shootout != 1L
    ) |>
    nrow()

if (output_row_count != results_row_count) {
    stop(
        "Join changed international_results row count. ",
        "Expected ",
        results_row_count,
        ", got ",
        output_row_count,
        "."
    )
}

if (duplicate_source_match_id_rows > 0L) {
    stop(
        "international_results_with_shootouts has duplicate source_match_id ",
        "values after join."
    )
}

if (shootout_played_count != matched_shootout_rows) {
    stop(
        "shootout_played count (",
        shootout_played_count,
        ") does not equal matched shootout rows (",
        matched_shootout_rows,
        ")."
    )
}

if (invalid_shootout_winners > 0L) {
    stop(
        "Found ",
        invalid_shootout_winners,
        " shootout rows where winner is neither home nor away team."
    )
}

regulation_result_unchanged <- identical(
    international_results$match_result,
    international_results_with_shootouts$match_result
) &&
    identical(
        international_results$result_class,
        international_results_with_shootouts$result_class
    )

if (!regulation_result_unchanged) {
    stop(
        "Join altered regulation-time match_result or result_class columns."
    )
}

shootout_identity_cols <- c("date", "home_team", "away_team")

matched_shootout_identity <- international_shootouts_for_join |>
    dplyr::inner_join(
        international_results_with_shootouts |>
            dplyr::filter(.data$shootout_played) |>
            dplyr::select(dplyr::all_of(join_cols)),
        by = join_cols
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(shootout_identity_cols)))

international_results_shootout_unmatched <- international_shootouts |>
    dplyr::anti_join(
        matched_shootout_identity,
        by = shootout_identity_cols
    )

if (nrow(international_results_shootout_unmatched) !=
        shootouts_row_count - matched_shootout_rows) {
    stop(
        "Unmatched shootout row count does not reconcile with input shootouts."
    )
}

join_summary <- tibble::tibble(
    metric = c(
        "results_rows",
        "shootouts_rows",
        "output_rows",
        "join_method",
        "shootout_played_rows",
        "matched_shootout_rows",
        "unmatched_shootout_rows",
        "duplicate_source_match_id_groups",
        "invalid_shootout_winner_rows",
        "regulation_match_result_unchanged"
    ),
    value = c(
        as.character(results_row_count),
        as.character(shootouts_row_count),
        as.character(output_row_count),
        join_method,
        as.character(shootout_played_count),
        as.character(matched_shootout_rows),
        as.character(nrow(international_results_shootout_unmatched)),
        as.character(duplicate_source_match_id_rows),
        as.character(invalid_shootout_winners),
        as.character(regulation_result_unchanged)
    )
)

readr::write_csv(international_results_with_shootouts, OUTPUT_PATH)
readr::write_csv(join_summary, SUMMARY_PATH)
readr::write_csv(international_results_shootout_unmatched, UNMATCHED_PATH)

message("Done.")
message("Join method: ", join_method)
message("International results rows (preserved): ", results_row_count)
message("Shootout rows in source table: ", shootouts_row_count)
message("Rows with shootout_played = TRUE: ", shootout_played_count)
message("Unmatched shootouts: ", nrow(international_results_shootout_unmatched))
message("Output: ", OUTPUT_PATH)
message("Join summary: ", SUMMARY_PATH)
message("Unmatched shootouts: ", UNMATCHED_PATH)
