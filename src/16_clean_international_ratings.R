# 16_clean_international_ratings.R
#
# Cleans World Football Elo ratings for modeling joins: normalized team names,
# rating dates, and a crosswalk for names that differ from match results.
#
# Reads: data/raw/international_ratings/world_football_elo.csv
#
# Writes:
# - data/processed/international_team_ratings.csv
# - data/validation/international_team_ratings_cleaning_summary.csv
# - data/metadata/team_name_crosswalk.csv (created or updated)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

RAW_RATINGS_PATH <- file.path(
    RAW_DIR,
    "international_ratings",
    "world_football_elo.csv"
)
OUTPUT_PATH <- file.path(PROCESSED_DIR, "international_team_ratings.csv")
SUMMARY_PATH <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_team_ratings_cleaning_summary.csv"
)
CROSSWALK_PATH <- file.path(META_DIR, "team_name_crosswalk.csv")
RESULTS_PATH <- file.path(PROCESSED_DIR, "international_results.csv")
RATINGS_SOURCE <- "world_football_elo"

standardize_column_names <- function(column_names) {
    clean_names <- janitor::make_clean_names(column_names)
    rename_map <- c(
        date = "rating_date",
        team_name = "team",
        elo = "rating",
        elo_rating = "rating",
        world_rank = "rank"
    )

    for (old_name in names(rename_map)) {
        clean_names[clean_names == old_name] <- rename_map[[old_name]]
    }

    clean_names
}

infer_first_present_column <- function(dat, candidates, label) {
    match_idx <- match(candidates, names(dat))

    if (all(is.na(match_idx))) {
        stop(
            "Could not infer ",
            label,
            " column. Expected one of: ",
            paste(candidates, collapse = ", "),
            call. = FALSE
        )
    }

    names(dat)[match_idx[!is.na(match_idx)][1L]]
}

if (!file.exists(RAW_RATINGS_PATH)) {
    stop(
        "Missing raw ratings file: ",
        RAW_RATINGS_PATH,
        "\nRun src/15_download_international_ratings.R first, or place a manual ",
        "World Football Elo CSV at that path.",
        call. = FALSE
    )
}

ratings_raw <- readr::read_csv(RAW_RATINGS_PATH, show_col_types = FALSE)
names(ratings_raw) <- standardize_column_names(names(ratings_raw))

date_col <- infer_first_present_column(
    ratings_raw,
    c("rating_date", "date", "from", "to"),
    "rating_date"
)
team_col <- infer_first_present_column(
    ratings_raw,
    c("team", "country", "club", "nation"),
    "team"
)
rating_col <- infer_first_present_column(
    ratings_raw,
    c("rating", "elo"),
    "rating"
)
rank_col <- if ("rank" %in% names(ratings_raw)) "rank" else NA_character_

input_rows <- nrow(ratings_raw)

international_team_ratings <- ratings_raw |>
    dplyr::transmute(
        rating_date = suppressWarnings(as.Date(.data[[date_col]])),
        team = stringr::str_squish(as.character(.data[[team_col]])),
        rating = suppressWarnings(as.numeric(.data[[rating_col]])),
        rank = if (!is.na(rank_col)) {
            suppressWarnings(as.integer(.data[[rank_col]]))
        } else {
            NA_integer_
        },
        source = RATINGS_SOURCE
    ) |>
    dplyr::filter(
        !is.na(.data$rating_date),
        !is.na(.data$team),
        .data$team != "",
        !is.na(.data$rating)
    ) |>
    dplyr::mutate(
        team_clean = make_team_clean(.data$team),
        source = RATINGS_SOURCE
    ) |>
    dplyr::distinct(
        .data$rating_date,
        .data$team,
        .data$team_clean,
        .data$rating,
        .data$rank,
        .data$source,
        .keep_all = FALSE
    ) |>
    dplyr::arrange(.data$team_clean, .data$rating_date)

duplicate_keys <- international_team_ratings |>
    dplyr::count(.data$team_clean, .data$rating_date, name = "n") |>
    dplyr::filter(.data$n > 1L)

if (nrow(duplicate_keys) > 0L) {
    stop(
        "Duplicate team_clean + rating_date rows remain after cleaning: ",
        nrow(duplicate_keys),
        call. = FALSE
    )
}

if (nrow(international_team_ratings) == 0L) {
    stop("No rating rows remain after cleaning.", call. = FALSE)
}

results_teams <- character()

if (file.exists(RESULTS_PATH)) {
    international_results <- read_processed_csv(RESULTS_PATH)
    results_teams <- c(
        international_results$home_team,
        international_results$away_team
    ) |>
        unique()
}

rating_teams <- unique(international_team_ratings$team)

all_raw_teams <- unique(c(results_teams, rating_teams))
all_raw_teams <- all_raw_teams[!is.na(all_raw_teams) & all_raw_teams != ""]

team_name_crosswalk <- load_team_name_crosswalk(CROSSWALK_PATH)

existing_keys <- team_name_crosswalk |>
    dplyr::distinct(.data$source, .data$raw_team)

new_team_rows <- tibble::tibble(
    source = RATINGS_SOURCE,
    raw_team = all_raw_teams,
    team_clean = make_team_clean(all_raw_teams),
    canonical_team = all_raw_teams,
    notes = "auto_initialized"
) |>
    dplyr::anti_join(existing_keys, by = c("source", "raw_team"))

if (nrow(new_team_rows) > 0L) {
    exact_match_notes <- new_team_rows |>
        dplyr::group_by(.data$team_clean) |>
        dplyr::mutate(
            notes = dplyr::if_else(
                dplyr::n() > 1L,
                "needs_review_duplicate_team_clean",
                .data$notes
            )
        ) |>
        dplyr::ungroup()
} else {
    exact_match_notes <- new_team_rows
}

team_name_crosswalk <- dplyr::bind_rows(
    team_name_crosswalk,
    exact_match_notes
) |>
    dplyr::distinct(.data$source, .data$raw_team, .keep_all = TRUE) |>
    dplyr::arrange(.data$source, .data$team_clean, .data$raw_team)

team_name_crosswalk <- apply_standard_result_to_elo_mappings(
    team_name_crosswalk,
    available_elo_teams = unique(international_team_ratings$team)
)

crosswalk_candidates_for_review <- team_name_crosswalk |>
    dplyr::filter(.data$notes == "needs_review_duplicate_team_clean")

cleaning_summary <- tibble::tibble(
    metric = c(
        "input_rows",
        "output_rows",
        "n_teams",
        "min_rating_date",
        "max_rating_date",
        "rows_dropped",
        "duplicate_key_groups",
        "crosswalk_rows",
        "crosswalk_review_rows"
    ),
    value = c(
        as.character(input_rows),
        as.character(nrow(international_team_ratings)),
        as.character(dplyr::n_distinct(international_team_ratings$team)),
        as.character(min(international_team_ratings$rating_date)),
        as.character(max(international_team_ratings$rating_date)),
        as.character(input_rows - nrow(international_team_ratings)),
        as.character(nrow(duplicate_keys)),
        as.character(nrow(team_name_crosswalk)),
        as.character(nrow(crosswalk_candidates_for_review))
    )
)

readr::write_csv(international_team_ratings, OUTPUT_PATH)
readr::write_csv(cleaning_summary, SUMMARY_PATH)
readr::write_csv(team_name_crosswalk, CROSSWALK_PATH)

message("Done.")
message("Wrote: ", OUTPUT_PATH)
message("Rating rows: ", nrow(international_team_ratings))
message("Teams: ", dplyr::n_distinct(international_team_ratings$team))
message("Wrote: ", SUMMARY_PATH)
message("Wrote: ", CROSSWALK_PATH)
