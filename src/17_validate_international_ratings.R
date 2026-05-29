# ============================================================
# 17_validate_international_ratings.R
# Validate cleaned international team ratings and match coverage
#
# Inputs:
#   data/processed/international_team_ratings.csv
#   data/processed/international_results_with_shootouts.csv
#
# Outputs:
#   data/validation/international_team_ratings_validation_summary.csv
#   data/validation/international_team_ratings_unmatched_teams.csv
#   data/validation/international_team_ratings_coverage_by_decade.csv
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

RATINGS_PATH <- file.path(PROCESSED_DIR, "international_team_ratings.csv")
MATCHES_PATH <- file.path(
    PROCESSED_DIR,
    "international_results_with_shootouts.csv"
)
SUMMARY_PATH <- file.path(
    VALIDATION_DIR,
    "international_team_ratings_validation_summary.csv"
)
UNMATCHED_TEAMS_PATH <- file.path(
    VALIDATION_DIR,
    "international_team_ratings_unmatched_teams.csv"
)
COVERAGE_BY_DECADE_PATH <- file.path(
    VALIDATION_DIR,
    "international_team_ratings_coverage_by_decade.csv"
)

REQUIRED_RATING_COLS <- c(
    "rating_date",
    "team",
    "team_clean",
    "rating",
    "rank",
    "source"
)

fail <- function(message) {
    stop(message, call. = FALSE)
}

if (!file.exists(RATINGS_PATH)) {
    fail(paste(
        "Missing international_team_ratings.csv.",
        "Run src/16_clean_international_ratings.R first."
    ))
}

if (!file.exists(MATCHES_PATH)) {
    fail(paste(
        "Missing international_results_with_shootouts.csv.",
        "Run src/14_join_international_shootouts_to_results.R first."
    ))
}

international_team_ratings <- readr::read_csv(
    RATINGS_PATH,
    show_col_types = FALSE
)
international_matches <- read_processed_csv(MATCHES_PATH)

missing_rating_cols <- setdiff(
    REQUIRED_RATING_COLS,
    names(international_team_ratings)
)

if (length(missing_rating_cols) > 0L) {
    fail(paste(
        "international_team_ratings.csv is missing required columns:",
        paste(missing_rating_cols, collapse = ", ")
    ))
}

rating_dates <- suppressWarnings(as.Date(international_team_ratings$rating_date))
ratings_numeric <- suppressWarnings(
    as.numeric(international_team_ratings$rating)
)

if (any(is.na(rating_dates))) {
    fail("rating_date contains values that do not parse as Date.")
}

if (any(is.na(ratings_numeric))) {
    fail("rating contains non-numeric or missing values.")
}

if (any(
    is.na(international_team_ratings$team_clean) |
        international_team_ratings$team_clean == "",
    na.rm = TRUE
)) {
    fail("team_clean has missing values.")
}

duplicate_rating_keys <- international_team_ratings |>
    dplyr::count(.data$team_clean, .data$rating_date, name = "n") |>
    dplyr::filter(.data$n > 1L)

if (nrow(duplicate_rating_keys) > 0L) {
    fail(paste(
        "Duplicate team_clean + rating_date rows:",
        nrow(duplicate_rating_keys)
    ))
}

match_teams <- c(
    international_matches$home_team,
    international_matches$away_team
) |>
    unique() |>
    sort()

rating_teams <- international_team_ratings$team |>
    unique() |>
    sort()

make_team_clean <- function(team_name) {
    team_name |>
        as.character() |>
        stringr::str_squish() |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("[^a-z0-9]+", "_") |>
        stringr::str_replace_all("^_|_$", "")
}

match_team_lookup <- tibble::tibble(
    team = match_teams,
    team_clean = make_team_clean(match_teams)
)

rating_team_lookup <- international_team_ratings |>
    dplyr::distinct(.data$team, .data$team_clean)

international_team_ratings_unmatched_teams <- match_team_lookup |>
    dplyr::anti_join(
        rating_team_lookup,
        by = "team_clean"
    ) |>
    dplyr::arrange(.data$team)

ratings_for_lookup <- international_team_ratings |>
    dplyr::transmute(
        team_clean = .data$team_clean,
        rating_date = rating_dates,
        rating = ratings_numeric
    ) |>
    dplyr::arrange(.data$team_clean, .data$rating_date)

matches_for_coverage <- international_matches |>
    dplyr::transmute(
        source_match_id = .data$source_match_id,
        date = .data$date,
        home_team_clean = make_team_clean(.data$home_team),
        away_team_clean = make_team_clean(.data$away_team),
        decade = paste0(floor(lubridate::year(.data$date) / 10) * 10, "s")
    )

matches_table <- data.table::as.data.table(matches_for_coverage)
ratings_table <- data.table::as.data.table(ratings_for_lookup)
data.table::setorder(ratings_table, team_clean, rating_date)

home_prior_lookup <- ratings_table[
    matches_table,
    on = .(team_clean = home_team_clean, rating_date < date),
    mult = "last",
    nomatch = NA,
    .(source_match_id, home_has_prior_rating = !is.na(rating))
]

away_prior_lookup <- ratings_table[
    matches_table,
    on = .(team_clean = away_team_clean, rating_date < date),
    mult = "last",
    nomatch = NA,
    .(source_match_id, away_has_prior_rating = !is.na(rating))
]

matches_for_coverage <- matches_for_coverage |>
    dplyr::left_join(
        as.data.frame(home_prior_lookup),
        by = "source_match_id"
    ) |>
    dplyr::left_join(
        as.data.frame(away_prior_lookup),
        by = "source_match_id"
    ) |>
    dplyr::mutate(
        home_has_prior_rating = dplyr::coalesce(
            .data$home_has_prior_rating,
            FALSE
        ),
        away_has_prior_rating = dplyr::coalesce(
            .data$away_has_prior_rating,
            FALSE
        ),
        both_have_prior_rating = .data$home_has_prior_rating &
            .data$away_has_prior_rating
    )

international_team_ratings_coverage_by_decade <- matches_for_coverage |>
    dplyr::group_by(.data$decade) |>
    dplyr::summarise(
        match_rows = dplyr::n(),
        home_prior_rating_rows = sum(.data$home_has_prior_rating),
        away_prior_rating_rows = sum(.data$away_has_prior_rating),
        both_prior_rating_rows = sum(.data$both_have_prior_rating),
        .groups = "drop"
    ) |>
    dplyr::mutate(
        pct_both_prior_rating = round(
            100 * .data$both_prior_rating_rows / .data$match_rows,
            2
        )
    ) |>
    dplyr::arrange(.data$decade)

validation_summary <- tibble::tibble(
    metric = c(
        "rating_rows",
        "rating_teams",
        "min_rating_date",
        "max_rating_date",
        "match_teams",
        "unmatched_match_teams",
        "duplicate_team_date_keys",
        "matches_with_both_prior_ratings",
        "pct_matches_with_both_prior_ratings"
    ),
    value = c(
        as.character(nrow(international_team_ratings)),
        as.character(dplyr::n_distinct(international_team_ratings$team)),
        as.character(min(rating_dates, na.rm = TRUE)),
        as.character(max(rating_dates, na.rm = TRUE)),
        as.character(length(match_teams)),
        as.character(nrow(international_team_ratings_unmatched_teams)),
        as.character(nrow(duplicate_rating_keys)),
        as.character(sum(matches_for_coverage$both_have_prior_rating)),
        as.character(round(
            100 * mean(matches_for_coverage$both_have_prior_rating),
            2
        ))
    )
)

readr::write_csv(validation_summary, SUMMARY_PATH)
readr::write_csv(
    international_team_ratings_unmatched_teams,
    UNMATCHED_TEAMS_PATH
)
readr::write_csv(
    international_team_ratings_coverage_by_decade,
    COVERAGE_BY_DECADE_PATH
)

message("Done.")
message("Rating rows: ", nrow(international_team_ratings))
message("Rating teams: ", dplyr::n_distinct(international_team_ratings$team))
message("Unmatched match teams: ", nrow(international_team_ratings_unmatched_teams))
message("Wrote: ", SUMMARY_PATH)
message("Wrote: ", UNMATCHED_TEAMS_PATH)
message("Wrote: ", COVERAGE_BY_DECADE_PATH)
