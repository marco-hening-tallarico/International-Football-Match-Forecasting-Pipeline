# 18_build_international_modeling_table.R
#
# Builds the international modeling table with pre-match Elo for both teams,
# rating age, tournament flags, and chronological train/validation/test labels.
#
# Reads:
# - data/processed/international_results_with_shootouts.csv
# - data/processed/international_team_ratings.csv
#
# Writes:
# - data/processed/international_modeling_table.csv
# - data/validation/international_modeling_table_summary.csv
# - data/validation/international_modeling_table_unmatched_ratings.csv
#
# Notes:
# - Ratings are the latest Elo on or before each match date.
# - Split date is CHRONOLOGICAL_SPLIT_DATE (2018-01-01).

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

MATCHES_PATH <- file.path(
    PROCESSED_DIR,
    "international_results_with_shootouts.csv"
)
RATINGS_PATH <- file.path(PROCESSED_DIR, "international_team_ratings.csv")
OUTPUT_PATH <- file.path(PROCESSED_DIR, "international_modeling_table.csv")
SUMMARY_PATH <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_modeling_table_summary.csv"
)
UNMATCHED_PATH <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_modeling_table_unmatched_ratings.csv"
)
UNRESOLVED_ELO_PATH <- file.path(
    VALIDATION_PROCESSED_DIR,
    "international_modeling_table_unresolved_elo_teams.csv"
)
CROSSWALK_PATH <- file.path(META_DIR, "team_name_crosswalk.csv")

PRESERVED_MATCH_COLS <- c(
    "source_match_id",
    "date",
    "season",
    "competition",
    "tournament",
    "home_team",
    "away_team",
    "home_score",
    "away_score",
    "match_result",
    "result_class",
    "home_win",
    "draw",
    "away_win",
    "neutral",
    "city",
    "country",
    "shootout_played",
    "shootout_winner",
    "home_won_shootout",
    "away_won_shootout"
)

add_tournament_flags <- function(match_table) {
    tournament_lower <- stringr::str_to_lower(
        stringr::str_squish(as.character(match_table$tournament))
    )

    is_world_cup_qualifier <- stringr::str_detect(
        tournament_lower,
        "world cup qualification|fifa world cup qualification"
    )

    match_table |>
        dplyr::mutate(
            is_friendly = stringr::str_detect(tournament_lower, "friendly"),
            is_world_cup = tournament_lower %in% c(
                "fifa world cup",
                "world cup"
            ),
            is_world_cup_qualifier = is_world_cup_qualifier,
            is_continental_qualifier = stringr::str_detect(
                tournament_lower,
                "qualification"
            ) & !is_world_cup_qualifier,
            is_continental_tournament = !is_friendly &
                !is_world_cup &
                !is_world_cup_qualifier &
                !is_continental_qualifier &
                stringr::str_detect(
                    tournament_lower,
                    "cup|championship|nations league|euro|gold cup|asian games"
                )
        )
}

lookup_pre_match_ratings <- function(matches, ratings) {
    ratings_table <- data.table::as.data.table(ratings)
    matches_table <- data.table::as.data.table(matches)
    matches_table[, rating_lookup_date := date - 1L]

    data.table::setorder(ratings_table, team_clean, rating_date)

    # data.table non-equi `rating_date < date` can still return same-day rows;
    # join against (match_date - 1) with `<=` for a strict pre-match lookup.
    rating_lookup <- ratings_table[
        matches_table,
        on = .(team_clean, rating_date <= rating_lookup_date),
        mult = "last",
        nomatch = NA,
        .(
            source_match_id = i.source_match_id,
            rating = x.rating,
            matched_rating_date = x.rating_date
        )
    ]

    as.data.frame(rating_lookup)
}

if (!file.exists(MATCHES_PATH)) {
    stop(
        "Missing international_results_with_shootouts.csv. ",
        "Run src/14_join_international_shootouts_to_results.R first.",
        call. = FALSE
    )
}

if (!file.exists(RATINGS_PATH)) {
    stop(
        "Missing international_team_ratings.csv. ",
        "Run src/16_clean_international_ratings.R first.",
        call. = FALSE
    )
}

international_matches <- read_processed_csv(MATCHES_PATH)
international_team_ratings <- readr::read_csv(
    RATINGS_PATH,
    show_col_types = FALSE
) |>
    dplyr::mutate(
        rating_date = as.Date(.data$rating_date),
        team_clean = make_team_clean(.data$team),
        rating = as.numeric(.data$rating)
    ) |>
    dplyr::arrange(.data$team_clean, .data$rating_date)

team_name_crosswalk <- load_team_name_crosswalk(CROSSWALK_PATH)
team_name_crosswalk <- apply_standard_result_to_elo_mappings(
    team_name_crosswalk,
    available_elo_teams = unique(international_team_ratings$team)
)
crosswalk_lookup <- build_result_to_elo_clean_lookup(team_name_crosswalk)
available_elo_team_clean <- unique(international_team_ratings$team_clean)

matches_row_count <- nrow(international_matches)

matches_for_join <- international_matches |>
    dplyr::mutate(
        home_team_clean = make_team_clean(.data$home_team),
        away_team_clean = make_team_clean(.data$away_team),
        home_elo_lookup_team_clean = resolve_elo_team_clean(
            .data$home_team,
            crosswalk_lookup = crosswalk_lookup,
            available_elo_team_clean = available_elo_team_clean
        ),
        away_elo_lookup_team_clean = resolve_elo_team_clean(
            .data$away_team,
            crosswalk_lookup = crosswalk_lookup,
            available_elo_team_clean = available_elo_team_clean
        )
    )

home_pre_match <- lookup_pre_match_ratings(
    matches_for_join |>
        dplyr::transmute(
            source_match_id = .data$source_match_id,
            date = .data$date,
            team_clean = .data$home_elo_lookup_team_clean
        ),
    international_team_ratings |>
        dplyr::transmute(
            team_clean = .data$team_clean,
            rating_date = .data$rating_date,
            rating = .data$rating
        )
) |>
    dplyr::rename(
        home_rating_pre_match = rating,
        home_rating_date = matched_rating_date
    )

away_pre_match <- lookup_pre_match_ratings(
    matches_for_join |>
        dplyr::transmute(
            source_match_id = .data$source_match_id,
            date = .data$date,
            team_clean = .data$away_elo_lookup_team_clean
        ),
    international_team_ratings |>
        dplyr::transmute(
            team_clean = .data$team_clean,
            rating_date = .data$rating_date,
            rating = .data$rating
        )
) |>
    dplyr::rename(
        away_rating_pre_match = rating,
        away_rating_date = matched_rating_date
    )

present_preserved_cols <- intersect(
    PRESERVED_MATCH_COLS,
    names(international_matches)
)

international_modeling_table <- international_matches |>
    dplyr::select(dplyr::all_of(present_preserved_cols)) |>
    dplyr::mutate(
        home_team_clean = make_team_clean(.data$home_team),
        away_team_clean = make_team_clean(.data$away_team)
    ) |>
    dplyr::left_join(home_pre_match, by = "source_match_id") |>
    dplyr::left_join(away_pre_match, by = "source_match_id") |>
    dplyr::mutate(
        home_rank_pre_match = NA_integer_,
        away_rank_pre_match = NA_integer_,
        rating_diff = .data$home_rating_pre_match - .data$away_rating_pre_match,
        rank_diff = NA_integer_,
        rating_age_days_home = as.integer(
            .data$date - .data$home_rating_date
        ),
        rating_age_days_away = as.integer(
            .data$date - .data$away_rating_date
        ),
        data_split = dplyr::if_else(
            .data$date < CHRONOLOGICAL_TEST_SPLIT_DATE,
            "train",
            "test"
        )
    ) |>
    add_tournament_flags() |>
    assign_data_split_modeling()

if (nrow(international_modeling_table) != matches_row_count) {
    stop(
        "Modeling table row count (",
        nrow(international_modeling_table),
        ") does not equal matches input (",
        matches_row_count,
        ").",
        call. = FALSE
    )
}

if (anyDuplicated(international_modeling_table$source_match_id) > 0L) {
    stop("source_match_id is not unique in international_modeling_table.")
}

if (any(is.na(international_modeling_table$date))) {
    stop("date has missing values in international_modeling_table.")
}

valid_result_classes <- c(-1L, 0L, 1L)
result_class_values <- suppressWarnings(as.integer(
    international_modeling_table$result_class
))

if (any(!result_class_values %in% valid_result_classes, na.rm = TRUE)) {
    stop("result_class must be only -1, 0, or 1.")
}

both_ratings_present <- !is.na(
    international_modeling_table$home_rating_pre_match
) & !is.na(international_modeling_table$away_rating_pre_match)

bad_rating_diff <- sum(
    both_ratings_present &
        abs(
            international_modeling_table$rating_diff -
                (
                    international_modeling_table$home_rating_pre_match -
                        international_modeling_table$away_rating_pre_match
                )
        ) > 1e-9,
    na.rm = TRUE
)

if (bad_rating_diff > 0L) {
    stop("rating_diff does not match home_rating_pre_match - away_rating_pre_match.")
}

home_rating_on_or_after_match <- sum(
    !is.na(international_modeling_table$home_rating_date) &
        international_modeling_table$home_rating_date >=
            international_modeling_table$date,
    na.rm = TRUE
)

if (home_rating_on_or_after_match > 0L) {
    stop(
        home_rating_on_or_after_match,
        " rows have home_rating_date on or after match date."
    )
}

away_rating_on_or_after_match <- sum(
    !is.na(international_modeling_table$away_rating_date) &
        international_modeling_table$away_rating_date >=
            international_modeling_table$date,
    na.rm = TRUE
)

if (away_rating_on_or_after_match > 0L) {
    stop(
        away_rating_on_or_after_match,
        " rows have away_rating_date on or after match date."
    )
}

home_rating_age_not_positive <- sum(
    !is.na(international_modeling_table$rating_age_days_home) &
        international_modeling_table$rating_age_days_home <= 0L,
    na.rm = TRUE
)

if (home_rating_age_not_positive > 0L) {
    stop(
        home_rating_age_not_positive,
        " rows have non-positive rating_age_days_home."
    )
}

away_rating_age_not_positive <- sum(
    !is.na(international_modeling_table$rating_age_days_away) &
        international_modeling_table$rating_age_days_away <= 0L,
    na.rm = TRUE
)

if (away_rating_age_not_positive > 0L) {
    stop(
        away_rating_age_not_positive,
        " rows have non-positive rating_age_days_away."
    )
}

home_same_day <- sum(
    !is.na(international_modeling_table$home_rating_date) &
        international_modeling_table$home_rating_date ==
            international_modeling_table$date,
    na.rm = TRUE
)

away_same_day <- sum(
    !is.na(international_modeling_table$away_rating_date) &
        international_modeling_table$away_rating_date ==
            international_modeling_table$date,
    na.rm = TRUE
)

home_after_match <- sum(
    !is.na(international_modeling_table$home_rating_date) &
        international_modeling_table$home_rating_date >
            international_modeling_table$date,
    na.rm = TRUE
)

away_after_match <- sum(
    !is.na(international_modeling_table$away_rating_date) &
        international_modeling_table$away_rating_date >
            international_modeling_table$date,
    na.rm = TRUE
)

if (home_same_day > 0L || away_same_day > 0L) {
    stop(
        "Same-day pre-match ratings detected (home_same_day=",
        home_same_day,
        ", away_same_day=",
        away_same_day,
        ")."
    )
}

if (home_after_match > 0L || away_after_match > 0L) {
    stop(
        "Post-match ratings detected (home_after_match=",
        home_after_match,
        ", away_after_match=",
        away_after_match,
        ")."
    )
}

if (!all(
    international_modeling_table$data_split %in% c("train", "test"),
    na.rm = TRUE
)) {
    stop("data_split must be only train or test.")
}

if (!all(
    international_modeling_table$data_split_modeling %in%
        c("train", "validation", "test"),
    na.rm = TRUE
)) {
    stop("data_split_modeling must be only train, validation, or test.")
}

international_modeling_table_unresolved_elo_teams <- dplyr::bind_rows(
    matches_for_join |>
        dplyr::transmute(
            team = .data$home_team,
            team_clean = .data$home_team_clean,
            elo_lookup_team_clean = .data$home_elo_lookup_team_clean
        ),
    matches_for_join |>
        dplyr::transmute(
            team = .data$away_team,
            team_clean = .data$away_team_clean,
            elo_lookup_team_clean = .data$away_elo_lookup_team_clean
        )
) |>
    dplyr::distinct(.data$team, .keep_all = TRUE) |>
    dplyr::filter(
        !.data$elo_lookup_team_clean %in% available_elo_team_clean
    ) |>
    dplyr::arrange(.data$team)

international_modeling_table_unmatched_ratings <- international_modeling_table |>
    dplyr::filter(
        is.na(.data$home_rating_pre_match) |
            is.na(.data$away_rating_pre_match)
    ) |>
    dplyr::select(
        .data$source_match_id,
        .data$date,
        .data$tournament,
        .data$home_team,
        .data$away_team,
        .data$home_team_clean,
        .data$away_team_clean,
        .data$home_rating_pre_match,
        .data$away_rating_pre_match
    )

coverage_by_decade <- international_modeling_table |>
    dplyr::mutate(
        decade = paste0(floor(lubridate::year(.data$date) / 10) * 10, "s"),
        has_both_ratings = !is.na(.data$home_rating_pre_match) &
            !is.na(.data$away_rating_pre_match)
    ) |>
    dplyr::group_by(.data$decade) |>
    dplyr::summarise(
        match_rows = dplyr::n(),
        both_ratings_rows = sum(.data$has_both_ratings),
        pct_both_ratings = round(
            100 * .data$both_ratings_rows / .data$match_rows,
            2
        ),
        .groups = "drop"
    )

coverage_by_tournament_type <- international_modeling_table |>
    dplyr::summarise(
        match_rows = dplyr::n(),
        friendly_rows = sum(.data$is_friendly, na.rm = TRUE),
        world_cup_rows = sum(.data$is_world_cup, na.rm = TRUE),
        world_cup_qualifier_rows = sum(
            .data$is_world_cup_qualifier,
            na.rm = TRUE
        ),
        continental_tournament_rows = sum(
            .data$is_continental_tournament,
            na.rm = TRUE
        ),
        continental_qualifier_rows = sum(
            .data$is_continental_qualifier,
            na.rm = TRUE
        ),
        both_ratings_rows = sum(
            !is.na(.data$home_rating_pre_match) &
                !is.na(.data$away_rating_pre_match),
            na.rm = TRUE
        ),
        pct_both_ratings = round(
            100 * .data$both_ratings_rows / .data$match_rows,
            2
        )
    )

all_rating_ages_days <- c(
    international_modeling_table$rating_age_days_home,
    international_modeling_table$rating_age_days_away
)
all_rating_ages_days <- all_rating_ages_days[!is.na(all_rating_ages_days)]

modeling_summary <- tibble::tibble(
    metric = c(
        "matches_input_rows",
        "modeling_table_rows",
        "train_rows",
        "validation_rows",
        "test_rows",
        "train_modeling_rows",
        "crosswalk_mapping_rows",
        "unresolved_elo_team_rows",
        "both_ratings_rows",
        "pct_both_ratings",
        "rows_missing_home_rating",
        "rows_missing_away_rating",
        "unmatched_rating_rows",
        "min_rating_age_days",
        "median_rating_age_days",
        "max_rating_age_days",
        "home_same_day",
        "away_same_day",
        "home_after_match",
        "away_after_match",
        "min_date",
        "max_date"
    ),
    value = c(
        as.character(matches_row_count),
        as.character(nrow(international_modeling_table)),
        as.character(sum(international_modeling_table$data_split == "train")),
        as.character(
            sum(international_modeling_table$data_split_modeling == "validation")
        ),
        as.character(sum(international_modeling_table$data_split == "test")),
        as.character(
            sum(international_modeling_table$data_split_modeling == "train")
        ),
        as.character(length(crosswalk_lookup)),
        as.character(nrow(international_modeling_table_unresolved_elo_teams)),
        as.character(sum(both_ratings_present)),
        as.character(round(100 * mean(both_ratings_present), 2)),
        as.character(sum(is.na(international_modeling_table$home_rating_pre_match))),
        as.character(sum(is.na(international_modeling_table$away_rating_pre_match))),
        as.character(nrow(international_modeling_table_unmatched_ratings)),
        as.character(min(all_rating_ages_days)),
        as.character(stats::median(all_rating_ages_days)),
        as.character(max(all_rating_ages_days)),
        as.character(home_same_day),
        as.character(away_same_day),
        as.character(home_after_match),
        as.character(away_after_match),
        as.character(min(international_modeling_table$date, na.rm = TRUE)),
        as.character(max(international_modeling_table$date, na.rm = TRUE))
    )
)

coverage_by_decade_summary <- coverage_by_decade |>
    tidyr::pivot_longer(
        -decade,
        names_to = "stat",
        values_to = "value"
    ) |>
    dplyr::mutate(
        metric = paste0("decade_", .data$decade, "_", .data$stat),
        value = as.character(.data$value)
    ) |>
    dplyr::select(metric, value)

modeling_summary <- dplyr::bind_rows(
    modeling_summary,
    coverage_by_tournament_type |>
        tidyr::pivot_longer(
            dplyr::everything(),
            names_to = "metric",
            values_to = "value"
        ) |>
        dplyr::mutate(value = as.character(.data$value)),
    coverage_by_decade_summary
)

readr::write_csv(international_modeling_table, OUTPUT_PATH)
readr::write_csv(modeling_summary, SUMMARY_PATH)
readr::write_csv(
    international_modeling_table_unmatched_ratings,
    UNMATCHED_PATH
)
readr::write_csv(
    international_modeling_table_unresolved_elo_teams,
    UNRESOLVED_ELO_PATH
)
readr::write_csv(team_name_crosswalk, CROSSWALK_PATH)

message("Done.")
message("Modeling table rows: ", nrow(international_modeling_table))
message(
    "Matches with both pre-match ratings: ",
    sum(both_ratings_present),
    " (",
    round(100 * mean(both_ratings_present), 2),
    "%)"
)
message("Unmatched rating rows: ", nrow(international_modeling_table_unmatched_ratings))
message("Min rating age (days): ", min(all_rating_ages_days))
message("Same-day ratings (home/away): ", home_same_day, " / ", away_same_day)
message(
    "After-match ratings (home/away): ",
    home_after_match,
    " / ",
    away_after_match
)
message("Wrote: ", OUTPUT_PATH)
message("Wrote: ", SUMMARY_PATH)
message("Wrote: ", UNMATCHED_PATH)
message("Wrote: ", UNRESOLVED_ELO_PATH)
message("Wrote: ", CROSSWALK_PATH)
