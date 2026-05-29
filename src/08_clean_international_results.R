# 08_clean_international_results.R
#
# Cleans martj42/international_results into one match-level table with
# normalized text, derived outcome columns, and stable source_match_id keys.
#
# Reads: data/raw/international_results/results.csv
#
# Writes: data/processed/international_results.csv

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

international_results_path <- file.path(
    RAW_DIR,
    "international_results",
    "results.csv"
)

if (!file.exists(international_results_path)) {
    stop("Missing international results file. Run 07_download_international_results.R first.")
}

international_raw <- readr::read_csv(
    international_results_path,
    show_col_types = FALSE
) |>
    janitor::clean_names()

required <- c(
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

missing_required <- setdiff(required, names(international_raw))

if (length(missing_required) > 0) {
    stop(
        "International results file is missing required columns: ",
        paste(missing_required, collapse = ", ")
    )
}

make_id_part <- function(x) {
    x |>
        as.character() |>
        stringr::str_squish() |>
        stringr::str_to_lower() |>
        stringr::str_replace_all("[^a-z0-9]+", "_") |>
        stringr::str_replace_all("^_|_$", "")
}

international_normalized <- international_raw |>
    dplyr::transmute(
        source = as.character("international_results"),
        raw_file = as.character(international_results_path),
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

bad_identity <- international_normalized |>
    dplyr::filter(
        is.na(date) |
            is.na(home_team) | home_team == "" |
            is.na(away_team) | away_team == ""
    )

if (nrow(bad_identity) > 0) {
    stop(
        "International results has missing required identity values. Bad rows: ",
        nrow(bad_identity)
    )
}

incomplete_results <- international_normalized |>
    dplyr::filter(is.na(home_score) | is.na(away_score))

if (nrow(incomplete_results) > 0) {
    message(
        "Excluding incomplete international fixtures without final scores: ",
        nrow(incomplete_results)
    )
}

international_results <- international_normalized |>
    dplyr::filter(!is.na(home_score), !is.na(away_score)) |>
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
    dplyr::select(
        source,
        raw_file,
        source_match_id,
        date,
        season,
        competition,
        home_team,
        away_team,
        home_score,
        away_score,
        match_result,
        result_class,
        home_win,
        draw,
        away_win,
        goal_difference,
        total_goals,
        neutral,
        tournament,
        city,
        country
    )

bad_required <- international_results |>
    dplyr::filter(is.na(source_match_id) | source_match_id == "")

if (nrow(bad_required) > 0) {
    stop(
        "International results has missing generated source_match_id values. Bad rows: ",
        nrow(bad_required)
    )
}

international_results_out <- file.path(
    PROCESSED_DIR,
    "international_results.csv"
)

readr::write_csv(
    international_results,
    international_results_out
)

saveRDS(
    international_results,
    file.path(PROCESSED_DIR, "international_results.rds")
)

message("Done.")
message("International results cleaned rows: ", nrow(international_results))
message("Output: ", international_results_out)
