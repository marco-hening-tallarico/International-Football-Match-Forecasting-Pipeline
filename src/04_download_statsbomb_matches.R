# 04_download_statsbomb_matches.R
#
# Downloads StatsBomb match metadata for each competition-season and builds
# the processed match table used by event, lineup, and 360 download scripts.
#
# Reads: data/processed/statsbomb_competitions.csv
#
# Writes:
# - data/processed/statsbomb_matches.csv
# - data/processed/statsbomb_matches.rds
# - data/metadata/source_manifest.csv (updated)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

source_manifest <- read_or_create_manifest(META_DIR)

STATSBOMB_BASE <- "https://raw.githubusercontent.com/statsbomb/open-data/master/data"

statsbomb_competitions_path <- file.path(
    PROCESSED_DIR,
    "statsbomb_competitions.csv"
)

if (!file.exists(statsbomb_competitions_path)) {
    stop("Missing statsbomb_competitions.csv. Run 03_download_statsbomb_competitions.R first.")
}

statsbomb_competitions <- readr::read_csv(
    statsbomb_competitions_path,
    col_types = processed_csv_col_types(statsbomb_competitions_path),
    show_col_types = FALSE
)

statsbomb_dir <- file.path(RAW_DIR, "statsbomb_open")
statsbomb_matches_dir <- file.path(statsbomb_dir, "matches")
dir.create(statsbomb_matches_dir, recursive = TRUE, showWarnings = FALSE)

download_statsbomb_matches <- function(competition_id, season_id) {
    url <- glue::glue("{STATSBOMB_BASE}/matches/{competition_id}/{season_id}.json")

    dest <- file.path(
        statsbomb_matches_dir,
        glue::glue("{competition_id}_{season_id}.json")
    )

    ok <- safe_download(
        url = url,
        destfile = dest,
        overwrite = FALSE
    )

    if (!isTRUE(ok) || !file.exists(dest)) {
        return(tibble::tibble())
    }

    add_manifest_record(
        source = "StatsBomb Open Data",
        dataset = glue::glue("matches_{competition_id}_{season_id}"),
        url = url,
        local_path = dest,
        notes = "StatsBomb match metadata"
    )
}

match_manifest <- statsbomb_competitions |>
    dplyr::distinct(competition_id, season_id) |>
    purrr::pmap_dfr(function(competition_id, season_id) {
        download_statsbomb_matches(competition_id, season_id)
    })

source_manifest <- dplyr::bind_rows(
    source_manifest,
    match_manifest
)

statsbomb_match_files <- fs::dir_ls(
    statsbomb_matches_dir,
    glob = "*.json",
    fail = FALSE
)

if (length(statsbomb_match_files) == 0) {
    stop("No StatsBomb match JSON files found. Run this script after StatsBomb competitions are available.")
}

get_chr <- function(df, col) {
    if (col %in% names(df)) {
        as.character(df[[col]])
    } else {
        rep(NA_character_, nrow(df))
    }
}

get_int <- function(df, col) {
    if (col %in% names(df)) {
        suppressWarnings(as.integer(df[[col]]))
    } else {
        rep(NA_integer_, nrow(df))
    }
}

read_one_statsbomb_match_file <- function(path) {
    dat <- safe_read_json(path)

    if (is.null(dat)) {
        return(tibble::tibble())
    }

    if (!is.data.frame(dat)) {
        stop("StatsBomb match JSON did not parse as a table: ", path)
    }

    if (nrow(dat) == 0) {
        return(tibble::tibble())
    }

    dat_clean <- tibble::as_tibble(dat) |>
        janitor::clean_names()

    required <- c(
        "match_id",
        "match_date",
        "home_team_home_team_name",
        "away_team_away_team_name",
        "home_score",
        "away_score"
    )

    missing_required <- setdiff(required, names(dat_clean))

    if (length(missing_required) > 0) {
        stop(
            "StatsBomb match file is missing required columns: ",
            paste(missing_required, collapse = ", "),
            "\nFile: ",
            path
        )
    }

    out <- tibble::tibble(
        source = as.character("StatsBomb Open Data"),
        raw_file = as.character(path),
        source_match_id = as.character(get_chr(dat_clean, "match_id")),

        date = suppressWarnings(as.Date(get_chr(dat_clean, "match_date"))),
        season = get_chr(dat_clean, "season_season_name"),
        competition = get_chr(dat_clean, "competition_competition_name"),

        home_team = stringr::str_squish(get_chr(dat_clean, "home_team_home_team_name")),
        away_team = stringr::str_squish(get_chr(dat_clean, "away_team_away_team_name")),

        home_score = get_int(dat_clean, "home_score"),
        away_score = get_int(dat_clean, "away_score"),

        competition_id = get_int(dat_clean, "competition_competition_id"),
        season_id = get_int(dat_clean, "season_season_id"),
        match_week = get_int(dat_clean, "match_week"),
        competition_country = get_chr(dat_clean, "competition_country_name"),
        stadium = get_chr(dat_clean, "stadium_name"),
        data_version = get_chr(dat_clean, "metadata_data_version"),
        shot_fidelity_version = get_chr(dat_clean, "metadata_shot_fidelity_version"),
        xy_fidelity_version = get_chr(dat_clean, "metadata_xy_fidelity_version")
    ) |>
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
            total_goals = home_score + away_score,
            neutral = as.logical(NA)
        )

    bad_required <- out |>
        dplyr::filter(
            is.na(source_match_id) | source_match_id == "" |
                is.na(date) |
                is.na(home_team) | home_team == "" |
                is.na(away_team) | away_team == "" |
                is.na(home_score) |
            is.na(away_score)
        )

    if (nrow(bad_required) > 0) {
        stop("StatsBomb match file has missing required identity/result values: ", path)
    }

    out
}

statsbomb_matches <- purrr::map_dfr(
    statsbomb_match_files,
    read_one_statsbomb_match_file
)

statsbomb_matches_out <- file.path(
    PROCESSED_DIR,
    "statsbomb_matches.csv"
)

readr::write_csv(
    statsbomb_matches,
    statsbomb_matches_out
)

saveRDS(
    statsbomb_matches,
    file.path(PROCESSED_DIR, "statsbomb_matches.rds")
)

write_source_manifest(
    records = source_manifest,
    meta_dir = META_DIR
)

message("Done.")
message("StatsBomb match files: ", length(statsbomb_match_files))
message("StatsBomb match rows: ", nrow(statsbomb_matches))
message("Processed matches table: ", statsbomb_matches_out)
