# ============================================================
# 04_download_statsbomb_matches.R
# Download StatsBomb match metadata for all available competitions
# ============================================================

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

    if (!isTRUE(ok)) {
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

statsbomb_matches <- purrr::map_dfr(
    statsbomb_match_files,
    function(path) {
        dat <- safe_read_json(path)

        if (is.null(dat)) {
            return(tibble::tibble())
        }

        if (!is.data.frame(dat)) {
            return(tibble::tibble())
        }

        tibble::as_tibble(dat) |>
            janitor::clean_names() |>
            dplyr::mutate(raw_file = path)
    }
)

statsbomb_matches_out <- file.path(
    PROCESSED_DIR,
    "statsbomb_matches.csv"
)

readr::write_csv(
    statsbomb_matches,
    statsbomb_matches_out
)

write_source_manifest(
    records = source_manifest,
    meta_dir = META_DIR
)

message("Done.")
message("StatsBomb match files: ", length(statsbomb_match_files))
message("StatsBomb match rows: ", nrow(statsbomb_matches))
message("Processed matches table: ", statsbomb_matches_out)