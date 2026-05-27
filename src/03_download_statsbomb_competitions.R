# ============================================================
# 03_download_statsbomb_competitions.R
# Download and clean StatsBomb Open Data competitions
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

source_manifest <- read_or_create_manifest(META_DIR)

STATSBOMB_BASE <- "https://raw.githubusercontent.com/statsbomb/open-data/master/data"

statsbomb_dir <- file.path(RAW_DIR, "statsbomb_open")
dir.create(statsbomb_dir, recursive = TRUE, showWarnings = FALSE)

statsbomb_comp_url <- glue::glue("{STATSBOMB_BASE}/competitions.json")
statsbomb_comp_path <- file.path(statsbomb_dir, "competitions.json")

download_ok <- safe_download(
    url = statsbomb_comp_url,
    destfile = statsbomb_comp_path,
    overwrite = FALSE
)

if (!isTRUE(download_ok)) {
    stop("StatsBomb competitions download failed.")
}

source_manifest <- dplyr::bind_rows(
    source_manifest,
    add_manifest_record(
        source = "StatsBomb Open Data",
        dataset = "competitions",
        url = statsbomb_comp_url,
        local_path = statsbomb_comp_path,
        notes = "StatsBomb competition-season catalogue"
    )
)

statsbomb_competitions_raw <- safe_read_json(statsbomb_comp_path)

if (is.null(statsbomb_competitions_raw)) {
    stop("Could not read StatsBomb competitions JSON.")
}

if (!is.data.frame(statsbomb_competitions_raw)) {
    stop("StatsBomb competitions JSON did not parse as a table.")
}

required_cols <- c(
    "competition_id",
    "season_id",
    "country_name",
    "competition_name",
    "season_name"
)

missing_cols <- setdiff(required_cols, names(statsbomb_competitions_raw))

if (length(missing_cols) > 0) {
    stop(
        "StatsBomb competitions file is missing required columns: ",
        paste(missing_cols, collapse = ", ")
    )
}

statsbomb_competitions <- statsbomb_competitions_raw |>
    tibble::as_tibble() |>
    janitor::clean_names()

statsbomb_competitions_out <- file.path(
    PROCESSED_DIR,
    "statsbomb_competitions.csv"
)

readr::write_csv(
    statsbomb_competitions,
    statsbomb_competitions_out
)

write_source_manifest(
    records = source_manifest,
    meta_dir = META_DIR
)

message("Done.")
message("Raw competitions file: ", statsbomb_comp_path)
message("Processed competitions table: ", statsbomb_competitions_out)
message("Rows: ", nrow(statsbomb_competitions))
message("Unique competition names: ", dplyr::n_distinct(statsbomb_competitions$competition_name))