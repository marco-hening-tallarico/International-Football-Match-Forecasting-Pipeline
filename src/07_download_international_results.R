# 07_download_international_results.R
#
# Downloads martj42/international_results raw CSVs (results, goalscorers,
# shootouts) into data/raw/international_results/.
#
# Writes: data/raw/international_results/*.csv

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

source_manifest <- read_or_create_manifest(META_DIR)

international_dir <- file.path(RAW_DIR, "international_results")
dir.create(international_dir, recursive = TRUE, showWarnings = FALSE)

international_base <- "https://raw.githubusercontent.com/martj42/international_results/master"

international_files <- c(
    "results.csv",
    "goalscorers.csv",
    "shootouts.csv"
)

download_one_international_file <- function(filename) {
    url <- glue::glue("{international_base}/{filename}")
    dest <- file.path(international_dir, filename)

    ok <- safe_download(
        url = url,
        destfile = dest,
        overwrite = FALSE
    )

    if (!isTRUE(ok)) {
        return(tibble::tibble())
    }

    add_manifest_record(
        source = "martj42/international_results",
        dataset = tools::file_path_sans_ext(filename),
        url = url,
        local_path = dest,
        notes = "Historical international football data"
    )
}

intl_manifest <- purrr::map_dfr(
    international_files,
    download_one_international_file
)

source_manifest <- dplyr::bind_rows(
    source_manifest,
    intl_manifest
)

write_source_manifest(
    records = source_manifest,
    meta_dir = META_DIR
)

message("Done.")
message("International raw directory: ", international_dir)