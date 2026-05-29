# 04d_download_statsbomb_360.R
#
# Downloads StatsBomb 360 freeze-frame JSON for every match in the processed
# match table. Many matches have no 360 file; those are logged as unavailable,
# not as errors.
#
# Reads: data/processed/statsbomb_matches.csv
#
# Writes:
# - data/raw/statsbomb_open/three-sixty/{match_id}.json (when available)
# - data/validation/statsbomb_360_download_coverage.csv
# - data/metadata/source_manifest.csv (updated for files on disk)
#
# Notes:
# - 360 coverage is limited to selected tournaments (e.g. EURO 2020, WC 2022).

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

source_manifest <- read_or_create_manifest(META_DIR)

STATSBOMB_BASE <- "https://raw.githubusercontent.com/statsbomb/open-data/master/data"

# Resolve match IDs from the processed match table

matches_path <- file.path(PROCESSED_DIR, "statsbomb_matches.csv")

if (!file.exists(matches_path)) {
    stop(
        "Missing statsbomb_matches.csv – ",
        "run src/04_download_statsbomb_matches.R first.",
        call. = FALSE
    )
}

statsbomb_matches <- readr::read_csv(
    matches_path,
    col_types = processed_csv_col_types(matches_path),
    show_col_types = FALSE
)

match_ids <- sort(unique(statsbomb_matches$source_match_id))
n_matches  <- length(match_ids)

message("StatsBomb matches found: ", n_matches)
message(
    "Note: 360 files are only available for selected matches. ",
    "Missing files will be recorded as 'unavailable'."
)

# Prepare output directories

# The StatsBomb repo uses a hyphen in the directory name.
threesixty_dir <- file.path(RAW_DIR, "statsbomb_open", "three-sixty")
coverage_path  <- file.path(VALIDATION_DIR, "statsbomb_360_download_coverage.csv")

dir.create(threesixty_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(VALIDATION_DIR,  recursive = TRUE, showWarnings = FALSE)

# Download one 360 file → one coverage-row tibble
#
# Failures are EXPECTED (only a subset of matches have 360 data),
# so we suppress the download warning and record status as
# "unavailable" rather than "failed".

download_one_360 <- function(match_id) {
    url        <- glue::glue("{STATSBOMB_BASE}/three-sixty/{match_id}.json")
    local_path <- file.path(threesixty_dir, glue::glue("{match_id}.json"))
    pre_exists <- file.exists(local_path)

    # suppressWarnings: 404 failures are expected for most matches.
    ok <- suppressWarnings(
        safe_download(url = url, destfile = local_path, overwrite = FALSE)
    )

    # Remove zero-byte stubs that download.file may leave after an HTTP error.
    if (!pre_exists && file.exists(local_path)) {
        sz <- file.info(local_path)$size
        if (!is.na(sz) && sz == 0L) {
            file.remove(local_path)
            ok <- FALSE
        }
    }

    post_exists <- file.exists(local_path)
    file_size   <- if (post_exists) as.numeric(file.info(local_path)$size) else NA_real_

    status <- dplyr::case_when(
        pre_exists  ~ "found_existing",
        isTRUE(ok) ~ "downloaded",
        TRUE        ~ "unavailable"   # expected for most matches
    )

    tibble::tibble(
        match_id        = as.character(match_id),
        expected_url    = as.character(url),
        local_path      = as.character(local_path),
        attempted       = TRUE,
        downloaded      = !pre_exists && isTRUE(ok),
        file_exists     = post_exists,
        file_size_bytes = file_size,
        status          = status,
        notes = dplyr::case_when(
            status == "unavailable" ~
                "Three-sixty data not available for this match",
            TRUE ~ NA_character_
        )
    )
}

# Run downloads

message("Attempting 360 downloads for ", n_matches, " matches...")

coverage <- purrr::map_dfr(match_ids, download_one_360)

# Update source manifest – only for files that actually exist

new_manifest_records <- coverage |>
    dplyr::filter(file_exists) |>
    dplyr::transmute(
        source        = "StatsBomb Open Data",
        dataset       = paste0("three_sixty_", match_id),
        url           = expected_url,
        local_path    = local_path,
        downloaded_at = as.character(Sys.time()),
        notes         = "StatsBomb 360 freeze-frame data (raw JSON)"
    )

source_manifest <- dplyr::bind_rows(source_manifest, new_manifest_records)
write_source_manifest(records = source_manifest, meta_dir = META_DIR)

# Write coverage CSV

readr::write_csv(coverage, coverage_path)

# Summary

n_existing    <- sum(coverage$status == "found_existing", na.rm = TRUE)
n_downloaded  <- sum(coverage$status == "downloaded",     na.rm = TRUE)
n_unavailable <- sum(coverage$status == "unavailable",    na.rm = TRUE)
n_available   <- sum(coverage$file_exists,               na.rm = TRUE)

message("")
message("StatsBomb 360 download summary")
message("  Matches attempted : ", n_matches)
message("  Already on disk   : ", n_existing)
message("  Newly downloaded  : ", n_downloaded)
message("  Unavailable       : ", n_unavailable, " (expected – 360 is only for selected matches)")
message("  Total available   : ", n_available)
message("")
message("Coverage CSV written: ", coverage_path)
