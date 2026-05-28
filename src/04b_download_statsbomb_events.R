# ============================================================
# 04b_download_statsbomb_events.R
# Download StatsBomb event JSON files for every match in
# data/processed/statsbomb_matches.csv.
#
# Events are expected to exist for every StatsBomb match.
# Failures are recorded as "failed" in the coverage CSV.
#
# Outputs:
#   data/raw/statsbomb_open/events/{match_id}.json
#   data/validation/statsbomb_events_download_coverage.csv
#   data/metadata/source_manifest.csv  (updated)
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

source_manifest <- read_or_create_manifest(META_DIR)

STATSBOMB_BASE <- "https://raw.githubusercontent.com/statsbomb/open-data/master/data"

# ============================================================
# Resolve match IDs from the processed match table
# ============================================================

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

# ============================================================
# Prepare output directories
# ============================================================

events_dir    <- file.path(RAW_DIR, "statsbomb_open", "events")
coverage_path <- file.path(VALIDATION_DIR, "statsbomb_events_download_coverage.csv")

dir.create(events_dir,    recursive = TRUE, showWarnings = FALSE)
dir.create(VALIDATION_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Download one event file → one coverage-row tibble
# ============================================================

download_one_event <- function(match_id) {
    url        <- glue::glue("{STATSBOMB_BASE}/events/{match_id}.json")
    local_path <- file.path(events_dir, glue::glue("{match_id}.json"))
    pre_exists <- file.exists(local_path)

    ok <- safe_download(url = url, destfile = local_path, overwrite = FALSE)

    # safe_download / download.file can leave a zero-byte stub on HTTP error;
    # treat that the same as a failed download.
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
        TRUE        ~ "failed"
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
            status == "failed" ~ "Download failed or produced an empty file",
            TRUE               ~ NA_character_
        )
    )
}

# ============================================================
# Run downloads
# ============================================================

message("Downloading event files for ", n_matches, " matches...")

coverage <- purrr::map_dfr(match_ids, download_one_event)

# ============================================================
# Update source manifest for all files that now exist on disk
# ============================================================

new_manifest_records <- coverage |>
    dplyr::filter(file_exists) |>
    dplyr::transmute(
        source        = "StatsBomb Open Data",
        dataset       = paste0("events_", match_id),
        url           = expected_url,
        local_path    = local_path,
        downloaded_at = as.character(Sys.time()),
        notes         = "StatsBomb match event log (raw JSON)"
    )

source_manifest <- dplyr::bind_rows(source_manifest, new_manifest_records)
write_source_manifest(records = source_manifest, meta_dir = META_DIR)

# ============================================================
# Write coverage CSV
# ============================================================

readr::write_csv(coverage, coverage_path)

# ============================================================
# Summary
# ============================================================

n_existing   <- sum(coverage$status == "found_existing", na.rm = TRUE)
n_downloaded <- sum(coverage$status == "downloaded",     na.rm = TRUE)
n_failed     <- sum(coverage$status == "failed",         na.rm = TRUE)
n_available  <- sum(coverage$file_exists,               na.rm = TRUE)

message("")
message("StatsBomb events download summary")
message("  Matches expected  : ", n_matches)
message("  Already on disk   : ", n_existing)
message("  Newly downloaded  : ", n_downloaded)
message("  Failed            : ", n_failed)
message("  Total available   : ", n_available)
message("")
message("Coverage CSV written: ", coverage_path)
