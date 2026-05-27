# ============================================================
# 02_helpers.R
# Reusable helper functions for downloads, reading files,
# and tracking data provenance
# ============================================================

safe_download <- function(url, destfile, overwrite = FALSE) {
    dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)

    if (file.exists(destfile) && !overwrite) {
        message("Already exists, skipping: ", destfile)
        return(TRUE)
    }

    message("Downloading: ", url)

    ok <- tryCatch(
        {
            download.file(url, destfile, mode = "wb", quiet = TRUE)
            TRUE
        },
        error = function(e) {
            warning("Download failed: ", url, "\nReason: ", conditionMessage(e))
            FALSE
        }
    )

    return(ok)
}


safe_read_json <- function(path) {
    tryCatch(
        jsonlite::fromJSON(path, flatten = TRUE),
        error = function(e) {
            warning("Could not read JSON: ", path, "\nReason: ", conditionMessage(e))
            NULL
        }
    )
}


safe_read_csv <- function(path) {
    tryCatch(
        readr::read_csv(path, show_col_types = FALSE),
        error = function(e) {
            warning("Could not read CSV: ", path, "\nReason: ", conditionMessage(e))
            NULL
        }
    )
}


create_empty_manifest <- function() {
    tibble::tibble(
        source = character(),
        dataset = character(),
        url = character(),
        local_path = character(),
        downloaded_at = character(),
        notes = character()
    )
}


read_or_create_manifest <- function(meta_dir) {
    manifest_path <- file.path(meta_dir, "source_manifest.csv")

    if (file.exists(manifest_path)) {
        readr::read_csv(
            manifest_path,
            col_types = readr::cols(.default = readr::col_character())
        )
    } else {
        create_empty_manifest()
    }
}


add_manifest_record <- function(source, dataset, url, local_path, notes = NA_character_) {
    tibble::tibble(
        source = source,
        dataset = dataset,
        url = as.character(url),
        local_path = as.character(local_path),
        downloaded_at = as.character(Sys.time()),
        notes = notes
    )
}


write_source_manifest <- function(records, meta_dir) {
    out <- file.path(meta_dir, "source_manifest.csv")

    records_clean <- records |>
        dplyr::distinct(source, dataset, url, local_path, .keep_all = TRUE) |>
        dplyr::arrange(source, dataset, local_path)

    readr::write_csv(records_clean, out)

    message("Wrote manifest: ", out)
    invisible(out)
}


parse_fd_uk_date <- function(x) {
    lubridate::parse_date_time(
        x,
        orders = c("dmy", "dmY", "ymd", "Ymd"),
        quiet = TRUE
    ) |>
        as.Date()
}


safe_integer <- function(x) {
    suppressWarnings(as.integer(x))
}


message("Helper functions loaded.")
