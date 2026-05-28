# ============================================================
# 11_build_data_inventory.R
# Source registry and coverage audit for the soccer data pipeline.
#
# Scans data/raw, data/processed, and data/metadata for every CSV
# and RDS file and records shape, date range, team/competition
# counts, file size, and modification time.
#
# Also inventories raw StatsBomb JSON under data/raw/statsbomb_open/
# using file metadata only (no full JSON parse; event files are large).
#
# Outputs:
#   data/metadata/data_inventory.csv
#   data/validation/source_coverage_summary.csv
#
# Design rules:
#   - tryCatch wraps every file read; one unreadable file never
#     stops the whole script.
#   - No hard-coded absolute paths beyond PROJECT_ROOT from setup.
#   - readr::write_csv for all outputs.
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

# ============================================================
# Constants
# ============================================================

DIRS_TO_SCAN <- c(RAW_DIR, PROCESSED_DIR, META_DIR)

INVENTORY_PATH <- file.path(META_DIR, "data_inventory.csv")
COVERAGE_PATH  <- file.path(VALIDATION_DIR, "source_coverage_summary.csv")

# ============================================================
# Helpers: path classification
# ============================================================

guess_source <- function(path) {
    p <- normalizePath(path, mustWork = FALSE)
    bname <- basename(p)

    # Raw-directory sub-folder takes precedence
    if (grepl(.Platform$file.sep %+% "statsbomb_open" %+% .Platform$file.sep, p, fixed = TRUE) ||
        grepl("/statsbomb_open/", p, fixed = TRUE)) {
        return("StatsBomb Open Data")
    }
    if (grepl(.Platform$file.sep %+% "football_data_uk" %+% .Platform$file.sep, p, fixed = TRUE) ||
        grepl("/football_data_uk/", p, fixed = TRUE)) {
        return("football-data.co.uk")
    }
    if (grepl(.Platform$file.sep %+% "international_results" %+% .Platform$file.sep, p, fixed = TRUE) ||
        grepl("/international_results/", p, fixed = TRUE)) {
        return("martj42/international_results")
    }

    # Processed files – infer from filename
    if (grepl("^statsbomb", bname)) return("StatsBomb Open Data")
    if (grepl("^football_data_uk", bname)) return("football-data.co.uk")
    if (grepl("^international_", bname)) return("martj42/international_results")
    if (grepl("intl_", bname)) return("martj42/international_results")

    # Metadata / validation infrastructure
    if (grepl("/metadata/", p, fixed = TRUE) ||
        grepl(.Platform$file.sep %+% "metadata" %+% .Platform$file.sep, p, fixed = TRUE)) {
        return("pipeline metadata")
    }

    NA_character_
}

is_statsbomb_raw_json <- function(path) {
    p <- normalizePath(path, mustWork = FALSE)
    tolower(tools::file_ext(path)) == "json" &&
        (grepl("/statsbomb_open/", p, fixed = TRUE) ||
            grepl(.Platform$file.sep %+% "statsbomb_open" %+% .Platform$file.sep, p, fixed = TRUE))
}

guess_statsbomb_raw_dataset <- function(path) {
    p <- normalizePath(path, mustWork = FALSE)
    bname <- basename(p)

    if (identical(bname, "competitions.json")) {
        return("statsbomb_competitions_raw")
    }

    statsbomb_subdir <- function(subdir) {
        grepl(paste0("/statsbomb_open/", subdir, "/"), p, fixed = TRUE) ||
            grepl(
                .Platform$file.sep %+% "statsbomb_open" %+% .Platform$file.sep %+%
                    subdir %+% .Platform$file.sep,
                p,
                fixed = TRUE
            )
    }

    if (statsbomb_subdir("events")) {
        return("statsbomb_events_raw")
    }
    if (statsbomb_subdir("lineups")) {
        return("statsbomb_lineups_raw")
    }
    if (statsbomb_subdir("three-sixty")) {
        return("statsbomb_360_raw")
    }
    if (statsbomb_subdir("matches")) {
        return("statsbomb_matches_raw")
    }

    tools::file_path_sans_ext(bname)
}

guess_dataset <- function(path) {
    if (is_statsbomb_raw_json(path)) {
        return(guess_statsbomb_raw_dataset(path))
    }

    tools::file_path_sans_ext(basename(path))
}

guess_layer <- function(path) {
    p <- normalizePath(path, mustWork = FALSE)
    if (grepl("/raw/", p, fixed = TRUE) ||
        grepl(.Platform$file.sep %+% "raw" %+% .Platform$file.sep, p, fixed = TRUE)) {
        return("raw")
    }
    if (grepl("/processed/", p, fixed = TRUE) ||
        grepl(.Platform$file.sep %+% "processed" %+% .Platform$file.sep, p, fixed = TRUE)) {
        return("processed")
    }
    if (grepl("/metadata/", p, fixed = TRUE) ||
        grepl(.Platform$file.sep %+% "metadata" %+% .Platform$file.sep, p, fixed = TRUE)) {
        return("metadata")
    }
    NA_character_
}

# Inline string concat helper (avoids needing glue in simple cases)
`%+%` <- paste0

# ============================================================
# Helpers: extract metrics from a data frame
# ============================================================

# use_fd_uk_parser = TRUE  →  call parse_fd_uk_date() from 02_helpers.R,
#                              which handles dd/mm/yy, dd/mm/yyyy, etc.
# use_fd_uk_parser = FALSE →  generic path: try as.Date(), then lubridate.
extract_date_range <- function(dat, use_fd_uk_parser = FALSE) {
    # Column lookup is done on clean names so Date / date both become "date".
    clean_names <- janitor::make_clean_names(names(dat))
    candidates  <- c("date", "downloaded_at", "match_date")
    hit         <- intersect(candidates, clean_names)[1]

    if (is.na(hit)) {
        return(list(date_min = NA_character_, date_max = NA_character_, date_note = NA_character_))
    }

    # Map back to the actual (possibly un-cleaned) column name in dat
    date_col <- names(dat)[clean_names == hit][1]

    result <- tryCatch({
        raw_vals <- dat[[date_col]]

        dates <- if (use_fd_uk_parser) {
            # parse_fd_uk_date() is defined in 02_helpers.R and handles
            # dd/mm/yy, dd/mm/yyyy, yyyy-mm-dd, and yyyy/mm/dd formats.
            parse_fd_uk_date(as.character(raw_vals))
        } else {
            # Generic path: fast ISO attempt, then lubridate multi-format.
            d <- suppressWarnings(as.Date(raw_vals))
            if (sum(!is.na(d)) < max(1L, 0.5 * length(d))) {
                d <- suppressWarnings(
                    lubridate::parse_date_time(
                        as.character(raw_vals),
                        orders = c("dmy", "dmY", "ymd", "Ymd"),
                        quiet  = TRUE
                    ) |> as.Date()
                )
            }
            d
        }

        valid <- dates[!is.na(dates)]

        if (length(valid) == 0L) {
            list(date_min = NA_character_, date_max = NA_character_, date_note = NA_character_)
        } else {
            list(
                date_min  = as.character(min(valid)),
                date_max  = as.character(max(valid)),
                date_note = NA_character_
            )
        }
    }, error = function(e) {
        list(
            date_min  = NA_character_,
            date_max  = NA_character_,
            date_note = paste0("date_parse_warning: ", conditionMessage(e))
        )
    })

    result
}

extract_n_teams <- function(dat) {
    cn <- janitor::make_clean_names(names(dat))
    names(dat) <- cn

    has_home <- "home_team" %in% cn
    has_away <- "away_team" %in% cn

    if (!has_home || !has_away) return(NA_integer_)

    dplyr::n_distinct(c(dat$home_team, dat$away_team), na.rm = TRUE)
}

extract_n_competitions <- function(dat) {
    cn <- janitor::make_clean_names(names(dat))
    names(dat) <- cn

    col <- intersect(c("competition", "competition_name", "tournament"), cn)[1]

    if (is.na(col)) return(NA_integer_)

    dplyr::n_distinct(dat[[col]], na.rm = TRUE)
}

# ============================================================
# StatsBomb raw JSON: metadata only (no parse)
# ============================================================

inspect_statsbomb_json_file <- function(path) {
    finfo <- tryCatch(file.info(path), error = function(e) NULL)

    file_size_bytes <- if (!is.null(finfo)) as.numeric(finfo$size) else NA_real_
    modified_time   <- if (!is.null(finfo)) as.character(finfo$mtime) else NA_character_

    tibble::tibble(
        path             = path,
        source_guess     = "StatsBomb Open Data",
        dataset_guess    = guess_statsbomb_raw_dataset(path),
        raw_or_processed = "raw",
        file_type        = "json",
        rows             = NA_integer_,
        columns          = NA_integer_,
        date_min         = NA_character_,
        date_max         = NA_character_,
        n_teams          = NA_integer_,
        n_competitions   = NA_integer_,
        file_size_bytes  = file_size_bytes,
        modified_time    = modified_time,
        notes            = NA_character_
    )
}

# ============================================================
# Core inspector: one file → one tibble row
# ============================================================

inspect_one_file <- function(path) {
    if (is_statsbomb_raw_json(path)) {
        return(inspect_statsbomb_json_file(path))
    }

    finfo <- tryCatch(file.info(path), error = function(e) NULL)

    file_size_bytes <- if (!is.null(finfo)) as.numeric(finfo$size) else NA_real_
    modified_time   <- if (!is.null(finfo)) as.character(finfo$mtime) else NA_character_
    file_type       <- tolower(tools::file_ext(path))

    source_guess     <- guess_source(path)
    dataset_guess    <- guess_dataset(path)
    raw_or_processed <- guess_layer(path)

    # Defaults
    rows          <- NA_integer_
    columns       <- NA_integer_
    date_min      <- NA_character_
    date_max      <- NA_character_
    n_teams       <- NA_integer_
    n_competitions <- NA_integer_
    notes         <- NA_character_

    result <- tryCatch(
        {
            dat <- if (file_type == "csv") {
                data.table::fread(
                    file         = path,
                    showProgress = FALSE,
                    data.table   = FALSE,
                    fill         = TRUE,
                    na.strings   = c("", "NA", "N/A"),
                    check.names  = TRUE,
                    encoding     = "Latin-1"
                ) |>
                    tibble::as_tibble(.name_repair = "unique")
            } else if (file_type == "rds") {
                obj <- readRDS(path)
                if (!is.data.frame(obj)) {
                    stop("RDS object is not a data frame (class: ",
                         paste(class(obj), collapse = "/"), ")")
                }
                tibble::as_tibble(obj)
            } else {
                stop("Unsupported file type: ", file_type)
            }

            # Route football-data.co.uk raw files through parse_fd_uk_date()
            # so dd/mm/yy legacy dates are interpreted correctly.
            is_fd_uk_raw <- identical(source_guess, "football-data.co.uk") &&
                            identical(raw_or_processed, "raw")

            # Date range: isolated tryCatch so a bad date column never
            # marks the whole file as unreadable.
            dr <- tryCatch(
                extract_date_range(dat, use_fd_uk_parser = is_fd_uk_raw),
                error = function(e) {
                    list(
                        date_min  = NA_character_,
                        date_max  = NA_character_,
                        date_note = paste0("date_parse_warning: ", conditionMessage(e))
                    )
                }
            )

            # Merge any date-parse note into the file-level notes
            file_note <- if (!is.na(dr$date_note)) dr$date_note else NA_character_

            list(
                rows           = nrow(dat),
                columns        = ncol(dat),
                date_min       = dr$date_min,
                date_max       = dr$date_max,
                n_teams        = extract_n_teams(dat),
                n_competitions = extract_n_competitions(dat),
                notes          = file_note
            )
        },
        error = function(e) {
            list(
                rows           = NA_integer_,
                columns        = NA_integer_,
                date_min       = NA_character_,
                date_max       = NA_character_,
                n_teams        = NA_integer_,
                n_competitions = NA_integer_,
                notes          = paste0("Read error: ", conditionMessage(e))
            )
        }
    )

    tibble::tibble(
        path             = path,
        source_guess     = source_guess,
        dataset_guess    = dataset_guess,
        raw_or_processed = raw_or_processed,
        file_type        = file_type,
        rows             = as.integer(result$rows),
        columns          = as.integer(result$columns),
        date_min         = result$date_min,
        date_max         = result$date_max,
        n_teams          = as.integer(result$n_teams),
        n_competitions   = as.integer(result$n_competitions),
        file_size_bytes  = file_size_bytes,
        modified_time    = modified_time,
        notes            = result$notes
    )
}

# ============================================================
# Collect all CSV and RDS files under the three scan directories
# ============================================================

message("Scanning directories for CSV and RDS files...")

all_file_paths <- purrr::map(
    DIRS_TO_SCAN,
    function(d) {
        if (!dir.exists(d)) {
            message("  Directory not found, skipping: ", d)
            return(character(0))
        }
        as.character(
            fs::dir_ls(d, recurse = TRUE, type = "file", fail = FALSE)
        )
    }
) |>
    purrr::list_c() |>
    unique()

# Keep CSV and RDS; StatsBomb raw JSON is inventoried separately
target_paths <- all_file_paths[grepl("\\.(csv|rds)$", all_file_paths, ignore.case = TRUE)]

statsbomb_raw_dir <- file.path(RAW_DIR, "statsbomb_open")
statsbomb_json_paths <- character(0)

if (dir.exists(statsbomb_raw_dir)) {
    statsbomb_all_files <- as.character(
        fs::dir_ls(statsbomb_raw_dir, recurse = TRUE, type = "file", fail = FALSE)
    )
    statsbomb_json_paths <- statsbomb_all_files[
        grepl("\\.json$", statsbomb_all_files, ignore.case = TRUE)
    ]
}

message("  Total CSV/RDS files found: ", length(target_paths))
message("  Total StatsBomb raw JSON files found: ", length(statsbomb_json_paths))

# ============================================================
# Inspect every file with progress messages
# ============================================================

n_files      <- length(target_paths)
log_interval <- max(1L, floor(n_files / 10L))  # message roughly every 10 %

message("Inspecting ", n_files, " files...")

inventory_rows <- vector("list", n_files)

for (i in seq_along(target_paths)) {
    inventory_rows[[i]] <- inspect_one_file(target_paths[[i]])

    if (i %% log_interval == 0L || i == n_files) {
        message(
            sprintf(
                "  [%d / %d] %.0f%% complete",
                i, n_files, 100 * i / n_files
            )
        )
    }
}

n_json_files      <- length(statsbomb_json_paths)
json_inventory_rows <- vector("list", n_json_files)

if (n_json_files > 0L) {
    json_log_interval <- max(1L, floor(n_json_files / 10L))
    message("Inspecting ", n_json_files, " StatsBomb raw JSON files (metadata only)...")

    for (i in seq_along(statsbomb_json_paths)) {
        json_inventory_rows[[i]] <- inspect_statsbomb_json_file(statsbomb_json_paths[[i]])

        if (i %% json_log_interval == 0L || i == n_json_files) {
            message(
                sprintf(
                    "  JSON [%d / %d] %.0f%% complete",
                    i, n_json_files, 100 * i / n_json_files
                )
            )
        }
    }
}

inventory <- dplyr::bind_rows(
    inventory_rows,
    json_inventory_rows
) |>
    dplyr::arrange(raw_or_processed, source_guess, dataset_guess, path)

# ============================================================
# Write data/metadata/data_inventory.csv
# ============================================================

readr::write_csv(inventory, INVENTORY_PATH)
message("Written: ", INVENTORY_PATH)

# ============================================================
# Build source_coverage_summary.csv
# ============================================================

coverage_summary <- inventory |>
    dplyr::group_by(source_guess, raw_or_processed, dataset_guess) |>
    dplyr::summarise(
        files            = dplyr::n(),
        total_rows       = sum(rows, na.rm = TRUE),
        # Convert to Date before min/max so we get chronological order,
        # not lexicographic string order (which breaks on legacy date strings).
        min_date = {
            valid <- suppressWarnings(as.Date(date_min[!is.na(date_min)]))
            valid <- valid[!is.na(valid)]
            if (length(valid) > 0L) as.character(min(valid)) else NA_character_
        },
        max_date = {
            valid <- suppressWarnings(as.Date(date_max[!is.na(date_max)]))
            valid <- valid[!is.na(valid)]
            if (length(valid) > 0L) as.character(max(valid)) else NA_character_
        },
        total_size_bytes = sum(file_size_bytes, na.rm = TRUE),
        files_with_notes = sum(!is.na(notes)),
        .groups          = "drop"
    ) |>
    dplyr::mutate(
        notes = dplyr::case_when(
            files_with_notes > 0L ~
                paste0(files_with_notes, " file(s) have notes; see data_inventory.csv for details"),
            TRUE ~ NA_character_
        )
    ) |>
    dplyr::select(-files_with_notes) |>
    dplyr::arrange(raw_or_processed, source_guess, dataset_guess)

readr::write_csv(coverage_summary, COVERAGE_PATH)
message("Written: ", COVERAGE_PATH)

# ============================================================
# Console summary
# ============================================================

message("")
message("============================================================")
message("Data Inventory Summary")
message("============================================================")
message("data_inventory.csv rows     : ", nrow(inventory))
message("source_coverage_summary.csv rows: ", nrow(coverage_summary))
message("")
message("Paths written:")
message("  1. ", INVENTORY_PATH)
message("  2. ", COVERAGE_PATH)
message("")
message("Unreadable files: ",
        sum(!is.na(inventory$notes)),
        " (see 'notes' column in data_inventory.csv)")
message("")
message("Coverage summary:")
print(
    coverage_summary |>
        dplyr::select(
            source_guess, raw_or_processed, dataset_guess, files,
            total_rows, min_date, max_date,
            total_size_bytes
        ),
    n = 50,
    width = 120
)

statsbomb_raw_datasets <- c(
    "statsbomb_competitions_raw",
    "statsbomb_matches_raw",
    "statsbomb_events_raw",
    "statsbomb_lineups_raw",
    "statsbomb_360_raw"
)

statsbomb_raw_summary <- coverage_summary |>
    dplyr::filter(
        source_guess == "StatsBomb Open Data",
        raw_or_processed == "raw",
        dataset_guess %in% statsbomb_raw_datasets
    ) |>
    dplyr::select(dataset_guess, files, total_size_bytes) |>
    dplyr::arrange(dataset_guess)

message("")
message("StatsBomb Open Data – raw JSON by dataset:")
if (nrow(statsbomb_raw_summary) == 0L) {
    message("  (no StatsBomb raw JSON datasets found)")
} else {
    print(statsbomb_raw_summary, n = 20, width = 120)
}
