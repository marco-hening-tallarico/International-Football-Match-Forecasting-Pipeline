# 04g_clean_statsbomb_360.R
#
# Flattens StatsBomb 360 freeze-frame JSON where raw files exist. Skips
# matches without 360 data (expected for most of the corpus).
#
# Reads:
# - data/raw/statsbomb_open/three-sixty/{match_id}.json
# - data/processed/statsbomb_matches.csv (lineage)
#
# Writes:
# - data/processed/statsbomb_360.csv
# - data/validation/statsbomb_360_cleaning_summary.csv
# - data/validation/statsbomb_360_schema_audit.csv

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

STATSBOMB_SOURCE <- "StatsBomb Open Data"
THREESIXTY_RAW_DIR <- file.path(RAW_DIR, "statsbomb_open", "three-sixty")
MATCHES_PATH <- file.path(PROCESSED_DIR, "statsbomb_matches.csv")

THREESIXTY_OUT <- file.path(PROCESSED_DIR, "statsbomb_360.csv")
SUMMARY_OUT <- file.path(VALIDATION_DIR, "statsbomb_360_cleaning_summary.csv")
AUDIT_OUT <- file.path(VALIDATION_DIR, "statsbomb_360_schema_audit.csv")

CHUNK_SIZE <- 50L
PROGRESS_EVERY <- 100L

THREESIXTY_COLUMNS <- c(
    "source",
    "raw_file",
    "match_id",
    "event_id",
    "visible_area_json",
    "freeze_frame_index",
    "player_id",
    "player_name",
    "teammate",
    "actor",
    "keeper",
    "location_x",
    "location_y"
)

# Safe column accessors

get_chr <- function(df, col, n = nrow(df)) {
    if (col %in% names(df)) {
        as.character(df[[col]])
    } else {
        rep(NA_character_, n)
    }
}

get_lgl <- function(df, col, n = nrow(df)) {
    if (col %in% names(df)) {
        as.logical(df[[col]])
    } else {
        rep(NA, n)
    }
}

coalesce_chr <- function(df, cols, n = nrow(df)) {
    out <- rep(NA_character_, n)

    for (col in cols) {
        if (!col %in% names(df)) {
            next
        }

        values <- as.character(df[[col]])
        replace_idx <- is.na(out) & !is.na(values) & values != ""
        out[replace_idx] <- values[replace_idx]
    }

    out
}

extract_list_coord <- function(column, index) {
    vapply(
        column,
        function(value) {
            if (is.null(value)) {
                return(NA_real_)
            }

            if (is.list(value) && length(value) >= index) {
                return(suppressWarnings(as.numeric(value[[index]])))
            }

            if (is.atomic(value) && length(value) >= index) {
                return(suppressWarnings(as.numeric(value[index])))
            }

            NA_real_
        },
        FUN.VALUE = numeric(1)
    )
}

serialize_visible_area <- function(value) {
    if (is.null(value)) {
        return(NA_character_)
    }

    if (length(value) == 0L) {
        return("[]")
    }

    as.character(
        jsonlite::toJSON(value, auto_unbox = TRUE, na = "null")
    )
}

extract_match_id_from_path <- function(path) {
    tools::file_path_sans_ext(basename(path))
}

flatten_freeze_frame <- function(frame) {
    if (is.null(frame)) {
        return(NULL)
    }

    if (is.data.frame(frame)) {
        if (nrow(frame) == 0L) {
            return(NULL)
        }

        return(tibble::as_tibble(frame) |>
            janitor::clean_names())
    }

    frame_json <- jsonlite::toJSON(frame, auto_unbox = TRUE, na = "null")
    flattened <- jsonlite::fromJSON(frame_json, flatten = TRUE)

    if (!is.data.frame(flattened) || nrow(flattened) == 0L) {
        return(NULL)
    }

    tibble::as_tibble(flattened) |>
        janitor::clean_names()
}

expand_events_to_freeze_rows <- function(
    events_table,
    match_id,
    raw_file
) {
    event_id_column <- if ("event_uuid" %in% names(events_table)) {
        "event_uuid"
    } else if ("event_id" %in% names(events_table)) {
        "event_id"
    } else {
        return(NULL)
    }

    if (!"freeze_frame" %in% names(events_table)) {
        return(NULL)
    }

    visible_area_column <- if ("visible_area" %in% names(events_table)) {
        events_table$visible_area
    } else {
        vector("list", nrow(events_table))
    }

    events_prepared <- events_table |>
        dplyr::mutate(
            source = STATSBOMB_SOURCE,
            raw_file = raw_file,
            match_id = as.character(match_id),
            event_id = .data[[event_id_column]],
            visible_area_json = vapply(
                visible_area_column,
                serialize_visible_area,
                FUN.VALUE = character(1)
            ),
            freeze_frame = lapply(events_table$freeze_frame, flatten_freeze_frame)
        ) |>
        dplyr::filter(!vapply(freeze_frame, is.null, FUN.VALUE = logical(1)))

    if (nrow(events_prepared) == 0L) {
        return(NULL)
    }

    expanded <- events_prepared |>
        tidyr::unnest(freeze_frame, keep_empty = FALSE) |>
        dplyr::group_by(event_id) |>
        dplyr::mutate(freeze_frame_index = dplyr::row_number()) |>
        dplyr::ungroup()

    n <- nrow(expanded)

    location_column <- if ("location" %in% names(expanded)) {
        expanded$location
    } else {
        vector("list", n)
    }

    tibble::tibble(
        source = expanded$source,
        raw_file = expanded$raw_file,
        match_id = expanded$match_id,
        event_id = as.character(expanded$event_id),
        visible_area_json = expanded$visible_area_json,
        freeze_frame_index = as.integer(expanded$freeze_frame_index),
        player_id = coalesce_chr(expanded, c("player_id"), n),
        player_name = coalesce_chr(expanded, c("player_name"), n),
        teammate = get_lgl(expanded, "teammate", n),
        actor = get_lgl(expanded, "actor", n),
        keeper = get_lgl(expanded, "keeper", n),
        location_x = extract_list_coord(location_column, 1L),
        location_y = extract_list_coord(location_column, 2L)
    )
}

# Transform one raw 360 file

transform_statsbomb_360_file <- function(path) {
    raw_file <- normalizePath(path, winslash = "/", mustWork = FALSE)
    match_id <- extract_match_id_from_path(path)

    audit_row <- tibble::tibble(
        raw_file = raw_file,
        columns_found = NA_character_,
        rows = 0L,
        status = "failed",
        notes = NA_character_
    )

    parsed <- tryCatch(
        {
            # Top-level arrays contain nested freeze_frame lists; flatten=FALSE
            # preserves structure. Each freeze_frame block is flattened with
            # fromJSON(..., flatten = TRUE) below.
            jsonlite::fromJSON(path, flatten = FALSE)
        },
        error = function(error) {
            audit_row$notes <<- conditionMessage(error)
            NULL
        }
    )

    if (is.null(parsed)) {
        return(list(
            threesixty = NULL,
            audit = audit_row
        ))
    }

    if (is.list(parsed) && !is.data.frame(parsed)) {
        parsed <- tryCatch(
            dplyr::bind_rows(parsed),
            error = function(error) {
                audit_row$notes <<- paste(
                    "Could not bind parsed list to events table:",
                    conditionMessage(error)
                )
                NULL
            }
        )
    }

    if (is.null(parsed)) {
        return(list(
            threesixty = NULL,
            audit = audit_row
        ))
    }

    if (!is.data.frame(parsed)) {
        audit_row$notes <- paste(
            "Unsupported parsed JSON type:",
            paste(class(parsed), collapse = "/")
        )
        return(list(
            threesixty = NULL,
            audit = audit_row
        ))
    }

    events_table <- tibble::as_tibble(parsed) |>
        janitor::clean_names()

    columns_found <- sort(names(events_table))
    audit_row$columns_found <- paste(columns_found, collapse = ";")

    threesixty_rows <- expand_events_to_freeze_rows(
        events_table = events_table,
        match_id = match_id,
        raw_file = raw_file
    )

    if (is.null(threesixty_rows)) {
        if (!"freeze_frame" %in% names(events_table)) {
            audit_row$notes <- "Parsed events table has no freeze_frame column"
        } else if (!any(c("event_uuid", "event_id") %in% names(events_table))) {
            audit_row$notes <- "Parsed events table has no event_uuid or event_id column"
        } else {
            audit_row$notes <- "No freeze-frame player rows in file"
        }

        audit_row$status <- "success_empty"
        audit_row$rows <- 0L
        return(list(
            threesixty = NULL,
            audit = audit_row
        ))
    }

    audit_row$rows <- nrow(threesixty_rows)
    audit_row$status <- "success"

    list(
        threesixty = threesixty_rows,
        audit = audit_row
    )
}

# Discover raw files

dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(VALIDATION_DIR, recursive = TRUE, showWarnings = FALSE)

raw_files_found <- 0L
threesixty_files <- character()

if (dir.exists(THREESIXTY_RAW_DIR)) {
    threesixty_files <- sort(
        fs::dir_ls(THREESIXTY_RAW_DIR, glob = "*.json", type = "file")
    )
    raw_files_found <- length(threesixty_files)
}

message("StatsBomb 360 JSON files found: ", raw_files_found)

if (raw_files_found == 0L) {
    message("No raw 360 JSON files to process; writing empty outputs.")
}

if (file.exists(MATCHES_PATH)) {
    message("Lineage reference: ", MATCHES_PATH)
} else {
    message(
        "Note: ",
        MATCHES_PATH,
        " not found (match_id will be taken from filenames)."
    )
}

statsbomb_360 <- tibble::tibble()
schema_audit <- tibble::tibble(
    raw_file = character(),
    columns_found = character(),
    rows = integer(),
    status = character(),
    notes = character()
)

files_read_successfully <- 0L
files_failed <- 0L

if (raw_files_found > 0L) {
    message("Processing in chunks of ", CHUNK_SIZE, "...")

    file_chunks <- split(
        threesixty_files,
        ceiling(seq_along(threesixty_files) / CHUNK_SIZE)
    )

    threesixty_chunks <- vector("list", length(file_chunks))
    audit_chunks <- vector("list", length(file_chunks))

    for (chunk_index in seq_along(file_chunks)) {
        chunk_paths <- file_chunks[[chunk_index]]
        chunk_results <- lapply(chunk_paths, transform_statsbomb_360_file)

        chunk_threesixty <- purrr::map(chunk_results, "threesixty")
        chunk_audit <- purrr::map_dfr(chunk_results, "audit")

        threesixty_chunks[[chunk_index]] <- dplyr::bind_rows(chunk_threesixty)
        audit_chunks[[chunk_index]] <- chunk_audit

        files_read_successfully <- files_read_successfully +
            sum(chunk_audit$status %in% c("success", "success_empty"), na.rm = TRUE)
        files_failed <- files_failed +
            sum(chunk_audit$status == "failed", na.rm = TRUE)

        files_processed <- min(chunk_index * CHUNK_SIZE, raw_files_found)

        if (files_processed %% PROGRESS_EVERY == 0L ||
            chunk_index == length(file_chunks)) {
            message(
                "  Processed ",
                files_processed,
                " / ",
                raw_files_found,
                " files"
            )
        }
    }

    statsbomb_360 <- dplyr::bind_rows(threesixty_chunks)
    schema_audit <- dplyr::bind_rows(audit_chunks)
}

if (nrow(statsbomb_360) > 0L) {
    statsbomb_360 <- statsbomb_360 |>
        dplyr::select(dplyr::all_of(THREESIXTY_COLUMNS))

    missing_match_id <- is.na(statsbomb_360$match_id) |
        statsbomb_360$match_id == ""

    if (any(missing_match_id)) {
        stop(
            "Processed 360 rows contain missing match_id values: ",
            sum(missing_match_id),
            call. = FALSE
        )
    }

    missing_event_id <- is.na(statsbomb_360$event_id) |
        statsbomb_360$event_id == ""

    if (any(missing_event_id)) {
        stop(
            "Processed 360 rows contain missing event_id values: ",
            sum(missing_event_id),
            call. = FALSE
        )
    }
} else {
    statsbomb_360 <- tibble::tibble(
        source = character(),
        raw_file = character(),
        match_id = character(),
        event_id = character(),
        visible_area_json = character(),
        freeze_frame_index = integer(),
        player_id = character(),
        player_name = character(),
        teammate = logical(),
        actor = logical(),
        keeper = logical(),
        location_x = double(),
        location_y = double()
    )

    missing_match_id <- logical()
    missing_event_id <- logical()
}

summary_notes <- paste(
    sprintf("missing player_id rows: %d", sum(is.na(statsbomb_360$player_id))),
    sprintf("missing player_name rows: %d", sum(is.na(statsbomb_360$player_name))),
    "360 coverage is partial by design",
    sep = "; "
)

cleaning_summary <- tibble::tibble(
    metric = c(
        "raw_files_found",
        "files_read_successfully",
        "files_failed",
        "output_rows",
        "distinct_matches",
        "distinct_events",
        "missing_match_id_rows",
        "missing_event_id_rows",
        "notes"
    ),
    value = c(
        raw_files_found,
        files_read_successfully,
        files_failed,
        nrow(statsbomb_360),
        if (nrow(statsbomb_360) > 0L) {
            dplyr::n_distinct(statsbomb_360$match_id)
        } else {
            0L
        },
        if (nrow(statsbomb_360) > 0L) {
            dplyr::n_distinct(statsbomb_360$event_id)
        } else {
            0L
        },
        sum(missing_match_id),
        sum(missing_event_id),
        summary_notes
    )
)

readr::write_csv(statsbomb_360, THREESIXTY_OUT)
readr::write_csv(cleaning_summary, SUMMARY_OUT)
readr::write_csv(schema_audit, AUDIT_OUT)

message("Done.")
message("StatsBomb 360 rows: ", nrow(statsbomb_360))
message("Files failed: ", files_failed)
message("360 output: ", THREESIXTY_OUT)
message("Cleaning summary: ", SUMMARY_OUT)
message("Schema audit: ", AUDIT_OUT)

message("")
message("Cleaning summary:")
print(cleaning_summary, n = Inf)

if (nrow(statsbomb_360) > 0L) {
    message("")
    message("Example rows (first 10):")
    print(utils::head(statsbomb_360, 10L), n = 10L, width = 120)
}
