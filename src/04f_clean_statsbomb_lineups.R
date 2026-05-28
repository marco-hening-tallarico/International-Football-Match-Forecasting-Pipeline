# ============================================================
# 04f_clean_statsbomb_lineups.R
# Flatten StatsBomb raw lineup JSON into processed player-lineup rows
#
# Inputs:
#   data/raw/statsbomb_open/lineups/{match_id}.json
#   data/processed/statsbomb_matches.csv  (lineage reference)
#
# Outputs:
#   data/processed/statsbomb_lineups.csv
#   data/validation/statsbomb_lineups_cleaning_summary.csv
#   data/validation/statsbomb_lineups_schema_audit.csv
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

STATSBOMB_SOURCE <- "StatsBomb Open Data"
LINEUPS_RAW_DIR <- file.path(RAW_DIR, "statsbomb_open", "lineups")
MATCHES_PATH <- file.path(PROCESSED_DIR, "statsbomb_matches.csv")

LINEUPS_OUT <- file.path(PROCESSED_DIR, "statsbomb_lineups.csv")
SUMMARY_OUT <- file.path(VALIDATION_DIR, "statsbomb_lineups_cleaning_summary.csv")
AUDIT_OUT <- file.path(VALIDATION_DIR, "statsbomb_lineups_schema_audit.csv")

CHUNK_SIZE <- 50L
PROGRESS_EVERY <- 100L

LINEUP_COLUMNS <- c(
    "source",
    "raw_file",
    "match_id",
    "team_id",
    "team_name",
    "player_id",
    "player_name",
    "player_nickname",
    "jersey_number",
    "country_id",
    "country_name",
    "positions_json",
    "cards_json"
)

# ============================================================
# Safe column accessors
# ============================================================

get_chr <- function(df, col, n = nrow(df)) {
    if (col %in% names(df)) {
        as.character(df[[col]])
    } else {
        rep(NA_character_, n)
    }
}

get_int <- function(df, col, n = nrow(df)) {
    if (col %in% names(df)) {
        suppressWarnings(as.integer(df[[col]]))
    } else {
        rep(NA_integer_, n)
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

serialize_nested_to_json <- function(value) {
    if (is.null(value)) {
        return(NA_character_)
    }

    if (is.data.frame(value)) {
        if (nrow(value) == 0L) {
            return("[]")
        }

        return(
            jsonlite::toJSON(
                value,
                auto_unbox = TRUE,
                na = "null",
                dataframe = "rows"
            )
        )
    }

    if (is.list(value)) {
        if (length(value) == 0L) {
            return("[]")
        }

        return(jsonlite::toJSON(value, auto_unbox = TRUE, na = "null"))
    }

    NA_character_
}

serialize_list_column <- function(column) {
    if (is.null(column)) {
        return(character())
    }

    vapply(
        column,
        serialize_nested_to_json,
        FUN.VALUE = character(1)
    )
}

extract_match_id_from_path <- function(path) {
    tools::file_path_sans_ext(basename(path))
}

flatten_team_lineup <- function(lineup_object) {
    if (is.null(lineup_object)) {
        return(NULL)
    }

    if (is.data.frame(lineup_object) && nrow(lineup_object) == 0L) {
        return(NULL)
    }

    lineup_json <- jsonlite::toJSON(lineup_object, auto_unbox = TRUE, na = "null")
    flattened <- jsonlite::fromJSON(lineup_json, flatten = TRUE)

    if (!is.data.frame(flattened) || nrow(flattened) == 0L) {
        return(NULL)
    }

    tibble::as_tibble(flattened) |>
        janitor::clean_names()
}

build_player_rows <- function(
    players_raw,
    match_id,
    team_id,
    team_name,
    raw_file
) {
    n <- nrow(players_raw)

    positions_column <- if ("positions" %in% names(players_raw)) {
        players_raw$positions
    } else {
        vector("list", n)
    }

    cards_column <- if ("cards" %in% names(players_raw)) {
        players_raw$cards
    } else {
        vector("list", n)
    }

    tibble::tibble(
        source = STATSBOMB_SOURCE,
        raw_file = raw_file,
        match_id = as.character(match_id),
        team_id = as.character(team_id),
        team_name = as.character(team_name),
        player_id = coalesce_chr(players_raw, c("player_id"), n),
        player_name = coalesce_chr(players_raw, c("player_name"), n),
        player_nickname = coalesce_chr(players_raw, c("player_nickname"), n),
        jersey_number = get_int(players_raw, "jersey_number", n),
        country_id = coalesce_chr(
            players_raw,
            c("country_id", "country.id"),
            n
        ),
        country_name = coalesce_chr(
            players_raw,
            c("country_name", "country.name"),
            n
        ),
        positions_json = serialize_list_column(positions_column),
        cards_json = serialize_list_column(cards_column)
    )
}

# ============================================================
# Transform one raw lineup file
# ============================================================

transform_statsbomb_lineups_file <- function(path) {
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
        jsonlite::fromJSON(path, flatten = FALSE),
        error = function(error) {
            audit_row$notes <<- conditionMessage(error)
            NULL
        }
    )

    if (is.null(parsed)) {
        return(list(
            lineups = NULL,
            audit = audit_row
        ))
    }

    columns_found <- character()

    if (is.data.frame(parsed)) {
        parsed <- tibble::as_tibble(parsed) |>
            janitor::clean_names()
        columns_found <- sort(names(parsed))

        if (all(c("player_id", "team_id") %in% names(parsed))) {
            lineup_rows <- build_player_rows(
                players_raw = parsed,
                match_id = coalesce_chr(parsed, c("match_id"), nrow(parsed)),
                team_id = coalesce_chr(parsed, c("team_id"), nrow(parsed)),
                team_name = coalesce_chr(parsed, c("team_name"), nrow(parsed)),
                raw_file = raw_file
            )

            missing_match <- is.na(lineup_rows$match_id) | lineup_rows$match_id == ""
            lineup_rows$match_id[missing_match] <- as.character(match_id)

            audit_row$columns_found <- paste(columns_found, collapse = ";")
            audit_row$rows <- nrow(lineup_rows)
            audit_row$status <- if (nrow(lineup_rows) > 0L) "success" else "success_empty"

            return(list(
                lineups = lineup_rows,
                audit = audit_row
            ))
        }

        if (!"lineup" %in% names(parsed)) {
            audit_row$columns_found <- paste(columns_found, collapse = ";")
            audit_row$notes <- "Parsed data frame has no lineup column"
            return(list(
                lineups = NULL,
                audit = audit_row
            ))
        }

        teams_table <- parsed
    } else if (is.list(parsed)) {
        teams_table <- tryCatch(
            dplyr::bind_rows(parsed),
            error = function(error) {
                audit_row$notes <<- paste(
                    "Could not bind parsed list to teams table:",
                    conditionMessage(error)
                )
                NULL
            }
        )

        if (is.null(teams_table)) {
            return(list(
                lineups = NULL,
                audit = audit_row
            ))
        }

        teams_table <- tibble::as_tibble(teams_table) |>
            janitor::clean_names()
        columns_found <- sort(names(teams_table))
    } else {
        audit_row$notes <- paste(
            "Unsupported parsed JSON type:",
            paste(class(parsed), collapse = "/")
        )
        return(list(
            lineups = NULL,
            audit = audit_row
        ))
    }

    audit_row$columns_found <- paste(columns_found, collapse = ";")

    if (!"lineup" %in% names(teams_table)) {
        audit_row$notes <- "Teams table has no lineup column"
        return(list(
            lineups = NULL,
            audit = audit_row
        ))
    }

    team_lineup_rows <- vector("list", nrow(teams_table))

    for (team_index in seq_len(nrow(teams_table))) {
        team_id <- coalesce_chr(
            teams_table[team_index, , drop = FALSE],
            c("team_id"),
            1L
        )[1L]
        team_name <- coalesce_chr(
            teams_table[team_index, , drop = FALSE],
            c("team_name"),
            1L
        )[1L]

        lineup_object <- teams_table$lineup[[team_index]]
        players_raw <- flatten_team_lineup(lineup_object)

        if (is.null(players_raw)) {
            next
        }

        team_lineup_rows[[team_index]] <- build_player_rows(
            players_raw = players_raw,
            match_id = match_id,
            team_id = team_id,
            team_name = team_name,
            raw_file = raw_file
        )
    }

    lineup_rows <- dplyr::bind_rows(team_lineup_rows)

    if (nrow(lineup_rows) == 0L) {
        audit_row$status <- "success_empty"
        audit_row$rows <- 0L
        return(list(
            lineups = NULL,
            audit = audit_row
        ))
    }

    audit_row$rows <- nrow(lineup_rows)
    audit_row$status <- "success"

    list(
        lineups = lineup_rows,
        audit = audit_row
    )
}

# ============================================================
# Discover raw files
# ============================================================

if (!dir.exists(LINEUPS_RAW_DIR)) {
    stop(
        "Missing StatsBomb lineups directory: ",
        LINEUPS_RAW_DIR,
        ". Run src/04c_download_statsbomb_lineups.R first.",
        call. = FALSE
    )
}

lineup_files <- sort(fs::dir_ls(LINEUPS_RAW_DIR, glob = "*.json", type = "file"))
raw_files_found <- length(lineup_files)

if (raw_files_found == 0L) {
    stop(
        "No StatsBomb lineup JSON files found in ",
        LINEUPS_RAW_DIR,
        call. = FALSE
    )
}

if (!file.exists(MATCHES_PATH)) {
    stop(
        "Missing processed statsbomb_matches.csv. ",
        "Run src/04_download_statsbomb_matches.R first.",
        call. = FALSE
    )
}

message("StatsBomb lineup JSON files found: ", raw_files_found)
message("Processing in chunks of ", CHUNK_SIZE, "...")

file_chunks <- split(
    lineup_files,
    ceiling(seq_along(lineup_files) / CHUNK_SIZE)
)

lineups_chunks <- vector("list", length(file_chunks))
audit_chunks <- vector("list", length(file_chunks))

files_read_successfully <- 0L
files_failed <- 0L

for (chunk_index in seq_along(file_chunks)) {
    chunk_paths <- file_chunks[[chunk_index]]
    chunk_results <- lapply(chunk_paths, transform_statsbomb_lineups_file)

    chunk_lineups <- purrr::map(chunk_results, "lineups")
    chunk_audit <- purrr::map_dfr(chunk_results, "audit")

    lineups_chunks[[chunk_index]] <- dplyr::bind_rows(chunk_lineups)
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

statsbomb_lineups <- dplyr::bind_rows(lineups_chunks)
schema_audit <- dplyr::bind_rows(audit_chunks)

if (nrow(statsbomb_lineups) == 0L) {
    stop(
        "Processed statsbomb_lineups.csv would be empty although raw lineup files exist.",
        call. = FALSE
    )
}

statsbomb_lineups <- statsbomb_lineups |>
    dplyr::select(dplyr::all_of(LINEUP_COLUMNS))

missing_match_id <- is.na(statsbomb_lineups$match_id) |
    statsbomb_lineups$match_id == ""

if (any(missing_match_id)) {
    stop(
        "Processed lineups contain missing match_id values: ",
        sum(missing_match_id),
        call. = FALSE
    )
}

missing_player_id <- is.na(statsbomb_lineups$player_id) |
    statsbomb_lineups$player_id == ""

missing_player_name <- is.na(statsbomb_lineups$player_name) |
    statsbomb_lineups$player_name == ""

if (all(missing_player_id)) {
    stop(
        "All processed lineup rows are missing player_id.",
        call. = FALSE
    )
}

if (all(missing_player_name)) {
    stop(
        "All processed lineup rows are missing player_name.",
        call. = FALSE
    )
}

summary_notes <- paste(
    sprintf("missing player_id rows: %d", sum(missing_player_id)),
    sprintf("missing player_name rows: %d", sum(missing_player_name)),
    sep = "; "
)

cleaning_summary <- tibble::tibble(
    metric = c(
        "raw_files_found",
        "files_read_successfully",
        "files_failed",
        "output_rows",
        "distinct_matches",
        "distinct_teams",
        "distinct_players",
        "missing_match_id_rows",
        "notes"
    ),
    value = c(
        raw_files_found,
        files_read_successfully,
        files_failed,
        nrow(statsbomb_lineups),
        dplyr::n_distinct(statsbomb_lineups$match_id),
        dplyr::n_distinct(
            paste(statsbomb_lineups$match_id, statsbomb_lineups$team_id, sep = ":")
        ),
        dplyr::n_distinct(statsbomb_lineups$player_id),
        sum(missing_match_id),
        summary_notes
    )
)

readr::write_csv(statsbomb_lineups, LINEUPS_OUT)
readr::write_csv(cleaning_summary, SUMMARY_OUT)
readr::write_csv(schema_audit, AUDIT_OUT)

message("Done.")
message("StatsBomb lineup rows: ", nrow(statsbomb_lineups))
message("Files failed: ", files_failed)
message("Lineups output: ", LINEUPS_OUT)
message("Cleaning summary: ", SUMMARY_OUT)
message("Schema audit: ", AUDIT_OUT)

message("")
message("Cleaning summary:")
print(cleaning_summary, n = Inf)
