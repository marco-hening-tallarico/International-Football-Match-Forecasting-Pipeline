# ============================================================
# 04e_clean_statsbomb_events.R
# Flatten StatsBomb raw event JSON into processed event and shot tables
#
# Inputs:
#   data/raw/statsbomb_open/events/{match_id}.json
#   data/processed/statsbomb_matches.csv  (lineage reference)
#
# Outputs:
#   data/processed/statsbomb_events.csv
#   data/processed/statsbomb_shots.csv
#   data/validation/statsbomb_events_cleaning_summary.csv
#   data/validation/statsbomb_events_schema_audit.csv
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

STATSBOMB_SOURCE <- "StatsBomb Open Data"
EVENTS_RAW_DIR <- file.path(RAW_DIR, "statsbomb_open", "events")
MATCHES_PATH <- file.path(PROCESSED_DIR, "statsbomb_matches.csv")

EVENTS_OUT <- file.path(PROCESSED_DIR, "statsbomb_events.csv")
SHOTS_OUT <- file.path(PROCESSED_DIR, "statsbomb_shots.csv")
SUMMARY_OUT <- file.path(VALIDATION_DIR, "statsbomb_events_cleaning_summary.csv")
AUDIT_OUT <- file.path(VALIDATION_DIR, "statsbomb_events_schema_audit.csv")

CHUNK_SIZE <- 50L
PROGRESS_EVERY <- 100L

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

get_dbl <- function(df, col, n = nrow(df)) {
    if (col %in% names(df)) {
        suppressWarnings(as.numeric(df[[col]]))
    } else {
        rep(NA_real_, n)
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

coalesce_lgl <- function(df, cols, n = nrow(df)) {
    out <- rep(NA, n)

    for (col in cols) {
        if (!col %in% names(df)) {
            next
        }

        values <- as.logical(df[[col]])
        replace_idx <- is.na(out) & !is.na(values)
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

serialize_related_events <- function(column) {
    vapply(
        column,
        function(value) {
            if (is.null(value) || length(value) == 0L) {
                return(NA_character_)
            }

            paste(as.character(unlist(value)), collapse = ";")
        },
        FUN.VALUE = character(1)
    )
}

extract_goalkeeper_from_freeze_frame <- function(column) {
    vapply(
        column,
        function(frame) {
            if (is.null(frame) || !is.data.frame(frame) || nrow(frame) == 0L) {
                return(NA_character_)
            }

            if ("position.name" %in% names(frame)) {
                keeper_idx <- frame$position.name == "Goalkeeper"
                if (any(keeper_idx, na.rm = TRUE) && "player.name" %in% names(frame)) {
                    return(as.character(frame$player.name[which(keeper_idx)[1L]]))
                }
            }

            if ("position_name" %in% names(frame)) {
                keeper_idx <- frame$position_name == "Goalkeeper"
                if (any(keeper_idx, na.rm = TRUE) && "player_name" %in% names(frame)) {
                    return(as.character(frame$player_name[which(keeper_idx)[1L]]))
                }
            }

            NA_character_
        },
        FUN.VALUE = character(1)
    )
}

extract_match_id_from_path <- function(path) {
    tools::file_path_sans_ext(basename(path))
}

# ============================================================
# Transform one raw event file
# ============================================================

transform_statsbomb_events_file <- function(path) {
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
        jsonlite::fromJSON(path, flatten = TRUE),
        error = function(error) {
            audit_row$notes <<- conditionMessage(error)
            NULL
        }
    )

    if (is.null(parsed)) {
        return(list(
            events = NULL,
            shots = NULL,
            audit = audit_row
        ))
    }

    if (!is.data.frame(parsed)) {
        audit_row$notes <- "Parsed JSON is not a data frame"
        return(list(
            events = NULL,
            shots = NULL,
            audit = audit_row
        ))
    }

    if (nrow(parsed) == 0L) {
        audit_row$status <- "success_empty"
        audit_row$columns_found <- ""
        return(list(
            events = NULL,
            shots = NULL,
            audit = audit_row
        ))
    }

    events_raw <- tibble::as_tibble(parsed) |>
        janitor::clean_names()

    audit_row$columns_found <- paste(sort(names(events_raw)), collapse = ";")
    audit_row$rows <- nrow(events_raw)

    n <- nrow(events_raw)

    location_column <- if ("location" %in% names(events_raw)) {
        events_raw$location
    } else {
        vector("list", n)
    }

    pass_end_location_column <- if ("pass_end_location" %in% names(events_raw)) {
        events_raw$pass_end_location
    } else {
        vector("list", n)
    }

    carry_end_location_column <- if ("carry_end_location" %in% names(events_raw)) {
        events_raw$carry_end_location
    } else {
        vector("list", n)
    }

    related_events_column <- if ("related_events" %in% names(events_raw)) {
        events_raw$related_events
    } else {
        vector("list", n)
    }

    events_clean <- tibble::tibble(
        source = STATSBOMB_SOURCE,
        raw_file = raw_file,
        match_id = as.character(match_id),
        event_id = coalesce_chr(events_raw, c("id"), n),
        index = get_int(events_raw, "index", n),
        period = get_int(events_raw, "period", n),
        timestamp = get_chr(events_raw, "timestamp", n),
        minute = get_int(events_raw, "minute", n),
        second = get_int(events_raw, "second", n),
        possession = get_int(events_raw, "possession", n),
        possession_team = coalesce_chr(
            events_raw,
            c("possession_team_name", "possession_team.name"),
            n
        ),
        team = coalesce_chr(events_raw, c("team_name", "team.name"), n),
        player = coalesce_chr(events_raw, c("player_name", "player.name"), n),
        position = coalesce_chr(events_raw, c("position_name", "position.name"), n),
        event_type = coalesce_chr(events_raw, c("type_name", "type.name"), n),
        play_pattern = coalesce_chr(
            events_raw,
            c("play_pattern_name", "play_pattern.name"),
            n
        ),
        location_x = extract_list_coord(location_column, 1L),
        location_y = extract_list_coord(location_column, 2L),
        duration = get_dbl(events_raw, "duration", n),
        under_pressure = get_lgl(events_raw, "under_pressure", n),
        counterpress = get_lgl(events_raw, "counterpress", n),
        off_camera = get_lgl(events_raw, "off_camera", n),
        out = get_lgl(events_raw, "out", n),
        related_events = serialize_related_events(related_events_column),
        tactics_formation = get_dbl(events_raw, "tactics_formation", n),
        pass_recipient = coalesce_chr(
            events_raw,
            c("pass_recipient_name", "pass.recipient.name"),
            n
        ),
        pass_length = get_dbl(events_raw, "pass_length", n),
        pass_angle = get_dbl(events_raw, "pass_angle", n),
        pass_height = coalesce_chr(
            events_raw,
            c("pass_height_name", "pass_height.name"),
            n
        ),
        pass_end_location_x = extract_list_coord(pass_end_location_column, 1L),
        pass_end_location_y = extract_list_coord(pass_end_location_column, 2L),
        pass_outcome = coalesce_chr(
            events_raw,
            c("pass_outcome_name", "pass_outcome.name"),
            n
        ),
        pass_body_part = coalesce_chr(
            events_raw,
            c("pass_body_part_name", "pass_body_part.name"),
            n
        ),
        carry_end_location_x = extract_list_coord(carry_end_location_column, 1L),
        carry_end_location_y = extract_list_coord(carry_end_location_column, 2L),
        dribble_outcome = coalesce_chr(
            events_raw,
            c("dribble_outcome_name", "dribble_outcome.name"),
            n
        ),
        duel_type = coalesce_chr(events_raw, c("duel_type_name", "duel_type.name"), n),
        duel_outcome = coalesce_chr(
            events_raw,
            c("duel_outcome_name", "duel_outcome.name"),
            n
        ),
        foul_committed_card = coalesce_chr(
            events_raw,
            c("foul_committed_card_name", "foul_committed_card.name"),
            n
        ),
        foul_won_defensive = get_lgl(events_raw, "foul_won_defensive", n),
        interception_outcome = coalesce_chr(
            events_raw,
            c("interception_outcome_name", "interception_outcome.name"),
            n
        ),
        goalkeeper_type = coalesce_chr(
            events_raw,
            c("goalkeeper_type_name", "goalkeeper_type.name"),
            n
        ),
        goalkeeper_outcome = coalesce_chr(
            events_raw,
            c("goalkeeper_outcome_name", "goalkeeper_outcome.name"),
            n
        ),
        substitution_replacement = coalesce_chr(
            events_raw,
            c("substitution_replacement_name", "substitution_replacement.name"),
            n
        ),
        bad_behaviour_card = coalesce_chr(
            events_raw,
            c("bad_behaviour_card_name", "bad_behaviour_card.name"),
            n
        )
    )

    shot_end_location_column <- if ("shot_end_location" %in% names(events_raw)) {
        events_raw$shot_end_location
    } else {
        vector("list", n)
    }

    shot_freeze_frame_column <- if ("shot_freeze_frame" %in% names(events_raw)) {
        events_raw$shot_freeze_frame
    } else {
        vector("list", n)
    }

    is_shot <- events_clean$event_type == "Shot"

    shots_clean <- events_clean |>
        dplyr::filter(is_shot) |>
        dplyr::mutate(
            shot_statsbomb_xg = get_dbl(events_raw, "shot_statsbomb_xg", n)[is_shot],
            shot_outcome = coalesce_chr(
                events_raw,
                c("shot_outcome_name", "shot_outcome.name"),
                n
            )[is_shot],
            shot_body_part = coalesce_chr(
                events_raw,
                c("shot_body_part_name", "shot_body_part.name"),
                n
            )[is_shot],
            shot_type = coalesce_chr(
                events_raw,
                c("shot_type_name", "shot_type.name"),
                n
            )[is_shot],
            shot_technique = coalesce_chr(
                events_raw,
                c("shot_technique_name", "shot_technique.name"),
                n
            )[is_shot],
            shot_first_time = coalesce_lgl(events_raw, c("shot_first_time"), n)[is_shot],
            shot_one_on_one = coalesce_lgl(events_raw, c("shot_one_on_one"), n)[is_shot],
            shot_open_goal = coalesce_lgl(events_raw, c("shot_open_goal"), n)[is_shot],
            shot_deflected = coalesce_lgl(events_raw, c("shot_deflected"), n)[is_shot],
            shot_end_location_x = extract_list_coord(shot_end_location_column, 1L)[is_shot],
            shot_end_location_y = extract_list_coord(shot_end_location_column, 2L)[is_shot],
            shot_end_location_z = extract_list_coord(shot_end_location_column, 3L)[is_shot],
            goalkeeper = coalesce_chr(
                events_raw,
                c(
                    "shot_goalkeeper_name",
                    "shot_goalkeeper.name",
                    "goalkeeper_name",
                    "goalkeeper.name"
                ),
                n
            )[is_shot]
        )

    if (nrow(shots_clean) > 0L) {
        missing_goalkeeper <- is.na(shots_clean$goalkeeper) | shots_clean$goalkeeper == ""

        if (any(missing_goalkeeper)) {
            inferred_goalkeeper <- extract_goalkeeper_from_freeze_frame(
                shot_freeze_frame_column
            )[is_shot]

            shots_clean$goalkeeper[missing_goalkeeper] <- inferred_goalkeeper[
                missing_goalkeeper
            ]
        }
    }

    shots_clean <- shots_clean |>
        dplyr::select(
            source,
            raw_file,
            match_id,
            event_id,
            index,
            period,
            timestamp,
            minute,
            second,
            team,
            player,
            position,
            location_x,
            location_y,
            shot_statsbomb_xg,
            shot_outcome,
            shot_body_part,
            shot_type,
            shot_technique,
            shot_first_time,
            shot_one_on_one,
            shot_open_goal,
            shot_deflected,
            shot_end_location_x,
            shot_end_location_y,
            shot_end_location_z,
            goalkeeper,
            possession,
            possession_team,
            under_pressure
        )

    audit_row$status <- "success"

    list(
        events = events_clean,
        shots = shots_clean,
        audit = audit_row
    )
}

# ============================================================
# Discover raw files
# ============================================================

if (!dir.exists(EVENTS_RAW_DIR)) {
    stop(
        "Missing StatsBomb events directory: ",
        EVENTS_RAW_DIR,
        ". Run src/04b_download_statsbomb_events.R first.",
        call. = FALSE
    )
}

event_files <- sort(fs::dir_ls(EVENTS_RAW_DIR, glob = "*.json", type = "file"))
raw_files_found <- length(event_files)

if (raw_files_found == 0L) {
    stop(
        "No StatsBomb event JSON files found in ",
        EVENTS_RAW_DIR,
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

message("StatsBomb event JSON files found: ", raw_files_found)
message("Processing in chunks of ", CHUNK_SIZE, "...")

file_chunks <- split(
    event_files,
    ceiling(seq_along(event_files) / CHUNK_SIZE)
)

events_chunks <- vector("list", length(file_chunks))
shots_chunks <- vector("list", length(file_chunks))
audit_chunks <- vector("list", length(file_chunks))

files_read_successfully <- 0L
files_failed <- 0L

for (chunk_index in seq_along(file_chunks)) {
  chunk_paths <- file_chunks[[chunk_index]]
  chunk_results <- lapply(chunk_paths, transform_statsbomb_events_file)

  chunk_events <- purrr::map(chunk_results, "events")
  chunk_shots <- purrr::map(chunk_results, "shots")
  chunk_audit <- purrr::map_dfr(chunk_results, "audit")

  events_chunks[[chunk_index]] <- dplyr::bind_rows(chunk_events)
  shots_chunks[[chunk_index]] <- dplyr::bind_rows(chunk_shots)
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

statsbomb_events <- dplyr::bind_rows(events_chunks)
statsbomb_shots <- dplyr::bind_rows(shots_chunks)
schema_audit <- dplyr::bind_rows(audit_chunks)

if (nrow(statsbomb_events) == 0L) {
    stop(
        "Processed statsbomb_events.csv would be empty although raw event files exist.",
        call. = FALSE
    )
}

missing_event_id <- is.na(statsbomb_events$event_id) |
    statsbomb_events$event_id == ""

if (any(missing_event_id)) {
    stop(
        "Processed events contain missing event_id values: ",
        sum(missing_event_id),
        call. = FALSE
    )
}

missing_match_id <- is.na(statsbomb_events$match_id) |
    statsbomb_events$match_id == ""

if (any(missing_match_id)) {
    stop(
        "Processed events contain missing match_id values: ",
        sum(missing_match_id),
        call. = FALSE
    )
}

events_per_match <- statsbomb_events |>
    dplyr::count(match_id, name = "events_in_match")

shot_events_in_events_table <- sum(statsbomb_events$event_type == "Shot", na.rm = TRUE)
shots_review_flag <- nrow(statsbomb_shots) == 0L && shot_events_in_events_table > 0L

cleaning_summary <- tibble::tibble(
    metric = c(
        "raw_files_found",
        "files_read_successfully",
        "files_failed",
        "events_rows",
        "shots_rows",
        "distinct_matches",
        "min_events_per_match",
        "max_events_per_match",
        "median_events_per_match",
        "shot_events_in_events_table",
        "shots_table_empty_review_flag"
    ),
    value = c(
        raw_files_found,
        files_read_successfully,
        files_failed,
        nrow(statsbomb_events),
        nrow(statsbomb_shots),
        dplyr::n_distinct(statsbomb_events$match_id),
        min(events_per_match$events_in_match),
        max(events_per_match$events_in_match),
        stats::median(events_per_match$events_in_match),
        shot_events_in_events_table,
        as.integer(shots_review_flag)
    )
)

readr::write_csv(statsbomb_events, EVENTS_OUT)
readr::write_csv(statsbomb_shots, SHOTS_OUT)
readr::write_csv(cleaning_summary, SUMMARY_OUT)
readr::write_csv(schema_audit, AUDIT_OUT)

message("Done.")
message("StatsBomb events rows: ", nrow(statsbomb_events))
message("StatsBomb shots rows: ", nrow(statsbomb_shots))
message("Files failed: ", files_failed)
message("Events output: ", EVENTS_OUT)
message("Shots output: ", SHOTS_OUT)
message("Cleaning summary: ", SUMMARY_OUT)
message("Schema audit: ", AUDIT_OUT)

if (isTRUE(shots_review_flag)) {
    warning(
        "Shot events exist in statsbomb_events but statsbomb_shots is empty; review required.",
        call. = FALSE
    )
}
