# 15_download_international_ratings.R
#
# Downloads World Football Elo per-team match-history TSV files from
# eloratings.net and combines them into one raw ratings CSV.
#
# Writes: data/raw/international_ratings/world_football_elo.csv
#
# Notes:
# - Falls back to manual-download instructions if automated fetch fails.

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

ELO_BASE_URI <- "http://www.eloratings.net/"
ELO_TEAMS_ENDPOINT <- "en.teams.tsv"
ELO_SUCCESSORS_ENDPOINT <- "teams.tsv"
RATINGS_SOURCE <- "world_football_elo"
MANUAL_DOWNLOAD_INSTRUCTIONS <- paste(
    "Place a manually prepared ratings CSV at:",
    file.path(RAW_DIR, "international_ratings", "world_football_elo.csv"),
  "Required columns (names may vary; cleaning script will map them):",
    "rating date, team name, numeric rating, optional rank.",
    "Source should be World Football Elo (eloratings.net).",
    sep = "\n"
)

ratings_dir <- file.path(RAW_DIR, "international_ratings")
output_path <- file.path(ratings_dir, "world_football_elo.csv")
dir.create(ratings_dir, recursive = TRUE, showWarnings = FALSE)

skip_download <- file.exists(output_path)

if (isTRUE(skip_download)) {
    existing <- readr::read_csv(output_path, show_col_types = FALSE)

    if (nrow(existing) > 0L) {
        message("Ratings raw file already exists: ", output_path)
        message("Rows: ", nrow(existing))
        message("Delete the file to force a fresh download from eloratings.net.")
        message("Done.")
    } else {
        message("Ratings raw file exists but is empty; re-downloading.")
        skip_download <- FALSE
    }
}

is_team_id <- function(x) {
    nchar(x) == 2L && grepl("^[A-Z]{2}$", x)
}

format_elo_page_name <- function(team_name) {
    ascii_name <- iconv(team_name, from = "", to = "ASCII//TRANSLIT")
    ascii_name <- ifelse(is.na(ascii_name), team_name, ascii_name)
    stringr::str_replace_all(ascii_name, " ", "_")
}

fetch_elo_text <- function(endpoint) {
    url <- paste0(ELO_BASE_URI, endpoint)
    message("Fetching: ", url)

    response <- tryCatch(
        httr2::request(url) |>
            httr2::req_timeout(120) |>
            httr2::req_perform(),
        error = function(error) {
            stop(
                "Could not reach eloratings.net: ",
                conditionMessage(error),
                "\n\n",
                MANUAL_DOWNLOAD_INSTRUCTIONS,
                call. = FALSE
            )
        }
    )

    if (httr2::resp_status(response) != 200L) {
        stop(
            "eloratings.net returned HTTP ",
            httr2::resp_status(response),
            " for ",
            url,
            "\n\n",
            MANUAL_DOWNLOAD_INSTRUCTIONS,
            call. = FALSE
        )
    }

    httr2::resp_body_string(response)
}

parse_elo_teams <- function(teams_text) {
    lines <- strsplit(teams_text, "\n", fixed = TRUE)[[1L]]
    lines <- lines[nzchar(lines)]

    parsed <- purrr::map(
        lines,
        function(line) {
            fields <- strsplit(line, "\t", fixed = TRUE)[[1L]]
            if (length(fields) < 2L || !is_team_id(fields[[1L]])) {
                return(NULL)
            }

            tibble::tibble(
                team_id = fields[[1L]],
                team_name = stringr::str_squish(fields[[2L]])
            )
        }
    )

    teams <- purrr::compact(parsed) |>
        dplyr::bind_rows() |>
        dplyr::distinct(.data$team_id, .keep_all = TRUE)

    if (nrow(teams) == 0L) {
        stop(
            "Could not parse team list from eloratings.net.\n\n",
            MANUAL_DOWNLOAD_INSTRUCTIONS,
            call. = FALSE
        )
    }

    teams
}

parse_elo_successors <- function(successors_text) {
    lines <- strsplit(successors_text, "\n", fixed = TRUE)[[1L]]
    lines <- lines[nzchar(lines)]

    parsed <- purrr::map(
        lines,
        function(line) {
            fields <- strsplit(line, "\t", fixed = TRUE)[[1L]]
            if (length(fields) < 2L ||
                    !is_team_id(fields[[1L]]) ||
                    !is_team_id(fields[[2L]])) {
                return(NULL)
            }

            tibble::tibble(
                predecessor_id = fields[[1L]],
                successor_id = fields[[2L]]
            )
        }
    )

    purrr::compact(parsed) |>
        dplyr::bind_rows() |>
        dplyr::distinct()
}

get_historical_team_ids <- function(team_id, successors) {
    predecessor_ids <- successors$predecessor_id[
        successors$successor_id == team_id
    ]
    unique(c(team_id, predecessor_ids))
}

parse_team_match_history <- function(history_text, team_id, historical_team_ids) {
    lines <- strsplit(history_text, "\n", fixed = TRUE)[[1L]]
    lines <- lines[nzchar(lines)]

    parsed_rows <- purrr::map(
        lines,
        function(line) {
            fields <- strsplit(line, "\t", fixed = TRUE)[[1L]]
            if (length(fields) < 12L) {
                return(NULL)
            }

            year <- suppressWarnings(as.integer(fields[[1L]]))
            month <- suppressWarnings(as.integer(fields[[2L]]))
            day <- suppressWarnings(as.integer(fields[[3L]]))

            if (any(is.na(c(year, month, day)))) {
                return(NULL)
            }

            month <- max(month, 1L)
            day <- max(day, 1L)
            rating_date <- suppressWarnings(
                as.Date(sprintf("%04d-%02d-%02d", year, month, day))
            )

            if (is.na(rating_date)) {
                return(NULL)
            }

            home_team_id <- fields[[4L]]
            away_team_id <- fields[[5L]]

            home_elo <- suppressWarnings(as.numeric(fields[[11L]]))
            away_elo <- suppressWarnings(as.numeric(fields[[12L]]))

            team_elo <- dplyr::case_when(
                home_team_id %in% historical_team_ids ~ home_elo,
                away_team_id %in% historical_team_ids ~ away_elo,
                TRUE ~ NA_real_
            )

            if (is.na(team_elo)) {
                return(NULL)
            }

            tibble::tibble(
                rating_date = rating_date,
                team_id = team_id,
                rating = team_elo
            )
        }
    )

    purrr::compact(parsed_rows) |>
        dplyr::bind_rows()
}

download_team_history <- function(page_name) {
    url <- paste0(ELO_BASE_URI, page_name, ".tsv")

    response <- tryCatch(
        httr2::request(url) |>
            httr2::req_timeout(60) |>
            httr2::req_perform(),
        error = function(error) {
            warning(
                "Failed to download team history: ",
                url,
                " — ",
                conditionMessage(error),
                call. = FALSE
            )
            NULL
        }
    )

    if (is.null(response) || httr2::resp_status(response) != 200L) {
        return(NULL)
    }

    httr2::resp_body_string(response)
}

if (isTRUE(skip_download)) {
    invisible(NULL)
} else {

teams <- parse_elo_teams(fetch_elo_text(ELO_TEAMS_ENDPOINT))
successors <- parse_elo_successors(fetch_elo_text(ELO_SUCCESSORS_ENDPOINT))

successor_by_predecessor <- stats::setNames(
    successors$successor_id,
    successors$predecessor_id
)

terminal_teams <- teams |>
    dplyr::filter(!.data$team_id %in% successors$predecessor_id) |>
    dplyr::arrange(.data$team_name)

message(
    "Downloading match-history ratings for ",
    nrow(terminal_teams),
    " current World Football Elo entities..."
)

rating_chunks <- vector("list", nrow(terminal_teams))
failed_teams <- character()

for (team_index in seq_len(nrow(terminal_teams))) {
    team_row <- terminal_teams[team_index, ]
    page_name <- format_elo_page_name(team_row$team_name)
    historical_team_ids <- get_historical_team_ids(
        team_row$team_id,
        successors
    )

    history_text <- download_team_history(page_name)

    if (is.null(history_text)) {
        failed_teams <- c(failed_teams, team_row$team_name)
        next
    }

    team_ratings <- parse_team_match_history(
        history_text,
        team_id = team_row$team_id,
        historical_team_ids = historical_team_ids
    )

    if (nrow(team_ratings) == 0L) {
        failed_teams <- c(failed_teams, team_row$team_name)
        next
    }

    rating_chunks[[team_index]] <- team_ratings |>
        dplyr::mutate(
            team = team_row$team_name,
            source = RATINGS_SOURCE
        )

    if (team_index %% 25L == 0L || team_index == nrow(terminal_teams)) {
        message("  Progress: ", team_index, " / ", nrow(terminal_teams))
    }

    Sys.sleep(0.05)
}

world_football_elo_raw <- dplyr::bind_rows(rating_chunks)

if (nrow(world_football_elo_raw) == 0L) {
    stop(
        "No ratings rows were downloaded from eloratings.net.\n\n",
        MANUAL_DOWNLOAD_INSTRUCTIONS,
        call. = FALSE
    )
}

if (length(failed_teams) > 0L) {
    warning(
        "Failed or empty histories for ",
        length(failed_teams),
        " teams (first 10): ",
        paste(head(failed_teams, 10L), collapse = ", "),
        call. = FALSE
    )
}

world_football_elo_raw <- world_football_elo_raw |>
    dplyr::arrange(.data$team, .data$rating_date) |>
    dplyr::group_by(.data$team, .data$rating_date) |>
    dplyr::slice_tail(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
        rating_date = .data$rating_date,
        team = .data$team,
        team_id = .data$team_id,
        rating = .data$rating,
        source = .data$source
    )

readr::write_csv(world_football_elo_raw, output_path)

source_manifest <- read_or_create_manifest(META_DIR)
source_manifest <- dplyr::bind_rows(
    source_manifest,
    add_manifest_record(
        source = "eloratings.net",
        dataset = "world_football_elo",
        url = paste0(ELO_BASE_URI, "{team_page}.tsv"),
        local_path = output_path,
        notes = paste(
            "Built from per-team match-history TSV files;",
            nrow(world_football_elo_raw),
            "rows"
        )
    )
)
write_source_manifest(source_manifest, META_DIR)

message("Done.")
message("Wrote: ", output_path)
message("Rating rows: ", nrow(world_football_elo_raw))
message("Teams with ratings: ", dplyr::n_distinct(world_football_elo_raw$team))
message(
    "Rating date range: ",
    min(world_football_elo_raw$rating_date),
    " to ",
    max(world_football_elo_raw$rating_date)
)

}
