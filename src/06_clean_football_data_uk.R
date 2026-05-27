# ============================================================
# 06_clean_football_data_uk.R
# Clean football-data.co.uk match files into one table
# Handles old files with inconsistent trailing empty columns
# ============================================================

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

fd_uk_dir <- file.path(RAW_DIR, "football_data_uk")

if (!dir.exists(fd_uk_dir)) {
    stop("Missing football_data_uk raw directory. Run 05_download_football_data_uk.R first.")
}

fd_uk_files <- fs::dir_ls(
    fd_uk_dir,
    recurse = TRUE,
    glob = "*.csv",
    fail = FALSE
)

if (length(fd_uk_files) == 0) {
    stop("No football-data.co.uk CSV files found.")
}

# -----------------------------
# Helper functions
# -----------------------------

read_fd_uk_csv <- function(path) {
    tryCatch(
        {
            dat <- data.table::fread(
                file = path,
                fill = Inf,
                na.strings = c("", "NA", "N/A"),
                showProgress = FALSE,
                data.table = FALSE,
                check.names = TRUE,
                encoding = "Latin-1"
            )

            dat <- tibble::as_tibble(
                dat,
                .name_repair = "unique"
            )

            dat
        },
        error = function(e) {
            warning(
                "Could not read football-data.co.uk CSV: ",
                path,
                "\nReason: ",
                conditionMessage(e)
            )
            NULL
        }
    )
}
get_chr <- function(df, col) {
    if (col %in% names(df)) {
        as.character(df[[col]])
    } else {
        rep(NA_character_, nrow(df))
    }
}

get_int <- function(df, col) {
    if (col %in% names(df)) {
        suppressWarnings(as.integer(df[[col]]))
    } else {
        rep(NA_integer_, nrow(df))
    }
}

get_num <- function(df, col) {
    if (col %in% names(df)) {
        suppressWarnings(as.numeric(df[[col]]))
    } else {
        rep(NA_real_, nrow(df))
    }
}

read_one_fd_uk_file <- function(path) {
    dat <- read_fd_uk_csv(path)

    if (is.null(dat)) {
        return(tibble::tibble())
    }

    if (nrow(dat) == 0) {
        return(tibble::tibble())
    }

    dat_clean <- dat |>
        janitor::clean_names()

    required <- c("div", "date", "home_team", "away_team", "fthg", "ftag", "ftr")

    if (!all(required %in% names(dat_clean))) {
        message("Skipping file with missing required columns: ", path)
        return(tibble::tibble())
    }

    path_parts <- stringr::str_split(path, .Platform$file.sep, simplify = TRUE)
    season_code <- path_parts[, ncol(path_parts) - 1]
    league_code <- fs::path_ext_remove(basename(path))

    out <- tibble::tibble(
        source = "football-data.co.uk",
        raw_file = path,
        source_match_id = paste0(
            league_code,
            "_",
            season_code,
            "_",
            seq_len(nrow(dat_clean))
        ),
        source_league_code = league_code,
        source_season_code = season_code,

        date = parse_fd_uk_date(get_chr(dat_clean, "date")),
        season = as.character(season_code),
        competition = get_chr(dat_clean, "div"),

        home_team = stringr::str_squish(get_chr(dat_clean, "home_team")),
        away_team = stringr::str_squish(get_chr(dat_clean, "away_team")),

        home_score = get_int(dat_clean, "fthg"),
        away_score = get_int(dat_clean, "ftag"),
        full_time_result = get_chr(dat_clean, "ftr"),

        half_time_home_score = get_int(dat_clean, "hthg"),
        half_time_away_score = get_int(dat_clean, "htag"),
        half_time_result = get_chr(dat_clean, "htr"),

        home_shots = get_int(dat_clean, "hs"),
        away_shots = get_int(dat_clean, "as"),
        home_shots_on_target = get_int(dat_clean, "hst"),
        away_shots_on_target = get_int(dat_clean, "ast"),

        home_corners = get_int(dat_clean, "hc"),
        away_corners = get_int(dat_clean, "ac"),

        home_fouls = get_int(dat_clean, "hf"),
        away_fouls = get_int(dat_clean, "af"),

        home_yellow_cards = get_int(dat_clean, "hy"),
        away_yellow_cards = get_int(dat_clean, "ay"),

        home_red_cards = get_int(dat_clean, "hr"),
        away_red_cards = get_int(dat_clean, "ar")
    ) |>
        dplyr::mutate(
            match_result = dplyr::case_when(
                home_score > away_score ~ "home_win",
                home_score == away_score ~ "draw",
                home_score < away_score ~ "away_win",
                TRUE ~ NA_character_
            ),
            result_class = dplyr::case_when(
                match_result == "home_win" ~ 1L,
                match_result == "draw" ~ 0L,
                match_result == "away_win" ~ -1L,
                TRUE ~ NA_integer_
            ),
            home_win = as.integer(match_result == "home_win"),
            draw = as.integer(match_result == "draw"),
            away_win = as.integer(match_result == "away_win"),
            goal_difference = home_score - away_score,
            total_goals = home_score + away_score,
            neutral = NA
        ) |>
        dplyr::filter(
            !is.na(date),
            !is.na(home_team),
            !is.na(away_team),
            !is.na(home_score),
            !is.na(away_score),
            home_team != "",
            away_team != ""
        )

    out
}

football_data_uk_matches <- purrr::map_dfr(
    fd_uk_files,
    read_one_fd_uk_file
)

football_data_uk_out <- file.path(
    PROCESSED_DIR,
    "football_data_uk_matches.csv"
)

readr::write_csv(
    football_data_uk_matches,
    football_data_uk_out
)

saveRDS(
    football_data_uk_matches,
    file.path(PROCESSED_DIR, "football_data_uk_matches.rds")
)

message("Done.")
message("football-data.co.uk cleaned rows: ", nrow(football_data_uk_matches))
message("Output: ", football_data_uk_out)