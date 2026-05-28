# ============================================================
# 06_clean_football_data_uk.R
# Clean football-data.co.uk match files into core + odds tables
# Handles old files with inconsistent trailing empty columns
#
# Outputs:
#   data/processed/football_data_uk_matches.csv
#   data/processed/football_data_uk_matches_core.csv
#   data/processed/football_data_uk_odds_wide.csv
#   data/validation/football_data_uk_cleaning_summary.csv
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

MATCH_RESULT_COLS <- c(
    "div",
    "date",
    "time",
    "home_team",
    "away_team",
    "fthg",
    "ftag",
    "ftr",
    "hthg",
    "htag",
    "htr",
    "referee",
    "hs",
    "as",
    "hst",
    "ast",
    "hf",
    "af",
    "hc",
    "ac",
    "hy",
    "ay",
    "hr",
    "ar"
)

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

            tibble::as_tibble(dat, .name_repair = "unique")
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

get_num <- function(df, col, n = nrow(df)) {
    if (col %in% names(df)) {
        suppressWarnings(as.numeric(df[[col]]))
    } else {
        rep(NA_real_, n)
    }
}

compute_market_probs <- function(home_odds, draw_odds, away_odds, n_rows) {
    home_odds <- rep_len(home_odds, n_rows)
    draw_odds <- rep_len(draw_odds, n_rows)
    away_odds <- rep_len(away_odds, n_rows)

    raw_home <- ifelse(is.finite(home_odds) & home_odds > 0, 1 / home_odds, NA_real_)
    raw_draw <- ifelse(is.finite(draw_odds) & draw_odds > 0, 1 / draw_odds, NA_real_)
    raw_away <- ifelse(is.finite(away_odds) & away_odds > 0, 1 / away_odds, NA_real_)
    total <- raw_home + raw_draw + raw_away

    tibble::tibble(
        home_implied_prob = ifelse(is.finite(total) & total > 0, raw_home / total, NA_real_),
        draw_implied_prob = ifelse(is.finite(total) & total > 0, raw_draw / total, NA_real_),
        away_implied_prob = ifelse(is.finite(total) & total > 0, raw_away / total, NA_real_),
        market_overround = ifelse(is.finite(total) & total > 0, total - 1, NA_real_)
    )
}

extract_preserved_odds_cols <- function(dat_clean, n_rows) {
    odds_cols <- setdiff(names(dat_clean), MATCH_RESULT_COLS)

    if (length(odds_cols) == 0L) {
        return(tibble::tibble())
    }

    preserved_values <- purrr::map(
        odds_cols,
        function(col_name) {
            tryCatch(
                {
                    values <- get_num(dat_clean, col_name, n_rows)
                    values <- rep_len(values, n_rows)

                    if (!all(is.na(values))) {
                        stats::setNames(list(values), col_name)
                    } else {
                        NULL
                    }
                },
                error = function(e) {
                    NULL
                }
            )
        }
    ) |>
        purrr::compact()

    if (length(preserved_values) == 0L) {
        return(tibble::tibble())
    }

    preserved <- tibble::as_tibble(purrr::list_c(preserved_values))

    if (nrow(preserved) != n_rows) {
        preserved <- preserved[seq_len(n_rows), , drop = FALSE]
    }

    preserved
}

read_one_fd_uk_file <- function(path) {
    dat <- read_fd_uk_csv(path)

    if (is.null(dat) || nrow(dat) == 0L) {
        return(list(core = tibble::tibble(), odds = tibble::tibble()))
    }

    dat_clean <- dat |>
        janitor::clean_names()

    required <- c("div", "date", "home_team", "away_team", "fthg", "ftag", "ftr")

    if (!all(required %in% names(dat_clean))) {
        message("Skipping file with missing required columns: ", path)
        return(list(core = tibble::tibble(), odds = tibble::tibble()))
    }

    path_parts <- stringr::str_split(path, .Platform$file.sep, simplify = TRUE)
    season_code <- path_parts[, ncol(path_parts) - 1] |>
        as.character() |>
        stringr::str_pad(width = 4, side = "left", pad = "0")
    league_code <- fs::path_ext_remove(basename(path)) |>
        as.character()

    n_rows <- nrow(dat_clean)

    source_row_index <- seq_len(n_rows)

    match_core <- tibble::tibble(
        source = as.character("football-data.co.uk"),
        raw_file = as.character(path),
        source_row_index = source_row_index,
        source_match_id = paste0(
            league_code,
            "_",
            season_code,
            "_",
            source_row_index
        ),
        source_league_code = league_code,
        source_season_code = season_code,
        date = parse_fd_uk_date(get_chr(dat_clean, "date", n_rows)),
        season = as.character(season_code),
        competition = get_chr(dat_clean, "div", n_rows),
        home_team = stringr::str_squish(get_chr(dat_clean, "home_team", n_rows)),
        away_team = stringr::str_squish(get_chr(dat_clean, "away_team", n_rows)),
        home_score = get_int(dat_clean, "fthg", n_rows),
        away_score = get_int(dat_clean, "ftag", n_rows),
        full_time_result = get_chr(dat_clean, "ftr", n_rows),
        half_time_home_score = get_int(dat_clean, "hthg", n_rows),
        half_time_away_score = get_int(dat_clean, "htag", n_rows),
        half_time_result = get_chr(dat_clean, "htr", n_rows),
        home_shots = get_int(dat_clean, "hs", n_rows),
        away_shots = get_int(dat_clean, "as", n_rows),
        home_shots_on_target = get_int(dat_clean, "hst", n_rows),
        away_shots_on_target = get_int(dat_clean, "ast", n_rows),
        home_corners = get_int(dat_clean, "hc", n_rows),
        away_corners = get_int(dat_clean, "ac", n_rows),
        home_fouls = get_int(dat_clean, "hf", n_rows),
        away_fouls = get_int(dat_clean, "af", n_rows),
        home_yellow_cards = get_int(dat_clean, "hy", n_rows),
        away_yellow_cards = get_int(dat_clean, "ay", n_rows),
        home_red_cards = get_int(dat_clean, "hr", n_rows),
        away_red_cards = get_int(dat_clean, "ar", n_rows)
    ) |>
        dplyr::mutate(
            match_result = dplyr::case_when(
                home_score > away_score ~ "H",
                home_score == away_score ~ "D",
                home_score < away_score ~ "A",
                TRUE ~ NA_character_
            ),
            result_class = dplyr::case_when(
                match_result == "H" ~ 1L,
                match_result == "D" ~ 0L,
                match_result == "A" ~ -1L,
                TRUE ~ NA_integer_
            ),
            home_win = as.integer(match_result == "H"),
            draw = as.integer(match_result == "D"),
            away_win = as.integer(match_result == "A"),
            goal_difference = home_score - away_score,
            total_goals = home_score + away_score,
            neutral = as.logical(NA)
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

    if (nrow(match_core) == 0L) {
        return(list(core = match_core, odds = tibble::tibble()))
    }

    dat_kept <- dat_clean[match_core$source_row_index, , drop = FALSE]
    n_kept <- nrow(dat_kept)

    avg_home_odds <- get_num(dat_kept, "avg_h", n_kept)
    avg_draw_odds <- get_num(dat_kept, "avg_d", n_kept)
    avg_away_odds <- get_num(dat_kept, "avg_a", n_kept)
    market_probs <- compute_market_probs(
        avg_home_odds,
        avg_draw_odds,
        avg_away_odds,
        n_kept
    )

    odds_wide <- tibble::tibble(
        source_match_id = match_core$source_match_id,
        raw_file = match_core$raw_file,
        source_league_code = match_core$source_league_code,
        source_season_code = match_core$source_season_code,
        date = match_core$date,
        home_team = match_core$home_team,
        away_team = match_core$away_team,
        avg_home_odds = avg_home_odds,
        avg_draw_odds = avg_draw_odds,
        avg_away_odds = avg_away_odds,
        max_home_odds = get_num(dat_kept, "max_h", n_kept),
        max_draw_odds = get_num(dat_kept, "max_d", n_kept),
        max_away_odds = get_num(dat_kept, "max_a", n_kept),
        closing_home_odds = get_num(dat_kept, "avg_ch", n_kept),
        closing_draw_odds = get_num(dat_kept, "avg_cd", n_kept),
        closing_away_odds = get_num(dat_kept, "avg_ca", n_kept)
    )

    odds_wide <- dplyr::bind_cols(odds_wide, market_probs)

    preserved_odds <- extract_preserved_odds_cols(dat_kept, n_kept)

    if (ncol(preserved_odds) > 0L) {
        odds_wide <- dplyr::bind_cols(odds_wide, preserved_odds)
    }

    match_core_out <- match_core |>
        dplyr::select(-source_row_index)

    list(core = match_core_out, odds = odds_wide)
}

fd_uk_parsed <- purrr::map(fd_uk_files, read_one_fd_uk_file)

football_data_uk_matches_core <- purrr::map_dfr(fd_uk_parsed, "core")
football_data_uk_odds_wide <- purrr::map_dfr(fd_uk_parsed, "odds")

football_data_uk_matches <- football_data_uk_matches_core

matches_out <- file.path(PROCESSED_DIR, "football_data_uk_matches.csv")
matches_core_out <- file.path(PROCESSED_DIR, "football_data_uk_matches_core.csv")
odds_wide_out <- file.path(PROCESSED_DIR, "football_data_uk_odds_wide.csv")
summary_out <- file.path(VALIDATION_DIR, "football_data_uk_cleaning_summary.csv")

readr::write_csv(football_data_uk_matches, matches_out)
readr::write_csv(football_data_uk_matches_core, matches_core_out)
readr::write_csv(football_data_uk_odds_wide, odds_wide_out)

saveRDS(
    football_data_uk_matches,
    file.path(PROCESSED_DIR, "football_data_uk_matches.rds")
)

cleaning_summary <- tibble::tibble(
    metric = c(
        "raw_files",
        "matches_core_rows",
        "odds_wide_rows",
        "distinct_source_match_id_core",
        "distinct_source_match_id_odds",
        "rows_with_avg_home_odds",
        "rows_with_any_preserved_bookmaker_odds"
    ),
    value = c(
        length(fd_uk_files),
        nrow(football_data_uk_matches_core),
        nrow(football_data_uk_odds_wide),
        dplyr::n_distinct(football_data_uk_matches_core$source_match_id),
        dplyr::n_distinct(football_data_uk_odds_wide$source_match_id),
        sum(!is.na(football_data_uk_odds_wide$avg_home_odds)),
        sum(
            rowSums(
                !is.na(
                    football_data_uk_odds_wide |>
                        dplyr::select(dplyr::any_of(
                            setdiff(
                                names(football_data_uk_odds_wide),
                                c(
                                    "source_match_id",
                                    "raw_file",
                                    "source_league_code",
                                    "source_season_code",
                                    "date",
                                    "home_team",
                                    "away_team",
                                    "avg_home_odds",
                                    "avg_draw_odds",
                                    "avg_away_odds",
                                    "max_home_odds",
                                    "max_draw_odds",
                                    "max_away_odds",
                                    "closing_home_odds",
                                    "closing_draw_odds",
                                    "closing_away_odds",
                                    "home_implied_prob",
                                    "draw_implied_prob",
                                    "away_implied_prob",
                                    "market_overround"
                                )
                            )
                        ))
                ),
                na.rm = TRUE
            ) > 0L
        )
    )
)

readr::write_csv(cleaning_summary, summary_out)

message("Done.")
message("football-data.co.uk core rows: ", nrow(football_data_uk_matches_core))
message("football-data.co.uk odds rows: ", nrow(football_data_uk_odds_wide))
message("Output: ", matches_out)
message("Output: ", matches_core_out)
message("Output: ", odds_wide_out)
message("Output: ", summary_out)
