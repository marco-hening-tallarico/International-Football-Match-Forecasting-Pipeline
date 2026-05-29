# 10_plot_international_results_validation.R
#
# Basic EDA plots for international results: volume by year, score
# distributions, train/test split view, and duplicate-check summaries.
#
# Reads: data/processed/international_results.csv
#
# Writes: reports/figures/international_results/*

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

fail <- function(message) {
    stop(message, call. = FALSE)
}

processed_path <- file.path(PROCESSED_DIR, "international_results.csv")

if (!file.exists(processed_path)) {
    fail("Missing processed international_results.csv. Run src/08_clean_international_results.R first.")
}

# Output folder

graphs_dir <- file.path(REPORTS_FIGURES_DIR, "international_results")
fs::dir_create(graphs_dir)

# Load data

matches <- read_processed_csv(processed_path) |>
    dplyr::mutate(
        date = as.Date(date),
        season = as.integer(season),
        home_score = as.integer(home_score),
        away_score = as.integer(away_score),
        total_goals = as.integer(total_goals),
        goal_difference = as.integer(goal_difference),
        neutral = as.logical(neutral),
        result_class = as.integer(result_class),
        match_result = as.character(match_result),
        year = lubridate::year(date)
    )

# Basic preprocessing / duplicate checks before plotting

duplicate_source_ids <- matches |>
    dplyr::count(source_match_id, name = "n") |>
    dplyr::filter(n > 1L)

duplicate_exact_matches <- matches |>
    dplyr::count(
        date,
        home_team,
        away_team,
        home_score,
        away_score,
        tournament,
        city,
        country,
        neutral,
        name = "n"
    ) |>
    dplyr::filter(n > 1L)

duplicate_fixture_keys <- matches |>
    dplyr::count(
        date,
        home_team,
        away_team,
        tournament,
        name = "n"
    ) |>
    dplyr::filter(n > 1L)

team_same_day <- matches |>
    dplyr::select(source_match_id, date, home_team, away_team) |>
    tidyr::pivot_longer(
        cols = c(home_team, away_team),
        names_to = "side",
        values_to = "team"
    ) |>
    dplyr::count(date, team, name = "matches_on_date") |>
    dplyr::filter(matches_on_date > 1L)

duplicate_summary <- tibble::tibble(
    check = c(
        "duplicate_source_match_id",
        "exact_duplicate_match_rows",
        "same_date_home_away_tournament",
        "same_team_multiple_matches_same_day"
    ),
    rows_affected = c(
        nrow(duplicate_source_ids),
        nrow(duplicate_exact_matches),
        nrow(duplicate_fixture_keys),
        nrow(team_same_day)
    )
)

readr::write_csv(
    duplicate_summary,
    file.path(graphs_dir, "duplicate_check_summary.csv")
)

readr::write_csv(
    duplicate_source_ids,
    file.path(graphs_dir, "duplicate_source_match_ids.csv")
)

readr::write_csv(
    duplicate_exact_matches,
    file.path(graphs_dir, "exact_duplicate_match_rows.csv")
)

readr::write_csv(
    duplicate_fixture_keys,
    file.path(graphs_dir, "same_date_home_away_tournament_repeats.csv")
)

readr::write_csv(
    team_same_day,
    file.path(graphs_dir, "same_team_multiple_matches_same_day.csv")
)

# Basic modeling-safe split
# Chronological split avoids future information leaking backward.

split_year <- 2018L

matches <- matches |>
    dplyr::mutate(
        data_split = dplyr::if_else(year < split_year, "train", "test")
    )

train_matches <- matches |>
    dplyr::filter(data_split == "train")

test_matches <- matches |>
    dplyr::filter(data_split == "test")

split_summary <- matches |>
    dplyr::count(data_split, name = "matches") |>
    dplyr::mutate(prop = matches / sum(matches))

readr::write_csv(
    split_summary,
    file.path(graphs_dir, "train_test_split_summary.csv")
)

readr::write_csv(
    matches |>
        dplyr::select(source_match_id, date, home_team, away_team, tournament, data_split),
    file.path(graphs_dir, "international_results_train_test_split.csv")
)

# Plot theme

base_theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "gray30"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

save_plot <- function(plot, filename, width = 9, height = 6) {
    ggplot2::ggsave(
        filename = file.path(graphs_dir, filename),
        plot = plot,
        width = width,
        height = height,
        dpi = 300
    )
}

# 1. Matches by year
# Training data only for modeling-safe EDA.

p_matches_by_year <- train_matches |>
    dplyr::count(year, name = "matches") |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = matches)) +
    ggplot2::geom_col() +
    ggplot2::labs(
        title = "International matches by year",
        subtitle = paste0("Training data only: years before ", split_year),
        x = "Year",
        y = "Matches"
    ) +
    base_theme

save_plot(p_matches_by_year, "01_matches_by_year_train.png")

# 2. Match result distribution

p_result_distribution <- train_matches |>
    dplyr::count(match_result, name = "matches") |>
    dplyr::mutate(prop = matches / sum(matches)) |>
    ggplot2::ggplot(ggplot2::aes(x = match_result, y = prop)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
        title = "Match result distribution",
        subtitle = "Training data only",
        x = "Result",
        y = "Share of matches"
    ) +
    base_theme

save_plot(p_result_distribution, "02_result_distribution_train.png")

# 3. Total goals histogram

p_total_goals <- train_matches |>
    ggplot2::ggplot(ggplot2::aes(x = total_goals)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = -0.5) +
    ggplot2::labs(
        title = "Total goals per match",
        subtitle = "Training data only",
        x = "Total goals",
        y = "Matches"
    ) +
    base_theme

save_plot(p_total_goals, "03_total_goals_histogram_train.png")

# 4. Goal difference histogram
# Positive means home team scored more.

p_goal_difference <- train_matches |>
    ggplot2::ggplot(ggplot2::aes(x = goal_difference)) +
    ggplot2::geom_histogram(binwidth = 1, boundary = -0.5) +
    ggplot2::labs(
        title = "Goal difference distribution",
        subtitle = "Training data only; positive values favor home team",
        x = "Home goals minus away goals",
        y = "Matches"
    ) +
    base_theme

save_plot(p_goal_difference, "04_goal_difference_histogram_train.png")

# 5. Home score vs away score
# Sampled for readability, not leakage prevention.

set.seed(20260527)

score_scatter_n <- min(5000L, nrow(train_matches))

score_scatter_sample <- train_matches |>
    dplyr::slice_sample(n = score_scatter_n)

p_score_scatter <- score_scatter_sample |>
    ggplot2::ggplot(ggplot2::aes(x = home_score, y = away_score)) +
    ggplot2::geom_jitter(width = 0.15, height = 0.15, alpha = 0.25) +
    ggplot2::coord_equal() +
    ggplot2::labs(
        title = "Home score vs away score",
        subtitle = "Sample of training matches for readability",
        x = "Home score",
        y = "Away score"
    ) +
    base_theme

save_plot(p_score_scatter, "05_home_score_vs_away_score_train_sample.png")

# 6. Neutral-site distribution

p_neutral_distribution <- train_matches |>
    dplyr::count(neutral, name = "matches") |>
    dplyr::mutate(
        neutral_label = dplyr::if_else(neutral, "Neutral site", "Not neutral"),
        prop = matches / sum(matches)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = neutral_label, y = prop)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
        title = "Neutral-site distribution",
        subtitle = "Training data only",
        x = NULL,
        y = "Share of matches"
    ) +
    base_theme

save_plot(p_neutral_distribution, "06_neutral_site_distribution_train.png")

# 7. Top tournaments by match count

p_top_tournaments <- train_matches |>
    dplyr::count(tournament, name = "matches") |>
    dplyr::slice_max(matches, n = 15, with_ties = FALSE) |>
    dplyr::mutate(tournament = forcats::fct_reorder(tournament, matches)) |>
    ggplot2::ggplot(ggplot2::aes(x = tournament, y = matches)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
        title = "Top tournaments by match count",
        subtitle = "Training data only",
        x = NULL,
        y = "Matches"
    ) +
    base_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))

save_plot(p_top_tournaments, "07_top_tournaments_train.png")

# 8. Extreme scorelines
# These are not automatically errors; they are QA review cases.

extreme_scorelines <- matches |>
    dplyr::filter(total_goals > 15L | abs(goal_difference) > 10L) |>
    dplyr::arrange(dplyr::desc(total_goals), dplyr::desc(abs(goal_difference))) |>
    dplyr::select(
        source_match_id,
        date,
        home_team,
        away_team,
        home_score,
        away_score,
        tournament,
        city,
        country,
        neutral,
        total_goals,
        goal_difference,
        data_split
    )

readr::write_csv(
    extreme_scorelines,
    file.path(graphs_dir, "extreme_scorelines_for_review.csv")
)

p_extreme_scorelines <- extreme_scorelines |>
    dplyr::slice_head(n = 25L) |>
    dplyr::mutate(
        match_label = paste0(home_team, " ", home_score, "-", away_score, " ", away_team),
        match_label = forcats::fct_reorder(match_label, total_goals)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = match_label, y = total_goals)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
        title = "Largest scorelines for manual review",
        subtitle = "Full dataset QA; not a modeling feature decision",
        x = NULL,
        y = "Total goals"
    ) +
    base_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))

save_plot(p_extreme_scorelines, "08_extreme_scorelines_full_data.png", width = 11, height = 8)

# 9. Full-data coverage plot
# This is acceptable as pure data validation, not model EDA.

p_full_matches_by_year <- matches |>
    dplyr::count(year, name = "matches") |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = matches)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = split_year, linetype = "dashed") +
    ggplot2::labs(
        title = "International matches by year: full dataset",
        subtitle = paste0("Dashed line marks test-start year: ", split_year),
        x = "Year",
        y = "Matches"
    ) +
    base_theme

save_plot(p_full_matches_by_year, "09_matches_by_year_full_data_with_split.png")

# Final console summary

message("Validation plots saved to: ", graphs_dir)
message("Duplicate check summary saved to: ", file.path(graphs_dir, "duplicate_check_summary.csv"))
message("Train/test split summary saved to: ", file.path(graphs_dir, "train_test_split_summary.csv"))
message("Extreme scorelines saved to: ", file.path(graphs_dir, "extreme_scorelines_for_review.csv"))

print(duplicate_summary)
print(split_summary)