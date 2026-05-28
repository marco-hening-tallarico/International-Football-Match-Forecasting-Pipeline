# ============================================================
# International Football Results: Data Checks, Graphs, Validation
# Author: generated for R ML Sport workflow
#
# Input expected:
#   international_results.csv
#
# Outputs:
#   outputs/
#     checks/*.csv
#     plots/*.png
#     validation/*.csv
#     validation/*.rds
#
# Notes:
#   - Validation is chronological to mimic deployment.
#   - Team-strength features are created using only matches played
#     before the current match to avoid target leakage.
# ============================================================

# -----------------------------
# 0. Setup
# -----------------------------

required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "lubridate",
  "stringr", "purrr", "nnet", "scales", "forcats"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required R packages: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them with:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(purrr)
library(nnet)
library(scales)
library(forcats)

set.seed(42)

input_file <- "data/processed/international_results.csv"

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Could not find ", input_file, " in the working directory.\n",
      "Set your working directory to the folder containing the CSV, or update input_file."
    ),
    call. = FALSE
  )
}

output_dir <- "outputs"
checks_dir <- file.path(output_dir, "checks")
plots_dir <- file.path(output_dir, "plots")
validation_dir <- file.path(output_dir, "validation")

dir.create(output_dir, showWarnings = FALSE)
dir.create(checks_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(validation_dir, showWarnings = FALSE, recursive = TRUE)

save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(plots_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

safe_divide <- function(x, y) {
  ifelse(is.na(y) | y == 0, NA_real_, x / y)
}

# -----------------------------
# 1. Load and type data
# -----------------------------

raw <- readr::read_csv(input_file, show_col_types = FALSE)

expected_cols <- c(
  "source", "raw_file", "source_match_id", "date", "season", "competition",
  "home_team", "away_team", "home_score", "away_score", "match_result",
  "result_class", "home_win", "draw", "away_win", "goal_difference",
  "total_goals", "neutral", "tournament", "city", "country"
)

schema_check <- tibble(
  expected_column = expected_cols,
  present = expected_cols %in% names(raw)
)

extra_cols <- setdiff(names(raw), expected_cols)

readr::write_csv(schema_check, file.path(checks_dir, "schema_expected_columns.csv"))
readr::write_csv(tibble(extra_column = extra_cols), file.path(checks_dir, "schema_extra_columns.csv"))

missing_required <- expected_cols[!expected_cols %in% names(raw)]

if (length(missing_required) > 0) {
  stop(
    paste0(
      "Missing required columns: ",
      paste(missing_required, collapse = ", ")
    ),
    call. = FALSE
  )
}

df <- raw %>%
  mutate(
    date = lubridate::ymd(date),
    season = as.integer(season),
    home_score = as.integer(home_score),
    away_score = as.integer(away_score),
    total_goals = as.integer(total_goals),
    goal_difference = as.integer(goal_difference),
    home_win = as.integer(home_win),
    draw = as.integer(draw),
    away_win = as.integer(away_win),
    neutral = as.logical(neutral),
    result_label = case_when(
      home_score > away_score ~ "Home win",
      home_score == away_score ~ "Draw",
      home_score < away_score ~ "Away win",
      TRUE ~ NA_character_
    ),
    result_label = factor(result_label, levels = c("Home win", "Draw", "Away win"))
  ) %>%
  arrange(date, source_match_id)

# -----------------------------
# 2. Data quality checks
# -----------------------------

basic_summary <- tibble(
  n_rows = nrow(df),
  n_columns = ncol(df),
  min_date = as.character(min(df$date, na.rm = TRUE)),
  max_date = as.character(max(df$date, na.rm = TRUE)),
  n_home_teams = n_distinct(df$home_team),
  n_away_teams = n_distinct(df$away_team),
  n_tournaments = n_distinct(df$tournament),
  n_countries = n_distinct(df$country)
)

missing_summary <- df %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(pct_missing = n_missing / nrow(df)) %>%
  arrange(desc(n_missing))

duplicate_matches <- df %>%
  count(date, home_team, away_team, home_score, away_score, tournament, city, country, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n), date)

invalid_values <- df %>%
  transmute(
    row_id = row_number(),
    date,
    home_team,
    away_team,
    home_score,
    away_score,
    total_goals,
    goal_difference,
    result_class,
    home_win,
    draw,
    away_win,
    match_result,
    bad_date = is.na(date),
    bad_score = is.na(home_score) | is.na(away_score) | home_score < 0 | away_score < 0,
    bad_total_goals = total_goals != home_score + away_score,
    bad_goal_difference = goal_difference != home_score - away_score,
    bad_one_hot_result = home_win + draw + away_win != 1,
    bad_home_win = home_win != as.integer(home_score > away_score),
    bad_draw = draw != as.integer(home_score == away_score),
    bad_away_win = away_win != as.integer(home_score < away_score)
  ) %>%
  filter(
    bad_date | bad_score | bad_total_goals | bad_goal_difference |
      bad_one_hot_result | bad_home_win | bad_draw | bad_away_win
  )

target_distribution <- df %>%
  count(result_label, name = "n") %>%
  mutate(pct = n / sum(n))

score_extremes <- df %>%
  arrange(desc(total_goals)) %>%
  select(date, tournament, home_team, away_team, home_score, away_score, total_goals, city, country) %>%
  slice_head(n = 50)

team_match_counts <- bind_rows(
  df %>% transmute(team = home_team),
  df %>% transmute(team = away_team)
) %>%
  count(team, name = "matches") %>%
  arrange(desc(matches))

readr::write_csv(basic_summary, file.path(checks_dir, "basic_summary.csv"))
readr::write_csv(missing_summary, file.path(checks_dir, "missing_summary.csv"))
readr::write_csv(duplicate_matches, file.path(checks_dir, "duplicate_match_candidates.csv"))
readr::write_csv(invalid_values, file.path(checks_dir, "invalid_value_checks.csv"))
readr::write_csv(target_distribution, file.path(checks_dir, "target_distribution.csv"))
readr::write_csv(score_extremes, file.path(checks_dir, "score_extremes_top_50.csv"))
readr::write_csv(team_match_counts, file.path(checks_dir, "team_match_counts.csv"))

# -----------------------------
# 3. Graphing / EDA
# -----------------------------

matches_by_year <- df %>%
  mutate(year = year(date)) %>%
  count(year, name = "matches") %>%
  filter(!is.na(year))

p_matches_by_year <- ggplot(matches_by_year, aes(x = year, y = matches)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "International Matches by Year",
    x = "Year",
    y = "Matches"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_matches_by_year, "matches_by_year.png")

p_result_distribution <- df %>%
  count(result_label) %>%
  ggplot(aes(x = result_label, y = n)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Match Result Distribution",
    x = "Result",
    y = "Matches"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_result_distribution, "result_distribution.png")

p_total_goals <- ggplot(df, aes(x = total_goals)) +
  geom_histogram(binwidth = 1, boundary = -0.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total Goals per Match",
    x = "Total goals",
    y = "Matches"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_total_goals, "total_goals_distribution.png")

home_advantage_by_year <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    matches = n(),
    home_win_rate = mean(home_win, na.rm = TRUE),
    draw_rate = mean(draw, na.rm = TRUE),
    away_win_rate = mean(away_win, na.rm = TRUE),
    avg_goal_difference = mean(goal_difference, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(matches >= 20)

readr::write_csv(home_advantage_by_year, file.path(checks_dir, "home_advantage_by_year.csv"))

p_home_advantage <- ggplot(home_advantage_by_year, aes(x = year, y = home_win_rate)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Home Win Rate by Year",
    subtitle = "Years with at least 20 matches",
    x = "Year",
    y = "Home win rate"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_home_advantage, "home_win_rate_by_year.png")

top_tournaments <- df %>%
  count(tournament, sort = TRUE) %>%
  slice_head(n = 20) %>%
  mutate(tournament = forcats::fct_reorder(tournament, n))

p_top_tournaments <- ggplot(top_tournaments, aes(x = tournament, y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 20 Tournaments by Match Count",
    x = "Tournament",
    y = "Matches"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_top_tournaments, "top_20_tournaments.png", width = 11, height = 8)

neutral_summary <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    matches = n(),
    neutral_rate = mean(neutral, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(matches >= 20)

readr::write_csv(neutral_summary, file.path(checks_dir, "neutral_rate_by_year.csv"))

p_neutral_rate <- ggplot(neutral_summary, aes(x = year, y = neutral_rate)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Neutral-Site Match Rate by Year",
    subtitle = "Years with at least 20 matches",
    x = "Year",
    y = "Neutral-site rate"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_neutral_rate, "neutral_rate_by_year.png")

score_heatmap <- df %>%
  filter(home_score <= 8, away_score <= 8) %>%
  count(home_score, away_score, name = "matches")

p_score_heatmap <- ggplot(score_heatmap, aes(x = home_score, y = away_score, fill = matches)) +
  geom_tile() +
  geom_text(aes(label = matches), size = 3) +
  scale_fill_continuous(labels = comma) +
  labs(
    title = "Scoreline Frequency Heatmap",
    subtitle = "Scores capped to 0-8 for readability",
    x = "Home score",
    y = "Away score",
    fill = "Matches"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_score_heatmap, "scoreline_heatmap_0_to_8.png", width = 8, height = 7)

# -----------------------------
# 4. Leakage-safe feature engineering
# -----------------------------
# Unit of observation: one match.
# Target: result_label = Home win / Draw / Away win.
# Prediction horizon: pre-match.
#
# All team-form features below are calculated before updating each team's
# state with the current match result.

make_prematch_features <- function(data) {
  data <- data %>%
    arrange(date, source_match_id) %>%
    mutate(row_id = row_number())

  teams <- sort(unique(c(data$home_team, data$away_team)))

  state <- tibble(
    team = teams,
    matches_played = 0L,
    points = 0,
    goals_for = 0,
    goals_against = 0,
    wins = 0L,
    draws = 0L,
    losses = 0L
  )

  rows <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    match <- data[i, ]

    h <- match$home_team
    a <- match$away_team

    hs <- match$home_score
    as <- match$away_score

    h_state <- state[state$team == h, ]
    a_state <- state[state$team == a, ]

    h_ppg <- safe_divide(h_state$points, h_state$matches_played)
    a_ppg <- safe_divide(a_state$points, a_state$matches_played)

    h_gf_pg <- safe_divide(h_state$goals_for, h_state$matches_played)
    a_gf_pg <- safe_divide(a_state$goals_for, a_state$matches_played)

    h_ga_pg <- safe_divide(h_state$goals_against, h_state$matches_played)
    a_ga_pg <- safe_divide(a_state$goals_against, a_state$matches_played)

    h_win_rate <- safe_divide(h_state$wins, h_state$matches_played)
    a_win_rate <- safe_divide(a_state$wins, a_state$matches_played)

    rows[[i]] <- match %>%
      mutate(
        home_matches_before = h_state$matches_played,
        away_matches_before = a_state$matches_played,
        home_ppg_before = h_ppg,
        away_ppg_before = a_ppg,
        ppg_diff_before = h_ppg - a_ppg,
        home_gf_pg_before = h_gf_pg,
        away_gf_pg_before = a_gf_pg,
        gf_pg_diff_before = h_gf_pg - a_gf_pg,
        home_ga_pg_before = h_ga_pg,
        away_ga_pg_before = a_ga_pg,
        ga_pg_diff_before = h_ga_pg - a_ga_pg,
        home_win_rate_before = h_win_rate,
        away_win_rate_before = a_win_rate,
        win_rate_diff_before = h_win_rate - a_win_rate,
        home_experience_log = log1p(h_state$matches_played),
        away_experience_log = log1p(a_state$matches_played),
        experience_log_diff = log1p(h_state$matches_played) - log1p(a_state$matches_played)
      )

    h_points <- ifelse(hs > as, 3, ifelse(hs == as, 1, 0))
    a_points <- ifelse(as > hs, 3, ifelse(hs == as, 1, 0))

    state[state$team == h, c("matches_played", "points", "goals_for", "goals_against", "wins", "draws", "losses")] <-
      state[state$team == h, c("matches_played", "points", "goals_for", "goals_against", "wins", "draws", "losses")] +
      tibble(
        matches_played = 1L,
        points = h_points,
        goals_for = hs,
        goals_against = as,
        wins = as.integer(hs > as),
        draws = as.integer(hs == as),
        losses = as.integer(hs < as)
      )

    state[state$team == a, c("matches_played", "points", "goals_for", "goals_against", "wins", "draws", "losses")] <-
      state[state$team == a, c("matches_played", "points", "goals_for", "goals_against", "wins", "draws", "losses")] +
      tibble(
        matches_played = 1L,
        points = a_points,
        goals_for = as,
        goals_against = hs,
        wins = as.integer(as > hs),
        draws = as.integer(hs == as),
        losses = as.integer(as < hs)
      )
  }

  bind_rows(rows)
}

model_df <- make_prematch_features(df) %>%
  mutate(
    neutral = as.integer(neutral),
    decade = floor(year(date) / 10) * 10
  )

readr::write_csv(
  model_df %>%
    select(
      date, home_team, away_team, result_label,
      home_matches_before, away_matches_before,
      ppg_diff_before, gf_pg_diff_before, ga_pg_diff_before,
      win_rate_diff_before, experience_log_diff, neutral
    ),
  file.path(validation_dir, "prematch_modeling_dataset.csv")
)

# -----------------------------
# 5. Chronological validation split
# -----------------------------

model_ready <- model_df %>%
  filter(
    !is.na(result_label),
    !is.na(date),
    home_matches_before >= 5,
    away_matches_before >= 5
  ) %>%
  mutate(
    result_label = factor(result_label, levels = c("Home win", "Draw", "Away win")),
    neutral = as.integer(neutral)
  )

split_index <- floor(0.80 * nrow(model_ready))

train <- model_ready[seq_len(split_index), ]
test <- model_ready[(split_index + 1):nrow(model_ready), ]

split_summary <- tibble(
  split = c("train", "test"),
  rows = c(nrow(train), nrow(test)),
  min_date = c(as.character(min(train$date)), as.character(min(test$date))),
  max_date = c(as.character(max(train$date)), as.character(max(test$date)))
)

readr::write_csv(split_summary, file.path(validation_dir, "chronological_split_summary.csv"))

# -----------------------------
# 6. Baselines
# -----------------------------

# Baseline 1: always predict the most common training class.
majority_class <- train %>%
  count(result_label, sort = TRUE) %>%
  slice(1) %>%
  pull(result_label) %>%
  as.character()

baseline_majority <- test %>%
  transmute(
    date,
    home_team,
    away_team,
    actual = result_label,
    pred = factor(majority_class, levels = levels(result_label))
  )

# Baseline 2: training class-prior probabilities, same probabilities for every match.
class_priors <- train %>%
  count(result_label, name = "n") %>%
  mutate(prob = n / sum(n))

baseline_prior_probs <- test %>%
  transmute(
    date,
    home_team,
    away_team,
    actual = result_label,
    pred = factor(class_priors$result_label[which.max(class_priors$prob)], levels = levels(result_label)),
    prob_home_win = class_priors$prob[class_priors$result_label == "Home win"],
    prob_draw = class_priors$prob[class_priors$result_label == "Draw"],
    prob_away_win = class_priors$prob[class_priors$result_label == "Away win"]
  )

# -----------------------------
# 7. Multinomial logistic model
# -----------------------------

feature_cols <- c(
  "neutral",
  "home_matches_before",
  "away_matches_before",
  "ppg_diff_before",
  "gf_pg_diff_before",
  "ga_pg_diff_before",
  "win_rate_diff_before",
  "experience_log_diff"
)

train_x <- train %>%
  select(result_label, all_of(feature_cols)) %>%
  mutate(across(all_of(feature_cols), ~replace_na(.x, 0)))

test_x <- test %>%
  select(date, home_team, away_team, result_label, all_of(feature_cols)) %>%
  mutate(across(all_of(feature_cols), ~replace_na(.x, 0)))

multinom_formula <- as.formula(
  paste("result_label ~", paste(feature_cols, collapse = " + "))
)

multinom_fit <- nnet::multinom(
  formula = multinom_formula,
  data = train_x,
  trace = FALSE,
  maxit = 300
)

saveRDS(multinom_fit, file.path(validation_dir, "multinomial_logistic_model.rds"))

model_probs <- as.data.frame(predict(multinom_fit, newdata = test_x, type = "probs"))

# nnet can return a vector if there are only two classes. This dataset has three,
# but this guard keeps the script safer.
if (!all(c("Home win", "Draw", "Away win") %in% names(model_probs))) {
  stop("Expected probability columns were not returned by nnet::multinom.", call. = FALSE)
}

model_predictions <- test_x %>%
  bind_cols(
    model_probs %>%
      rename(
        prob_home_win = `Home win`,
        prob_draw = Draw,
        prob_away_win = `Away win`
      )
  ) %>%
  rowwise() %>%
  mutate(
    pred = factor(
      c("Home win", "Draw", "Away win")[which.max(c(prob_home_win, prob_draw, prob_away_win))],
      levels = levels(result_label)
    )
  ) %>%
  ungroup() %>%
  transmute(
    date,
    home_team,
    away_team,
    actual = result_label,
    pred,
    prob_home_win,
    prob_draw,
    prob_away_win
  )

readr::write_csv(model_predictions, file.path(validation_dir, "multinomial_logistic_predictions.csv"))

# -----------------------------
# 8. Metrics
# -----------------------------

accuracy_score <- function(actual, pred) {
  mean(actual == pred, na.rm = TRUE)
}

multiclass_log_loss <- function(actual, prob_home_win, prob_draw, prob_away_win, eps = 1e-15) {
  actual_chr <- as.character(actual)

  p <- case_when(
    actual_chr == "Home win" ~ prob_home_win,
    actual_chr == "Draw" ~ prob_draw,
    actual_chr == "Away win" ~ prob_away_win,
    TRUE ~ NA_real_
  )

  p <- pmin(pmax(p, eps), 1 - eps)

  -mean(log(p), na.rm = TRUE)
}

brier_multiclass <- function(actual, prob_home_win, prob_draw, prob_away_win) {
  y_home <- as.integer(actual == "Home win")
  y_draw <- as.integer(actual == "Draw")
  y_away <- as.integer(actual == "Away win")

  mean(
    (prob_home_win - y_home)^2 +
      (prob_draw - y_draw)^2 +
      (prob_away_win - y_away)^2,
    na.rm = TRUE
  )
}

metrics <- bind_rows(
  tibble(
    model = "Majority class baseline",
    accuracy = accuracy_score(baseline_majority$actual, baseline_majority$pred),
    log_loss = NA_real_,
    brier = NA_real_
  ),
  tibble(
    model = "Class-prior probability baseline",
    accuracy = accuracy_score(baseline_prior_probs$actual, baseline_prior_probs$pred),
    log_loss = multiclass_log_loss(
      baseline_prior_probs$actual,
      baseline_prior_probs$prob_home_win,
      baseline_prior_probs$prob_draw,
      baseline_prior_probs$prob_away_win
    ),
    brier = brier_multiclass(
      baseline_prior_probs$actual,
      baseline_prior_probs$prob_home_win,
      baseline_prior_probs$prob_draw,
      baseline_prior_probs$prob_away_win
    )
  ),
  tibble(
    model = "Multinomial logistic regression",
    accuracy = accuracy_score(model_predictions$actual, model_predictions$pred),
    log_loss = multiclass_log_loss(
      model_predictions$actual,
      model_predictions$prob_home_win,
      model_predictions$prob_draw,
      model_predictions$prob_away_win
    ),
    brier = brier_multiclass(
      model_predictions$actual,
      model_predictions$prob_home_win,
      model_predictions$prob_draw,
      model_predictions$prob_away_win
    )
  )
)

readr::write_csv(metrics, file.path(validation_dir, "model_metrics.csv"))

confusion_matrix <- model_predictions %>%
  count(actual, pred, name = "n") %>%
  group_by(actual) %>%
  mutate(row_pct = n / sum(n)) %>%
  ungroup()

readr::write_csv(confusion_matrix, file.path(validation_dir, "confusion_matrix_multinomial_logistic.csv"))

p_confusion <- ggplot(confusion_matrix, aes(x = pred, y = actual, fill = row_pct)) +
  geom_tile() +
  geom_text(aes(label = paste0(n, "\n", percent(row_pct, accuracy = 0.1))), size = 3.5) +
  scale_fill_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Confusion Matrix: Multinomial Logistic Regression",
    subtitle = "Rows normalized by actual class",
    x = "Predicted",
    y = "Actual",
    fill = "Row %"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_confusion, "confusion_matrix_multinomial_logistic.png", width = 8, height = 6)

# Calibration table by predicted confidence bucket.
calibration <- model_predictions %>%
  mutate(
    pred_prob = pmax(prob_home_win, prob_draw, prob_away_win),
    correct = as.integer(actual == pred),
    confidence_bucket = cut(
      pred_prob,
      breaks = seq(0, 1, by = 0.1),
      include.lowest = TRUE
    )
  ) %>%
  group_by(confidence_bucket) %>%
  summarise(
    n = n(),
    avg_predicted_confidence = mean(pred_prob, na.rm = TRUE),
    empirical_accuracy = mean(correct, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(calibration, file.path(validation_dir, "calibration_by_confidence_bucket.csv"))

p_calibration <- ggplot(calibration, aes(x = avg_predicted_confidence, y = empirical_accuracy, size = n)) +
  geom_point(alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Calibration Check by Confidence Bucket",
    x = "Average predicted confidence",
    y = "Empirical accuracy",
    size = "Matches"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_calibration, "calibration_by_confidence_bucket.png", width = 8, height = 6)

# Coefficients for interpretability.
coef_table <- summary(multinom_fit)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column("class_vs_reference") %>%
  pivot_longer(
    cols = -class_vs_reference,
    names_to = "feature",
    values_to = "coefficient"
  ) %>%
  arrange(class_vs_reference, desc(abs(coefficient)))

readr::write_csv(coef_table, file.path(validation_dir, "multinomial_logistic_coefficients.csv"))

p_coef <- coef_table %>%
  filter(feature != "(Intercept)") %>%
  group_by(class_vs_reference) %>%
  slice_max(order_by = abs(coefficient), n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(feature = forcats::fct_reorder(feature, coefficient)) %>%
  ggplot(aes(x = feature, y = coefficient)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~class_vs_reference, scales = "free_y") +
  labs(
    title = "Largest Multinomial Logistic Coefficients",
    subtitle = "Compared with the reference class",
    x = "Feature",
    y = "Coefficient"
  ) +
  theme_minimal(base_size = 12)

save_plot(p_coef, "multinomial_logistic_coefficients.png", width = 11, height = 7)

# -----------------------------
# 9. Simple walk-forward backtest by season
# -----------------------------
# Expanding-window season backtest:
# train on all prior seasons, test on the next season.
# This is more realistic than one static split, but early seasons with
# too little history are skipped.

min_train_rows <- 1000

walk_forward_results <- list()

seasons <- sort(unique(model_ready$season))

for (s in seasons) {
  train_s <- model_ready %>%
    filter(season < s) %>%
    mutate(across(all_of(feature_cols), ~replace_na(.x, 0)))

  test_s <- model_ready %>%
    filter(season == s) %>%
    mutate(across(all_of(feature_cols), ~replace_na(.x, 0)))

  if (nrow(train_s) < min_train_rows || nrow(test_s) == 0) {
    next
  }

  # Require all outcome classes in training.
  if (n_distinct(train_s$result_label) < 3) {
    next
  }

  fit_s <- nnet::multinom(
    multinom_formula,
    data = train_s %>% select(result_label, all_of(feature_cols)),
    trace = FALSE,
    maxit = 300
  )

  probs_s <- as.data.frame(predict(fit_s, newdata = test_s, type = "probs"))

  preds_s <- test_s %>%
    bind_cols(
      probs_s %>%
        rename(
          prob_home_win = `Home win`,
          prob_draw = Draw,
          prob_away_win = `Away win`
        )
    ) %>%
    rowwise() %>%
    mutate(
      pred = factor(
        c("Home win", "Draw", "Away win")[which.max(c(prob_home_win, prob_draw, prob_away_win))],
        levels = levels(result_label)
      )
    ) %>%
    ungroup()

  walk_forward_results[[as.character(s)]] <- tibble(
    season = s,
    test_rows = nrow(preds_s),
    accuracy = accuracy_score(preds_s$result_label, preds_s$pred),
    log_loss = multiclass_log_loss(
      preds_s$result_label,
      preds_s$prob_home_win,
      preds_s$prob_draw,
      preds_s$prob_away_win
    ),
    brier = brier_multiclass(
      preds_s$result_label,
      preds_s$prob_home_win,
      preds_s$prob_draw,
      preds_s$prob_away_win
    )
  )
}

walk_forward_metrics <- bind_rows(walk_forward_results)

readr::write_csv(walk_forward_metrics, file.path(validation_dir, "walk_forward_metrics_by_season.csv"))

if (nrow(walk_forward_metrics) > 0) {
  p_walk_forward_logloss <- ggplot(walk_forward_metrics, aes(x = season, y = log_loss)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    labs(
      title = "Walk-Forward Validation Log Loss by Season",
      x = "Test season",
      y = "Log loss"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_walk_forward_logloss, "walk_forward_log_loss_by_season.png")

  p_walk_forward_accuracy <- ggplot(walk_forward_metrics, aes(x = season, y = accuracy)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Walk-Forward Validation Accuracy by Season",
      x = "Test season",
      y = "Accuracy"
    ) +
    theme_minimal(base_size = 12)

  save_plot(p_walk_forward_accuracy, "walk_forward_accuracy_by_season.png")
}

# -----------------------------
# 10. Write final run manifest
# -----------------------------

manifest <- tibble(
  artifact = c(
    "basic_summary.csv",
    "missing_summary.csv",
    "duplicate_match_candidates.csv",
    "invalid_value_checks.csv",
    "target_distribution.csv",
    "score_extremes_top_50.csv",
    "team_match_counts.csv",
    "prematch_modeling_dataset.csv",
    "chronological_split_summary.csv",
    "model_metrics.csv",
    "confusion_matrix_multinomial_logistic.csv",
    "calibration_by_confidence_bucket.csv",
    "multinomial_logistic_coefficients.csv",
    "walk_forward_metrics_by_season.csv",
    "plots/*.png",
    "multinomial_logistic_model.rds"
  ),
  location = c(
    rep(checks_dir, 7),
    validation_dir,
    validation_dir,
    validation_dir,
    validation_dir,
    validation_dir,
    validation_dir,
    validation_dir,
    plots_dir,
    validation_dir
  )
)

readr::write_csv(manifest, file.path(output_dir, "run_manifest.csv"))

message("Done. Outputs saved under: ", normalizePath(output_dir))
message("Key files:")
message(" - ", file.path(validation_dir, "model_metrics.csv"))
message(" - ", file.path(validation_dir, "walk_forward_metrics_by_season.csv"))
message(" - ", file.path(plots_dir, "calibration_by_confidence_bucket.png"))
message(" - ", file.path(plots_dir, "confusion_matrix_multinomial_logistic.png"))
