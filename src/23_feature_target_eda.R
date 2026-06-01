# 23_feature_target_eda.R
#
# Exploratory plots and summary tables for approved pre-match features and
# the H/D/A target in the international modeling table.
#
# Reads:
# - data/processed/international_modeling_table.csv
# - reports/tables/approved_feature_sets_final.R
#
# Writes:
# - reports/tables/eda_*.csv
# - reports/figures/eda_*.png

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
    library(ggplot2)
    library(forcats)
})

# Paths

model_path <- "data/processed/international_modeling_table.csv"
feature_set_path <- "reports/tables/approved_feature_sets_final.R"

tables_dir <- "reports/tables"
figures_dir <- "reports/figures"

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Load data and approved features

model_df <- read_csv(model_path, show_col_types = FALSE)

if (!file.exists(feature_set_path)) {
    stop(
        "Missing approved feature set file: ",
        feature_set_path,
        "\nRun: Rscript src/22_finalize_feature_review.R"
    )
}

source(feature_set_path, local = TRUE)

required_objects <- c(
    "safe_features",
    "careful_features",
    "model_features",
    "excluded_features",
    "review_features"
)

missing_objects <- setdiff(required_objects, ls())

if (length(missing_objects) > 0) {
    stop(
        "The approved feature set file is missing objects: ",
        paste(missing_objects, collapse = ", ")
    )
}

pending_review_features <- review_features[
    !is.na(review_features) & nzchar(review_features)
]

if (length(pending_review_features) > 0) {
    stop(
        "Feature review is not complete. Remaining review features: ",
        paste(pending_review_features, collapse = ", ")
    )
}

missing_model_features <- setdiff(model_features, names(model_df))

if (length(missing_model_features) > 0) {
    stop(
        "These approved model features are not in model_df: ",
        paste(missing_model_features, collapse = ", ")
    )
}

# Build canonical target
# Goal:
#   Create outcome as H / D / A.
#   Use result_class or match_result if available.
#   Fall back to home_win / draw / away_win indicator columns.
#
# Important:
#   Do not use case_when() to choose between whole columns based on scalar
#   column-existence checks. Newer dplyr warns about that. Use if/else first.

if ("result_class" %in% names(model_df)) {
    outcome_raw_vec <- as.character(model_df$result_class)
} else if ("match_result" %in% names(model_df)) {
    outcome_raw_vec <- as.character(model_df$match_result)
} else {
    outcome_raw_vec <- rep(NA_character_, nrow(model_df))
}

has_indicator_targets <- all(c("home_win", "draw", "away_win") %in% names(model_df))

model_df <- model_df |>
    mutate(
        outcome_raw = outcome_raw_vec,
        outcome_raw_clean = str_squish(str_to_lower(outcome_raw)),
        outcome = case_when(
            outcome_raw_clean %in% c(
                "h",
                "home",
                "home_win",
                "home win",
                "home_win_class",
                "home win class"
            ) ~ "H",

            outcome_raw_clean %in% c(
                "d",
                "draw",
                "tie"
            ) ~ "D",

            outcome_raw_clean %in% c(
                "a",
                "away",
                "away_win",
                "away win",
                "away_win_class",
                "away win class"
            ) ~ "A",

            has_indicator_targets & home_win == 1 ~ "H",
            has_indicator_targets & draw == 1 ~ "D",
            has_indicator_targets & away_win == 1 ~ "A",

            TRUE ~ NA_character_
        ),
        outcome = factor(outcome, levels = c("H", "D", "A"))
    )

target_construction_check <- model_df |>
    count(outcome_raw, outcome_raw_clean, outcome, sort = TRUE)

write_csv(
    target_construction_check,
    file.path(tables_dir, "eda_target_construction_check.csv")
)

target_na_prop <- mean(is.na(model_df$outcome))

if (target_na_prop == 1) {
    stop(
        "Target construction failed: outcome is 100% NA. Inspect reports/tables/eda_target_construction_check.csv"
    )
}

if (target_na_prop > 0.01) {
    warning(
        round(100 * target_na_prop, 2),
        "% of outcome values are NA. Inspect reports/tables/eda_target_construction_check.csv"
    )
}

# Identify date/split columns

date_col <- case_when(
    "date" %in% names(model_df) ~ "date",
    "match_date" %in% names(model_df) ~ "match_date",
    TRUE ~ NA_character_
)

split_col <- case_when(
    "data_split" %in% names(model_df) ~ "data_split",
    "split" %in% names(model_df) ~ "split",
    TRUE ~ NA_character_
)

if (!is.na(date_col)) {
    model_df <- model_df |>
        mutate(.eda_date = as.Date(.data[[date_col]]))
} else {
    model_df <- model_df |>
        mutate(.eda_date = as.Date(NA))
}

if (!is.na(split_col)) {
    model_df <- model_df |>
        mutate(.eda_split = as.character(.data[[split_col]]))
} else {
    model_df <- model_df |>
        mutate(.eda_split = NA_character_)
}

# Feature type groups

feature_classes <- tibble(
    column = model_features,
    r_class = map_chr(model_features, ~ paste(class(model_df[[.x]]), collapse = "/"))
)

numeric_features <- feature_classes |>
    filter(map_lgl(column, ~ is.numeric(model_df[[.x]]) || is.integer(model_df[[.x]]))) |>
    pull(column)

categorical_features <- setdiff(model_features, numeric_features)

# 1. Target balance

target_balance <- model_df |>
    filter(!is.na(outcome)) |>
    count(outcome, name = "n") |>
    mutate(prop = n / sum(n))

write_csv(
    target_balance,
    file.path(tables_dir, "eda_target_balance.csv")
)

p_target_balance <- ggplot(target_balance, aes(x = outcome, y = prop)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
        title = "Target balance",
        x = "Outcome",
        y = "Share of matches"
    )

ggsave(
    file.path(figures_dir, "eda_target_balance.png"),
    p_target_balance,
    width = 7,
    height = 5,
    dpi = 150
)

# 2. Target balance by split

if (!all(is.na(model_df$.eda_split))) {
    target_by_split <- model_df |>
        filter(!is.na(outcome)) |>
        count(.eda_split, outcome, name = "n") |>
        group_by(.eda_split) |>
        mutate(prop = n / sum(n)) |>
        ungroup()

    write_csv(
        target_by_split,
        file.path(tables_dir, "eda_target_balance_by_split.csv")
    )

    if (nrow(target_by_split) > 0) {
        p_target_by_split <- ggplot(
            target_by_split,
            aes(x = .eda_split, y = prop, fill = outcome)
        ) +
            geom_col(position = "dodge") +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(
                title = "Target balance by split",
                x = "Split",
                y = "Share of matches",
                fill = "Outcome"
            )

        ggsave(
            file.path(figures_dir, "eda_target_balance_by_split.png"),
            p_target_by_split,
            width = 8,
            height = 5,
            dpi = 150
        )
    }
}

# 3. Target trend by season

if ("season" %in% names(model_df)) {
    target_by_season <- model_df |>
        filter(!is.na(season), !is.na(outcome)) |>
        count(season, outcome, name = "n") |>
        group_by(season) |>
        mutate(
            season_n = sum(n),
            prop = n / season_n
        ) |>
        ungroup()

    write_csv(
        target_by_season,
        file.path(tables_dir, "eda_target_balance_by_season.csv")
    )

    if (nrow(target_by_season) > 0) {
        p_target_by_season <- ggplot(
            target_by_season,
            aes(x = season, y = prop, color = outcome)
        ) +
            geom_line() +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(
                title = "Outcome rates by season",
                x = "Season",
                y = "Outcome rate",
                color = "Outcome"
            )

        ggsave(
            file.path(figures_dir, "eda_target_balance_by_season.png"),
            p_target_by_season,
            width = 9,
            height = 5,
            dpi = 150
        )
    }
}

# 4. Missingness by feature

feature_missingness <- tibble(column = model_features) |>
    mutate(
        feature_group = case_when(
            column %in% safe_features ~ "safe",
            column %in% careful_features ~ "use_with_care",
            TRUE ~ "other"
        ),
        missing_n = map_int(column, ~ sum(is.na(model_df[[.x]]))),
        missing_percent = round(100 * missing_n / nrow(model_df), 2),
        non_missing_n = nrow(model_df) - missing_n,
        n_distinct = map_int(column, ~ n_distinct(model_df[[.x]], na.rm = TRUE)),
        r_class = map_chr(column, ~ paste(class(model_df[[.x]]), collapse = "/"))
    ) |>
    arrange(desc(missing_percent), column)

write_csv(
    feature_missingness,
    file.path(tables_dir, "eda_model_feature_missingness.csv")
)

p_missingness <- feature_missingness |>
    mutate(column = fct_reorder(column, missing_percent)) |>
    ggplot(aes(x = column, y = missing_percent)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Missingness among approved model features",
        x = "Feature",
        y = "Missing %"
    )

ggsave(
    file.path(figures_dir, "eda_model_feature_missingness.png"),
    p_missingness,
    width = 9,
    height = 6,
    dpi = 150
)

# 5. Numeric feature summary

numeric_summary <- map_dfr(
    numeric_features,
    function(col) {
        x <- model_df[[col]]

        tibble(
            column = col,
            n = sum(!is.na(x)),
            missing_percent = round(100 * mean(is.na(x)), 2),
            mean = mean(x, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE),
            min = min(x, na.rm = TRUE),
            p01 = quantile(x, 0.01, na.rm = TRUE, names = FALSE),
            p05 = quantile(x, 0.05, na.rm = TRUE, names = FALSE),
            p25 = quantile(x, 0.25, na.rm = TRUE, names = FALSE),
            median = median(x, na.rm = TRUE),
            p75 = quantile(x, 0.75, na.rm = TRUE, names = FALSE),
            p95 = quantile(x, 0.95, na.rm = TRUE, names = FALSE),
            p99 = quantile(x, 0.99, na.rm = TRUE, names = FALSE),
            max = max(x, na.rm = TRUE)
        )
    }
) |>
    arrange(column)

write_csv(
    numeric_summary,
    file.path(tables_dir, "eda_numeric_feature_summary.csv")
)

# 6. Numeric feature summaries by outcome

numeric_by_outcome <- model_df |>
    filter(!is.na(outcome)) |>
    select(outcome, all_of(numeric_features)) |>
    pivot_longer(
        cols = all_of(numeric_features),
        names_to = "feature",
        values_to = "value"
    ) |>
    group_by(feature, outcome) |>
    summarise(
        n = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        .groups = "drop"
    ) |>
    arrange(feature, outcome)

write_csv(
    numeric_by_outcome,
    file.path(tables_dir, "eda_numeric_features_by_outcome.csv")
)

# 7. Numeric feature histograms

for (col in numeric_features) {
    p <- ggplot(model_df, aes(x = .data[[col]])) +
        geom_histogram(bins = 40, na.rm = TRUE) +
        labs(
            title = paste("Distribution:", col),
            x = col,
            y = "Matches"
        )

    ggsave(
        file.path(figures_dir, paste0("eda_hist_", col, ".png")),
        p,
        width = 7,
        height = 5,
        dpi = 150
    )
}

# 8. Numeric feature distributions by outcome

for (col in numeric_features) {
    p <- model_df |>
        filter(!is.na(outcome)) |>
        ggplot(aes(x = outcome, y = .data[[col]])) +
        geom_boxplot(na.rm = TRUE, outlier.alpha = 0.2) +
        labs(
            title = paste("Feature by outcome:", col),
            x = "Outcome",
            y = col
        )

    ggsave(
        file.path(figures_dir, paste0("eda_box_", col, "_by_outcome.png")),
        p,
        width = 7,
        height = 5,
        dpi = 150
    )
}

# 9. Categorical cardinality and top levels

categorical_summary <- tibble(column = categorical_features) |>
    mutate(
        n_missing = map_int(column, ~ sum(is.na(model_df[[.x]]))),
        missing_percent = round(100 * n_missing / nrow(model_df), 2),
        n_distinct = map_int(column, ~ n_distinct(model_df[[.x]], na.rm = TRUE)),
        top_values = map_chr(
            column,
            function(col) {
                model_df |>
                    count(.data[[col]], sort = TRUE, name = "n") |>
                    filter(!is.na(.data[[col]])) |>
                    slice_head(n = 8) |>
                    mutate(label = paste0(.data[[col]], "=", n)) |>
                    pull(label) |>
                    paste(collapse = " | ")
            }
        )
    ) |>
    arrange(desc(n_distinct), column)

write_csv(
    categorical_summary,
    file.path(tables_dir, "eda_categorical_feature_summary.csv")
)

categorical_by_outcome <- map_dfr(
    categorical_features,
    function(col) {
        model_df |>
            filter(!is.na(.data[[col]]), !is.na(outcome)) |>
            count(feature = col, level = as.character(.data[[col]]), outcome, name = "n") |>
            group_by(feature, level) |>
            mutate(
                level_n = sum(n),
                outcome_rate = n / level_n
            ) |>
            ungroup()
    }
)

write_csv(
    categorical_by_outcome,
    file.path(tables_dir, "eda_categorical_features_by_outcome.csv")
)

# 10. Rating-diff specific diagnostics

if ("rating_diff" %in% names(model_df)) {
    rating_diff_summary <- model_df |>
        filter(!is.na(rating_diff), !is.na(outcome)) |>
        mutate(
            abs_rating_diff = abs(rating_diff),
            rating_diff_bucket = cut(
                rating_diff,
                breaks = c(-Inf, -500, -300, -150, -50, 50, 150, 300, 500, Inf),
                include.lowest = TRUE
            ),
            abs_rating_diff_bucket = cut(
                abs_rating_diff,
                breaks = c(-Inf, 25, 50, 100, 150, 250, 400, Inf),
                include.lowest = TRUE
            )
        )

    rating_diff_by_outcome <- rating_diff_summary |>
        count(rating_diff_bucket, outcome, name = "n") |>
        group_by(rating_diff_bucket) |>
        mutate(
            bucket_n = sum(n),
            outcome_rate = n / bucket_n
        ) |>
        ungroup()

    write_csv(
        rating_diff_by_outcome,
        file.path(tables_dir, "eda_rating_diff_bucket_outcome_rates.csv")
    )

    if (nrow(rating_diff_by_outcome) > 0) {
        p_rating_diff_bucket <- ggplot(
            rating_diff_by_outcome,
            aes(x = rating_diff_bucket, y = outcome_rate, fill = outcome)
        ) +
            geom_col(position = "dodge") +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(
                title = "Outcome rates by rating-diff bucket",
                x = "Rating diff bucket",
                y = "Outcome rate",
                fill = "Outcome"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))

        ggsave(
            file.path(figures_dir, "eda_rating_diff_bucket_outcome_rates.png"),
            p_rating_diff_bucket,
            width = 10,
            height = 5,
            dpi = 150
        )
    }

    draw_rate_by_abs_rating_diff <- rating_diff_summary |>
        count(abs_rating_diff_bucket, outcome, name = "n") |>
        group_by(abs_rating_diff_bucket) |>
        mutate(
            bucket_n = sum(n),
            outcome_rate = n / bucket_n
        ) |>
        ungroup() |>
        filter(outcome == "D")

    write_csv(
        draw_rate_by_abs_rating_diff,
        file.path(tables_dir, "eda_draw_rate_by_abs_rating_diff_bucket.csv")
    )

    if (nrow(draw_rate_by_abs_rating_diff) > 0) {
        p_draw_abs_rating <- ggplot(
            draw_rate_by_abs_rating_diff,
            aes(x = abs_rating_diff_bucket, y = outcome_rate)
        ) +
            geom_col() +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(
                title = "Draw rate by absolute rating-diff bucket",
                x = "Absolute rating-diff bucket",
                y = "Draw rate"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))

        ggsave(
            file.path(figures_dir, "eda_draw_rate_by_abs_rating_diff_bucket.png"),
            p_draw_abs_rating,
            width = 9,
            height = 5,
            dpi = 150
        )
    }
}

# 11. Neutral-site diagnostics

if ("neutral" %in% names(model_df)) {
    neutral_outcome <- model_df |>
        filter(!is.na(neutral), !is.na(outcome)) |>
        count(neutral, outcome, name = "n") |>
        group_by(neutral) |>
        mutate(
            group_n = sum(n),
            outcome_rate = n / group_n
        ) |>
        ungroup()

    write_csv(
        neutral_outcome,
        file.path(tables_dir, "eda_neutral_outcome_rates.csv")
    )

    if (nrow(neutral_outcome) > 0) {
        p_neutral <- ggplot(
            neutral_outcome,
            aes(x = as.factor(neutral), y = outcome_rate, fill = outcome)
        ) +
            geom_col(position = "dodge") +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(
                title = "Outcome rates by neutral-site flag",
                x = "Neutral",
                y = "Outcome rate",
                fill = "Outcome"
            )

        ggsave(
            file.path(figures_dir, "eda_neutral_outcome_rates.png"),
            p_neutral,
            width = 7,
            height = 5,
            dpi = 150
        )
    }
}

# 12. Competition/tournament diagnostics

for (col in intersect(c("competition", "tournament"), names(model_df))) {
    comp_outcome <- model_df |>
        filter(!is.na(.data[[col]]), !is.na(outcome)) |>
        count(level = as.character(.data[[col]]), outcome, name = "n") |>
        group_by(level) |>
        mutate(
            level_n = sum(n),
            outcome_rate = n / level_n
        ) |>
        ungroup() |>
        filter(level_n >= 100)

    write_csv(
        comp_outcome,
        file.path(tables_dir, paste0("eda_", col, "_outcome_rates_min100.csv"))
    )

    if (nrow(comp_outcome) > 0) {
        top_levels <- comp_outcome |>
            distinct(level, level_n) |>
            arrange(desc(level_n)) |>
            slice_head(n = 15) |>
            pull(level)

        p_comp <- comp_outcome |>
            filter(level %in% top_levels) |>
            mutate(level = fct_reorder(level, level_n)) |>
            ggplot(aes(x = level, y = outcome_rate, fill = outcome)) +
            geom_col(position = "dodge") +
            coord_flip() +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(
                title = paste("Outcome rates by", col, "- top levels"),
                x = col,
                y = "Outcome rate",
                fill = "Outcome"
            )

        ggsave(
            file.path(figures_dir, paste0("eda_", col, "_outcome_rates_top_levels.png")),
            p_comp,
            width = 9,
            height = 6,
            dpi = 150
        )
    }
}

# 13. Rating freshness diagnostics

age_cols <- intersect(c("rating_age_days_home", "rating_age_days_away"), names(model_df))

if (length(age_cols) > 0) {
    rating_age_summary <- model_df |>
        filter(!is.na(outcome)) |>
        select(all_of(age_cols), outcome) |>
        pivot_longer(
            cols = all_of(age_cols),
            names_to = "feature",
            values_to = "rating_age_days"
        ) |>
        group_by(feature, outcome) |>
        summarise(
            n = sum(!is.na(rating_age_days)),
            mean = mean(rating_age_days, na.rm = TRUE),
            median = median(rating_age_days, na.rm = TRUE),
            p95 = quantile(rating_age_days, 0.95, na.rm = TRUE, names = FALSE),
            max = max(rating_age_days, na.rm = TRUE),
            .groups = "drop"
        )

    write_csv(
        rating_age_summary,
        file.path(tables_dir, "eda_rating_age_days_by_outcome.csv")
    )

    p_rating_age <- model_df |>
        select(all_of(age_cols)) |>
        pivot_longer(
            cols = everything(),
            names_to = "feature",
            values_to = "rating_age_days"
        ) |>
        ggplot(aes(x = rating_age_days)) +
        geom_histogram(bins = 50, na.rm = TRUE) +
        facet_wrap(~ feature, scales = "free_y") +
        labs(
            title = "Rating age distributions",
            x = "Rating age in days",
            y = "Matches"
        )

    ggsave(
        file.path(figures_dir, "eda_rating_age_days_distribution.png"),
        p_rating_age,
        width = 9,
        height = 5,
        dpi = 150
    )
}

# 14. Split drift diagnostics

if (!all(is.na(model_df$.eda_split))) {
    numeric_split_drift <- map_dfr(
        numeric_features,
        function(col) {
            model_df |>
                group_by(.eda_split) |>
                summarise(
                    feature = col,
                    n = sum(!is.na(.data[[col]])),
                    mean = mean(.data[[col]], na.rm = TRUE),
                    median = median(.data[[col]], na.rm = TRUE),
                    sd = sd(.data[[col]], na.rm = TRUE),
                    missing_percent = round(100 * mean(is.na(.data[[col]])), 2),
                    .groups = "drop"
                )
        }
    ) |>
        arrange(feature, .eda_split)

    write_csv(
        numeric_split_drift,
        file.path(tables_dir, "eda_numeric_feature_split_drift.csv")
    )
}

# 15. Correlation among numeric features

if (length(numeric_features) >= 2) {
    corr_df <- model_df |>
        select(all_of(numeric_features)) |>
        mutate(across(everything(), as.numeric))

    corr_mat <- suppressWarnings(
        cor(corr_df, use = "pairwise.complete.obs")
    )

    corr_long <- as.data.frame(as.table(corr_mat)) |>
        as_tibble() |>
        rename(feature_1 = Var1, feature_2 = Var2, correlation = Freq) |>
        arrange(desc(abs(correlation)))

    write_csv(
        corr_long,
        file.path(tables_dir, "eda_numeric_feature_correlations.csv")
    )

    p_corr <- ggplot(
        corr_long,
        aes(x = feature_1, y = feature_2, fill = correlation)
    ) +
        geom_tile() +
        labs(
            title = "Correlation among numeric model features",
            x = "Feature",
            y = "Feature",
            fill = "Correlation"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(
        file.path(figures_dir, "eda_numeric_feature_correlations.png"),
        p_corr,
        width = 8,
        height = 7,
        dpi = 150
    )
}

# 16. Compact EDA index

eda_index <- tibble(
    output = c(
        "eda_target_construction_check.csv",
        "eda_target_balance.csv",
        "eda_target_balance_by_split.csv",
        "eda_target_balance_by_season.csv",
        "eda_model_feature_missingness.csv",
        "eda_numeric_feature_summary.csv",
        "eda_numeric_features_by_outcome.csv",
        "eda_categorical_feature_summary.csv",
        "eda_categorical_features_by_outcome.csv",
        "eda_rating_diff_bucket_outcome_rates.csv",
        "eda_draw_rate_by_abs_rating_diff_bucket.csv",
        "eda_neutral_outcome_rates.csv",
        "eda_rating_age_days_by_outcome.csv",
        "eda_numeric_feature_split_drift.csv",
        "eda_numeric_feature_correlations.csv"
    ),
    description = c(
        "Checks how raw target values were mapped to H/D/A.",
        "Overall H/D/A class balance.",
        "H/D/A balance by validation/test split.",
        "Outcome rates over time by season.",
        "Missingness and cardinality for approved model features.",
        "Distribution summary for numeric model features.",
        "Numeric feature summaries grouped by outcome.",
        "Cardinality and top levels for categorical model features.",
        "Categorical feature outcome rates by level.",
        "Outcome rates across signed rating-diff buckets.",
        "Draw rate across absolute rating-diff buckets.",
        "Outcome rates for neutral vs non-neutral matches.",
        "Rating freshness summaries by outcome.",
        "Numeric feature distribution drift by split.",
        "Pairwise correlations among numeric model features."
    )
)

write_csv(
    eda_index,
    file.path(tables_dir, "eda_output_index.csv")
)

# Console output

cat("\nEDA complete.\n")

cat("\nRows:\n")
print(nrow(model_df))

cat("\nTarget construction check:\n")
print(target_construction_check)

cat("\nTarget balance:\n")
print(target_balance)

cat("\nNumeric features:\n")
print(numeric_features)

cat("\nCategorical/use-with-care features:\n")
print(categorical_features)

cat("\nWrote EDA tables to:\n")
cat(tables_dir, "\n")

cat("\nWrote EDA figures to:\n")
cat(figures_dir, "\n")







# Additional EDA: useful missing plots

# 17. Outcome rates by logical/tournament flags
# These are low-cardinality features, so they are safe and readable to plot.

flag_features <- intersect(
    c(
        "neutral",
        "is_friendly",
        "is_world_cup",
        "is_world_cup_qualifier",
        "is_continental_qualifier",
        "is_continental_tournament"
    ),
    names(model_df)
)

flag_outcome_rates <- map_dfr(
    flag_features,
    function(col) {
        model_df |>
            filter(!is.na(outcome), !is.na(.data[[col]])) |>
            count(
                feature = col,
                value = as.character(.data[[col]]),
                outcome,
                name = "n"
            ) |>
            group_by(feature, value) |>
            mutate(
                group_n = sum(n),
                outcome_rate = n / group_n
            ) |>
            ungroup()
    }
)

write_csv(
    flag_outcome_rates,
    file.path(tables_dir, "eda_flag_feature_outcome_rates.csv")
)

for (col in flag_features) {
    plot_df <- flag_outcome_rates |>
        filter(feature == col)

    if (nrow(plot_df) > 0) {
        p <- ggplot(
            plot_df,
            aes(x = value, y = outcome_rate, fill = outcome)
        ) +
            geom_col(position = "dodge") +
            scale_y_continuous(labels = scales::percent_format()) +
            labs(
                title = paste("Outcome rates by", col),
                x = col,
                y = "Outcome rate",
                fill = "Outcome"
            )

        ggsave(
            file.path(figures_dir, paste0("eda_flag_", col, "_outcome_rates.png")),
            p,
            width = 7,
            height = 5,
            dpi = 150
        )
    }
}

# 18. Top home-team outcome rates
# Only plot teams with enough matches to avoid noisy tiny samples.

if ("home_team_clean" %in% names(model_df)) {
    home_team_outcome_rates <- model_df |>
        filter(!is.na(home_team_clean), !is.na(outcome)) |>
        count(home_team_clean, outcome, name = "n") |>
        group_by(home_team_clean) |>
        mutate(
            team_n = sum(n),
            outcome_rate = n / team_n
        ) |>
        ungroup() |>
        filter(team_n >= 100)

    write_csv(
        home_team_outcome_rates,
        file.path(tables_dir, "eda_home_team_outcome_rates_min100.csv")
    )

    top_home_teams <- home_team_outcome_rates |>
        distinct(home_team_clean, team_n) |>
        arrange(desc(team_n)) |>
        slice_head(n = 20) |>
        pull(home_team_clean)

    p_home_teams <- home_team_outcome_rates |>
        filter(home_team_clean %in% top_home_teams) |>
        mutate(home_team_clean = fct_reorder(home_team_clean, team_n)) |>
        ggplot(aes(x = home_team_clean, y = outcome_rate, fill = outcome)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
            title = "Outcome rates for most common home teams",
            x = "Home team",
            y = "Outcome rate",
            fill = "Outcome"
        )

    ggsave(
        file.path(figures_dir, "eda_home_team_outcome_rates_top20.png"),
        p_home_teams,
        width = 9,
        height = 7,
        dpi = 150
    )
}

# 19. Top away-team outcome rates
# Useful because away strength/away win behavior is different from home behavior.

if ("away_team_clean" %in% names(model_df)) {
    away_team_outcome_rates <- model_df |>
        filter(!is.na(away_team_clean), !is.na(outcome)) |>
        count(away_team_clean, outcome, name = "n") |>
        group_by(away_team_clean) |>
        mutate(
            team_n = sum(n),
            outcome_rate = n / team_n
        ) |>
        ungroup() |>
        filter(team_n >= 100)

    write_csv(
        away_team_outcome_rates,
        file.path(tables_dir, "eda_away_team_outcome_rates_min100.csv")
    )

    top_away_teams <- away_team_outcome_rates |>
        distinct(away_team_clean, team_n) |>
        arrange(desc(team_n)) |>
        slice_head(n = 20) |>
        pull(away_team_clean)

    p_away_teams <- away_team_outcome_rates |>
        filter(away_team_clean %in% top_away_teams) |>
        mutate(away_team_clean = fct_reorder(away_team_clean, team_n)) |>
        ggplot(aes(x = away_team_clean, y = outcome_rate, fill = outcome)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
            title = "Outcome rates for most common away teams",
            x = "Away team",
            y = "Outcome rate",
            fill = "Outcome"
        )

    ggsave(
        file.path(figures_dir, "eda_away_team_outcome_rates_top20.png"),
        p_away_teams,
        width = 9,
        height = 7,
        dpi = 150
    )
}

# 20. Top country/location outcome rates
# This helps check whether country/location metadata is acting like home advantage.

if ("country" %in% names(model_df)) {
    country_outcome_rates <- model_df |>
        filter(!is.na(country), !is.na(outcome)) |>
        count(country, outcome, name = "n") |>
        group_by(country) |>
        mutate(
            country_n = sum(n),
            outcome_rate = n / country_n
        ) |>
        ungroup() |>
        filter(country_n >= 100)

    write_csv(
        country_outcome_rates,
        file.path(tables_dir, "eda_country_outcome_rates_min100.csv")
    )

    top_countries <- country_outcome_rates |>
        distinct(country, country_n) |>
        arrange(desc(country_n)) |>
        slice_head(n = 20) |>
        pull(country)

    p_country <- country_outcome_rates |>
        filter(country %in% top_countries) |>
        mutate(country = fct_reorder(country, country_n)) |>
        ggplot(aes(x = country, y = outcome_rate, fill = outcome)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
            title = "Outcome rates for most common match countries",
            x = "Country",
            y = "Outcome rate",
            fill = "Outcome"
        )

    ggsave(
        file.path(figures_dir, "eda_country_outcome_rates_top20.png"),
        p_country,
        width = 9,
        height = 7,
        dpi = 150
    )
}

# 21. Rating missingness by season
# This checks whether missing ratings are concentrated in early history.

rating_missing_cols <- intersect(
    c("rating_diff", "home_rating_pre_match", "away_rating_pre_match"),
    names(model_df)
)

if ("season" %in% names(model_df) && length(rating_missing_cols) > 0) {
    rating_missing_by_season <- model_df |>
        select(season, all_of(rating_missing_cols)) |>
        pivot_longer(
            cols = all_of(rating_missing_cols),
            names_to = "feature",
            values_to = "value"
        ) |>
        group_by(season, feature) |>
        summarise(
            matches = n(),
            missing_n = sum(is.na(value)),
            missing_pct = mean(is.na(value)),
            .groups = "drop"
        )

    write_csv(
        rating_missing_by_season,
        file.path(tables_dir, "eda_rating_missingness_by_season.csv")
    )

    p_rating_missing_season <- rating_missing_by_season |>
        ggplot(aes(x = season, y = missing_pct, color = feature)) +
        geom_line() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
            title = "Rating missingness by season",
            x = "Season",
            y = "Missing %",
            color = "Feature"
        )

    ggsave(
        file.path(figures_dir, "eda_rating_missingness_by_season.png"),
        p_rating_missing_season,
        width = 9,
        height = 5,
        dpi = 150
    )
}




# 22. Rating staleness by season
# This checks whether some periods rely on very old ratings.
# Patched to avoid max()/median()/quantile() warnings when a season-feature group
# has zero non-missing rating-age values.

age_cols <- intersect(
    c("rating_age_days_home", "rating_age_days_away"),
    names(model_df)
)

if ("season" %in% names(model_df) && length(age_cols) > 0) {
    rating_age_long_by_season <- model_df |>
        select(season, all_of(age_cols)) |>
        pivot_longer(
            cols = all_of(age_cols),
            names_to = "feature",
            values_to = "rating_age_days"
        )

    rating_age_empty_groups <- rating_age_long_by_season |>
        group_by(season, feature) |>
        summarise(
            matches = n(),
            non_missing_n = sum(!is.na(rating_age_days)),
            .groups = "drop"
        ) |>
        filter(non_missing_n == 0)

    write_csv(
        rating_age_empty_groups,
        file.path(tables_dir, "eda_rating_age_empty_groups.csv")
    )

    rating_age_by_season <- rating_age_long_by_season |>
        group_by(season, feature) |>
        summarise(
            matches = n(),
            non_missing_n = sum(!is.na(rating_age_days)),

            median_age_days = if (non_missing_n[1] > 0) {
                median(rating_age_days, na.rm = TRUE)
            } else {
                NA_real_
            },

            p90_age_days = if (non_missing_n[1] > 0) {
                quantile(rating_age_days, 0.90, na.rm = TRUE, names = FALSE)
            } else {
                NA_real_
            },

            p95_age_days = if (non_missing_n[1] > 0) {
                quantile(rating_age_days, 0.95, na.rm = TRUE, names = FALSE)
            } else {
                NA_real_
            },

            max_age_days = if (non_missing_n[1] > 0) {
                max(rating_age_days, na.rm = TRUE)
            } else {
                NA_real_
            },

            stale_gt_365_pct = if (non_missing_n[1] > 0) {
                mean(rating_age_days > 365, na.rm = TRUE)
            } else {
                NA_real_
            },

            stale_gt_730_pct = if (non_missing_n[1] > 0) {
                mean(rating_age_days > 730, na.rm = TRUE)
            } else {
                NA_real_
            },

            stale_gt_3650_pct = if (non_missing_n[1] > 0) {
                mean(rating_age_days > 3650, na.rm = TRUE)
            } else {
                NA_real_
            },

            .groups = "drop"
        )

    write_csv(
        rating_age_by_season,
        file.path(tables_dir, "eda_rating_age_by_season.csv")
    )

    p_rating_age_p95 <- rating_age_by_season |>
        filter(is.finite(p95_age_days)) |>
        ggplot(aes(x = season, y = p95_age_days, color = feature)) +
        geom_line() +
        labs(
            title = "Rating staleness by season: 95th percentile age",
            x = "Season",
            y = "95th percentile rating age in days",
            color = "Feature"
        )

    ggsave(
        file.path(figures_dir, "eda_rating_age_p95_by_season.png"),
        p_rating_age_p95,
        width = 9,
        height = 5,
        dpi = 150
    )

    p_stale_730 <- rating_age_by_season |>
        filter(is.finite(stale_gt_730_pct)) |>
        ggplot(aes(x = season, y = stale_gt_730_pct, color = feature)) +
        geom_line() +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
            title = "Share of ratings older than 2 years by season",
            x = "Season",
            y = "Share older than 730 days",
            color = "Feature"
        )

    ggsave(
        file.path(figures_dir, "eda_rating_age_gt730_by_season.png"),
        p_stale_730,
        width = 9,
        height = 5,
        dpi = 150
    )
}

