# 21_baseline_plus_draw_features.R
#
# Extends 19_baseline.R with |rating_diff| and rating_diff^2 to see whether
# simple draw-aware terms help before the full Model 26 feature variants.
#
# Reads: data/processed/international_modeling_table.csv
#
# Writes:
# - reports/tables/baseline_plus_*.csv
# - reports/figures/baseline_plus_*.png
#
# Notes:
# - Model selection uses validation log loss, not accuracy.

set.seed(20240529)

required_packages <- c(
    "tidyverse",
    "nnet",
    "yardstick",
    "scales"
)

missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
    stop(
        "Missing required R packages: ",
        paste(missing_packages, collapse = ", "),
        ". Install them before running this script.",
        call. = FALSE
    )
}

suppressPackageStartupMessages({
    library(tidyverse)
    library(nnet)
    library(yardstick)
    library(scales)
})

dir.create("reports", showWarnings = FALSE)
dir.create("reports/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)


# 1. Load data

data_path <- "data/processed/international_modeling_table.csv"

if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path, call. = FALSE)
}

raw_df <- readr::read_csv(data_path, show_col_types = FALSE)

required_cols <- c(
    "match_result",
    "rating_diff",
    "data_split"
)

missing_required <- setdiff(required_cols, names(raw_df))

if (length(missing_required) > 0) {
    stop(
        "Missing required columns: ",
        paste(missing_required, collapse = ", "),
        call. = FALSE
    )
}


# 2. Filter candidate rows

target_levels <- c("H", "D", "A")

df <- raw_df %>%
    filter(
        match_result %in% target_levels,
        !is.na(rating_diff),
        data_split %in% c("train", "test")
    ) %>%
    mutate(
        match_result = factor(match_result, levels = target_levels),
        data_split = factor(data_split, levels = c("train", "test")),
        # Draw-aware features (see comments below).
        # abs_rating_diff: draws are more likely when teams are evenly matched;
        #   large |rating_diff| usually means a clear favorite (H or A).
        # rating_diff_sq: adds curvature so draw probability can peak near 0
        #   rather than changing linearly with signed rating_diff alone.
        abs_rating_diff = abs(rating_diff),
        rating_diff_sq = rating_diff^2
    )

if (nrow(df) == 0) {
    stop("No rows remain after filtering candidate rows.", call. = FALSE)
}

if (!identical(levels(df$match_result), target_levels)) {
    stop("match_result factor levels are not c('H', 'D', 'A').", call. = FALSE)
}


# 3. Neutral handling (same logic as 19_baseline.R)

coerce_neutral_model <- function(neutral_vector) {
    if (is.logical(neutral_vector)) {
        if (all(is.na(neutral_vector))) {
            return(NULL)
        }

        return(as.integer(neutral_vector))
    }

    if (is.numeric(neutral_vector)) {
        if (!all(neutral_vector %in% c(0, 1, NA))) {
            return(NULL)
        }

        return(as.integer(neutral_vector))
    }

    if (is.character(neutral_vector)) {
        normalized <- toupper(str_trim(neutral_vector))

        if (!all(normalized %in% c("TRUE", "FALSE", "1", "0", NA))) {
            return(NULL)
        }

        return(ifelse(normalized %in% c("TRUE", "1"), 1L, 0L))
    }

    NULL
}

neutral_model_valid <- FALSE

if ("neutral" %in% names(df)) {
    neutral_model_values <- coerce_neutral_model(df$neutral)

    if (!is.null(neutral_model_values)) {
        df$neutral_model <- neutral_model_values
        neutral_model_valid <- TRUE
        message("neutral column coerced to neutral_model (0/1).")
    } else {
        message(
            "neutral column present but could not be safely coerced. ",
            "Skipping neutral_model in formulas."
        )
    }
} else {
    message("neutral column not found. Skipping neutral_model in formulas.")
}


# 4. Leakage guard

leakage_cols <- c(
    "home_score",
    "away_score",
    "goal_difference",
    "total_goals",
    "result_class",
    "shootout_winner",
    "home_won_shootout",
    "away_won_shootout"
)

leakage_cols_present <- intersect(leakage_cols, names(df))

message(
    "Leakage columns present and excluded from all predictor allowlists: ",
    ifelse(
        length(leakage_cols_present) == 0,
        "none",
        paste(leakage_cols_present, collapse = ", ")
    )
)

predictor_allowlists <- list(
    frequency_baseline = character(0),
    rating_diff_multinom = c("rating_diff"),
    rating_diff_neutral_multinom = c("rating_diff", "neutral_model"),
    draw_aware_abs_multinom = c("rating_diff", "abs_rating_diff", "neutral_model"),
    draw_aware_quadratic_multinom = c("rating_diff", "rating_diff_sq", "neutral_model")
)

if (!neutral_model_valid) {
    predictor_allowlists$rating_diff_neutral_multinom <- NULL
    predictor_allowlists$draw_aware_abs_multinom <- c("rating_diff", "abs_rating_diff")
    predictor_allowlists$draw_aware_quadratic_multinom <- c("rating_diff", "rating_diff_sq")
}

for (allowlist_name in names(predictor_allowlists)) {
    allowlist <- predictor_allowlists[[allowlist_name]]

    if (is.null(allowlist)) {
        next
    }

    bad_predictors <- intersect(allowlist, leakage_cols)

    if (length(bad_predictors) > 0) {
        stop(
            "Leakage predictors found in allowlist for ",
            allowlist_name,
            ": ",
            paste(bad_predictors, collapse = ", "),
            call. = FALSE
        )
    }
}


# 5. Train / test split

train_full <- df %>%
    filter(data_split == "train")

test_df <- df %>%
    filter(data_split == "test")

if (nrow(train_full) == 0) {
    stop("No training rows found.", call. = FALSE)
}

if (nrow(test_df) == 0) {
    stop("No test rows found.", call. = FALSE)
}


# 6. Chronological validation split

if ("date" %in% names(train_full)) {
    train_full <- train_full %>%
        mutate(.split_date = as.Date(date)) %>%
        arrange(.split_date)

    if (all(is.na(train_full$.split_date))) {
        warning(
            "date column exists but could not be parsed as Date. ",
            "Using original row order within train instead.",
            call. = FALSE
        )

        train_full <- train_full %>%
            arrange(row_number())
    } else {
        message("Using chronological validation split by date column.")
    }
} else {
    warning(
        "No usable date column found. ",
        "Using original row order within train for validation split.",
        call. = FALSE
    )

    train_full <- train_full %>%
        arrange(row_number())
}

n_train_full <- nrow(train_full)
inner_train_size <- floor(0.80 * n_train_full)

if (inner_train_size < 10) {
    stop(
        "Training set is too small to create an inner train / validation split.",
        call. = FALSE
    )
}

inner_train <- train_full %>%
    slice(1:inner_train_size)

validation_df <- train_full %>%
    slice((inner_train_size + 1):n_train_full)

message("Inner training rows: ", nrow(inner_train))
message("Validation rows: ", nrow(validation_df))
message("Test rows: ", nrow(test_df))


# 7. Utility functions (aligned with 19_baseline.R)

prob_cols <- c(".pred_H", ".pred_D", ".pred_A")
prob_drift_tolerance <- 1e-6
prob_error_tolerance <- 1e-3

normalize_probs <- function(prob_matrix) {
    prob_matrix <- pmax(prob_matrix, 0)
    row_sums <- rowSums(prob_matrix)

    if (any(row_sums == 0)) {
        stop("At least one probability row sums to zero.", call. = FALSE)
    }

    prob_matrix / row_sums
}

check_probability_frame <- function(
    pred_df,
    prob_columns = c(".pred_H", ".pred_D", ".pred_A")
) {
    missing_probs <- setdiff(prob_columns, names(pred_df))

    if (length(missing_probs) > 0) {
        stop(
            "Missing probability columns: ",
            paste(missing_probs, collapse = ", "),
            call. = FALSE
        )
    }

    prob_matrix <- as.matrix(pred_df[, prob_columns, drop = FALSE])

    if (any(is.na(prob_matrix))) {
        stop("Missing predicted probabilities detected.", call. = FALSE)
    }

    if (any(prob_matrix < -1e-10)) {
        stop("Negative predicted probabilities detected.", call. = FALSE)
    }

    row_sums <- rowSums(prob_matrix)
    drift <- abs(row_sums - 1)

    if (any(drift > prob_drift_tolerance)) {
        if (any(drift > prob_error_tolerance)) {
            stop(
                "Predicted probabilities do not sum to 1 within tolerance.",
                call. = FALSE
            )
        }

        prob_matrix <- normalize_probs(prob_matrix)
        pred_df[[prob_columns[1]]] <- prob_matrix[, 1]
        pred_df[[prob_columns[2]]] <- prob_matrix[, 2]
        pred_df[[prob_columns[3]]] <- prob_matrix[, 3]
    }

    pred_df
}

add_predicted_class <- function(pred_df) {
    pred_df %>%
        mutate(
            .pred_class = factor(
                target_levels[
                    max.col(
                        as.matrix(across(all_of(prob_cols))),
                        ties.method = "first"
                    )
                ],
                levels = target_levels
            )
        )
}

multiclass_brier <- function(pred_df, truth_col = "match_result") {
    truth <- pred_df[[truth_col]]

    brier_h <- mean((pred_df$.pred_H - as.integer(truth == "H"))^2)
    brier_d <- mean((pred_df$.pred_D - as.integer(truth == "D"))^2)
    brier_a <- mean((pred_df$.pred_A - as.integer(truth == "A"))^2)

    brier_h + brier_d + brier_a
}

safe_ratio <- function(numerator, denominator) {
    if (denominator == 0) {
        return(0)
    }

    numerator / denominator
}

safe_f1 <- function(precision, recall) {
    if (precision + recall == 0) {
        return(0)
    }

    2 * precision * recall / (precision + recall)
}

compute_classwise_metrics <- function(
    pred_df,
    truth_col = "match_result",
    predicted_col = ".pred_class"
) {
    pred_df <- check_probability_frame(pred_df) %>%
        add_predicted_class()

    truth_vector <- as.character(pred_df[[truth_col]])
    predicted_vector <- as.character(pred_df[[predicted_col]])

    purrr::map_dfr(target_levels, function(class_label) {
        support <- sum(truth_vector == class_label, na.rm = TRUE)
        predicted_n <- sum(predicted_vector == class_label, na.rm = TRUE)
        true_positive <- sum(
            truth_vector == class_label & predicted_vector == class_label,
            na.rm = TRUE
        )
        false_positive <- sum(
            truth_vector != class_label & predicted_vector == class_label,
            na.rm = TRUE
        )
        false_negative <- sum(
            truth_vector == class_label & predicted_vector != class_label,
            na.rm = TRUE
        )

        precision <- safe_ratio(true_positive, true_positive + false_positive)
        recall <- safe_ratio(true_positive, true_positive + false_negative)
        f1 <- safe_f1(precision, recall)

        tibble(
            class = class_label,
            support = support,
            predicted_n = predicted_n,
            true_positive = true_positive,
            false_positive = false_positive,
            false_negative = false_negative,
            precision = precision,
            recall = recall,
            f1 = f1
        )
    })
}

compute_macro_f1 <- function(classwise_metrics) {
    mean(classwise_metrics$f1)
}

compute_metrics <- function(pred_df, truth_col = "match_result") {
    pred_df <- check_probability_frame(pred_df) %>%
        add_predicted_class()

    classwise_metrics <- compute_classwise_metrics(
        pred_df,
        truth_col = truth_col,
        predicted_col = ".pred_class"
    )

    log_loss_value <- yardstick::mn_log_loss(
        pred_df,
        truth = !!rlang::sym(truth_col),
        !!!rlang::syms(prob_cols)
    ) %>%
        pull(.estimate)

    accuracy_value <- yardstick::accuracy(
        pred_df,
        truth = !!rlang::sym(truth_col),
        estimate = .pred_class
    ) %>%
        pull(.estimate)

    macro_f1_value <- compute_macro_f1(classwise_metrics)

    tibble(
        n = nrow(pred_df),
        log_loss = log_loss_value,
        brier = multiclass_brier(pred_df, truth_col = truth_col),
        accuracy = accuracy_value,
        macro_f1 = macro_f1_value
    )
}

build_confusion_matrix <- function(
    pred_df,
    truth_col = "match_result",
    predicted_col = ".pred_class"
) {
    pred_df <- check_probability_frame(pred_df) %>%
        add_predicted_class()

    pred_df %>%
        transmute(
            match_result = as.character(.data[[truth_col]]),
            predicted_result = as.character(.data[[predicted_col]])
        ) %>%
        count(match_result, predicted_result, name = "n") %>%
        group_by(match_result) %>%
        mutate(row_prop = n / sum(n)) %>%
        ungroup()
}

enrich_predictions <- function(pred_df, source_df) {
    pred_df <- check_probability_frame(pred_df) %>%
        add_predicted_class() %>%
        mutate(
            predicted_result = as.character(.pred_class),
            confidence = pmax(.pred_H, .pred_D, .pred_A)
        )

    identity_cols <- c(
        "date",
        "home_team",
        "away_team",
        "tournament",
        "neutral",
        "rating_diff",
        "abs_rating_diff",
        "rating_diff_sq",
        "home_score",
        "away_score"
    )

    source_identity <- source_df %>%
        select(dplyr::any_of(identity_cols))

    if (nrow(source_identity) != nrow(pred_df)) {
        stop(
            "Prediction rows and source rows must align for enrichment.",
            call. = FALSE
        )
    }

    bind_cols(source_identity, pred_df)
}

make_constant_probability_predictions <- function(new_data, class_probs) {
    class_probs <- class_probs[target_levels]
    class_probs <- class_probs / sum(class_probs)

    tibble(
        match_result = new_data$match_result,
        .pred_H = class_probs[["H"]],
        .pred_D = class_probs[["D"]],
        .pred_A = class_probs[["A"]]
    )
}

predict_multinom_probs <- function(model, new_data) {
    raw_probs <- predict(model, newdata = new_data, type = "probs")

    if (is.vector(raw_probs)) {
        raw_probs <- matrix(raw_probs, nrow = 1)
    }

    raw_probs <- as.data.frame(raw_probs)

    for (level_name in setdiff(target_levels, names(raw_probs))) {
        raw_probs[[level_name]] <- 0
    }

    raw_probs <- raw_probs[, target_levels, drop = FALSE]
    raw_probs <- normalize_probs(as.matrix(raw_probs))

    tibble(
        match_result = new_data$match_result,
        .pred_H = raw_probs[, "H"],
        .pred_D = raw_probs[, "D"],
        .pred_A = raw_probs[, "A"]
    )
}

estimate_class_probabilities <- function(training_data) {
    class_frequency <- training_data %>%
        count(match_result) %>%
        complete(
            match_result = factor(target_levels, levels = target_levels),
            fill = list(n = 0)
        ) %>%
        mutate(probability = n / sum(n))

    class_probs <- class_frequency$probability
    names(class_probs) <- as.character(class_frequency$match_result)
    class_probs
}

build_calibration_table <- function(
    pred_df,
    model_name,
    split_name,
    n_bins = 10
) {
    pred_df <- check_probability_frame(pred_df) %>%
        add_predicted_class()

    confidence_breaks <- seq(0, 1, length.out = n_bins + 1)

    pred_df %>%
        mutate(
            confidence = pmax(.pred_H, .pred_D, .pred_A),
            correct = as.integer(.pred_class == match_result),
            bin = cut(
                confidence,
                breaks = confidence_breaks,
                include.lowest = TRUE,
                labels = FALSE
            )
        ) %>%
        group_by(bin) %>%
        summarise(
            model = model_name,
            split = split_name,
            n = n(),
            mean_confidence = mean(confidence),
            accuracy = mean(correct),
            .groups = "drop"
        )
}

append_metrics_row <- function(
    metrics_table,
    model_name,
    split_name,
    pred_df
) {
    split_metrics <- compute_metrics(pred_df) %>%
        mutate(
            model = model_name,
            split = split_name
        ) %>%
        select(model, split, n, log_loss, brier, accuracy, macro_f1)

    bind_rows(metrics_table, split_metrics)
}

fit_and_store_multinom <- function(
    model_name,
    formula,
    prediction_store,
    metrics_table
) {
    fitted_model <- nnet::multinom(
        formula,
        data = inner_train,
        trace = FALSE
    )

    val_pred <- predict_multinom_probs(fitted_model, validation_df)
    test_pred <- predict_multinom_probs(fitted_model, test_df)

    prediction_store[[model_name]] <- list(
        validation = val_pred,
        test = test_pred,
        model = fitted_model
    )

    metrics_table <- append_metrics_row(
        metrics_table,
        model_name,
        "validation",
        val_pred
    )
    metrics_table <- append_metrics_row(
        metrics_table,
        model_name,
        "test",
        test_pred
    )

    list(
        prediction_store = prediction_store,
        metrics_table = metrics_table
    )
}

prediction_store <- list()
metrics_table <- tibble()
draw_aware_model_names <- c(
    "draw_aware_abs_multinom",
    "draw_aware_quadratic_multinom"
)


# 8. Class-frequency baseline

class_probs_inner <- estimate_class_probabilities(inner_train)

frequency_val_pred <- make_constant_probability_predictions(
    validation_df,
    class_probs_inner
)

frequency_test_pred <- make_constant_probability_predictions(
    test_df,
    class_probs_inner
)

prediction_store$frequency_baseline <- list(
    validation = frequency_val_pred,
    test = frequency_test_pred
)

metrics_table <- append_metrics_row(
    metrics_table,
    "frequency_baseline",
    "validation",
    frequency_val_pred
)
metrics_table <- append_metrics_row(
    metrics_table,
    "frequency_baseline",
    "test",
    frequency_test_pred
)


# 9. rating_diff (+ neutral) baseline

if (neutral_model_valid) {
    baseline_fit <- fit_and_store_multinom(
        model_name = "rating_diff_neutral_multinom",
        formula = match_result ~ rating_diff + neutral_model,
        prediction_store = prediction_store,
        metrics_table = metrics_table
    )
} else {
    baseline_fit <- fit_and_store_multinom(
        model_name = "rating_diff_multinom",
        formula = match_result ~ rating_diff,
        prediction_store = prediction_store,
        metrics_table = metrics_table
    )
}

prediction_store <- baseline_fit$prediction_store
metrics_table <- baseline_fit$metrics_table


# 10. Draw-aware multinomial models

if (neutral_model_valid) {
    abs_formula <- match_result ~ rating_diff + abs_rating_diff + neutral_model
    quad_formula <- match_result ~ rating_diff + rating_diff_sq + neutral_model
} else {
    abs_formula <- match_result ~ rating_diff + abs_rating_diff
    quad_formula <- match_result ~ rating_diff + rating_diff_sq
}

abs_fit <- fit_and_store_multinom(
    model_name = "draw_aware_abs_multinom",
    formula = abs_formula,
    prediction_store = prediction_store,
    metrics_table = metrics_table
)

prediction_store <- abs_fit$prediction_store
metrics_table <- abs_fit$metrics_table

quad_fit <- fit_and_store_multinom(
    model_name = "draw_aware_quadratic_multinom",
    formula = quad_formula,
    prediction_store = prediction_store,
    metrics_table = metrics_table
)

prediction_store <- quad_fit$prediction_store
metrics_table <- quad_fit$metrics_table


# 11. Save tables

metrics_table <- metrics_table %>%
    arrange(model, split)

readr::write_csv(
    metrics_table,
    "reports/tables/baseline_plus_model_comparison.csv"
)

evaluation_splits <- c("validation", "test")

classwise_metrics_table <- purrr::map_dfr(
    names(prediction_store),
    function(model_name) {
        purrr::map_dfr(
            evaluation_splits,
            function(split_name) {
                compute_classwise_metrics(
                    prediction_store[[model_name]][[split_name]]
                ) %>%
                    mutate(
                        model = model_name,
                        split = split_name
                    )
            }
        )
    }
) %>%
    select(
        model,
        split,
        class,
        support,
        predicted_n,
        true_positive,
        false_positive,
        false_negative,
        precision,
        recall,
        f1
    )

readr::write_csv(
    classwise_metrics_table,
    "reports/tables/baseline_plus_classwise_metrics.csv"
)

confusion_matrix_table <- purrr::map_dfr(
    names(prediction_store),
    function(model_name) {
        purrr::map_dfr(
            evaluation_splits,
            function(split_name) {
                build_confusion_matrix(
                    prediction_store[[model_name]][[split_name]]
                ) %>%
                    mutate(
                        model = model_name,
                        split = split_name
                    )
            }
        )
    }
) %>%
    select(model, split, match_result, predicted_result, n, row_prop)

readr::write_csv(
    confusion_matrix_table,
    "reports/tables/baseline_plus_confusion_matrix.csv"
)

best_validation_model <- metrics_table %>%
    filter(split == "validation") %>%
    slice_min(log_loss, n = 1, with_ties = FALSE) %>%
    pull(model)

best_test_predictions <- enrich_predictions(
    prediction_store[[best_validation_model]]$test,
    test_df
)

confident_wrong_predictions <- best_test_predictions %>%
    filter(predicted_result != as.character(match_result)) %>%
    arrange(desc(confidence)) %>%
    mutate(match_result = as.character(match_result)) %>%
    select(
        dplyr::any_of(c(
            "date",
            "home_team",
            "away_team",
            "tournament",
            "neutral",
            "rating_diff",
            "abs_rating_diff",
            "rating_diff_sq",
            "match_result",
            "predicted_result",
            "confidence",
            ".pred_H",
            ".pred_D",
            ".pred_A",
            "home_score",
            "away_score"
        ))
    )

readr::write_csv(
    confident_wrong_predictions,
    "reports/tables/baseline_plus_confident_wrong_predictions.csv"
)

validation_calibration <- purrr::map_dfr(
    names(prediction_store),
    function(model_name) {
        build_calibration_table(
            prediction_store[[model_name]]$validation,
            model_name = model_name,
            split_name = "validation"
        )
    }
)

test_calibration <- purrr::map_dfr(
    names(prediction_store),
    function(model_name) {
        build_calibration_table(
            prediction_store[[model_name]]$test,
            model_name = model_name,
            split_name = "test"
        )
    }
)

readr::write_csv(
    validation_calibration,
    "reports/tables/baseline_plus_validation_calibration.csv"
)

readr::write_csv(
    test_calibration,
    "reports/tables/baseline_plus_test_calibration.csv"
)


# 12. Plots

make_metric_comparison_plot <- function(metrics_long, title_text, subtitle_text) {
    ggplot(metrics_long, aes(x = reorder(model, value), y = value, fill = split)) +
        geom_col(position = "dodge") +
        facet_wrap(~ metric, scales = "free_y") +
        coord_flip() +
        labs(
            title = title_text,
            subtitle = subtitle_text,
            x = "Model",
            y = "Metric value",
            fill = "Split"
        ) +
        theme_minimal()
}

metrics_long <- metrics_table %>%
    select(model, split, log_loss, brier, accuracy, macro_f1) %>%
    pivot_longer(
        cols = c(log_loss, brier, accuracy, macro_f1),
        names_to = "metric",
        values_to = "value"
    )

metric_comparison_lower_plot <- metrics_long %>%
    filter(metric %in% c("log_loss", "brier")) %>%
    make_metric_comparison_plot(
        title_text = "Baseline+ metrics (lower is better)",
        subtitle_text = "log_loss and brier — lower values indicate better scoring."
    )

metric_comparison_higher_plot <- metrics_long %>%
    filter(metric %in% c("accuracy", "macro_f1")) %>%
    make_metric_comparison_plot(
        title_text = "Baseline+ metrics (higher is better)",
        subtitle_text = "accuracy and macro_f1 — higher values indicate better discrimination."
    )

ggsave(
    "reports/figures/baseline_plus_metric_comparison_lower_is_better.png",
    metric_comparison_lower_plot,
    width = 10,
    height = 5,
    dpi = 300
)

ggsave(
    "reports/figures/baseline_plus_metric_comparison_higher_is_better.png",
    metric_comparison_higher_plot,
    width = 10,
    height = 5,
    dpi = 300
)

best_test_confusion <- confusion_matrix_table %>%
    filter(
        model == best_validation_model,
        split == "test"
    ) %>%
    mutate(
        match_result = factor(match_result, levels = target_levels),
        predicted_result = factor(predicted_result, levels = target_levels)
    )

confusion_matrix_plot <- ggplot(
    best_test_confusion,
    aes(x = predicted_result, y = match_result, fill = row_prop)
) +
    geom_tile(color = "white") +
    geom_text(
        aes(label = scales::percent(row_prop, accuracy = 0.1)),
        color = "white",
        size = 3.5
    ) +
    scale_fill_viridis_c(
        labels = percent_format(accuracy = 1),
        name = "Row %"
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    labs(
        title = "Test confusion matrix: best baseline+ model",
        subtitle = paste0(
            "Model: ",
            best_validation_model,
            " (selected by validation log loss)"
        ),
        x = "Predicted result",
        y = "Actual result"
    ) +
    theme_minimal() +
    coord_fixed()

ggsave(
    "reports/figures/baseline_plus_confusion_matrix.png",
    confusion_matrix_plot,
    width = 7,
    height = 6,
    dpi = 300
)

plot_calibration_curves <- function(calibration_df, title_text) {
    ggplot(
        calibration_df,
        aes(
            x = mean_confidence,
            y = accuracy,
            color = model
        )
    ) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
        geom_point(aes(size = n), alpha = 0.8) +
        geom_line(alpha = 0.7) +
        scale_x_continuous(labels = percent_format(accuracy = 1)) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(
            title = title_text,
            x = "Mean confidence (max predicted probability)",
            y = "Accuracy",
            color = "Model",
            size = "Bin n"
        ) +
        theme_minimal()
}

test_calibration_plot <- plot_calibration_curves(
    test_calibration,
    "Test calibration by model (baseline+)"
)

ggsave(
    "reports/figures/baseline_plus_test_calibration.png",
    test_calibration_plot,
    width = 10,
    height = 6,
    dpi = 300
)


# 13. Draw probability curve

draw_aware_models_available <- intersect(
    draw_aware_model_names,
    names(prediction_store)
)

curve_model_name <- if (length(draw_aware_models_available) > 0) {
    metrics_table %>%
        filter(
            split == "validation",
            model %in% draw_aware_models_available
        ) %>%
        slice_min(log_loss, n = 1, with_ties = FALSE) %>%
        pull(model)
} else {
    best_validation_model
}

curve_model <- prediction_store[[curve_model_name]]$model

rating_grid <- tibble(
    rating_diff = seq(
        min(inner_train$rating_diff, na.rm = TRUE),
        max(inner_train$rating_diff, na.rm = TRUE),
        length.out = 300
    ),
    abs_rating_diff = abs(rating_diff),
    rating_diff_sq = rating_diff^2,
    match_result = factor(NA_character_, levels = target_levels)
)

model_term_labels <- attr(terms(curve_model), "term.labels")

if ("neutral_model" %in% model_term_labels) {
    rating_grid$neutral_model <- 0L
}

grid_probs <- predict_multinom_probs(curve_model, rating_grid) %>%
    mutate(rating_diff = rating_grid$rating_diff)

draw_probability_curve_plot <- grid_probs %>%
    ggplot(aes(x = rating_diff, y = .pred_D)) +
    geom_line(linewidth = 1, color = "#2C7BB6") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
        title = "Predicted draw probability by rating difference",
        subtitle = paste0("Model: ", curve_model_name),
        x = "rating_diff",
        y = "Predicted draw probability (.pred_D)"
    ) +
    theme_minimal()

ggsave(
    "reports/figures/baseline_plus_draw_probability_curve.png",
    draw_probability_curve_plot,
    width = 8,
    height = 5,
    dpi = 300
)


# 14. Final summary

best_validation_row <- metrics_table %>%
    filter(split == "validation") %>%
    slice_min(log_loss, n = 1, with_ties = FALSE)

best_test_row <- metrics_table %>%
    filter(split == "test") %>%
    slice_min(log_loss, n = 1, with_ties = FALSE)

best_model_test_metrics <- metrics_table %>%
    filter(
        model == best_validation_model,
        split == "test"
    )

best_model_draw_metrics <- classwise_metrics_table %>%
    filter(
        model == best_validation_model,
        split == "test",
        class == "D"
    )

cat("\n")
cat("============================================================\n")
cat("Baseline+ draw-feature modeling complete\n")
cat("============================================================\n")
cat(
    "Best validation log-loss model: ",
    best_validation_row$model,
    " (",
    round(best_validation_row$log_loss, 5),
    ")\n",
    sep = ""
)
cat(
    "Best test log-loss model: ",
    best_test_row$model,
    " (",
    round(best_test_row$log_loss, 5),
    ")\n",
    sep = ""
)
cat(
    "Best model test log_loss: ",
    round(best_model_test_metrics$log_loss, 5),
    "\n",
    sep = ""
)
cat(
    "Best model test accuracy: ",
    round(best_model_test_metrics$accuracy, 5),
    "\n",
    sep = ""
)
cat(
    "Best model test macro_f1: ",
    round(best_model_test_metrics$macro_f1, 5),
    "\n",
    sep = ""
)
cat(
    "Best model test draw recall: ",
    round(best_model_draw_metrics$recall, 5),
    "\n",
    sep = ""
)
cat(
    "Best model test draw F1: ",
    round(best_model_draw_metrics$f1, 5),
    "\n",
    sep = ""
)
cat(
    "Number of confident wrong predictions saved: ",
    nrow(confident_wrong_predictions),
    "\n",
    sep = ""
)
cat("============================================================\n")
