# ============================================================
# 19_baseline.R
# Baseline models for international soccer match outcome prediction
#
# Target:
#   match_result ∈ {H, D, A}
#
# Models:
#   1. Class-frequency baseline
#   2. Majority-class baseline with epsilon smoothing
#   3. rating_diff multinomial logistic regression
#   4. rating_diff + neutral multinomial logistic regression (if neutral valid)
#
# Outputs:
#   reports/tables/baseline_model_comparison.csv
#   reports/tables/baseline_validation_calibration.csv
#   reports/tables/baseline_test_calibration.csv
#
#   reports/figures/baseline_class_distribution.png
#   reports/figures/baseline_rating_diff_by_result.png
#   reports/figures/baseline_elo_probability_curves.png
#   reports/figures/baseline_validation_calibration.png
#   reports/figures/baseline_test_calibration.png
#   reports/figures/baseline_metric_comparison.png
#   reports/figures/baseline_metric_comparison_lower_is_better.png
#   reports/figures/baseline_metric_comparison_higher_is_better.png
#   reports/figures/baseline_confusion_matrix.png
#
#   reports/tables/baseline_classwise_metrics.csv
#   reports/tables/baseline_confusion_matrix.csv
#   reports/tables/baseline_confident_wrong_predictions.csv
# ============================================================


# -----------------------------
# 0. Setup
# -----------------------------

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


# -----------------------------
# 1. Load data
# -----------------------------

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


# -----------------------------
# 2. Filter candidate rows
# -----------------------------

target_levels <- c("H", "D", "A")

df <- raw_df %>%
    filter(
        match_result %in% target_levels,
        !is.na(rating_diff),
        data_split %in% c("train", "test")
    ) %>%
    mutate(
        match_result = factor(match_result, levels = target_levels),
        data_split = factor(data_split, levels = c("train", "test"))
    )

if (nrow(df) == 0) {
    stop("No rows remain after filtering candidate rows.", call. = FALSE)
}

if (!identical(levels(df$match_result), target_levels)) {
    stop("match_result factor levels are not c('H', 'D', 'A').", call. = FALSE)
}


# -----------------------------
# 3. Neutral handling
# -----------------------------

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
            "Skipping neutral-based models."
        )
    }
} else {
    message("neutral column not found. Skipping neutral-based models.")
}


# -----------------------------
# 4. Leakage guard
# -----------------------------

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
    majority_baseline = character(0),
    rating_diff_multinom = c("rating_diff"),
    rating_diff_neutral_multinom = c("rating_diff", "neutral_model")
)

for (allowlist_name in names(predictor_allowlists)) {
    bad_predictors <- intersect(predictor_allowlists[[allowlist_name]], leakage_cols)

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


# -----------------------------
# 5. Train / test split
# -----------------------------

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


# -----------------------------
# 6. Chronological validation split
# -----------------------------

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


# -----------------------------
# 7. Utility functions
# -----------------------------

prob_cols <- c(".pred_H", ".pred_D", ".pred_A")
prob_drift_tolerance <- 1e-6
prob_error_tolerance <- 1e-3

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


normalize_probs <- function(prob_matrix) {
    prob_matrix <- pmax(prob_matrix, 0)
    row_sums <- rowSums(prob_matrix)

    if (any(row_sums == 0)) {
        stop("At least one probability row sums to zero.", call. = FALSE)
    }

    prob_matrix / row_sums
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


make_majority_probs <- function(training_data, epsilon = 0.01) {
    majority_class <- training_data %>%
        count(match_result, sort = TRUE) %>%
        slice(1) %>%
        pull(match_result) %>%
        as.character()

    probs <- rep(epsilon, length(target_levels))
    names(probs) <- target_levels
    probs[majority_class] <- 1 - 2 * epsilon
    probs / sum(probs)
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


prediction_store <- list()
metrics_table <- tibble()


# -----------------------------
# 8. Class-frequency baseline
# -----------------------------

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


# -----------------------------
# 9. Majority-class baseline
# -----------------------------

epsilon <- 0.01
majority_probs_inner <- make_majority_probs(inner_train, epsilon = epsilon)

majority_val_pred <- make_constant_probability_predictions(
    validation_df,
    majority_probs_inner
)

majority_test_pred <- make_constant_probability_predictions(
    test_df,
    majority_probs_inner
)

prediction_store$majority_baseline <- list(
    validation = majority_val_pred,
    test = majority_test_pred
)

metrics_table <- append_metrics_row(
    metrics_table,
    "majority_baseline",
    "validation",
    majority_val_pred
)
metrics_table <- append_metrics_row(
    metrics_table,
    "majority_baseline",
    "test",
    majority_test_pred
)


# -----------------------------
# 10. rating_diff multinomial logistic regression
# -----------------------------

rating_diff_model <- nnet::multinom(
    match_result ~ rating_diff,
    data = inner_train,
    trace = FALSE
)

rating_diff_val_pred <- predict_multinom_probs(
    rating_diff_model,
    validation_df
)

rating_diff_test_pred <- predict_multinom_probs(
    rating_diff_model,
    test_df
)

prediction_store$rating_diff_multinom <- list(
    validation = rating_diff_val_pred,
    test = rating_diff_test_pred,
    model = rating_diff_model
)

metrics_table <- append_metrics_row(
    metrics_table,
    "rating_diff_multinom",
    "validation",
    rating_diff_val_pred
)
metrics_table <- append_metrics_row(
    metrics_table,
    "rating_diff_multinom",
    "test",
    rating_diff_test_pred
)


# -----------------------------
# 11. rating_diff + neutral multinomial logistic regression
# -----------------------------

if (neutral_model_valid) {
    rating_diff_neutral_model <- nnet::multinom(
        match_result ~ rating_diff + neutral_model,
        data = inner_train,
        trace = FALSE
    )

    rating_diff_neutral_val_pred <- predict_multinom_probs(
        rating_diff_neutral_model,
        validation_df
    )

    rating_diff_neutral_test_pred <- predict_multinom_probs(
        rating_diff_neutral_model,
        test_df
    )

    prediction_store$rating_diff_neutral_multinom <- list(
        validation = rating_diff_neutral_val_pred,
        test = rating_diff_neutral_test_pred,
        model = rating_diff_neutral_model
    )

    metrics_table <- append_metrics_row(
        metrics_table,
        "rating_diff_neutral_multinom",
        "validation",
        rating_diff_neutral_val_pred
    )
    metrics_table <- append_metrics_row(
        metrics_table,
        "rating_diff_neutral_multinom",
        "test",
        rating_diff_neutral_test_pred
    )
} else {
    message("Skipping rating_diff + neutral_model multinomial model.")
}


# -----------------------------
# 12. Save model comparison table
# -----------------------------

metrics_table <- metrics_table %>%
    arrange(model, split)

readr::write_csv(
    metrics_table,
    "reports/tables/baseline_model_comparison.csv"
)

print(metrics_table)


# -----------------------------
# 12b. Classwise metrics and confusion matrices
# -----------------------------

baseline_splits <- c("validation", "test")

classwise_metrics_table <- purrr::map_dfr(
    names(prediction_store),
    function(model_name) {
        purrr::map_dfr(
            baseline_splits,
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
    "reports/tables/baseline_classwise_metrics.csv"
)

confusion_matrix_table <- purrr::map_dfr(
    names(prediction_store),
    function(model_name) {
        purrr::map_dfr(
            baseline_splits,
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
    "reports/tables/baseline_confusion_matrix.csv"
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
            "rating_diff"
        )),
        match_result,
        predicted_result,
        confidence,
        .pred_H,
        .pred_D,
        .pred_A,
        dplyr::any_of(c("home_score", "away_score"))
    )

readr::write_csv(
    confident_wrong_predictions,
    "reports/tables/baseline_confident_wrong_predictions.csv"
)


# -----------------------------
# 13. Calibration tables
# -----------------------------

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
    "reports/tables/baseline_validation_calibration.csv"
)

readr::write_csv(
    test_calibration,
    "reports/tables/baseline_test_calibration.csv"
)


# -----------------------------
# 14. Plots
# -----------------------------

class_distribution_plot <- df %>%
    count(data_split, match_result) %>%
    group_by(data_split) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(x = match_result, y = prop, fill = data_split)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
        title = "Class distribution: train vs test",
        x = "Match result",
        y = "Share of matches",
        fill = "Split"
    ) +
    theme_minimal()

ggsave(
    "reports/figures/baseline_class_distribution.png",
    class_distribution_plot,
    width = 8,
    height = 5,
    dpi = 300
)

rating_diff_plot <- inner_train %>%
    ggplot(aes(x = rating_diff, fill = match_result)) +
    geom_density(alpha = 0.35) +
    labs(
        title = "Inner-training rating_diff distribution by result",
        x = "rating_diff",
        y = "Density",
        fill = "Result"
    ) +
    theme_minimal()

ggsave(
    "reports/figures/baseline_rating_diff_by_result.png",
    rating_diff_plot,
    width = 8,
    height = 5,
    dpi = 300
)

rating_grid <- tibble(
    rating_diff = seq(
        min(inner_train$rating_diff, na.rm = TRUE),
        max(inner_train$rating_diff, na.rm = TRUE),
        length.out = 300
    )
)

rating_grid_probs <- predict(
    rating_diff_model,
    newdata = rating_grid,
    type = "probs"
)

if (is.vector(rating_grid_probs)) {
    rating_grid_probs <- matrix(rating_grid_probs, nrow = 1)
}

rating_grid_probs <- as.data.frame(rating_grid_probs)

for (level_name in setdiff(target_levels, names(rating_grid_probs))) {
    rating_grid_probs[[level_name]] <- 0
}

elo_curve_df <- bind_cols(
    rating_grid,
    rating_grid_probs[, target_levels, drop = FALSE]
) %>%
    pivot_longer(
        cols = all_of(target_levels),
        names_to = "class",
        values_to = "predicted_probability"
    )

elo_probability_curve_plot <- elo_curve_df %>%
    ggplot(aes(x = rating_diff, y = predicted_probability, color = class)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
        title = "rating_diff multinomial probability curves",
        x = "rating_diff",
        y = "Predicted probability",
        color = "Result"
    ) +
    theme_minimal()

ggsave(
    "reports/figures/baseline_elo_probability_curves.png",
    elo_probability_curve_plot,
    width = 8,
    height = 5,
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

validation_calibration_plot <- plot_calibration_curves(
    validation_calibration,
    "Validation calibration by model"
)

test_calibration_plot <- plot_calibration_curves(
    test_calibration,
    "Test calibration by model"
)

ggsave(
    "reports/figures/baseline_validation_calibration.png",
    validation_calibration_plot,
    width = 10,
    height = 6,
    dpi = 300
)

ggsave(
    "reports/figures/baseline_test_calibration.png",
    test_calibration_plot,
    width = 10,
    height = 6,
    dpi = 300
)

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

metric_comparison_plot <- make_metric_comparison_plot(
    metrics_long,
    title_text = "Baseline metric comparison",
    subtitle_text = "Lower is better: log_loss, brier. Higher is better: accuracy, macro_f1."
)

ggsave(
    "reports/figures/baseline_metric_comparison.png",
    metric_comparison_plot,
    width = 11,
    height = 7,
    dpi = 300
)

metric_comparison_lower_plot <- metrics_long %>%
    filter(metric %in% c("log_loss", "brier")) %>%
    make_metric_comparison_plot(
        title_text = "Baseline metrics (lower is better)",
        subtitle_text = "log_loss and brier — lower values indicate better calibration / scoring."
    )

metric_comparison_higher_plot <- metrics_long %>%
    filter(metric %in% c("accuracy", "macro_f1")) %>%
    make_metric_comparison_plot(
        title_text = "Baseline metrics (higher is better)",
        subtitle_text = "accuracy and macro_f1 — higher values indicate better discrimination."
    )

ggsave(
    "reports/figures/baseline_metric_comparison_lower_is_better.png",
    metric_comparison_lower_plot,
    width = 10,
    height = 5,
    dpi = 300
)

ggsave(
    "reports/figures/baseline_metric_comparison_higher_is_better.png",
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
        title = "Test confusion matrix: best baseline model",
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
    "reports/figures/baseline_confusion_matrix.png",
    confusion_matrix_plot,
    width = 7,
    height = 6,
    dpi = 300
)


# -----------------------------
# 15. Final summary
# -----------------------------

best_validation_row <- metrics_table %>%
    filter(split == "validation") %>%
    slice_min(log_loss, n = 1, with_ties = FALSE)

best_test_row <- metrics_table %>%
    filter(split == "test") %>%
    slice_min(log_loss, n = 1, with_ties = FALSE)

cat("\n")
cat("============================================================\n")
cat("Baseline modeling complete\n")
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
cat("============================================================\n")




