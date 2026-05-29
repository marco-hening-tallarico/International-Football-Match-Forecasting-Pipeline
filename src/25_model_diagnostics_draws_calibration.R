# 25_model_diagnostics_draws_calibration.R
#
# Post-hoc diagnostics for Model 24: draw calibration, confusion patterns,
# high-confidence errors, and alternate draw decision rules.
#
# Reads:
# - reports/tables/model_24_predictions.csv
# - reports/tables/model_24_classwise_metrics.csv
# - reports/tables/model_24_confusion_matrices.csv
# - data/processed/international_modeling_table.csv
#
# Writes:
# - reports/tables/model_25_*.csv
# - reports/figures/model_25_*.png

set.seed(2026)

# 0. Project setup

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

dir.create("reports/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)

# 1. Required packages

required_pkgs <- c(
    "readr",
    "dplyr",
    "tidyr",
    "tibble",
    "ggplot2",
    "scales"
)

missing_required <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_required) > 0) {
    stop(
        "Missing required packages: ",
        paste(missing_required, collapse = ", "),
        call. = FALSE
    )
}

# 1b. Draw-rank helper functions

# Tie-break order for probability ranks: H > D > A when probabilities are equal.
# Rank 1 = highest probability (after tie-break); rank 3 = lowest.
# argmax_class_from_probs uses max.col(..., ties.method = "first"), so H wins
# over D over A on exact ties — consistent with the rank helpers above.
compute_pred_d_rank <- function(pred_H, pred_D, pred_A) {
    prob_matrix <- cbind(pred_H, pred_D, pred_A)
    vapply(seq_len(nrow(prob_matrix)), function(row_index) {
        row_probs <- prob_matrix[row_index, ]
        sorted_index <- order(
            -row_probs,
            seq_along(row_probs),
            method = "radix"
        )
        which(sorted_index == 2L)
    }, integer(1))
}

compute_second_pred_prob <- function(pred_H, pred_D, pred_A) {
    prob_matrix <- cbind(pred_H, pred_D, pred_A)
    vapply(seq_len(nrow(prob_matrix)), function(row_index) {
        row_probs <- prob_matrix[row_index, ]
        sorted_probs <- row_probs[order(
            -row_probs,
            seq_along(row_probs),
            method = "radix"
        )]
        sorted_probs[2L]
    }, numeric(1))
}

argmax_class_from_probs <- function(pred_H, pred_D, pred_A) {
    prob_matrix <- cbind(pred_H, pred_D, pred_A)
    c("H", "D", "A")[max.col(prob_matrix, ties.method = "first")]
}

validate_prediction_probabilities <- function(predictions) {
    prob_cols <- c("pred_H", "pred_D", "pred_A")

    if (any(is.na(predictions[, prob_cols]))) {
        stop("Predictions contain NA values in pred_H, pred_D, or pred_A.", call. = FALSE)
    }

    prob_matrix <- as.matrix(predictions[, prob_cols])

    if (any(!is.finite(prob_matrix))) {
        stop("Predictions contain non-finite values in probability columns.", call. = FALSE)
    }

    row_sums <- rowSums(prob_matrix)

    if (any(abs(row_sums - 1) > 1e-6)) {
        stop("Prediction probability rows do not sum approximately to 1.", call. = FALSE)
    }

    valid_results <- c("H", "D", "A")

    if (any(!predictions$match_result %in% valid_results)) {
        stop(
            "match_result contains values outside H, D, A.",
            call. = FALSE
        )
    }

    invisible(TRUE)
}

multiclass_log_loss <- function(truth, probs) {
    prob_matrix <- as.matrix(probs[, c("pred_H", "pred_D", "pred_A")])
    prob_matrix <- pmin(pmax(prob_matrix, 1e-15), 1)

    truth <- as.character(truth)
    class_index <- match(truth, c("H", "D", "A"))

    if (any(is.na(class_index))) {
        stop("Truth contains values outside H, D, A.", call. = FALSE)
    }

    -mean(log(prob_matrix[cbind(seq_along(class_index), class_index)]))
}

classwise_metrics_from_pred <- function(truth, pred, model_name, split_name) {
    truth <- as.character(truth)
    pred <- as.character(pred)

    dplyr::bind_rows(lapply(c("H", "D", "A"), function(class_label) {
        true_positive <- sum(pred == class_label & truth == class_label)
        false_positive <- sum(pred == class_label & truth != class_label)
        false_negative <- sum(pred != class_label & truth == class_label)

        precision <- if (true_positive + false_positive == 0) {
            0
        } else {
            true_positive / (true_positive + false_positive)
        }

        recall <- if (true_positive + false_negative == 0) {
            0
        } else {
            true_positive / (true_positive + false_negative)
        }

        f1 <- if (precision + recall == 0) {
            0
        } else {
            2 * precision * recall / (precision + recall)
        }

        tibble::tibble(
            model = model_name,
            split = split_name,
            class = class_label,
            precision = precision,
            recall = recall,
            f1 = f1
        )
    }))
}

macro_f1_from_pred <- function(truth, pred) {
    metrics <- classwise_metrics_from_pred(truth, pred, "tmp", "tmp")
    mean(metrics$f1)
}

apply_alternate_draw_rule <- function(pred_H, pred_D, pred_A, threshold) {
    max_pred_prob <- pmax(pred_H, pred_D, pred_A)
    argmax_class <- argmax_class_from_probs(pred_H, pred_D, pred_A)
    draw_within_threshold <- (max_pred_prob - pred_D) <= threshold

    dplyr::if_else(draw_within_threshold, "D", argmax_class)
}

evaluate_alternate_draw_threshold <- function(
    truth,
    pred_H,
    pred_D,
    pred_A,
    threshold,
    model_name,
    split_name
) {
    pred_class <- apply_alternate_draw_rule(pred_H, pred_D, pred_A, threshold)
    probs <- tibble::tibble(pred_H = pred_H, pred_D = pred_D, pred_A = pred_A)
    class_metrics <- classwise_metrics_from_pred(
        truth,
        pred_class,
        model_name,
        split_name
    )
    draw_metrics <- class_metrics |>
        dplyr::filter(class == "D")

    tibble::tibble(
        model = model_name,
        split = split_name,
        threshold = threshold,
        n = length(truth),
        accuracy = mean(pred_class == truth),
        macro_f1 = macro_f1_from_pred(truth, pred_class),
        draw_recall = draw_metrics$recall[1],
        draw_precision = draw_metrics$precision[1],
        predicted_draw_rate = mean(pred_class == "D"),
        actual_draw_rate = mean(truth == "D"),
        # Log loss uses original probabilities only; threshold changes hard labels,
        # not the probability estimates, so log_loss is identical across thresholds.
        log_loss = multiclass_log_loss(truth, probs)
    )
}

# 2. Load inputs

prediction_path <- "reports/tables/model_24_predictions.csv"
modeling_path <- "data/processed/international_modeling_table.csv"
classwise_path <- "reports/tables/model_24_classwise_metrics.csv"
confusion_path <- "reports/tables/model_24_confusion_matrices.csv"

if (!file.exists(prediction_path)) {
    stop("Missing predictions file. Run src/24_model_glm_lightgbm_approved_features.R first.", call. = FALSE)
}

if (!file.exists(modeling_path)) {
    stop("Missing modeling table: ", modeling_path, call. = FALSE)
}

predictions <- readr::read_csv(prediction_path, show_col_types = FALSE)
modeling <- readr::read_csv(modeling_path, show_col_types = FALSE)

message("Loaded predictions rows: ", nrow(predictions))
message("Loaded modeling rows: ", nrow(modeling))

validate_prediction_probabilities(predictions)

# 3. Validate expected columns

required_prediction_cols <- c(
    "model",
    "split",
    "source_match_id",
    "date",
    "match_result",
    "pred_class",
    "pred_H",
    "pred_D",
    "pred_A"
)

missing_prediction_cols <- setdiff(required_prediction_cols, names(predictions))

if (length(missing_prediction_cols) > 0) {
    stop(
        "Predictions file is missing columns: ",
        paste(missing_prediction_cols, collapse = ", "),
        call. = FALSE
    )
}

required_modeling_cols <- c(
    "source_match_id",
    "rating_diff",
    "neutral",
    "is_world_cup",
    "is_world_cup_qualifier",
    "is_continental_tournament",
    "is_continental_qualifier",
    "is_friendly"
)

missing_modeling_cols <- setdiff(required_modeling_cols, names(modeling))

if (length(missing_modeling_cols) > 0) {
    stop(
        "Modeling table is missing columns needed for diagnostics: ",
        paste(missing_modeling_cols, collapse = ", "),
        call. = FALSE
    )
}

# 4. Join predictions to modeling features

diagnostic_data <- predictions |>
    dplyr::left_join(
        modeling |>
            dplyr::select(
                dplyr::all_of(required_modeling_cols),
                dplyr::any_of(c(
                    "home_team_clean",
                    "away_team_clean",
                    "tournament",
                    "competition",
                    "country",
                    "city",
                    "season"
                ))
            ),
        by = "source_match_id"
    ) |>
    dplyr::mutate(
        date = as.Date(date),
        match_result = factor(match_result, levels = c("H", "D", "A")),
        pred_class = factor(pred_class, levels = c("H", "D", "A")),
        correct = match_result == pred_class,
        abs_rating_diff = abs(rating_diff),
        max_pred_prob = pmax(pred_H, pred_D, pred_A),
        pred_D_rank = compute_pred_d_rank(pred_H, pred_D, pred_A),
        second_pred_prob = compute_second_pred_prob(pred_H, pred_D, pred_A),
        draw_margin_to_top = max_pred_prob - pred_D,
        draw_margin_to_second = second_pred_prob - pred_D,
        draw_is_top = pred_D == max_pred_prob,
        draw_is_second = pred_D_rank == 2L,
        draw_is_bottom = pred_D_rank == 3L,
        draw_within_2pts_of_top = draw_margin_to_top <= 0.02,
        draw_within_5pts_of_top = draw_margin_to_top <= 0.05,
        draw_within_10pts_of_top = draw_margin_to_top <= 0.10,
        true_class_prob = dplyr::case_when(
            match_result == "H" ~ pred_H,
            match_result == "D" ~ pred_D,
            match_result == "A" ~ pred_A,
            TRUE ~ NA_real_
        ),
        predicted_draw = pred_class == "D",
        actual_draw = match_result == "D"
    )

if (any(is.na(diagnostic_data$rating_diff))) {
    message("Warning: some joined rows have missing rating_diff.")
}

# 5. Rating-difference buckets

diagnostic_data <- diagnostic_data |>
    dplyr::mutate(
        abs_rating_diff_bucket = cut(
            abs_rating_diff,
            breaks = c(-Inf, 25, 50, 100, 150, 250, 400, Inf),
            labels = c(
                "0-25",
                "25-50",
                "50-100",
                "100-150",
                "150-250",
                "250-400",
                "400+"
            ),
            right = TRUE
        )
    )

# 6. Overall draw diagnostics by model

draw_diagnostics_by_model <- diagnostic_data |>
    dplyr::group_by(model, split) |>
    dplyr::summarise(
        n = dplyr::n(),
        actual_draw_rate = mean(actual_draw),
        predicted_draw_rate = mean(predicted_draw),
        mean_pred_D = mean(pred_D),
        median_pred_D = median(pred_D),
        draw_recall = sum(predicted_draw & actual_draw) / sum(actual_draw),
        draw_precision = dplyr::if_else(
            sum(predicted_draw) == 0,
            0,
            sum(predicted_draw & actual_draw) / sum(predicted_draw)
        ),
        avg_true_class_prob = mean(true_class_prob),
        avg_max_pred_prob = mean(max_pred_prob),
        accuracy = mean(correct),
        .groups = "drop"
    ) |>
    dplyr::arrange(split, model)

readr::write_csv(
    draw_diagnostics_by_model,
    "reports/tables/model_25_draw_diagnostics_by_model.csv"
)

print(draw_diagnostics_by_model)

# 7. Draw diagnostics by rating bucket

draw_diagnostics_by_rating_bucket <- diagnostic_data |>
    dplyr::group_by(model, split, abs_rating_diff_bucket) |>
    dplyr::summarise(
        n = dplyr::n(),
        actual_draw_rate = mean(actual_draw),
        predicted_draw_rate = mean(predicted_draw),
        mean_pred_D = mean(pred_D),
        draw_recall = dplyr::if_else(
            sum(actual_draw) == 0,
            0,
            sum(predicted_draw & actual_draw) / sum(actual_draw)
        ),
        draw_precision = dplyr::if_else(
            sum(predicted_draw) == 0,
            0,
            sum(predicted_draw & actual_draw) / sum(predicted_draw)
        ),
        accuracy = mean(correct),
        .groups = "drop"
    ) |>
    dplyr::arrange(split, model, abs_rating_diff_bucket)

readr::write_csv(
    draw_diagnostics_by_rating_bucket,
    "reports/tables/model_25_draw_diagnostics_by_rating_bucket.csv"
)

# 8. Draw probability summary

draw_probability_summary <- diagnostic_data |>
    dplyr::group_by(model, split, match_result) |>
    dplyr::summarise(
        n = dplyr::n(),
        mean_pred_D = mean(pred_D),
        median_pred_D = median(pred_D),
        q10_pred_D = stats::quantile(pred_D, 0.10),
        q25_pred_D = stats::quantile(pred_D, 0.25),
        q75_pred_D = stats::quantile(pred_D, 0.75),
        q90_pred_D = stats::quantile(pred_D, 0.90),
        .groups = "drop"
    ) |>
    dplyr::arrange(split, model, match_result)

readr::write_csv(
    draw_probability_summary,
    "reports/tables/model_25_draw_probability_summary.csv"
)

# 9. High-confidence wrong predictions

high_confidence_wrong <- diagnostic_data |>
    dplyr::filter(
        split == "test",
        !correct
    ) |>
    dplyr::arrange(dplyr::desc(max_pred_prob)) |>
    dplyr::select(
        model,
        split,
        source_match_id,
        date,
        dplyr::any_of(c("home_team_clean", "away_team_clean", "tournament", "country", "city")),
        match_result,
        pred_class,
        pred_H,
        pred_D,
        pred_A,
        max_pred_prob,
        true_class_prob,
        rating_diff,
        abs_rating_diff,
        neutral,
        is_world_cup,
        is_world_cup_qualifier,
        is_continental_tournament,
        is_continental_qualifier,
        is_friendly
    ) |>
    dplyr::slice_head(n = 200)

readr::write_csv(
    high_confidence_wrong,
    "reports/tables/model_25_high_confidence_wrong.csv"
)

# 10. Confusion heatmap from predictions

confusion_from_predictions <- diagnostic_data |>
    dplyr::count(model, split, match_result, pred_class, name = "n") |>
    dplyr::group_by(model, split, match_result) |>
    dplyr::mutate(row_pct = n / sum(n)) |>
    dplyr::ungroup()

confusion_heatmap <- confusion_from_predictions |>
    dplyr::filter(split == "test") |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = pred_class,
            y = match_result,
            fill = row_pct
        )
    ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
        ggplot2::aes(label = scales::percent(row_pct, accuracy = 0.1)),
        size = 3
    ) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::labs(
        title = "Model 25 test-set confusion heatmap",
        x = "Predicted class",
        y = "Actual class",
        fill = "Row %"
    )

ggplot2::ggsave(
    filename = "reports/figures/model_25_confusion_heatmap.png",
    plot = confusion_heatmap,
    width = 10,
    height = 5,
    dpi = 300
)

# 11. Plot: actual draw rate vs predicted draw probability by rating bucket

draw_bucket_plot_data <- draw_diagnostics_by_rating_bucket |>
    dplyr::filter(split == "test")

draw_rate_by_abs_rating_diff_plot <- draw_bucket_plot_data |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = abs_rating_diff_bucket,
            group = model
        )
    ) +
    ggplot2::geom_line(
        ggplot2::aes(y = actual_draw_rate, linetype = "Actual draw rate")
    ) +
    ggplot2::geom_point(
        ggplot2::aes(y = actual_draw_rate, shape = "Actual draw rate")
    ) +
    ggplot2::geom_line(
        ggplot2::aes(y = mean_pred_D, linetype = "Mean predicted draw probability")
    ) +
    ggplot2::geom_point(
        ggplot2::aes(y = mean_pred_D, shape = "Mean predicted draw probability")
    ) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::labs(
        title = "Model 25 draw rate by absolute rating difference",
        x = "|rating_diff| bucket",
        y = "Rate / probability",
        linetype = NULL,
        shape = NULL
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggsave(
    filename = "reports/figures/model_25_draw_rate_by_abs_rating_diff.png",
    plot = draw_rate_by_abs_rating_diff_plot,
    width = 11,
    height = 6,
    dpi = 300
)

# 12. Plot: draw calibration by model

draw_calibration <- diagnostic_data |>
    dplyr::filter(split == "test") |>
    dplyr::group_by(model) |>
    dplyr::mutate(draw_prob_bin = dplyr::ntile(pred_D, 10)) |>
    dplyr::ungroup() |>
    dplyr::group_by(model, draw_prob_bin) |>
    dplyr::summarise(
        n = dplyr::n(),
        mean_pred_D = mean(pred_D),
        observed_draw_rate = mean(actual_draw),
        .groups = "drop"
    )

draw_calibration_plot <- draw_calibration |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = mean_pred_D,
            y = observed_draw_rate
        )
    ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ model) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
        title = "Model 25 draw calibration on test set",
        x = "Mean predicted draw probability",
        y = "Observed draw rate"
    )

ggplot2::ggsave(
    filename = "reports/figures/model_25_draw_calibration_by_model.png",
    plot = draw_calibration_plot,
    width = 10,
    height = 5,
    dpi = 300
)

# 13. Plot: high-confidence wrong predictions by actual/predicted class

high_conf_wrong_summary <- diagnostic_data |>
    dplyr::filter(
        split == "test",
        !correct,
        max_pred_prob >= 0.60
    ) |>
    dplyr::count(model, match_result, pred_class, name = "n") |>
    dplyr::arrange(model, dplyr::desc(n))

high_conf_wrong_plot <- high_conf_wrong_summary |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = match_result,
            y = n,
            fill = pred_class
        )
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~ model) +
    ggplot2::labs(
        title = "Model 25 high-confidence wrong predictions on test set",
        subtitle = "Only wrong predictions with max predicted probability >= 0.60",
        x = "Actual class",
        y = "Count",
        fill = "Predicted class"
    )

ggplot2::ggsave(
    filename = "reports/figures/model_25_high_confidence_wrong_by_class.png",
    plot = high_conf_wrong_plot,
    width = 10,
    height = 5,
    dpi = 300
)

# 15. Draw rank / margin-to-top diagnostics by model

draw_rank_diagnostics_by_model <- diagnostic_data |>
    dplyr::group_by(model, split) |>
    dplyr::summarise(
        n = dplyr::n(),
        actual_draw_rate = mean(actual_draw),
        mean_pred_D = mean(pred_D),
        median_pred_D = median(pred_D),
        draw_top_rate = mean(draw_is_top),
        draw_second_rate = mean(draw_is_second),
        draw_bottom_rate = mean(draw_is_bottom),
        draw_within_2pts_of_top_rate = mean(draw_within_2pts_of_top),
        draw_within_5pts_of_top_rate = mean(draw_within_5pts_of_top),
        draw_within_10pts_of_top_rate = mean(draw_within_10pts_of_top),
        mean_draw_margin_to_top = mean(draw_margin_to_top),
        median_draw_margin_to_top = stats::median(draw_margin_to_top),
        q25_draw_margin_to_top = stats::quantile(draw_margin_to_top, 0.25),
        q75_draw_margin_to_top = stats::quantile(draw_margin_to_top, 0.75),
        mean_draw_margin_to_top_when_actual_draw = mean(
            draw_margin_to_top[actual_draw]
        ),
        median_draw_margin_to_top_when_actual_draw = stats::median(
            draw_margin_to_top[actual_draw]
        ),
        actual_draws_where_draw_within_5pts_of_top_rate = mean(
            draw_within_5pts_of_top[actual_draw]
        ),
        actual_draws_where_draw_within_10pts_of_top_rate = mean(
            draw_within_10pts_of_top[actual_draw]
        ),
        .groups = "drop"
    ) |>
    dplyr::arrange(split, model)

readr::write_csv(
    draw_rank_diagnostics_by_model,
    "reports/tables/model_25_draw_rank_diagnostics_by_model.csv"
)

message("Draw rank diagnostics by model:")
print(draw_rank_diagnostics_by_model)

# 16. Draw rank diagnostics by rating bucket

draw_rank_diagnostics_by_rating_bucket <- diagnostic_data |>
    dplyr::group_by(model, split, abs_rating_diff_bucket) |>
    dplyr::summarise(
        n = dplyr::n(),
        actual_draw_rate = mean(actual_draw),
        mean_pred_D = mean(pred_D),
        draw_top_rate = mean(draw_is_top),
        draw_second_rate = mean(draw_is_second),
        draw_within_5pts_of_top_rate = mean(draw_within_5pts_of_top),
        draw_within_10pts_of_top_rate = mean(draw_within_10pts_of_top),
        mean_draw_margin_to_top = mean(draw_margin_to_top),
        median_draw_margin_to_top = stats::median(draw_margin_to_top),
        accuracy = mean(correct),
        .groups = "drop"
    ) |>
    dplyr::arrange(split, model, abs_rating_diff_bucket)

readr::write_csv(
    draw_rank_diagnostics_by_rating_bucket,
    "reports/tables/model_25_draw_rank_diagnostics_by_rating_bucket.csv"
)

# 17. Draw near-miss examples (test actual draws almost predicted as D)

draw_near_miss_examples <- diagnostic_data |>
    dplyr::filter(
        split == "test",
        match_result == "D",
        !draw_is_top,
        draw_margin_to_top <= 0.05
    ) |>
    dplyr::arrange(draw_margin_to_top, dplyr::desc(pred_D)) |>
    dplyr::select(
        model,
        split,
        source_match_id,
        date,
        dplyr::any_of(c(
            "home_team_clean",
            "away_team_clean",
            "tournament",
            "country",
            "city"
        )),
        match_result,
        pred_class,
        pred_H,
        pred_D,
        pred_A,
        max_pred_prob,
        pred_D_rank,
        draw_margin_to_top,
        rating_diff,
        abs_rating_diff,
        neutral,
        is_world_cup,
        is_world_cup_qualifier,
        is_continental_tournament,
        is_continental_qualifier,
        is_friendly
    ) |>
    dplyr::slice_head(n = 200)

readr::write_csv(
    draw_near_miss_examples,
    "reports/tables/model_25_draw_near_miss_examples.csv"
)

# 18. Alternate draw-rule threshold tradeoffs

draw_rule_thresholds <- c(0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.10)

model_split_groups <- diagnostic_data |>
    dplyr::distinct(model, split)

alternative_draw_rule_thresholds <- dplyr::bind_rows(
    lapply(seq_len(nrow(model_split_groups)), function(group_index) {
        model_name <- model_split_groups$model[group_index]
        split_name <- model_split_groups$split[group_index]

        split_data <- diagnostic_data |>
            dplyr::filter(
                model == model_name,
                split == split_name
            )

        dplyr::bind_rows(lapply(draw_rule_thresholds, function(threshold) {
            evaluate_alternate_draw_threshold(
                truth = split_data$match_result,
                pred_H = split_data$pred_H,
                pred_D = split_data$pred_D,
                pred_A = split_data$pred_A,
                threshold = threshold,
                model_name = model_name,
                split_name = split_name
            )
        }))
    })
) |>
    dplyr::arrange(model, split, threshold)

readr::write_csv(
    alternative_draw_rule_thresholds,
    "reports/tables/model_25_alternative_draw_rule_thresholds.csv"
)

# 19. Figure: draw margin to top distribution (test)

draw_margin_distribution_plot <- diagnostic_data |>
    dplyr::filter(split == "test") |>
    ggplot2::ggplot(
        ggplot2::aes(x = draw_margin_to_top)
    ) +
    ggplot2::geom_histogram(
        bins = 40,
        fill = "#4575b4",
        color = "white"
    ) +
    ggplot2::facet_wrap(~ model, scales = "free_y") +
    ggplot2::labs(
        title = "Model 25 draw margin to top probability (test set)",
        subtitle = "draw_margin_to_top = max(pred_H, pred_D, pred_A) - pred_D",
        x = "Draw margin to top",
        y = "Count"
    )

ggplot2::ggsave(
    filename = "reports/figures/model_25_draw_margin_distribution.png",
    plot = draw_margin_distribution_plot,
    width = 10,
    height = 5,
    dpi = 300
)

# 20. Figure: draw margin by actual class (test)

draw_margin_by_actual_class_plot <- diagnostic_data |>
    dplyr::filter(split == "test") |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = match_result,
            y = draw_margin_to_top,
            fill = match_result
        )
    ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.2) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::labs(
        title = "Model 25 draw margin to top by actual outcome (test set)",
        x = "Actual class",
        y = "Draw margin to top",
        fill = "Actual class"
    ) +
    ggplot2::theme(legend.position = "none")

ggplot2::ggsave(
    filename = "reports/figures/model_25_draw_margin_by_actual_class.png",
    plot = draw_margin_by_actual_class_plot,
    width = 10,
    height = 5,
    dpi = 300
)

# 21. Figure: draw near-top rate by rating bucket (test)

draw_near_top_rate_plot_data <- draw_rank_diagnostics_by_rating_bucket |>
    dplyr::filter(split == "test") |>
    tidyr::pivot_longer(
        cols = c(
            draw_within_5pts_of_top_rate,
            draw_within_10pts_of_top_rate
        ),
        names_to = "near_top_metric",
        values_to = "rate"
    ) |>
    dplyr::mutate(
        near_top_metric = dplyr::recode(
            near_top_metric,
            draw_within_5pts_of_top_rate = "Within 5 pts of top",
            draw_within_10pts_of_top_rate = "Within 10 pts of top"
        )
    )

draw_near_top_rate_by_rating_bucket_plot <- draw_near_top_rate_plot_data |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = abs_rating_diff_bucket,
            y = rate,
            color = near_top_metric,
            group = near_top_metric
        )
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
        title = "Model 25 draw near-top rate by |rating_diff| bucket (test set)",
        x = "|rating_diff| bucket",
        y = "Share of matches",
        color = NULL
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggsave(
    filename = "reports/figures/model_25_draw_near_top_rate_by_rating_bucket.png",
    plot = draw_near_top_rate_by_rating_bucket_plot,
    width = 11,
    height = 6,
    dpi = 300
)

# 22. Figure: alternate draw-rule tradeoff (test)

alternative_draw_rule_tradeoff_plot_data <- alternative_draw_rule_thresholds |>
    dplyr::filter(split == "test") |>
    tidyr::pivot_longer(
        cols = c(accuracy, macro_f1, draw_recall, predicted_draw_rate),
        names_to = "metric",
        values_to = "value"
    ) |>
    dplyr::mutate(
        metric = dplyr::recode(
            metric,
            accuracy = "Accuracy",
            macro_f1 = "Macro F1",
            draw_recall = "Draw recall",
            predicted_draw_rate = "Predicted draw rate"
        )
    )

alternative_draw_rule_tradeoff_plot <- alternative_draw_rule_tradeoff_plot_data |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = threshold,
            y = value,
            color = metric,
            group = metric
        )
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
        title = "Model 25 alternate draw-rule threshold tradeoff (test set)",
        subtitle = "Override argmax to D when pred_D is within threshold of max probability",
        x = "Draw margin threshold",
        y = "Metric value",
        color = NULL
    )

ggplot2::ggsave(
    filename = "reports/figures/model_25_alternative_draw_rule_tradeoff.png",
    plot = alternative_draw_rule_tradeoff_plot,
    width = 11,
    height = 6,
    dpi = 300
)

# 23. Final summary

test_draw_rank_summary <- draw_rank_diagnostics_by_model |>
    dplyr::filter(split == "test")

test_threshold_summary <- alternative_draw_rule_thresholds |>
    dplyr::filter(split == "test")

draw_rank_mode <- function(draw_top_rate, draw_second_rate, draw_bottom_rate) {
    rates <- c(
        top = draw_top_rate,
        second = draw_second_rate,
        bottom = draw_bottom_rate
    )
    names(which.max(rates))
}

message("=================================================================")
message("Draw rank interpretation (test split):")
for (model_name in unique(test_draw_rank_summary$model)) {
    model_row <- test_draw_rank_summary |>
        dplyr::filter(model == model_name)

    dominant_rank <- draw_rank_mode(
        model_row$draw_top_rate,
        model_row$draw_second_rate,
        model_row$draw_bottom_rate
    )

    message(
        "- ", model_name, ": draw is most often ",
        dominant_rank,
        " (top=", scales::percent(model_row$draw_top_rate, accuracy = 0.1),
        ", second=", scales::percent(model_row$draw_second_rate, accuracy = 0.1),
        ", bottom=", scales::percent(model_row$draw_bottom_rate, accuracy = 0.1),
        "); within 5 pts of top on ",
        scales::percent(model_row$draw_within_5pts_of_top_rate, accuracy = 0.1),
        " of test rows"
    )

    baseline_accuracy <- test_threshold_summary |>
        dplyr::filter(model == model_name, threshold == 0) |>
        dplyr::pull(accuracy)

    threshold_005 <- test_threshold_summary |>
        dplyr::filter(model == model_name, threshold == 0.05)

    threshold_010 <- test_threshold_summary |>
        dplyr::filter(model == model_name, threshold == 0.10)

    message(
        "  Threshold rule: at t=0.05 draw_recall=",
        scales::percent(threshold_005$draw_recall, accuracy = 0.1),
        ", accuracy=",
        scales::percent(threshold_005$accuracy, accuracy = 0.1),
        " (baseline accuracy=",
        scales::percent(baseline_accuracy, accuracy = 0.1),
        "); at t=0.10 draw_recall=",
        scales::percent(threshold_010$draw_recall, accuracy = 0.1),
        ", accuracy=",
        scales::percent(threshold_010$accuracy, accuracy = 0.1)
    )
}
message("=================================================================")
message("Model 25 diagnostics complete.")
message("Tables written:")
message("- reports/tables/model_25_draw_diagnostics_by_model.csv")
message("- reports/tables/model_25_draw_diagnostics_by_rating_bucket.csv")
message("- reports/tables/model_25_high_confidence_wrong.csv")
message("- reports/tables/model_25_draw_probability_summary.csv")
message("- reports/tables/model_25_draw_rank_diagnostics_by_model.csv")
message("- reports/tables/model_25_draw_rank_diagnostics_by_rating_bucket.csv")
message("- reports/tables/model_25_draw_near_miss_examples.csv")
message("- reports/tables/model_25_alternative_draw_rule_thresholds.csv")
message("Figures written:")
message("- reports/figures/model_25_draw_rate_by_abs_rating_diff.png")
message("- reports/figures/model_25_draw_calibration_by_model.png")
message("- reports/figures/model_25_confusion_heatmap.png")
message("- reports/figures/model_25_high_confidence_wrong_by_class.png")
message("- reports/figures/model_25_draw_margin_distribution.png")
message("- reports/figures/model_25_draw_margin_by_actual_class.png")
message("- reports/figures/model_25_draw_near_top_rate_by_rating_bucket.png")
message("- reports/figures/model_25_alternative_draw_rule_tradeoff.png")
message("=================================================================")