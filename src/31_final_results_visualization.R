# 31_final_results_visualization.R
#
# Reporting-only: assembles cross-stage performance tables, figures, and a
# markdown summary from Model 24–28 outputs. Does not train models or change
# processed data.
#
# Reads: reports/tables/model_* and optional baseline tables
#
# Writes:
# - reports/final/*
# - reports/figures/final_model/ (legacy cross-stage plots when present)

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

dir.create("reports/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/final", recursive = TRUE, showWarnings = FALSE)

required_pkgs <- c(
    "readr",
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "ggplot2"
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

has_scales <- requireNamespace("scales", quietly = TRUE)

MATCH_RESULT_LEVELS <- c("H", "D", "A")

files_read <- character()
files_missing_optional <- character()

read_optional_csv <- function(relative_path) {
    full_path <- file.path(PROJECT_ROOT, relative_path)

    if (!file.exists(full_path)) {
        files_missing_optional <<- c(files_missing_optional, relative_path)
        warning("Optional file not found (skipping): ", relative_path, call. = FALSE)
        return(NULL)
    }

    files_read <<- c(files_read, relative_path)
    readr::read_csv(full_path, show_col_types = FALSE)
}

read_essential_csv <- function(relative_path) {
    full_path <- file.path(PROJECT_ROOT, relative_path)

    if (!file.exists(full_path)) {
        stop("Essential file not found: ", relative_path, call. = FALSE)
    }

    files_read <<- c(files_read, relative_path)
    readr::read_csv(full_path, show_col_types = FALSE)
}

# 1. Load essential and optional inputs

model_24_metrics <- read_essential_csv("reports/tables/model_24_metrics.csv")
model_28_metrics <- read_essential_csv("reports/tables/model_28_metrics.csv")
model_28_predictions <- read_essential_csv("reports/tables/model_28_predictions.csv")

model_26_metrics <- read_optional_csv("reports/tables/model_26_metrics.csv")
model_25_draw_diagnostics <- read_optional_csv(
    "reports/tables/model_25_draw_diagnostics_by_model.csv"
)
model_25_draw_rank_diagnostics <- read_optional_csv(
    "reports/tables/model_25_draw_rank_diagnostics_by_model.csv"
)
baseline_metrics <- read_optional_csv("reports/tables/baseline_model_comparison.csv")
baseline_plus_metrics <- read_optional_csv(
    "reports/tables/baseline_plus_model_comparison.csv"
)

baseline_files_available <- !is.null(baseline_metrics) || !is.null(baseline_plus_metrics)

# 2. Helpers: best validation model and metric extraction

ensure_feature_variant <- function(metrics_tbl) {
    if (!"feature_variant" %in% names(metrics_tbl)) {
        metrics_tbl$feature_variant <- NA_character_
    }

    metrics_tbl
}

pick_best_validation_row <- function(metrics_tbl) {
    metrics_tbl |>
        dplyr::filter(.data$split == "validation") |>
        dplyr::arrange(.data$log_loss, .data$model) |>
        dplyr::slice(1)
}

pick_matching_test_row <- function(metrics_tbl, validation_row) {
    test_tbl <- metrics_tbl |>
        dplyr::filter(.data$split == "test")

    if ("feature_variant" %in% names(validation_row) &&
        !is.na(validation_row$feature_variant[[1]])) {
        test_tbl <- test_tbl |>
            dplyr::filter(
                .data$feature_variant == validation_row$feature_variant[[1]],
                .data$model == validation_row$model[[1]]
            )
    } else {
        test_tbl <- test_tbl |>
            dplyr::filter(.data$model == validation_row$model[[1]])
    }

    if (nrow(test_tbl) == 0) {
        return(NULL)
    }

    test_tbl |>
        dplyr::slice(1)
}

extract_test_draw_fields <- function(test_row) {
    if (is.null(test_row) || nrow(test_row) == 0) {
        return(list(
            test_draw_recall = NA_real_,
            test_predicted_draw_rate = NA_real_
        ))
    }

    list(
        test_draw_recall = if ("draw_recall" %in% names(test_row)) {
            test_row$draw_recall[[1]]
        } else {
            NA_real_
        },
        test_predicted_draw_rate = if ("predicted_draw_rate" %in% names(test_row)) {
            test_row$predicted_draw_rate[[1]]
        } else {
            NA_real_
        }
    )
}

build_stage_row <- function(
    stage_order,
    stage_name,
    script,
    main_change,
    metrics_tbl,
    interpretation
) {
    metrics_tbl <- ensure_feature_variant(metrics_tbl)
    validation_row <- pick_best_validation_row(metrics_tbl)
    test_row <- pick_matching_test_row(metrics_tbl, validation_row)
    draw_fields <- extract_test_draw_fields(test_row)

    feature_variant <- validation_row$feature_variant[[1]]
    if (length(feature_variant) == 0 || is.na(feature_variant)) {
        feature_variant <- NA_character_
    }

    tibble::tibble(
        stage_order = stage_order,
        stage_name = stage_name,
        script = script,
        main_change = main_change,
        selected_feature_variant = feature_variant,
        selected_model = validation_row$model[[1]],
        validation_log_loss = validation_row$log_loss[[1]],
        test_log_loss = if (!is.null(test_row)) test_row$log_loss[[1]] else NA_real_,
        test_accuracy = if (!is.null(test_row)) test_row$accuracy[[1]] else NA_real_,
        test_macro_f1 = if (!is.null(test_row)) test_row$macro_f1[[1]] else NA_real_,
        test_draw_recall = draw_fields$test_draw_recall,
        test_predicted_draw_rate = draw_fields$test_predicted_draw_rate,
        interpretation = interpretation
    )
}

build_best_by_stage_row <- function(stage_name, metrics_tbl) {
    metrics_tbl <- ensure_feature_variant(metrics_tbl)
    validation_row <- pick_best_validation_row(metrics_tbl)
    test_row <- pick_matching_test_row(metrics_tbl, validation_row)

    out <- validation_row |>
        dplyr::mutate(
            stage = stage_name,
            selection_split = "validation"
        )

    if (!is.null(test_row)) {
        test_out <- test_row |>
            dplyr::mutate(
                stage = stage_name,
                selection_split = "test"
            )
        out <- dplyr::bind_rows(out, test_out)
    }

    out |>
        dplyr::rename(feature_variant = dplyr::any_of("feature_variant")) |>
        dplyr::select(
            stage,
            feature_variant,
            model,
            selection_split,
            dplyr::any_of(c(
                "n",
                "log_loss",
                "accuracy",
                "macro_f1",
                "draw_recall",
                "draw_precision",
                "predicted_draw_rate",
                "actual_draw_rate",
                "mean_pred_D",
                "median_pred_D"
            ))
        )
}

normalize_metrics_table <- function(metrics_tbl, stage_label) {
    metrics_tbl <- ensure_feature_variant(metrics_tbl)

    metrics_tbl |>
        dplyr::mutate(
            stage = stage_label,
            feature_variant = dplyr::if_else(
                is.na(.data$feature_variant),
                NA_character_,
                .data$feature_variant
            )
        ) |>
        dplyr::rename(split = dplyr::any_of("split")) |>
        dplyr::select(
            stage,
            feature_variant,
            model,
            split,
            dplyr::any_of(c(
                "n",
                "log_loss",
                "accuracy",
                "macro_f1",
                "draw_recall",
                "draw_precision",
                "predicted_draw_rate",
                "actual_draw_rate",
                "mean_pred_D",
                "median_pred_D"
            ))
        )
}

# 3. Baseline metrics (optional)

baseline_combined <- NULL

if (baseline_files_available) {
    baseline_parts <- list()

    if (!is.null(baseline_metrics)) {
        baseline_parts <- c(
            baseline_parts,
            list(baseline_metrics |> dplyr::mutate(baseline_source = "baseline"))
        )
    }

    if (!is.null(baseline_plus_metrics)) {
        baseline_parts <- c(
            baseline_parts,
            list(
                baseline_plus_metrics |>
                    dplyr::mutate(baseline_source = "baseline_plus")
            )
        )
    }

    baseline_combined <- dplyr::bind_rows(baseline_parts)
}

# 4. Stage-level incremental progress

stage_rows <- list()

if (!is.null(baseline_combined)) {
    stage_rows$baseline <- build_stage_row(
        stage_order = 1L,
        stage_name = "baseline",
        script = "src/19_baseline.R / src/21_baseline_plus_draw_features.R",
        main_change = "frequency/Elo/draw-aware baselines",
        metrics_tbl = baseline_combined,
        interpretation = paste(
            "Simple frequency and Elo-based baselines are already competitive;",
            "rating-difference models set a strong reference point."
        )
    )
}

stage_rows$model_24 <- build_stage_row(
    stage_order = if (baseline_files_available) 2L else 1L,
    stage_name = "model_24_safe_features",
    script = "src/24_model_glm_lightgbm_approved_features.R",
    main_change = "GLM, glmnet, and LightGBM on approved safe pre-match features",
    metrics_tbl = model_24_metrics,
    interpretation = paste(
        "Safe-feature GLM, glmnet ridge, and LightGBM were effectively tied;",
        "validation log loss differences were negligible."
    )
)

if (!is.null(model_26_metrics)) {
    stage_rows$model_26 <- build_stage_row(
        stage_order = max(purrr::map_int(stage_rows, ~ .x$stage_order[[1]]), 0L) + 1L,
        stage_name = "model_26_draw_aware_elo",
        script = "src/26_model_draw_aware_features.R",
        main_change = "draw-aware Elo transformations",
        metrics_tbl = model_26_metrics,
        interpretation = paste(
            "Draw-aware Elo transformations did not materially improve",
            "validation or test log loss versus Model 24."
        )
    )
}

stage_rows$model_28 <- build_stage_row(
    stage_order = max(purrr::map_int(stage_rows, ~ .x$stage_order[[1]]), 0L) + 1L,
    stage_name = "model_28_lagged_form",
    script = "src/28_model_with_lagged_form.R",
    main_change = "strictly lagged team-form features",
    metrics_tbl = model_28_metrics,
    interpretation = paste(
        "Compact lagged form features produced the clearest incremental gain,",
        "but improvement over safe Elo features was modest rather than transformational."
    )
)

final_incremental_model_progress <- dplyr::bind_rows(stage_rows) |>
    dplyr::mutate(
        stage_name = factor(
            .data$stage_name,
            levels = .data$stage_name[order(.data$stage_order)]
        )
    )

stages_included <- final_incremental_model_progress$stage_name |>
    as.character()

# 5. Best models by stage

best_stage_parts <- list()

if (!is.null(baseline_combined)) {
    best_stage_parts$baseline <- build_best_by_stage_row(
        "baseline",
        baseline_combined
    )
}

best_stage_parts$model_24 <- build_best_by_stage_row(
    "model_24_safe_features",
    model_24_metrics
)

if (!is.null(model_26_metrics)) {
    best_stage_parts$model_26 <- build_best_by_stage_row(
        "model_26_draw_aware_elo",
        model_26_metrics
    )
}

best_stage_parts$model_28 <- build_best_by_stage_row(
    "model_28_lagged_form",
    model_28_metrics
)

final_best_models_by_stage <- dplyr::bind_rows(best_stage_parts)

# 6. Final selected model (Model 28, validation log loss)

model_28_metrics <- ensure_feature_variant(model_28_metrics)
final_validation_row <- pick_best_validation_row(model_28_metrics)
final_test_row <- pick_matching_test_row(model_28_metrics, final_validation_row)

final_selected_feature_variant <- final_validation_row$feature_variant[[1]]
final_selected_model <- final_validation_row$model[[1]]

if (is.na(final_selected_feature_variant)) {
    final_selected_feature_variant <- NA_character_
}

final_selected_model_test_metrics <- dplyr::bind_rows(
    final_validation_row |>
        dplyr::mutate(selection_split = "validation"),
    if (!is.null(final_test_row)) {
        final_test_row |>
            dplyr::mutate(selection_split = "test")
    }
) |>
    dplyr::mutate(
        selection_rule = "best validation log loss within model_28_metrics.csv"
    )

# 7. Combined model comparison table

comparison_parts <- list(
    model_24 = normalize_metrics_table(model_24_metrics, "model_24_safe_features")
)

if (!is.null(model_26_metrics)) {
    comparison_parts$model_26 <- normalize_metrics_table(
        model_26_metrics,
        "model_26_draw_aware_elo"
    )
}

comparison_parts$model_28 <- normalize_metrics_table(
    model_28_metrics,
    "model_28_lagged_form"
)

final_model_comparison <- dplyr::bind_rows(comparison_parts)

# 8. Draw diagnostics summary

draw_summary_parts <- list()

if (!is.null(model_25_draw_diagnostics)) {
    draw_summary_parts$model_25 <- model_25_draw_diagnostics |>
        dplyr::filter(.data$split == "test") |>
        dplyr::transmute(
            source = "model_25_draw_diagnostics",
            model = .data$model,
            feature_variant = NA_character_,
            split = .data$split,
            actual_draw_rate = .data$actual_draw_rate,
            mean_pred_D = .data$mean_pred_D,
            predicted_draw_rate = .data$predicted_draw_rate,
            draw_recall = .data$draw_recall,
            draw_precision = .data$draw_precision,
            draw_top_rate = NA_real_,
            draw_second_rate = NA_real_,
            median_draw_margin_to_top = NA_real_
        )
}

if (!is.null(model_25_draw_rank_diagnostics)) {
    rank_test <- model_25_draw_rank_diagnostics |>
        dplyr::filter(.data$split == "test")

    if (!is.null(draw_summary_parts$model_25)) {
        draw_summary_parts$model_25 <- draw_summary_parts$model_25 |>
            dplyr::left_join(
                rank_test |>
                    dplyr::select(
                        model,
                        draw_top_rate,
                        draw_second_rate,
                        median_draw_margin_to_top
                    ),
                by = "model"
            )
    } else {
        draw_summary_parts$model_25 <- rank_test |>
            dplyr::transmute(
                source = "model_25_draw_rank_diagnostics",
                model = .data$model,
                feature_variant = NA_character_,
                split = .data$split,
                actual_draw_rate = .data$actual_draw_rate,
                mean_pred_D = .data$mean_pred_D,
                predicted_draw_rate = NA_real_,
                draw_recall = NA_real_,
                draw_precision = NA_real_,
                draw_top_rate = .data$draw_top_rate,
                draw_second_rate = .data$draw_second_rate,
                median_draw_margin_to_top = .data$median_draw_margin_to_top
            )
    }
}

if (!is.null(final_test_row)) {
    draw_summary_parts$final_model_28 <- tibble::tibble(
        source = "model_28_final_selected",
        model = final_selected_model,
        feature_variant = final_selected_feature_variant,
        split = "test",
        actual_draw_rate = if ("actual_draw_rate" %in% names(final_test_row)) {
            final_test_row$actual_draw_rate[[1]]
        } else {
            NA_real_
        },
        mean_pred_D = if ("mean_pred_D" %in% names(final_test_row)) {
            final_test_row$mean_pred_D[[1]]
        } else {
            NA_real_
        },
        predicted_draw_rate = if ("predicted_draw_rate" %in% names(final_test_row)) {
            final_test_row$predicted_draw_rate[[1]]
        } else {
            NA_real_
        },
        draw_recall = if ("draw_recall" %in% names(final_test_row)) {
            final_test_row$draw_recall[[1]]
        } else {
            NA_real_
        },
        draw_precision = if ("draw_precision" %in% names(final_test_row)) {
            final_test_row$draw_precision[[1]]
        } else {
            NA_real_
        },
        draw_top_rate = NA_real_,
        draw_second_rate = NA_real_,
        median_draw_margin_to_top = NA_real_
    )
}

final_draw_diagnostics_summary <- if (length(draw_summary_parts) > 0) {
    dplyr::bind_rows(draw_summary_parts)
} else {
    tibble::tibble(
        source = character(),
        model = character(),
        feature_variant = character(),
        split = character(),
        actual_draw_rate = double(),
        mean_pred_D = double(),
        predicted_draw_rate = double(),
        draw_recall = double(),
        draw_precision = double(),
        draw_top_rate = double(),
        draw_second_rate = double(),
        median_draw_margin_to_top = double()
    )
}

# 9. Final model predictions (test split)

required_prob_cols <- c("pred_H", "pred_D", "pred_A")

if (!all(required_prob_cols %in% names(model_28_predictions))) {
    stop(
        "model_28_predictions.csv must contain pred_H, pred_D, pred_A.",
        call. = FALSE
    )
}

final_predictions_test <- model_28_predictions |>
    dplyr::filter(.data$split == "test")

if ("feature_variant" %in% names(final_predictions_test) &&
    !is.na(final_selected_feature_variant)) {
    final_predictions_test <- final_predictions_test |>
        dplyr::filter(
            .data$feature_variant == final_selected_feature_variant,
            .data$model == final_selected_model
        )
} else {
    final_predictions_test <- final_predictions_test |>
        dplyr::filter(.data$model == final_selected_model)
}

if (nrow(final_predictions_test) == 0) {
    stop(
        "No test predictions found for final selected model: ",
        final_selected_model,
        if (!is.na(final_selected_feature_variant)) {
            paste0(" (", final_selected_feature_variant, ")")
        } else {
            ""
        },
        call. = FALSE
    )
}

prob_matrix <- as.matrix(final_predictions_test[, required_prob_cols])

if (any(!is.finite(prob_matrix))) {
    stop("Final model predictions contain non-finite probabilities.", call. = FALSE)
}

row_sums <- rowSums(prob_matrix)

if (any(abs(row_sums - 1) > 1e-5)) {
    warning(
        "Some final model probability rows do not sum to 1 (max deviation: ",
        round(max(abs(row_sums - 1)), 6),
        ").",
        call. = FALSE
    )
}

final_predictions_test <- final_predictions_test |>
    dplyr::mutate(
        match_result = factor(.data$match_result, levels = MATCH_RESULT_LEVELS),
        pred_class = factor(.data$pred_class, levels = MATCH_RESULT_LEVELS),
        max_pred_prob = pmax(.data$pred_H, .data$pred_D, .data$pred_A),
        true_class_prob = dplyr::case_when(
            .data$match_result == "H" ~ .data$pred_H,
            .data$match_result == "D" ~ .data$pred_D,
            .data$match_result == "A" ~ .data$pred_A,
            TRUE ~ NA_real_
        ),
        is_correct = .data$pred_class == .data$match_result
    )

sample_top_n <- function(data, sample_size, ...) {
    ranked <- data |>
        dplyr::arrange(...)

    dplyr::slice_head(ranked, n = min(sample_size, nrow(ranked)))
}

example_slices <- list(
    high_confidence_correct = sample_top_n(
        final_predictions_test |> dplyr::filter(.data$is_correct),
        10,
        dplyr::desc(.data$max_pred_prob)
    ),
    high_confidence_wrong = sample_top_n(
        final_predictions_test |> dplyr::filter(!.data$is_correct),
        10,
        dplyr::desc(.data$max_pred_prob)
    ),
    most_uncertain = sample_top_n(
        final_predictions_test,
        10,
        .data$max_pred_prob
    ),
    highest_predicted_draw = sample_top_n(
        final_predictions_test,
        10,
        dplyr::desc(.data$pred_D)
    ),
    actual_draws_highest_pred_draw = sample_top_n(
        final_predictions_test |> dplyr::filter(.data$match_result == "D"),
        10,
        dplyr::desc(.data$pred_D)
    )
)

final_prediction_examples <- dplyr::bind_rows(
    example_slices$high_confidence_correct |>
        dplyr::mutate(example_type = "high_confidence_correct"),
    example_slices$high_confidence_wrong |>
        dplyr::mutate(example_type = "high_confidence_wrong"),
    example_slices$most_uncertain |>
        dplyr::mutate(example_type = "most_uncertain"),
    example_slices$highest_predicted_draw |>
        dplyr::mutate(example_type = "highest_predicted_draw"),
    example_slices$actual_draws_highest_pred_draw |>
        dplyr::mutate(example_type = "actual_draw_highest_pred_draw")
) |>
    dplyr::select(
        model,
        dplyr::any_of("feature_variant"),
        split,
        source_match_id,
        date,
        match_result,
        pred_class,
        pred_H,
        pred_D,
        pred_A,
        max_pred_prob,
        true_class_prob,
        example_type
    )

# 10. Project summary table

final_project_summary <- tibble::tibble(
    item = c(
        "final_selected_model",
        "final_selected_feature_variant",
        "selection_rule",
        "validation_log_loss",
        "test_log_loss",
        "test_accuracy",
        "test_macro_f1",
        "test_draw_recall",
        "test_predicted_draw_rate",
        "baseline_files_available",
        "stages_included",
        "baseline_note"
    ),
    value = c(
        final_selected_model,
        as.character(final_selected_feature_variant),
        "best validation log loss within model_28_metrics.csv",
        as.character(final_validation_row$log_loss[[1]]),
        as.character(if (!is.null(final_test_row)) final_test_row$log_loss[[1]] else NA),
        as.character(if (!is.null(final_test_row)) final_test_row$accuracy[[1]] else NA),
        as.character(if (!is.null(final_test_row)) final_test_row$macro_f1[[1]] else NA),
        as.character(
            if (!is.null(final_test_row) && "draw_recall" %in% names(final_test_row)) {
                final_test_row$draw_recall[[1]]
            } else {
                NA
            }
        ),
        as.character(
            if (
                !is.null(final_test_row) && "predicted_draw_rate" %in% names(final_test_row)
            ) {
                final_test_row$predicted_draw_rate[[1]]
            } else {
                NA
            }
        ),
        as.character(baseline_files_available),
        paste(stages_included, collapse = "; "),
        if (baseline_files_available) {
            "Baseline comparison files were loaded for incremental staging."
        } else {
            "Baseline files were unavailable to this script; baseline stage omitted."
        }
    )
)

# 11. Write final tables

output_tables <- c(
    "reports/tables/final_incremental_model_progress.csv",
    "reports/tables/final_best_models_by_stage.csv",
    "reports/tables/final_selected_model_test_metrics.csv",
    "reports/tables/final_model_comparison.csv",
    "reports/tables/final_draw_diagnostics_summary.csv",
    "reports/tables/final_prediction_examples.csv",
    "reports/tables/final_project_summary.csv"
)

readr::write_csv(
    final_incremental_model_progress |> dplyr::mutate(stage_name = as.character(stage_name)),
    output_tables[[1]]
)
readr::write_csv(final_best_models_by_stage, output_tables[[2]])
readr::write_csv(final_selected_model_test_metrics, output_tables[[3]])
readr::write_csv(final_model_comparison, output_tables[[4]])
readr::write_csv(final_draw_diagnostics_summary, output_tables[[5]])
readr::write_csv(final_prediction_examples, output_tables[[6]])
readr::write_csv(final_project_summary, output_tables[[7]])

# 12. Figures

progress_plot_data <- final_incremental_model_progress |>
    dplyr::mutate(
        stage_label = paste0(.data$stage_order, ". ", .data$stage_name)
    ) |>
    dplyr::mutate(
        stage_label = factor(
            .data$stage_label,
            levels = .data$stage_label[order(.data$stage_order)]
        )
    )

label_format <- function(x) {
    ifelse(is.na(x), "", sprintf("%.3f", x))
}

plot_incremental_log_loss <- function(metric_col, title_text, filename) {
    plot_data <- progress_plot_data |>
        dplyr::select(stage_label, stage_order, metric_value = dplyr::all_of(metric_col))

    p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$stage_label, y = .data$metric_value)
    ) +
        ggplot2::geom_col(fill = "#2c6e8a", width = 0.7) +
        ggplot2::geom_text(
            ggplot2::aes(label = label_format(.data$metric_value)),
            vjust = -0.4,
            size = 3.2
        ) +
        ggplot2::labs(
            title = title_text,
            x = NULL,
            y = metric_col
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
        ) +
        ggplot2::expand_limits(y = max(plot_data$metric_value, na.rm = TRUE) * 1.08)

    ggplot2::ggsave(
        filename = file.path(PROJECT_ROOT, filename),
        plot = p,
        width = 10,
        height = 5.5,
        dpi = 300
    )
}

plot_incremental_log_loss(
    "validation_log_loss",
    "Incremental validation log loss by modeling stage",
    "reports/figures/final_incremental_validation_log_loss.png"
)

plot_incremental_log_loss(
    "test_log_loss",
    "Held-out test log loss by modeling stage",
    "reports/figures/final_incremental_test_log_loss.png"
)

accuracy_f1_plot_data <- progress_plot_data |>
    tidyr::pivot_longer(
        cols = c(test_accuracy, test_macro_f1),
        names_to = "metric",
        values_to = "metric_value"
    ) |>
    dplyr::mutate(
        metric = dplyr::recode(
            .data$metric,
            test_accuracy = "Test accuracy",
            test_macro_f1 = "Test macro F1"
        )
    )

accuracy_f1_plot <- ggplot2::ggplot(
    accuracy_f1_plot_data,
    ggplot2::aes(
        x = .data$stage_label,
        y = .data$metric_value,
        fill = .data$metric
    )
) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.72) +
    ggplot2::geom_text(
        ggplot2::aes(label = label_format(.data$metric_value)),
        position = ggplot2::position_dodge(width = 0.8),
        vjust = -0.35,
        size = 2.8
    ) +
    ggplot2::labs(
        title = "Test accuracy and macro F1 by modeling stage",
        x = NULL,
        y = "Metric value",
        fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
    )

ggplot2::ggsave(
    filename = file.path(PROJECT_ROOT, "reports/figures/final_incremental_accuracy_macro_f1.png"),
    plot = accuracy_f1_plot,
    width = 10,
    height = 5.5,
    dpi = 300
)

reference_draw_rate <- NA_real_

if (nrow(final_draw_diagnostics_summary) > 0) {
    reference_draw_rate <- final_draw_diagnostics_summary |>
        dplyr::filter(!is.na(.data$actual_draw_rate)) |>
        dplyr::slice(1) |>
        dplyr::pull(.data$actual_draw_rate)
}

draw_metrics_plot_data <- progress_plot_data |>
    tidyr::pivot_longer(
        cols = c(test_draw_recall, test_predicted_draw_rate),
        names_to = "metric",
        values_to = "metric_value"
    ) |>
    dplyr::filter(!is.na(.data$metric_value)) |>
    dplyr::mutate(
        metric = dplyr::recode(
            .data$metric,
            test_draw_recall = "Test draw recall",
            test_predicted_draw_rate = "Test predicted draw rate"
        )
    )

draw_metrics_plot <- if (nrow(draw_metrics_plot_data) == 0) {
    ggplot2::ggplot() +
        ggplot2::labs(
            title = "Draw prediction behavior by modeling stage",
            subtitle = "Draw metrics unavailable for included stages"
        ) +
        ggplot2::theme_minimal()
} else {
    draw_metrics_plot <- ggplot2::ggplot(
        draw_metrics_plot_data,
        ggplot2::aes(
            x = .data$stage_label,
            y = .data$metric_value,
            fill = .data$metric
        )
    ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.72) +
        ggplot2::geom_text(
            ggplot2::aes(label = label_format(.data$metric_value)),
            position = ggplot2::position_dodge(width = 0.8),
            vjust = -0.35,
            size = 2.8
        )

    if (!is.na(reference_draw_rate)) {
        draw_metrics_plot <- draw_metrics_plot +
            ggplot2::geom_hline(
                yintercept = reference_draw_rate,
                linetype = "dashed",
                color = "#444444"
            ) +
            ggplot2::annotate(
                "text",
                x = 0.6,
                y = reference_draw_rate,
                label = paste0("Actual draw rate ≈ ", round(reference_draw_rate, 3)),
                hjust = 0,
                vjust = -0.4,
                size = 3
            )
    }

    draw_metrics_plot +
        ggplot2::labs(
            title = "Draw prediction behavior by modeling stage",
            x = NULL,
            y = "Rate",
            fill = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
        )
}

ggplot2::ggsave(
    filename = file.path(PROJECT_ROOT, "reports/figures/final_incremental_draw_metrics.png"),
    plot = draw_metrics_plot,
    width = 10,
    height = 5.5,
    dpi = 300
)

confusion_heatmap_data <- final_predictions_test |>
    dplyr::count(.data$match_result, .data$pred_class, name = "n") |>
    dplyr::group_by(.data$match_result) |>
    dplyr::mutate(row_pct = .data$n / sum(.data$n)) |>
    dplyr::ungroup()

confusion_heatmap <- confusion_heatmap_data |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = .data$pred_class,
            y = .data$match_result,
            fill = .data$row_pct
        )
    ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
        ggplot2::aes(
            label = if (has_scales) {
                scales::percent(.data$row_pct, accuracy = 0.1)
            } else {
                sprintf("%.1f%%", 100 * .data$row_pct)
            }
        ),
        size = 4
    ) +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08519c") +
    ggplot2::labs(
        title = "Final model confusion matrix on held-out test set",
        subtitle = paste0(
            final_selected_feature_variant,
            " + ",
            final_selected_model
        ),
        x = "Predicted class",
        y = "Actual class",
        fill = "Row %"
    ) +
    ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
    filename = file.path(PROJECT_ROOT, "reports/figures/final_model_confusion_heatmap.png"),
    plot = confusion_heatmap,
    width = 7,
    height = 5.5,
    dpi = 300
)

calibration_long <- final_predictions_test |>
    tidyr::pivot_longer(
        cols = c(pred_H, pred_D, pred_A),
        names_to = "probability_column",
        values_to = "predicted_probability"
    ) |>
    dplyr::mutate(
        outcome_class = dplyr::recode(
            .data$probability_column,
            pred_H = "H",
            pred_D = "D",
            pred_A = "A"
        ),
        is_observed = .data$match_result == .data$outcome_class
    )

calibration_binned <- calibration_long |>
    dplyr::group_by(.data$outcome_class) |>
    dplyr::mutate(probability_decile = dplyr::ntile(.data$predicted_probability, 10)) |>
    dplyr::group_by(.data$outcome_class, .data$probability_decile) |>
    dplyr::summarize(
        mean_predicted_probability = mean(.data$predicted_probability),
        observed_event_rate = mean(.data$is_observed),
        .groups = "drop"
    ) |>
    dplyr::mutate(
        outcome_class = factor(.data$outcome_class, levels = MATCH_RESULT_LEVELS)
    )

calibration_plot <- calibration_binned |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = .data$mean_predicted_probability,
            y = .data$observed_event_rate
        )
    ) +
    ggplot2::geom_point(size = 2.5, color = "#2c6e8a") +
    ggplot2::geom_line(color = "#2c6e8a") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#666666") +
    ggplot2::facet_wrap(~ outcome_class) +
    ggplot2::labs(
        title = "Final model probability calibration on held-out test set",
        subtitle = paste0(
            final_selected_feature_variant,
            " + ",
            final_selected_model
        ),
        x = "Mean predicted probability (decile bins)",
        y = "Observed event rate"
    ) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
    filename = file.path(PROJECT_ROOT, "reports/figures/final_model_calibration_plot.png"),
    plot = calibration_plot,
    width = 9,
    height = 4.5,
    dpi = 300
)

confidence_plot <- final_predictions_test |>
    ggplot2::ggplot(ggplot2::aes(x = .data$max_pred_prob, fill = .data$is_correct)) +
    ggplot2::geom_histogram(
        bins = 30,
        position = "identity",
        alpha = 0.55,
        color = "white"
    ) +
    ggplot2::labs(
        title = "Final model prediction confidence",
        subtitle = paste0(
            final_selected_feature_variant,
            " + ",
            final_selected_model
        ),
        x = "Maximum predicted class probability",
        y = "Count",
        fill = "Correct prediction"
    ) +
    ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
    filename = file.path(
        PROJECT_ROOT,
        "reports/figures/final_model_prediction_confidence.png"
    ),
    plot = confidence_plot,
    width = 8,
    height = 5,
    dpi = 300
)

probability_distribution_data <- final_predictions_test |>
    tidyr::pivot_longer(
        cols = c(pred_H, pred_D, pred_A),
        names_to = "probability_column",
        values_to = "predicted_probability"
    ) |>
    dplyr::mutate(
        probability_class = dplyr::recode(
            .data$probability_column,
            pred_H = "H",
            pred_D = "D",
            pred_A = "A"
        ),
        probability_class = factor(.data$probability_class, levels = MATCH_RESULT_LEVELS)
    )

probability_distribution_plot <- probability_distribution_data |>
    ggplot2::ggplot(
        ggplot2::aes(x = .data$predicted_probability, fill = .data$probability_class)
    ) +
    ggplot2::geom_histogram(
        bins = 35,
        alpha = 0.75,
        color = "white",
        position = "identity"
    ) +
    ggplot2::facet_wrap(~ probability_class, ncol = 1, scales = "free_y") +
    ggplot2::labs(
        title = "Distribution of final model predicted probabilities",
        subtitle = paste0(
            final_selected_feature_variant,
            " + ",
            final_selected_model
        ),
        x = "Predicted probability",
        y = "Count",
        fill = "Class"
    ) +
    ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
    filename = file.path(
        PROJECT_ROOT,
        "reports/figures/final_model_probability_distribution.png"
    ),
    plot = probability_distribution_plot,
    width = 8,
    height = 8,
    dpi = 300
)

stage_summary_labels <- progress_plot_data |>
    dplyr::mutate(
        summary_text = paste0(
            "Val LL: ",
            sprintf("%.3f", .data$validation_log_loss),
            "\nTest LL: ",
            sprintf("%.3f", .data$test_log_loss),
            "\n",
            .data$main_change
        )
    )

stage_summary_plot <- ggplot2::ggplot(
    stage_summary_labels,
    ggplot2::aes(
        x = 1,
        y = .data$stage_label,
        fill = .data$validation_log_loss
    )
) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(
        ggplot2::aes(label = .data$summary_text),
        size = 3.1,
        color = "black"
    ) +
    ggplot2::scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
    ggplot2::labs(
        title = "Modeling stage summary",
        subtitle = "Darker fill = lower validation log loss (better)",
        x = NULL,
        y = NULL,
        fill = "Validation\nlog loss"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
    )

ggplot2::ggsave(
    filename = file.path(PROJECT_ROOT, "reports/figures/final_modeling_stage_summary.png"),
    plot = stage_summary_plot,
    width = 10,
    height = 6,
    dpi = 300
)

output_figures <- c(
    "reports/figures/final_incremental_validation_log_loss.png",
    "reports/figures/final_incremental_test_log_loss.png",
    "reports/figures/final_incremental_accuracy_macro_f1.png",
    "reports/figures/final_incremental_draw_metrics.png",
    "reports/figures/final_model_confusion_heatmap.png",
    "reports/figures/final_model_calibration_plot.png",
    "reports/figures/final_model_prediction_confidence.png",
    "reports/figures/final_model_probability_distribution.png",
    "reports/figures/final_modeling_stage_summary.png"
)

# 13. Markdown report

format_md_number <- function(x, digits = 3) {
    ifelse(
        is.na(x),
        "NA",
        sprintf(paste0("%.", digits, "f"), x)
    )
}

progress_md_table <- final_incremental_model_progress |>
    dplyr::mutate(stage_name = as.character(.data$stage_name)) |>
    dplyr::transmute(
        Stage = .data$stage_name,
        `Best model` = .data$selected_model,
        `Feature variant` = dplyr::if_else(
            is.na(.data$selected_feature_variant),
            "—",
            .data$selected_feature_variant
        ),
        `Val log loss` = format_md_number(.data$validation_log_loss),
        `Test log loss` = format_md_number(.data$test_log_loss),
        `Test accuracy` = format_md_number(.data$test_accuracy),
        `Test macro F1` = format_md_number(.data$test_macro_f1)
    )

progress_md_lines <- apply(progress_md_table, 1, function(row_values) {
    paste0("| ", paste(row_values, collapse = " | "), " |")
})

progress_md_header <- paste(
    "|",
    paste(names(progress_md_table), collapse = " | "),
    "|"
)
progress_md_sep <- paste(
    "|",
    paste(rep("---", ncol(progress_md_table)), collapse = " | "),
    "|"
)

final_val_log_loss <- final_validation_row$log_loss[[1]]
final_test_log_loss <- if (!is.null(final_test_row)) final_test_row$log_loss[[1]] else NA_real_
final_test_accuracy <- if (!is.null(final_test_row)) final_test_row$accuracy[[1]] else NA_real_
final_test_macro_f1 <- if (!is.null(final_test_row)) final_test_row$macro_f1[[1]] else NA_real_

markdown_report <- c(
    "# Soccer-R-Verse Final Modeling Summary",
    "",
    "## Goal",
    "Forecast international association football match outcomes as calibrated",
    "multiclass probabilities for home win (H), draw (D), and away win (A).",
    "",
    "## Data and leakage controls",
    "- One row per match keyed by `source_match_id`.",
    "- Chronological train, validation, and held-out test splits.",
    "- Strict pre-match features only (approved Elo/tournament context).",
    "- Lagged team-form features (Model 27/28) built from prior matches only.",
    "- Test split reserved for final reporting; model selection uses validation log loss.",
    "",
    "## Modeling progression",
    progress_md_header,
    progress_md_sep,
    progress_md_lines,
    "",
    "## Final selected model",
    "- Selected by **validation log loss**, not test performance.",
    paste0(
        "- Final stage: Model 28 (`",
        final_selected_feature_variant,
        "` + `",
        final_selected_model,
        "`)."
    ),
    "",
    "## Performance",
    paste0(
        "- Validation log loss: **",
        format_md_number(final_val_log_loss),
        "**."
    ),
    paste0(
        "- Test log loss: **",
        format_md_number(final_test_log_loss),
        "**."
    ),
    paste0(
        "- Test accuracy: **",
        format_md_number(final_test_accuracy),
        "**; test macro F1: **",
        format_md_number(final_test_macro_f1),
        "**."
    ),
    "",
    "## Draw behavior",
    "- Mean predicted draw probability (`mean_pred_D`) is often in a plausible range.",
    "- Draw is rarely the top predicted class in earlier Model 24/25 diagnostics.",
    "- Lagged form (Model 28) improved draw recall modestly but did not fully resolve draw ranking.",
    "",
    "## Interpretation",
    "- Elo/simple baselines are already strong; supervised models refine probabilities modestly.",
    "- Safe-feature learners in Model 24 were effectively tied.",
    "- Draw-aware Elo transforms (Model 26) did not materially move validation metrics.",
    "- Compact lagged form gave the clearest incremental gain, but gains are modest.",
    "- The project is a solid MVP forecasting baseline, not yet state-of-the-art.",
    "",
    "## Future work",
    "- Incorporate market odds as features or calibration anchors.",
    "- Add squad value and player availability signals.",
    "- Build tournament simulation on top of match-level probabilities.",
    "- Post-process probabilities for improved calibration.",
    "- Tune LightGBM hyperparameters and class weights for draws.",
    "- Add explainability (e.g., SHAP) for feature attribution.",
    "- Wire the selected model into a World Cup forecast pipeline.",
    "",
    "## Generated artifacts",
    "### Tables",
    paste0("- `", output_tables, "`", collapse = "\n"),
    "",
    "### Figures",
    paste0("- `", output_figures, "`", collapse = "\n")
)

writeLines(
    markdown_report,
    con = file.path(PROJECT_ROOT, "reports/final/final_results_summary.md")
)

# 14. Console summary

message("Files read:")
for (path in files_read) {
    message("  - ", path)
}

if (length(files_missing_optional) > 0) {
    message("Optional files missing:")
    for (path in files_missing_optional) {
        message("  - ", path)
    }
}

message("Stages included: ", paste(stages_included, collapse = ", "))
message(
    "Final selected model: ",
    final_selected_model,
    if (!is.na(final_selected_feature_variant)) {
        paste0(" (", final_selected_feature_variant, ")")
    } else {
        ""
    }
)

message("Final selected model validation metrics:")
message(
    "  log_loss=",
    format_md_number(final_validation_row$log_loss[[1]]),
    ", accuracy=",
    format_md_number(final_validation_row$accuracy[[1]]),
    ", macro_f1=",
    format_md_number(final_validation_row$macro_f1[[1]])
)

if (!is.null(final_test_row)) {
    message("Final selected model test metrics:")
    message(
        "  log_loss=",
        format_md_number(final_test_row$log_loss[[1]]),
        ", accuracy=",
        format_md_number(final_test_row$accuracy[[1]]),
        ", macro_f1=",
        format_md_number(final_test_row$macro_f1[[1]])
    )
}

message("Output tables written:")
for (path in output_tables) {
    message("  - ", path)
}

message("Output figures written:")
for (path in output_figures) {
    message("  - ", path)
}

message("Markdown report written: reports/final/final_results_summary.md")
message("Final results visualization complete.")
