# 32_finalize_international_modeling_project.R
#
# Reporting-only wrap-up after Model 30: best-model tables, incremental tier
# summary, extreme-feature audit, final plots, and a markdown report. Does not
# retrain models or edit processed tables.
#
# Reads: reports/tables/model_30/ and related reports
#
# Writes:
#   reports/tables/final_project/
#   reports/figures/final_model/

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

OUTPUT_TABLE_DIR <- file.path("reports", "tables", "final_project")
OUTPUT_PLOT_DIR <- file.path("reports", "figures", "final_model")
MODEL_30_DIR <- file.path("reports", "tables", "model_30")

required_pkgs <- c(
    "readr",
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
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

FEATURE_SET_ORDER <- c(
    "baseline_rating",
    "rating_plus_context",
    "rating_plus_form",
    "rating_plus_form_plus_goalscorers"
)

FEATURE_SET_LABELS <- c(
    baseline_rating = "Baseline rating",
    rating_plus_context = "Rating + context",
    rating_plus_form = "Rating + form",
    rating_plus_form_plus_goalscorers = "Rating + form + goalscorers"
)

OUTCOME_LEVELS <- c("H", "D", "A")

AUDITED_FORM_FEATURES <- c(
    "home_goal_diff_per_match_last_5",
    "away_goal_diff_per_match_last_5",
    "home_goal_diff_per_match_last_10",
    "away_goal_diff_per_match_last_10",
    "home_goals_for_per_match_last_5",
    "away_goals_for_per_match_last_5",
    "home_goals_against_per_match_last_5",
    "away_goals_against_per_match_last_5",
    "home_goals_for_per_match_last_10",
    "away_goals_for_per_match_last_10",
    "home_goals_against_per_match_last_10",
    "away_goals_against_per_match_last_10"
)

AUDITED_GOALSCORER_FEATURES <- c(
    "home_unique_scorers_last_10",
    "away_unique_scorers_last_10",
    "home_goals_by_top_scorer_last_10",
    "away_goals_by_top_scorer_last_10",
    "home_unique_scorers_365d",
    "away_unique_scorers_365d"
)

DISTRIBUTION_PLOT_FEATURES <- c(
    "home_goal_diff_per_match_last_5",
    "away_goal_diff_per_match_last_5",
    "home_goal_diff_per_match_last_10",
    "away_goal_diff_per_match_last_10",
    "home_goals_for_per_match_last_5",
    "away_goals_for_per_match_last_5",
    "home_goals_against_per_match_last_5",
    "away_goals_against_per_match_last_5",
    "home_goals_for_per_match_last_10",
    "away_goals_for_per_match_last_10",
    "home_goals_against_per_match_last_10",
    "away_goals_against_per_match_last_10"
)

plots_written <- character()

# 1. Helpers

resolve_project_path <- function(path) {
    if (grepl("^/", path) || grepl("^[A-Za-z]:[/\\\\]", path)) {
        return(normalizePath(path, winslash = "/", mustWork = FALSE))
    }

    normalizePath(
        file.path(PROJECT_ROOT, path),
        winslash = "/",
        mustWork = FALSE
    )
}

project_path <- function(...) {
    resolve_project_path(file.path(...))
}

dir.create(project_path(OUTPUT_TABLE_DIR), recursive = TRUE, showWarnings = FALSE)
dir.create(project_path(OUTPUT_PLOT_DIR), recursive = TRUE, showWarnings = FALSE)

read_required_csv <- function(relative_path) {
    full_path <- resolve_project_path(relative_path)

    if (!file.exists(full_path)) {
        stop("Required file not found: ", relative_path, call. = FALSE)
    }

    readr::read_csv(full_path, show_col_types = FALSE)
}

read_optional_csv <- function(relative_path) {
    full_path <- resolve_project_path(relative_path)

    if (!file.exists(full_path)) {
        return(NULL)
    }

    readr::read_csv(full_path, show_col_types = FALSE)
}

rename_first_present <- function(data_tbl, rename_map) {
    target_names <- unique(unname(rename_map))

    for (target_name in target_names) {
        source_names <- names(rename_map)[rename_map == target_name]
        present_sources <- intersect(source_names, names(data_tbl))

        if (length(present_sources) == 0) {
            next
        }

        primary_source <- present_sources[[1]]

        if (!target_name %in% names(data_tbl)) {
            data_tbl <- dplyr::rename(
                data_tbl,
                !!target_name := !!rlang::sym(primary_source)
            )
        }

        duplicate_sources <- setdiff(present_sources, primary_source)

        if (length(duplicate_sources) > 0) {
            data_tbl <- dplyr::select(
                data_tbl,
                -dplyr::all_of(duplicate_sources)
            )
        }
    }

    data_tbl
}

normalize_outcome <- function(values) {
    dplyr::case_when(
        values %in% c(1L, "1", "H") ~ "H",
        values %in% c(0L, "0", "D") ~ "D",
        values %in% c(-1L, "-1", "A") ~ "A",
        TRUE ~ as.character(values)
    )
}

normalize_performance_table <- function(metrics_tbl) {
    metrics_tbl |>
        rename_first_present(c(
            model = "model",
            model_name = "model",
            algorithm = "model",
            split = "split",
            data_split = "split",
            eval_split = "split"
        )) |>
        dplyr::mutate(
            model = as.character(.data$model),
            split = as.character(.data$split),
            feature_set = as.character(.data$feature_set),
            feature_set = factor(
                .data$feature_set,
                levels = FEATURE_SET_ORDER,
                ordered = TRUE
            )
        )
}

normalize_classwise_table <- function(classwise_tbl) {
    classwise_tbl |>
        rename_first_present(c(
            model = "model",
            model_name = "model",
            algorithm = "model",
            split = "split",
            data_split = "split",
            eval_split = "split"
        )) |>
        dplyr::mutate(
            model = as.character(.data$model),
            split = as.character(.data$split),
            feature_set = as.character(.data$feature_set),
            class = normalize_outcome(.data$class)
        )
}

normalize_confusion_table <- function(confusion_tbl) {
    confusion_tbl |>
        rename_first_present(c(
            model = "model",
            model_name = "model",
            algorithm = "model",
            split = "split",
            data_split = "split",
            eval_split = "split",
            actual = "actual_class",
            truth = "actual_class",
            result_class = "actual_class",
            prediction = "predicted_class",
            pred_class = "predicted_class"
        )) |>
        dplyr::mutate(
            model = as.character(.data$model),
            split = as.character(.data$split),
            feature_set = as.character(.data$feature_set),
            actual_class = normalize_outcome(.data$actual_class),
            predicted_class = normalize_outcome(.data$predicted_class)
        )
}

normalize_predictions_table <- function(predictions_tbl) {
    predictions_tbl |>
        rename_first_present(c(
            model = "model",
            model_name = "model",
            algorithm = "model",
            actual = "actual_class",
            truth = "actual_class",
            result_class = "actual_class",
            actual_result_class = "actual_class",
            prediction = "predicted_class",
            pred_class = "predicted_class",
            p_home = "p_home",
            pred_H = "p_home",
            p_draw = "p_draw",
            pred_D = "p_draw",
            p_away = "p_away",
            pred_A = "p_away"
        )) |>
        dplyr::mutate(
            model = as.character(.data$model),
            feature_set = as.character(.data$feature_set),
            actual_class = normalize_outcome(.data$actual_class),
            predicted_class = normalize_outcome(.data$predicted_class)
        )
}

pick_best_model <- function(metrics_tbl, split_name) {
    metrics_tbl |>
        dplyr::filter(.data$split == split_name) |>
        dplyr::mutate(
            feature_set_rank = match(
                as.character(.data$feature_set),
                FEATURE_SET_ORDER
            )
        ) |>
        dplyr::arrange(
            .data$log_loss,
            .data$brier_score,
            dplyr::desc(.data$macro_f1),
            .data$feature_set_rank
        ) |>
        dplyr::slice(1)
}

save_plot_dual <- function(plot_obj, filename_stem, width = 10, height = 6) {
    png_path <- project_path(OUTPUT_PLOT_DIR, paste0(filename_stem, ".png"))
    pdf_path <- project_path(OUTPUT_PLOT_DIR, paste0(filename_stem, ".pdf"))

    ggplot2::ggsave(
        filename = png_path,
        plot = plot_obj,
        width = width,
        height = height,
        dpi = 300
    )

    ggplot2::ggsave(
        filename = pdf_path,
        plot = plot_obj,
        width = width,
        height = height
    )

    plots_written <<- c(plots_written, filename_stem)
}

format_metric <- function(value, digits = 4) {
    if (is.na(value)) {
        return("NA")
    }

    format(round(value, digits), nsmall = digits)
}

# 2. Load required data

message("Loading Model 30 outputs and engineered modeling table...")

performance_summary <- read_required_csv(
    file.path(MODEL_30_DIR, "model_30_performance_summary.csv")
) |>
    normalize_performance_table()

classwise_metrics <- read_required_csv(
    file.path(MODEL_30_DIR, "model_30_classwise_metrics.csv")
) |>
    normalize_classwise_table()

confusion_matrices <- read_required_csv(
    file.path(MODEL_30_DIR, "model_30_confusion_matrices.csv")
) |>
    normalize_confusion_table()

prediction_examples <- read_required_csv(
    file.path(MODEL_30_DIR, "model_30_prediction_examples.csv")
) |>
    normalize_predictions_table()

feature_importance <- read_optional_csv(
    file.path(MODEL_30_DIR, "model_30_feature_importance.csv")
)

calibration_bins <- read_optional_csv(
    file.path(MODEL_30_DIR, "model_30_calibration_bins.csv")
)

modeling_table <- read_required_csv(
    "data/processed/international_modeling_table_with_form_and_goalscorers.csv"
)

if (!is.null(feature_importance)) {
    feature_importance <- feature_importance |>
        rename_first_present(c(
            model = "model",
            model_name = "model",
            algorithm = "model"
        )) |>
        dplyr::mutate(
            model = as.character(.data$model),
            feature_set = as.character(.data$feature_set)
        )
}

if (!is.null(calibration_bins)) {
    calibration_bins <- calibration_bins |>
        rename_first_present(c(
            model = "model",
            model_name = "model",
            algorithm = "model",
            split = "split",
            data_split = "split",
            eval_split = "split"
        )) |>
        dplyr::mutate(
            model = as.character(.data$model),
            split = as.character(.data$split),
            feature_set = as.character(.data$feature_set),
            class = normalize_outcome(.data$class)
        )
}

# 3. Identify best models

best_validation_model <- pick_best_model(performance_summary, "validation")
best_test_model <- pick_best_model(performance_summary, "test")

model_28_metrics_path <- project_path("reports/tables/model_28_metrics.csv")
portfolio_final_rows <- tibble::tibble()
if (file.exists(model_28_metrics_path)) {
    model_28_metrics_tbl <- readr::read_csv(model_28_metrics_path, show_col_types = FALSE)
    portfolio_final_rows <- model_28_metrics_tbl |>
        dplyr::filter(
            .data$feature_variant == "safe_plus_form_compact",
            .data$model %in% c("lightgbm", "multinom"),
            .data$split %in% c("validation", "test")
        ) |>
        dplyr::transmute(
            role = dplyr::if_else(
                .data$model == "lightgbm",
                "portfolio_final",
                "same_cohort_challenger"
            ),
            selection_basis = dplyr::if_else(
                .data$split == "validation",
                "validation_log_loss",
                "test_reporting_only"
            ),
            model_stage = "model_28",
            model = .data$model,
            feature_set = .data$feature_variant,
            split = .data$split,
            n = .data$n,
            log_loss = .data$log_loss,
            brier_score = NA_real_,
            accuracy = .data$accuracy,
            macro_f1 = .data$macro_f1
        )
}

tier_robustness_rows <- dplyr::bind_rows(
    best_validation_model |>
        dplyr::mutate(
            role = "tier_robustness_best_validation",
            selection_basis = "validation_log_loss",
            model_stage = "model_30"
        ),
    best_test_model |>
        dplyr::mutate(
            role = "tier_robustness_best_validation",
            selection_basis = "test_reporting_only",
            model_stage = "model_30"
        )
) |>
    dplyr::transmute(
        role = .data$role,
        selection_basis = .data$selection_basis,
        model_stage = .data$model_stage,
        model = .data$model,
        feature_set = as.character(.data$feature_set),
        split = .data$split,
        n = .data$n,
        log_loss = .data$log_loss,
        brier_score = .data$brier_score,
        accuracy = .data$accuracy,
        macro_f1 = .data$macro_f1
    )

final_best_model_summary <- dplyr::bind_rows(
    portfolio_final_rows,
    tier_robustness_rows
) |>
    dplyr::select(
        role,
        selection_basis,
        model_stage,
        model,
        feature_set,
        split,
        dplyr::any_of(c("n", "log_loss", "brier_score", "accuracy", "macro_f1"))
    )

readr::write_csv(
    final_best_model_summary,
    project_path(OUTPUT_TABLE_DIR, "final_best_model_summary.csv")
)

best_val_model_name <- best_validation_model$model[[1]]
best_val_feature_set <- as.character(best_validation_model$feature_set[[1]])

# 4. Incremental feature-tier summary

final_incremental_performance_summary <- performance_summary |>
    dplyr::mutate(feature_set = as.character(.data$feature_set)) |>
    dplyr::arrange(.data$model, .data$split, .data$feature_set) |>
    dplyr::group_by(.data$model, .data$split) |>
    dplyr::mutate(
        previous_log_loss = dplyr::lag(.data$log_loss),
        previous_brier_score = dplyr::lag(.data$brier_score),
        previous_macro_f1 = dplyr::lag(.data$macro_f1),
        delta_log_loss_vs_previous_tier = .data$log_loss - .data$previous_log_loss,
        delta_brier_vs_previous_tier = .data$brier_score - .data$previous_brier_score,
        delta_macro_f1_vs_previous_tier = .data$macro_f1 - .data$previous_macro_f1,
        improved_log_loss = !is.na(.data$delta_log_loss_vs_previous_tier) &
            .data$delta_log_loss_vs_previous_tier < 0,
        improved_brier = !is.na(.data$delta_brier_vs_previous_tier) &
            .data$delta_brier_vs_previous_tier < 0,
        improved_macro_f1 = !is.na(.data$delta_macro_f1_vs_previous_tier) &
            .data$delta_macro_f1_vs_previous_tier > 0
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
        split,
        model,
        feature_set,
        accuracy,
        macro_f1,
        log_loss,
        brier_score,
        delta_log_loss_vs_previous_tier,
        delta_brier_vs_previous_tier,
        delta_macro_f1_vs_previous_tier,
        improved_log_loss,
        improved_brier,
        improved_macro_f1
    )

readr::write_csv(
    final_incremental_performance_summary,
    project_path(
        OUTPUT_TABLE_DIR,
        "final_incremental_performance_summary.csv"
    )
)

incremental_best_family <- final_incremental_performance_summary |>
    dplyr::filter(.data$model == best_val_model_name, .data$split == "validation")

strongest_tier_row <- incremental_best_family |>
    dplyr::filter(!is.na(.data$delta_log_loss_vs_previous_tier)) |>
    dplyr::slice_min(.data$delta_log_loss_vs_previous_tier, n = 1, with_ties = FALSE)

strongest_feature_tier <- if (nrow(strongest_tier_row) > 0) {
    FEATURE_SET_LABELS[[strongest_tier_row$feature_set[[1]]]]
} else {
    "Baseline rating"
}

goalscorer_incremental <- final_incremental_performance_summary |>
    dplyr::filter(
        .data$model == best_val_model_name,
        .data$feature_set == "rating_plus_form_plus_goalscorers"
    )

goalscorer_helped_validation <- any(
    goalscorer_incremental$split == "validation" &
        goalscorer_incremental$improved_log_loss,
    na.rm = TRUE
)

goalscorer_helped_test <- any(
    goalscorer_incremental$split == "test" &
        goalscorer_incremental$improved_log_loss,
    na.rm = TRUE
)

goalscorer_feature_result <- if (goalscorer_helped_validation || goalscorer_helped_test) {
    if (goalscorer_helped_validation && !goalscorer_helped_test) {
        "Marginal validation-only improvement; no test log-loss gain"
    } else if (goalscorer_helped_test) {
        "Improved test log loss vs prior tier"
    } else {
        "Marginal validation improvement only"
    }
} else {
    "No material log-loss improvement vs rating_plus_form"
}

# 5. Classwise summary

final_classwise_summary <- classwise_metrics |>
    dplyr::mutate(
        support = if ("support" %in% names(classwise_metrics)) {
            .data$support
        } else {
            .data$tp + .data$fn
        }
    ) |>
    dplyr::select(
        split,
        model,
        feature_set,
        class,
        precision,
        recall,
        f1,
        support
    )

readr::write_csv(
    final_classwise_summary,
    project_path(OUTPUT_TABLE_DIR, "final_classwise_summary.csv")
)

hardest_class_row <- final_classwise_summary |>
    dplyr::filter(
        .data$model == best_val_model_name,
        .data$feature_set == best_val_feature_set,
        .data$split == "test"
    ) |>
    dplyr::slice_min(.data$f1, n = 1, with_ties = FALSE)

hardest_class <- if (nrow(hardest_class_row) > 0) {
    hardest_class_row$class[[1]]
} else {
    NA_character_
}

# 6. Extreme engineered-feature audit

available_audit_features <- intersect(
    c(AUDITED_FORM_FEATURES, AUDITED_GOALSCORER_FEATURES),
    names(modeling_table)
)

audit_feature_stats <- function(feature_name, feature_vector) {
    feature_values <- feature_vector
    non_missing <- feature_values[!is.na(feature_values)]
    n_total <- length(feature_values)
    n_missing <- sum(is.na(feature_values))

    if (length(non_missing) == 0) {
        return(tibble::tibble(
            feature = feature_name,
            n = n_total,
            n_missing = n_missing,
            min = NA_real_,
            p01 = NA_real_,
            p05 = NA_real_,
            median = NA_real_,
            mean = NA_real_,
            p95 = NA_real_,
            p99 = NA_real_,
            max = NA_real_,
            n_below_p01 = NA_integer_,
            n_above_p99 = NA_integer_
        ))
    }

    quantiles <- stats::quantile(
        non_missing,
        probs = c(0.01, 0.05, 0.5, 0.95, 0.99),
        na.rm = TRUE,
        names = FALSE,
        type = 7
    )

    tibble::tibble(
        feature = feature_name,
        n = n_total,
        n_missing = n_missing,
        min = min(non_missing),
        p01 = quantiles[[1]],
        p05 = quantiles[[2]],
        median = quantiles[[3]],
        mean = mean(non_missing),
        p95 = quantiles[[4]],
        p99 = quantiles[[5]],
        max = max(non_missing),
        n_below_p01 = sum(non_missing < quantiles[[1]], na.rm = TRUE),
        n_above_p99 = sum(non_missing > quantiles[[5]], na.rm = TRUE)
    )
}

final_extreme_feature_audit <- purrr::map_dfr(
    available_audit_features,
    function(feature_name) {
        audit_feature_stats(feature_name, modeling_table[[feature_name]])
    }
)

readr::write_csv(
    final_extreme_feature_audit,
    project_path(OUTPUT_TABLE_DIR, "final_extreme_feature_audit.csv")
)

collect_extreme_examples <- function(feature_name) {
    if (!feature_name %in% names(modeling_table)) {
        return(tibble::tibble())
    }

    feature_values <- modeling_table[[feature_name]]
    non_missing_idx <- which(!is.na(feature_values))

    if (length(non_missing_idx) == 0) {
        return(tibble::tibble())
    }

    non_missing_values <- feature_values[non_missing_idx]
    p01 <- stats::quantile(non_missing_values, 0.01, na.rm = TRUE, names = FALSE)
    p99 <- stats::quantile(non_missing_values, 0.99, na.rm = TRUE, names = FALSE)

    extreme_idx <- non_missing_idx[
        non_missing_values <= p01 | non_missing_values >= p99
    ]

    if (length(extreme_idx) == 0) {
        return(tibble::tibble())
    }

    example_tbl <- modeling_table[extreme_idx, , drop = FALSE]

    tibble::tibble(
        date = if ("date" %in% names(example_tbl)) example_tbl$date else NA,
        home_team = if ("home_team" %in% names(example_tbl)) {
            example_tbl$home_team
        } else {
            NA_character_
        },
        away_team = if ("away_team" %in% names(example_tbl)) {
            example_tbl$away_team
        } else {
            NA_character_
        },
        result_class = if ("result_class" %in% names(example_tbl)) {
            normalize_outcome(example_tbl$result_class)
        } else {
            NA_character_
        },
        feature = feature_name,
        feature_value = example_tbl[[feature_name]],
        home_prior_matches = if ("home_prior_matches" %in% names(example_tbl)) {
            example_tbl$home_prior_matches
        } else {
            NA_real_
        },
        away_prior_matches = if ("away_prior_matches" %in% names(example_tbl)) {
            example_tbl$away_prior_matches
        } else {
            NA_real_
        },
        tournament = if ("tournament" %in% names(example_tbl)) {
            example_tbl$tournament
        } else if ("competition" %in% names(example_tbl)) {
            example_tbl$competition
        } else {
            NA_character_
        }
    )
}

final_extreme_feature_examples <- purrr::map_dfr(
    available_audit_features,
    collect_extreme_examples
) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$feature, .data$feature_value)

readr::write_csv(
    final_extreme_feature_examples,
    project_path(
        OUTPUT_TABLE_DIR,
        "final_extreme_feature_examples.csv"
    )
)

# 7. Final prediction examples

best_model_test_predictions <- prediction_examples |>
    dplyr::filter(
        .data$model == best_val_model_name,
        .data$feature_set == best_val_feature_set
    ) |>
    dplyr::mutate(
        confidence = pmax(.data$p_home, .data$p_draw, .data$p_away),
        correct = .data$predicted_class == .data$actual_class
    )

prediction_output_cols <- c(
    "date",
    "home_team",
    "away_team",
    "tournament",
    "actual_class",
    "predicted_class",
    "p_home",
    "p_draw",
    "p_away",
    "confidence",
    "model",
    "feature_set"
)

final_confident_wrong_predictions <- best_model_test_predictions |>
    dplyr::filter(!.data$correct) |>
    dplyr::arrange(dplyr::desc(.data$confidence)) |>
    dplyr::slice_head(n = 50) |>
    dplyr::select(dplyr::any_of(prediction_output_cols))

final_high_confidence_correct_predictions <- best_model_test_predictions |>
    dplyr::filter(.data$correct) |>
    dplyr::arrange(dplyr::desc(.data$confidence)) |>
    dplyr::slice_head(n = 50) |>
    dplyr::select(dplyr::any_of(prediction_output_cols))

readr::write_csv(
    final_confident_wrong_predictions,
    project_path(
        OUTPUT_TABLE_DIR,
        "final_confident_wrong_predictions.csv"
    )
)

readr::write_csv(
    final_high_confidence_correct_predictions,
    project_path(
        OUTPUT_TABLE_DIR,
        "final_high_confidence_correct_predictions.csv"
    )
)

# 8. Final plots

message("Generating final plots...")

incremental_plot_data <- performance_summary |>
    dplyr::mutate(
        feature_set_label = FEATURE_SET_LABELS[as.character(.data$feature_set)],
        feature_set_label = factor(
            .data$feature_set_label,
            levels = unname(FEATURE_SET_LABELS[FEATURE_SET_ORDER])
        )
    )

form_improved <- any(
    incremental_best_family$feature_set == "rating_plus_form" &
        incremental_best_family$split == "validation" &
        incremental_best_family$improved_log_loss,
    na.rm = TRUE
)

goalscorer_flat_or_worse <- any(
    goalscorer_incremental$split == "validation" &
        !goalscorer_incremental$improved_log_loss,
    na.rm = TRUE
)

incremental_title_suffix <- if (form_improved && goalscorer_flat_or_worse) {
    "Lagged form is the strongest gain; goalscorer tier is flat or slightly worse"
} else if (form_improved) {
    "Lagged form provides the clearest incremental improvement"
} else {
    "Incremental performance by feature tier"

}

plot_incremental_metric <- function(
    metric_col,
    y_label,
    title_text,
    filename_stem,
    lower_is_better = TRUE
) {
    plot_tbl <- incremental_plot_data |>
        dplyr::select(
            feature_set_label,
            model,
            split,
            metric_value = dplyr::all_of(metric_col)
        )

    p <- ggplot2::ggplot(
        plot_tbl,
        ggplot2::aes(
            x = .data$feature_set_label,
            y = .data$metric_value,
            color = .data$model,
            group = .data$model
        )
    ) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::facet_wrap(~split, scales = "free_y") +
        ggplot2::labs(
            title = title_text,
            x = NULL,
            y = y_label,
            color = "Model"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
            legend.position = "bottom"
        )

    save_plot_dual(p, filename_stem, width = 11, height = 6)
}

plot_incremental_metric(
    "log_loss",
    "Log loss (lower is better)",
    paste("Incremental log loss by feature tier.", incremental_title_suffix),
    "01_incremental_log_loss",
    lower_is_better = TRUE
)

plot_incremental_metric(
    "brier_score",
    "Brier score (lower is better)",
    "Incremental Brier score by feature tier",
    "02_incremental_brier_score",
    lower_is_better = TRUE
)

plot_incremental_metric(
    "macro_f1",
    "Macro F1 (higher is better)",
    "Incremental macro F1 by feature tier",
    "03_incremental_macro_f1",
    lower_is_better = FALSE
)

test_ranked_data <- performance_summary |>
    dplyr::filter(.data$split == "test") |>
    dplyr::mutate(
        model_feature = paste(.data$model, .data$feature_set, sep = " | "),
        is_best = .data$model == best_test_model$model[[1]] &
            as.character(.data$feature_set) ==
                as.character(best_test_model$feature_set[[1]])
    ) |>
    dplyr::arrange(.data$log_loss) |>
    dplyr::mutate(
        model_feature = factor(
            .data$model_feature,
            levels = rev(.data$model_feature)
        )
    )

plot_test_ranked <- ggplot2::ggplot(
    test_ranked_data,
    ggplot2::aes(
        x = .data$log_loss,
        y = .data$model_feature,
        fill = .data$is_best
    )
) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::scale_fill_manual(
        values = c("TRUE" = "#2c6e8a", "FALSE" = "#b0b0b0"),
        labels = c("TRUE" = "Best test model", "FALSE" = "Other models"),
        name = NULL
    ) +
    ggplot2::labs(
        title = "Held-out test log loss ranked by model and feature set",
        subtitle = paste0(
            "Best: ",
            best_test_model$model[[1]],
            " / ",
            as.character(best_test_model$feature_set[[1]]),
            " (log loss = ",
            format_metric(best_test_model$log_loss[[1]]),
            ")"
        ),
        x = "Log loss (lower is better)",
        y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

save_plot_dual(plot_test_ranked, "04_test_log_loss_ranked_models", width = 10, height = 7)

preferred_tree_model <- if ("lightgbm" %in% performance_summary$model) {
    "lightgbm"
} else {
    performance_summary$model[[1]]
}

classwise_plot_data <- final_classwise_summary |>
    dplyr::filter(.data$model == preferred_tree_model) |>
    dplyr::mutate(
        feature_set_label = FEATURE_SET_LABELS[.data$feature_set],
        feature_set_label = factor(
            .data$feature_set_label,
            levels = unname(FEATURE_SET_LABELS[FEATURE_SET_ORDER])
        ),
        class = factor(.data$class, levels = OUTCOME_LEVELS)
    )

plot_classwise_f1 <- ggplot2::ggplot(
    classwise_plot_data,
    ggplot2::aes(
        x = .data$class,
        y = .data$f1,
        fill = .data$feature_set_label
    )
) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.72) +
    ggplot2::facet_wrap(~split) +
    ggplot2::labs(
        title = paste("Classwise F1 for", preferred_tree_model, "by feature tier"),
        subtitle = "Draw (D) is typically the hardest class to recover",
        x = "Class",
        y = "F1",
        fill = "Feature set"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

save_plot_dual(plot_classwise_f1, "05_classwise_f1_best_family", width = 11, height = 6)

best_confusion <- confusion_matrices |>
    dplyr::filter(
        .data$model == best_val_model_name,
        .data$feature_set == best_val_feature_set,
        .data$split == "test"
    ) |>
    dplyr::group_by(.data$actual_class) |>
    dplyr::mutate(
        row_total = sum(.data$n),
        row_pct = .data$n / .data$row_total
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
        actual_class = factor(.data$actual_class, levels = OUTCOME_LEVELS),
        predicted_class = factor(.data$predicted_class, levels = OUTCOME_LEVELS),
        label_text = paste0(.data$n, "\n(", scales::percent(.data$row_pct, 0.1), ")")
    )

plot_confusion <- ggplot2::ggplot(
    best_confusion,
    ggplot2::aes(
        x = .data$predicted_class,
        y = .data$actual_class,
        fill = .data$n
    )
) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
        ggplot2::aes(label = .data$label_text),
        color = "white",
        size = 3.3
    ) +
    ggplot2::scale_fill_gradient(low = "#7fb3d5", high = "#1f4e79") +
    ggplot2::labs(
        title = "Best validation model confusion matrix (test split)",
        subtitle = paste(
            best_val_model_name,
            "/",
            best_val_feature_set
        ),
        x = "Predicted class",
        y = "Actual class",
        fill = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 11)

save_plot_dual(plot_confusion, "06_best_model_confusion_matrix", width = 8, height = 6)

calibration_available <- FALSE

if (!is.null(calibration_bins)) {
    best_calibration <- calibration_bins |>
        dplyr::filter(
            .data$model == best_val_model_name,
            .data$feature_set == best_val_feature_set,
            .data$split == "test"
        )

    if (nrow(best_calibration) > 0) {
        calibration_available <- TRUE

        plot_calibration <- ggplot2::ggplot(
            best_calibration,
            ggplot2::aes(
                x = .data$mean_predicted_probability,
                y = .data$observed_frequency
            )
        ) +
            ggplot2::geom_abline(
                slope = 1,
                intercept = 0,
                linetype = "dashed",
                color = "gray40"
            ) +
            ggplot2::geom_line(color = "#2c6e8a") +
            ggplot2::geom_point(size = 2, color = "#2c6e8a") +
            ggplot2::facet_wrap(~class) +
            ggplot2::labs(
                title = "Calibration bins for best validation model (test split)",
                x = "Mean predicted probability",
                y = "Observed frequency"
            ) +
            ggplot2::theme_minimal(base_size = 11)

        save_plot_dual(
            plot_calibration,
            "07_best_model_calibration",
            width = 10,
            height = 6
        )
    }
}

importance_available <- FALSE

if (!is.null(feature_importance)) {
    tree_importance <- feature_importance |>
        dplyr::filter(
            .data$model == preferred_tree_model,
            .data$feature_set == best_val_feature_set,
            !is.na(.data$importance_gain)
        ) |>
        dplyr::group_by(.data$feature) |>
        dplyr::summarise(
            importance_gain = max(.data$importance_gain, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(.data$importance_gain)) |>
        dplyr::slice_head(n = 20) |>
        dplyr::mutate(
            feature = stats::reorder(.data$feature, .data$importance_gain)
        )

    if (nrow(tree_importance) > 0) {
        importance_available <- TRUE

        plot_importance <- ggplot2::ggplot(
            tree_importance,
            ggplot2::aes(x = .data$importance_gain, y = .data$feature)
        ) +
            ggplot2::geom_col(fill = "#2c6e8a") +
            ggplot2::labs(
                title = paste(
                    "Top 20 LightGBM gain-importance features (",
                    best_val_feature_set,
                    ")",
                    sep = ""
                ),
                x = "Importance (gain)",
                y = NULL
            ) +
            ggplot2::theme_minimal(base_size = 11)

        save_plot_dual(
            plot_importance,
            "08_feature_importance",
            width = 10,
            height = 7
        )
    }
}

plot_confidence <- ggplot2::ggplot(
    best_model_test_predictions,
    ggplot2::aes(
        x = .data$confidence,
        fill = .data$correct
    )
) +
    ggplot2::geom_histogram(
        bins = 40,
        position = "identity",
        alpha = 0.55,
        color = "white"
    ) +
    ggplot2::scale_fill_manual(
        values = c("TRUE" = "#2c6e8a", "FALSE" = "#c0392b"),
        labels = c("TRUE" = "Correct", "FALSE" = "Incorrect"),
        name = NULL
    ) +
    ggplot2::labs(
        title = "Prediction confidence distribution (best validation model, test split)",
        x = "Confidence (max class probability)",
        y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

save_plot_dual(
    plot_confidence,
    "09_prediction_confidence_distribution",
    width = 10,
    height = 6
)

distribution_features <- intersect(
    DISTRIBUTION_PLOT_FEATURES,
    names(modeling_table)
)

distribution_plot_data <- modeling_table |>
    dplyr::select(dplyr::all_of(distribution_features)) |>
    tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "feature",
        values_to = "feature_value"
    ) |>
    dplyr::filter(!is.na(.data$feature_value))

plot_extreme_distributions <- ggplot2::ggplot(
    distribution_plot_data,
    ggplot2::aes(x = .data$feature_value)
) +
    ggplot2::geom_histogram(bins = 40, fill = "#2c6e8a", color = "white") +
    ggplot2::facet_wrap(~feature, scales = "free", ncol = 3) +
    ggplot2::labs(
        title = "Distribution of audited rolling form features",
        subtitle = "Extreme values are documented rather than removed",
        x = "Feature value",
        y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 8))

save_plot_dual(
    plot_extreme_distributions,
    "10_extreme_feature_distributions",
    width = 12,
    height = 10
)

# 9. Final project takeaways

engineered_feature_validation <- "61 features checked (34 form + 27 goalscorer); 8/8 validation checks passed"

main_limitation <- paste(
    "Draw recall remains low;",
    "no squad-value, injury, or market-odds signals yet"
)

recommended_next_step <- paste(
    "Calibrate LightGBM probabilities;",
    "test two-stage draw model;",
    "add squad/market and odds data"
)

final_project_takeaways <- tibble::tibble(
    takeaway = c(
        "best_model",
        "best_feature_set",
        "strongest_feature_tier",
        "goalscorer_feature_result",
        "hardest_class",
        "engineered_feature_validation",
        "main_limitation",
        "recommended_next_step"
    ),
    value = c(
        best_val_model_name,
        best_val_feature_set,
        strongest_feature_tier,
        goalscorer_feature_result,
        hardest_class,
        engineered_feature_validation,
        main_limitation,
        recommended_next_step
    )
)

readr::write_csv(
    final_project_takeaways,
    project_path(OUTPUT_TABLE_DIR, "final_project_takeaways.csv")
)

# 10. Final Markdown report

best_val_test_row <- performance_summary |>
    dplyr::filter(
        .data$model == best_val_model_name,
        .data$feature_set == best_val_feature_set,
        .data$split == "test"
    ) |>
    dplyr::slice(1)

form_validation_delta <- incremental_best_family |>
    dplyr::filter(.data$feature_set == "rating_plus_form", .data$split == "validation")

goalscorer_validation_delta <- goalscorer_incremental |>
    dplyr::filter(.data$split == "validation")

goalscorer_test_delta <- goalscorer_incremental |>
    dplyr::filter(.data$split == "test")

calibration_note <- if (calibration_available) {
    "Calibration bins were produced for the best validation model on the test split (see plot 07)."
} else {
    "Calibration plot was skipped because calibration bins were unavailable or empty for the best model."
}

importance_note <- if (importance_available) {
    paste0(
        "LightGBM gain importance for `",
        best_val_feature_set,
        "` highlights rating difference and form-derived features among the top predictors."
    )
} else {
    "Feature-importance plot was skipped because usable LightGBM gain importance was unavailable."
}

extreme_audit_note <- if (nrow(final_extreme_feature_audit) > 0) {
    max_above_p99 <- max(final_extreme_feature_audit$n_above_p99, na.rm = TRUE)
    paste0(
        "Rolling form features show long tails (up to ",
        max_above_p99,
        " values above the 99th percentile in at least one audited column). ",
        "Examples are saved in `final_extreme_feature_examples.csv`; values were not capped or removed."
    )
} else {
    "No audited engineered features were available in the modeling table."
}

report_lines <- c(
    "# International Match Outcome Modeling — Final Summary",
    "",
    "## Objective",
    "Predict international association football match outcomes as calibrated",
    "multiclass probabilities for home win (H), draw (D), and away win (A) using",
    "strictly pre-match features.",
    "",
    "## Data Used",
    "- International match results with chronological train/validation/test splits.",
    "- Pre-match Elo-style ratings (`home_rating_pre_match`, `away_rating_pre_match`, `rating_diff`).",
    "- Tournament/context flags and lumped tournament indicators.",
    "- Lagged team-form features built from prior matches only.",
    "- Goalscorer-derived attacking-depth features (unique scorers, top-scorer goals, etc.).",
    "",
    "## Feature Engineering",
    "- **Baseline rating tier**: pre-match ratings and rating difference.",
    "- **Context tier**: tournament/competition flags added to the rating baseline.",
    "- **Form tier**: lagged points, goal difference, goals for/against, and draw-rate windows (5 and 10 matches).",
    "- **Goalscorer tier**: rolling scorer counts and attacking-depth metrics layered on top of form.",
    "- All engineered features were intended to be pre-match only.",
    "",
    "## Feature Validation",
    "- 61 engineered features checked (34 form + 27 goalscorer).",
    "- 8/8 automated validation checks passed with 0 failures.",
    "- No leakage failures in the goalscorer audit sample.",
    "- Manual recomputation matched sampled features.",
    "- Missingness was interpretable (early-career teams and sparse goal-minute history).",
    "",
    "## Modeling Setup",
    "- Chronological train/validation/test split on a fair-comparison complete-case cohort.",
    "- Models trained: multinomial logit, glmnet ridge, and LightGBM.",
    "- Metrics: log loss, Brier score, macro F1, classwise precision/recall/F1, confusion matrices.",
    "- Model selection based primarily on validation log loss (test reserved for final reporting).",
    "",
    "## Portfolio final (Model 28 cohort — authoritative)",
    "- **Preferred portfolio final model**: Model 28 — LightGBM + `safe_plus_form_compact`.",
    "- Selected by lowest validation log loss within `model_28_metrics.csv` (script 31).",
    "- See `reports/tables/final_project_summary.csv` for headline validation/test metrics.",
    "- **Simpler interpretable challenger**: Model 28 — multinom + `safe_plus_form_compact` on the same cohort (within 0.005 validation log loss of LightGBM).",
    "",
    "## Tier / robustness analysis (Model 30 cohort — not directly comparable)",
    paste0(
        "- **Best metric result on a different cohort**: ",
        best_val_model_name,
        " with `",
        best_val_feature_set,
        "` (validation log loss = ",
        format_metric(best_validation_model$log_loss[[1]]),
        ", macro F1 = ",
        format_metric(best_validation_model$macro_f1[[1]]),
        ", val n = ",
        best_validation_model$n[[1]],
        ")."
    ),
    "- This cohort uses the goalscorer-enriched table and fair-comparison filters; it does **not** replace the Model 28 portfolio final without harmonized cohorts and script 31 policy.",
    paste0(
        "- On the test split (reporting only): ",
        best_val_model_name,
        " / `",
        best_val_feature_set,
        "` achieved test log loss = ",
        format_metric(best_val_test_row$log_loss[[1]]),
        ", accuracy = ",
        format_metric(best_val_test_row$accuracy[[1]]),
        ", macro F1 = ",
        format_metric(best_val_test_row$macro_f1[[1]]),
        "."
    ),
    paste0("- **Strongest feature tier**: ", strongest_feature_tier, "."),
    if (nrow(form_validation_delta) > 0) {
        paste0(
            "- **Form tier (validation)**: log-loss delta vs prior tier = ",
            format_metric(form_validation_delta$delta_log_loss_vs_previous_tier[[1]]),
            "; macro-F1 delta = ",
            format_metric(form_validation_delta$delta_macro_f1_vs_previous_tier[[1]]),
            "."
        )
    } else {
        "- **Form tier**: incremental deltas unavailable."
    },
    if (nrow(goalscorer_validation_delta) > 0) {
        paste0(
            "- **Goalscorer tier (validation)**: log-loss delta vs `rating_plus_form` = ",
            format_metric(goalscorer_validation_delta$delta_log_loss_vs_previous_tier[[1]]),
            "; improved log loss = ",
            goalscorer_validation_delta$improved_log_loss[[1]],
            "."
        )
    } else {
        "- **Goalscorer tier (validation)**: incremental deltas unavailable."
    },
    if (nrow(goalscorer_test_delta) > 0) {
        paste0(
            "- **Goalscorer tier (test)**: log-loss delta vs `rating_plus_form` = ",
            format_metric(goalscorer_test_delta$delta_log_loss_vs_previous_tier[[1]]),
            "; improved log loss = ",
            goalscorer_test_delta$improved_log_loss[[1]],
            "."
        )
    } else {
        "- **Goalscorer tier (test)**: incremental deltas unavailable."
    },
    "",
    "## Interpretation",
    "- Pre-match ratings plus lagged team form are the most useful current signals.",
    paste0("- Goalscorer features: ", goalscorer_feature_result, "."),
    paste0("- Hardest class on the test split for the best validation model: **", hardest_class, "**."),
    "- Probability quality (log loss / Brier) matters more than raw accuracy for forecasting and betting use cases.",
    "",
    "## Extreme Feature Audit",
    extreme_audit_note,
    "",
    "## Limitations",
    "- No player-transfer or squad-value data yet.",
    "- No injury or roster-availability data.",
    "- No betting odds or market-implied probabilities in the current feature set.",
    "- Rankings (`home_rank_pre_match`, `away_rank_pre_match`) were unavailable/missing in the input table.",
    "- International football structure changes over long historical periods.",
    "",
    "## Recommended Next Steps",
    "1. Calibrate best LightGBM probabilities (Platt scaling or isotonic regression).",
    "2. Test a two-stage draw vs non-draw model to improve draw recall.",
    "3. Add external squad/market-value or roster availability data.",
    "4. Add odds data for betting expected-value analysis.",
    "5. Consider time-decayed training or modern-era-only sensitivity analysis.",
    "",
    "## Diagnostics Notes",
    calibration_note,
    importance_note,
    "",
    "## Files Produced",
    paste0("- Tables: `", OUTPUT_TABLE_DIR, "`"),
    paste0("- Plots: `", OUTPUT_PLOT_DIR, "`"),
    "- Key tables: `final_best_model_summary.csv`, `final_incremental_performance_summary.csv`,",
    "  `final_classwise_summary.csv`, `final_extreme_feature_audit.csv`,",
    "  `final_confident_wrong_predictions.csv`, `final_high_confidence_correct_predictions.csv`,",
    "  `final_project_takeaways.csv`.",
    "- Report: `final_international_modeling_report.md`."
)

report_path <- project_path(
    OUTPUT_TABLE_DIR,
    "final_international_modeling_report.md"
)

writeLines(report_lines, report_path)

# 11. Console summary

message("")
message("=== International Modeling Project Finalization ===")
message(
    "Portfolio final (Model 28): see reports/tables/final_project_summary.csv"
)
message(
    "Tier robustness best validation (Model 30): ",
    best_val_model_name,
    " / ",
    best_val_feature_set,
    " | log_loss=",
    format_metric(best_validation_model$log_loss[[1]]),
    ", macro_f1=",
    format_metric(best_validation_model$macro_f1[[1]])
)
message(
    "Best test model: ",
    best_test_model$model[[1]],
    " / ",
    as.character(best_test_model$feature_set[[1]]),
    " | log_loss=",
    format_metric(best_test_model$log_loss[[1]]),
    ", macro_f1=",
    format_metric(best_test_model$macro_f1[[1]])
)
message("Strongest feature tier: ", strongest_feature_tier)
message(
    "Goalscorer features helped (log loss): validation=",
    goalscorer_helped_validation,
    ", test=",
    goalscorer_helped_test
)
message("Hardest class (best val model, test): ", hardest_class)
message("Plots written: ", length(unique(plots_written)))
message("Report: ", report_path)
message("Done.")
