# 33_model_hyperparameter_sensitivity.R
#
# Controlled hyperparameter sensitivity for Model 28's final feature tier
# (safe_plus_form_compact). Validation-only selection; single test evaluation
# for the validation winner.
#
# Reads:  data/processed/international_modeling_table_with_form.csv
#         reports/tables/model_28_metrics.csv (optional reference)
#
# Writes: reports/tables/model_33_*
#         reports/figures/model_33_*
#         reports/final/model_33_hyperparameter_sensitivity_summary.md
#         models/model_33_* (only if validation improves materially)

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

RANDOM_SEED <- 2026L
TARGET_LEVELS <- c("H", "D", "A")
FEATURE_VARIANT <- "safe_plus_form_compact"
RATING_FRESHNESS_DAYS <- 365L
MINIMUM_PRIOR_MATCHES <- 10L
VALIDATION_FRACTION <- 0.20
MATERIALITY_THRESHOLD <- 0.005
MODEST_IMPROVEMENT_THRESHOLD <- 0.003

dir.create(file.path(REPORTS_DIR, "final"), recursive = TRUE, showWarnings = FALSE)

required_pkgs <- c("readr", "dplyr", "tidyr", "tibble", "purrr", "ggplot2", "nnet")
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

has_glmnet <- requireNamespace("glmnet", quietly = TRUE)
has_lightgbm <- requireNamespace("lightgbm", quietly = TRUE)

if (!has_glmnet) {
    stop("Package glmnet is required for Model 33 sensitivity.", call. = FALSE)
}
if (!has_lightgbm) {
    stop("Package lightgbm is required for Model 33 sensitivity.", call. = FALSE)
}

# --- helpers -----------------------------------------------------------------

clip_probs <- function(probability_values, eps = 1e-15) {
    pmax(pmin(probability_values, 1 - eps), eps)
}

validate_probability_table <- function(probs, model_name) {
    required_cols <- c("pred_H", "pred_D", "pred_A")
    missing_cols <- setdiff(required_cols, names(probs))
    if (length(missing_cols) > 0) {
        stop(
            model_name,
            " probability table is missing columns: ",
            paste(missing_cols, collapse = ", "),
            call. = FALSE
        )
    }

    probs_matrix <- as.matrix(probs[, required_cols])
    if (any(!is.finite(probs_matrix))) {
        stop(model_name, " probability table contains non-finite values.", call. = FALSE)
    }
    if (any(probs_matrix < -1e-8 | probs_matrix > 1 + 1e-8)) {
        stop(model_name, " probability table contains values outside [0, 1].", call. = FALSE)
    }
    row_sums <- rowSums(probs_matrix)
    if (any(abs(row_sums - 1) > 1e-6)) {
        stop(model_name, " probability rows do not sum to 1.", call. = FALSE)
    }
    invisible(TRUE)
}

multiclass_log_loss <- function(truth, probs) {
    probs <- as.matrix(probs[, c("pred_H", "pred_D", "pred_A")])
    probs <- clip_probs(probs)
    truth <- as.character(truth)
    class_index <- match(truth, TARGET_LEVELS)
    if (any(is.na(class_index))) {
        stop("Truth contains values outside H, D, A.", call. = FALSE)
    }
    -mean(log(probs[cbind(seq_along(class_index), class_index)]))
}

multiclass_brier <- function(truth, probs) {
    truth <- as.character(truth)
    brier_h <- mean((probs$pred_H - as.integer(truth == "H"))^2)
    brier_d <- mean((probs$pred_D - as.integer(truth == "D"))^2)
    brier_a <- mean((probs$pred_A - as.integer(truth == "A"))^2)
    brier_h + brier_d + brier_a
}

classwise_metrics <- function(
    truth,
    probs,
    model_family,
    config_id,
    split_name
) {
    truth <- as.character(truth)
    pred <- TARGET_LEVELS[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    purrr::map_dfr(TARGET_LEVELS, function(cls) {
        tp <- sum(pred == cls & truth == cls)
        fp <- sum(pred == cls & truth != cls)
        fn <- sum(pred != cls & truth == cls)
        tn <- sum(pred != cls & truth != cls)

        precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
        recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
        f1 <- ifelse(
            precision + recall == 0,
            0,
            2 * precision * recall / (precision + recall)
        )

        tibble::tibble(
            model_family = model_family,
            config_id = config_id,
            feature_variant = FEATURE_VARIANT,
            split = split_name,
            class = cls,
            tp = tp,
            fp = fp,
            fn = fn,
            tn = tn,
            precision = precision,
            recall = recall,
            f1 = f1
        )
    })
}

macro_f1_score <- function(truth, probs) {
    classwise_metrics(
        truth,
        probs,
        model_family = "tmp",
        config_id = "tmp",
        split_name = "tmp"
    ) |>
        dplyr::summarise(macro_f1 = mean(f1), .groups = "drop") |>
        dplyr::pull(macro_f1)
}

score_model <- function(
    truth,
    probs,
    model_family,
    config_id,
    split_name
) {
    validate_probability_table(probs, config_id)
    truth <- as.character(truth)
    pred <- TARGET_LEVELS[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    tibble::tibble(
        model_family = model_family,
        config_id = config_id,
        feature_variant = FEATURE_VARIANT,
        split = split_name,
        n = length(truth),
        log_loss = multiclass_log_loss(truth, probs),
        brier_score = multiclass_brier(truth, probs),
        accuracy = mean(pred == truth),
        macro_f1 = macro_f1_score(truth, probs)
    )
}

safe_rename_probability_columns <- function(probs) {
    probs <- as.data.frame(probs)
    if (all(c("pred_H", "pred_D", "pred_A") %in% names(probs))) {
        return(probs[, c("pred_H", "pred_D", "pred_A"), drop = FALSE])
    }
    missing_cols <- setdiff(TARGET_LEVELS, names(probs))
    if (length(missing_cols) > 0) {
        stop(
            "Predicted probability table missing class columns: ",
            paste(missing_cols, collapse = ", "),
            call. = FALSE
        )
    }
    probs |>
        dplyr::transmute(
            pred_H = .data$H,
            pred_D = .data$D,
            pred_A = .data$A
        )
}

lightgbm_raw_probs_to_pred_columns <- function(raw_pred, n_rows) {
    if (is.matrix(raw_pred)) {
        prob_matrix <- raw_pred
    } else {
        prob_matrix <- matrix(raw_pred, ncol = 3, byrow = TRUE)
    }
    if (nrow(prob_matrix) != n_rows) {
        stop("LightGBM prediction row count mismatch.", call. = FALSE)
    }
    colnames(prob_matrix) <- c("pred_A", "pred_D", "pred_H")
    as.data.frame(prob_matrix) |>
        dplyr::select(pred_H, pred_D, pred_A)
}

make_calibration_bins <- function(
    truth,
    probs,
    model_family,
    config_id,
    split_name,
    n_bins = 10
) {
    truth <- as.character(truth)
    probability_breaks <- seq(0, 1, length.out = n_bins + 1)

    purrr::map_dfr(TARGET_LEVELS, function(cls) {
        prob_col <- paste0("pred_", cls)
        truth_binary <- as.integer(truth == cls)
        prob_values <- probs[[prob_col]]

        tibble::tibble(
            class = cls,
            probability_value = prob_values,
            observed = truth_binary
        ) |>
            dplyr::mutate(
                probability_bin = cut(
                    probability_value,
                    breaks = probability_breaks,
                    include.lowest = TRUE,
                    labels = FALSE
                )
            ) |>
            dplyr::group_by(class, probability_bin) |>
            dplyr::summarise(
                model_family = model_family,
                config_id = config_id,
                feature_variant = FEATURE_VARIANT,
                split = split_name,
                mean_predicted_probability = mean(probability_value),
                observed_frequency = mean(observed),
                n = dplyr::n(),
                .groups = "drop"
            )
    })
}

make_chronological_splits <- function(modeling_df) {
    make_chronological_modeling_splits(
        modeling_df,
        validation_fraction = VALIDATION_FRACTION
    )
}

constant_probability_probs <- function(new_data, class_probs) {
    class_probs <- class_probs[TARGET_LEVELS]
    class_probs <- class_probs / sum(class_probs)
    tibble::tibble(
        pred_H = class_probs[["H"]],
        pred_D = class_probs[["D"]],
        pred_A = class_probs[["A"]]
    )[rep(1, nrow(new_data)), , drop = FALSE]
}

estimate_class_probabilities <- function(training_data) {
    class_frequency <- training_data |>
        dplyr::count(match_result) |>
        tidyr::complete(
            match_result = factor(TARGET_LEVELS, levels = TARGET_LEVELS),
            fill = list(n = 0)
        ) |>
        dplyr::mutate(probability = n / sum(n))

    class_probs <- class_frequency$probability
    names(class_probs) <- as.character(class_frequency$match_result)
    class_probs
}

make_majority_probs <- function(training_data, epsilon = 0.01) {
    majority_class <- training_data |>
        dplyr::count(match_result, sort = TRUE) |>
        dplyr::slice(1) |>
        dplyr::pull(match_result) |>
        as.character()

    probs <- stats::setNames(rep(epsilon / 2, 3), TARGET_LEVELS)
    probs[[majority_class]] <- 1 - epsilon
    probs / sum(probs)
}

lightgbm_reproducibility_params <- function() {
    list(
        seed = RANDOM_SEED,
        feature_fraction_seed = RANDOM_SEED,
        bagging_seed = RANDOM_SEED,
        data_random_seed = RANDOM_SEED,
        deterministic = TRUE,
        force_col_wise = TRUE
    )
}

fit_lightgbm_config <- function(
    train,
    validation,
    selected_features,
    config_id,
    grid_params
) {
    label_map <- c("A" = 0, "D" = 1, "H" = 2)

    train_lgb <- train |>
        dplyr::mutate(label = unname(label_map[as.character(match_result)]))
    validation_lgb <- validation |>
        dplyr::mutate(label = unname(label_map[as.character(match_result)]))

    x_train_lgb <- as.matrix(train_lgb[, selected_features])
    x_validation_lgb <- as.matrix(validation_lgb[, selected_features])

    dtrain <- lightgbm::lgb.Dataset(data = x_train_lgb, label = train_lgb$label)
    dvalidation <- lightgbm::lgb.Dataset(
        data = x_validation_lgb,
        label = validation_lgb$label
    )

    lgb_params <- c(
        list(
            objective = "multiclass",
            metric = "multi_logloss",
            num_class = 3,
            learning_rate = grid_params$learning_rate,
            num_leaves = grid_params$num_leaves,
            max_depth = grid_params$max_depth,
            min_data_in_leaf = grid_params$min_data_in_leaf,
            feature_fraction = grid_params$feature_fraction,
            bagging_fraction = grid_params$bagging_fraction,
            bagging_freq = 1L,
            lambda_l1 = grid_params$lambda_l1,
            lambda_l2 = grid_params$lambda_l2,
            verbosity = -1
        ),
        lightgbm_reproducibility_params()
    )

    lgb_fit <- lightgbm::lgb.train(
        params = lgb_params,
        data = dtrain,
        nrounds = 1000,
        valids = list(validation = dvalidation),
        early_stopping_rounds = 50,
        verbose = 0
    )

    val_probs <- lightgbm_raw_probs_to_pred_columns(
        predict(lgb_fit, x_validation_lgb),
        nrow(validation)
    )

    list(
        fit = lgb_fit,
        validation_probs = val_probs,
        best_iteration = lgb_fit$best_iter
    )
}

predict_lightgbm_matrix <- function(lgb_fit, feature_matrix) {
    lightgbm_raw_probs_to_pred_columns(
        predict(lgb_fit, feature_matrix),
        nrow(feature_matrix)
    )
}

# --- feature set (Model 28 safe_plus_form_compact) ---------------------------

base_safe_features <- c(
    "rating_diff",
    "home_rating_pre_match",
    "away_rating_pre_match",
    "rating_age_days_home",
    "rating_age_days_away",
    "neutral",
    "is_world_cup",
    "is_world_cup_qualifier",
    "is_continental_tournament",
    "is_continental_qualifier",
    "is_friendly"
)

form_compact_features <- c(
    "home_points_per_match_last_5",
    "away_points_per_match_last_5",
    "form_points_diff_last_5",
    "home_goal_diff_per_match_last_5",
    "away_goal_diff_per_match_last_5",
    "form_goal_diff_diff_last_5",
    "home_draw_rate_last_10",
    "away_draw_rate_last_10",
    "form_draw_rate_mean_last_10"
)

selected_features <- c(base_safe_features, form_compact_features)

leakage_cols <- c(
    "home_score",
    "away_score",
    "home_goals",
    "away_goals",
    "score",
    "goal_diff",
    "total_goals",
    "result_class",
    "match_result_numeric",
    "shootout_winner",
    "shootout_result",
    "home_penalties",
    "away_penalties",
    "winner",
    "outcome",
    "home_win",
    "draw",
    "away_win",
    "home_won_shootout",
    "away_won_shootout",
    "shootout_played"
)

use_with_care_features <- c(
    "home_team_clean",
    "away_team_clean",
    "city",
    "competition",
    "country",
    "season",
    "tournament"
)

forbidden_predictors <- unique(c(use_with_care_features, leakage_cols))
bad_features <- intersect(selected_features, forbidden_predictors)
if (length(bad_features) > 0) {
    stop(
        "Selected features include forbidden columns: ",
        paste(bad_features, collapse = ", "),
        call. = FALSE
    )
}

# --- LightGBM grid -----------------------------------------------------------

lightgbm_grid_specs <- list(
    current_final = list(
        learning_rate = 0.03,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 100,
        feature_fraction = 1.00,
        bagging_fraction = 1.00,
        lambda_l1 = 0,
        lambda_l2 = 0
    ),
    slower_small = list(
        learning_rate = 0.02,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 100,
        feature_fraction = 0.90,
        bagging_fraction = 0.90,
        lambda_l1 = 0,
        lambda_l2 = 0
    ),
    faster_small = list(
        learning_rate = 0.05,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 100,
        feature_fraction = 0.90,
        bagging_fraction = 0.90,
        lambda_l1 = 0,
        lambda_l2 = 0
    ),
    shallower = list(
        learning_rate = 0.03,
        num_leaves = 7,
        max_depth = 3,
        min_data_in_leaf = 100,
        feature_fraction = 0.90,
        bagging_fraction = 0.90,
        lambda_l1 = 0,
        lambda_l2 = 0
    ),
    deeper = list(
        learning_rate = 0.03,
        num_leaves = 31,
        max_depth = 5,
        min_data_in_leaf = 100,
        feature_fraction = 0.90,
        bagging_fraction = 0.90,
        lambda_l1 = 0,
        lambda_l2 = 0
    ),
    more_regular = list(
        learning_rate = 0.03,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 200,
        feature_fraction = 0.80,
        bagging_fraction = 0.80,
        lambda_l1 = 0,
        lambda_l2 = 1
    ),
    less_regular = list(
        learning_rate = 0.03,
        num_leaves = 31,
        max_depth = 5,
        min_data_in_leaf = 50,
        feature_fraction = 0.90,
        bagging_fraction = 0.90,
        lambda_l1 = 0,
        lambda_l2 = 0
    ),
    compact_robust = list(
        learning_rate = 0.02,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 200,
        feature_fraction = 0.80,
        bagging_fraction = 0.80,
        lambda_l1 = 0,
        lambda_l2 = 1
    ),
    l2_regularized = list(
        learning_rate = 0.03,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 100,
        feature_fraction = 0.90,
        bagging_fraction = 0.90,
        lambda_l1 = 0,
        lambda_l2 = 5
    ),
    l1_l2_regularized = list(
        learning_rate = 0.03,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 100,
        feature_fraction = 0.90,
        bagging_fraction = 0.90,
        lambda_l1 = 0.1,
        lambda_l2 = 1
    )
)

lightgbm_grid_tbl <- tibble::tibble(
    config_id = names(lightgbm_grid_specs)
) |>
    dplyr::bind_cols(
        dplyr::bind_rows(lightgbm_grid_specs)
    )

glmnet_alpha_values <- c(0, 0.25, 0.5, 0.75, 1)
multinom_decay_values <- c(0, 1e-5, 1e-4, 1e-3, 1e-2)

# --- load and filter cohort --------------------------------------------------

modeling_path <- file.path(PROCESSED_DIR, "international_modeling_table_with_form.csv")
if (!file.exists(modeling_path)) {
    stop("Missing modeling table: ", modeling_path, call. = FALSE)
}

modeling <- readr::read_csv(modeling_path, show_col_types = FALSE)

filter_counts <- tibble::tibble(step = "raw_modeling_table", n_rows = nrow(modeling))

df <- modeling |>
    dplyr::filter(match_result %in% TARGET_LEVELS)
filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "valid_match_result", n_rows = nrow(df))
)

df <- df |>
    dplyr::filter(
        !is.na(rating_age_days_home),
        !is.na(rating_age_days_away),
        rating_age_days_home <= RATING_FRESHNESS_DAYS,
        rating_age_days_away <= RATING_FRESHNESS_DAYS
    )
filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "rating_freshness_filter", n_rows = nrow(df))
)

df <- df |>
    dplyr::filter(
        !is.na(home_prior_matches),
        !is.na(away_prior_matches),
        home_prior_matches >= MINIMUM_PRIOR_MATCHES,
        away_prior_matches >= MINIMUM_PRIOR_MATCHES
    )
filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "minimum_prior_matches_filter", n_rows = nrow(df))
)

df <- df |>
    dplyr::select(
        source_match_id,
        date,
        data_split,
        match_result,
        dplyr::all_of(selected_features)
    ) |>
    dplyr::mutate(
        date = as.Date(date),
        match_result = factor(match_result, levels = TARGET_LEVELS),
        dplyr::across(
            dplyr::all_of(selected_features),
            ~ if (is.logical(.x)) as.integer(.x) else .x
        )
    )

variant_complete_rows <- df |>
    dplyr::select(dplyr::all_of(selected_features)) |>
    stats::complete.cases() |>
    sum()

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(
        step = paste0("complete_cases_", FEATURE_VARIANT),
        n_rows = variant_complete_rows
    )
)

df <- df |>
    tidyr::drop_na(dplyr::all_of(selected_features))

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "complete_selected_features", n_rows = nrow(df))
)

splits <- make_chronological_splits(df)
train <- splits$train
validation <- splits$validation
test <- splits$test

message("Train rows: ", nrow(train))
message("Validation rows: ", nrow(validation))
message("Test rows: ", nrow(test))

model_formula <- stats::as.formula(
    paste("match_result ~", paste(selected_features, collapse = " + "))
)

x_train_glmnet <- stats::model.matrix(model_formula, data = train)[, -1, drop = FALSE]
x_validation_glmnet <- stats::model.matrix(model_formula, data = validation)[, -1, drop = FALSE]
x_test_glmnet <- stats::model.matrix(model_formula, data = test)[, -1, drop = FALSE]
y_train <- train$match_result

label_map <- c("A" = 0, "D" = 1, "H" = 2)
x_train_lgb <- as.matrix(train[, selected_features])
x_validation_lgb <- as.matrix(validation[, selected_features])
x_test_lgb <- as.matrix(test[, selected_features])

# --- fit models (validation metrics only for selection) ----------------------

validation_results <- tibble::tibble()
classwise_all <- tibble::tibble()
lightgbm_fits <- list()
lightgbm_grid_results <- tibble::tibble()
glmnet_grid_results <- tibble::tibble()
multinom_grid_results <- tibble::tibble()

message("Fitting LightGBM sensitivity grid...")
for (config_id in names(lightgbm_grid_specs)) {
    grid_params <- lightgbm_grid_specs[[config_id]]
    message("  LightGBM: ", config_id)

    lgb_result <- fit_lightgbm_config(
        train = train,
        validation = validation,
        selected_features = selected_features,
        config_id = config_id,
        grid_params = grid_params
    )

    lightgbm_fits[[config_id]] <- lgb_result$fit

    val_metrics <- score_model(
        validation$match_result,
        lgb_result$validation_probs,
        "lightgbm",
        config_id,
        "validation"
    )

    validation_results <- dplyr::bind_rows(validation_results, val_metrics)
    classwise_all <- dplyr::bind_rows(
        classwise_all,
        classwise_metrics(
            validation$match_result,
            lgb_result$validation_probs,
            "lightgbm",
            config_id,
            "validation"
        )
    )

    lightgbm_grid_results <- dplyr::bind_rows(
        lightgbm_grid_results,
        lightgbm_grid_tbl |>
            dplyr::filter(config_id == !!config_id) |>
            dplyr::mutate(
                best_iteration = lgb_result$best_iteration,
                validation_log_loss = val_metrics$log_loss,
                validation_brier_score = val_metrics$brier_score,
                validation_accuracy = val_metrics$accuracy,
                validation_macro_f1 = val_metrics$macro_f1
            )
    )
}

message("Fitting glmnet alpha sensitivity (train CV for lambda)...")
for (alpha_value in glmnet_alpha_values) {
    config_id <- paste0("glmnet_alpha_", alpha_value)

    glmnet_fit <- glmnet::cv.glmnet(
        x = x_train_glmnet,
        y = y_train,
        family = "multinomial",
        type.measure = "deviance",
        alpha = alpha_value,
        nfolds = 5
    )

    glmnet_val_array <- predict(
        glmnet_fit,
        newx = x_validation_glmnet,
        s = "lambda.min",
        type = "response"
    )
    glmnet_val_probs <- as.data.frame(glmnet_val_array[, , 1]) |>
        safe_rename_probability_columns()

    val_metrics <- score_model(
        validation$match_result,
        glmnet_val_probs,
        "glmnet",
        config_id,
        "validation"
    )

    validation_results <- dplyr::bind_rows(validation_results, val_metrics)
    classwise_all <- dplyr::bind_rows(
        classwise_all,
        classwise_metrics(
            validation$match_result,
            glmnet_val_probs,
            "glmnet",
            config_id,
            "validation"
        )
    )

    glmnet_grid_results <- dplyr::bind_rows(
        glmnet_grid_results,
        tibble::tibble(
            config_id = config_id,
            alpha = alpha_value,
            lambda_min = glmnet_fit$lambda.min,
            validation_log_loss = val_metrics$log_loss,
            validation_brier_score = val_metrics$brier_score,
            validation_accuracy = val_metrics$accuracy,
            validation_macro_f1 = val_metrics$macro_f1,
            cv_fit = list(glmnet_fit)
        )
    )
}

message("Fitting multinom decay sensitivity...")
for (decay_value in multinom_decay_values) {
    config_id <- paste0("multinom_decay_", format(decay_value, scientific = TRUE))

    multinom_fit <- nnet::multinom(
        formula = model_formula,
        data = train,
        trace = FALSE,
        MaxNWts = 10000,
        maxit = 500,
        decay = decay_value
    )

    multinom_val_probs <- predict(multinom_fit, newdata = validation, type = "probs") |>
        safe_rename_probability_columns()

    val_metrics <- score_model(
        validation$match_result,
        multinom_val_probs,
        "multinom",
        config_id,
        "validation"
    )

    validation_results <- dplyr::bind_rows(validation_results, val_metrics)
    classwise_all <- dplyr::bind_rows(
        classwise_all,
        classwise_metrics(
            validation$match_result,
            multinom_val_probs,
            "multinom",
            config_id,
            "validation"
        )
    )

    multinom_grid_results <- dplyr::bind_rows(
        multinom_grid_results,
        tibble::tibble(
            config_id = config_id,
            decay = decay_value,
            validation_log_loss = val_metrics$log_loss,
            validation_brier_score = val_metrics$brier_score,
            validation_accuracy = val_metrics$accuracy,
            validation_macro_f1 = val_metrics$macro_f1,
            fit = list(multinom_fit)
        )
    )
}

message("Fitting untuned baselines...")
class_probs_train <- estimate_class_probabilities(train)
majority_probs_train <- make_majority_probs(train)

frequency_val_probs <- constant_probability_probs(validation, class_probs_train)
majority_val_probs <- constant_probability_probs(validation, majority_probs_train)

rating_diff_model <- nnet::multinom(
    match_result ~ rating_diff,
    data = train,
    trace = FALSE,
    MaxNWts = 10000
)
rating_diff_val_probs <- predict(rating_diff_model, newdata = validation, type = "probs")
if (is.vector(rating_diff_val_probs)) {
    rating_diff_val_probs <- matrix(rating_diff_val_probs, nrow = 1)
}
rating_diff_val_probs <- as.data.frame(rating_diff_val_probs)
for (level_name in setdiff(TARGET_LEVELS, names(rating_diff_val_probs))) {
    rating_diff_val_probs[[level_name]] <- 0
}
rating_diff_val_probs <- rating_diff_val_probs[, TARGET_LEVELS, drop = FALSE]
rating_diff_val_probs <- safe_rename_probability_columns(rating_diff_val_probs)

baseline_specs <- list(
    frequency_baseline = frequency_val_probs,
    majority_baseline = majority_val_probs,
    rating_diff_multinom = rating_diff_val_probs
)

for (config_id in names(baseline_specs)) {
    val_probs <- baseline_specs[[config_id]]
    val_metrics <- score_model(
        validation$match_result,
        val_probs,
        "baseline",
        config_id,
        "validation"
    )
    validation_results <- dplyr::bind_rows(validation_results, val_metrics)
    classwise_all <- dplyr::bind_rows(
        classwise_all,
        classwise_metrics(
            validation$match_result,
            val_probs,
            "baseline",
            config_id,
            "validation"
        )
    )
}

# Model 28 official reference (not in selection pool)
model_28_reference <- NULL
model_28_metrics_path <- file.path(REPORTS_TABLES_DIR, "model_28_metrics.csv")
if (file.exists(model_28_metrics_path)) {
    model_28_metrics <- readr::read_csv(model_28_metrics_path, show_col_types = FALSE)
    model_28_reference <- model_28_metrics |>
        dplyr::filter(
            feature_variant == FEATURE_VARIANT,
            model == "lightgbm"
        ) |>
        dplyr::transmute(
            model_family = "lightgbm",
            config_id = "model_28_lightgbm_official",
            feature_variant = FEATURE_VARIANT,
            split = split,
            n = n,
            log_loss = log_loss,
            brier_score = NA_real_,
            accuracy = accuracy,
            macro_f1 = macro_f1,
            evaluation_role = dplyr::if_else(
                split == "test",
                "model_28_reference_comparison",
                "model_28_reference_not_selected"
            )
        )
}

# --- select validation winner ------------------------------------------------

validation_ranking <- validation_results |>
    dplyr::arrange(log_loss)

best_validation_row <- validation_ranking |>
    dplyr::slice_head(n = 1)

best_config <- best_validation_row$config_id[[1]]
best_family <- best_validation_row$model_family[[1]]

best_config_tbl <- validation_ranking |>
    dplyr::filter(config_id == best_config, model_family == best_family) |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
        selection_metric = "validation_log_loss",
        model_28_validation_log_loss = if (
            !is.null(model_28_reference)
        ) {
            model_28_reference |>
                dplyr::filter(split == "validation") |>
                dplyr::pull(log_loss)
        } else {
            NA_real_
        },
        improvement_vs_model_28_validation = model_28_validation_log_loss - log_loss,
        material_improvement_vs_model_28 = improvement_vs_model_28_validation >=
            MATERIALITY_THRESHOLD
    )

current_final_validation <- validation_results |>
    dplyr::filter(config_id == "current_final", model_family == "lightgbm")

improvement_vs_current_final <- current_final_validation$log_loss -
    best_validation_row$log_loss

# --- single test evaluation for winner ---------------------------------------

message("Evaluating test set once for validation winner: ", best_config)

test_results <- tibble::tibble()
calibration_all <- tibble::tibble()
winner_fit_object <- NULL

if (best_family == "lightgbm") {
    winner_fit <- lightgbm_fits[[best_config]]
    winner_fit_object <- winner_fit
    test_probs <- predict_lightgbm_matrix(winner_fit, x_test_lgb)
} else if (best_family == "glmnet") {
    winner_row <- glmnet_grid_results |>
        dplyr::filter(config_id == best_config)
    winner_fit <- winner_row$cv_fit[[1]]
    winner_fit_object <- winner_fit
    glmnet_test_array <- predict(
        winner_fit,
        newx = x_test_glmnet,
        s = "lambda.min",
        type = "response"
    )
    test_probs <- as.data.frame(glmnet_test_array[, , 1]) |>
        safe_rename_probability_columns()
} else if (best_family == "multinom") {
    winner_row <- multinom_grid_results |>
        dplyr::filter(config_id == best_config)
    winner_fit <- winner_row$fit[[1]]
    winner_fit_object <- winner_fit
    test_probs <- predict(winner_fit, newdata = test, type = "probs") |>
        safe_rename_probability_columns()
} else if (best_family == "baseline") {
    if (best_config == "frequency_baseline") {
        test_probs <- constant_probability_probs(test, class_probs_train)
    } else if (best_config == "majority_baseline") {
        test_probs <- constant_probability_probs(test, majority_probs_train)
    } else {
        test_probs <- predict(rating_diff_model, newdata = test, type = "probs")
        if (is.vector(test_probs)) {
            test_probs <- matrix(test_probs, nrow = 1)
        }
        test_probs <- as.data.frame(test_probs)
        for (level_name in setdiff(TARGET_LEVELS, names(test_probs))) {
            test_probs[[level_name]] <- 0
        }
        test_probs <- test_probs[, TARGET_LEVELS, drop = FALSE]
        test_probs <- safe_rename_probability_columns(test_probs)
    }
    winner_fit_object <- list(config_id = best_config)
} else {
    stop("Unknown winning model family: ", best_family, call. = FALSE)
}

test_winner_metrics <- score_model(
    test$match_result,
    test_probs,
    best_family,
    best_config,
    "test"
) |>
    dplyr::mutate(evaluation_role = "validation_selected_winner")

test_results <- dplyr::bind_rows(test_results, test_winner_metrics)
classwise_all <- dplyr::bind_rows(
    classwise_all,
    classwise_metrics(
        test$match_result,
        test_probs,
        best_family,
        best_config,
        "test"
    )
)
calibration_all <- dplyr::bind_rows(
    calibration_all,
    make_calibration_bins(
        test$match_result,
        test_probs,
        best_family,
        best_config,
        "test"
    )
)

# Comparison: current_final on test (if not winner)
if (!(best_family == "lightgbm" && best_config == "current_final")) {
    current_final_test_probs <- predict_lightgbm_matrix(
        lightgbm_fits[["current_final"]],
        x_test_lgb
    )
    test_current_final <- score_model(
        test$match_result,
        current_final_test_probs,
        "lightgbm",
        "current_final",
        "test"
    ) |>
        dplyr::mutate(evaluation_role = "current_final_grid_comparison")

    test_results <- dplyr::bind_rows(test_results, test_current_final)
    classwise_all <- dplyr::bind_rows(
        classwise_all,
        classwise_metrics(
            test$match_result,
            current_final_test_probs,
            "lightgbm",
            "current_final",
            "test"
        )
    )
}

# Model 28 official test reference from saved artifact
if (!is.null(model_28_reference)) {
    model_28_test_ref <- model_28_reference |>
        dplyr::filter(split == "test") |>
        dplyr::mutate(evaluation_role = "model_28_official_artifact")
    test_results <- dplyr::bind_rows(test_results, model_28_test_ref)
}

# --- save artifacts ----------------------------------------------------------

readr::write_csv(filter_counts, file.path(REPORTS_TABLES_DIR, "model_33_filter_counts.csv"))
readr::write_csv(
    validation_results,
    file.path(REPORTS_TABLES_DIR, "model_33_hyperparameter_validation_results.csv")
)
readr::write_csv(test_results, file.path(REPORTS_TABLES_DIR, "model_33_hyperparameter_test_results.csv"))
readr::write_csv(best_config_tbl, file.path(REPORTS_TABLES_DIR, "model_33_best_config.csv"))
readr::write_csv(classwise_all, file.path(REPORTS_TABLES_DIR, "model_33_classwise_metrics.csv"))
readr::write_csv(calibration_all, file.path(REPORTS_TABLES_DIR, "model_33_calibration_bins.csv"))
readr::write_csv(lightgbm_grid_results, file.path(REPORTS_TABLES_DIR, "model_33_lightgbm_grid.csv"))
readr::write_csv(
    glmnet_grid_results |> dplyr::select(-cv_fit),
    file.path(REPORTS_TABLES_DIR, "model_33_glmnet_grid.csv")
)
readr::write_csv(
    multinom_grid_results |> dplyr::select(-fit),
    file.path(REPORTS_TABLES_DIR, "model_33_multinom_grid.csv")
)

saveRDS(selected_features, file.path(MODELS_DIR, "model_33_feature_names.rds"))

materially_better <- isTRUE(best_config_tbl$material_improvement_vs_model_28[[1]])
if (materially_better) {
    saveRDS(
        list(
            model_family = best_family,
            config_id = best_config,
            feature_variant = FEATURE_VARIANT,
            selected_features = selected_features,
            fit = winner_fit_object
        ),
        file.path(MODELS_DIR, "model_33_tuned_best_model.rds")
    )
}

# --- figures -----------------------------------------------------------------

validation_log_loss_plot <- validation_results |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(config_id, log_loss), y = log_loss, fill = model_family)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
        title = "Model 33 validation log loss by configuration",
        x = "Configuration",
        y = "Multiclass log loss",
        fill = "Model family"
    )

ggplot2::ggsave(
    file.path(REPORTS_FIGURES_DIR, "model_33_validation_log_loss_by_config.png"),
    validation_log_loss_plot,
    width = 11,
    height = 8,
    dpi = 300
)

top_n <- 10
validation_top_plot <- validation_results |>
    dplyr::slice_min(order_by = log_loss, n = top_n, with_ties = FALSE) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(config_id, -log_loss), y = log_loss, fill = model_family)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
        title = paste0("Model 33 top ", top_n, " validation configurations"),
        x = "Configuration",
        y = "Multiclass log loss",
        fill = "Model family"
    )

ggplot2::ggsave(
    file.path(REPORTS_FIGURES_DIR, "model_33_validation_log_loss_top_configs.png"),
    validation_top_plot,
    width = 10,
    height = 6,
    dpi = 300
)

family_summary <- validation_results |>
    dplyr::group_by(model_family) |>
    dplyr::slice_min(order_by = log_loss, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

family_comparison_plot <- family_summary |>
    ggplot2::ggplot(ggplot2::aes(x = model_family, y = log_loss, fill = model_family)) +
    ggplot2::geom_col() +
    ggplot2::labs(
        title = "Model 33 best validation log loss by model family",
        x = "Model family",
        y = "Best validation log loss",
        fill = "Model family"
    )

ggplot2::ggsave(
    file.path(REPORTS_FIGURES_DIR, "model_33_model_family_comparison.png"),
    family_comparison_plot,
    width = 8,
    height = 5,
    dpi = 300
)

calibration_plot <- calibration_all |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = mean_predicted_probability,
            y = observed_frequency,
            color = class
        )
    ) +
    ggplot2::geom_point(ggplot2::aes(size = n)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::facet_wrap(~class) +
    ggplot2::labs(
        title = "Model 33 best validation winner — test calibration by class",
        subtitle = paste0(best_family, " / ", best_config),
        x = "Mean predicted probability (bin)",
        y = "Observed frequency",
        color = "Class"
    )

ggplot2::ggsave(
    file.path(REPORTS_FIGURES_DIR, "model_33_best_model_calibration.png"),
    calibration_plot,
    width = 10,
    height = 6,
    dpi = 300
)

confusion_tbl <- classwise_all |>
    dplyr::filter(
        split == "test",
        config_id == best_config,
        model_family == best_family
    )

pred_counts <- test |>
    dplyr::mutate(
        truth = as.character(match_result),
        prediction = TARGET_LEVELS[max.col(test_probs[, c("pred_H", "pred_D", "pred_A")])]
    ) |>
    dplyr::count(truth, prediction, name = "n") |>
    tidyr::complete(
        truth = TARGET_LEVELS,
        prediction = TARGET_LEVELS,
        fill = list(n = 0)
    )

confusion_heatmap <- pred_counts |>
    ggplot2::ggplot(ggplot2::aes(x = prediction, y = truth, fill = n)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = n), color = "white") +
    ggplot2::labs(
        title = "Model 33 best winner — test confusion matrix",
        subtitle = paste0(best_family, " / ", best_config),
        x = "Predicted",
        y = "Actual",
        fill = "Count"
    )

ggplot2::ggsave(
    file.path(REPORTS_FIGURES_DIR, "model_33_best_model_confusion_heatmap.png"),
    confusion_heatmap,
    width = 7,
    height = 6,
    dpi = 300
)

if (best_family == "lightgbm") {
    importance_tbl <- lightgbm::lgb.importance(
        lightgbm_fits[[best_config]],
        percentage = TRUE
    )
    importance_plot <- importance_tbl |>
        dplyr::slice_head(n = 20) |>
        ggplot2::ggplot(ggplot2::aes(x = reorder(Feature, Gain), y = Gain)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = "Model 33 LightGBM feature importance (validation winner)",
            x = "Feature",
            y = "Gain (%)"
        )

    ggplot2::ggsave(
        file.path(REPORTS_FIGURES_DIR, "model_33_lightgbm_feature_importance.png"),
        importance_plot,
        width = 9,
        height = 7,
        dpi = 300
    )
}

# --- summary markdown --------------------------------------------------------

model_28_val_ll <- best_config_tbl$model_28_validation_log_loss[[1]]
delta_vs_m28 <- best_config_tbl$improvement_vs_model_28_validation[[1]]
delta_vs_current <- improvement_vs_current_final[[1]]

recommendation <- if (materially_better && best_family == "lightgbm") {
    "Consider updating the production narrative to the tuned Model 33 LightGBM winner; improvement exceeds the materiality threshold versus Model 28 validation log loss."
} else if (delta_vs_m28 >= MODEST_IMPROVEMENT_THRESHOLD && delta_vs_m28 < MATERIALITY_THRESHOLD) {
    "Keep Model 28 as the official final model; validation gains are modest (below 0.005) and do not justify changing the project narrative."
} else {
    "Keep Model 28 (`lightgbm` + `safe_plus_form_compact`) as the official final model; hyperparameter sensitivity did not materially beat the existing selection."
}

summary_lines <- c(
    "# Model 33 hyperparameter sensitivity summary",
    "",
    paste0("**Feature variant:** `", FEATURE_VARIANT, "` (", length(selected_features), " features)"),
    paste0("**Cohort after filters:** ", nrow(df), " matches (validation ", nrow(validation), ", test ", nrow(test), ")"),
    "",
    "## Validation winner",
    "",
    paste0(
        "- **Best configuration:** `",
        best_config,
        "` (",
        best_family,
        ")"
    ),
    paste0("- **Validation log loss:** ", round(best_validation_row$log_loss[[1]], 6)),
    paste0("- **Validation Brier:** ", round(best_validation_row$brier_score[[1]], 6)),
    paste0("- **Validation accuracy:** ", round(best_validation_row$accuracy[[1]], 4)),
    paste0("- **Validation macro F1:** ", round(best_validation_row$macro_f1[[1]], 4)),
    "",
    "## Comparison to Model 28 / current_final grid",
    "",
    paste0("- **Model 28 official validation log loss (artifact):** ", round(model_28_val_ll, 6)),
    paste0("- **Delta vs Model 28 validation:** ", round(delta_vs_m28, 6), " (positive = improvement)"),
    paste0("- **current_final grid validation log loss:** ", round(current_final_validation$log_loss[[1]], 6)),
    paste0("- **Delta vs current_final grid:** ", round(delta_vs_current, 6)),
    paste0(
        "- **Material improvement threshold:** ",
        MATERIALITY_THRESHOLD,
        " log-loss units on validation; modest band ",
        MODEST_IMPROVEMENT_THRESHOLD,
        "–",
        MATERIALITY_THRESHOLD,
        "."
    ),
    "",
    "## Test evaluation (single pass for winner)",
    "",
    paste0("- **Winner test log loss:** ", round(test_winner_metrics$log_loss[[1]], 6)),
    paste0("- **Winner test Brier:** ", round(test_winner_metrics$brier_score[[1]], 6)),
    paste0("- **Winner test accuracy:** ", round(test_winner_metrics$accuracy[[1]], 4)),
    paste0("- **Winner test macro F1:** ", round(test_winner_metrics$macro_f1[[1]], 4)),
    "",
    "## Recommendation",
    "",
    recommendation,
    "",
    "## Notes",
    "",
    "- Selection used **validation multiclass log loss only**; test was evaluated once for the winner.",
    "- LightGBM runs set reproducibility seeds (`seed`, `feature_fraction_seed`, `bagging_seed`, `data_random_seed`, `deterministic`, `force_col_wise`).",
    "- `current_final` in this script follows the Model 33 grid spec (e.g. `feature_fraction = 1.0`, `lambda_l2 = 0`); Model 28 code used `feature_fraction = 0.9`, `bagging_fraction = 0.9`, `lambda_l2 = 1`. Official Model 28 test metrics are retained from `model_28_metrics.csv` when available.",
    "- Baselines (`frequency_baseline`, `majority_baseline`, `rating_diff_multinom`) were included for comparison and not tuned.",
    ""
)

writeLines(summary_lines, file.path(REPORTS_DIR, "final", "model_33_hyperparameter_sensitivity_summary.md"))

# --- console report ----------------------------------------------------------

cat("\n=== Model 33 filter counts ===\n")
print(filter_counts)

cat("\n=== Top 10 validation configurations (log loss) ===\n")
print(validation_ranking |> dplyr::slice_head(n = 10))

cat("\n=== Best validation configuration ===\n")
print(best_config_tbl)

cat("\n=== Test metrics (selected winner) ===\n")
print(test_winner_metrics)

saved_tables <- c(
    "reports/tables/model_33_hyperparameter_validation_results.csv",
    "reports/tables/model_33_hyperparameter_test_results.csv",
    "reports/tables/model_33_best_config.csv",
    "reports/tables/model_33_filter_counts.csv",
    "reports/tables/model_33_classwise_metrics.csv",
    "reports/tables/model_33_calibration_bins.csv",
    "reports/tables/model_33_lightgbm_grid.csv",
    "reports/tables/model_33_glmnet_grid.csv",
    "reports/tables/model_33_multinom_grid.csv"
)

saved_figures <- c(
    "reports/figures/model_33_validation_log_loss_by_config.png",
    "reports/figures/model_33_validation_log_loss_top_configs.png",
    "reports/figures/model_33_model_family_comparison.png",
    "reports/figures/model_33_best_model_calibration.png",
    "reports/figures/model_33_best_model_confusion_heatmap.png"
)

if (best_family == "lightgbm") {
    saved_figures <- c(saved_figures, "reports/figures/model_33_lightgbm_feature_importance.png")
}

cat("\n=== Saved tables ===\n")
cat(paste0(" - ", saved_tables, collapse = "\n"), "\n")

cat("\n=== Saved figures ===\n")
cat(paste0(" - ", saved_figures, collapse = "\n"), "\n")

cat("\n=== Summary ===\n")
cat(file.path(REPORTS_DIR, "final", "model_33_hyperparameter_sensitivity_summary.md"), "\n")

message("Model 33 hyperparameter sensitivity complete.")
