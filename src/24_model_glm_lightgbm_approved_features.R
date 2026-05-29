# 24_model_glm_lightgbm_approved_features.R
#
# Model 24: multiclass H/D/A models on the approved safe pre-match feature set
# (multinomial logit, glmnet ridge, LightGBM when installed). Same chronological
# splits and complete-case cohort as later modeling stages.
#
# Reads:
# - data/processed/international_modeling_table.csv
# - reports/tables/approved_feature_sets_final.R
#
# Writes: reports/tables/model_24_* and reports/figures/model_24_*
#
# Notes:
# - Post-match fields and market odds stay out of the safe feature list.

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

# 1. Package checks

required_pkgs <- c(
    "readr",
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "ggplot2",
    "nnet"
)

missing_required <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

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
    message("Package glmnet is not installed. Regularized multinomial GLM will be skipped.")
}

if (!has_lightgbm) {
    message("Package lightgbm is not installed. LightGBM model will be skipped.")
}

# 2. Helpers

clip_probs <- function(p, eps = 1e-15) {
    pmax(pmin(p, 1 - eps), eps)
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
    class_index <- match(truth, c("H", "D", "A"))

    if (any(is.na(class_index))) {
        stop("Truth contains values outside H, D, A.", call. = FALSE)
    }

    -mean(log(probs[cbind(seq_along(class_index), class_index)]))
}

accuracy_score <- function(truth, probs) {
    truth <- as.character(truth)
    pred <- c("H", "D", "A")[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    mean(pred == truth)
}

classwise_metrics <- function(truth, probs, model_name, split_name) {
    truth <- as.character(truth)
    pred <- c("H", "D", "A")[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    purrr::map_dfr(c("H", "D", "A"), function(cls) {
        tp <- sum(pred == cls & truth == cls)
        fp <- sum(pred == cls & truth != cls)
        fn <- sum(pred != cls & truth == cls)
        tn <- sum(pred != cls & truth != cls)

        precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
        recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
        f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

        tibble::tibble(
            model = model_name,
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
    classwise_metrics(truth, probs, model_name = "tmp", split_name = "tmp") |>
        dplyr::summarise(macro_f1 = mean(f1), .groups = "drop") |>
        dplyr::pull(macro_f1)
}

confusion_matrix_tbl <- function(truth, probs, model_name, split_name) {
    truth <- as.character(truth)
    pred <- c("H", "D", "A")[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    tibble::tibble(
        model = model_name,
        split = split_name,
        truth = factor(truth, levels = c("H", "D", "A")),
        prediction = factor(pred, levels = c("H", "D", "A"))
    ) |>
        dplyr::count(model, split, truth, prediction, name = "n") |>
        tidyr::complete(
            model,
            split,
            truth = factor(c("H", "D", "A"), levels = c("H", "D", "A")),
            prediction = factor(c("H", "D", "A"), levels = c("H", "D", "A")),
            fill = list(n = 0)
        )
}

score_model <- function(truth, probs, model_name, split_name) {
    validate_probability_table(probs, model_name)

    tibble::tibble(
        model = model_name,
        split = split_name,
        n = length(truth),
        log_loss = multiclass_log_loss(truth, probs),
        accuracy = accuracy_score(truth, probs),
        macro_f1 = macro_f1_score(truth, probs)
    )
}

make_prediction_tbl <- function(base_df, probs, model_name, split_name) {
    pred_class <- c("H", "D", "A")[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    tibble::tibble(
        model = model_name,
        split = split_name,
        source_match_id = base_df$source_match_id,
        date = base_df$date,
        match_result = as.character(base_df$match_result),
        pred_class = pred_class,
        pred_H = probs$pred_H,
        pred_D = probs$pred_D,
        pred_A = probs$pred_A
    )
}

safe_rename_probability_columns <- function(probs) {
    probs <- as.data.frame(probs)

    if (all(c("pred_H", "pred_D", "pred_A") %in% names(probs))) {
        return(probs[, c("pred_H", "pred_D", "pred_A"), drop = FALSE])
    }

    missing_cols <- setdiff(c("H", "D", "A"), names(probs))

    if (length(missing_cols) > 0) {
        stop(
            "Predicted probability table does not contain expected class columns: ",
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
    # lightgbm::predict() returns an n x 3 matrix (classes 0=A, 1=D, 2=H) on current versions.
    if (is.matrix(raw_pred)) {
        prob_matrix <- raw_pred
    } else {
        prob_matrix <- matrix(raw_pred, ncol = 3, byrow = TRUE)
    }

    if (nrow(prob_matrix) != n_rows) {
        stop(
            "LightGBM prediction row count (",
            nrow(prob_matrix),
            ") does not match expected rows (",
            n_rows,
            ").",
            call. = FALSE
        )
    }

    colnames(prob_matrix) <- c("pred_A", "pred_D", "pred_H")

    as.data.frame(prob_matrix) |>
        dplyr::select(pred_H, pred_D, pred_A)
}

# 3. Load data

modeling_path <- "data/processed/international_modeling_table.csv"

if (!file.exists(modeling_path)) {
    stop("Missing modeling table: ", modeling_path, call. = FALSE)
}

modeling <- readr::read_csv(modeling_path, show_col_types = FALSE)

message("Loaded modeling table rows: ", nrow(modeling))
message("Loaded modeling table columns: ", ncol(modeling))

# 4. Feature set (first-pass safe pre-match features only)
# Pins the safe 11-feature set; approved_feature_sets_final.R also lists
# use-with-care categoricals that this script does not load.

selected_features <- c(
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

missing_selected_features <- setdiff(selected_features, names(modeling))

if (length(missing_selected_features) > 0) {
    stop(
        "Modeling table is missing selected feature columns: ",
        paste(missing_selected_features, collapse = ", "),
        call. = FALSE
    )
}

use_with_care_features <- c(
    "home_team_clean",
    "away_team_clean",
    "city",
    "competition",
    "country",
    "season",
    "tournament"
)

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

forbidden_predictors <- c(use_with_care_features, leakage_cols)

outcome_like_cols <- setdiff(
    names(modeling)[grepl("result|outcome|score|goal", names(modeling), ignore.case = TRUE)],
    "match_result"
)

forbidden_predictors <- unique(c(forbidden_predictors, outcome_like_cols))

bad_features <- intersect(selected_features, forbidden_predictors)

if (length(bad_features) > 0) {
    stop(
        "Selected features include leakage or use-with-care columns: ",
        paste(bad_features, collapse = ", "),
        call. = FALSE
    )
}

message("Selected features (safe pre-match set):")
print(selected_features)

required_cols <- c("source_match_id", "date", "data_split", "match_result", selected_features)
missing_cols <- setdiff(required_cols, names(modeling))

if (length(missing_cols) > 0) {
    stop(
        "Modeling table is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
    )
}

# 5. Validation and filtering

if (anyDuplicated(modeling$source_match_id) > 0) {
    duplicate_examples <- modeling |>
        dplyr::count(source_match_id, name = "n") |>
        dplyr::filter(n > 1) |>
        dplyr::slice_head(n = 10)

    print(duplicate_examples)

    stop("source_match_id is duplicated. Modeling must be one row per match.", call. = FALSE)
}

invalid_results <- setdiff(unique(stats::na.omit(modeling$match_result)), c("H", "D", "A"))

if (length(invalid_results) > 0) {
    stop(
        "match_result contains values outside H, D, A: ",
        paste(invalid_results, collapse = ", "),
        call. = FALSE
    )
}

rating_freshness_days <- 365

filter_counts <- tibble::tibble(
    step = character(),
    n_rows = integer()
)

df <- modeling

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "raw_modeling_table", n_rows = nrow(df))
)

df <- df |>
    dplyr::filter(match_result %in% c("H", "D", "A"))

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "valid_match_result", n_rows = nrow(df))
)

df <- df |>
    dplyr::filter(
        !is.na(rating_age_days_home),
        !is.na(rating_age_days_away),
        rating_age_days_home <= rating_freshness_days,
        rating_age_days_away <= rating_freshness_days
    )

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "rating_freshness_filter", n_rows = nrow(df))
)

df <- df |>
    dplyr::select(
        source_match_id,
        date,
        data_split,
        match_result,
        dplyr::all_of(selected_features)
    ) |>
    tidyr::drop_na()

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "complete_model_features", n_rows = nrow(df))
)

readr::write_csv(filter_counts, "reports/tables/model_24_filter_counts.csv")
print(filter_counts)

df <- df |>
    dplyr::mutate(
        date = as.Date(date),
        match_result = factor(match_result, levels = c("H", "D", "A")),
        dplyr::across(dplyr::all_of(selected_features), ~ {
            if (is.logical(.x)) {
                as.integer(.x)
            } else {
                .x
            }
        })
    )

if (!all(df$data_split %in% c("train", "test"))) {
    message("data_split contains values other than train/test:")
    print(table(df$data_split, useNA = "ifany"))
}

train_all <- df |>
    dplyr::filter(data_split == "train") |>
    dplyr::arrange(date)

test <- df |>
    dplyr::filter(data_split == "test") |>
    dplyr::arrange(date)

if (nrow(train_all) == 0) {
    stop("No training rows after filtering.", call. = FALSE)
}

if (nrow(test) == 0) {
    stop("No test rows after filtering.", call. = FALSE)
}

# Chronological validation split inside training data:
# final 20% of training rows become validation.
validation_fraction <- 0.20
validation_start_index <- floor(nrow(train_all) * (1 - validation_fraction)) + 1

train <- train_all[seq_len(validation_start_index - 1), ]
validation <- train_all[validation_start_index:nrow(train_all), ]

message("Train rows: ", nrow(train))
message("Validation rows: ", nrow(validation))
message("Test rows: ", nrow(test))

split_summary <- tibble::tibble(
    split = c("train", "validation", "test"),
    n_rows = c(nrow(train), nrow(validation), nrow(test)),
    min_date = c(min(train$date), min(validation$date), min(test$date)),
    max_date = c(max(train$date), max(validation$date), max(test$date))
)

readr::write_csv(split_summary, "reports/tables/model_24_split_summary.csv")
print(split_summary)

# 6. Shared formula and design matrices

model_formula <- stats::as.formula(
    paste("match_result ~", paste(selected_features, collapse = " + "))
)

# 7. Model 1: multinomial logistic regression

message("Fitting multinomial logistic regression...")

multinom_fit <- nnet::multinom(
    formula = model_formula,
    data = train,
    trace = FALSE,
    MaxNWts = 10000
)

multinom_val_probs <- predict(multinom_fit, newdata = validation, type = "probs") |>
    safe_rename_probability_columns()

multinom_test_probs <- predict(multinom_fit, newdata = test, type = "probs") |>
    safe_rename_probability_columns()

# 8. Model 2: regularized multinomial GLM via glmnet

glmnet_val_probs <- NULL
glmnet_test_probs <- NULL
glmnet_fit <- NULL

if (has_glmnet) {
    message("Fitting regularized multinomial GLM via glmnet...")

    x_train <- stats::model.matrix(model_formula, data = train)[, -1, drop = FALSE]
    y_train <- train$match_result

    x_validation <- stats::model.matrix(model_formula, data = validation)[, -1, drop = FALSE]
    x_test <- stats::model.matrix(model_formula, data = test)[, -1, drop = FALSE]

    glmnet_fit <- glmnet::cv.glmnet(
        x = x_train,
        y = y_train,
        family = "multinomial",
        type.measure = "deviance",
        alpha = 0,
        nfolds = 5
    )

    glmnet_val_array <- predict(
        glmnet_fit,
        newx = x_validation,
        s = "lambda.min",
        type = "response"
    )

    glmnet_test_array <- predict(
        glmnet_fit,
        newx = x_test,
        s = "lambda.min",
        type = "response"
    )

    glmnet_val_probs <- as.data.frame(glmnet_val_array[, , 1]) |>
        safe_rename_probability_columns()

    glmnet_test_probs <- as.data.frame(glmnet_test_array[, , 1]) |>
        safe_rename_probability_columns()
}

# 9. Model 3: LightGBM multiclass

lgb_val_probs <- NULL
lgb_test_probs <- NULL
lgb_fit <- NULL

if (has_lightgbm) {
    message("Fitting LightGBM multiclass classifier...")

    # LightGBM multiclass label encoding (must match probability column order below):
    # A = 0, D = 1, H = 2  =>  raw predict() columns map to pred_A, pred_D, pred_H
    label_map <- c("A" = 0, "D" = 1, "H" = 2)

    train_lgb <- train |>
        dplyr::mutate(label = unname(label_map[as.character(match_result)]))

    validation_lgb <- validation |>
        dplyr::mutate(label = unname(label_map[as.character(match_result)]))

    test_lgb <- test |>
        dplyr::mutate(label = unname(label_map[as.character(match_result)]))

    x_train_lgb <- as.matrix(train_lgb[, selected_features])
    x_validation_lgb <- as.matrix(validation_lgb[, selected_features])
    x_test_lgb <- as.matrix(test_lgb[, selected_features])

    dtrain <- lightgbm::lgb.Dataset(
        data = x_train_lgb,
        label = train_lgb$label
    )

    dvalidation <- lightgbm::lgb.Dataset(
        data = x_validation_lgb,
        label = validation_lgb$label
    )

    lgb_params <- list(
        objective = "multiclass",
        metric = "multi_logloss",
        num_class = 3,
        learning_rate = 0.03,
        num_leaves = 15,
        max_depth = 4,
        min_data_in_leaf = 100,
        feature_fraction = 0.9,
        bagging_fraction = 0.9,
        bagging_freq = 1,
        lambda_l2 = 1,
        verbosity = -1
    )

    lgb_fit <- lightgbm::lgb.train(
        params = lgb_params,
        data = dtrain,
        nrounds = 1000,
        valids = list(validation = dvalidation),
        early_stopping_rounds = 50,
        verbose = 1
    )

    raw_val_pred <- predict(lgb_fit, x_validation_lgb)
    raw_test_pred <- predict(lgb_fit, x_test_lgb)

    lgb_val_probs <- lightgbm_raw_probs_to_pred_columns(raw_val_pred, nrow(validation))
    lgb_test_probs <- lightgbm_raw_probs_to_pred_columns(raw_test_pred, nrow(test))
}

# 10. Metrics

metrics <- dplyr::bind_rows(
    score_model(validation$match_result, multinom_val_probs, "multinom", "validation"),
    score_model(test$match_result, multinom_test_probs, "multinom", "test")
)

classwise <- dplyr::bind_rows(
    classwise_metrics(validation$match_result, multinom_val_probs, "multinom", "validation"),
    classwise_metrics(test$match_result, multinom_test_probs, "multinom", "test")
)

confusions <- dplyr::bind_rows(
    confusion_matrix_tbl(validation$match_result, multinom_val_probs, "multinom", "validation"),
    confusion_matrix_tbl(test$match_result, multinom_test_probs, "multinom", "test")
)

predictions <- dplyr::bind_rows(
    make_prediction_tbl(validation, multinom_val_probs, "multinom", "validation"),
    make_prediction_tbl(test, multinom_test_probs, "multinom", "test")
)

if (!is.null(glmnet_val_probs)) {
    metrics <- dplyr::bind_rows(
        metrics,
        score_model(validation$match_result, glmnet_val_probs, "glmnet_multinomial_ridge", "validation"),
        score_model(test$match_result, glmnet_test_probs, "glmnet_multinomial_ridge", "test")
    )

    classwise <- dplyr::bind_rows(
        classwise,
        classwise_metrics(validation$match_result, glmnet_val_probs, "glmnet_multinomial_ridge", "validation"),
        classwise_metrics(test$match_result, glmnet_test_probs, "glmnet_multinomial_ridge", "test")
    )

    confusions <- dplyr::bind_rows(
        confusions,
        confusion_matrix_tbl(validation$match_result, glmnet_val_probs, "glmnet_multinomial_ridge", "validation"),
        confusion_matrix_tbl(test$match_result, glmnet_test_probs, "glmnet_multinomial_ridge", "test")
    )

    predictions <- dplyr::bind_rows(
        predictions,
        make_prediction_tbl(validation, glmnet_val_probs, "glmnet_multinomial_ridge", "validation"),
        make_prediction_tbl(test, glmnet_test_probs, "glmnet_multinomial_ridge", "test")
    )
}

if (!is.null(lgb_val_probs)) {
    metrics <- dplyr::bind_rows(
        metrics,
        score_model(validation$match_result, lgb_val_probs, "lightgbm", "validation"),
        score_model(test$match_result, lgb_test_probs, "lightgbm", "test")
    )

    classwise <- dplyr::bind_rows(
        classwise,
        classwise_metrics(validation$match_result, lgb_val_probs, "lightgbm", "validation"),
        classwise_metrics(test$match_result, lgb_test_probs, "lightgbm", "test")
    )

    confusions <- dplyr::bind_rows(
        confusions,
        confusion_matrix_tbl(validation$match_result, lgb_val_probs, "lightgbm", "validation"),
        confusion_matrix_tbl(test$match_result, lgb_test_probs, "lightgbm", "test")
    )

    predictions <- dplyr::bind_rows(
        predictions,
        make_prediction_tbl(validation, lgb_val_probs, "lightgbm", "validation"),
        make_prediction_tbl(test, lgb_test_probs, "lightgbm", "test")
    )
}

metrics <- metrics |>
    dplyr::arrange(split, log_loss)

readr::write_csv(metrics, "reports/tables/model_24_metrics.csv")
readr::write_csv(classwise, "reports/tables/model_24_classwise_metrics.csv")
readr::write_csv(confusions, "reports/tables/model_24_confusion_matrices.csv")
readr::write_csv(predictions, "reports/tables/model_24_predictions.csv")

print(metrics)

# 11. Feature importance

feature_importance <- tibble::tibble()

if (!is.null(lgb_fit)) {
    lgb_importance <- lightgbm::lgb.importance(
        model = lgb_fit,
        percentage = TRUE
    )

    feature_importance <- lgb_importance |>
        tibble::as_tibble() |>
        dplyr::mutate(model = "lightgbm") |>
        dplyr::relocate(model)

    readr::write_csv(
        feature_importance,
        "reports/tables/model_24_feature_importance.csv"
    )
} else {
    readr::write_csv(
        feature_importance,
        "reports/tables/model_24_feature_importance.csv"
    )
}

# 12. Figures

log_loss_plot <- metrics |>
    dplyr::filter(split %in% c("validation", "test")) |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = reorder(model, log_loss),
            y = log_loss
        )
    ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ split, scales = "free_y") +
    ggplot2::labs(
        title = "Model 24 log loss comparison",
        x = "Model",
        y = "Multiclass log loss"
    )

ggplot2::ggsave(
    filename = "reports/figures/model_24_log_loss_comparison.png",
    plot = log_loss_plot,
    width = 9,
    height = 5,
    dpi = 300
)

calibration_data <- predictions |>
    dplyr::filter(split == "test") |>
    tidyr::pivot_longer(
        cols = c(pred_H, pred_D, pred_A),
        names_to = "predicted_class",
        values_to = "pred_prob"
    ) |>
    dplyr::mutate(
        predicted_class = dplyr::case_when(
            predicted_class == "pred_H" ~ "H",
            predicted_class == "pred_D" ~ "D",
            predicted_class == "pred_A" ~ "A",
            TRUE ~ predicted_class
        ),
        actual_binary = as.integer(match_result == predicted_class),
        prob_bin = dplyr::ntile(pred_prob, 10)
    ) |>
    dplyr::group_by(model, predicted_class, prob_bin) |>
    dplyr::summarise(
        mean_pred_prob = mean(pred_prob),
        observed_rate = mean(actual_binary),
        n = dplyr::n(),
        .groups = "drop"
    )

calibration_plot <- calibration_data |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = mean_pred_prob,
            y = observed_rate
        )
    ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(predicted_class ~ model) +
    ggplot2::labs(
        title = "Model 24 test-set calibration by class",
        x = "Mean predicted probability",
        y = "Observed rate"
    ) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1))

ggplot2::ggsave(
    filename = "reports/figures/model_24_calibration_plot.png",
    plot = calibration_plot,
    width = 10,
    height = 7,
    dpi = 300
)

# 13. Final selection summary

validation_ranking <- metrics |>
    dplyr::filter(split == "validation") |>
    dplyr::arrange(log_loss)

selected_model <- validation_ranking$model[[1]]

final_test_metrics <- metrics |>
    dplyr::filter(split == "test", model == selected_model)

message("=================================================================")
message("Model 24 complete.")
message("Best validation model by log loss: ", selected_model)
message("Final test metrics for selected model:")
print(final_test_metrics)
message("Outputs written to reports/tables and reports/figures.")
message("=================================================================")