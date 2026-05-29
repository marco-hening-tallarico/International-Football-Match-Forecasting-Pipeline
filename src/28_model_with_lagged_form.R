# 28_model_with_lagged_form.R
#
# Model 28: tests whether lagged form features beat the safe Elo baseline from
# Models 24/26 on the same complete-case cohort and chronological splits.
#
# Reads: data/processed/international_modeling_table_with_form.csv
#
# Writes: reports/tables/model_28_* and reports/figures/model_28_*
#
# Notes:
# - Variants: baseline_safe, safe_plus_form_compact, safe_plus_form_draw.

set.seed(2026)

# 0. Project setup

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

dir.create("reports/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)

LOG_LOSS_BEAT_THRESHOLD_SMALL <- 0.002
LOG_LOSS_BEAT_THRESHOLD_LARGE <- 0.005
LOG_LOSS_MATERIAL_THRESHOLD <- 0.005

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
    message("Package glmnet is not installed. Regularized multinomial GLM will be skipped.")
}

if (!has_lightgbm) {
    message("Package lightgbm is not installed. LightGBM model will be skipped.")
}

# 2. Helpers

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
    class_index <- match(truth, c("H", "D", "A"))

    if (any(is.na(class_index))) {
        stop("Truth contains values outside H, D, A.", call. = FALSE)
    }

    -mean(log(probs[cbind(seq_along(class_index), class_index)]))
}

classwise_metrics <- function(
    truth,
    probs,
    model_name,
    split_name,
    feature_variant
) {
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
            feature_variant = feature_variant,
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
    classwise_metrics(
        truth,
        probs,
        model_name = "tmp",
        split_name = "tmp",
        feature_variant = "tmp"
    ) |>
        dplyr::summarise(macro_f1 = mean(f1), .groups = "drop") |>
        dplyr::pull(macro_f1)
}

confusion_matrix_tbl <- function(
    truth,
    probs,
    model_name,
    split_name,
    feature_variant
) {
    truth <- as.character(truth)
    pred <- c("H", "D", "A")[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    tibble::tibble(
        feature_variant = feature_variant,
        model = model_name,
        split = split_name,
        truth = factor(truth, levels = c("H", "D", "A")),
        prediction = factor(pred, levels = c("H", "D", "A"))
    ) |>
        dplyr::count(
            feature_variant,
            model,
            split,
            truth,
            prediction,
            name = "n"
        ) |>
        tidyr::complete(
            feature_variant,
            model,
            split,
            truth = factor(c("H", "D", "A"), levels = c("H", "D", "A")),
            prediction = factor(c("H", "D", "A"), levels = c("H", "D", "A")),
            fill = list(n = 0)
        )
}

score_model <- function(
    truth,
    probs,
    model_name,
    split_name,
    feature_variant
) {
    validate_probability_table(probs, model_name)

    truth <- as.character(truth)
    pred <- c("H", "D", "A")[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    max_pred_prob <- pmax(probs$pred_H, probs$pred_D, probs$pred_A)
    true_class_prob <- dplyr::case_when(
        truth == "H" ~ probs$pred_H,
        truth == "D" ~ probs$pred_D,
        truth == "A" ~ probs$pred_A,
        TRUE ~ NA_real_
    )

    draw_true_positive <- sum(pred == "D" & truth == "D")
    draw_false_positive <- sum(pred == "D" & truth != "D")
    draw_false_negative <- sum(pred != "D" & truth == "D")

    draw_precision <- ifelse(
        draw_true_positive + draw_false_positive == 0,
        0,
        draw_true_positive / (draw_true_positive + draw_false_positive)
    )
    draw_recall <- ifelse(
        draw_true_positive + draw_false_negative == 0,
        0,
        draw_true_positive / (draw_true_positive + draw_false_negative)
    )

    tibble::tibble(
        feature_variant = feature_variant,
        model = model_name,
        split = split_name,
        n = length(truth),
        log_loss = multiclass_log_loss(truth, probs),
        accuracy = mean(pred == truth),
        macro_f1 = macro_f1_score(truth, probs),
        draw_recall = draw_recall,
        draw_precision = draw_precision,
        predicted_draw_rate = mean(pred == "D"),
        actual_draw_rate = mean(truth == "D"),
        mean_pred_D = mean(probs$pred_D),
        median_pred_D = stats::median(probs$pred_D),
        mean_max_pred_prob = mean(max_pred_prob),
        mean_true_class_prob = mean(true_class_prob)
    )
}

make_prediction_tbl <- function(
    base_df,
    probs,
    model_name,
    split_name,
    feature_variant
) {
    pred_class <- c("H", "D", "A")[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    tibble::tibble(
        feature_variant = feature_variant,
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

compute_feature_correlations <- function(
    train_df,
    feature_names,
    feature_variant
) {
    feature_matrix <- train_df[, feature_names, drop = FALSE]

    non_numeric <- names(feature_matrix)[!vapply(feature_matrix, is.numeric, logical(1))]

    if (length(non_numeric) > 0) {
        stop(
            "Non-numeric features in correlation matrix for ",
            feature_variant,
            ": ",
            paste(non_numeric, collapse = ", "),
            call. = FALSE
        )
    }

    correlation_matrix <- stats::cor(feature_matrix, use = "complete.obs")

    as.data.frame(as.table(correlation_matrix)) |>
        tibble::as_tibble() |>
        dplyr::rename(
            feature_1 = Var1,
            feature_2 = Var2,
            correlation = Freq
        ) |>
        dplyr::mutate(
            feature_variant = feature_variant,
            feature_1 = as.character(feature_1),
            feature_2 = as.character(feature_2)
        ) |>
        dplyr::filter(feature_1 < feature_2) |>
        dplyr::mutate(
            abs_correlation = abs(correlation),
            high_correlation = abs_correlation >= 0.90
        ) |>
        dplyr::select(
            feature_variant,
            feature_1,
            feature_2,
            correlation,
            abs_correlation,
            high_correlation
        )
}

fit_variant_models <- function(
    train,
    validation,
    test,
    selected_features,
    feature_variant
) {
    model_formula <- stats::as.formula(
        paste("match_result ~", paste(selected_features, collapse = " + "))
    )

    results <- list(
        metrics = tibble::tibble(),
        classwise = tibble::tibble(),
        confusions = tibble::tibble(),
        predictions = tibble::tibble()
    )

    message("  Fitting multinom for ", feature_variant, "...")

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

    results$metrics <- dplyr::bind_rows(
        results$metrics,
        score_model(
            validation$match_result,
            multinom_val_probs,
            "multinom",
            "validation",
            feature_variant
        ),
        score_model(
            test$match_result,
            multinom_test_probs,
            "multinom",
            "test",
            feature_variant
        )
    )

    results$classwise <- dplyr::bind_rows(
        results$classwise,
        classwise_metrics(
            validation$match_result,
            multinom_val_probs,
            "multinom",
            "validation",
            feature_variant
        ),
        classwise_metrics(
            test$match_result,
            multinom_test_probs,
            "multinom",
            "test",
            feature_variant
        )
    )

    results$confusions <- dplyr::bind_rows(
        results$confusions,
        confusion_matrix_tbl(
            validation$match_result,
            multinom_val_probs,
            "multinom",
            "validation",
            feature_variant
        ),
        confusion_matrix_tbl(
            test$match_result,
            multinom_test_probs,
            "multinom",
            "test",
            feature_variant
        )
    )

    results$predictions <- dplyr::bind_rows(
        results$predictions,
        make_prediction_tbl(
            validation,
            multinom_val_probs,
            "multinom",
            "validation",
            feature_variant
        ),
        make_prediction_tbl(
            test,
            multinom_test_probs,
            "multinom",
            "test",
            feature_variant
        )
    )

    if (has_glmnet) {
        message("  Fitting glmnet_multinomial_ridge for ", feature_variant, "...")

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

        results$metrics <- dplyr::bind_rows(
            results$metrics,
            score_model(
                validation$match_result,
                glmnet_val_probs,
                "glmnet_multinomial_ridge",
                "validation",
                feature_variant
            ),
            score_model(
                test$match_result,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                "test",
                feature_variant
            )
        )

        results$classwise <- dplyr::bind_rows(
            results$classwise,
            classwise_metrics(
                validation$match_result,
                glmnet_val_probs,
                "glmnet_multinomial_ridge",
                "validation",
                feature_variant
            ),
            classwise_metrics(
                test$match_result,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                "test",
                feature_variant
            )
        )

        results$confusions <- dplyr::bind_rows(
            results$confusions,
            confusion_matrix_tbl(
                validation$match_result,
                glmnet_val_probs,
                "glmnet_multinomial_ridge",
                "validation",
                feature_variant
            ),
            confusion_matrix_tbl(
                test$match_result,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                "test",
                feature_variant
            )
        )

        results$predictions <- dplyr::bind_rows(
            results$predictions,
            make_prediction_tbl(
                validation,
                glmnet_val_probs,
                "glmnet_multinomial_ridge",
                "validation",
                feature_variant
            ),
            make_prediction_tbl(
                test,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                "test",
                feature_variant
            )
        )
    }

    if (has_lightgbm) {
        message("  Fitting lightgbm for ", feature_variant, "...")

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
            verbose = 0
        )

        raw_val_pred <- predict(lgb_fit, x_validation_lgb)
        raw_test_pred <- predict(lgb_fit, x_test_lgb)

        lgb_val_probs <- lightgbm_raw_probs_to_pred_columns(
            raw_val_pred,
            nrow(validation)
        )
        lgb_test_probs <- lightgbm_raw_probs_to_pred_columns(
            raw_test_pred,
            nrow(test)
        )

        results$metrics <- dplyr::bind_rows(
            results$metrics,
            score_model(
                validation$match_result,
                lgb_val_probs,
                "lightgbm",
                "validation",
                feature_variant
            ),
            score_model(
                test$match_result,
                lgb_test_probs,
                "lightgbm",
                "test",
                feature_variant
            )
        )

        results$classwise <- dplyr::bind_rows(
            results$classwise,
            classwise_metrics(
                validation$match_result,
                lgb_val_probs,
                "lightgbm",
                "validation",
                feature_variant
            ),
            classwise_metrics(
                test$match_result,
                lgb_test_probs,
                "lightgbm",
                "test",
                feature_variant
            )
        )

        results$confusions <- dplyr::bind_rows(
            results$confusions,
            confusion_matrix_tbl(
                validation$match_result,
                lgb_val_probs,
                "lightgbm",
                "validation",
                feature_variant
            ),
            confusion_matrix_tbl(
                test$match_result,
                lgb_test_probs,
                "lightgbm",
                "test",
                feature_variant
            )
        )

        results$predictions <- dplyr::bind_rows(
            results$predictions,
            make_prediction_tbl(
                validation,
                lgb_val_probs,
                "lightgbm",
                "validation",
                feature_variant
            ),
            make_prediction_tbl(
                test,
                lgb_test_probs,
                "lightgbm",
                "test",
                feature_variant
            )
        )
    }

    results
}

make_chronological_splits <- function(modeling_df) {
    train_all <- modeling_df |>
        dplyr::filter(data_split == "train") |>
        dplyr::arrange(date)

    test <- modeling_df |>
        dplyr::filter(data_split == "test") |>
        dplyr::arrange(date)

    if (nrow(train_all) == 0) {
        stop("No training rows after filtering.", call. = FALSE)
    }

    if (nrow(test) == 0) {
        stop("No test rows after filtering.", call. = FALSE)
    }

    validation_fraction <- 0.20
    validation_start_index <- floor(nrow(train_all) * (1 - validation_fraction)) + 1

    list(
        train = train_all[seq_len(validation_start_index - 1), ],
        validation = train_all[validation_start_index:nrow(train_all), ],
        test = test
    )
}

# 3. Load data

modeling_path <- "data/processed/international_modeling_table_with_form.csv"

if (!file.exists(modeling_path)) {
    stop("Missing modeling table: ", modeling_path, call. = FALSE)
}

modeling <- readr::read_csv(modeling_path, show_col_types = FALSE)

message("Loaded modeling table rows: ", nrow(modeling))
message("Loaded modeling table columns: ", ncol(modeling))

# 4. Feature variants and forbidden predictors

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

form_draw_features <- c(
    "home_draw_rate_last_5",
    "away_draw_rate_last_5",
    "form_draw_rate_mean_last_5",
    "home_draw_rate_last_10",
    "away_draw_rate_last_10",
    "form_draw_rate_mean_last_10"
)

feature_variants <- list(
    baseline_safe = base_safe_features,
    safe_plus_form_compact = c(base_safe_features, form_compact_features),
    safe_plus_form_draw = c(base_safe_features, form_draw_features)
)

feature_variants_tbl <- tibble::tibble(
    feature_variant = names(feature_variants),
    n_features = vapply(feature_variants, length, integer(1)),
    features = vapply(
        feature_variants,
        function(feat_list) paste(feat_list, collapse = "; "),
        character(1)
    )
)

all_variant_features <- unique(unlist(feature_variants))

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

forbidden_predictors <- unique(c(use_with_care_features, leakage_cols))

bad_features <- intersect(all_variant_features, forbidden_predictors)

if (length(bad_features) > 0) {
    stop(
        "Feature variants include leakage or use-with-care columns: ",
        paste(bad_features, collapse = ", "),
        call. = FALSE
    )
}

required_cols <- c(
    "source_match_id",
    "date",
    "data_split",
    "match_result",
    "home_prior_matches",
    "away_prior_matches",
    all_variant_features
)

missing_cols <- setdiff(required_cols, names(modeling))

if (length(missing_cols) > 0) {
    stop(
        "Modeling table is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
    )
}

message("Feature variants to test:")
print(feature_variants_tbl)

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
minimum_prior_matches <- 10L

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
    dplyr::filter(
        !is.na(home_prior_matches),
        !is.na(away_prior_matches),
        home_prior_matches >= minimum_prior_matches,
        away_prior_matches >= minimum_prior_matches
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
        dplyr::all_of(all_variant_features)
    ) |>
    dplyr::mutate(
        date = as.Date(date),
        match_result = factor(match_result, levels = c("H", "D", "A")),
        dplyr::across(dplyr::all_of(all_variant_features), ~ {
            if (is.logical(.x)) {
                as.integer(.x)
            } else {
                .x
            }
        })
    )

for (variant_name in names(feature_variants)) {
    variant_features <- feature_variants[[variant_name]]
    variant_complete_rows <- df |>
        dplyr::select(dplyr::all_of(variant_features)) |>
        stats::complete.cases() |>
        sum()

    filter_counts <- dplyr::bind_rows(
        filter_counts,
        tibble::tibble(
            step = paste0("complete_cases_", variant_name),
            n_rows = variant_complete_rows
        )
    )
}

df <- df |>
    tidyr::drop_na(dplyr::all_of(all_variant_features))

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "complete_all_variant_features", n_rows = nrow(df))
)

readr::write_csv(filter_counts, "reports/tables/model_28_filter_counts.csv")
print(filter_counts)

if (!all(df$data_split %in% c("train", "test"))) {
    message("data_split contains values other than train/test:")
    print(table(df$data_split, useNA = "ifany"))
}

splits <- make_chronological_splits(df)
train <- splits$train
validation <- splits$validation
test <- splits$test

message("Train rows: ", nrow(train))
message("Validation rows: ", nrow(validation))
message("Test rows: ", nrow(test))

split_summary <- tibble::tibble(
    split = c("train", "validation", "test"),
    n_rows = c(nrow(train), nrow(validation), nrow(test)),
    min_date = c(min(train$date), min(validation$date), min(test$date)),
    max_date = c(max(train$date), max(validation$date), max(test$date))
)

readr::write_csv(split_summary, "reports/tables/model_28_split_summary.csv")
readr::write_csv(feature_variants_tbl, "reports/tables/model_28_feature_variants.csv")
print(split_summary)

# 6. Variant loop: correlations, model fitting, metrics

all_metrics <- tibble::tibble()
all_classwise <- tibble::tibble()
all_confusions <- tibble::tibble()
all_predictions <- tibble::tibble()
all_correlations <- tibble::tibble()

for (variant_name in names(feature_variants)) {
    selected_features <- feature_variants[[variant_name]]

    bad_variant_features <- intersect(selected_features, forbidden_predictors)

    if (length(bad_variant_features) > 0) {
        stop(
            "Variant ",
            variant_name,
            " includes leakage or use-with-care columns: ",
            paste(bad_variant_features, collapse = ", "),
            call. = FALSE
        )
    }

    message("Processing feature variant: ", variant_name)

    variant_correlations <- compute_feature_correlations(
        train,
        selected_features,
        variant_name
    )

    all_correlations <- dplyr::bind_rows(all_correlations, variant_correlations)

    variant_results <- fit_variant_models(
        train = train,
        validation = validation,
        test = test,
        selected_features = selected_features,
        feature_variant = variant_name
    )

    all_metrics <- dplyr::bind_rows(all_metrics, variant_results$metrics)
    all_classwise <- dplyr::bind_rows(all_classwise, variant_results$classwise)
    all_confusions <- dplyr::bind_rows(all_confusions, variant_results$confusions)
    all_predictions <- dplyr::bind_rows(all_predictions, variant_results$predictions)
}

high_feature_correlations <- all_correlations |>
    dplyr::filter(high_correlation)

readr::write_csv(all_metrics, "reports/tables/model_28_metrics.csv")
readr::write_csv(all_classwise, "reports/tables/model_28_classwise_metrics.csv")
readr::write_csv(all_confusions, "reports/tables/model_28_confusion_matrices.csv")
readr::write_csv(all_predictions, "reports/tables/model_28_predictions.csv")
readr::write_csv(all_correlations, "reports/tables/model_28_high_feature_correlations.csv")

# 7. Best validation models

validation_ranking <- all_metrics |>
    dplyr::filter(split == "validation") |>
    dplyr::arrange(log_loss)

best_validation_models <- validation_ranking |>
    dplyr::slice_head(n = 1)

best_by_variant <- all_metrics |>
    dplyr::filter(split == "validation") |>
    dplyr::group_by(feature_variant) |>
    dplyr::slice_min(order_by = log_loss, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

readr::write_csv(
    dplyr::bind_rows(
        best_validation_models |>
            dplyr::mutate(selection_type = "overall_best"),
        best_by_variant |>
            dplyr::mutate(selection_type = "best_within_variant")
    ),
    "reports/tables/model_28_best_validation_models.csv"
)

# 8. Comparison to Models 24 and 26

compare_sources <- list()

model_24_metrics_path <- "reports/tables/model_24_metrics.csv"

if (file.exists(model_24_metrics_path)) {
    model_24_metrics <- readr::read_csv(model_24_metrics_path, show_col_types = FALSE)

    compare_sources[["model_24"]] <- model_24_metrics |>
        dplyr::mutate(
            feature_variant = "model_24_baseline_safe",
            source = "model_24"
        )
}

model_26_metrics_path <- "reports/tables/model_26_metrics.csv"

if (file.exists(model_26_metrics_path)) {
    model_26_metrics <- readr::read_csv(model_26_metrics_path, show_col_types = FALSE)

    compare_sources[["model_26"]] <- model_26_metrics |>
        dplyr::mutate(source = "model_26")
}

if (length(compare_sources) > 0) {
    compare_metrics <- dplyr::bind_rows(
        !!!compare_sources,
        all_metrics |>
            dplyr::mutate(source = "model_28")
    ) |>
        dplyr::select(
            source,
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
                "median_pred_D",
                "mean_max_pred_prob",
                "mean_true_class_prob"
            ))
        ) |>
        dplyr::arrange(split, log_loss)

    readr::write_csv(
        compare_metrics,
        "reports/tables/model_28_compare_to_model_24_and_26.csv"
    )
} else {
    message(
        "model_24_metrics.csv and model_26_metrics.csv not found; ",
        "skipping comparison table."
    )
}

# 9. Figures

validation_log_loss_plot <- all_metrics |>
    dplyr::filter(split == "validation") |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = feature_variant,
            y = log_loss,
            fill = model
        )
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
        title = "Model 28 validation log loss by feature variant",
        x = "Feature variant",
        y = "Multiclass log loss",
        fill = "Model"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

ggplot2::ggsave(
    filename = "reports/figures/model_28_validation_log_loss_by_variant.png",
    plot = validation_log_loss_plot,
    width = 10,
    height = 6,
    dpi = 300
)

test_log_loss_plot <- all_metrics |>
    dplyr::filter(split == "test") |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = feature_variant,
            y = log_loss,
            fill = model
        )
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
        title = "Model 28 test log loss by feature variant",
        x = "Feature variant",
        y = "Multiclass log loss",
        fill = "Model"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

ggplot2::ggsave(
    filename = "reports/figures/model_28_test_log_loss_by_variant.png",
    plot = test_log_loss_plot,
    width = 10,
    height = 6,
    dpi = 300
)

draw_recall_plot <- all_metrics |>
    dplyr::filter(split == "test") |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = feature_variant,
            y = draw_recall,
            fill = model
        )
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
        title = "Model 28 test draw recall by feature variant",
        x = "Feature variant",
        y = "Draw recall",
        fill = "Model"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

ggplot2::ggsave(
    filename = "reports/figures/model_28_draw_recall_by_variant.png",
    plot = draw_recall_plot,
    width = 10,
    height = 6,
    dpi = 300
)

predicted_vs_actual_draw_plot <- all_metrics |>
    dplyr::filter(split == "test") |>
    tidyr::pivot_longer(
        cols = c(predicted_draw_rate, actual_draw_rate),
        names_to = "rate_type",
        values_to = "rate"
    ) |>
    dplyr::mutate(
        rate_type = dplyr::recode(
            rate_type,
            predicted_draw_rate = "Predicted draw rate",
            actual_draw_rate = "Actual draw rate"
        )
    ) |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = feature_variant,
            y = rate,
            fill = rate_type
        )
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~ model) +
    ggplot2::labs(
        title = "Model 28 predicted vs actual draw rate (test set)",
        x = "Feature variant",
        y = "Rate",
        fill = NULL
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

ggplot2::ggsave(
    filename = "reports/figures/model_28_predicted_vs_actual_draw_rate.png",
    plot = predicted_vs_actual_draw_plot,
    width = 11,
    height = 6,
    dpi = 300
)

macro_f1_plot <- all_metrics |>
    dplyr::filter(split == "test") |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = feature_variant,
            y = macro_f1,
            fill = model
        )
    ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
        title = "Model 28 test macro F1 by feature variant",
        x = "Feature variant",
        y = "Macro F1",
        fill = "Model"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

ggplot2::ggsave(
    filename = "reports/figures/model_28_macro_f1_by_variant.png",
    plot = macro_f1_plot,
    width = 10,
    height = 6,
    dpi = 300
)

# 10. Final summary

selected_model_row <- best_validation_models[1, ]
selected_test_metrics <- all_metrics |>
    dplyr::filter(
        split == "test",
        feature_variant == selected_model_row$feature_variant,
        model == selected_model_row$model
    )

baseline_safe_validation <- all_metrics |>
    dplyr::filter(
        split == "validation",
        feature_variant == "baseline_safe"
    )

best_baseline_log_loss <- min(baseline_safe_validation$log_loss)

form_variants <- setdiff(names(feature_variants), "baseline_safe")

form_beat_baseline_small <- validation_ranking |>
    dplyr::filter(
        feature_variant %in% form_variants,
        log_loss <= best_baseline_log_loss - LOG_LOSS_BEAT_THRESHOLD_SMALL
    )

form_beat_baseline_large <- validation_ranking |>
    dplyr::filter(
        feature_variant %in% form_variants,
        log_loss <= best_baseline_log_loss - LOG_LOSS_BEAT_THRESHOLD_LARGE
    )

draw_recall_improvements <- all_metrics |>
    dplyr::filter(split == "validation") |>
    dplyr::inner_join(
        baseline_safe_validation |>
            dplyr::select(model, baseline_log_loss = log_loss, baseline_draw_recall = draw_recall),
        by = "model"
    ) |>
    dplyr::filter(feature_variant %in% form_variants) |>
    dplyr::mutate(
        log_loss_delta = log_loss - baseline_log_loss,
        draw_recall_delta = draw_recall - baseline_draw_recall,
        improved_draw_recall_without_material_log_loss_worsening = (
            draw_recall_delta > 0 &
                log_loss_delta <= LOG_LOSS_MATERIAL_THRESHOLD
        )
    )

worth_adopting_draw <- draw_recall_improvements |>
    dplyr::filter(improved_draw_recall_without_material_log_loss_worsening)

message("=================================================================")
message("Model 28 complete.")
message("")
message("1. Filter counts:")
print(filter_counts)
message("")
message("2. Split summary:")
print(split_summary)
message("")
message("3. Feature variants tested:")
print(feature_variants_tbl)
message("")
message("4. High-correlation feature pairs (|r| >= 0.90):")
if (nrow(high_feature_correlations) == 0) {
    message("  (none)")
} else {
    print(high_feature_correlations)
}
message("")
message("5. Validation ranking by log loss:")
print(validation_ranking)
message("")
message("6. Best selected model by validation log loss:")
print(selected_model_row)
message("")
message("7. Selected model test metrics:")
print(selected_test_metrics)
message("")
message(
    "8. Form variants beating baseline_safe on validation log loss by at least ",
    LOG_LOSS_BEAT_THRESHOLD_SMALL,
    ":"
)
if (nrow(form_beat_baseline_small) == 0) {
    message("  None.")
} else {
    print(form_beat_baseline_small)
}
message("")
message(
    "9. Form variants beating baseline_safe on validation log loss by at least ",
    LOG_LOSS_BEAT_THRESHOLD_LARGE,
    ":"
)
if (nrow(form_beat_baseline_large) == 0) {
    message("  None.")
} else {
    print(form_beat_baseline_large)
}
message("")
message("10. Form variants with improved draw recall without materially")
message("    worsening validation log loss (threshold = ", LOG_LOSS_MATERIAL_THRESHOLD, "):")
if (nrow(worth_adopting_draw) == 0) {
    message("  None.")
} else {
    print(worth_adopting_draw)
}
message("")
message("Best model within each feature variant (validation log loss):")
print(best_by_variant)
message("")
message("Outputs written to reports/tables and reports/figures.")
message("=================================================================")
