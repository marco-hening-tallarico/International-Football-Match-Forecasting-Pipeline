# 30_model_with_goalscorer_features.R
#
# Model 30: trains H/D/A models with lagged form plus goalscorer features on
# the fair-comparison complete-case cohort. Model selection uses validation
# log loss.
#
# Reads: data/processed/international_modeling_table_with_form_and_goalscorers.csv
#
# Writes:
#   reports/tables/model_30/
#   data/predictions/model_30_lightgbm_predictions.csv (when exported)

set.seed(2026)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

OUTPUT_DIR <- file.path(REPORTS_TABLES_DIR, "model_30")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PREDICTIONS_DIR, recursive = TRUE, showWarnings = FALSE)

MODELING_PATH <- file.path(
    PROCESSED_DIR,
    "international_modeling_table_with_form_and_goalscorers.csv"
)

TARGET_LEVELS <- c("H", "D", "A")
TOURNAMENT_LUMP_MIN_COUNT <- 100L
VALIDATION_FRACTION <- 0.20

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
has_ranger <- requireNamespace("ranger", quietly = TRUE)

tree_model_name <- if (has_lightgbm) {
    "lightgbm"
} else if (has_ranger) {
    "ranger"
} else {
    NA_character_
}

# 2. Helpers

clip_probs <- function(probability_values, eps = 1e-15) {
    pmax(pmin(probability_values, 1 - eps), eps)
}

result_class_to_outcome <- function(result_class_values) {
    dplyr::case_when(
        result_class_values %in% c(1L, "1", "H") ~ "H",
        result_class_values %in% c(0L, "0", "D") ~ "D",
        result_class_values %in% c(-1L, "-1", "A") ~ "A",
        TRUE ~ NA_character_
    )
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
    model_name,
    split_name,
    feature_set
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
        f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

        tibble::tibble(
            feature_set = feature_set,
            model_name = model_name,
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
        feature_set = "tmp"
    ) |>
        dplyr::summarise(macro_f1 = mean(f1), .groups = "drop") |>
        dplyr::pull(macro_f1)
}

confusion_matrix_tbl <- function(
    truth,
    probs,
    model_name,
    split_name,
    feature_set
) {
    truth <- as.character(truth)
    pred <- TARGET_LEVELS[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    tibble::tibble(
        feature_set = feature_set,
        model_name = model_name,
        split = split_name,
        actual_class = factor(truth, levels = TARGET_LEVELS),
        predicted_class = factor(pred, levels = TARGET_LEVELS)
    ) |>
        dplyr::count(
            feature_set,
            model_name,
            split,
            actual_class,
            predicted_class,
            name = "n"
        ) |>
        tidyr::complete(
            feature_set,
            model_name,
            split,
            actual_class = factor(TARGET_LEVELS, levels = TARGET_LEVELS),
            predicted_class = factor(TARGET_LEVELS, levels = TARGET_LEVELS),
            fill = list(n = 0)
        )
}

score_model <- function(
    truth,
    probs,
    model_name,
    split_name,
    feature_set
) {
    validate_probability_table(probs, model_name)

    truth <- as.character(truth)
    pred <- TARGET_LEVELS[max.col(probs[, c("pred_H", "pred_D", "pred_A")])]

    tibble::tibble(
        feature_set = feature_set,
        model_name = model_name,
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

make_prediction_examples <- function(
    base_df,
    probs,
    model_name,
    feature_set
) {
    predicted_class <- TARGET_LEVELS[
        max.col(probs[, c("pred_H", "pred_D", "pred_A")])
    ]

    tibble::tibble(
        date = base_df$date,
        home_team = base_df$home_team,
        away_team = base_df$away_team,
        tournament = if ("tournament" %in% names(base_df)) base_df$tournament else NA_character_,
        actual_result_class = as.character(base_df$outcome),
        predicted_class = predicted_class,
        p_home = probs$pred_H,
        p_draw = probs$pred_D,
        p_away = probs$pred_A,
        model_name = model_name,
        feature_set = feature_set
    )
}

make_calibration_bins <- function(
    truth,
    probs,
    model_name,
    feature_set,
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
                model_name = model_name,
                feature_set = feature_set,
                split = split_name,
                mean_predicted_probability = mean(probability_value),
                observed_frequency = mean(observed),
                n = dplyr::n(),
                .groups = "drop"
            )
    })
}

prepare_tournament_lumping <- function(
    tournament_values,
    training_tournament_values,
    min_count = TOURNAMENT_LUMP_MIN_COUNT
) {
    training_counts <- sort(
        table(training_tournament_values),
        decreasing = TRUE
    )
    keep_levels <- names(training_counts[training_counts >= min_count])

    lumped_values <- as.character(tournament_values)
    lumped_values[!lumped_values %in% keep_levels] <- "Other"

    factor(lumped_values, levels = c(keep_levels, "Other"))
}

expand_model_features <- function(
    train_df,
    validation_df,
    test_df,
    selected_features
) {
    if (!"tournament_lumped" %in% selected_features) {
        numeric_features <- selected_features

        coerce_numeric <- function(data_frame) {
            data_frame |>
                dplyr::mutate(
                    dplyr::across(
                        dplyr::all_of(numeric_features),
                        ~ if (is.logical(.x)) as.integer(.x) else .x
                    )
                )
        }

        return(list(
            train = coerce_numeric(train_df),
            validation = coerce_numeric(validation_df),
            test = coerce_numeric(test_df),
            model_features = numeric_features
        ))
    }

    other_features <- setdiff(selected_features, "tournament_lumped")
    reference_levels <- levels(train_df$tournament_lumped)

    encode_split <- function(data_frame) {
        data_frame <- data_frame |>
            dplyr::mutate(
                tournament_lumped = factor(
                    as.character(tournament_lumped),
                    levels = reference_levels
                )
            )

        dummy_matrix <- stats::model.matrix(
            ~ tournament_lumped - 1,
            data = data_frame
        )
        dummy_colnames <- colnames(dummy_matrix)

        if (length(other_features) > 0) {
            feature_matrix <- data_frame |>
                dplyr::select(dplyr::all_of(other_features)) |>
                dplyr::mutate(
                    dplyr::across(
                        dplyr::everything(),
                        ~ if (is.logical(.x)) as.integer(.x) else .x
                    )
                ) |>
                as.data.frame()

            combined <- cbind(feature_matrix, dummy_matrix)
        } else {
            combined <- as.data.frame(dummy_matrix)
        }

        colnames(combined) <- make.names(colnames(combined), unique = TRUE)

        combined
    }

    train_encoded <- encode_split(train_df)
    validation_encoded <- encode_split(validation_df)
    test_encoded <- encode_split(test_df)

    list(
        train = train_encoded,
        validation = validation_encoded,
        test = test_encoded,
        model_features = colnames(train_encoded)
    )
}

fit_variant_models <- function(
    train,
    validation,
    test,
    selected_features,
    feature_set
) {
    expanded <- expand_model_features(
        train,
        validation,
        test,
        selected_features
    )

    train_x <- expanded$train
    validation_x <- expanded$validation
    test_x <- expanded$test
    model_features <- expanded$model_features

    model_formula <- stats::as.formula(
        paste("outcome ~", paste(model_features, collapse = " + "))
    )

    results <- list(
        metrics = tibble::tibble(),
        classwise = tibble::tibble(),
        confusions = tibble::tibble(),
        predictions = tibble::tibble(),
        feature_importance = tibble::tibble()
    )

    message("  Fitting multinom for ", feature_set, "...")

    train_modeling <- train |>
        dplyr::select(outcome, dplyr::all_of(selected_features)) |>
        dplyr::mutate(
            outcome = factor(outcome, levels = TARGET_LEVELS),
            dplyr::across(
                dplyr::where(is.logical),
                ~ as.integer(.x)
            )
        )

    validation_modeling <- validation |>
        dplyr::select(outcome, dplyr::all_of(selected_features))

    test_modeling <- test |>
        dplyr::select(outcome, dplyr::all_of(selected_features))

    multinom_formula <- stats::as.formula(
        paste("outcome ~", paste(selected_features, collapse = " + "))
    )

    multinom_fit <- nnet::multinom(
        formula = multinom_formula,
        data = train_modeling,
        trace = FALSE,
        MaxNWts = 20000
    )

    multinom_val_probs <- predict(multinom_fit, newdata = validation_modeling, type = "probs") |>
        safe_rename_probability_columns()

    if (is.vector(multinom_val_probs)) {
        multinom_val_probs <- tibble::tibble(
            pred_H = rep(NA_real_, nrow(validation_modeling)),
            pred_D = rep(NA_real_, nrow(validation_modeling)),
            pred_A = rep(NA_real_, nrow(validation_modeling))
        )
        if (nrow(validation_modeling) == 1) {
            class_label <- as.character(validation_modeling$outcome)
            prob_col <- paste0("pred_", class_label)
            multinom_val_probs[[prob_col]] <- 1
        }
    }

    multinom_test_probs <- predict(multinom_fit, newdata = test_modeling, type = "probs") |>
        safe_rename_probability_columns()

    if (is.vector(multinom_test_probs)) {
        multinom_test_probs <- tibble::tibble(
            pred_H = rep(NA_real_, nrow(test_modeling)),
            pred_D = rep(NA_real_, nrow(test_modeling)),
            pred_A = rep(NA_real_, nrow(test_modeling))
        )
        if (nrow(test_modeling) == 1) {
            class_label <- as.character(test_modeling$outcome)
            prob_col <- paste0("pred_", class_label)
            multinom_test_probs[[prob_col]] <- 1
        }
    }

    results$metrics <- dplyr::bind_rows(
        results$metrics,
        score_model(
            validation$outcome,
            multinom_val_probs,
            "multinom",
            "validation",
            feature_set
        ),
        score_model(
            test$outcome,
            multinom_test_probs,
            "multinom",
            "test",
            feature_set
        )
    )

    results$classwise <- dplyr::bind_rows(
        results$classwise,
        classwise_metrics(
            validation$outcome,
            multinom_val_probs,
            "multinom",
            "validation",
            feature_set
        ),
        classwise_metrics(
            test$outcome,
            multinom_test_probs,
            "multinom",
            "test",
            feature_set
        )
    )

    results$confusions <- dplyr::bind_rows(
        results$confusions,
        confusion_matrix_tbl(
            validation$outcome,
            multinom_val_probs,
            "multinom",
            "validation",
            feature_set
        ),
        confusion_matrix_tbl(
            test$outcome,
            multinom_test_probs,
            "multinom",
            "test",
            feature_set
        )
    )

    results$predictions <- dplyr::bind_rows(
        results$predictions,
        make_prediction_examples(
            test,
            multinom_test_probs,
            "multinom",
            feature_set
        )
    )

    multinom_coef <- summary(multinom_fit)$coefficients
    if (!is.null(multinom_coef)) {
        coef_tbl <- as.data.frame(multinom_coef) |>
            tibble::rownames_to_column("class") |>
            tidyr::pivot_longer(
                cols = -class,
                names_to = "feature",
                values_to = "coefficient"
            ) |>
            dplyr::mutate(
                feature_set = feature_set,
                model_name = "multinom"
            )

        results$feature_importance <- dplyr::bind_rows(
            results$feature_importance,
            coef_tbl
        )
    }

    if (has_glmnet) {
        message("  Fitting glmnet_multinomial_ridge for ", feature_set, "...")

        x_train <- stats::model.matrix(model_formula, data = cbind(outcome = train$outcome, train_x))[, -1, drop = FALSE]
        y_train <- factor(train$outcome, levels = TARGET_LEVELS)

        x_validation <- stats::model.matrix(model_formula, data = cbind(outcome = validation$outcome, validation_x))[, -1, drop = FALSE]
        x_test <- stats::model.matrix(model_formula, data = cbind(outcome = test$outcome, test_x))[, -1, drop = FALSE]

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
                validation$outcome,
                glmnet_val_probs,
                "glmnet_multinomial_ridge",
                "validation",
                feature_set
            ),
            score_model(
                test$outcome,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                "test",
                feature_set
            )
        )

        results$classwise <- dplyr::bind_rows(
            results$classwise,
            classwise_metrics(
                validation$outcome,
                glmnet_val_probs,
                "glmnet_multinomial_ridge",
                "validation",
                feature_set
            ),
            classwise_metrics(
                test$outcome,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                "test",
                feature_set
            )
        )

        results$confusions <- dplyr::bind_rows(
            results$confusions,
            confusion_matrix_tbl(
                validation$outcome,
                glmnet_val_probs,
                "glmnet_multinomial_ridge",
                "validation",
                feature_set
            ),
            confusion_matrix_tbl(
                test$outcome,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                "test",
                feature_set
            )
        )

        results$predictions <- dplyr::bind_rows(
            results$predictions,
            make_prediction_examples(
                test,
                glmnet_test_probs,
                "glmnet_multinomial_ridge",
                feature_set
            )
        )

        glmnet_coef <- coef(glmnet_fit, s = "lambda.min")
        coef_rows <- purrr::map_dfr(names(glmnet_coef), function(class_label) {
            coef_matrix <- as.matrix(glmnet_coef[[class_label]])
            tibble::tibble(
                feature_set = feature_set,
                model_name = "glmnet_multinomial_ridge",
                class = class_label,
                feature = rownames(coef_matrix),
                coefficient = as.numeric(coef_matrix[, 1])
            )
        })
        results$feature_importance <- dplyr::bind_rows(
            results$feature_importance,
            coef_rows
        )
    }

    if (has_lightgbm) {
        message("  Fitting lightgbm for ", feature_set, "...")

        label_map <- c("A" = 0, "D" = 1, "H" = 2)

        train_lgb <- train |>
            dplyr::mutate(label = unname(label_map[as.character(outcome)]))

        validation_lgb <- validation |>
            dplyr::mutate(label = unname(label_map[as.character(outcome)]))

        x_train_lgb <- as.matrix(train_x[, model_features, drop = FALSE])
        x_validation_lgb <- as.matrix(validation_x[, model_features, drop = FALSE])
        x_test_lgb <- as.matrix(test_x[, model_features, drop = FALSE])

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
                validation$outcome,
                lgb_val_probs,
                "lightgbm",
                "validation",
                feature_set
            ),
            score_model(
                test$outcome,
                lgb_test_probs,
                "lightgbm",
                "test",
                feature_set
            )
        )

        results$classwise <- dplyr::bind_rows(
            results$classwise,
            classwise_metrics(
                validation$outcome,
                lgb_val_probs,
                "lightgbm",
                "validation",
                feature_set
            ),
            classwise_metrics(
                test$outcome,
                lgb_test_probs,
                "lightgbm",
                "test",
                feature_set
            )
        )

        results$confusions <- dplyr::bind_rows(
            results$confusions,
            confusion_matrix_tbl(
                validation$outcome,
                lgb_val_probs,
                "lightgbm",
                "validation",
                feature_set
            ),
            confusion_matrix_tbl(
                test$outcome,
                lgb_test_probs,
                "lightgbm",
                "test",
                feature_set
            )
        )

        results$predictions <- dplyr::bind_rows(
            results$predictions,
            make_prediction_examples(
                test,
                lgb_test_probs,
                "lightgbm",
                feature_set
            )
        )

        lgb_importance <- lightgbm::lgb.importance(
            model = lgb_fit,
            percentage = TRUE
        )

        results$feature_importance <- dplyr::bind_rows(
            results$feature_importance,
            lgb_importance |>
                tibble::as_tibble() |>
                dplyr::mutate(
                    feature_set = feature_set,
                    model_name = "lightgbm"
                ) |>
                dplyr::rename(feature = Feature, importance_gain = Gain)
        )
    } else if (has_ranger) {
        message("  Fitting ranger (LightGBM unavailable) for ", feature_set, "...")

        ranger_formula <- stats::as.formula(
            paste("outcome ~", paste(model_features, collapse = " + "))
        )

        ranger_train <- cbind(outcome = factor(train$outcome, levels = TARGET_LEVELS), train_x)

        ranger_fit <- ranger::ranger(
            formula = ranger_formula,
            data = ranger_train,
            probability = TRUE,
            num.trees = 500,
            seed = 2026
        )

        ranger_val_probs <- predict(ranger_fit, data = validation_x)$predictions |>
            as.data.frame() |>
            safe_rename_probability_columns()

        ranger_test_probs <- predict(ranger_fit, data = test_x)$predictions |>
            as.data.frame() |>
            safe_rename_probability_columns()

        results$metrics <- dplyr::bind_rows(
            results$metrics,
            score_model(
                validation$outcome,
                ranger_val_probs,
                "ranger",
                "validation",
                feature_set
            ),
            score_model(
                test$outcome,
                ranger_test_probs,
                "ranger",
                "test",
                feature_set
            )
        )

        results$classwise <- dplyr::bind_rows(
            results$classwise,
            classwise_metrics(
                validation$outcome,
                ranger_val_probs,
                "ranger",
                "validation",
                feature_set
            ),
            classwise_metrics(
                test$outcome,
                ranger_test_probs,
                "ranger",
                "test",
                feature_set
            )
        )

        results$confusions <- dplyr::bind_rows(
            results$confusions,
            confusion_matrix_tbl(
                validation$outcome,
                ranger_val_probs,
                "ranger",
                "validation",
                feature_set
            ),
            confusion_matrix_tbl(
                test$outcome,
                ranger_test_probs,
                "ranger",
                "test",
                feature_set
            )
        )

        results$predictions <- dplyr::bind_rows(
            results$predictions,
            make_prediction_examples(
                test,
                ranger_test_probs,
                "ranger",
                feature_set
            )
        )

        if (!is.null(ranger_fit$variable.importance)) {
            results$feature_importance <- dplyr::bind_rows(
                results$feature_importance,
                tibble::tibble(
                    feature_set = feature_set,
                    model_name = "ranger",
                    feature = names(ranger_fit$variable.importance),
                    importance = as.numeric(ranger_fit$variable.importance)
                )
            )
        }
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

    validation_start_index <- floor(nrow(train_all) * (1 - VALIDATION_FRACTION)) + 1

    list(
        train = train_all[seq_len(validation_start_index - 1), ],
        validation = train_all[validation_start_index:nrow(train_all), ],
        test = test
    )
}

select_available_features <- function(
    candidate_features,
    available_columns,
    max_missing_fraction = 0.99
) {
    candidate_features <- intersect(candidate_features, available_columns)
    candidate_features <- candidate_features[
        vapply(candidate_features, function(feature_name) {
            missing_fraction <- mean(is.na(modeling[[feature_name]]))
            missing_fraction < max_missing_fraction
        }, logical(1))
    ]
    candidate_features
}

build_excluded_columns_audit <- function(
    all_columns,
    allowed_modeling_columns,
    selected_feature_union
) {
    leakage_patterns <- c(
        "score",
        "result",
        "outcome",
        "winner",
        "shootout",
        "penalty_result",
        "home_score",
        "away_score",
        "home_goals",
        "away_goals"
    )

    past_only_allowlist <- unique(c(
        "result_class",
        "goal_diff_last",
        "goals_for_last",
        "goals_against_last",
        "non_penalty_goals_last",
        "penalty_goals_last",
        "top_scorer_goals",
        "avg_goal_minute",
        selected_feature_union
    ))

    purrr::map_dfr(all_columns, function(column_name) {
        matched_patterns <- leakage_patterns[
            vapply(
                leakage_patterns,
                function(pattern) grepl(pattern, column_name, ignore.case = TRUE),
                logical(1)
            )
        ]

        is_allowlisted <- column_name %in% past_only_allowlist ||
            any(vapply(
                past_only_allowlist,
                function(allowed_name) {
                    grepl(paste0("^", allowed_name), column_name, ignore.case = TRUE)
                },
                logical(1)
            )) ||
            column_name %in% selected_feature_union

        excluded <- length(matched_patterns) > 0 && !is_allowlisted

        tibble::tibble(
            column = column_name,
            matched_patterns = paste(matched_patterns, collapse = "; "),
            is_allowlisted = is_allowlisted,
            excluded_from_modeling = excluded,
            reason = dplyr::case_when(
                is_allowlisted ~ "allowlisted_past_only_or_selected_feature",
                excluded ~ "matches_leakage_pattern",
                TRUE ~ "not_used_as_predictor"
            )
        )
    })
}

# 3. Load data

if (!file.exists(MODELING_PATH)) {
    stop("Missing modeling table: ", MODELING_PATH, call. = FALSE)
}

modeling <- readr::read_csv(MODELING_PATH, show_col_types = FALSE)

message("Loaded modeling table rows: ", nrow(modeling))
message("Loaded modeling table columns: ", ncol(modeling))

if (!"result_class" %in% names(modeling)) {
    stop("Modeling table is missing required outcome column: result_class", call. = FALSE)
}

# 4. Feature definitions

baseline_rating_candidates <- c(
    "rating_diff",
    "home_rating_pre_match",
    "away_rating_pre_match",
    "home_rank_pre_match",
    "away_rank_pre_match",
    "neutral"
)

context_candidates <- c(
    "is_friendly",
    "is_world_cup",
    "is_world_cup_qualifier",
    "is_continental_tournament",
    "is_continental_qualifier"
)

form_candidates <- c(
    "form_points_diff_last_5",
    "form_points_diff_last_10",
    "form_goal_diff_diff_last_5",
    "form_goal_diff_diff_last_10",
    "form_goals_for_diff_last_5",
    "form_goals_for_diff_last_10",
    "form_goals_against_diff_last_5",
    "form_goals_against_diff_last_10",
    "form_draw_rate_mean_last_5",
    "form_draw_rate_mean_last_10",
    "form_draw_rate_diff_last_5",
    "form_draw_rate_diff_last_10",
    "home_prior_matches",
    "away_prior_matches",
    "home_points_per_match_last_5",
    "away_points_per_match_last_5",
    "home_points_per_match_last_10",
    "away_points_per_match_last_10",
    "home_goal_diff_per_match_last_5",
    "away_goal_diff_per_match_last_5",
    "home_goal_diff_per_match_last_10",
    "away_goal_diff_per_match_last_10",
    "home_goals_for_per_match_last_5",
    "away_goals_for_per_match_last_5",
    "home_goals_for_per_match_last_10",
    "away_goals_for_per_match_last_10",
    "home_goals_against_per_match_last_5",
    "away_goals_against_per_match_last_5",
    "home_goals_against_per_match_last_10",
    "away_goals_against_per_match_last_10",
    "home_draw_rate_last_5",
    "away_draw_rate_last_5",
    "home_draw_rate_last_10",
    "away_draw_rate_last_10"
)

goalscorer_main_candidates <- c(
    "unique_scorers_diff_last_10",
    "top_scorer_goals_diff_last_10",
    "non_penalty_goals_diff_last_10",
    "penalty_goals_diff_last_10",
    "unique_scorers_diff_365d",
    "top_scorer_goals_diff_365d",
    "non_penalty_goals_diff_365d",
    "penalty_goals_diff_365d"
)

goalscorer_optional_candidates <- c("avg_goal_minute_diff_365d")

baseline_rating_features <- select_available_features(
    baseline_rating_candidates,
    names(modeling)
)

context_features <- select_available_features(
    context_candidates,
    names(modeling)
)

form_features <- select_available_features(
    form_candidates,
    names(modeling)
)

goalscorer_main_features <- select_available_features(
    goalscorer_main_candidates,
    names(modeling)
)

goalscorer_optional_features <- select_available_features(
    goalscorer_optional_candidates,
    names(modeling)
)

include_tournament <- "tournament" %in% names(modeling)

if (include_tournament) {
    context_features <- unique(c("tournament_lumped", context_features))
}

feature_sets <- list(
    baseline_rating = baseline_rating_features,
    rating_plus_context = c(baseline_rating_features, context_features),
    rating_plus_form = c(
        baseline_rating_features,
        context_features,
        form_features
    ),
    rating_plus_form_plus_goalscorers = c(
        baseline_rating_features,
        context_features,
        form_features,
        goalscorer_main_features
    )
)

if (length(goalscorer_optional_features) > 0) {
    feature_sets$rating_plus_form_plus_goalscorers_with_avg_minute <- c(
        baseline_rating_features,
        context_features,
        form_features,
        goalscorer_main_features,
        goalscorer_optional_features
    )
}

main_feature_sets <- c(
    "baseline_rating",
    "rating_plus_context",
    "rating_plus_form",
    "rating_plus_form_plus_goalscorers"
)

fair_comparison_features <- unique(unlist(feature_sets[main_feature_sets]))

feature_sets_tbl <- tibble::tibble(
    feature_set = names(feature_sets),
    n_features = vapply(feature_sets, length, integer(1)),
    in_fair_comparison_cohort = names(feature_sets) %in% main_feature_sets,
    features = vapply(
        feature_sets,
        function(feature_list) paste(feature_list, collapse = "; "),
        character(1)
    )
)

readr::write_csv(
    feature_sets_tbl,
    file.path(OUTPUT_DIR, "model_30_feature_sets.csv")
)

# 5. Excluded-column audit

excluded_columns_audit <- build_excluded_columns_audit(
    all_columns = names(modeling),
    allowed_modeling_columns = c(
        "source_match_id",
        "date",
        "data_split",
        "home_team",
        "away_team",
        "tournament",
        "tournament_lumped",
        fair_comparison_features
    ),
    selected_feature_union = fair_comparison_features
)

readr::write_csv(
    excluded_columns_audit,
    file.path(OUTPUT_DIR, "model_30_excluded_columns.csv")
)

# 6. Filtering and fair-comparison cohort

required_rating_features <- intersect(
    c("rating_diff", "home_rating_pre_match", "away_rating_pre_match"),
    names(modeling)
)

if (length(required_rating_features) == 0) {
    stop(
        "No required pre-match rating features found in modeling table.",
        call. = FALSE
    )
}

filter_counts <- tibble::tibble(
    step = character(),
    n_rows = integer()
)

df <- modeling

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "input_rows", n_rows = nrow(df))
)

df <- df |>
    dplyr::mutate(
        outcome = result_class_to_outcome(result_class),
        outcome = factor(outcome, levels = TARGET_LEVELS),
        date = as.Date(date)
    )

df <- df |>
    dplyr::filter(!is.na(outcome))

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(step = "valid_result_class", n_rows = nrow(df))
)

for (rating_feature in required_rating_features) {
    df <- df |>
        dplyr::filter(!is.na(.data[[rating_feature]]))
}

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(
        step = "required_pre_match_ratings_present",
        n_rows = nrow(df)
    )
)

if (include_tournament) {
    training_tournament_values <- df |>
        dplyr::filter(data_split == "train") |>
        dplyr::pull(tournament)

    df <- df |>
        dplyr::mutate(
            tournament_lumped = prepare_tournament_lumping(
                tournament,
                training_tournament_values
            )
        )
}

fair_comparison_features_for_na <- setdiff(
    fair_comparison_features,
    "tournament_lumped"
)

df <- df |>
    tidyr::drop_na(dplyr::all_of(fair_comparison_features_for_na))

filter_counts <- dplyr::bind_rows(
    filter_counts,
    tibble::tibble(
        step = "fair_comparison_complete_cases",
        n_rows = nrow(df)
    )
)

if (nrow(df) == 0) {
    stop("No rows remain after fair-comparison filtering.", call. = FALSE)
}

metadata_columns <- c(
    "source_match_id",
    "date",
    "data_split",
    "home_team",
    "away_team",
    "tournament",
    "outcome",
    fair_comparison_features
)

metadata_columns <- intersect(metadata_columns, names(df))

df <- df |>
    dplyr::select(dplyr::all_of(metadata_columns)) |>
    dplyr::mutate(
        dplyr::across(
            dplyr::where(is.logical),
            ~ as.integer(.x)
        )
    )

splits <- make_chronological_splits(df)
train <- splits$train
validation <- splits$validation
test <- splits$test

message("Train rows: ", nrow(train))
message("Validation rows: ", nrow(validation))
message("Test rows: ", nrow(test))

# 7. Model fitting loop (main feature sets)

all_metrics <- tibble::tibble()
all_classwise <- tibble::tibble()
all_confusions <- tibble::tibble()
all_predictions <- tibble::tibble()
all_feature_importance <- tibble::tibble()

for (feature_set_name in main_feature_sets) {
    selected_features <- feature_sets[[feature_set_name]]

    message("Processing feature set: ", feature_set_name)

    variant_results <- fit_variant_models(
        train = train,
        validation = validation,
        test = test,
        selected_features = selected_features,
        feature_set = feature_set_name
    )

    all_metrics <- dplyr::bind_rows(all_metrics, variant_results$metrics)
    all_classwise <- dplyr::bind_rows(all_classwise, variant_results$classwise)
    all_confusions <- dplyr::bind_rows(all_confusions, variant_results$confusions)
    all_predictions <- dplyr::bind_rows(all_predictions, variant_results$predictions)
    all_feature_importance <- dplyr::bind_rows(
        all_feature_importance,
        variant_results$feature_importance
    )
}

# 8. Optional sensitivity variant (avg_goal_minute)

sensitivity_metrics <- tibble::tibble()
sensitivity_notes <- character()

if ("rating_plus_form_plus_goalscorers_with_avg_minute" %in% names(feature_sets)) {
    sensitivity_features <- feature_sets$rating_plus_form_plus_goalscorers_with_avg_minute

    df_sensitivity <- modeling |>
        dplyr::mutate(
            outcome = factor(
                result_class_to_outcome(result_class),
                levels = TARGET_LEVELS
            ),
            date = as.Date(date)
        ) |>
        dplyr::filter(!is.na(outcome))

    for (rating_feature in required_rating_features) {
        df_sensitivity <- df_sensitivity |>
            dplyr::filter(!is.na(.data[[rating_feature]]))
    }

    if (include_tournament) {
        training_tournament_values <- df_sensitivity |>
            dplyr::filter(data_split == "train") |>
            dplyr::pull(tournament)

        df_sensitivity <- df_sensitivity |>
            dplyr::mutate(
                tournament_lumped = prepare_tournament_lumping(
                    tournament,
                    training_tournament_values
                )
            )
    }

    sensitivity_features_for_na <- setdiff(
        sensitivity_features,
        "tournament_lumped"
    )

    sensitivity_metadata <- intersect(
        c(
            "source_match_id",
            "date",
            "data_split",
            "home_team",
            "away_team",
            "tournament",
            "outcome",
            sensitivity_features
        ),
        names(df_sensitivity)
    )

    df_sensitivity <- df_sensitivity |>
        dplyr::select(dplyr::all_of(sensitivity_metadata)) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::where(is.logical),
                ~ as.integer(.x)
            )
        ) |>
        tidyr::drop_na(dplyr::all_of(sensitivity_features_for_na))

    filter_counts <- dplyr::bind_rows(
        filter_counts,
        tibble::tibble(
            step = "sensitivity_cohort_with_avg_goal_minute",
            n_rows = nrow(df_sensitivity)
        )
    )

    sensitivity_splits <- make_chronological_splits(df_sensitivity)

    sensitivity_results <- fit_variant_models(
        train = sensitivity_splits$train,
        validation = sensitivity_splits$validation,
        test = sensitivity_splits$test,
        selected_features = sensitivity_features,
        feature_set = "rating_plus_form_plus_goalscorers_with_avg_minute"
    )

    sensitivity_metrics <- sensitivity_results$metrics
    all_feature_importance <- dplyr::bind_rows(
        all_feature_importance,
        sensitivity_results$feature_importance
    )

    sensitivity_notes <- c(
        sensitivity_notes,
        paste0(
            "Optional sensitivity variant including avg_goal_minute_diff_365d ",
            "used ",
            nrow(df_sensitivity),
            " rows (vs ",
            nrow(df),
            " in main fair-comparison cohort)."
        )
    )
} else {
    sensitivity_notes <- c(
        sensitivity_notes,
        "avg_goal_minute_diff_365d was not available; sensitivity variant skipped."
    )
}

# 9. Calibration for best validation model

validation_ranking <- all_metrics |>
    dplyr::filter(split == "validation") |>
    dplyr::arrange(log_loss)

best_validation_model <- validation_ranking |>
    dplyr::slice_head(n = 1)

calibration_bins <- tibble::tibble()

if (nrow(best_validation_model) == 1) {
    best_feature_set <- best_validation_model$feature_set
    best_model_name <- best_validation_model$model_name
    best_selected_features <- feature_sets[[best_feature_set]]

    expanded_best <- expand_model_features(
        train,
        validation,
        test,
        best_selected_features
    )

    if (best_model_name == "multinom") {
        train_modeling <- train |>
            dplyr::select(outcome, dplyr::all_of(best_selected_features)) |>
            dplyr::mutate(outcome = factor(outcome, levels = TARGET_LEVELS))

        validation_modeling <- validation |>
            dplyr::select(outcome, dplyr::all_of(best_selected_features))

        test_modeling <- test |>
            dplyr::select(outcome, dplyr::all_of(best_selected_features))

        multinom_formula <- stats::as.formula(
            paste("outcome ~", paste(best_selected_features, collapse = " + "))
        )

        multinom_fit <- nnet::multinom(
            formula = multinom_formula,
            data = train_modeling,
            trace = FALSE,
            MaxNWts = 20000
        )

        val_probs <- predict(multinom_fit, newdata = validation_modeling, type = "probs") |>
            safe_rename_probability_columns()
        test_probs <- predict(multinom_fit, newdata = test_modeling, type = "probs") |>
            safe_rename_probability_columns()
    } else if (best_model_name == "glmnet_multinomial_ridge" && has_glmnet) {
        model_formula <- stats::as.formula(
            paste("outcome ~", paste(expanded_best$model_features, collapse = " + "))
        )
        x_train <- stats::model.matrix(
            model_formula,
            data = cbind(outcome = train$outcome, expanded_best$train)
        )[, -1, drop = FALSE]
        x_validation <- stats::model.matrix(
            model_formula,
            data = cbind(outcome = validation$outcome, expanded_best$validation)
        )[, -1, drop = FALSE]
        x_test <- stats::model.matrix(
            model_formula,
            data = cbind(outcome = test$outcome, expanded_best$test)
        )[, -1, drop = FALSE]

        glmnet_fit <- glmnet::cv.glmnet(
            x = x_train,
            y = factor(train$outcome, levels = TARGET_LEVELS),
            family = "multinomial",
            type.measure = "deviance",
            alpha = 0,
            nfolds = 5
        )

        val_probs <- as.data.frame(
            predict(glmnet_fit, newx = x_validation, s = "lambda.min", type = "response")[, , 1]
        ) |>
            safe_rename_probability_columns()
        test_probs <- as.data.frame(
            predict(glmnet_fit, newx = x_test, s = "lambda.min", type = "response")[, , 1]
        ) |>
            safe_rename_probability_columns()
    } else if (best_model_name == "lightgbm" && has_lightgbm) {
        label_map <- c("A" = 0, "D" = 1, "H" = 2)
        train_lgb <- train |>
            dplyr::mutate(label = unname(label_map[as.character(outcome)]))
        validation_lgb <- validation |>
            dplyr::mutate(label = unname(label_map[as.character(outcome)]))

        x_train_lgb <- as.matrix(expanded_best$train[, expanded_best$model_features, drop = FALSE])
        x_validation_lgb <- as.matrix(
            expanded_best$validation[, expanded_best$model_features, drop = FALSE]
        )
        x_test_lgb <- as.matrix(expanded_best$test[, expanded_best$model_features, drop = FALSE])

        dtrain <- lightgbm::lgb.Dataset(data = x_train_lgb, label = train_lgb$label)
        dvalidation <- lightgbm::lgb.Dataset(
            data = x_validation_lgb,
            label = validation_lgb$label
        )

        lgb_fit <- lightgbm::lgb.train(
            params = list(
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
            ),
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
        test_probs <- lightgbm_raw_probs_to_pred_columns(
            predict(lgb_fit, x_test_lgb),
            nrow(test)
        )
    }

    if (exists("val_probs") && exists("test_probs")) {
        calibration_bins <- dplyr::bind_rows(
            make_calibration_bins(
                validation$outcome,
                val_probs,
                best_model_name,
                best_feature_set,
                "validation"
            ),
            make_calibration_bins(
                test$outcome,
                test_probs,
                best_model_name,
                best_feature_set,
                "test"
            )
        )
    }
}

# 10. Goalscorer uplift comparison

best_by_feature_set <- all_metrics |>
    dplyr::group_by(feature_set, split) |>
    dplyr::slice_min(order_by = log_loss, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

form_reference <- best_by_feature_set |>
    dplyr::filter(feature_set == "rating_plus_form")

goalscorer_variant <- best_by_feature_set |>
    dplyr::filter(feature_set == "rating_plus_form_plus_goalscorers")

goalscorer_uplift <- dplyr::inner_join(
    form_reference |>
        dplyr::select(split, model_name, log_loss, brier_score, macro_f1),
    goalscorer_variant |>
        dplyr::select(
            split,
            model_name,
            log_loss,
            brier_score,
            macro_f1
        ),
    by = c("split", "model_name"),
    suffix = c("_form", "_goalscorers")
) |>
    dplyr::mutate(
        log_loss_improved = log_loss_goalscorers < log_loss_form,
        brier_improved = brier_score_goalscorers < brier_score_form,
        macro_f1_improved = macro_f1_goalscorers > macro_f1_form
    )

draw_f1_comparison <- all_classwise |>
    dplyr::filter(class == "D", feature_set %in% c("rating_plus_form", "rating_plus_form_plus_goalscorers")) |>
    dplyr::group_by(feature_set, model_name, split) |>
    dplyr::slice_max(order_by = f1, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
        id_cols = c(model_name, split),
        names_from = feature_set,
        values_from = f1,
        names_prefix = "draw_f1_"
    ) |>
    dplyr::mutate(
        draw_f1_improved = draw_f1_rating_plus_form_plus_goalscorers > draw_f1_rating_plus_form
    )

# 11. Write outputs

readr::write_csv(
    all_metrics,
    file.path(OUTPUT_DIR, "model_30_performance_summary.csv")
)

readr::write_csv(
    all_classwise,
    file.path(OUTPUT_DIR, "model_30_classwise_metrics.csv")
)

readr::write_csv(
    all_confusions,
    file.path(OUTPUT_DIR, "model_30_confusion_matrices.csv")
)

readr::write_csv(
    all_predictions,
    file.path(OUTPUT_DIR, "model_30_prediction_examples.csv")
)

if (nrow(all_feature_importance) > 0) {
    readr::write_csv(
        all_feature_importance,
        file.path(OUTPUT_DIR, "model_30_feature_importance.csv")
    )
} else {
    notes_feature_importance <- "Feature importance/coefficients were not available."
}

readr::write_csv(
    calibration_bins,
    file.path(OUTPUT_DIR, "model_30_calibration_bins.csv")
)

best_test_model <- all_metrics |>
    dplyr::filter(split == "test") |>
    dplyr::arrange(log_loss) |>
    dplyr::slice_head(n = 1)

skipped_features <- c(
    setdiff(baseline_rating_candidates, baseline_rating_features),
    setdiff(context_candidates, context_features),
    setdiff(form_candidates, form_features),
    setdiff(goalscorer_main_candidates, goalscorer_main_features),
    setdiff(goalscorer_optional_candidates, goalscorer_optional_features)
)

skipped_features_notes <- if (length(skipped_features) > 0) {
    paste(
        "Skipped unavailable or all-missing features:",
        paste(skipped_features, collapse = ", ")
    )
} else {
    "No candidate features were skipped."
}

tree_model_note <- if (has_lightgbm) {
    "LightGBM was used for tree-based models."
} else if (has_ranger) {
    "LightGBM unavailable; ranger random forest was used instead."
} else {
    "LightGBM and ranger unavailable; tree-based models were skipped."
}

rank_note <- if (all(c("home_rank_pre_match", "away_rank_pre_match") %in% skipped_features)) {
    "home_rank_pre_match and away_rank_pre_match were excluded (100% missing in input table)."
} else {
    "Rank features included where available."
}

tournament_note <- if (include_tournament) {
    paste0(
        "tournament encoded via training-set frequency lumping (min_count=",
        TOURNAMENT_LUMP_MIN_COUNT,
        ") into tournament_lumped; no target encoding used."
    )
} else {
    "tournament column not present; context flags only."
}

goalscorer_helped_validation <- any(
    goalscorer_uplift$split == "validation" &
        (goalscorer_uplift$log_loss_improved |
            goalscorer_uplift$brier_improved |
            goalscorer_uplift$macro_f1_improved),
    na.rm = TRUE
)

goalscorer_helped_test <- any(
    goalscorer_uplift$split == "test" &
        (goalscorer_uplift$log_loss_improved |
            goalscorer_uplift$brier_improved |
            goalscorer_uplift$macro_f1_improved),
    na.rm = TRUE
)

notes_lines <- c(
    "# Model 30 Notes",
    "",
    "## Data and filtering",
    paste0("- Input file: ", MODELING_PATH),
    paste0("- Input rows: ", filter_counts$n_rows[filter_counts$step == "input_rows"]),
    paste0(
        "- Fair-comparison cohort rows: ",
        filter_counts$n_rows[filter_counts$step == "fair_comparison_complete_cases"]
    ),
    "",
    "### Filter steps",
    paste0("- ", filter_counts$step, ": ", filter_counts$n_rows, collapse = "\n"),
    "",
    "## Feature sets",
    paste0(
        "- ",
        feature_sets_tbl$feature_set,
        " (n=",
        feature_sets_tbl$n_features,
        ", fair_comparison=",
        feature_sets_tbl$in_fair_comparison_cohort,
        ")"
    ),
    "",
    "## Skipped features",
    skipped_features_notes,
    rank_note,
    paste0(
        "- avg_goal_minute_diff_365d excluded from main fair-comparison union due to high missingness (~52%)."
    ),
    "",
    "## Modeling notes",
    tree_model_note,
    tournament_note,
    paste0("- glmnet available: ", has_glmnet),
    paste0("- Models trained per feature set: multinom", if (has_glmnet) ", glmnet_multinomial_ridge" else "", if (!is.na(tree_model_name)) paste0(", ", tree_model_name) else ""),
    "",
    "## Sensitivity analysis",
    sensitivity_notes,
    "",
    "## Best models",
    paste0(
        "- Best validation model (log loss): ",
        best_validation_model$feature_set,
        " / ",
        best_validation_model$model_name,
        " (log_loss=",
        round(best_validation_model$log_loss, 4),
        ")"
    ),
    paste0(
        "- Best test model (log loss): ",
        best_test_model$feature_set,
        " / ",
        best_test_model$model_name,
        " (log_loss=",
        round(best_test_model$log_loss, 4),
        ")"
    ),
    "",
    "## Goalscorer uplift vs rating_plus_form (best log loss model per split)",
    if (nrow(goalscorer_uplift) > 0) {
        paste0(
            "- ",
            goalscorer_uplift$split,
            ": log_loss improved=",
            goalscorer_uplift$log_loss_improved,
            ", brier improved=",
            goalscorer_uplift$brier_improved,
            ", macro_f1 improved=",
            goalscorer_uplift$macro_f1_improved
        )
    } else {
        "- Goalscorer uplift comparison unavailable."
    },
    paste0("- Goalscorer features helped on validation: ", goalscorer_helped_validation),
    paste0("- Goalscorer features helped on test: ", goalscorer_helped_test),
    "",
    "## Draw F1 comparison (best model per feature set)",
    if (nrow(draw_f1_comparison) > 0) {
        paste0(
            "- ",
            draw_f1_comparison$split,
            " / ",
            draw_f1_comparison$model_name,
            ": draw F1 form=",
            round(draw_f1_comparison$draw_f1_rating_plus_form, 4),
            ", goalscorers=",
            round(draw_f1_comparison$draw_f1_rating_plus_form_plus_goalscorers, 4),
            ", improved=",
            draw_f1_comparison$draw_f1_improved
        )
    } else {
        "- Draw F1 comparison unavailable."
    },
    "",
    paste0(
        "- Generated at: ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
)

writeLines(notes_lines, file.path(OUTPUT_DIR, "model_30_notes.md"))

# 12. Final summary

message("=================================================================")
message("Model 30 complete.")
message("")
message("Input rows: ", filter_counts$n_rows[filter_counts$step == "input_rows"])
message(
    "Fair-comparison rows: ",
    filter_counts$n_rows[filter_counts$step == "fair_comparison_complete_cases"]
)
message("")
message("Feature sets used:")
print(feature_sets_tbl)
message("")
message("Models trained: multinom", if (has_glmnet) ", glmnet_multinomial_ridge" else "", if (!is.na(tree_model_name)) paste0(", ", tree_model_name) else "")
message("")
message("Best validation model by log loss:")
print(best_validation_model)
message("")
message("Best test model by log loss:")
print(best_test_model)
message("")
message("Goalscorer uplift vs rating_plus_form:")
print(goalscorer_uplift)
message("")
message("Outputs written to: ", OUTPUT_DIR)
message("=================================================================")
