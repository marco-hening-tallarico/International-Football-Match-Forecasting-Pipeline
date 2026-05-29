# ============================================================
# 20_feature_audit.R
# Column-level audit of the international modeling table
#
# Reads:
#   data/processed/international_modeling_table.csv
#
# Writes:
#   reports/tables/feature_audit_international_modeling_table.csv
# ============================================================


# -----------------------------
# 0. Setup
# -----------------------------

suppressPackageStartupMessages({
    library(tidyverse)
})

dir.create("reports/tables", recursive = TRUE, showWarnings = FALSE)

data_path <- "data/processed/international_modeling_table.csv"

if (!file.exists(data_path)) {
    stop("Data file not found: ", data_path, call. = FALSE)
}

modeling_table <- readr::read_csv(data_path, show_col_types = FALSE)


# -----------------------------
# 1. Classify feature roles
# -----------------------------

TARGET_COLUMNS <- "match_result"
SPLIT_COLUMNS <- "data_split"
DATE_COLUMNS <- "date"
IDENTIFIER_COLUMNS <- c(
    "home_team",
    "away_team",
    "source_match_id",
    "match_id",
    "source"
)
POST_MATCH_LEAKAGE_COLUMNS <- c(
    "home_score",
    "away_score",
    "goal_difference",
    "total_goals",
    "result_class",
    "shootout_winner",
    "home_won_shootout",
    "away_won_shootout"
)
ALLOWED_PRE_MATCH_COLUMNS <- c(
    "rating_diff",
    "neutral",
    "neutral_model",
    "home_rating",
    "away_rating"
)
ENGINEERED_PRE_MATCH_COLUMNS <- c(
    "abs_rating_diff",
    "rating_diff_sq"
)

classify_feature_role <- function(column_name) {
    if (column_name %in% TARGET_COLUMNS) {
        return("target")
    }

    if (column_name %in% SPLIT_COLUMNS) {
        return("split")
    }

    if (column_name %in% DATE_COLUMNS) {
        return("date")
    }

    if (column_name %in% IDENTIFIER_COLUMNS) {
        return("identifier")
    }

    if (column_name %in% POST_MATCH_LEAKAGE_COLUMNS) {
        return("post_match_leakage")
    }

    if (column_name %in% ALLOWED_PRE_MATCH_COLUMNS) {
        return("allowed_pre_match_feature")
    }

    if (column_name %in% ENGINEERED_PRE_MATCH_COLUMNS) {
        return("engineered_pre_match_feature")
    }

    "unknown_review_required"
}


role_notes <- function(column_name, feature_role) {
    if (feature_role == "post_match_leakage") {
        return("Known post-match outcome or score column; do not use as a predictor.")
    }

    if (feature_role == "target") {
        return("Multiclass outcome label (H / D / A).")
    }

    if (feature_role == "split") {
        return("Train / test assignment for held-out evaluation.")
    }

    if (feature_role == "unknown_review_required") {
        if (grepl("_pre_match$", column_name) || grepl("_rating", column_name)) {
            return(
                paste(
                    "Not auto-classified; name suggests a pre-match field.",
                    "Review before using in models."
                )
            )
        }

        if (grepl("^is_", column_name)) {
            return("Tournament or match-type flag; review for pre-match use.")
        }
    }

    NA_character_
}


format_example_values <- function(column_vector, max_examples = 5L) {
    non_missing_values <- column_vector[!is.na(column_vector)]

    if (length(non_missing_values) == 0L) {
        return(NA_character_)
    }

    if (is.numeric(column_vector) || is.logical(column_vector)) {
        unique_values <- unique(non_missing_values)
        example_values <- head(sort(unique_values), max_examples)
        paste(example_values, collapse = "; ")
    } else {
        unique_values <- unique(as.character(non_missing_values))
        example_values <- head(unique_values, max_examples)
        paste(example_values, collapse = "; ")
    }
}


# -----------------------------
# 2. Build audit table
# -----------------------------

feature_audit_table <- purrr::map_dfr(names(modeling_table), function(column_name) {
    column_vector <- modeling_table[[column_name]]
    missing_n <- sum(is.na(column_vector))
    non_missing_n <- nrow(modeling_table) - missing_n

    tibble(
        column = column_name,
        r_class = class(column_vector)[1],
        non_missing_n = non_missing_n,
        missing_n = missing_n,
        missing_prop = missing_n / nrow(modeling_table),
        n_distinct = dplyr::n_distinct(column_vector, na.rm = TRUE),
        example_values = format_example_values(column_vector),
        feature_role = classify_feature_role(column_name),
        notes = role_notes(column_name, classify_feature_role(column_name))
    )
})

output_path <- "reports/tables/feature_audit_international_modeling_table.csv"

readr::write_csv(feature_audit_table, output_path)


# -----------------------------
# 3. Summary
# -----------------------------

role_summary <- feature_audit_table %>%
    count(feature_role, name = "n_columns") %>%
    arrange(desc(n_columns))

cat("\n")
cat("============================================================\n")
cat("Feature audit complete\n")
cat("============================================================\n")
cat("Wrote: ", output_path, "\n", sep = "")
cat("Columns audited: ", nrow(feature_audit_table), "\n", sep = "")
cat("\nSummary count by feature_role:\n")
print(role_summary)
cat("============================================================\n")
