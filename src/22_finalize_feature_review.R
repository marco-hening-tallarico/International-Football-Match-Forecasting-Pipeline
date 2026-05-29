# 22_finalize_feature_review.R
#
# Combines the feature audit with manual review rules and writes approved
# feature lists for downstream EDA and modeling scripts.
#
# Reads:
# - data/processed/international_modeling_table.csv
# - reports/tables/feature_audit_international_modeling_table.csv (if present)
#
# Writes:
# - reports/tables/feature_review_all_columns.csv
# - reports/feature_review_international_modeling_table.md
# - reports/tables/approved_feature_sets.R

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
    library(glue)
})

model_path <- "data/processed/international_modeling_table.csv"
audit_path <- "reports/tables/feature_audit_international_modeling_table.csv"

out_csv <- "reports/tables/feature_review_all_columns.csv"
out_md  <- "reports/feature_review_international_modeling_table.md"
out_r   <- "reports/tables/approved_feature_sets.R"

dir.create("reports/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("reports", recursive = TRUE, showWarnings = FALSE)

model_df <- read_csv(model_path, show_col_types = FALSE)

audit <- if (file.exists(audit_path)) {
    read_csv(audit_path, show_col_types = FALSE)
} else {
    tibble(column = names(model_df))
}

# Helpers

safe_examples <- function(x, n = 6) {
    vals <- x[!is.na(x)]
    vals <- unique(vals)
    vals <- head(vals, n)

    if (length(vals) == 0) {
        return("ALL_NA")
    }

    paste(as.character(vals), collapse = " | ")
}

guess_decision <- function(col, x) {
    col_l <- str_to_lower(col)
    all_missing <- all(is.na(x))

    case_when(
        all_missing ~ "exclude",

        col_l %in% c("home_win", "draw", "away_win") ~ "exclude",

        str_detect(col_l, "score|goals|goal_diff|winner|margin|shootout|penalt") ~ "exclude",

        col_l %in% c("date", "match_date") ~ "exclude_as_feature_keep_for_split",

        str_detect(col_l, "_rating_date$") ~ "exclude_as_feature_keep_for_audit",

        col_l %in% c(
            "home_team_clean",
            "away_team_clean",
            "city",
            "country"
        ) ~ "use_with_care",

        col_l %in% c(
            "season",
            "competition",
            "tournament"
        ) ~ "use_with_care",

        str_detect(col_l, "rating_pre_match|rating_diff|rating_age_days") ~ "keep",

        col_l %in% c("neutral") ~ "keep",

        str_detect(col_l, "^is_") ~ "keep",

        TRUE ~ "review"
    )
}

format_r_character_vector_lines <- function(object_name, values) {
    if (length(values) == 0) {
        return(paste0(object_name, " <- character(0)"))
    }

    c(
        paste0(object_name, " <- c("),
        paste0('  "', values, '"', collapse = ",\n"),
        ")"
    )
}

guess_reason <- function(col, decision) {
    col_l <- str_to_lower(col)

    case_when(
        decision == "exclude" & col_l %in% c("home_win", "draw", "away_win") ~
            "Direct target/result label. Cannot be used as a feature.",

        decision == "exclude" & str_detect(col_l, "shootout") ~
            "Post-match field. Not known before kickoff.",

        decision == "exclude" ~
            "Excluded due to missingness, target leakage risk, or unusable current form.",

        decision == "exclude_as_feature_keep_for_split" ~
            "Useful for chronological splitting/backtesting, but not as a direct model feature.",

        decision == "exclude_as_feature_keep_for_audit" ~
            "Useful for validating rating freshness, but not used directly as a model feature.",

        decision == "use_with_care" & col_l %in% c("season") ~
            "Known pre-match, but may create era/time shortcuts. Allowed only with chronological validation.",

        decision == "use_with_care" & col_l %in% c("competition", "tournament") ~
            "Known pre-match competition context. May need rare-level grouping and careful validation.",

        decision == "use_with_care" & col_l %in% c("home_team_clean", "away_team_clean") ~
            "Known pre-match team identifier. Can improve fit but may overfit sparse teams and eras.",

        decision == "use_with_care" & col_l %in% c("city", "country") ~
            "Known pre-match location metadata. Prefer engineered location features, but allowed with care.",

        decision == "keep" ~
            "Allowed pre-match feature.",

        TRUE ~
            "Needs manual review."
    )
}

# Column inspection table

column_report <- tibble(column = names(model_df)) |>
    mutate(
        r_class = map_chr(column, ~ paste(class(model_df[[.x]]), collapse = "/")),
        non_missing_n = map_int(column, ~ sum(!is.na(model_df[[.x]]))),
        missing_n = map_int(column, ~ sum(is.na(model_df[[.x]]))),
        missing_prop = missing_n / nrow(model_df),
        missing_percent = round(100 * missing_prop, 2),
        n_distinct = map_int(column, ~ n_distinct(model_df[[.x]], na.rm = TRUE)),
        example_values = map_chr(column, ~ safe_examples(model_df[[.x]])),
        suggested_decision = map2_chr(column, column, ~ guess_decision(.x, model_df[[.x]])),
        reason = map2_chr(column, suggested_decision, guess_reason)
    )

review_table <- column_report |>
    left_join(
        audit |> select(any_of(c("column", "feature_role", "notes"))),
        by = "column"
    ) |>
    mutate(
        final_decision = suggested_decision,
        final_notes = reason
    ) |>
    arrange(
        factor(
            final_decision,
            levels = c(
                "keep",
                "use_with_care",
                "exclude_as_feature_keep_for_split",
                "exclude_as_feature_keep_for_audit",
                "exclude",
                "review"
            )
        ),
        desc(missing_percent),
        column
    )

write_csv(review_table, out_csv)

# Feature vectors

safe_features <- review_table |>
    filter(final_decision == "keep") |>
    pull(column)

careful_features <- review_table |>
    filter(final_decision == "use_with_care") |>
    pull(column)

excluded_features <- review_table |>
    filter(str_starts(final_decision, "exclude")) |>
    pull(column)

review_features <- review_table |>
    filter(final_decision == "review") |>
    pull(column)

model_features <- c(safe_features, careful_features)

feature_set_lines <- c(
    "# reports/tables/approved_feature_sets.R",
    "# Generated by src/22_feature_review_helper.R",
    "",
    format_r_character_vector_lines("safe_features", safe_features),
    "",
    format_r_character_vector_lines("careful_features", careful_features),
    "",
    "model_features <- c(safe_features, careful_features)",
    "",
    format_r_character_vector_lines("excluded_features", excluded_features),
    "",
    format_r_character_vector_lines("review_features", review_features)
)

writeLines(feature_set_lines, out_r)

# Markdown review doc

md_header <- c(
    "# Feature Review: international_modeling_table.csv",
    "",
    "Prediction task: pre-match multiclass international football outcome model.",
    "",
    "Target: home win / draw / away win.",
    "",
    "Rule: model features must be known before kickoff. Direct result labels and post-match fields are excluded.",
    "",
    "Features marked `use_with_care` are allowed, but they require strict chronological validation and should be monitored for overfitting.",
    "",
    "## Feature decision table",
    "",
    "| Column | Class | Missing % | Distinct | Decision | Examples | Notes |",
    "|---|---:|---:|---:|---|---|---|"
)

md_rows <- review_table |>
    mutate(
        example_values = str_replace_all(example_values, "\\|", "/"),
        final_notes = str_replace_all(final_notes, "\\|", "/")
    ) |>
    transmute(
        row = glue(
            "| `{column}` | {r_class} | {missing_percent} | {n_distinct} | **{final_decision}** | {example_values} | {final_notes} |"
        )
    ) |>
    pull(row)

md_sets <- c(
    "",
    "## Approved feature sets",
    "",
    "### Safe features",
    "",
    "```r",
    "safe_features <- c(",
    paste0('  "', safe_features, '"', collapse = ",\n"),
    ")",
    "```",
    "",
    "### Use-with-care features",
    "",
    "```r",
    "careful_features <- c(",
    paste0('  "', careful_features, '"', collapse = ",\n"),
    ")",
    "```",
    "",
    "### Modeling feature set",
    "",
    "```r",
    "model_features <- c(safe_features, careful_features)",
    "```",
    "",
    "### Excluded features",
    "",
    "```r",
    "excluded_features <- c(",
    paste0('  "', excluded_features, '"', collapse = ",\n"),
    ")",
    "```",
    "",
    "## Required leakage checks",
    "",
    "Before using rating features, verify rating dates are never after the match date.",
    "",
    "```r",
    "df |> summarise(any_future_home_rating = any(home_rating_date > date, na.rm = TRUE))",
    "df |> summarise(any_future_away_rating = any(away_rating_date > date, na.rm = TRUE))",
    "```"
)

writeLines(c(md_header, md_rows, md_sets), out_md)

# Console output

cat("\nWrote feature review table:\n")
cat(out_csv, "\n")

cat("\nWrote markdown review document:\n")
cat(out_md, "\n")

cat("\nWrote approved feature vectors:\n")
cat(out_r, "\n")

cat("\nDecision counts:\n")
print(review_table |> count(final_decision, sort = TRUE))

cat("\nSafe features:\n")
print(safe_features)

cat("\nUse-with-care features:\n")
print(careful_features)

cat("\nStill requiring manual review:\n")
print(review_features)


library(readr)
library(dplyr)

review_path <- "reports/tables/feature_review_all_columns.csv"

review_table <- read_csv(review_path, show_col_types = FALSE)

review_table_final <- review_table |>
    mutate(
        final_decision = case_when(
            column %in% c("away_team", "home_team") ~ "exclude",
            column == "data_split" ~ "exclude_as_feature_keep_for_split",
            column %in% c("match_result", "result_class") ~ "exclude",
            column == "source_match_id" ~ "exclude",
            TRUE ~ final_decision
        ),
        final_notes = case_when(
            column %in% c("away_team", "home_team") ~
                "Raw team identifier. Excluded because cleaned team identifiers are already available.",
            column == "data_split" ~
                "Split/control column. Keep for validation workflow only; never use as a model feature.",
            column %in% c("match_result", "result_class") ~
                "Target or target-derived result field. Direct leakage if used as a feature.",
            column == "source_match_id" ~
                "Source identifier only. Excluded to avoid memorization and because it has no pre-match signal.",
            TRUE ~ final_notes
        )
    )

write_csv(
    review_table_final,
    "reports/tables/feature_review_all_columns_final.csv"
)

safe_features <- review_table_final |>
    filter(final_decision == "keep") |>
    pull(column)

careful_features <- review_table_final |>
    filter(final_decision == "use_with_care") |>
    pull(column)

excluded_features <- review_table_final |>
    filter(grepl("^exclude", final_decision)) |>
    pull(column)

review_features <- review_table_final |>
    filter(final_decision == "review") |>
    pull(column)

model_features <- c(safe_features, careful_features)

feature_set_lines <- c(
    "# reports/tables/approved_feature_sets_final.R",
    "# Final approved feature sets after manual review",
    "",
    format_r_character_vector_lines("safe_features", safe_features),
    "",
    format_r_character_vector_lines("careful_features", careful_features),
    "",
    "model_features <- c(safe_features, careful_features)",
    "",
    format_r_character_vector_lines("excluded_features", excluded_features),
    "",
    format_r_character_vector_lines("review_features", review_features)
)

writeLines(
    feature_set_lines,
    "reports/tables/approved_feature_sets_final.R"
)

cat("\nFinal decision counts:\n")
print(review_table_final |> count(final_decision, sort = TRUE))

cat("\nFinal model features:\n")
print(model_features)

cat("\nRemaining review features:\n")
print(review_features)
