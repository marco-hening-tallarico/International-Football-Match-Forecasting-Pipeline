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

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

model_path <- file.path(PROCESSED_DIR, "international_modeling_table.csv")
audit_path <- file.path(
    REPORTS_TABLES_DIR,
    "feature_audit_international_modeling_table.csv"
)

out_csv <- file.path(REPORTS_TABLES_DIR, "feature_review_all_columns.csv")
out_md <- file.path(REPORTS_DIR, "feature_review_international_modeling_table.md")
out_r <- file.path(REPORTS_TABLES_DIR, "approved_feature_sets.R")

model_df <- readr::read_csv(model_path, show_col_types = FALSE)

audit <- if (file.exists(audit_path)) {
    readr::read_csv(audit_path, show_col_types = FALSE)
} else {
    tibble::tibble(column = names(model_df))
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
    col_l <- stringr::str_to_lower(col)
    all_missing <- all(is.na(x))

    dplyr::case_when(
        all_missing ~ "exclude",

        col_l %in% c("home_win", "draw", "away_win") ~ "exclude",

        stringr::str_detect(col_l, "score|goals|goal_diff|winner|margin|shootout|penalt") ~ "exclude",

        col_l %in% c("date", "match_date") ~ "exclude_as_feature_keep_for_split",

        stringr::str_detect(col_l, "_rating_date$") ~ "exclude_as_feature_keep_for_audit",

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

        stringr::str_detect(col_l, "rating_pre_match|rating_diff|rating_age_days") ~ "keep",

        col_l %in% c("neutral") ~ "keep",

        stringr::str_detect(col_l, "^is_") ~ "keep",

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
    col_l <- stringr::str_to_lower(col)

    dplyr::case_when(
        decision == "exclude" & col_l %in% c("home_win", "draw", "away_win") ~
            "Direct target/result label. Cannot be used as a feature.",

        decision == "exclude" & stringr::str_detect(col_l, "shootout") ~
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

column_report <- tibble::tibble(column = names(model_df)) |>
    dplyr::mutate(
        r_class = purrr::map_chr(column, ~ paste(class(model_df[[.x]]), collapse = "/")),
        non_missing_n = purrr::map_int(column, ~ sum(!is.na(model_df[[.x]]))),
        missing_n = purrr::map_int(column, ~ sum(is.na(model_df[[.x]]))),
        missing_prop = missing_n / nrow(model_df),
        missing_percent = round(100 * missing_prop, 2),
        n_distinct = purrr::map_int(column, ~ dplyr::n_distinct(model_df[[.x]], na.rm = TRUE)),
        example_values = purrr::map_chr(column, ~ safe_examples(model_df[[.x]])),
        suggested_decision = purrr::map2_chr(column, column, ~ guess_decision(.x, model_df[[.x]])),
        reason = purrr::map2_chr(column, suggested_decision, guess_reason)
    )

review_table <- column_report |>
    dplyr::left_join(
        audit |> dplyr::select(dplyr::any_of(c("column", "feature_role", "notes"))),
        by = "column"
    ) |>
    dplyr::mutate(
        final_decision = suggested_decision,
        final_notes = reason
    ) |>
    dplyr::arrange(
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
        dplyr::desc(missing_percent),
        column
    )

readr::write_csv(review_table, out_csv)

# Feature vectors

safe_features <- review_table |>
    dplyr::filter(final_decision == "keep") |>
    dplyr::pull(column)

careful_features <- review_table |>
    dplyr::filter(final_decision == "use_with_care") |>
    dplyr::pull(column)

excluded_features <- review_table |>
    dplyr::filter(stringr::str_starts(final_decision, "exclude")) |>
    dplyr::pull(column)

review_features <- review_table |>
    dplyr::filter(final_decision == "review") |>
    dplyr::pull(column)

model_features <- c(safe_features, careful_features)

feature_set_lines <- c(
    "# reports/tables/approved_feature_sets.R",
    "# Generated by src/22_finalize_feature_review.R",
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
    dplyr::mutate(
        example_values = stringr::str_replace_all(example_values, "\\|", "/"),
        final_notes = stringr::str_replace_all(final_notes, "\\|", "/")
    ) |>
    dplyr::transmute(
        row = glue::glue(
            "| `{column}` | {r_class} | {missing_percent} | {n_distinct} | **{final_decision}** | {example_values} | {final_notes} |"
        )
    ) |>
    dplyr::pull(row)

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
print(review_table |> dplyr::count(final_decision, sort = TRUE))

cat("\nSafe features:\n")
print(safe_features)

cat("\nUse-with-care features:\n")
print(careful_features)

cat("\nStill requiring manual review:\n")
print(review_features)

review_path <- file.path(REPORTS_TABLES_DIR, "feature_review_all_columns.csv")

review_table <- readr::read_csv(review_path, show_col_types = FALSE)

review_table_final <- review_table |>
    dplyr::mutate(
        final_decision = dplyr::case_when(
            column %in% c("away_team", "home_team") ~ "exclude",
            column %in% c("data_split", "data_split_modeling") ~
                "exclude_as_feature_keep_for_split",
            column %in% c("match_result", "result_class") ~ "exclude",
            column == "source_match_id" ~ "exclude",
            TRUE ~ final_decision
        ),
        final_notes = dplyr::case_when(
            column %in% c("away_team", "home_team") ~
                "Raw team identifier. Excluded because cleaned team identifiers are already available.",
            column == "data_split" ~
                "Legacy train/test split (test from 2018-01-01). Keep for compatibility; prefer data_split_modeling for modeling.",
            column == "data_split_modeling" ~
                "Authoritative train/validation/test split for modeling. Never use as a model feature.",
            column %in% c("match_result", "result_class") ~
                "Target or target-derived result field. Direct leakage if used as a feature.",
            column == "source_match_id" ~
                "Source identifier only. Excluded to avoid memorization and because it has no pre-match signal.",
            TRUE ~ final_notes
        )
    )

readr::write_csv(
    review_table_final,
    file.path(REPORTS_TABLES_DIR, "feature_review_all_columns_final.csv")
)

safe_features <- review_table_final |>
    dplyr::filter(final_decision == "keep") |>
    dplyr::pull(column)

careful_features <- review_table_final |>
    dplyr::filter(final_decision == "use_with_care") |>
    dplyr::pull(column)

excluded_features <- review_table_final |>
    dplyr::filter(grepl("^exclude", final_decision)) |>
    dplyr::pull(column)

review_features <- review_table_final |>
    dplyr::filter(final_decision == "review") |>
    dplyr::pull(column)

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
    file.path(REPORTS_TABLES_DIR, "approved_feature_sets_final.R")
)

cat("\nFinal decision counts:\n")
print(review_table_final |> dplyr::count(final_decision, sort = TRUE))

cat("\nFinal model features:\n")
print(model_features)

cat("\nRemaining review features:\n")
print(review_features)
