# run_light_pipeline.R
#
# International match pipeline for day-to-day work: download, clean, join
# shootouts, validate, and plot. Optionally runs Elo ratings and the
# modeling-table build when those scripts are present.
#
# Writes: data/processed/international_* and data/validation/international_*

scripts <- c(
    # Setup
    "src/00_project_setup.R",
    "src/01_packages.R",
    "src/02_helpers.R",

    # International download and cleaning
    "src/07_download_international_results.R",
    "src/08_clean_international_results.R",
    "src/08b_clean_international_goalscorers.R",
    "src/08c_clean_international_shootouts.R",
    "src/14_join_international_shootouts_to_results.R",

    # International validation and plots
    "src/09_validate_international_results.R",
    "src/10_plot_international_results_validation.R"
)

ratings_scripts <- c(
    "src/15_download_international_ratings.R",
    "src/16_clean_international_ratings.R",
    "src/17_validate_international_ratings.R",
    "src/18_build_international_modeling_table.R"
)

ratings_raw_path <- file.path(
    "data",
    "raw",
    "international_ratings",
    "world_football_elo.csv"
)

for (script in scripts) {
    if (!file.exists(script)) {
        stop("Pipeline script not found: ", script, call. = FALSE)
    }

    message("============================================================")
    message("Running: ", script)
    message("============================================================")

    source(script, local = new.env(parent = globalenv()))
}

message("============================================================")
message("International ratings layer (optional)")
message("============================================================")

ratings_download_script <- ratings_scripts[[1L]]

if (!file.exists(ratings_download_script)) {
    stop("Pipeline script not found: ", ratings_download_script, call. = FALSE)
}

tryCatch(
    source(ratings_download_script, local = new.env(parent = globalenv())),
    error = function(error) {
        message(
            "Ratings download/ingest failed; skipping ratings clean/validate/modeling.\n",
            "Reason: ",
            conditionMessage(error)
        )
    }
)

if (file.exists(ratings_raw_path)) {
    for (script in ratings_scripts[-1L]) {
        if (!file.exists(script)) {
            stop("Pipeline script not found: ", script, call. = FALSE)
        }

        message("============================================================")
        message("Running: ", script)
        message("============================================================")

        source(script, local = new.env(parent = globalenv()))
    }
} else {
    message(
        "Ratings raw file not found at:\n",
        ratings_raw_path,
        "\nSkipping src/16-18. Run src/15_download_international_ratings.R ",
        "or place a manual World Football Elo CSV at that path."
    )
}

message("============================================================")
message("Light pipeline completed successfully.")
message("============================================================")
