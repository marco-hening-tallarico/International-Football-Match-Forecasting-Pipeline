# ============================================================
# run_pipeline.R
# Run the full raw-to-processed data pipeline
# ============================================================

scripts <- c(
    "src/00_project_setup.R",
    "src/01_packages.R",
    "src/02_helpers.R",

    "src/03_download_statsbomb_competitions.R",
    "src/04_download_statsbomb_matches.R",
    "src/04b_download_statsbomb_events.R",
    "src/04e_clean_statsbomb_events.R",
    "src/04c_download_statsbomb_lineups.R",
    "src/04f_clean_statsbomb_lineups.R",
    "src/04d_download_statsbomb_360.R",
    "src/04g_clean_statsbomb_360.R",

    "src/05_download_football_data_uk.R",
    "src/06_clean_football_data_uk.R",

    "src/07_download_international_results.R",
    "src/08_clean_international_results.R",
    "src/08b_clean_international_goalscorers.R",
    "src/08c_clean_international_shootouts.R",

    "src/09_validate_international_results.R",
    "src/validation.R",

    "src/10_plot_international_results_validation.R",
    "src/11_international_results_analysis.R"
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
message("Pipeline completed successfully.")
message("============================================================")