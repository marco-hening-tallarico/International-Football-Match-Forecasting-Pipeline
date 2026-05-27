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
    "src/05_download_football_data_uk.R",
    "src/06_clean_football_data_uk.R",
    "src/07_download_international_results.R",
    "src/08_clean_international_results.R",
    "src/validation.R"
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
