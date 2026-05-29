# run_pipeline.R
#
# Runs the full raw-to-processed pipeline: StatsBomb, club football, and
# international data, plus inventory and validation. For routine international
# work, use run_light_pipeline.R instead.
#
# Reads: src/*.R scripts listed below (no single input table)
#
# Writes: processed tables and validation outputs from each sourced script

message("============================================================")
message("Full data pipeline: includes heavy StatsBomb steps")
message("For routine international work use src/run_light_pipeline.R")
message("============================================================")

scripts <- c(
    # Setup
    "src/00_project_setup.R",
    "src/01_packages.R",
    "src/02_helpers.R",

    # StatsBomb
    "src/03_download_statsbomb_competitions.R",
    "src/04_download_statsbomb_matches.R",
    "src/04b_download_statsbomb_events.R",
    "src/04e_clean_statsbomb_events.R",
    "src/04c_download_statsbomb_lineups.R",
    "src/04f_clean_statsbomb_lineups.R",
    "src/04d_download_statsbomb_360.R",
    "src/04g_clean_statsbomb_360.R",

    # Club football
    "src/05_download_football_data_uk.R",
    "src/06_clean_football_data_uk.R",
    "src/13_build_football_data_modeling_table.R",

    # International
    "src/07_download_international_results.R",
    "src/08_clean_international_results.R",
    "src/08b_clean_international_goalscorers.R",
    "src/08c_clean_international_shootouts.R",
    "src/14_join_international_shootouts_to_results.R",
    "src/09_validate_international_results.R",
    "src/10_plot_international_results_validation.R",

    # Metadata and validation
    "src/11_build_data_inventory.R",
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
message("Full data pipeline completed successfully.")
message("============================================================")
