# run_club_pipeline.R
#
# Downloads, cleans, and joins football-data.co.uk club match files into a
# modeling table with wide odds columns kept separate from core match fields.
#
# Writes:
# - data/processed/football_data_uk_*.csv
# - data/validation/football_data_uk_*.csv

scripts <- c(
    # Setup
    "src/00_project_setup.R",
    "src/01_packages.R",
    "src/02_helpers.R",

    # Club download, cleaning, and modeling table
    "src/05_download_football_data_uk.R",
    "src/06_clean_football_data_uk.R",
    "src/13_build_football_data_modeling_table.R"
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
message("Club pipeline completed successfully.")
message("============================================================")
