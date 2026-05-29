# run_statsbomb_pipeline.R
#
# StatsBomb Open Data only: competitions, matches, events, lineups, and 360
# freeze frames. Event cleaning and 360 flattening can take many hours.
#
# Writes: data/processed/statsbomb_* and data/raw/statsbomb_open/*

message("============================================================")
message("StatsBomb pipeline: HEAVY")
message("Event cleaning and 360 processing can take many hours")
message("and may fail due to memory or runtime limits.")
message("============================================================")

scripts <- c(
    # Setup
    "src/00_project_setup.R",
    "src/01_packages.R",
    "src/02_helpers.R",

    # StatsBomb download and cleaning
    "src/03_download_statsbomb_competitions.R",
    "src/04_download_statsbomb_matches.R",
    "src/04b_download_statsbomb_events.R",
    "src/04e_clean_statsbomb_events.R",
    "src/04c_download_statsbomb_lineups.R",
    "src/04f_clean_statsbomb_lineups.R",
    "src/04d_download_statsbomb_360.R",
    "src/04g_clean_statsbomb_360.R"
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
message("StatsBomb pipeline completed successfully.")
message("============================================================")
