# run_analysis_pipeline.R
#
# Post-processing analysis on international results after the light pipeline
# has produced data/processed/international_results.csv.
#
# Writes: reports/figures/international_results/* and legacy analysis tables

required_processed_path <- file.path(
    "data",
    "processed",
    "international_results.csv"
)

if (!file.exists(required_processed_path)) {
    stop(
        "Missing required processed file: ",
        required_processed_path,
        "\nRun src/run_light_pipeline.R or the international section of ",
        "src/run_pipeline.R first.",
        call. = FALSE
    )
}

scripts <- c(
    # Setup
    "src/00_project_setup.R",
    "src/01_packages.R",
    "src/02_helpers.R",

    # International results analysis / modeling
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
message("Analysis pipeline completed successfully.")
message("============================================================")
