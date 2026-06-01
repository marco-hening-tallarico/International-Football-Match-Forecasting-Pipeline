# run_modeling_pipeline.R
#
# International match-outcome modeling pipeline: feature review through final
# reporting. Requires processed tables from run_light_pipeline.R (through script
# 18 at minimum; scripts 27 and 29 for form and goalscorer features).
#
# Writes: reports/tables/, reports/figures/, data/predictions/, data/validation/

message("============================================================")
message("International modeling pipeline")
message("Ensure data/processed/international_modeling_table.csv exists.")
message("============================================================")

modeling_table_path <- file.path(
    "data",
    "processed",
    "international_modeling_table.csv"
)

if (!file.exists(modeling_table_path)) {
    stop(
        "Missing ", modeling_table_path,
        "\nRun src/run_light_pipeline.R first (through ratings/modeling table).",
        call. = FALSE
    )
}

scripts <- c(
    "src/19_baseline.R",
    "src/20_feature_audit.R",
    "src/22_finalize_feature_review.R",
    "src/23_feature_target_eda.R",
    "src/24_model_glm_lightgbm_approved_features.R",
    "src/25_model_diagnostics_draws_calibration.R",
    "src/26_model_draw_aware_features.R",
    "src/27_build_lagged_team_form_features.R",
    "src/28_model_with_lagged_form.R",
    "src/29_build_goalscorer_form_features.R",
    "src/30b_validate_engineered_features.R",
    "src/30_model_with_goalscorer_features.R",
    "src/31_final_results_visualization.R",
    "src/32_finalize_international_modeling_project.R"
    # Optional — controlled hyperparameter sensitivity (not required for Model 28 final):
    # "src/33_model_hyperparameter_sensitivity.R"
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
message("Modeling pipeline completed successfully.")
message("See reports/final/final_results_summary.md for the headline results.")
message("============================================================")
