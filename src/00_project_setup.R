# ============================================================
# 00_project_setup.R
# Defines project-root-relative paths and creates the standard
# folder layout used by all pipeline scripts.
#
# Inputs:  none (detects project root from working directory)
#
# Outputs: creates data/, reports/, models/, docs/, and subfolders
#
# Notes:   Sourced by nearly every pipeline script. Run R from the
#          project root, or from src/ — root detection walks upward.
# ============================================================

detect_project_root <- function(start_dir = getwd()) {
    current <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

    for (i in seq_len(20L)) {
        marker <- file.path(current, "src", "00_project_setup.R")
        if (file.exists(marker)) {
            return(current)
        }

        parent <- dirname(current)
        if (identical(parent, current)) {
            break
        }
        current <- parent
    }

    normalizePath(start_dir, winslash = "/", mustWork = FALSE)
}

PROJECT_ROOT <- detect_project_root()

if (!identical(normalizePath(getwd(), winslash = "/"), PROJECT_ROOT)) {
    setwd(PROJECT_ROOT)
}

DATA_DIR <- file.path(PROJECT_ROOT, "data")
RAW_DIR <- file.path(DATA_DIR, "raw")
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
MODELING_DATA_DIR <- file.path(DATA_DIR, "modeling")
PREDICTIONS_DIR <- file.path(DATA_DIR, "predictions")
META_DIR <- file.path(DATA_DIR, "metadata")
VALIDATION_DIR <- file.path(DATA_DIR, "validation")
VALIDATION_PROCESSED_DIR <- file.path(VALIDATION_DIR, "processed_data")
VALIDATION_ENGINEERED_DIR <- file.path(VALIDATION_DIR, "engineered_features")
VALIDATION_MODELING_DIR <- file.path(VALIDATION_DIR, "modeling")

REPORTS_DIR <- file.path(PROJECT_ROOT, "reports")
REPORTS_TABLES_DIR <- file.path(REPORTS_DIR, "tables")
REPORTS_FIGURES_DIR <- file.path(REPORTS_DIR, "figures")

MODELS_DIR <- file.path(PROJECT_ROOT, "models")
DOCS_DIR <- file.path(PROJECT_ROOT, "docs")
DB_DIR <- file.path(PROJECT_ROOT, "database")
NOTEBOOKS_DIR <- file.path(PROJECT_ROOT, "notebooks")
LOGS_DIR <- file.path(PROJECT_ROOT, "logs")

# Legacy alias — prefer REPORTS_FIGURES_DIR in new code
GRAPHS_DIR <- REPORTS_FIGURES_DIR

standard_dirs <- c(
    DATA_DIR,
    RAW_DIR,
    PROCESSED_DIR,
    MODELING_DATA_DIR,
    PREDICTIONS_DIR,
    META_DIR,
    VALIDATION_DIR,
    VALIDATION_PROCESSED_DIR,
    VALIDATION_ENGINEERED_DIR,
    VALIDATION_MODELING_DIR,
    REPORTS_DIR,
    REPORTS_TABLES_DIR,
    REPORTS_FIGURES_DIR,
    MODELS_DIR,
    DOCS_DIR,
    DB_DIR,
    NOTEBOOKS_DIR,
    LOGS_DIR,
    file.path(MODELS_DIR, "multinom"),
    file.path(MODELS_DIR, "ridge"),
    file.path(MODELS_DIR, "lightgbm"),
    file.path(REPORTS_DIR, "final")
)

invisible(lapply(
    standard_dirs,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
))

message("Project root: ", PROJECT_ROOT)
