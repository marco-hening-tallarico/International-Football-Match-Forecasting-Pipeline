# ============================================================
# 00_project_setup.R
# Create reproducible project folder structure
# ============================================================

PROJECT_ROOT <- getwd()

DATA_DIR      <- file.path(PROJECT_ROOT, "data")
RAW_DIR       <- file.path(DATA_DIR, "raw")
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
META_DIR      <- file.path(DATA_DIR, "metadata")
VALIDATION_DIR <- file.path(DATA_DIR, "validation")
DB_DIR        <- file.path(PROJECT_ROOT, "database")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(META_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(VALIDATION_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DB_DIR, recursive = TRUE, showWarnings = FALSE)

message("Project root: ", PROJECT_ROOT)
message("Data directory: ", DATA_DIR)
message("Raw directory: ", RAW_DIR)
message("Processed directory: ", PROCESSED_DIR)
message("Metadata directory: ", META_DIR)
message("Validation directory: ", VALIDATION_DIR)
message("Database directory: ", DB_DIR)
