# 01_packages.R
#
# Installs missing CRAN packages and loads the shared library set.
# Run after 00_project_setup.R.

core_packages <- c(
    # Data manipulation
    "dplyr",
    "tidyr",
    "readr",
    "purrr",
    "stringr",
    "tibble",
    "data.table",

    # Dates and cleaning
    "lubridate",
    "janitor",

    # Files, paths, strings
    "fs",
    "glue",

    # JSON and APIs
    "jsonlite",
    "httr2",

    # SQL / local database
    "DBI",
    "duckdb",

    # Visualization
    "ggplot2",

    # Miscellaneous
    "countrycode",

    # Diagramsx
    "DiagrammeR"
)

modeling_packages <- c(
    "tidymodels",
    "xgboost",
    "ranger",
    "glmnet",
    "yardstick"
)

bayesian_packages <- c(
    "brms",
    "posterior",
    "bayesplot",
    "loo"
)

dashboard_packages <- c(
    "shiny",
    "bslib",
    "DT",
    "plotly"
)

dev_packages <- c(
    "devtools",
    "usethis",
    "testthat",
    "roxygen2"
)

# Start lean. Add other groups later when needed.
required_packages <- c(
    core_packages
    # modeling_packages,
    # bayesian_packages,
    # dashboard_packages,
    # dev_packages
)

install_if_missing <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

    if (length(missing) > 0) {
        install.packages(missing, repos = "https://cloud.r-project.org")
    }
}

install_if_missing(required_packages)

invisible(
    lapply(required_packages, library, character.only = TRUE)
)

message("Packages installed and loaded.")