# 05_download_football_data_uk.R
#
# Downloads season CSV files from football-data.co.uk into data/raw.
# Does not clean or reshape; see 06_clean_football_data_uk.R.
#
# Writes:
# - data/raw/football_data_uk/*.csv
# - data/metadata/source_manifest.csv (updated)

source("src/00_project_setup.R")
source("src/01_packages.R")
source("src/02_helpers.R")

source_manifest <- read_or_create_manifest(META_DIR)

fd_uk_dir <- file.path(RAW_DIR, "football_data_uk")
dir.create(fd_uk_dir, recursive = TRUE, showWarnings = FALSE)

league_codes <- c(
    # England
    "E0", "E1", "E2", "E3",

    # Spain
    "SP1", "SP2",

    # Germany
    "D1", "D2",

    # Italy
    "I1", "I2",

    # France
    "F1", "F2",

    # Netherlands, Portugal, Scotland
    "N1", "P1", "SC0"
)

season_codes <- c(
    "2324", "2223", "2122", "2021", "1920",
    "1819", "1718", "1617", "1516", "1415",
    "1314", "1213", "1112", "1011", "0910",
    "0809", "0708", "0607", "0506", "0405",
    "0304", "0203", "0102", "0001", "9900",
    "9899", "9798", "9697", "9596", "9495",
    "9394"
)

download_grid <- tidyr::crossing(
    season_code = season_codes,
    league_code = league_codes
) |>
    dplyr::mutate(
        url = glue::glue(
            "https://www.football-data.co.uk/mmz4281/{season_code}/{league_code}.csv"
        ),
        local_path = file.path(
            fd_uk_dir,
            season_code,
            glue::glue("{league_code}.csv")
        )
    )

download_one_fd_uk_file <- function(season_code, league_code, url, local_path) {
    ok <- safe_download(
        url = url,
        destfile = local_path,
        overwrite = FALSE
    )

    if (!isTRUE(ok)) {
        return(tibble::tibble())
    }

    if (!file.exists(local_path)) {
        return(tibble::tibble())
    }

    file_size <- file.info(local_path)$size

    if (is.na(file_size) || file_size == 0) {
        file.remove(local_path)
        return(tibble::tibble())
    }

    add_manifest_record(
        source = "football-data.co.uk",
        dataset = glue::glue("{league_code}_{season_code}"),
        url = url,
        local_path = local_path,
        notes = "Historical football results, match statistics, and betting odds CSV"
    )
}

fd_uk_manifest <- purrr::pmap_dfr(
    download_grid,
    download_one_fd_uk_file
)

source_manifest <- dplyr::bind_rows(
    source_manifest,
    fd_uk_manifest
)

write_source_manifest(
    records = source_manifest,
    meta_dir = META_DIR
)

downloaded_files <- fs::dir_ls(
    fd_uk_dir,
    recurse = TRUE,
    glob = "*.csv",
    fail = FALSE
)

message("Done.")
message("football-data.co.uk CSV files downloaded or found: ", length(downloaded_files))
message("Raw directory: ", fd_uk_dir)