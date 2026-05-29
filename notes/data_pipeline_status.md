# Data pipeline status

This project builds reproducible processed tables from three main sources:
StatsBomb Open Data, football-data.co.uk club results, and
martj42/international_results. Predictive models and advanced feature
engineering are intentionally out of scope for the core data runners.

## Pipeline runners

| Runner | Purpose |
|--------|---------|
| `src/run_pipeline.R` | Full raw-to-processed rebuild (all sources + inventory + `validation.R`) |
| `src/run_light_pipeline.R` | International data only — use for routine development |
| `src/run_statsbomb_pipeline.R` | StatsBomb download and cleaning only (heavy) |
| `src/run_club_pipeline.R` | football-data.co.uk download, clean, modeling table |
| `src/run_analysis_pipeline.R` | Post-processed analysis/modeling (not core cleaning) |

Run from the project root, for example:

```bash
Rscript src/run_light_pipeline.R
```

## Full pipeline (`run_pipeline.R`)

Sections, in order:

1. **Setup** — project paths, packages, helpers
2. **StatsBomb** — competitions, matches, events, lineups, 360 (download + clean)
3. **Club football** — football-data.co.uk raw download, clean, modeling table
4. **International** — download, clean results/goalscorers/shootouts, join shootouts, validate, plots
5. **Metadata and validation** — `11_build_data_inventory.R`, `validation.R`

Does **not** run `11_international_results_analysis.R` (that fits models; use `run_analysis_pipeline.R`).

Ends with: `Full data pipeline completed successfully.`

## Light pipeline (`run_light_pipeline.R`)

International path only: download → clean results, goalscorers, shootouts → join shootouts onto results → international validation scripts and plots → optional World Football Elo ratings layer (when raw ratings are available).

Use this for routine international forecasting work. It avoids StatsBomb event/360 processing (hours, high memory) and club downloads.

The ratings block (`src/15`–`src/18`) runs after the shootout join. Script `15` attempts an automatic download from [eloratings.net](http://www.eloratings.net/) (per-team match-history TSV files). If download fails or the raw file is missing, scripts `16`–`18` are skipped with a clear message so the rest of the light pipeline still completes.

Ends with: `Light pipeline completed successfully.`

## StatsBomb pipeline (`run_statsbomb_pipeline.R`)

StatsBomb-only heavy steps: competitions through 360 clean.

**Why it is heavy:** `04e_clean_statsbomb_events.R` flattens every event JSON into multi-gigabyte CSVs. `04g_clean_statsbomb_360.R` processes freeze-frame JSON and can run for hours or fail on memory. **360 download** only succeeds for a subset of matches (EURO 2020, World Cup 2022, etc.); missing 360 files are expected for most matches.

Ends with: `StatsBomb pipeline completed successfully.`

## Club pipeline (`run_club_pipeline.R`)

football-data.co.uk: download raw season files, build core + odds-wide tables, build `football_data_uk_modeling_table.csv`.

Ends with: `Club pipeline completed successfully.`

## Analysis pipeline (`run_analysis_pipeline.R`)

Runs `11_international_results_analysis.R` after checking that
`data/processed/international_results.csv` exists. That script is
**analysis/modeling** (multinomial baseline, leakage-aware features, plots under `outputs/`), not part of the core cleaning pipeline.

Ends with: `Analysis pipeline completed successfully.`

## Expected processed outputs

| File | Source |
|------|--------|
| `statsbomb_competitions.csv` | StatsBomb |
| `statsbomb_matches.csv` | StatsBomb |
| `statsbomb_events.csv` | StatsBomb (heavy) |
| `statsbomb_shots.csv` | StatsBomb (heavy) |
| `statsbomb_lineups.csv` | StatsBomb (heavy) |
| `statsbomb_360.csv` | StatsBomb (heavy; partial raw coverage) |
| `football_data_uk_matches.csv` | football-data.co.uk |
| `football_data_uk_matches_core.csv` | football-data.co.uk |
| `football_data_uk_odds_wide.csv` | football-data.co.uk |
| `football_data_uk_modeling_table.csv` | football-data.co.uk |
| `international_results.csv` | martj42 |
| `international_goalscorers.csv` | martj42 |
| `international_shootouts.csv` | martj42 |
| `international_results_with_shootouts.csv` | join script `14_` |
| `international_team_ratings.csv` | World Football Elo (`15_`–`16_`) |
| `international_modeling_table.csv` | pre-match Elo join (`18_`) |

## International ratings layer

| Item | Detail |
|------|--------|
| Source | World Football Elo ([eloratings.net](http://www.eloratings.net/)) |
| Ingest | **Automatic** via `src/15_download_international_ratings.R` (downloads per-team histories and writes one raw CSV). **Manual fallback:** place `world_football_elo.csv` under `data/raw/international_ratings/`. |
| Raw file | `data/raw/international_ratings/world_football_elo.csv` |
| Processed ratings | `data/processed/international_team_ratings.csv` |
| Modeling table | `data/processed/international_modeling_table.csv` |
| Team crosswalk | `data/metadata/team_name_crosswalk.csv` |

Rebuild only the modeling table (after matches and ratings exist):

```bash
Rscript src/18_build_international_modeling_table.R
```

Full ratings path from raw:

```bash
Rscript src/15_download_international_ratings.R
Rscript src/16_clean_international_ratings.R
Rscript src/17_validate_international_ratings.R
Rscript src/18_build_international_modeling_table.R
```

**Note:** `international_modeling_table.csv` is a feature-ready match table (pre-match Elo, tournament flags, train/test split). It is **not** a trained predictive model.

## Validation (`validation.R`)

Hard checks on required processed tables, raw coverage, manifest, and
international shootout join integrity. When ratings raw/processed files exist, also checks `international_team_ratings.csv` and `international_modeling_table.csv` (unique `source_match_id`, `data_split`, `rating_diff` arithmetic, row count vs `international_results_with_shootouts.csv`). StatsBomb **heavy** files
(events, shots, lineups, 360) are optional when missing: validation
prints a note and skips extended StatsBomb row checks instead of
treating absence as a data bug (typical after `run_light_pipeline.R` only).

If heavy files exist but fail row/integrity checks, that is a true validation failure.

## Metadata inventory (`11_build_data_inventory.R`)

Writes:

- `data/metadata/data_inventory.csv`
- `data/validation/source_coverage_summary.csv`

Scans `data/raw`, `data/processed`, `data/metadata`, and `data/validation`.

## Script categories (audit)

| Category | Scripts |
|----------|---------|
| Setup | `00_project_setup.R`, `01_packages.R`, `02_helpers.R` |
| StatsBomb download | `03_`, `04_`, `04b_`, `04c_`, `04d_` |
| StatsBomb clean (heavy) | `04e_`, `04f_`, `04g_` |
| Club download | `05_download_football_data_uk.R` |
| Club clean / modeling table | `06_clean_football_data_uk.R`, `13_build_football_data_modeling_table.R` |
| International download | `07_download_international_results.R` |
| International clean (light) | `08_`, `08b_`, `08c_`, `14_join_international_shootouts_to_results.R` |
| International ratings / modeling table | `15_`–`18_` (World Football Elo) |
| International validation | `09_validate_international_results.R`, `10_plot_international_results_validation.R`, `17_validate_international_ratings.R` |
| Metadata | `11_build_data_inventory.R` |
| Global validation | `validation.R` |
| Analysis / modeling | `11_international_results_analysis.R` |
| Pipeline runners | `run_pipeline.R`, `run_light_pipeline.R`, `run_statsbomb_pipeline.R`, `run_club_pipeline.R`, `run_analysis_pipeline.R` |
