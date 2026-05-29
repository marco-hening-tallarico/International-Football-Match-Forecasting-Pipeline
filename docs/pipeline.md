# Pipeline Reference

Run all commands from the **project root**. Scripts are numbered in dependency order within each stage.

## Orchestrators

| Script | Purpose |
|--------|---------|
| `src/run_pipeline.R` | Full raw → processed rebuild (StatsBomb + club + international + inventory) |
| `src/run_light_pipeline.R` | International data only (recommended for routine work) |
| `src/run_statsbomb_pipeline.R` | StatsBomb download and clean only (heavy) |
| `src/run_club_pipeline.R` | football-data.co.uk only |
| `src/run_analysis_pipeline.R` | Legacy exploratory analysis on international results |
| `src/run_modeling_pipeline.R` | Feature review through final model reporting |

## Setup (always sourced first)

| Script | Purpose | Inputs | Outputs |
|--------|---------|--------|---------|
| `00_project_setup.R` | Paths, directories | — | Creates `data/`, `reports/`, `models/`, `docs/` |
| `01_packages.R` | Install/load CRAN packages | — | — |
| `02_helpers.R` | Shared IO and manifest helpers | — | — |

## International data (scripts 07–18)

| Script | Purpose | Must run after |
|--------|---------|----------------|
| `07_download_international_results.R` | Download martj42 CSVs | 00–02 |
| `08_clean_international_results.R` | Clean match results | 07 |
| `08b_clean_international_goalscorers.R` | Clean goalscorer events | 07 |
| `08c_clean_international_shootouts.R` | Clean shootout records | 07 |
| `14_join_international_shootouts_to_results.R` | Join shootouts onto results | 08, 08c |
| `09_validate_international_results.R` | Hard validation checks | 08 |
| `10_plot_international_results_validation.R` | EDA validation plots | 08 |
| `15_download_international_ratings.R` | Download World Football Elo | — |
| `16_clean_international_ratings.R` | Clean Elo ratings | 15 |
| `17_validate_international_ratings.R` | Ratings QA | 16 |
| `18_build_international_modeling_table.R` | Pre-match Elo + flags + split | 08, 16 |

**Processed outputs:** `international_results.csv`, `international_modeling_table.csv`, etc.  
**Validation outputs:** `data/validation/processed_data/international_*`

## Feature engineering (scripts 27, 29)

| Script | Purpose | Outputs |
|--------|---------|---------|
| `27_build_lagged_team_form_features.R` | Lagged form from prior matches | `international_modeling_table_with_form.csv` |
| `29_build_goalscorer_form_features.R` | Goalscorer depth features | `international_modeling_table_with_form_and_goalscorers.csv` |
| `30b_validate_engineered_features.R` | Schema, range, leakage checks | `data/validation/engineered_features/` |

## Modeling (scripts 19–32)

| Script | Purpose | Key outputs |
|--------|---------|-------------|
| `19_baseline.R` | Frequency, majority, Elo multinomial baselines | `reports/tables/baseline_*` |
| `20_feature_audit.R` | Column-level feature audit | `reports/tables/feature_audit_*` |
| `22_finalize_feature_review.R` | Approved feature sets | `reports/tables/approved_feature_sets*.R` |
| `23_feature_target_eda.R` | Target and feature EDA | `reports/tables/eda_*`, `reports/figures/eda_*` |
| `24_model_glm_lightgbm_approved_features.R` | Safe-feature ML models | `reports/tables/model_24_*` |
| `25_model_diagnostics_draws_calibration.R` | Draw-focused diagnostics | `reports/tables/model_25_*` |
| `26_model_draw_aware_features.R` | Draw-aware Elo transforms | `reports/tables/model_26_*` |
| `28_model_with_lagged_form.R` | Form-feature model comparison | `reports/tables/model_28_*` |
| `30_model_with_goalscorer_features.R` | Full tier comparison incl. goalscorers | `reports/tables/model_30/` |
| `31_final_results_visualization.R` | Cross-stage summary plots | `reports/final/`, `reports/figures/` |
| `32_finalize_international_modeling_project.R` | Final tables + report | `reports/tables/final_project/` |

## Optional / heavy pipelines

StatsBomb (`03`–`04g`), club football (`05`–`06`, `13`), metadata (`11_build_data_inventory.R`), global validation (`validation.R`).

## Typical workflows

```bash
# Data only
Rscript src/run_light_pipeline.R

# Data + modeling
Rscript src/run_light_pipeline.R
Rscript src/run_modeling_pipeline.R

# Rebuild modeling table only
Rscript src/18_build_international_modeling_table.R
Rscript src/27_build_lagged_team_form_features.R
Rscript src/29_build_goalscorer_form_features.R
```
