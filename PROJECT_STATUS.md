# Project Status

Last updated: 2026-05-29

## Status table

| Area | Status | Notes |
|------|--------|-------|
| Data acquisition | **Done** | International results, goalscorers, shootouts, Elo ratings |
| Cleaning | **Done** | Normalized tables in `data/processed/` |
| Validation | **Done** | Hard checks + engineered-feature leakage audits |
| Feature engineering | **Done** | Elo join, lagged form, goalscorer depth |
| Modeling | **Done** | Baselines through Model 30 (LightGBM + glmnet + multinom) |
| Evaluation | **Done** | Chronological splits, calibration, classwise metrics |
| Visualization | **Done** | Incremental comparison plots + final model diagnostics |
| Final report | **Done** | `reports/final/final_results_summary.md`, `MODEL_CARD.md` |

## What is done

- End-to-end pipeline from raw international data to model comparison tables.
- Leakage-safe lagged form and goalscorer features with manual recompute checks.
- Incremental model comparison: baseline Elo → safe features → draw-aware Elo → lagged form → goalscorers.
- Final selected model (Model 28 tier): **LightGBM** with `safe_plus_form_compact` features.
- Portfolio documentation: README, pipeline reference, leakage audit, model card.

## What remains (optional improvements)

- [ ] Add `renv.lock` for fully pinned reproducibility from a fresh clone.
- [ ] Add `notebooks/notebook_4_final_project_report.Rmd` as a single reviewer-facing HTML report.
- [ ] Trim exploratory figures to 5–8 headline plots in `reports/figures/final_model/`.
- [ ] Wire best model into a World Cup tournament simulation script.
- [ ] Add market-odds features or post-hoc calibration layer.
- [ ] Archive or remove legacy `outputs/` and `graphs/` folders after confirming no references.

## Known issues

| Issue | Severity | Mitigation |
|-------|----------|------------|
| Draw class rarely top-ranked | Medium | Documented; draw diagnostics in Model 25 |
| Sparse early-era ratings | Low | `rating_age_days` tracked; complete-case filtering documented |
| Elo download can fail offline | Low | Manual CSV fallback documented in `docs/data_sources.md` |
| Duplicate script number resolved | — | `29_build_goalscorer_form_features.R`; reporting moved to `31_final_results_visualization.R` |
| Legacy `data/model_outputs/` copies | Low | New runs write to `reports/tables/`; old folder kept for compatibility |

## Job-application reviewer checklist

- [x] README explains the project in under 60 seconds
- [x] Clear pipeline runners (`run_light_pipeline.R`, `run_modeling_pipeline.R`)
- [x] Leakage audit documented (`docs/leakage_audit.md`)
- [x] Chronological train/validation/test splits
- [x] Model comparison table (`reports/tables/final_model_comparison.csv`)
- [x] Limitations stated honestly in README and MODEL_CARD
- [ ] Fresh-clone reproducibility with `renv` (recommended next step)
- [ ] Single HTML final report notebook (recommended next step)

## Cleanup log (2026-05-29)

See the cleanup report in the assistant conversation summary. Key changes:

- Standardized paths in `src/00_project_setup.R` (portable root detection).
- Removed hardcoded `~/Documents/...` paths from modeling scripts.
- Validation CSVs organized under `data/validation/processed_data/` and `engineered_features/`.
- Model outputs redirected to `reports/tables/model_30/` and `reports/tables/final_project/`.
- Renamed `29_final_results_visualization.R` → `31_final_results_visualization.R`.
- Renamed `final_final_model_*` figures → `final_model_*`.
