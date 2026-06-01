# Final reproducibility check

**Date:** 2026-06-01  
**Portfolio final:** Model 28 — LightGBM + `safe_plus_form_compact` (selected by validation log loss in script 31).

## Status summary

| Item | Status | Notes |
|------|--------|-------|
| `renv.lock` | **Present** (partial) | Created via `renv::init(bare = TRUE)` + `renv::snapshot()` on R 4.5.2. Lockfile may omit packages not installed at snapshot time; `renv::status()` may report out-of-sync. Notebook dependency scan warned about unclosed chunk in `notebooks/notebook_1.Rmd`. |
| `LICENSE` | **Missing** | Placeholder guidance: [license_needed.md](license_needed.md) |
| `models/final_preferred_model.rds` | **Missing** | `src/28_model_with_lagged_form.R` does not `saveRDS` the LightGBM fit |
| `models/final_preferred_feature_names.rds` | **Missing** | Use `reports/tables/approved_feature_sets_final.R` or variant list in `model_28_feature_variants.csv` |
| `data/predictions/final_preferred_model_predictions.csv` | **Present** | Copied from `reports/tables/model_28_predictions.csv` (lightgbm + safe_plus_form_compact; 14,927 rows + header) |

Existing unrelated artifact: `models/model_33_feature_names.rds` (hyperparameter sensitivity script only).

## Exact reproduction commands

From repository root, with R ≥ 4.2 and packages from `src/01_packages.R`:

```bash
# 1. Install packages (once per machine)
Rscript -e 'source("src/01_packages.R")'

# 2. Data through modeling table (skips Elo download if offline + no manual file)
Rscript src/run_light_pipeline.R

# 3. Baselines through final reporting (includes scripts 27, 29, 30b, 28, 30, 31, 32)
Rscript src/run_modeling_pipeline.R
```

**Reporting only** (after `model_28_metrics.csv` and Model 30 tables exist):

```bash
Rscript src/31_final_results_visualization.R
Rscript src/32_finalize_international_modeling_project.R
```

**Optional** hyperparameter sensitivity (not in default orchestrator):

```bash
Rscript src/33_model_hyperparameter_sensitivity.R
```

**Optional** renv lockfile (run on your machine if cache permissions allow):

```bash
Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv"); renv::init(bare = TRUE); renv::snapshot(prompt = FALSE)'
```

**Refresh portfolio-final prediction export** (no retrain):

```bash
mkdir -p data/predictions
{ head -1 reports/tables/model_28_predictions.csv
  awk -F, '$1=="safe_plus_form_compact" && $2=="lightgbm"' reports/tables/model_28_predictions.csv
} > data/predictions/final_preferred_model_predictions.csv
```

## Inference without serialized model

To regenerate probabilities for the portfolio final:

1. Ensure `data/processed/international_modeling_table_with_form.csv` exists.
2. Run `Rscript src/28_model_with_lagged_form.R` (retrains all Model 28 variants).
3. Read rows from `reports/tables/model_28_predictions.csv` where `feature_variant == "safe_plus_form_compact"` and `model == "lightgbm"`, or use `data/predictions/final_preferred_model_predictions.csv` if already exported.

Adding `saveRDS(lgb_fit, "models/final_preferred_model.rds")` to script 28 would require a small code change and a full Model 28 retrain; not done in this cleanup pass.

## Known reproducibility limitations

| Limitation | Impact |
|------------|--------|
| No `renv.lock` | Package versions may differ across machines |
| LightGBM `seed` unset in Model 28 | Boosting outputs may differ slightly |
| `glmnet::cv.glmnet` without `seed` | Comparator models may vary slightly |
| Raw data gitignored | Fresh clone needs downloads or manual Elo CSV |
| Validation used for early stopping and variant selection | Validation metrics are not fully unbiased |
| No post-hoc calibration | Reported probabilities are model-native |

## Related documents

- [final_consistency_check.md](final_consistency_check.md) — documentation alignment
- [hyperparameter_audit.md](hyperparameter_audit.md) — parameters and splits
- [model_selection_rationale.md](model_selection_rationale.md) — why Model 28 is portfolio final
- [license_needed.md](license_needed.md) — before public release
