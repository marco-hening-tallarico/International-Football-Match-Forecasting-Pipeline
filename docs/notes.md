# Project Notes

Miscellaneous notes consolidated from development. For the canonical pipeline reference see [pipeline.md](pipeline.md).

## Naming conventions (2026-05-29 cleanup)

- Processed tables: `international_<entity>.csv`
- Validation: `data/validation/processed_data/` (cleaning QA), `engineered_features/` (leakage audits)
- Model metrics: `reports/tables/model_<NN>/` or `reports/tables/model_<NN>_*.csv`
- Final summary: `reports/tables/final_project/`
- Figures: `reports/figures/` (subfolders by topic)

## Script numbering note

Two scripts previously shared prefix `29`. Resolved as:

- `29_build_goalscorer_form_features.R` — feature engineering
- `31_final_results_visualization.R` — reporting (was `29_final_results_visualization.R`)

## Legacy folders

| Folder | Status |
|--------|--------|
| `graphs/` | Superseded by `reports/figures/` — kept for backward compatibility |
| `outputs/` | Superseded by `reports/tables/legacy_analysis/` |
| `data/model_outputs/` | Superseded by `reports/tables/model_30/` and `final_project/` |

New pipeline runs write to the new locations.

## renv

Not yet initialized. Recommended after confirming a clean run:

```r
install.packages("renv")
renv::init()
```

## Future work

See `reports/final/final_results_summary.md` — market odds, squad value, tournament simulation, draw class weighting, SHAP explainability.
