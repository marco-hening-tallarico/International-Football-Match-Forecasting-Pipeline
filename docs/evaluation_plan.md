# Evaluation Plan

## Primary metric

**Multiclass log loss** (cross-entropy) on H/D/A probabilities.

Used for model and feature-variant selection on the validation split.

## Secondary metrics

| Metric | Notes |
|--------|-------|
| Brier score | Multiclass Brier; lower is better |
| Accuracy | Argmax class vs actual |
| Macro F1 | Unweighted mean F1 across H, D, A |
| Classwise precision / recall | Especially draw class |
| Calibration bins | Predicted vs observed rates by probability decile |

## Split design

Defined on `international_modeling_table.csv` (script 18):

| Column | Values | Use |
|--------|--------|-----|
| `data_split_modeling` | train / validation / test | **Authoritative** split for modeling scripts |
| `data_split` | train / test | Legacy train vs held-out test (`date >= 2018-01-01`) |

| Split | Rule | Typical use |
|-------|------|-------------|
| train | `data_split_modeling == "train"` | Fit model parameters |
| validation | `data_split_modeling == "validation"` | Select model / feature variant (last 20% of pre-2018 training rows by `date`) |
| test | `data_split_modeling == "test"` | Final held-out report (`date >= 2018-01-01`) |

Modeling scripts use `data_split_modeling` when present and fall back to carving validation from `data_split == "train"` otherwise. Splits are **chronological** — no random shuffling.

**Model 28 cohort filters** (applied after the split, on complete-case rows): both teams must have pre-match Elo with `rating_age_days_* <= 365` and at least 10 prior international matches per team. Counts: `reports/tables/model_28_filter_counts.csv`.

## Cohort rules

- Complete-case filtering: rows with required features non-missing for the variant under test.
- Filter counts exported per script (`model_*_filter_counts.csv`).
- Different tiers may have slightly different N due to form cold-start missingness.

## Calibration assessment

- Reliability diagrams: `reports/figures/baseline_*_calibration.png`, `model_24_calibration_plot.png`
- Bin-level tables: `model_30_calibration_bins.csv`, `baseline_validation_calibration.csv`
- Draw-specific: Model 25 diagnostics (`model_25_draw_*`)

## Reporting artifacts

| Artifact | Location |
|----------|----------|
| Full stage comparison | `reports/tables/final_model_comparison.csv` |
| Incremental tier summary | `reports/tables/final_project/final_incremental_performance_summary.csv` |
| Best model summary | `reports/tables/final_project/final_best_model_summary.csv` |
| Narrative summary | `reports/final/final_results_summary.md` |

## Interpretation guidelines

- Log loss differences < 0.005 vs strong baselines are **modest** — report honestly.
- High accuracy with poor draw recall indicates home/away dominance in argmax predictions.
- Compare validation and test metrics together; large gaps suggest overfitting or cohort shift.
