# International Match Outcome Modeling — Final Report

## Objective

Predict international association football match outcomes as calibrated multiclass probabilities for **home win (H), draw (D), and away win (A)** using strictly pre-match features and chronological train / validation / test evaluation.

## Data and feature policy

- **Sources:** [martj42/international_results](https://github.com/martj42/international_results), [World Football Elo](http://www.eloratings.net/).
- **Table (portfolio path):** `international_modeling_table_with_form.csv` (Model 28).
- **Table (tier study):** `international_modeling_table_with_form_and_goalscorers.csv` (Model 30).
- **Leakage policy:** no post-kickoff scores, lineups, or in-match events in features. See [docs/leakage_audit.md](../../docs/leakage_audit.md).
- **Splits:** chronological by match date; test reserved for final reporting.

## Final model story (three roles)

### 1. Preferred portfolio final model

| Field | Value |
|-------|-------|
| Stage | Model 28 |
| Algorithm | LightGBM |
| Features | `safe_plus_form_compact` (20 pre-match features) |
| Selection | Lowest validation log loss within `model_28_metrics.csv` |
| Validation log loss | **0.89309** (*n* = 7,366) |
| Test log loss | **0.87285** (*n* = 7,561) |
| Test accuracy / macro F1 | 59.5% / 0.437 |

Authority: `src/31_final_results_visualization.R` → `reports/tables/final_project_summary.csv`.

### 2. Simpler interpretable challenger (same cohort)

| Field | Value |
|-------|-------|
| Stage | Model 28 |
| Algorithm | Multinomial logit |
| Features | `safe_plus_form_compact` |
| Validation log loss | 0.89485 (Δ **+0.00176** vs portfolio final) |
| Test log loss | **0.87074** (reporting only; better than LightGBM on test in artifacts) |

Within the **0.005** practical-difference threshold on validation. Does **not** replace the portfolio final unless the selection rule intentionally favors interpretability over a marginal validation gain.

### 3. Tier / robustness analysis (different cohort)

| Field | Value |
|-------|-------|
| Stage | Model 30 |
| Algorithm | LightGBM |
| Features | `rating_plus_form` |
| Cohort | Fair-comparison complete cases on goalscorer-enriched table |
| Validation log loss | **0.88884** (*n* = 7,334) |
| Test log loss | 0.86965 (*n* = 7,384) |

**Best metric result on a different cohort** — not directly comparable to Model 28. Kept for incremental tier study (rating → context → form → goalscorers), not as portfolio final unless cohorts are harmonized and script 31 policy changes.

## Feature engineering summary

- **Rating tier:** pre-match Elo and `rating_diff`.
- **Context tier:** tournament / competition flags, neutral site.
- **Form tier:** lagged points, goal difference, draw rates (5- and 10-match windows).
- **Goalscorer tier (Model 30 only):** rolling scorer depth metrics — no material validation gain vs `rating_plus_form`.

## Feature validation

- 61 engineered features checked (34 form + 27 goalscorer).
- Automated validation checks passed; no leakage failures in goalscorer audit sample.
- Missingness is interpretable (early-era teams, sparse scorer history).

## Modeling setup

- Families: multinomial logit, glmnet ridge, LightGBM (where installed).
- **Primary selection metric:** validation multiclass log loss.
- **Test metrics:** reported only after selection.

## Tier study highlights (Model 30 cohort)

- **Strongest tier on this cohort:** rating + form (`rating_plus_form`).
- **Form tier (validation):** log-loss improvement vs prior tier ≈ −0.0041; macro-F1 gain ≈ +0.022.
- **Goalscorer tier (validation):** Δ vs `rating_plus_form` ≈ +6×10⁻⁵ (worse).
- **Hardest class:** draw (D) on test for the tier-best configuration.

## Calibration and diagnostics

- Portfolio final calibration: `reports/figures/final_model_calibration_plot.png`.
- Tier cohort diagnostics: `reports/figures/final_model/` (script 32).
- Feature importance (tier cohort): `reports/figures/final_model/08_feature_importance.png`.

## Limitations

- Modest gains over strong Elo baselines; trees vs multinom differ by ~0.002 validation log loss on the Model 28 cohort.
- Draw remains hard to rank as the top class.
- Cohort heterogeneity across scripts — rank models only within the same cohort.
- No market odds, squad value, or injury data in the current feature set.
- No serialized portfolio final artifact; reproduction requires re-running the pipeline.

## Recommended next steps

1. Optional cohort harmonization before any Model 30 promotion to portfolio final.
2. Probability calibration (Platt / isotonic) on the portfolio final.
3. Draw-focused modeling or two-stage H/D/A decomposition.
4. External squad / market data; `renv` for reproducibility.

## Artifact index

| Path | Description |
|------|-------------|
| [docs/model_selection_rationale.md](../../docs/model_selection_rationale.md) | Full rationale |
| [reports/tables/final_project/model_selection_rationale.csv](../tables/final_project/model_selection_rationale.csv) | Candidate table |
| [reports/tables/final_project_summary.csv](../tables/final_project_summary.csv) | Portfolio headline metrics |
| [reports/tables/final_project/final_best_model_summary.csv](../tables/final_project/final_best_model_summary.csv) | Portfolio, challenger, tier rows |
| [reports/tables/model_28_metrics.csv](../tables/model_28_metrics.csv) | Model 28 selection pool |
| [reports/tables/model_30/model_30_performance_summary.csv](../tables/model_30/model_30_performance_summary.csv) | Tier metrics |
