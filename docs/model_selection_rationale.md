# Model selection rationale

**Date:** 2026-06-01  
**Scope:** International H/D/A match forecasting (scripts 19–32; optional 33).  
**Sources:** Existing metrics and artifacts only—no retraining, no edits to README, MODEL_CARD, or `reports/final/final_international_modeling_report.md`.

---

## Executive decision

**Preferred portfolio final model:** **Model 28 — LightGBM + `safe_plus_form_compact`** (20 pre-match features), selected by script `31_final_results_visualization.R` from `model_28_metrics.csv`.

This choice stands because it is the only configuration selected on the **official Model 28 cohort** using **validation log loss only**, with leakage-safe compact lagged form, and it is already aligned with `final_project_summary.csv`, README, and MODEL_CARD.

**Model 30 — LightGBM + `rating_plus_form`** has the **lowest validation log loss among saved tier experiments** (0.8888) but on a **different table, filter pipeline, and row set**. Treat it as **tier / robustness analysis**, not as a drop-in replacement for the portfolio final unless the project explicitly harmonizes cohorts and re-runs selection under one policy.

**Simpler interpretable challenger:** **Model 28 — multinomial logit + `safe_plus_form_compact`** is a **serious preferred-model candidate** (validation log loss within **0.002** of the official model on the **same cohort**; test log loss **slightly better** than LightGBM). **Elo-only `rating_diff_neutral_multinom`** (script 19) remains a strong **communication baseline** but must not be ranked as “best same-cohort” without re-scoring on the Model 28 filtered table.

---

## Four roles (do not conflate)

| Role | Model | Validation log loss | Notes |
|------|--------|---------------------|--------|
| **1. Best metric model (overall artifacts)** | Model 30 — LightGBM + `rating_plus_form` | **0.88884** | Model 30 fair-comparison cohort (val *n* = 7,334). Not comparable 1:1 to Model 28. |
| **2. Best same-cohort model** | Model 28 — LightGBM + `safe_plus_form_compact` | **0.89309** | Model 28 cohort (val *n* = 7,366). Selection metric for portfolio. |
| **3. Preferred portfolio final** | Model 28 — LightGBM + `safe_plus_form_compact` | **0.89309** | Script 31 authority; reviewer clarity + documented leakage path. |
| **4. Simpler interpretable challenger** | Model 28 — multinom + `safe_plus_form_compact` | **0.89485** (Δ **+0.00176** vs best same-cohort) | Within **0.005** threshold → defensible alternative. Elo-only baseline: see caveats. |

---

## Why Model 28 and Model 30 must not be compared directly

| Dimension | Model 28 (portfolio) | Model 30 (script 32 tier report) |
|-----------|----------------------|----------------------------------|
| Input table | `international_modeling_table_with_form.csv` | `international_modeling_table_with_form_and_goalscorers.csv` |
| Filters | Rating freshness (≤730 days), ≥5 prior matches per team, variant complete cases | Fair-comparison complete-case cohort for tier features |
| Rows after filters | 44,387 | Documented separately in Model 30 notes (not identical to 44,387) |
| Validation *n* | **7,366** | **7,334** |
| Test *n* | **7,561** | **7,384** |
| Feature variant for “best” | `safe_plus_form_compact` (20 features) | `rating_plus_form` (broader tier set than compact) |
| Selection authority | `src/31_final_results_visualization.R` → `final_project_summary.csv` | `src/32_finalize_international_modeling_project.R` → `final_best_model_summary.csv` |

A lower validation log loss on Model 30 (**0.8888** vs **0.8931**) does **not** prove superiority on the portfolio task; it reflects a **different experimental cell**. Per project decision framework: call Model 30 a **tier-analysis / robustness** result unless cohort and features are harmonized and selection is re-run once.

Optional Model 33 hyperparameter sensitivity used yet another filtered *n* (validation 6,820) and did **not** materially beat Model 28 artifacts under the project’s 0.005 threshold—**not adopted**.

---

## Is a simpler model defensible?

**Yes**, on two grounds:

1. **Same cohort (Model 28):** `multinom` + `safe_plus_form_compact` validation log loss **0.89485** vs LightGBM **0.89309** → Δ = **0.00176** (< **0.005**). Gain from trees is **marginal** on the selection split. On **test** (reporting only), multinom log loss **0.87074** vs LightGBM **0.87285**—linear model is slightly better on held-out test despite worse validation ranking (illustrates modest signal and split noise).

2. **Elo baseline (script 19):** `rating_diff_neutral_multinom` test log loss **0.87064** is competitive with the final LightGBM test **0.87285**, but validation **0.89330** uses baseline cohort (val *n* = 7,931)—**not** the Model 28 filtered cohort. Prefer Elo for **narrative and stability**, not as “winner” in the same-cohort table without re-fitting on Model 28 rows.

Choosing multinom + `safe_plus_form_compact` for the portfolio would trade a **small validation metric edge** for **interpretability** (linear coefficients, no tree ensemble) with **negligible test penalty** in current artifacts.

---

## Model 30: final model or reframe?

**Reframe.** Keep Model 30 as:

- **Incremental tier study** (rating → context → form → goalscorers) on the goalscorer-enriched table;
- **Robustness check** that richer tiers can reach lower validation log loss **within that study’s cohort**;
- Source for script 32 diagnostics (calibration, feature importance on `rating_plus_form`).

Do **not** present Model 30 LightGBM as the official portfolio final without changing selection policy and updating script 31, README, and MODEL_CARD together.

Goalscorer tier did **not** improve validation log loss vs `rating_plus_form` (Δ ≈ **+6×10⁻⁵** on validation—worse).

---

## Candidate model table

Validation log loss is the **selection** metric; test metrics are **final reporting only**.

| Model ID | Family | Feature set | Cohort note | Val log loss | Test log loss | Test acc | Test macro F1 | Interpretability | Recommendation |
|----------|--------|-------------|-------------|--------------|---------------|----------|---------------|------------------|----------------|
| **M28-LGB-compact** | LightGBM | `safe_plus_form_compact` | Model 28 filtered; val 7,366 / test 7,561 | **0.89309** | 0.87285 | 0.595 | 0.437 | Low–medium (trees) | **Portfolio final** |
| **M28-multinom-compact** | Multinom | `safe_plus_form_compact` | Same as M28-LGB | 0.89485 | **0.87074** | 0.599 | **0.440** | High (linear, 20 feat) | **Same-cohort challenger** |
| **M30-LGB-form** | LightGBM | `rating_plus_form` | Model 30 fair-comparison; val 7,334 / test 7,384 | **0.88884** | **0.86965** | **0.602** | **0.463** | Low–medium | **Tier robustness (not portfolio final)** |
| **M30-multinom-form** | Multinom | `rating_plus_form` | Model 30 cohort | 0.89080 | 0.86977 | 0.598 | 0.468 | High | Tier interpretable alternative |
| **M19-Elo-neutral** | Multinom | `rating_diff` (+ neutral) | Baseline table; val 7,931 / test 7,661 | 0.89330 | 0.87064 | **0.600** | 0.439 | **Highest** (1 signal) | Baseline reference (different cohort) |
| **M24-LGB-safe** | LightGBM | Approved safe (no form) | Model 24 cohort; val 7,615 | 0.89344 | 0.87416 | 0.595 | 0.436 | Low–medium | Staging reference |
| **M26-LGB-draw** | LightGBM | `draw_abs_only` | Model 26 cohort; val 7,615 | 0.89341 | 0.87432 | 0.594 | 0.435 | Low–medium | No material gain vs M24 |
| **M21-draw-quad** | Multinom | Draw-aware Elo | Baseline+ cohort; val 7,349 | 0.89259 | 0.87189 | 0.597 | 0.438 | High (Elo transforms) | Baseline+ reference (different cohort) |

*Bold* in test columns highlights reporting-only strengths that do not override validation-based portfolio selection.

---

## Final wording for README and MODEL_CARD

Use this block (or close paraphrase) when those files are updated:

> **Final model:** LightGBM trained on the **`safe_plus_form_compact`** feature set (Model 28), selected by **lowest validation multiclass log loss** on the Model 28 filtered cohort (`international_modeling_table_with_form.csv`; rating freshness and minimum prior-match history). **Test metrics are reported only after selection** and are not used for tuning.
>
> **Validation:** log loss **0.893** (*n* = 7,366); **test:** log loss **0.873** (*n* = 7,561), accuracy **59.5%**, macro F1 **0.437**.
>
> **Tier analysis (Model 30):** On a separate fair-comparison cohort with extended feature tiers, LightGBM + `rating_plus_form` achieved validation log loss **0.889**—**not directly comparable** to the Model 28 final. Goalscorer features did not improve on `rating_plus_form`.
>
> **Interpretable alternative:** Multinomial logit on the same Model 28 features is within **0.002** validation log loss of LightGBM and is a defensible choice if linear interpretability outweighs a marginal validation gain.

---

## Caveats and limitations

- **Cohort heterogeneity:** Baseline (19), baseline+ (21), Model 24/26, Model 28, Model 30, and Model 33 sensitivity use different row filters and *n*; only rank models within the same script/cohort.
- **Validation reuse:** LightGBM early stopping uses the validation fold; selection on the same fold is standard but slightly optimistic.
- **Test vs validation disagreement:** Model 28 multinom beats LightGBM on test log loss while losing on validation—gains are **modest** (~0.002–0.003 log loss).
- **Draw class:** Final LightGBM still rarely predicts draw as the top class on test; macro F1 and draw diagnostics remain weak for class D.
- **Reproducibility:** No `renv.lock`; Model 28 LightGBM lacks pinned LightGBM `seed` in training code; serialized final model not saved—inference requires pipeline re-run.
- **Metric rounding:** README/MODEL_CARD may show 0.892/0.874; use CSVs (`model_28_metrics.csv`, `final_selected_model_test_metrics.csv`) for exact values.
- **Files not found:** `baseline_validation_metrics.csv` / `baseline_test_metrics.csv` and `reports/tables/final_project/final_project_summary.csv` were not present; metrics taken from `baseline_model_comparison.csv`, `final_project_summary.csv` (parent `reports/tables/`), and paths above.

---

## Artifact index

| Artifact | Role |
|----------|------|
| `reports/tables/model_28_metrics.csv` | Official selection pool (script 31) |
| `reports/tables/final_project_summary.csv` | Portfolio final summary |
| `reports/tables/final_selected_model_test_metrics.csv` | Post-selection test row |
| `reports/tables/model_30/model_30_performance_summary.csv` | Tier / robustness metrics |
| `reports/tables/final_project/final_best_model_summary.csv` | Script 32 summary: portfolio final (Model 28) + tier robustness (Model 30) + same-cohort challenger rows |
| `reports/tables/baseline_model_comparison.csv` | Elo baselines (script 19) |
| `reports/tables/final_model_comparison.csv` | Cross-stage staging comparison |
| `docs/final_project_completion_audit.md` | Prior audit of dual-final narrative |

Structured machine-readable rows: `reports/tables/final_project/model_selection_rationale.csv`.
