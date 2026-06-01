# Hyperparameter and Reproducibility Audit

**Audit date:** 2026-06-01  
**Scope:** Final selected international match outcome model (H / D / A), LightGBM training in Model 28, and how it relates to Model 30 and reporting scripts 31–32.

This audit confirms claims in `README.md`, `MODEL_CARD.md`, and `reports/final/final_results_summary.md` against source scripts and saved tables—not against documentation alone.

---

## Executive summary

| Question | Answer |
|----------|--------|
| Final model | **Model 28** — `lightgbm` + `safe_plus_form_compact` |
| Training script | `src/28_model_with_lagged_form.R` |
| Selection rule | Lowest **validation log loss** within `model_28_metrics.csv` |
| Hyperparameters | **Manually set conservative defaults** (shared across scripts 24/26/28/30); **not** grid-searched |
| Test set for tuning | **No** — test used only for reporting; validation used for model/variant selection and LightGBM early stopping |
| Reproducibility | **Partial** — `set.seed(2026)` at script start; LightGBM has **no** `seed` param; no `renv`; models not serialized to `models/` |
| Immediate tuning? | **Not required** — document params in `MODEL_CARD.md`; optional small validation-only sensitivity check |

Model 30 explored a broader feature tier on a **different cohort**; it is **not** the project’s official final selection (see [§1](#1-which-model-is-currently-selected-as-final) and [§11](#11-are-the-results-reproducible-from-the-current-scripts)).

---

## 1. Which model is currently selected as final?

**Model 28 — LightGBM with feature variant `safe_plus_form_compact`.**

Evidence (consistent across artifacts):

| Source | Selection |
|--------|-----------|
| `reports/tables/model_28_best_validation_models.csv` | `overall_best`: `safe_plus_form_compact` / `lightgbm`, validation log loss **0.89309** |
| `reports/tables/final_project_summary.csv` | `final_selected_model` = `lightgbm`, `final_selected_feature_variant` = `safe_plus_form_compact` |
| `reports/tables/final_selected_model_test_metrics.csv` | Same pair; test log loss **0.87285** (reporting only) |
| `reports/final/final_results_summary.md` | Three-role final story (portfolio + tier + challenger) |
| `MODEL_CARD.md` | **Preferred portfolio final** → Model 28 LightGBM; Model 30 tier labeled separately |

Model 30’s best validation configuration (`lightgbm` + `rating_plus_form`, log loss **0.88884** on 7,334 validation rows) is **not** promoted to portfolio final by `src/31_final_results_visualization.R`, which selects exclusively from `model_28_metrics.csv`. Model 30 uses a different input table, cohort filters, and feature naming (`rating_plus_form` vs `safe_plus_form_compact`); metrics are **not directly comparable** to Model 28’s 7,366 validation rows.

---

## 2. Which script trains it?

**Primary training:** `src/28_model_with_lagged_form.R`

- **Input:** `data/processed/international_modeling_table_with_form.csv`
- **Outputs:** `reports/tables/model_28_*.csv`, `reports/figures/model_28_*.png`

**Reporting only (no retraining):**

- `src/31_final_results_visualization.R` — locks final selection from `model_28_metrics.csv`
- `src/32_finalize_international_modeling_project.R` — Model 30 tier summary only

Orchestrator: `src/run_modeling_pipeline.R` runs scripts 28 → 31 → 32 in order.

---

## 3. Which feature set does it use?

**Variant name:** `safe_plus_form_compact` (**20 features**)

Defined in `src/28_model_with_lagged_form.R` and frozen in `reports/tables/model_28_feature_variants.csv`:

**Base safe (11):**  
`rating_diff`, `home_rating_pre_match`, `away_rating_pre_match`, `rating_age_days_home`, `rating_age_days_away`, `neutral`, `is_world_cup`, `is_world_cup_qualifier`, `is_continental_tournament`, `is_continental_qualifier`, `is_friendly`

**Compact form (9):**  
`home_points_per_match_last_5`, `away_points_per_match_last_5`, `form_points_diff_last_5`, `home_goal_diff_per_match_last_5`, `away_goal_diff_per_match_last_5`, `form_goal_diff_diff_last_5`, `home_draw_rate_last_10`, `away_draw_rate_last_10`, `form_draw_rate_mean_last_10`

**Cohort filters (Model 28 only, before split):**

- Valid `match_result` ∈ {H, D, A}
- Rating freshness ≤ 365 days (both teams)
- `home_prior_matches` and `away_prior_matches` ≥ 10
- Complete cases on all variant features → **41,386** rows (`model_28_filter_counts.csv`)

---

## 4. What exact hyperparameters are used?

### LightGBM (final selected algorithm)

From `fit_variant_models()` in `src/28_model_with_lagged_form.R` (identical block in `src/24_model_glm_lightgbm_approved_features.R`, `src/26_model_draw_aware_features.R`, `src/30_model_with_goalscorer_features.R`):

| Setting | Value |
|---------|-------|
| `objective` | `multiclass` |
| `metric` | `multi_logloss` |
| `num_class` | 3 |
| `learning_rate` | 0.03 |
| `num_leaves` | 15 |
| `max_depth` | 4 |
| `min_data_in_leaf` | 100 |
| `feature_fraction` | 0.9 |
| `bagging_fraction` | 0.9 |
| `bagging_freq` | 1 |
| `lambda_l2` | 1 |
| `verbosity` | -1 |
| `nrounds` (max) | 1000 |
| `early_stopping_rounds` | 50 |
| Early-stopping monitor | Validation set (`valids = list(validation = dvalidation)`) |
| Class label encoding | A=0, D=1, H=2 |

**Not set in code:** `seed`, `feature_fraction_seed`, `bagging_seed`, `num_threads`, `deterministic`, `min_gain_to_split`, `min_sum_hessian_in_leaf`, class weights, `is_unbalance`.

The **effective number of trees** is chosen by early stopping on validation log loss (not fixed at 1000).

### Other algorithms in Model 28 (not selected)

| Algorithm | Key settings | Tuning |
|-----------|--------------|--------|
| `nnet::multinom` | `trace = FALSE`, `MaxNWts = 10000` | Defaults; no explicit seed in call |
| `glmnet::cv.glmnet` | `family = "multinomial"`, `alpha = 0` (ridge), `nfolds = 5`, `type.measure = "deviance"`, predict at `s = "lambda.min"` | **5-fold CV on train only** for λ |

---

## 5. Are those hyperparameters defaults, manually chosen, or tuned?

| Component | Status |
|-----------|--------|
| LightGBM structural params (`num_leaves`, `max_depth`, `learning_rate`, etc.) | **Manually chosen** conservative values, copied across modeling scripts; **no grid or Bayesian search** |
| LightGBM `nrounds` | **Validation-tuned** via early stopping (not test) |
| glmnet λ | **Train CV-tuned** (`cv.glmnet`, 5 folds on training matrix only) |
| Feature variant `safe_plus_form_compact` | **Selected on validation log loss** among three variants in Model 28 |
| Model family `lightgbm` vs `multinom` / `glmnet` | **Selected on validation log loss** within Model 28 |

`docs/modeling_plan.md` explicitly lists large-scale hyperparameter search as a **non-goal** for this project version.

---

## 6. What random seeds are set?

| Location | Seed | Affects final LightGBM? |
|----------|------|-------------------------|
| `src/28_model_with_lagged_form.R` line 13 | `set.seed(2026)` | Indirectly (R-level RNG before fit) |
| `src/24`, `26`, `30`, `31`, `32`, `25`, `27`, `29`, `30b` | `set.seed(2026)` | Same convention |
| `src/19_baseline.R`, `21_baseline_plus_draw_features.R` | `set.seed(20240529)` | Baselines only |
| `src/30_model_with_goalscorer_features.R` (ranger fallback) | `seed = 2026` | Only if LightGBM unavailable |
| LightGBM `lgb.train` params | **None** | **Gap** — boosting may vary across runs/OS/threading |
| `glmnet::cv.glmnet` | No `seed` argument | CV fold splits may vary slightly |

---

## 7. What train / validation / test split is used?

### Test vs pre-test (script 18)

- **Column:** `data_split` on `international_modeling_table*.csv`
- **Rule:** `date < 2018-01-01` → `train`; `date >= 2018-01-01` → `test` (`CHRONOLOGICAL_SPLIT_DATE` in `src/18_build_international_modeling_table.R`)

### Validation carve-out (scripts 28, 30, etc.)

- **Function:** `make_chronological_splits()`
- **Rule:** Among `data_split == "train"` rows, sort by `date`; last **20%** → validation; remainder → train
- **Constant:** `validation_fraction <- 0.20` (Model 28); `VALIDATION_FRACTION <- 0.20` (Model 30)

### Model 28 realized counts (`model_28_split_summary.csv`)

| Split | Rows | Date range |
|-------|------|------------|
| Train | 29,460 | 1881-03-12 → 2009-10-10 |
| Validation | 7,366 | 2009-10-10 → 2017-12-29 |
| Test | 7,561 | 2018-01-02 → 2026-03-31 |

Test boundaries align with the 2018-01-01 project split; validation is the **final 20% of pre-2018 training-era** matches.

---

## 8. Is model selection based only on validation log loss?

**Yes, for the official final model.**

In `src/28_model_with_lagged_form.R`:

```r
validation_ranking <- all_metrics |>
    dplyr::filter(split == "validation") |>
    dplyr::arrange(log_loss)
best_validation_models <- validation_ranking |> dplyr::slice_head(n = 1)
```

`src/31_final_results_visualization.R` applies the same rule to `model_28_metrics.csv` and writes `final_project_summary.csv` / `final_results_summary.md`.

**Nuances:**

- Tie-breaking is implicit row order after `arrange(log_loss)` (no secondary metric in Model 28 selection).
- LightGBM **early stopping** also uses validation `multi_logloss` (selects tree count).
- Script 32 reports a separate “best test model” row for Model 30 tiers—that is **diagnostic only**, not the project final choice.

---

## 9. Is the test set protected from hyperparameter tuning?

| Mechanism | Uses test? |
|-----------|------------|
| Feature variant selection (Model 28) | **No** — validation log loss only |
| Algorithm selection within variant | **No** |
| glmnet λ (5-fold CV) | **No** — folds on **train** matrix only |
| LightGBM early stopping | **No** — uses **validation** split carved from pre-2018 train |
| Final test metrics / figures | **Yes** — reporting only, after selection |

The held-out **test** split (2018+) is not passed to `lgb.train(..., valids=...)`. This matches `docs/evaluation_plan.md` and `docs/leakage_audit.md`.

**Caveat:** Validation is used both for **early stopping** and for **variant/model selection**, so validation information influences both tree count and which configuration is declared “final.” That is standard practice but means validation metrics are not fully “unused”; test remains untouched for those decisions.

---

## 10. Are final predictions and metrics saved?

**Yes.**

| Artifact | Contents |
|----------|----------|
| `reports/tables/model_28_metrics.csv` | All models × variants × validation/test metrics |
| `reports/tables/model_28_predictions.csv` | Row-level probabilities (validation + test) |
| `reports/tables/model_28_best_validation_models.csv` | Selected configuration |
| `reports/tables/final_selected_model_test_metrics.csv` | Validation + test for final selection |
| `reports/tables/final_project_summary.csv` | Key-value summary |
| `reports/tables/final_prediction_examples.csv` | Curated test examples |
| `reports/final/final_results_summary.md` | Narrative + metric table |
| `reports/figures/final_model_*.png` | Confusion, calibration, confidence plots |

**Not saved:** Serialized portfolio-final LightGBM object (`models/final_preferred_model.rds` absent). Row-level probabilities for the portfolio final are in `reports/tables/model_28_predictions.csv` (and optionally mirrored under `data/predictions/`). Reproducing the fitted booster requires re-running `src/28_model_with_lagged_form.R`.

Model 30 exports parallel tables under `reports/tables/model_30/`; optional prediction export path is noted in script header but the final headline product remains Model 28 outputs from script 31.

---

## 11. Are the results reproducible from the current scripts?

**Mostly, with known gaps.**

| Factor | Status |
|--------|--------|
| Scripted pipeline (`run_modeling_pipeline.R`) | ✅ End-to-end path exists |
| Pinned dependencies (`renv`) | ❌ Not configured (`README.md`) |
| `set.seed(2026)` in Model 28 | ✅ Present |
| LightGBM deterministic training | ⚠️ No `seed` / `deterministic` in `lgb_params` |
| Saved fitted model | ❌ Must refit to regenerate probabilities |
| Identical cohort filters | ✅ Encoded in script (filters + `drop_na`) |
| glmnet CV | ⚠️ May vary slightly without `seed` in `cv.glmnet` |

Re-running `src/28_model_with_lagged_form.R` on the same processed CSV and package versions should reproduce **very similar** metrics; exact bitwise equality of LightGBM outputs is **not guaranteed** without explicit LightGBM seeds and thread control.

---

## 12. What are the main hyperparameter risks?

1. **No LightGBM `seed`** — Stochastic bagging/feature subsampling can shift log loss at the third decimal place across machines.
2. **Early stopping on validation** — Tree count is validation-adapted; combined with variant selection on the same split, validation log loss is optimistically biased vs a fresh temporal fold.
3. **Conservative manual tree params** — `min_data_in_leaf = 100` and shallow `max_depth = 4` may underfit; no evidence of overfitting from a large search, but also no proof of optimality.
4. **No class weights** — Draw recall remains extremely low (test draw recall ≈ **0.002** in saved metrics) despite reasonable mean draw probability (~0.22).
5. **Cohort-specific winner** — Model 28 filters (rating age, min 10 prior matches) differ from Model 30’s fair-comparison cohort; “best” on one table may not win on another.
6. **glmnet λ via `lambda.min`** — Can be slightly optimistic vs `lambda.1se`; not used for final selection but affects comparator models in the same script.
7. **Documentation drift** — `MODEL_CARD.md` validation accuracy/macro F1 (58.6% / 0.449) differ slightly from `final_selected_model_test_metrics.csv` for the same split because of rounding or an older run; log loss values align at three decimals.

---

## LightGBM parameter reference table

| Parameter | Value used | Where defined | Why it matters | Tune later? |
|-----------|------------|---------------|----------------|-------------|
| `objective` | `multiclass` | `src/28_model_with_lagged_form.R` ~L616 | 3-class H/D/A | No — fixed by problem |
| `metric` | `multi_logloss` | same | Aligns with selection metric | No |
| `num_class` | 3 | same | Class count | No |
| `learning_rate` | 0.03 | same | Speed vs stability of boosting | **Low priority** — try 0.02–0.05 on validation only |
| `num_leaves` | 15 | same | Model capacity (with `max_depth`) | **Medium** — interacts with depth |
| `max_depth` | 4 | same | Limits interaction depth | **Medium** |
| `min_data_in_leaf` | 100 | same | Regularization; large for ~27k train rows | **Medium** — sensitivity for stability |
| `feature_fraction` | 0.9 | same | Column subsampling | **Low** |
| `bagging_fraction` | 0.9 | same | Row subsampling | **Low** |
| `bagging_freq` | 1 | same | Subsample every iteration | **Low** |
| `lambda_l2` | 1 | same | L2 on leaves | **Low–medium** |
| `nrounds` | 1000 (max) | `lgb.train` call ~L634 | Upper bound; actual rounds from ES | ES already adapts |
| `early_stopping_rounds` | 50 | same | Patience on validation | **Low** |
| `seed` | *unset* | — | Reproducibility | **Yes** — set `seed = 2026` when documenting |
| `verbosity` | -1 | params list | Logging only | No |

---

## Co-trained models in Model 28 (reference)

For completeness, the same script fits comparators with these settings:

- **multinom:** `MaxNWts = 10000`, no weight decay tuning  
- **glmnet:** ridge multinomial, 5-fold CV on train, `lambda.min` at predict time  

Neither was selected for the final artifact.

---

## Recommendations

### Immediate actions

1. **No large hyperparameter search is needed** — Current values are conservative, consistent across stages, and appropriate for a leakage-safe MVP baseline.
2. **Document LightGBM params in `MODEL_CARD.md`** — Add a short “Training hyperparameters” subsection with the table above, `nrounds` / early stopping behavior, and the note that `seed` is not yet set in `lgb_params`.
3. **Optional reproducibility hardening in `src/28_model_with_lagged_form.R` only** (minimal diff):
   - Add `seed = 2026` (and optionally `deterministic = TRUE` if supported by installed `lightgbm` version) to `lgb_params`.
   - Pass `seed = 2026` to `glmnet::cv.glmnet` when glmnet is used.

### Optional validation-only sensitivity (no new script required unless you want a CSV artifact)

If you want empirical reassurance without changing the final model, compare **3–5** preset configs on the **Model 28 cohort and splits only**, ranking by **validation log loss**:

| Config label | Change vs baseline |
|--------------|-------------------|
| `baseline` | Current params (reference) |
| `shallower` | `num_leaves = 8`, `max_depth = 3` |
| `stronger_l2` | `lambda_l2 = 5`, `min_data_in_leaf = 150` |
| `slower_lr` | `learning_rate = 0.02`, `nrounds = 1500`, `early_stopping_rounds = 75` |
| `more_stochastic` | `feature_fraction = 0.8`, `bagging_fraction = 0.8` |

Save to `reports/tables/final_project/lightgbm_hyperparameter_sensitivity.csv` if automated; **do not** use test log loss for ranking or auto-update the selected model.

A dedicated `src/33_lightgbm_hyperparameter_sensitivity.R` is **not required** for this audit; add it only if you want a one-command reproducible sensitivity table alongside the modeling pipeline.

### Not recommended

- Tuning on the test set  
- Large random/grid search over many parameters  
- Replacing Model 28 as final based on Model 30 metrics without harmonizing cohorts and feature definitions  

---

## Quick answers index

| # | Question | Short answer |
|---|----------|--------------|
| 1 | Final model? | Model 28: `lightgbm` + `safe_plus_form_compact` |
| 2 | Training script? | `src/28_model_with_lagged_form.R` |
| 3 | Feature set? | 20 features — safe Elo/context + compact lagged form |
| 4 | Hyperparameters? | See [§4](#4-what-exact-hyperparameters-are-used) and parameter table |
| 5 | Defaults vs tuned? | Manual + validation early stopping; glmnet λ via train CV |
| 6 | Seeds? | `set.seed(2026)`; LightGBM seed unset |
| 7 | Splits? | Pre-2018 train → 80/20 train/val; 2018+ test |
| 8 | Selection metric? | Validation log loss |
| 9 | Test protected? | Yes for selection/tuning; used for final report only |
| 10 | Artifacts saved? | Yes (tables + figures; no serialized model) |
| 11 | Reproducible? | Mostly; pin packages and add LightGBM seed for strict reproducibility |
| 12 | Main risks? | Stochastic LGBM, validation reuse, draw class, cohort mismatch vs Model 30 |

---

## Related documents

- [MODEL_CARD.md](../MODEL_CARD.md) — model card (metrics + limitations)  
- [docs/modeling_plan.md](modeling_plan.md) — stages and non-goals  
- [docs/evaluation_plan.md](evaluation_plan.md) — metrics and split protocol  
- [docs/leakage_audit.md](leakage_audit.md) — feature timing  
- [reports/final/final_results_summary.md](../reports/final/final_results_summary.md) — headline results  
