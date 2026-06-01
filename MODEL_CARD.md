# Model Card — International Match Outcome Forecast

## Model identity

| Field | Value |
|-------|-------|
| **Preferred portfolio final model** | Model 28 — LightGBM + `safe_plus_form_compact` |
| Task | Multiclass H / D / A match outcome probabilities |
| Training script | `src/28_model_with_lagged_form.R` |
| Selection script | `src/31_final_results_visualization.R` |
| Input table | `data/processed/international_modeling_table_with_form.csv` |
| Cohort filters | Rating freshness ≤ 730 days; ≥ 5 prior matches per team; variant complete cases |

## Candidate and benchmark models

| Role | Model | Features | Val log loss | Notes |
|------|-------|----------|--------------|-------|
| **Portfolio final** | Model 28 LightGBM | `safe_plus_form_compact` | **0.89309** | Selected on Model 28 cohort (*n* = 7,366 val) |
| **Same-cohort challenger** | Model 28 multinom | `safe_plus_form_compact` | 0.89485 | Δ +0.00176; simpler interpretable alternative |
| **Tier robustness (different cohort)** | Model 30 LightGBM | `rating_plus_form` | **0.88884** | Not directly comparable; tier study only |
| **Communication baseline** | Script 19 Elo multinom | `rating_diff_neutral` | 0.89330 | Different cohort (*n* = 7,931 val) |

Full candidate table: [docs/model_selection_rationale.md](docs/model_selection_rationale.md), `reports/tables/final_project/model_selection_rationale.csv`.

## Simpler interpretable challenger

**Model 28 — multinomial logit + `safe_plus_form_compact`** on the same filtered cohort as the portfolio final.

- Validation log loss **0.89485** (+0.00176 vs LightGBM; below **0.005** practical-difference threshold).
- Test log loss **0.87074** vs LightGBM **0.87285** (reporting only — test was not used for selection).
- The tree model’s validation gain is **marginal**; multinom is defensible when linear interpretability matters.

## Model 30 robustness result

**Model 30 — LightGBM + `rating_plus_form`** on the goalscorer-enriched table and fair-comparison cohort (val *n* = 7,334; test *n* = 7,384).

- Lowest validation log loss among saved tier experiments: **0.88884**.
- **Not** the portfolio final: different table, filters, feature variant, and row set than Model 28.
- Goalscorer tier did not beat `rating_plus_form` on validation (Δ ≈ +6×10⁻⁵).
- Source: `reports/tables/model_30/model_30_performance_summary.csv`, `reports/tables/final_project/final_best_model_summary.csv`.

## Prediction target

Multiclass match outcome: **H** (home win), **D** (draw), **A** (away win), from `international_results.csv` final scores.

## Input features (portfolio final)

20 features in `safe_plus_form_compact`, including:

- Pre-match Elo: `home_rating_pre_match`, `away_rating_pre_match`, `rating_diff`
- Tournament / context flags (World Cup, qualifiers, friendlies, neutral site, etc.)
- Compact lagged form: points, goal difference, goals for/against, draw rates (5- and 10-match windows)

Feature list artifact: `models/model_33_feature_names.rds` (same variant naming).

## Excluded leakage columns

Post-match scores, lineups, in-match events, and any column computed using the current match outcome. See [docs/leakage_audit.md](docs/leakage_audit.md) and approved feature sets in `reports/tables/approved_feature_sets_final.R`.

## Train / validation / test protocol

| Split | Use |
|-------|-----|
| Train | Fit model parameters |
| Validation | Select feature variant and model family; LightGBM early stopping |
| Test | Final held-out metrics only — **not** used for selection |

- **Unit:** one international match (`source_match_id`).
- **Method:** chronological by `date`, not random.

## Selection criterion

**Lowest validation multiclass log loss** within `reports/tables/model_28_metrics.csv` for the Model 28 filtered cohort.

Test metrics are reported only after selection.

## Validation metrics (portfolio final)

| Metric | Value |
|--------|-------|
| Log loss | **0.89309** |
| Accuracy | 58.6% |
| Macro F1 | 0.433 |
| *n* | 7,366 |

## Final test metrics (portfolio final)

| Metric | Value |
|--------|-------|
| Log loss | **0.87285** |
| Accuracy | **59.5%** |
| Macro F1 | **0.437** |
| *n* | 7,561 |

Sources: `reports/tables/final_project_summary.csv`, `reports/tables/final_selected_model_test_metrics.csv`, `reports/tables/model_28_metrics.csv`.

## Calibration notes

- Test calibration plot: `reports/figures/final_model_calibration_plot.png`.
- Draw probabilities are often in a plausible range but draw is rarely the **top** predicted class.
- No post-hoc calibration (Platt / isotonic) applied in the published run.

## Model families tested

| Family | Scripts |
|--------|---------|
| Frequency / majority baselines | 19 |
| Elo multinomial | 19, 21 |
| Safe-feature multinomial / glmnet / LightGBM | 24, 26, 28, 30 |
| Hyperparameter sensitivity | 33 (not adopted) |

## Limitations

- Modest improvement over strong Elo baselines; complex vs simple models differ by ~0.002 validation log loss on the Model 28 cohort.
- Draw class remains weak for top-label prediction.
- Cohort mismatch between Model 28 and Model 30 — do not merge into one leaderboard.
- No market odds, squad value, or player availability.
- No pinned `renv.lock`; LightGBM training may vary slightly across runs.
- Serialized portfolio final model not saved — inference requires pipeline re-run.

## Intended use

- Portfolio demonstration of leakage-safe sports forecasting workflow.
- Research or exploratory tournament simulation with calibrated match probabilities.
- Teaching chronological evaluation and feature-tier ablation.

## Non-intended uses

- Real-money betting or gambling.
- Production deployment without fresh data, recalibration, and monitoring.
- In-match or live prediction (pre-match only).
- Claims of state-of-the-art World Cup forecasting without further validation.

## Related artifacts

| File | Purpose |
|------|---------|
| [README.md](README.md) | Project overview and reproduction |
| [reports/final/final_results_summary.md](reports/final/final_results_summary.md) | Headline results |
| [reports/final/final_international_modeling_report.md](reports/final/final_international_modeling_report.md) | Full final narrative |
| `reports/tables/final_model_comparison.csv` | Incremental staging comparison |
| `reports/tables/baseline_model_comparison.csv` | Elo baselines |
