# Model Card — International Match Outcome Forecast

## Prediction target

Multiclass match outcome: **H** (home win), **D** (draw), **A** (away win).

Derived from final scores in `international_results.csv`. Shootout-decided knockout matches use the recorded full-time result before penalties where applicable.

## Model families tested

| Family | Implementation | Stage |
|--------|----------------|-------|
| Frequency baseline | Historical class proportions | 19 |
| Majority baseline | Always predict most common class | 19 |
| Elo multinomial | `nnet::multinom` on rating difference | 19 |
| Safe-feature multinomial | Approved pre-match feature set | 24 |
| glmnet ridge | Multinomial logistic with L2 penalty | 24, 28, 30 |
| LightGBM | Gradient boosted trees (when installed) | 24, 26, 28, 30 |

## Training / validation / test strategy

- **Unit of analysis:** one international match (`source_match_id`).
- **Split method:** chronological by `date` on the modeling table — not random.
- **Selection:** best validation **log loss** within each stage.
- **Test set:** held out for final reporting only; not used for hyperparameter or feature-set selection.
- **Cohort:** complete-case rows for the feature set under test (documented per script).

## Feature groups

| Tier | Examples | Script |
|------|----------|--------|
| Baseline Elo | `home_rating_pre_match`, `away_rating_pre_match`, `rating_diff` | 18 |
| Tournament context | World Cup / qualifier / friendly flags, neutral site | 18, 22 |
| Lagged form | Points, goal difference, draw rate over last 5/10 matches | 27 |
| Goalscorer depth | Unique scorers, top-scorer share (365d / last 10) | 29 |

All features are restricted to information available **before** the match date. See [docs/leakage_audit.md](docs/leakage_audit.md).

## Primary metrics

| Metric | Direction | Use |
|--------|-----------|-----|
| Log loss | Lower is better | Primary selection metric |
| Brier score | Lower is better | Probabilistic accuracy |
| Accuracy | Higher is better | Reporting |
| Macro F1 | Higher is better | Class-imbalance-aware reporting |
| Draw recall / calibration bins | Reporting | Draw-specific diagnostics (Model 25) |

## Best model (current)

**Stage:** Model 28 — lagged form tier  
**Algorithm:** LightGBM  
**Feature variant:** `safe_plus_form_compact`  
**Selection basis:** validation log loss

| Split | Log loss | Accuracy | Macro F1 |
|-------|----------|----------|----------|
| Validation | 0.892 | 58.6% | 0.449 |
| Test | 0.874 | 59.5% | 0.463 |

Model 30 (goalscorer features) did not beat Model 28 on validation log loss. Model 30 best validation result: LightGBM + `rating_plus_form`, log loss 0.889 (test 0.870).

Artifacts: `reports/tables/final_model_comparison.csv`, `reports/tables/final_project/final_best_model_summary.csv`.

## Limitations

- Modest improvement over strong Elo baselines (~0.003 log loss test gain form → best ML).
- Draw remains hard to rank as the top class even when draw probability is reasonable.
- No market odds, squad value, or player-level availability in the current feature set.
- International friendlies and early-era matches add noise and missing ratings.
- Not tuned for tournament knockout rules (extra time, penalties) beyond recorded results.

## Intended use

- Portfolio demonstration of sports analytics / ML workflow.
- Baseline calibrated match probabilities for research or exploratory tournament simulation.
- Teaching example for leakage-safe feature engineering and chronological evaluation.

## Non-intended use

- Real-money betting or gambling decisions.
- Production deployment without fresh data pipelines and recalibration.
- Player- or minute-level in-match prediction (pre-match only).
- Claims of state-of-the-art World Cup forecasting without further validation on recent cycles.
