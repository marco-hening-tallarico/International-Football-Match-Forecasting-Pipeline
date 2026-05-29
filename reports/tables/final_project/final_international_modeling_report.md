# International Match Outcome Modeling — Final Summary

## Objective
Predict international association football match outcomes as calibrated
multiclass probabilities for home win (H), draw (D), and away win (A) using
strictly pre-match features.

## Data Used
- International match results with chronological train/validation/test splits.
- Pre-match Elo-style ratings (`home_rating_pre_match`, `away_rating_pre_match`, `rating_diff`).
- Tournament/context flags and lumped tournament indicators.
- Lagged team-form features built from prior matches only.
- Goalscorer-derived attacking-depth features (unique scorers, top-scorer goals, etc.).

## Feature Engineering
- **Baseline rating tier**: pre-match ratings and rating difference.
- **Context tier**: tournament/competition flags added to the rating baseline.
- **Form tier**: lagged points, goal difference, goals for/against, and draw-rate windows (5 and 10 matches).
- **Goalscorer tier**: rolling scorer counts and attacking-depth metrics layered on top of form.
- All engineered features were intended to be pre-match only.

## Feature Validation
- 61 engineered features checked (34 form + 27 goalscorer).
- 8/8 automated validation checks passed with 0 failures.
- No leakage failures in the goalscorer audit sample.
- Manual recomputation matched sampled features.
- Missingness was interpretable (early-career teams and sparse goal-minute history).

## Modeling Setup
- Chronological train/validation/test split on a fair-comparison complete-case cohort.
- Models trained: multinomial logit, glmnet ridge, and LightGBM.
- Metrics: log loss, Brier score, macro F1, classwise precision/recall/F1, confusion matrices.
- Model selection based primarily on validation log loss (test reserved for final reporting).

## Main Results
- **Best validation model**: lightgbm with `rating_plus_form` (validation log loss = 0.8888, macro F1 = 0.4490).
- **Best test model**: lightgbm with `rating_plus_form` (test log loss = 0.8696, accuracy = 0.6024, macro F1 = 0.4629).
- **Best validation model on test split**: validation-selected lightgbm / `rating_plus_form` achieved test log loss = 0.8696, accuracy = 0.6024, macro F1 = 0.4629.
- **Strongest feature tier**: Rating + form.
- **Form tier (validation)**: log-loss delta vs prior tier = -0.0041; macro-F1 delta = 0.0222.
- **Goalscorer tier (validation)**: log-loss delta vs `rating_plus_form` = 1e-04; improved log loss = FALSE.
- **Goalscorer tier (test)**: log-loss delta vs `rating_plus_form` = 2e-04; improved log loss = FALSE.

## Interpretation
- Pre-match ratings plus lagged team form are the most useful current signals.
- Goalscorer features: No material log-loss improvement vs rating_plus_form.
- Hardest class on the test split for the best validation model: **D**.
- Probability quality (log loss / Brier) matters more than raw accuracy for forecasting and betting use cases.

## Extreme Feature Audit
Rolling form features show long tails (up to 491 values above the 99th percentile in at least one audited column). Examples are saved in `final_extreme_feature_examples.csv`; values were not capped or removed.

## Limitations
- No player-transfer or squad-value data yet.
- No injury or roster-availability data.
- No betting odds or market-implied probabilities in the current feature set.
- Rankings (`home_rank_pre_match`, `away_rank_pre_match`) were unavailable/missing in the input table.
- International football structure changes over long historical periods.

## Recommended Next Steps
1. Calibrate best LightGBM probabilities (Platt scaling or isotonic regression).
2. Test a two-stage draw vs non-draw model to improve draw recall.
3. Add external squad/market-value or roster availability data.
4. Add odds data for betting expected-value analysis.
5. Consider time-decayed training or modern-era-only sensitivity analysis.

## Diagnostics Notes
Calibration bins were produced for the best validation model on the test split (see plot 07).
LightGBM gain importance for `rating_plus_form` highlights rating difference and form-derived features among the top predictors.

## Files Produced
- Tables: `data/model_outputs/final_project_summary`
- Plots: `graphs/final_model_results`
- Key tables: `final_best_model_summary.csv`, `final_incremental_performance_summary.csv`,
  `final_classwise_summary.csv`, `final_extreme_feature_audit.csv`,
  `final_confident_wrong_predictions.csv`, `final_high_confidence_correct_predictions.csv`,
  `final_project_takeaways.csv`.
- Report: `final_international_modeling_report.md`.
