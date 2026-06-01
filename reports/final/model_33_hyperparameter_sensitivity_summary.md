# Model 33 hyperparameter sensitivity summary

**Feature variant:** `safe_plus_form_compact` (20 features)
**Cohort after filters:** 41386 matches (validation 6820, test 7289)

## Validation winner

- **Best configuration:** `slower_small` (lightgbm)
- **Validation log loss:** 0.891727
- **Validation Brier:** 0.525693
- **Validation accuracy:** 0.5864
- **Validation macro F1:** 0.4316

## Comparison to Model 28 / current_final grid

- **Model 28 official validation log loss (artifact):** 0.891517
- **Delta vs Model 28 validation:** -0.00021 (positive = improvement)
- **current_final grid validation log loss:** 0.892184
- **Delta vs current_final grid:** 0.000457
- **Material improvement threshold:** 0.005 log-loss units on validation; modest band 0.003–0.005.

## Test evaluation (single pass for winner)

- **Winner test log loss:** 0.874516
- **Winner test Brier:** 0.515501
- **Winner test accuracy:** 0.5953
- **Winner test macro F1:** 0.4378

## Recommendation

Keep Model 28 (`lightgbm` + `safe_plus_form_compact`) as the official final model; hyperparameter sensitivity did not materially beat the existing selection.

## Notes

- Selection used **validation multiclass log loss only**; test was evaluated once for the winner.
- LightGBM runs set reproducibility seeds (`seed`, `feature_fraction_seed`, `bagging_seed`, `data_random_seed`, `deterministic`, `force_col_wise`).
- `current_final` in this script follows the Model 33 grid spec (e.g. `feature_fraction = 1.0`, `lambda_l2 = 0`); Model 28 code used `feature_fraction = 0.9`, `bagging_fraction = 0.9`, `lambda_l2 = 1`. Official Model 28 test metrics are retained from `model_28_metrics.csv` when available.
- Baselines (`frequency_baseline`, `majority_baseline`, `rating_diff_multinom`) were included for comparison and not tuned.

