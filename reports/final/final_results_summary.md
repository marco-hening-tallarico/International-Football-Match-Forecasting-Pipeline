# Soccer-R-Verse Final Modeling Summary

## Goal
Forecast international association football match outcomes as calibrated
multiclass probabilities for home win (H), draw (D), and away win (A).

## Data and leakage controls
- One row per match keyed by `source_match_id`.
- Chronological train, validation, and held-out test splits.
- Strict pre-match features only (approved Elo/tournament context).
- Lagged team-form features (Model 27/28) built from prior matches only.
- Test split reserved for final reporting; model selection uses validation log loss.

## Modeling progression
| Stage | Best model | Feature variant | Val log loss | Test log loss | Test accuracy | Test macro F1 |
| --- | --- | --- | --- | --- | --- | --- |
| baseline | draw_aware_quadratic_multinom | — | 0.893 | 0.872 | 0.597 | 0.438 |
| model_24_safe_features | lightgbm | — | 0.894 | 0.876 | 0.594 | 0.436 |
| model_26_draw_aware_elo | lightgbm | draw_abs_only | 0.894 | 0.877 | 0.592 | 0.434 |
| model_28_lagged_form | lightgbm | safe_plus_form_compact | 0.892 | 0.874 | 0.595 | 0.438 |

## Final selected model
- Selected by **validation log loss**, not test performance.- Final stage: Model 28 (`safe_plus_form_compact` + `lightgbm`).

## Performance
- Validation log loss: **0.892**.
- Test log loss: **0.874**.
- Test accuracy: **0.595**; test macro F1: **0.438**.

## Draw behavior
- Mean predicted draw probability (`mean_pred_D`) is often in a plausible range.
- Draw is rarely the top predicted class in earlier Model 24/25 diagnostics.
- Lagged form (Model 28) improved draw recall modestly but did not fully resolve draw ranking.

## Interpretation
- Elo/simple baselines are already strong; supervised models refine probabilities modestly.
- Safe-feature learners in Model 24 were effectively tied.
- Draw-aware Elo transforms (Model 26) did not materially move validation metrics.
- Compact lagged form gave the clearest incremental gain, but gains are modest.
- The project is a solid MVP forecasting baseline, not yet state-of-the-art.

## Future work
- Incorporate market odds as features or calibration anchors.
- Add squad value and player availability signals.
- Build tournament simulation on top of match-level probabilities.
- Post-process probabilities for improved calibration.
- Tune LightGBM hyperparameters and class weights for draws.
- Add explainability (e.g., SHAP) for feature attribution.
- Wire the selected model into a World Cup forecast pipeline.

## Generated artifacts
### Tables
- `reports/tables/final_incremental_model_progress.csv`
- `reports/tables/final_best_models_by_stage.csv`
- `reports/tables/final_selected_model_test_metrics.csv`
- `reports/tables/final_model_comparison.csv`
- `reports/tables/final_draw_diagnostics_summary.csv`
- `reports/tables/final_prediction_examples.csv`
- `reports/tables/final_project_summary.csv`

### Figures
- `reports/figures/final_incremental_validation_log_loss.png`
- `reports/figures/final_incremental_test_log_loss.png`
- `reports/figures/final_incremental_accuracy_macro_f1.png`
- `reports/figures/final_incremental_draw_metrics.png`
- `reports/figures/final_model_confusion_heatmap.png`
- `reports/figures/final_model_calibration_plot.png`
- `reports/figures/final_model_prediction_confidence.png`
- `reports/figures/final_model_probability_distribution.png`
- `reports/figures/final_modeling_stage_summary.png`
