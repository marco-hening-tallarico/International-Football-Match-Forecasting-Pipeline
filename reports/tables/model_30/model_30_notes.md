# Model 30 Notes

## Data and filtering
- Input file: data/processed/international_modeling_table_with_form_and_goalscorers.csv
- Input rows: 49257
- Fair-comparison cohort rows: 44054

### Filter steps
- input_rows: 49257
- valid_result_class: 49257
- required_pre_match_ratings_present: 44128
- fair_comparison_complete_cases: 44054
- sensitivity_cohort_with_avg_goal_minute: 21930

## Feature sets
- baseline_rating (n=4, fair_comparison=TRUE)
- rating_plus_context (n=10, fair_comparison=TRUE)
- rating_plus_form (n=44, fair_comparison=TRUE)
- rating_plus_form_plus_goalscorers (n=52, fair_comparison=TRUE)
- rating_plus_form_plus_goalscorers_with_avg_minute (n=53, fair_comparison=FALSE)

## Skipped features
Skipped unavailable or all-missing features: home_rank_pre_match, away_rank_pre_match
home_rank_pre_match and away_rank_pre_match were excluded (100% missing in input table).
- avg_goal_minute_diff_365d excluded from main fair-comparison union due to high missingness (~52%).

## Modeling notes
LightGBM was used for tree-based models.
tournament encoded via training-set frequency lumping (min_count=100) into tournament_lumped; no target encoding used.
- glmnet available: TRUE
- Models trained per feature set: multinom, glmnet_multinomial_ridge, lightgbm

## Sensitivity analysis
Optional sensitivity variant including avg_goal_minute_diff_365d used 21930 rows (vs 44054 in main fair-comparison cohort).

## Best models
- Best validation model (log loss): rating_plus_form / lightgbm (log_loss=0.8888)
- Best test model (log loss): rating_plus_form / lightgbm (log_loss=0.8696)

## Goalscorer uplift vs rating_plus_form (best log loss model per split)
- test: log_loss improved=FALSE, brier improved=FALSE, macro_f1 improved=FALSE
- validation: log_loss improved=FALSE, brier improved=FALSE, macro_f1 improved=TRUE
- Goalscorer features helped on validation: TRUE
- Goalscorer features helped on test: FALSE

## Draw F1 comparison (best model per feature set)
- test / glmnet_multinomial_ridge: draw F1 form=0.0642, goalscorers=0.0662, improved=TRUE
- validation / glmnet_multinomial_ridge: draw F1 form=0.0493, goalscorers=0.0493, improved=FALSE
- test / lightgbm: draw F1 form=0.0619, goalscorers=0.0596, improved=FALSE
- validation / lightgbm: draw F1 form=0.0599, goalscorers=0.0637, improved=TRUE
- test / multinom: draw F1 form=0.0851, goalscorers=0.0889, improved=TRUE
- validation / multinom: draw F1 form=0.0711, goalscorers=0.0806, improved=TRUE

- Generated at: 2026-05-29 17:13:50 EDT
