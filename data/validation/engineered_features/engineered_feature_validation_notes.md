# Engineered feature validation notes

- Validation run: 2026-05-29 17:19:22.207444
- Input table: /Users/marco/Documents/Projects/worldcup-forecast-r/data/processed/international_modeling_table_with_form_and_goalscorers.csv
- Input row count: 49257
- Engineered features checked: 61
- Validation checks passed: 8 / 8
- Validation checks failed: 0
- Modeling table safe for modeling: yes

## Largest missingness features
- avg_goal_minute_diff_365d: 51.66% missing
- home_avg_goal_minute_365d: 42.48% missing
- away_avg_goal_minute_365d: 42.18% missing
- form_points_diff_last_5: 0.61% missing
- form_goal_diff_diff_last_5: 0.61% missing

## Range violations
- None

## Leakage violations
- None in sampled goalscorer audit

## Manual recomputation mismatches
- None in sampled goalscorer audit

## Caveats
- Goalscorer leakage and recomputation checks use a random sample of 25 matches.
- form_draw_rate_mean_* features are means, not home-minus-away differences.
- Early-career teams may legitimately have NA form or goalscorer history.
- Full feature distributions are in the multi-page PDF; PNG shows an overview subset.
- Form-only table available at: /Users/marco/Documents/Projects/worldcup-forecast-r/data/processed/international_modeling_table_with_form.csv
