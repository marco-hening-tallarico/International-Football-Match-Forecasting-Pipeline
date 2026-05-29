# Notebook 2 EDA Notes

This notebook inspects `data/processed/international_modeling_table.csv`.

Main validation findings to confirm:
- `source_match_id` is unique.
- `data_split` follows the chronological split.
- Elo ratings are strictly pre-match.
- No same-day or post-match Elo ratings are used.
- `rating_diff` equals home pre-match Elo minus away pre-match Elo.

Main modeling-safe rule:
- Use the train split for model development.
- Keep the 2018+ test split untouched for final evaluation.

First candidate model dataset:
- Rows with both home and away pre-match Elo ratings.
- Valid `match_result` in H/D/A.
- Non-missing `rating_diff`.

Recommended first model:
- Multinomial logistic regression.
- Target: `match_result`.
- Predictors: `rating_diff`, `neutral`, and tournament flags.

Do not use post-match columns as predictors:
- `home_score`
- `away_score`
- `goal_difference`
- `total_goals`
- `shootout_winner`
- `home_won_shootout`
- `away_won_shootout`
