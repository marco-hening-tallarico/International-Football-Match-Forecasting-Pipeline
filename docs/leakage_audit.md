# Leakage Audit

## Target variable

**`match_result`** (H / D / A) — derived from final scores. This is the prediction target and must never appear as a model input.

Excluded derived columns: `home_score`, `away_score`, `goal_difference`, `total_goals`, `home_win`, `draw`, `away_win`, `result_class`.

## Allowed pre-match features

Features must be knowable **before kickoff** on the match date:

| Group | Examples | Built in |
|-------|----------|----------|
| Elo ratings | `home_rating_pre_match`, `away_rating_pre_match`, `rating_diff` | 18 (crosswalk + pre-match join) |
| Rating freshness | `rating_age_days_home`, `rating_age_days_away` | 18 |
| Tournament context | `flag_is_world_cup`, `flag_is_friendly`, `neutral`, etc. | 18 |
| Lagged form | `home_points_per_match_last_5`, `away_goal_diff_per_match_last_10`, … | 27 |
| Goalscorer depth | `home_unique_scorers_last_10`, `home_goals_by_top_scorer_last_10`, … | 29 |

Approved lists are frozen in `reports/tables/approved_feature_sets_final.R` (script 22).

## Excluded post-match / in-match columns

- All score and outcome fields (see above).
- Shootout winner fields used only for join QA, not as model inputs.
- Same-match goalscorer minutes and counts.
- Any column computed using goals scored **on or after** the fixture date.

## Lagged feature validation

Script `27_build_lagged_team_form_features.R` builds form using only matches with `date < current_match_date`. Kickoff times are unavailable, so same-calendar-day fixtures for the same team are excluded from lagged windows (conservative; avoids same-day leakage).

Checks written to `data/validation/engineered_features/`:

| File | Check |
|------|-------|
| `lagged_form_leakage_check.csv` | No future goals in rolling windows |
| `lagged_form_feature_summary.csv` | Distribution sanity |
| `lagged_form_missingness.csv` | Cold-start missingness for early careers |

Script `30b_validate_engineered_features.R` adds:

- Manual recompute on sampled rows (`goalscorer_feature_manual_recompute_check.csv`)
- Leakage audit comparing goal dates vs match dates (`goalscorer_feature_leakage_audit.csv`)
- Schema, range, and diff-vs-form-table checks

## Known leakage risks

| Risk | Status | Mitigation |
|------|--------|------------|
| Same-match goals in form features | **Audited — pass** | Strict date filter in 27, 29 |
| Elo rating after the match | **Controlled** | Pre-match rating join in 18 (`rating_lookup_date = date - 1`) with hard timing checks; result team names mapped via crosswalk before join |
| Unknown team coerced to another Elo team | **Avoided** | Crosswalk provides explicit `elo_team_clean` only; otherwise lookup slug is unchanged and join may return `NA` (logged) |
| Train/test random shuffle | **Avoided** | Chronological split by date |
| Test set used for selection | **Avoided** | Selection on validation log loss only |
| Goalscorer features using future scorers | **Audited — pass** | 30b leakage audit |

## Validation status

Last engineered-feature validation: see `data/validation/engineered_features/engineered_feature_validation_notes.md`.

All sampled leakage checks in the goalscorer audit report `passed = TRUE` for rows with sufficient history. Rows with zero prior matches correctly show `NA` form fields, not zero-filled future information.

## Reviewer quick check

```bash
Rscript src/30b_validate_engineered_features.R
```

Inspect `goalscorer_feature_leakage_audit.csv` — column `passed` should be `TRUE` for all audited rows with history.
