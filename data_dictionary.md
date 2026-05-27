# Data Dictionary

Last reviewed: 2026-05-27

## Primary Validation Target

`data/processed/international_results.csv` is the main dataset for this international match forecasting project. The current workflow validates it as a historical match-result table only. It does not create predictive features, ratings, form measures, rest-day fields, team-strength estimates, model-ready tables, or training data.

Grain: one international football match with a final score.

Primary key: `source_match_id`.

Producing script: `src/08_clean_international_results.R`.

Validation script: `src/09_validate_international_results.R`.

Validation outputs:

| File | Purpose |
| --- | --- |
| `data/validation/international_results_validation_checks.csv` | One row per validation check, with severity, status, affected-row count, and details. |
| `data/validation/international_results_validation_summary.csv` | Small run summary with row counts, date range, entity counts, result counts, and validation status totals. |
| `data/validation/international_results_validation_examples.csv` | Row-level examples for checks that need human review, such as repeated fixture keys or extreme scorelines. |

## International Results Columns

| Column | Type | Nullable | Definition / validation rule |
| --- | --- | --- | --- |
| `source` | character | No | Constant source label: `international_results`. |
| `raw_file` | character | No | Local raw file used to produce the row; should point to `data/raw/international_results/results.csv`. |
| `source_match_id` | character | No | Generated match identifier; must be unique. |
| `date` | date | No | Match date; must parse and must not be in the future at validation time. |
| `season` | character | No | Calendar year of `date`; must equal `year(date)`. |
| `competition` | character | No | Common match-table competition field; must match `tournament`. |
| `home_team` | character | No | Home team name after whitespace normalization; must differ from `away_team`. |
| `away_team` | character | No | Away team name after whitespace normalization; must differ from `home_team`. |
| `home_score` | integer | No | Final home goals; must be present, integer-valued, and non-negative. |
| `away_score` | integer | No | Final away goals; must be present, integer-valued, and non-negative. |
| `match_result` | character | No | Home-perspective result; must be `H`, `D`, or `A` and agree with scores. |
| `result_class` | integer | No | Numeric outcome label; `1` home win, `0` draw, `-1` away win. |
| `home_win` | integer | No | Must be `1` only when `match_result == "H"`. |
| `draw` | integer | No | Must be `1` only when `match_result == "D"`. |
| `away_win` | integer | No | Must be `1` only when `match_result == "A"`. |
| `goal_difference` | integer | No | Must equal `home_score - away_score`. |
| `total_goals` | integer | No | Must equal `home_score + away_score`. |
| `neutral` | logical | No | Source-provided neutral-site flag; must be populated. |
| `tournament` | character | No | Source tournament name after whitespace normalization. |
| `city` | character | No | Host city after whitespace normalization. |
| `country` | character | No | Host country after whitespace normalization. |

## Validation Workflow

Run the workflow after downloading and cleaning the international results source:

```sh
Rscript src/09_validate_international_results.R
```

The script performs hard error checks for schema, required values, primary-key uniqueness, score domains, date domains, result derivations, text normalization, neutral-site coverage, and exact reconciliation between complete scored raw rows and the processed table.

It also records warning-level review items for historically plausible but inspection-worthy rows, including repeated date/team/tournament fixture keys, teams appearing more than once on the same date, and extreme scorelines. These warnings are not model features and are not used to alter the data automatically.

## Secondary Files

The project also contains:

| File | Role in this validation task |
| --- | --- |
| `data/processed/statsbomb_matches.csv` | Secondary match metadata. Not a primary validation target for international forecasting. |
| `data/processed/statsbomb_competitions.csv` | Secondary StatsBomb competition metadata. Existing pipeline checks cover basic consistency. |
| `data/processed/football_data_uk_matches.csv` | Club match data. Existing pipeline checks cover basic consistency, but this workflow does not expand club validation. |

