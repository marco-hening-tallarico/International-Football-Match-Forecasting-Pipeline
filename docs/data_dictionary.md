# Data Dictionary

Last reviewed: 2026-06-01

Full schema documentation for processed tables. For data provenance see [data_sources.md](data_sources.md).

## Primary table: international_results.csv

**Grain:** one international football match with final score  
**Primary key:** `source_match_id`  
**Scripts:** `08_clean_international_results.R`, `09_validate_international_results.R`

| Column | Type | Definition |
|--------|------|------------|
| `source` | character | Source label (`international_results`) |
| `raw_file` | character | Path to raw CSV used |
| `source_match_id` | character | Unique match identifier |
| `date` | date | Match date |
| `season` | character | Calendar year of match |
| `competition` | character | Competition name |
| `home_team` | character | Home team (normalized) |
| `away_team` | character | Away team (normalized) |
| `home_score` | integer | Final home goals |
| `away_score` | integer | Final away goals |
| `match_result` | character | H / D / A (home perspective) |
| `result_class` | integer | 1 / 0 / -1 |
| `home_win`, `draw`, `away_win` | integer | Binary outcome indicators |
| `goal_difference` | integer | home_score − away_score |
| `total_goals` | integer | home_score + away_score |
| `neutral` | logical | Neutral venue flag |
| `tournament` | character | Tournament name |
| `city`, `country` | character | Host location |

Validation outputs: `data/validation/processed_data/international_results_validation_*.csv`

## Modeling table: international_modeling_table.csv

**Script:** `18_build_international_modeling_table.R`  
**Grain:** one match with pre-match features and split labels

Key columns beyond results:

| Column | Description |
|--------|-------------|
| `home_rating_pre_match`, `away_rating_pre_match` | Elo before kickoff (joined via `data/metadata/team_name_crosswalk.csv` when names differ) |
| `rating_diff` | home − away rating |
| `rating_age_days_home`, `rating_age_days_away` | Days since last Elo update |
| `flag_is_world_cup`, `flag_is_friendly`, … | Tournament context |
| `data_split` | Legacy **train** / **test** only (test from `2018-01-01`; kept for backward compatibility) |
| `data_split_modeling` | Authoritative **train** / **validation** / **test** for modeling (validation = last 20% of pre-2018 train rows by date) |

Elo joins use the latest rating on or before `match_date - 1`. Unresolved result team names (no crosswalk mapping and no matching Elo slug) are logged to `data/validation/processed_data/international_modeling_table_unresolved_elo_teams.csv`.

Extended tables:

| File | Added by |
|------|----------|
| `international_modeling_table_with_form.csv` | Script 27 |
| `international_modeling_table_with_form_and_goalscorers.csv` | Script 29 |

## Secondary processed files

| File | Role |
|------|------|
| `international_goalscorers.csv` | Goal-level events |
| `international_shootouts.csv` | Penalty shootout records |
| `international_results_with_shootouts.csv` | Results + shootout metadata |
| `international_team_ratings.csv` | Clean Elo history |
| `statsbomb_matches.csv` | Club match metadata (optional) |
| `football_data_uk_matches.csv` | Club results (optional) |

## Validation file layout

| Subfolder | Contents |
|-----------|----------|
| `processed_data/` | Cleaning and join QA for raw tables |
| `engineered_features/` | Lagged form and goalscorer leakage audits |
| `modeling/` | Legacy modeling validation outputs |
