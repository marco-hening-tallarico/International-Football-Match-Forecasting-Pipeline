# Data Collection Gap Report
_Generated: 2026-05-28 12:06:12 EDT_

---

## 1. Current Pipeline Script Order

The pipeline is orchestrated by `src/run_pipeline.R`, which `source()`s each script in order:

| Step | Script | Purpose |
|------|--------|---------|
| 1 | `src/00_project_setup.R` | Create folder structure, define path constants |
| 2 | `src/01_packages.R` | Install and load required R packages |
| 3 | `src/02_helpers.R` | Load reusable helper functions (download, read, manifest) |
| 4 | `src/03_download_statsbomb_competitions.R` | Download StatsBomb `competitions.json`; produce `statsbomb_competitions.csv/.rds` |
| 5 | `src/04_download_statsbomb_matches.R` | Download one match JSON per competition-season; produce `statsbomb_matches.csv/.rds` |
| 6 | `src/05_download_football_data_uk.R` | Download 15 league × 31 season CSVs from football-data.co.uk |
| 7 | `src/06_clean_football_data_uk.R` | Combine all raw CSVs into `football_data_uk_matches.csv/.rds` |
| 8 | `src/07_download_international_results.R` | Download `results.csv`, `goalscorers.csv`, `shootouts.csv` from martj42 |
| 9 | `src/08_clean_international_results.R` | Clean `results.csv` into `international_results.csv/.rds` |
| 10 | `src/09_validate_international_results.R` | Detailed row-level validation of `international_results.csv`; writes 3 validation CSVs |
| 11 | `src/validation.R` | Hard pipeline validation: file existence, row counts, derived-field consistency, manifest coverage |
| 12 | `src/10_plot_international_results_validation.R` | Validation plots and train/test split for international results |
| 13 | `src/11_international_results_analysis.R` | EDA, leakage-safe feature engineering, multinomial logistic baseline, metrics |

> **Note:** `src/validation.R` appears _between_ the plotting scripts in `run_pipeline.R` (position 11), which is unusual — it runs _after_ `09_validate_international_results.R` but _before_ the plots.

---

## 2. Current Raw Datasets Downloaded

### 2.1 StatsBomb Open Data (`data/raw/statsbomb_open/`)

| File / Folder | Status | Count / Notes |
|---|---|---|
| `competitions.json` | ✅ Downloaded | All competition-seasons available in the open-data repo |
| `matches/<competition_id>_<season_id>.json` | ✅ Downloaded | **80 JSON files** (one per competition-season) |
| `events/<match_id>.json` | ❌ Not downloaded | One JSON per match; contains all on-ball events |
| `lineups/<match_id>.json` | ❌ Not downloaded | One JSON per match; team lineups and positions |
| `three-sixty/<match_id>.json` | ❌ Not downloaded | Subset of matches; 360-degree player tracking frames |

**Competitions covered by StatsBomb Open Data (processed):**

1. Bundesliga, African Cup of Nations, Champions League, Copa America, Copa del Rey, FA Women's Super League, FIFA U20 World Cup, FIFA World Cup, Frauen Bundesliga, Indian Super league, La Liga, Liga F, Liga Profesional, Ligue 1, Major League Soccer, North American League, NWSL, Premier League, Serie A, Serie A Women, UEFA Euro, UEFA Europa League, UEFA Women's Euro, Women's World Cup

### 2.2 football-data.co.uk (`data/raw/football_data_uk/`)

- **Seasons:** 1993/94 – 2023/24 (31 season folders)
- **Leagues:** 15 league codes: E0, E1, E2, E3, SP1, SP2, D1, D2, I1, I2, F1, F2, N1, P1, SC0
- **Total CSV files:** 464 (some older league/season combinations do not exist upstream)
- **Content per file:** Date, home/away teams, full-time and half-time scores, shots, shots on target, corners, fouls, yellow/red cards, betting odds (where available)
- **Missing from download:** The "extra" / worldwide leagues (Belgium B1, Greece G1, Turkey T1, Argentina, Brazil, etc.) listed at `football-data.co.uk/new_leagues_data.php` are **not** in the current `league_codes` vector.

### 2.3 martj42/international_results (`data/raw/international_results/`)

| File | Status | Rows | Notes |
|---|---|---|---|
| `results.csv` | ✅ Downloaded | 49257 (complete matches) | Core international match results used in pipeline |
| `goalscorers.csv` | ✅ Downloaded | 47601 | Player + minute + OG/penalty flags – **not yet cleaned or processed** |
| `shootouts.csv` | ✅ Downloaded | 677 | Penalty shootout outcomes – **not yet cleaned or processed** |

---

## 3. Current Processed Datasets

All processed files live in `data/processed/`.

| File | Rows | Date Range | Schema Highlights |
|---|---|---|---|
| `statsbomb_competitions.csv/.rds` | 80 | — | competition_id, season_id, country_name, competition_name, season_name, gender, youth/international flags |
| `statsbomb_matches.csv/.rds` | 3961 | 1958-06-24 – 2025-07-27 | Standard match schema + competition_id, season_id, match_week, stadium, data/shot/xy version |
| `football_data_uk_matches.csv/.rds` | 177295 | 1993-07-23 – 2024-06-02 | Standard match schema + league/season code, half-time scores, 14 match-stats columns |
| `international_results.csv/.rds` | 49257 | 1872-11-30 – 2026-03-31 | Standard match schema + tournament, city, country, neutral flag |

**Common match schema** (all four processed tables share this core):
`source, raw_file, source_match_id, date, season, competition, home_team, away_team, home_score, away_score, match_result, result_class, home_win, draw, away_win, goal_difference, total_goals, neutral`

---

## 4. Current Validation Checks

### 4.1 Hard pipeline checks (`src/validation.R`)

- **File existence:** All 4 processed CSVs must exist before validation begins.
- **Fixed row-count assertions:** Exact row counts for all 4 processed tables are hard-coded (`assert_count`). Any data refresh that changes counts will fail the pipeline intentionally.
- **Match-table schema:** Every processed match table is validated for required columns, no-missing required values, non-negative scores, correct derived fields (`match_result`, `result_class`, `home_win`, `draw`, `away_win`, `goal_difference`, `total_goals`).
- **StatsBomb FK integrity:** Every `(competition_id, season_id)` in `statsbomb_matches.csv` must exist in `statsbomb_competitions.csv`.
- **football-data.co.uk stat columns:** Nullable integer checks; verifies no fake zero-imputation when the raw file lacked the stat column.
- **Raw-to-processed coverage:** Every downloaded raw file must be represented in the processed output (or listed as a known skip exception).
- **Source manifest completeness:** Every downloaded file must appear in `data/metadata/source_manifest.csv`.

### 4.2 International results deep validation (`src/09_validate_international_results.R`)

Writes three CSVs to `data/validation/`:

| Check | Severity | Type |
|---|---|---|
| Required schema columns present | error | Schema |
| No unexpected processed columns | warning | Schema |
| Required raw columns present | error | Schema |
| No missing required values | error | Completeness |
| `source` field is constant | error | Provenance |
| `raw_file` points to `results.csv` | error | Provenance |
| `source_match_id` unique | error | Uniqueness |
| No exact duplicate match rows | error | Uniqueness |
| Same date/home/away/tournament keys | warning | Uniqueness |
| Same team in multiple matches same day | warning | Uniqueness |
| Scores non-negative integers | error | Domain |
| Home team ≠ away team | error | Domain |
| Date valid and not future | error | Domain |
| `season` = calendar year of date | error | Domain |
| `competition` mirrors `tournament` | error | Consistency |
| Derived result fields consistent | error | Consistency |
| Text fields whitespace-squished | error | Formatting |
| `neutral` fully populated | error | Completeness |
| Extreme scorelines flagged | warning | QA review |
| Raw complete rows reconcile to processed rows | error | Reconciliation |
| Raw incomplete fixtures excluded from processed | info | Reconciliation |

### 4.3 Validation outputs (`data/validation/`)

| File | Content |
|---|---|
| `international_results_validation_checks.csv` | One row per check with severity/status/rows_affected |
| `international_results_validation_summary.csv` | Aggregate metrics (dates, teams, tournaments, etc.) |
| `international_results_validation_examples.csv` | Example rows flagged by each check |

**Gaps in current validation:**
- `football_data_uk_matches.csv` and `statsbomb_matches.csv` do **not** have a deep, check-by-check validation report analogous to the international results report.
- `goalscorers.csv` and `shootouts.csv` are downloaded but have **no validation at all** (not even an existence check in `validation.R` beyond raw file listing).
- `statsbomb_competitions.csv` has only a primary-key uniqueness check and a fixed row count, but **no schema validation report**.

---

## 5. Missing Datasets from Already-Used Sources

### 5.1 StatsBomb Open Data – not yet collected

| Missing Dataset | Available At | Why It Matters |
|---|---|---|
| **Match events** (`events/<match_id>.json`) | GitHub `open-data/data/events/` | On-ball event log per match: shots, passes, dribbles, carries, pressures, etc. Essential for xG, pass networks, and player-level features. |
| **Lineups** (`lineups/<match_id>.json`) | GitHub `open-data/data/lineups/` | Starting XI + substitutes with player IDs, jersey numbers, positions. Needed to link events to players and build squad-level features. |
| **Three-sixty tracking** (`three-sixty/<match_id>.json`) | GitHub `open-data/data/three-sixty/` | Freeze-frame player positions for selected events; subset of matches only. Enables spatial and pressure-map features. |
| **Player-level aggregate table** (derived) | Derived from events + lineups | Minutes played, goals, assists, xG, pressures per player per match. Not available as a raw file; must be built from events. |

### 5.2 football-data.co.uk – extra / worldwide leagues not downloaded

The current `league_codes` in `05_download_football_data_uk.R` covers 15 European leagues.
The site also provides additional leagues under `/new_leagues_data.php` and `/new_leagues_data_extra.php`:

| League Code | Country / League | Approximate Seasons Available |
|---|---|---|
| `B1` | Belgium First Division A | ~2000– |
| `G1` | Greece Super League | ~2000– |
| `T1` | Turkey Süper Lig | ~2000– |
| `A1` | Argentina Primera División | ~2012– |
| `BRA1` | Brazil Série A | ~2012– |
| `MX1` | Mexico Liga MX | ~2012– |
| `J1` | Japan J-League | ~2012– |
| `CH1` | Switzerland Super League | ~2011– |
| `OE1` | Austria Bundesliga | ~2011– |
| `ARG` | Argentina alternate feed | ~2012– |

> These are **not included** in the current download list and represent untapped coverage of non-European club football.

### 5.3 martj42/international_results – downloaded but not processed

| Raw File | Rows | Currently Used? | What Is Missing |
|---|---|---|---|
| `goalscorers.csv` | 47601 | ❌ No | Goalscorer name, minute, own-goal flag, penalty flag per match event. No cleaning, no processed table, no validation. |
| `shootouts.csv` | 677 | ❌ No | Penalty shootout winner, first kick, home/away outcomes. No cleaning, no processed table, no validation. |

**Impact of missing goalscorers processing:**
- Cannot compute goals scored by individual players across tournaments.
- Cannot flag penalty-kick goals vs. open-play goals.
- Cannot build a player appearance / goal history table for international squads.

**Impact of missing shootouts processing:**
- Cannot identify matches where the result was decided on penalties (as opposed to normal time or extra time).
- `international_results.csv` currently records draws for all matches that go to penalties, which overstates draw frequency for knockout rounds.
- World Cup and major tournament knockout modeling will be incorrect without a shootout flag.

---

## 6. Recommended New Scripts

These scripts address the identified gaps without adding any new external sources.

| Priority | Script | Inputs | Outputs | Addresses Gap |
|---|---|---|---|---|
| **High** | `12_clean_goalscorers.R` | `raw/international_results/goalscorers.csv` | `processed/intl_goalscorers.csv/.rds` | Processes goalscorers.csv into clean event table |
| **High** | `13_clean_shootouts.R` | `raw/international_results/shootouts.csv` | `processed/intl_shootouts.csv/.rds` | Processes shootouts.csv; adds shootout winner/flag |
| **High** | `14_download_statsbomb_events.R` | `processed/statsbomb_matches.csv` | `raw/statsbomb_open/events/<match_id>.json` | Downloads all StatsBomb event JSONs |
| **High** | `15_download_statsbomb_lineups.R` | `processed/statsbomb_matches.csv` | `raw/statsbomb_open/lineups/<match_id>.json` | Downloads all StatsBomb lineup JSONs |
| **Medium** | `16_clean_statsbomb_events.R` | Event JSONs | `processed/statsbomb_events.csv/.rds` | Flattens event data to one row per event |
| **Medium** | `17_clean_statsbomb_lineups.R` | Lineup JSONs | `processed/statsbomb_lineups.csv/.rds` | Flattens lineup data to player-match rows |
| **Medium** | `18_download_statsbomb_threesixty.R` | `processed/statsbomb_matches.csv` | `raw/statsbomb_open/three-sixty/<match_id>.json` | Downloads 360 freeze-frame JSONs for eligible matches |
| **Medium** | `19_clean_statsbomb_threesixty.R` | 360 JSONs | `processed/statsbomb_threesixty.csv/.rds` | Flattens 360 freeze-frame data |
| **Low** | `20_download_fd_uk_extra_leagues.R` | — | `raw/football_data_uk_extra/<league>/<season>.csv` | Downloads extra/worldwide leagues from football-data.co.uk |
| **Low** | `21_clean_fd_uk_extra_leagues.R` | Extra league CSVs | `processed/football_data_uk_extra_matches.csv/.rds` | Cleans extra league files using same logic as script 06 |
| **Low** | `22_validate_statsbomb_deep.R` | `processed/statsbomb_*.csv` | `data/validation/statsbomb_validation_checks.csv` | Deep validation for StatsBomb tables (analogous to script 09) |
| **Low** | `23_validate_football_data_uk_deep.R` | `processed/football_data_uk_matches.csv` | `data/validation/football_data_uk_validation_checks.csv` | Deep validation for football-data.co.uk table |

---

## 7. Recommended New Processed Tables

| Table | Source | Schema Highlights | Blocking For |
|---|---|---|---|
| `intl_goalscorers.csv` | `goalscorers.csv` | match_id, player, team, minute, own_goal, penalty, date, tournament | Player goal history; OG/penalty tagging |
| `intl_shootouts.csv` | `shootouts.csv` | date, home_team, away_team, tournament, winner, first_kicker_home | Correct shootout results; penalty-win flag for knockout modeling |
| `intl_results_with_shootout_flag.csv` | `international_results` + `intl_shootouts` | All columns of international_results + `decided_by_shootout` + `shootout_winner` | Accurate outcome labeling for knockout rounds |
| `statsbomb_events.csv` | StatsBomb events JSONs | match_id, event_id, type, period, minute, second, team, player, location_x, location_y, outcome | xG computation; passing networks; pressing features |
| `statsbomb_lineups.csv` | StatsBomb lineup JSONs | match_id, team, player_id, player_name, jersey_number, position, country | Squad composition; player linkage to events |
| `statsbomb_shots.csv` (derived) | `statsbomb_events` | match_id, event_id, minute, team, player, xg, shot_outcome, technique, body_part | Shot-level xG table |
| `statsbomb_threesixty.csv` | StatsBomb 360 JSONs | match_id, event_id, visible_area, actor frame, teammate/opponent positions | Spatial pressure features |
| `football_data_uk_extra_matches.csv` | football-data.co.uk extra leagues | Same schema as `football_data_uk_matches.csv` | Broader club form coverage |

---

## 8. Risks and Limitations

### 8.1 Hard-coded row counts in `validation.R`

`src/validation.R` uses `assert_count()` with hard-coded expected values:
- `football_data_uk_matches.csv`: 177,295 rows
- `statsbomb_matches.csv`: 3,961 rows
- `statsbomb_competitions.csv`: 80 rows
- `international_results.csv`: 49,257 rows
- Raw football-data.co.uk CSVs: 464 files
- Raw StatsBomb match JSONs: 80 files

**Risk:** Any upstream data refresh (StatsBomb adds a new competition-season; martj42 adds new international matches; football-data.co.uk updates a current-season file) will cause the pipeline to fail until these constants are manually updated.

### 8.2 StatsBomb events / lineups not collected

The most analytically rich StatsBomb assets (events, lineups, 360) are not yet downloaded. Downloading all events for 3,961 matches will require significant network I/O and disk space (event files average ~500 KB each → ~2 GB total for all events). The current `safe_download` with `overwrite = FALSE` handles incremental updates correctly.

### 8.3 Shootout outcomes inflate draw rates

`international_results.csv` records penalty-shootout knockout matches as draws (the score at full time). Without joining `shootouts.csv`, any modeling of international match outcomes in knockout rounds will overestimate the draw probability and have no signal for the actual winner.

### 8.4 Goalscorers and lineups are completely unprocessed

`goalscorers.csv` (
47601
 rows) and `shootouts.csv` (
677
 rows) are downloaded and tracked in the source manifest, but have no cleaning scripts, no validation, and are not used anywhere in the pipeline.

### 8.5 football-data.co.uk betting odds not extracted

The raw CSVs contain many betting-odds columns (B365, BW, IW, PS, WH, VC, etc.) that are **silently dropped** during cleaning in `06_clean_football_data_uk.R`. These are potentially valuable calibration features for any probability model and should be explicitly retained or documented as intentionally excluded.

### 8.6 No cross-source entity resolution

Team names differ between sources (e.g., "Real Madrid" vs "Real Madrid CF" vs "Real Madrid Club de Fútbol"). There is currently no shared team name harmonization table, which will make cross-source joins unreliable.

### 8.7 `validation.R` is not in `run_pipeline.R` in alphabetical position

`validation.R` is listed at position 11 in `run_pipeline.R` (between `09_validate_international_results.R` and `10_plot_international_results_validation.R`), but its file name has no numeric prefix. This breaks the convention established by all other scripts and makes the ordering ambiguous.

### 8.8 `11_international_results_analysis.R` trains a model inside the data pipeline

`11_international_results_analysis.R` fits a multinomial logistic regression and saves the model object to `outputs/validation/`. This is a modeling step embedded in the data collection pipeline, which may cause confusion and could slow down data-refresh runs significantly as the dataset grows.

---

## Summary Table

| Category | Count |
|---|---|
| Pipeline scripts (in run_pipeline.R) | 13 |
| Raw source directories | 3 |
| Processed tables | 4 |
| Validation output files | 3 |
| StatsBomb raw assets NOT downloaded | 3 (events, lineups, 360) |
| martj42 raw files NOT processed | 2 (goalscorers, shootouts) |
| football-data.co.uk leagues NOT downloaded | ~10 extra/worldwide leagues |
| Recommended new scripts | 12 |
| Recommended new processed tables | 8 |

