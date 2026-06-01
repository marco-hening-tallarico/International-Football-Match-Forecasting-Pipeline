# Data Sources

## martj42/international_results

| Item | Detail |
|------|--------|
| Source | [GitHub — martj42/international_results](https://github.com/martj42/international_results) |
| Raw files | `data/raw/international_results/results.csv`, `goalscorers.csv`, `shootouts.csv` |
| Scripts | `07_`, `08_`, `08b_`, `08c_`, `14_` |
| Processed | `international_results.csv`, `international_goalscorers.csv`, `international_shootouts.csv`, `international_results_with_shootouts.csv` |

**Strengths:** Long historical coverage, goalscorer and shootout tables, widely used in research.  
**Limitations:** Team names need crosswalk for Elo join; no xG or market odds.

## World Football Elo

| Item | Detail |
|------|--------|
| Source | [eloratings.net](http://www.eloratings.net/) |
| Raw file | `data/raw/international_ratings/world_football_elo.csv` |
| Scripts | `15_`–`18_` |
| Processed | `international_team_ratings.csv`, `international_modeling_table.csv` |
| Crosswalk | `data/metadata/team_name_crosswalk.csv` |

**Strengths:** Pre-match strength signal with long history; strong baseline predictor.  
**Limitations:** Download can fail offline; team name mismatches require crosswalk; rating staleness varies by team.

## StatsBomb Open Data

| Item | Detail |
|------|--------|
| Source | [statsbomb/open-data](https://github.com/statsbomb/open-data) |
| Raw | `data/raw/statsbomb_open/` |
| Scripts | `03_`–`04g_` |
| Processed | `statsbomb_matches.csv`, `statsbomb_events.csv`, etc. |

**Strengths:** Rich event-level detail for club competitions.  
**Limitations:** Not the primary international forecasting dataset; event/360 files are very large and partially gitignored.

## football-data.co.uk

| Item | Detail |
|------|--------|
| Source | [football-data.co.uk](https://www.football-data.co.uk/) |
| Raw | `data/raw/football_data_uk/` |
| Scripts | `05_`, `06_`, `13_` |
| Processed | `football_data_uk_matches.csv`, `football_data_uk_modeling_table.csv` |

**Strengths:** Club results with bookmaker odds (potential future calibration anchor).  
**Limitations:** Separate from international match table; wide odds file exceeds GitHub size limits.

## Metadata

| File | Purpose |
|------|---------|
| `data/metadata/source_manifest.csv` | Download log |
| `data/metadata/team_name_crosswalk.csv` | Maps result team names → Elo lookup slugs (`elo_team`, `elo_team_clean`); applied in scripts 17–18 before pre-match rating joins. Unresolved teams are logged, not coerced. |
| `data/metadata/data_inventory.csv` | File inventory from `11_build_data_inventory.R` |
