# Script map

A reviewer-oriented guide to what lives in `src/`, which paths matter for the **international match forecasting** deliverable, and how the optional heavy tracks fit in.

Scripts are numbered by dependency within each track. **Do not rely on renumbering**—orchestrators (`run_*.R`) are the supported entry points.

---

## Three parallel tracks

| Track | Question it answers | Fed into final international model? |
|-------|---------------------|--------------------------------------|
| **International** | Who wins/draws/loses on the full historical international table? | **Yes** — this is the project |
| **StatsBomb** | Can we ingest open club event/lineup/360 data cleanly? | **No** — optional infrastructure |
| **Club / odds** | Can we build a club match table with bookmaker odds? | **No** — side exploration for future work |

The headline result (LightGBM + compact lagged form, Model 28) uses only the international track: martj42 results, World Football Elo, and engineered pre-match features.

---

## Orchestrators (start here)

| Script | What it runs | When to use |
|--------|--------------|-------------|
| `run_light_pipeline.R` | International download → clean → validate → plots; Elo + modeling table if ratings raw file exists | **Default data rebuild** |
| `run_modeling_pipeline.R` | Baselines → feature review → ML stages → final report | After `international_modeling_table.csv` exists |
| `run_pipeline.R` | StatsBomb + club + international + inventory + global validation | Full monorepo rebuild (slow) |
| `run_statsbomb_pipeline.R` | StatsBomb only (`03`–`04g`) | Optional; hours of I/O |
| `run_club_pipeline.R` | football-data.co.uk only (`05`, `06`, `13`) | Optional club/odds table |
| `run_analysis_pipeline.R` | Legacy exploratory analysis (`11_international_results_analysis.R`) | Not needed for final metrics |

Root `validation.R` is a thin wrapper around `src/validation.R`.

---

## International forecasting pipeline (required)

### 1. Setup — always first

| Script | Role |
|--------|------|
| `00_project_setup.R` | Project root, `data/` / `reports/` / `models/` paths |
| `01_packages.R` | Install/load CRAN dependencies |
| `02_helpers.R` | Shared IO, manifests, utilities |

### 2. Raw → processed international tables

| Script | Role | Key output |
|--------|------|------------|
| `07_download_international_results.R` | Pull martj42 CSVs | `data/raw/international_results/` |
| `08_clean_international_results.R` | Normalize match results | `international_results.csv` |
| `08b_clean_international_goalscorers.R` | Goal-level events | `international_goalscorers.csv` |
| `08c_clean_international_shootouts.R` | Penalty shootout winners | `international_shootouts.csv` |
| `14_join_international_shootouts_to_results.R` | Attach shootout info without changing FT scores | `international_results_with_shootouts.csv` |
| `09_validate_international_results.R` | Hard QA (schema, keys, scores) | `data/validation/processed_data/` |
| `10_plot_international_results_validation.R` | Split-aware EDA plots | `reports/figures/international_results/` |

### 3. Elo ratings → modeling table

| Script | Role | Key output |
|--------|------|------------|
| `15_download_international_ratings.R` | World Football Elo download (or manual CSV fallback) | `data/raw/international_ratings/` |
| `16_clean_international_ratings.R` | Clean rating history | `international_team_ratings.csv` |
| `17_validate_international_ratings.R` | Ratings QA, unmatched teams | `data/validation/processed_data/` |
| `18_build_international_modeling_table.R` | Pre-match Elo, tournament flags, chronological splits | **`international_modeling_table.csv`** |

`run_light_pipeline.R` runs blocks 2–3; ratings steps 16–18 are skipped if download fails and no manual Elo file is present.

### 4. Feature engineering (inside modeling run)

| Script | Role | Key output |
|--------|------|------------|
| `27_build_lagged_team_form_features.R` | Points, GD, draw rate from **prior** matches only | `international_modeling_table_with_form.csv` |
| `29_build_goalscorer_form_features.R` | Attacking-depth aggregates from **prior** goals | `international_modeling_table_with_form_and_goalscorers.csv` |
| `30b_validate_engineered_features.R` | Leakage, ranges, schema checks on engineered columns | `data/validation/engineered_features/` |

### 5. Modeling and final reporting

| Script | Role | Notes |
|--------|------|-------|
| `19_baseline.R` | Frequency, majority, Elo multinomial baselines | **Required** |
| `20_feature_audit.R` | Column-level audit | Required |
| `22_finalize_feature_review.R` | Approved feature sets | Required |
| `23_feature_target_eda.R` | Target balance, correlations, plots | Required |
| `24_model_glm_lightgbm_approved_features.R` | Safe pre-match features → glmnet + LightGBM | Required |
| `25_model_diagnostics_draws_calibration.R` | Draw calibration and ranking diagnostics | Required |
| `26_model_draw_aware_features.R` | Draw-aware Elo transforms | Required (shows limited gain) |
| `28_model_with_lagged_form.R` | **Best incremental stage** (Model 28) | Required |
| `30_model_with_goalscorer_features.R` | Full tier comparison incl. goalscorers | Required |
| `31_final_results_visualization.R` | Cross-stage summary figures | Required |
| `32_finalize_international_modeling_project.R` | Final tables + `reports/final/` narrative | Required |

**Selected model:** LightGBM with `safe_plus_form_compact` (script 28). See [MODEL_CARD.md](../MODEL_CARD.md) and [reports/final/final_results_summary.md](../reports/final/final_results_summary.md).

---

## StatsBomb pipeline (optional, heavy)

Club competitions only. Supports reproducible ingestion, not the international deliverable.

| Script | Role |
|--------|------|
| `03_download_statsbomb_competitions.R` | Competition index |
| `04_download_statsbomb_matches.R` | Match metadata |
| `04b_download_statsbomb_events.R` | Per-match event JSON (large) |
| `04e_clean_statsbomb_events.R` | Flatten JSON → `statsbomb_events.csv`, `statsbomb_shots.csv` |
| `04c_download_statsbomb_lineups.R` | Lineup JSON |
| `04f_clean_statsbomb_lineups.R` | Flat lineup table |
| `04d_download_statsbomb_360.R` | 360 freeze-frame JSON |
| `04g_clean_statsbomb_360.R` | Flatten 360 frames |

**Runtime:** event cleaning and 360 flattening can take **many hours** and may hit memory limits. Use `run_statsbomb_pipeline.R` only when you explicitly need these processed tables.

---

## Club / odds side track (optional)

| Script | Role | Key output |
|--------|------|------------|
| `05_download_football_data_uk.R` | Season CSVs from football-data.co.uk | `data/raw/football_data_uk/` |
| `06_clean_football_data_uk.R` | Normalize club matches | `football_data_uk_matches.csv` |
| `13_build_football_data_modeling_table.R` | Match fields + wide odds columns (kept separate) | `football_data_uk_modeling_table.csv` |

Useful for future market-odds or calibration work; **not** joined into `international_modeling_table.csv` today.

---

## Exploratory, legacy, and convenience scripts

| Item | Classification | Notes |
|------|----------------|-------|
| `11_international_results_analysis.R` | Exploratory / legacy | Early EDA; writes `reports/tables/legacy_analysis/` |
| `21_baseline_plus_draw_features.R` | Optional supplement | Draw-aware quadratic Elo baselines; **not** in `run_modeling_pipeline.R`; script 31 uses its tables if present |
| `11_build_data_inventory.R` | Optional metadata | File inventory; only in `run_pipeline.R` |
| `src/validation.R` | Optional QA | End-to-end processed-file checks; only in `run_pipeline.R` |
| `notebooks/notebook_1.Rmd`, `notebook_2.Rmd` | Exploratory | Not production pipeline |
| `database/data_dictionary.Rmd` | Documentation | Not executed by runners |
| `reports/tables/approved_feature_sets*.R` | Generated artifacts | Produced by script 22 |
| `outputs/`, `graphs/` | Legacy output paths | Older runs; new work uses `reports/` and `data/` |

---

## Required vs optional (final project)

**Required to reproduce final international results**

1. `Rscript -e 'source("src/01_packages.R")'` (first time)
2. `Rscript src/run_light_pipeline.R` → through script 18
3. `Rscript src/run_modeling_pipeline.R` → scripts 19–32 (includes 27, 29, 30b)

**Optional**

- Entire StatsBomb track (`03`–`04g`, `run_statsbomb_pipeline.R`)
- Entire club track (`05`, `06`, `13`, `run_club_pipeline.R`)
- `run_pipeline.R` (combines all of the above + inventory + validation)
- `21_baseline_plus_draw_features.R`, `run_analysis_pipeline.R`
- Notebooks and legacy folders

**Minimum artifacts a reviewer should find**

- `data/processed/international_modeling_table.csv` (and form/goalscorer variants after modeling)
- `reports/tables/final_project/` and `reports/final/final_results_summary.md`
- `MODEL_CARD.md`

---

## Recommended run order

```bash
# From project root (R ≥ 4.2)

# Once: packages
Rscript -e 'source("src/01_packages.R")'

# 1) International data + modeling table (~minutes; needs network for downloads)
Rscript src/run_light_pipeline.R

# 2) Modeling, feature tiers, final report (~tens of minutes depending on LightGBM)
Rscript src/run_modeling_pipeline.R
```

**Partial reruns** (when upstream tables already exist):

| Goal | Run |
|------|-----|
| Refresh modeling table only | `18` → then full `run_modeling_pipeline.R` |
| Rebuild form / goalscorer features only | `27`, `29`, `30b` → then modeling scripts that need them |
| Re-run models only | `run_modeling_pipeline.R` (if processed tables unchanged) |
| StatsBomb processed tables | `run_statsbomb_pipeline.R` |
| Club odds table | `run_club_pipeline.R` |

Place manual fallbacks under `data/raw/` as described in [data_sources.md](data_sources.md) if downloads fail offline.

---

## Why not model raw StatsBomb JSON directly?

Raw files under `data/raw/statsbomb_open/events/{match_id}.json` are **nested, event-type-heterogeneous documents** (passes, shots, carries, etc.), not modeling-ready rows.

1. **Structure** — Each file is a list of events with different fields per `type.name`. You need explicit flattening (script `04e`) into stable columns before any join or aggregate.
2. **Scale** — Thousands of matches × hundreds of events per match → very large on disk and in memory. Cleaning is chunked and can take hours; modeling on raw JSON would repeat that cost every experiment.
3. **Scope** — StatsBomb open data is **club competition** coverage. The published international forecast uses martj42 + Elo, which span decades of national-team history StatsBomb does not provide.
4. **Leakage risk** — Event timestamps and post-shot outcomes are in-match information. Using JSON without careful pre-match aggregation would leak future information relative to kickoff.
5. **Reproducibility** — Processed CSVs (`statsbomb_events.csv`, validation summaries) give fixed schemas and QA artifacts; raw JSON alone does not.

The intended pattern: **download JSON once → clean to tabular processed files → (optional) derive match-level features with documented timing**. The international pipeline already follows that pattern for results and goalscorers.

---

## Related documentation

| Doc | Contents |
|-----|----------|
| [pipeline.md](pipeline.md) | Script-by-script I/O reference |
| [data_sources.md](data_sources.md) | Raw paths and manual fallbacks |
| [leakage_audit.md](leakage_audit.md) | Pre-match feature rules |
| [modeling_plan.md](modeling_plan.md) | Feature tiers and model stages |
| [evaluation_plan.md](evaluation_plan.md) | Metrics and split protocol |
