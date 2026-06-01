# Project Overview

This document is a concise, reviewer-facing map of the **worldcup-forecast-r** project. It summarizes the existing documentation; for schemas, script I/O, audits, and metrics, follow the links at the end.

---

## Objective

The project predicts **international football match outcomes** using information available **before kickoff**. Each match is labeled with a three-class target:

| Code | Meaning |
|------|---------|
| **H** | Home win |
| **D** | Draw |
| **A** | Away win |

The modeling goal is calibrated multiclass probabilities P(H), P(D), and P(A), not just a single predicted class. See [modeling_plan.md](modeling_plan.md) and [evaluation_plan.md](evaluation_plan.md) for the full protocol.

---

## Main Deliverable

The **primary deliverable** is the **international match forecasting pipeline**: historical national-team results, World Football Elo ratings, engineered pre-match features, baselines, and supervised models with a final comparison report.

The repository also contains **optional side tracks** that are **not** required for the final international forecast:

- **StatsBomb Open Data** — club competition event/lineup/360 ingestion (heavy I/O).
- **football-data.co.uk** — club results and bookmaker odds (exploratory; not joined into the international modeling table today).

Those tracks support reproducible data plumbing and future work; they do not feed the headline international model. See [script_map.md](script_map.md) for how tracks are separated.

---

## Data Sources

| Source | Role | Detail doc |
|--------|------|------------|
| **[martj42/international_results](https://github.com/martj42/international_results)** | Core match history | [data_sources.md](data_sources.md) |
| **[World Football Elo](http://www.eloratings.net/)** | Pre-match team strength | [data_sources.md](data_sources.md) |
| **Goalscorers & shootouts** (same repo) | Goal-level events and penalty metadata | [data_sources.md](data_sources.md) |
| **[StatsBomb Open Data](https://github.com/statsbomb/open-data)** | Optional club event data | [data_sources.md](data_sources.md) |
| **[football-data.co.uk](https://www.football-data.co.uk/)** | Optional club matches and odds | [data_sources.md](data_sources.md) |

**International results** provide long historical coverage (results, goalscorers, shootouts). Team names require a crosswalk to join Elo ratings (`data/metadata/team_name_crosswalk.csv`).

**World Football Elo** supplies the main pre-match strength signal. Downloads can fail offline; a manual CSV fallback under `data/raw/international_ratings/` is supported (see [data_sources.md](data_sources.md)).

**StatsBomb** and **football-data.co.uk** are documented and scripted in the repo but are optional for reproducing final international metrics.

---

## Final Modeling Dataset

**Grain:** one row per international match, keyed by `source_match_id`.

Processed match results live in `international_results.csv`. The modeling tables add pre-match Elo, tournament context flags, and chronological train/validation/test splits (`data_split`).

| Table | Description |
|-------|-------------|
| `international_modeling_table.csv` | Base pre-match features + split (script 18) |
| `international_modeling_table_with_form.csv` | + lagged team form from **prior** matches only (script 27) |
| `international_modeling_table_with_form_and_goalscorers.csv` | + goalscorer depth from **prior** goals only (script 29) |

Column definitions and validation layout: [data_dictionary.md](data_dictionary.md).

---

## Pipeline Summary

Scripts under `src/` are numbered by dependency within each stage. **Orchestrators** (`run_*.R`) are the supported entry points; see [pipeline.md](pipeline.md) for per-script inputs and outputs.

| Stage | What happens |
|-------|----------------|
| 1. Setup / helpers | Paths, packages, shared IO (`00`–`02`) |
| 2. International raw data | Download and clean martj42 results, goalscorers, shootouts (`07`–`14`, `09`–`10`) |
| 3. Elo | Download, clean, validate ratings; build modeling table (`15`–`18`) |
| 4. Modeling table | Pre-match Elo, flags, chronological splits → `international_modeling_table.csv` |
| 5. Lagged features | Form (`27`) and goalscorer features (`29`); validation (`30b`) |
| 6. Baselines & ML | Feature audit, EDA, glmnet/LightGBM, tier comparisons (`19`–`30`) |
| 7. Final reporting | Summary figures and consolidated tables (`31`–`32`) |

**Recommended commands** (from project root; R ≥ 4.2):

```bash
Rscript -e 'source("src/01_packages.R")'
Rscript src/run_light_pipeline.R
Rscript src/run_modeling_pipeline.R
```

- `run_light_pipeline.R` — international data through the modeling table (Elo steps skip if download fails and no manual file is present).
- `run_modeling_pipeline.R` — baselines through final report (includes feature engineering scripts 27, 29, 30b).

Optional full rebuild: `run_pipeline.R` (StatsBomb + club + international + inventory). See [pipeline.md](pipeline.md) for partial reruns.

---

## Script Structure

The `src/` folder is crowded because the project maintains **several parallel tracks**:

| Track | Purpose | Required for final international forecast? |
|-------|---------|---------------------------------------------|
| **International** | National-team results → modeling → report | **Yes** |
| **StatsBomb** | Club event/lineup/360 ingestion | No |
| **Club / odds** | football-data.co.uk tables | No |
| **Exploratory / legacy** | Early EDA, optional supplements, notebooks | No |

For the full script inventory, orchestrators, required vs optional scripts, and why raw StatsBomb JSON must not be modeled directly, see **[script_map.md](script_map.md)**.

---

## Modeling Strategy

Models are compared in **incremental feature tiers** (same chronological splits within each tier):

1. **baseline_rating** — Elo difference only  
2. **rating_plus_context** — + tournament / neutral flags  
3. **rating_plus_form** — + compact lagged form  
4. **rating_plus_form_plus_goalscorers** — + goalscorer depth  

**Model families** (by stage; see [modeling_plan.md](modeling_plan.md)):

- **Baselines (script 19):** class frequency, majority class, multinomial logistic regression on Elo  
- **Supervised (24+):** `multinom`, `glmnet` multinomial ridge, **LightGBM** (if installed)  
- Complete-case cohorts per feature variant; filter counts are logged per script  

**Selection protocol:** fit on train, select on **validation log loss**, report **test** metrics once for the chosen configuration. No test-set peeking during selection.

**Final model story (three roles — do not conflate):**

| Role | Configuration | Val log loss | Notes |
|------|---------------|--------------|-------|
| **Portfolio final** | Model 28 — LightGBM + `safe_plus_form_compact` | **0.89309** | Selected by script 31 from `model_28_metrics.csv` |
| **Tier / robustness** | Model 30 — LightGBM + `rating_plus_form` | **0.88884** | Different cohort; not directly comparable |
| **Interpretable challenger** | Model 28 — multinom + `safe_plus_form_compact` | 0.89485 | +0.00176 vs LightGBM on validation; test 0.87074 reported only |

See `reports/final/final_results_summary.md`, [MODEL_CARD.md](../MODEL_CARD.md), and [model_selection_rationale.md](model_selection_rationale.md). Gains over strong Elo baselines are **modest**; goalscorer features did not beat `rating_plus_form` on the Model 30 tier cohort. Exact metrics and caveats are in those artifacts—not repeated here.

**Non-goals** for this project version: large-scale hyperparameter search, neural/embedding team models, in-play updating, betting-market integration (noted as future work in [modeling_plan.md](modeling_plan.md)).

---

## Evaluation Strategy

| Aspect | Approach |
|--------|----------|
| **Primary metric** | Multiclass **log loss** (cross-entropy on H/D/A probabilities) |
| **Secondary metrics** | Brier score, accuracy, macro F1, classwise precision/recall, calibration bins |
| **Splits** | Chronological **train / validation / test** via `data_split` on the modeling table (script 18) |
| **Selection** | Validation split only |
| **Final report** | Test split used once for held-out reporting |

Validation mimics forecasting future matches from past data (no random shuffle). Different feature tiers may have slightly different row counts due to form cold-start missingness. Interpretation guidelines (e.g., small log-loss deltas, draw recall vs accuracy): [evaluation_plan.md](evaluation_plan.md).

---

## Leakage Controls

Only **pre-match** features may enter models. The target `match_result` and anything derived from the final score or same-match events are excluded from inputs.

**Excluded from model inputs (examples):**

- Final scores and outcome indicators (`home_score`, `away_score`, `home_win`, `draw`, `away_win`, etc.)
- Same-match goalscorer minutes/counts
- In-match event timestamps or post-shot outcomes (relevant if using StatsBomb-derived features carelessly)

**Allowed groups** include pre-match Elo, rating freshness, tournament/neutral flags, lagged form (strict `date < match_date`), and goalscorer aggregates from prior matches only. Approved column lists are frozen in `reports/tables/approved_feature_sets_final.R` (script 22).

Engineered features are audited in `data/validation/engineered_features/`. Reviewer quick check:

```bash
Rscript src/30b_validate_engineered_features.R
```

Full rules, audit files, and known risks: **[leakage_audit.md](leakage_audit.md)**.

---

## Required vs Optional Tracks

| Track | Status | Notes |
|-------|--------|-------|
| martj42 international results + Elo + form/goalscorer features | **Required** | Reproduces final international results |
| StatsBomb Open Data | **Optional** | Large JSON under `data/raw/statsbomb_open/`; flatten via scripts `03`–`04g` |
| football-data.co.uk club/odds | **Optional** | Separate `football_data_uk_modeling_table.csv`; not merged into international table |

**Raw StatsBomb JSON must not be modeled directly.** Files are nested, heterogeneous event documents—not tabular modeling rows. The intended pattern is: download once → clean to processed CSVs (`statsbomb_events.csv`, etc.) with documented schemas and QA → optionally derive **pre-match** aggregates. Event cleaning can take many hours; club coverage does not replace decades of international history. See [script_map.md](script_map.md#why-not-model-raw-statsbomb-json-directly).

---

## Key Outputs

After a full modeling run, reviewers should find artifacts under `reports/` and `data/processed/`:

| Location | Contents |
|----------|----------|
| `data/processed/international_modeling_table*.csv` | Modeling tables (base, form, goalscorers) |
| `reports/tables/final_project/` | Incremental tier summary, best-model tables |
| `reports/figures/` | EDA, calibration, final comparison plots |
| `reports/final/final_results_summary.md` | Narrative summary of stages and portfolio final |
| `reports/final/final_international_modeling_report.md` | Curated reviewer report (may differ slightly from script 32 output) |
| [MODEL_CARD.md](../MODEL_CARD.md) | Model card (portfolio final + tier robustness) |
| `data/predictions/final_preferred_model_predictions.csv` | Portfolio-final row-level probabilities (when exported) |
| `reports/tables/model_<NN>/` | Per-stage metrics and diagnostics |
| `data/validation/processed_data/`, `data/validation/engineered_features/` | Cleaning and leakage QA |

Legacy paths (`graphs/`, `outputs/`) may exist from older runs; new pipeline output uses `reports/` and `data/` (see [notes.md](notes.md)).

---

## Links to Detailed Documentation

| Document | What you will find there |
|----------|--------------------------|
| [data_sources.md](data_sources.md) | Raw paths, download scripts, strengths/limitations, manual fallbacks |
| [data_dictionary.md](data_dictionary.md) | Column definitions, grains, primary keys, validation file layout |
| [pipeline.md](pipeline.md) | Script-by-script I/O, orchestrators, typical workflows |
| [script_map.md](script_map.md) | Full `src/` inventory, tracks, required vs optional, StatsBomb guidance |
| [leakage_audit.md](leakage_audit.md) | Allowed/excluded features, audit files, reviewer checks |
| [modeling_plan.md](modeling_plan.md) | Stages, feature tiers, model families, selection protocol |
| [evaluation_plan.md](evaluation_plan.md) | Metrics, splits, cohort rules, calibration artifacts |
| [notes.md](notes.md) | Naming conventions, legacy folders, renv, future work pointers |

---

*Last consolidated from project docs: 2026-06-01 (portfolio final = Model 28 LightGBM). For schema or metric details that may change after a new pipeline run, prefer the generated artifacts and the linked docs above.*
