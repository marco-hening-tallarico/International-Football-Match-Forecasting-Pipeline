# Feature Review: international_modeling_table.csv

Prediction task: pre-match multiclass international football outcome model.

Target: home win / draw / away win.

Rule: model features must be known before kickoff. Direct result labels and post-match fields are excluded.

Features marked `use_with_care` are allowed, but they require strict chronological validation and should be monitored for overfitting.

## Feature decision table

| Column | Class | Missing % | Distinct | Decision | Examples | Notes |
|---|---:|---:|---:|---|---|---|
| `rating_diff` | numeric | 10.41 | 1798 | **keep** | 6 / -28 / 12 / -6 / -21 / -530 | Allowed pre-match feature. |
| `away_rating_pre_match` | numeric | 6.31 | 1653 | **keep** | 1997 / 2014 / 1994 / 2003 / 2011 / 2029 | Allowed pre-match feature. |
| `rating_age_days_away` | numeric | 6.31 | 1025 | **keep** | 98 / 364 / 343 / 2 / 383 / 301 | Allowed pre-match feature. |
| `home_rating_pre_match` | numeric | 5.98 | 1637 | **keep** | 2003 / 1986 / 2006 / 1997 / 2010 / 1990 | Allowed pre-match feature. |
| `rating_age_days_home` | numeric | 5.98 | 957 | **keep** | 98 / 364 / 21 / 345 / 362 / 322 | Allowed pre-match feature. |
| `is_continental_qualifier` | logical | 0 | 2 | **keep** | FALSE / TRUE | Allowed pre-match feature. |
| `is_continental_tournament` | logical | 0 | 2 | **keep** | FALSE / TRUE | Allowed pre-match feature. |
| `is_friendly` | logical | 0 | 2 | **keep** | TRUE / FALSE | Allowed pre-match feature. |
| `is_world_cup` | logical | 0 | 2 | **keep** | FALSE / TRUE | Allowed pre-match feature. |
| `is_world_cup_qualifier` | logical | 0 | 2 | **keep** | FALSE / TRUE | Allowed pre-match feature. |
| `neutral` | logical | 0 | 2 | **keep** | FALSE / TRUE | Allowed pre-match feature. |
| `away_team_clean` | character | 0 | 321 | **use_with_care** | england / scotland / wales / northern_ireland / canada / argentina | Known pre-match team identifier. Can improve fit but may overfit sparse teams and eras. |
| `city` | character | 0 | 2138 | **use_with_care** | Glasgow / London / Wrexham / Blackburn / Belfast / Liverpool | Known pre-match location metadata. Prefer engineered location features, but allowed with care. |
| `competition` | character | 0 | 198 | **use_with_care** | Friendly / British Home Championship / Évence Coppée Trophy / Muratti Vase / Copa Lipton / Copa Newton | Known pre-match competition context. May need rare-level grouping and careful validation. |
| `country` | character | 0 | 269 | **use_with_care** | Scotland / England / Wales / Ireland / United States / Uruguay | Known pre-match location metadata. Prefer engineered location features, but allowed with care. |
| `home_team_clean` | character | 0 | 327 | **use_with_care** | scotland / england / wales / northern_ireland / united_states / uruguay | Known pre-match team identifier. Can improve fit but may overfit sparse teams and eras. |
| `season` | numeric | 0 | 155 | **use_with_care** | 1872 / 1873 / 1874 / 1875 / 1876 / 1877 | Known pre-match, but may create era/time shortcuts. Allowed only with chronological validation. |
| `tournament` | character | 0 | 198 | **use_with_care** | Friendly / British Home Championship / Évence Coppée Trophy / Muratti Vase / Copa Lipton / Copa Newton | Known pre-match competition context. May need rare-level grouping and careful validation. |
| `date` | Date | 0 | 16434 | **exclude_as_feature_keep_for_split** | 1872-11-30 / 1873-03-08 / 1874-03-07 / 1875-03-06 / 1876-03-04 / 1876-03-25 | Useful for chronological splitting/backtesting, but not as a direct model feature. |
| `away_rating_date` | Date | 6.31 | 14184 | **exclude_as_feature_keep_for_audit** | 1872-11-30 / 1873-03-08 / 1874-03-07 / 1875-03-06 / 1876-03-25 / 1877-03-03 | Useful for validating rating freshness, but not used directly as a model feature. |
| `home_rating_date` | Date | 5.98 | 14290 | **exclude_as_feature_keep_for_audit** | 1872-11-30 / 1873-03-08 / 1874-03-07 / 1875-03-06 / 1876-03-04 / 1876-03-25 | Useful for validating rating freshness, but not used directly as a model feature. |
| `away_rank_pre_match` | logical | 100 | 0 | **exclude** | ALL_NA | Excluded due to missingness, target leakage risk, or unusable current form. |
| `home_rank_pre_match` | logical | 100 | 0 | **exclude** | ALL_NA | Excluded due to missingness, target leakage risk, or unusable current form. |
| `rank_diff` | logical | 100 | 0 | **exclude** | ALL_NA | Excluded due to missingness, target leakage risk, or unusable current form. |
| `shootout_winner` | character | 98.63 | 182 | **exclude** | Taiwan / South Korea / Iraq / Thailand / Ghana / Guinea | Post-match field. Not known before kickoff. |
| `away_score` | numeric | 0 | 22 | **exclude** | 0 / 2 / 1 / 3 / 4 / 6 | Excluded due to missingness, target leakage risk, or unusable current form. |
| `away_win` | numeric | 0 | 2 | **exclude** | 0 / 1 | Direct target/result label. Cannot be used as a feature. |
| `away_won_shootout` | numeric | 0 | 2 | **exclude** | 0 / 1 | Post-match field. Not known before kickoff. |
| `draw` | numeric | 0 | 2 | **exclude** | 1 / 0 | Direct target/result label. Cannot be used as a feature. |
| `home_score` | numeric | 0 | 26 | **exclude** | 0 / 4 / 2 / 3 / 1 / 7 | Excluded due to missingness, target leakage risk, or unusable current form. |
| `home_win` | numeric | 0 | 2 | **exclude** | 0 / 1 | Direct target/result label. Cannot be used as a feature. |
| `home_won_shootout` | numeric | 0 | 2 | **exclude** | 0 / 1 | Post-match field. Not known before kickoff. |
| `shootout_played` | logical | 0 | 2 | **exclude** | FALSE / TRUE | Post-match field. Not known before kickoff. |
| `away_team` | character | 0 | 321 | **review** | England / Scotland / Wales / Northern Ireland / Canada / Argentina | Needs manual review. |
| `data_split` | character | 0 | 2 | **review** | train / test | Needs manual review. |
| `home_team` | character | 0 | 327 | **review** | Scotland / England / Wales / Northern Ireland / United States / Uruguay | Needs manual review. |
| `match_result` | character | 0 | 3 | **review** | D / H / A | Needs manual review. |
| `result_class` | numeric | 0 | 3 | **review** | 0 / 1 / -1 | Needs manual review. |
| `source_match_id` | character | 0 | 49257 | **review** | 1872_11_30_scotland_england_friendly / 1873_03_08_england_scotland_friendly / 1874_03_07_scotland_england_friendly / 1875_03_06_england_scotland_friendly / 1876_03_04_scotland_england_friendly / 1876_03_25_scotland_wales_friendly | Needs manual review. |

## Approved feature sets

### Safe features

```r
safe_features <- c(
  "rating_diff",
  "away_rating_pre_match",
  "rating_age_days_away",
  "home_rating_pre_match",
  "rating_age_days_home",
  "is_continental_qualifier",
  "is_continental_tournament",
  "is_friendly",
  "is_world_cup",
  "is_world_cup_qualifier",
  "neutral"
)
```

### Use-with-care features

```r
careful_features <- c(
  "away_team_clean",
  "city",
  "competition",
  "country",
  "home_team_clean",
  "season",
  "tournament"
)
```

### Modeling feature set

```r
model_features <- c(safe_features, careful_features)
```

### Excluded features

```r
excluded_features <- c(
  "date",
  "away_rating_date",
  "home_rating_date",
  "away_rank_pre_match",
  "home_rank_pre_match",
  "rank_diff",
  "shootout_winner",
  "away_score",
  "away_win",
  "away_won_shootout",
  "draw",
  "home_score",
  "home_win",
  "home_won_shootout",
  "shootout_played"
)
```

## Required leakage checks

Before using rating features, verify rating dates are never after the match date.

```r
df |> summarise(any_future_home_rating = any(home_rating_date > date, na.rm = TRUE))
df |> summarise(any_future_away_rating = any(away_rating_date > date, na.rm = TRUE))
```
