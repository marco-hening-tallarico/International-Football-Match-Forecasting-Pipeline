# reports/tables/approved_feature_sets_final.R
#
# Final approved feature lists after manual review. Loaded by Model 24+ scripts.

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

careful_features <- c(
  "away_team_clean",
  "city",
  "competition",
  "country",
  "home_team_clean",
  "season",
  "tournament"
)

model_features <- c(safe_features, careful_features)

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
  "shootout_played",
  "away_team",
  "data_split",
  "home_team",
  "match_result",
  "result_class",
  "source_match_id"
)

review_features <- character(0)
