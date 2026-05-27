# ⚽ Soccer-R-Verse

A comprehensive R-based project for international football match forecasting and data validation.

## Overview

Soccer-R-Verse is a data analytics and forecasting project focused on international football match results. The project combines data cleaning, validation, and visualization workflows to enable predictive modeling of international match outcomes.

**Primary Dataset:** `data/processed/international_results.csv` - A historical international football match results table with detailed match information and outcome metrics.

## Project Structure

```
Soccer-R-Verse/
├── data/
│   ├── raw/              # Original raw source data
│   ├── interim/          # Intermediate processing stages
│   ├── processed/        # Final cleaned datasets
│   └── validation/       # Validation reports and checks
├── src/                  # R scripts for data processing
├── graphs/               # Generated visualizations
├── database/             # Database-related files
├── data_dictionary.md    # Complete data schema documentation
└── README.md
```

## Key Features

- **Data Cleaning Pipeline**: Standardizes international match data with whitespace normalization and consistent formatting
- **Comprehensive Validation**: Performs hard error checks and warning-level audits on match records
- **Match Forecasting**: Foundation for predicting international football match outcomes
- **Multi-Source Integration**: Combines data from:
  - International match results
  - StatsBomb match metadata
  - Football Data UK club matches

## Dataset: International Results

### Overview

A historical record of international football matches with complete match metadata and outcome derivations.

**Grain**: One international football match with final score  
**Primary Key**: `source_match_id`  
**Last Reviewed**: 2026-05-27

### Key Columns

| Column | Type | Description |
|--------|------|-------------|
| `source_match_id` | character | Unique match identifier |
| `date` | date | Match date (validated, no future dates) |
| `season` | character | Calendar year |
| `competition` | character | Tournament/competition name |
| `home_team` | character | Home team name (normalized) |
| `away_team` | character | Away team name (normalized) |
| `home_score` | integer | Final home goals (non-negative) |
| `away_score` | integer | Final away goals (non-negative) |
| `match_result` | character | Home perspective: `H` (home win), `D` (draw), `A` (away win) |
| `result_class` | integer | Numeric outcome: `1` (home win), `0` (draw), `-1` (away win) |
| `home_win` | integer | Binary home win indicator |
| `draw` | integer | Binary draw indicator |
| `away_win` | integer | Binary away win indicator |
| `goal_difference` | integer | Home score minus away score |
| `total_goals` | integer | Sum of both teams' goals |
| `neutral` | logical | Neutral-site match flag |
| `tournament` | character | Tournament name (normalized) |
| `city` | character | Host city (normalized) |
| `country` | character | Host country (normalized) |

See [data_dictionary.md](data_dictionary.md) for the complete schema and validation rules.

## Processing Scripts

### Data Cleaning
- **Script**: `src/08_clean_international_results.R`
- **Purpose**: Standardizes raw international match data, normalizes text fields, and derives outcome metrics

### Validation
- **Script**: `src/09_validate_international_results.R`
- **Purpose**: Performs comprehensive data quality checks and generates validation reports

### Running Validation

```bash
Rscript src/09_validate_international_results.R
```

### Validation Outputs

| File | Purpose |
|------|---------|
| `data/validation/international_results_validation_checks.csv` | Detailed validation check results with severity, status, affected-row counts, and details |
| `data/validation/international_results_validation_summary.csv` | Run summary with row counts, date ranges, entity counts, result distributions, and validation status |
| `data/validation/international_results_validation_examples.csv` | Row-level examples for manual review (e.g., repeated fixtures, extreme scorelines) |

## Validation Rules

### Hard Errors
- Schema compliance and required field presence
- Primary key uniqueness (`source_match_id`)
- Score domains (non-negative integers)
- Date domains (valid dates, no future matches)
- Result derivation consistency (H/D/A matches actual scores)
- Text normalization (whitespace handling)
- Neutral-site coverage
- Record completeness

### Warnings (Review Items)
- Repeated date/team/tournament fixture keys
- Multiple matches per team on the same date
- Extreme scorelines
- Historic anomalies

## Secondary Datasets

- `data/processed/statsbomb_matches.csv` - StatsBomb match metadata
- `data/processed/statsbomb_competitions.csv` - StatsBomb competition reference
- `data/processed/football_data_uk_matches.csv` - Club match data

## Technology Stack

- **Language**: R
- **Data Processing**: Standard R data manipulation
- **Version Control**: Git

## Getting Started

1. **Clone the repository**
   ```bash
   git clone https://github.com/marco-hening-tallarico/Soccer-R-Verse.git
   cd Soccer-R-Verse
   ```

2. **Review the data structure**
   - Check [data_dictionary.md](data_dictionary.md) for complete schema details

3. **Run validation workflow**
   ```bash
   Rscript src/09_validate_international_results.R
   ```

4. **Explore results**
   - Validation reports are saved to `data/validation/`
   - Processed data is available in `data/processed/`

## File Tracking

Raw data files are excluded from version control (see `.gitignore`):
- `data/raw/` - Raw source files
- `data/interim/` - Intermediate processing files
- `*.log` - R session logs

## Future Enhancements

Potential areas for expansion:
- Predictive models for match outcomes
- Player-level performance metrics
- Advanced feature engineering for forecasting
- Web application for match predictions
- Historical trend analysis

## License

This project is open source. See LICENSE file for details.

## Contact

Created by [marco-hening-tallarico](https://github.com/marco-hening-tallarico)

---

**Last Updated**: 2026-05-27
