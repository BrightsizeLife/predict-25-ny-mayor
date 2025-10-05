# predict-25-ny-mayor

Bayesian multinomial model for the 2025 NYC Mayoral General Election polls.

## Overview

This project reproduces a presidential-model workflow for the 2025 NYC mayoral general election using:
- **Data source:** Wikipedia's "General election polls" table
- **Model:** Bayesian multinomial regression with splines + random effects (brms)
- **Candidates:** Mamdani (D), Cuomo (I), Adams (withdrawn 2025-09-28), Sliwa (R), Other

## Data

- **Raw polling data**: Scraped Wikipedia tables stored in `data/raw/polls_YYYYmmdd_HHMMSS.csv` (timestamped, immutable)
- **Processed data**: Cleaned and model-ready CSVs in `data/processed/` with wave identifiers, scenario types, and candidate percentages/counts

## Project Structure

```
R/                      # Analysis scripts (numbered 00-07)
data/
  raw/                  # Timestamped raw scrapes from Wikipedia
  processed/            # Cleaned data (wide + long formats)
artifacts/models/       # RDS model fits
analysis/               # Markdown reports with model comparisons
plots/                  # EDA and diagnostic figures
scripts/                # Helper utilities
```

## Quick Start

```bash
# 1. Install dependencies
Rscript R/00_setup.R

# 2. Scrape latest polls (dry run)
Rscript R/01_scrape_wiki_nyc.R

# 2a. Execute scrape
Rscript R/01_scrape_wiki_nyc.R --dryrun=false

# 3. Transform raw data (dry run)
Rscript R/02_clean_transform.R

# 3a. Execute transform
Rscript R/02_clean_transform.R --dryrun=false

# 3b. Transform with custom input
Rscript R/02_clean_transform.R --input=data/raw/polls_20251005_092241.csv --dryrun=false

# 4. EDA (coming soon)
# Rscript R/03_eda_plots.R --input data/processed/polls_primary_*.csv

# 5. Fit models (coming soon)
# Rscript R/04_fit_models.R --input data/processed/polls_primary_*.csv

# 6. Compare via LOO (coming soon)
# Rscript R/05_compare_loo.R --models artifacts/models/*.rds
```

## Testing

Before running analysis scripts, verify your environment:

```bash
Rscript scripts/check_env.R
```

For comprehensive testing procedures, see [`TESTING.md`](TESTING.md) which includes:
- Environment smoke tests
- Scraper validation (selector stability, rowspan handling, footnote stripping)
- Data cleaning checks (date parsing, candidate normalization, sanity checks)
- Model convergence criteria
- Reproducibility verification

## Git Workflow

- **Main branch:** `main` (stable releases only)
- **Feature branches:** `feat/*`, `fix/*`, `analysis/*`
- **Commits:** Small, atomic, descriptive messages with timestamps in artifacts

## Notes

- Eric Adams withdrew on 2025-09-28; rows before/after retained for event analysis
- Reallocation scenarios (pollster-provided) tracked separately
- All outputs timestamped with `YYYYmmdd_HHMMSS` format

---

**Repository:** https://github.com/BrightsizeLife/predict-25-ny-mayor
**License:** MIT (TBD)
