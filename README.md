# predict-25-ny-mayor

Bayesian multinomial model for the 2025 NYC Mayoral General Election polls.

## Overview

This project reproduces a presidential-model workflow for the 2025 NYC mayoral general election using:
- **Data source:** Wikipedia's "General election polls" table
- **Model:** Bayesian multinomial regression with splines + random effects (brms)
- **Candidates:** Mamdani (D), Cuomo (I), Adams (withdrawn 2025-09-28), Sliwa (R), Other

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

```r
# 1. Install dependencies
source("R/00_setup.R")

# 2. Scrape latest polls
source("R/01_scrape_wiki_nyc.R")

# 3. Clean and transform
source("R/02_clean_transform.R")

# 4. EDA
source("R/03_eda_plots.R")

# 5. Fit models
source("R/04_fit_models.R")

# 6. Compare via LOO
source("R/05_compare_loo.R")
```

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
