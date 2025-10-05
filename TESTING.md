# TESTING.md — NYC Mayoral Polls Project

This file describes how we validate each stage and how to run smoke tests locally and via Claude Code (CC).

## 0) Environment smoke
- Run `Rscript R/00_setup.R`
- Check `cmdstanr::cmdstan_version()` (warn if missing)
- Log R version, CPU cores, package versions

**Pass:** No errors; cmdstanr available or warned; versions logged

## 1) Scraper tests (R/01_scrape_wiki_nyc.R)
- Selector: `<h4 id="General_election_polls">` → first `table.wikitable`
- Log header vector; store header hash
- Drift: unknown headers → INFO log + save `data/raw/html_snapshot_YYYYmmdd_HHMMSS.html`
- Rowspan/colspan filled down; scenarios labeled (`full_field`, `adams_removed`, `head_to_head_*`)
- Footnotes stripped; `—` → NA
- Event row ("Adams withdraws…") captured

**Artifacts:** `data/raw/polls_YYYYmmdd_HHMMSS.csv` (+ optional html snapshot)

## 2) Transform tests (R/02_clean_transform.R)

**Goal:** Verify date parsing, numeric conversion, scenario classification, wave deduplication

**Checklist:**
- [ ] Date parsing rate: `date_median` non-NA for >95% of rows
- [ ] Percentage to numeric: all `*_pct` columns in range [0,100] or NA
- [ ] Row sums: For full_field rows, `sum(mamdani_pct + cuomo_pct + adams_pct + sliwa_pct + other_pct + undecided_pct)` in [95,105]
- [ ] Scenario histogram: Counts for full_field, adams_removed, head_to_head_* look reasonable (~30, ~8, ~20)
- [ ] **Primary rows == #waves**: `nrow(primary) == n_distinct(clean$pollster_wave_id)` (MUST be TRUE)
- [ ] Wave IDs: No duplicate `pollster_wave_id` in primary CSV
- [ ] Sample size preservation: `sum(primary$sample_size)` roughly equals sum of primary-flagged rows in cleaned CSV

**Commands:**
```bash
# Run transform (dry run - default)
Rscript R/02_clean_transform.R

# Run transform (execute)
Rscript R/02_clean_transform.R --dryrun=false

# Verify outputs exist
ls -lh data/processed/polls_cleaned_*.csv
ls -lh data/processed/polls_primary_*.csv

# Check primary rows == waves assertion (CRITICAL)
Rscript -e "
  library(tidyverse)
  p <- read_csv(list.files('data/processed', pattern='primary.*csv\$', full.names=TRUE)[1], show_col_types=FALSE)
  n_rows <- nrow(p)
  n_waves <- n_distinct(p\$pollster_wave_id)
  cat('Primary rows:', n_rows, '| Unique waves:', n_waves, '\n')
  stopifnot(n_rows == n_waves)
  cat('✓ PASS: Primary rows == waves\n')
"

# View report
cat analysis/02_transform_report_*.md
```

**Artifacts:**
- `data/processed/polls_cleaned_YYYYmmdd_HHMMSS.csv` (all non-event rows)
- `data/processed/polls_primary_YYYYmmdd_HHMMSS.csv` (one per wave)
- `analysis/02_transform_report_YYYYmmdd_HHMMSS.md`

## 3) EDA tests (R/03_eda_plots.R)
- Time-series per candidate; save `plots/EDA_*_YYYYmmdd_HHMMSS.png`

## 4) Modeling tests (R/04_fit_models.R, R/05_compare_loo.R)
- brms multinomial; `(1|pollster)+(1|vote_status)`; time as linear and splines df=3,4,5
- Convergence: R-hat≈1; sufficient ESS; tune `adapt_delta` if needed
- LOO compare; write `analysis/model_compare_YYYYmmdd_HHMMSS.md`

## 5) Events (R/06_explore_events.R)
- Pre/Post around 2025-09-28 (Adams withdrawal) ±14 days; density plots; summary tables

## 6) Forecasts (R/07_forecasts.R)
- Same-day prediction at max date; forward-date forecast with caveats; save plot + markdown

## 7) Reproducibility & rollback
- Timestamp all outputs; never overwrite
- Re-run produces new files; same raw → identical processed (hash match)
- Use feature branches; PR merges; `git tag` gates (e.g., gate-1-scrape)

---

### Suggested commands

```bash
# Env smoke
Rscript scripts/check_env.R

# Scrape
Rscript R/01_scrape_wiki_nyc.R --dryrun=false

# Transform (dry run)
Rscript R/02_clean_transform.R

# Transform (execute)
Rscript R/02_clean_transform.R --dryrun=false

# Transform with custom input
Rscript R/02_clean_transform.R \
  --input=data/raw/polls_20251005_092241.csv \
  --dryrun=false

# EDA
Rscript R/03_eda_plots.R --input data/processed/polls_cleaned_*.csv

# Fit & compare
Rscript R/04_fit_models.R --input data/processed/polls_cleaned_*.csv
Rscript R/05_compare_loo.R --models artifacts/models/*.rds

# Events & forecast
Rscript R/06_explore_events.R --models artifacts/models/best_model.rds
Rscript R/07_forecasts.R --models artifacts/models/best_model.rds
```
