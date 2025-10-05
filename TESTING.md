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

## 2) Cleaning tests (R/02_clean_transform.R)
- Date range → median; keep original string
- Parse `sample_size_raw` → `sample_size` + `vote_status` ∈ {LV, RV, A}
- Normalize to columns: mamdani, cuomo, adams, sliwa, other, undecided; fold Walden → other
- Percent sanity: sum in [95, 105]; log outliers
- Counts: `*_n = round(sample_size * pct / 100)`

**Artifact:** `data/processed/polls_cleaned_YYYYmmdd_HHMMSS.csv`

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

# Clean
Rscript R/02_clean_transform.R \
  --input data/raw/polls_YYYYmmdd_HHMMSS.csv \
  --output data/processed/polls_cleaned_YYYYmmdd_HHMMSS.csv

# EDA
Rscript R/03_eda_plots.R --input data/processed/polls_cleaned_*.csv

# Fit & compare
Rscript R/04_fit_models.R --input data/processed/polls_cleaned_*.csv
Rscript R/05_compare_loo.R --models artifacts/models/*.rds

# Events & forecast
Rscript R/06_explore_events.R --models artifacts/models/best_model.rds
Rscript R/07_forecasts.R --models artifacts/models/best_model.rds
```
