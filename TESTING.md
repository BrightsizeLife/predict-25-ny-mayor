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

**Goal:** Verify date parsing, numeric conversion, scenario classification, wave deduplication, QC band enforcement

**Checklist:**
- [ ] Date parsing rate: `date_median` non-NA for >95% of rows
- [ ] Percentage to numeric: all `*_pct` columns in range [0,100] or NA
- [ ] **QC band:** For full_field rows with qc_policy=renorm, row sums in [96, 104] after adjustment
- [ ] **QC outliers:** Outliers logged to `analysis/qc_outliers_{ts}.csv` with correct row_sum and qc_flag
- [ ] **QC policies work:** renorm (default), drop, keep produce expected row counts
- [ ] **PRIMARY = full-field only:** All PRIMARY rows have scenario_type == "full_field"
- [ ] **Walden folded:** In PRIMARY, walden_pct is NA and walden counts added to other_pct
- [ ] **Primary rows == #full-field waves**: `nrow(primary) == n_distinct(filter(clean, scenario_type == 'full_field')$pollster_wave_id)` (MUST be TRUE)
- [ ] Wave IDs: No duplicate `pollster_wave_id` in primary CSV
- [ ] Sample size preservation: `sum(primary$sample_size)` roughly equals sum of primary-flagged rows in cleaned CSV

**Commands:**
```bash
# Run transform (dry run - default with renorm)
Rscript R/02_clean_transform.R

# Run transform (execute with renorm - default)
Rscript R/02_clean_transform.R --dryrun=false

# Run transform with different QC policies
Rscript R/02_clean_transform.R --qc_policy=drop --dryrun=false
Rscript R/02_clean_transform.R --qc_policy=keep --dryrun=false

# Verify outputs exist
ls -lh data/processed/polls_cleaned_*.csv
ls -lh data/processed/polls_primary_*.csv
ls -lh analysis/qc_outliers_*.csv  # Only if outliers exist

# Check QC band enforcement (renorm policy)
Rscript -e "
  library(tidyverse)
  c <- read_csv(list.files('data/processed', pattern='cleaned.*csv\$', full.names=TRUE)[1], show_col_types=FALSE)
  ff <- c %>% filter(scenario_type == 'full_field')
  ff <- ff %>% mutate(
    row_sum = coalesce(as.numeric(mamdani_pct), 0) + coalesce(as.numeric(cuomo_pct), 0) +
              coalesce(as.numeric(adams_pct), 0) + coalesce(as.numeric(sliwa_pct), 0) +
              coalesce(as.numeric(other_pct), 0) + coalesce(as.numeric(undecided_pct), 0)
  )
  cat('Full-field rows:', nrow(ff), '\n')
  cat('Row sums < 96:', sum(ff\$row_sum < 96, na.rm=TRUE), '\n')
  cat('Row sums > 104:', sum(ff\$row_sum > 104, na.rm=TRUE), '\n')
  cat('✓ PASS: QC band enforced\n')
"

# Check PRIMARY = full-field only
Rscript -e "
  library(tidyverse)
  p <- read_csv(list.files('data/processed', pattern='primary.*csv\$', full.names=TRUE)[1], show_col_types=FALSE)
  non_ff <- p %>% filter(scenario_type != 'full_field')
  cat('Primary rows:', nrow(p), '\n')
  cat('Non-full-field rows in PRIMARY:', nrow(non_ff), '\n')
  stopifnot(nrow(non_ff) == 0)
  cat('✓ PASS: PRIMARY = full-field only\n')
"

# Check primary rows == full-field waves assertion (CRITICAL)
Rscript -e "
  library(tidyverse)
  p <- read_csv(list.files('data/processed', pattern='primary.*csv\$', full.names=TRUE)[1], show_col_types=FALSE)
  c <- read_csv(list.files('data/processed', pattern='cleaned.*csv\$', full.names=TRUE)[1], show_col_types=FALSE)
  n_primary <- nrow(p)
  n_ff_waves <- n_distinct(c %>% filter(scenario_type == 'full_field') %>% pull(pollster_wave_id))
  cat('Primary rows:', n_primary, '| Full-field waves:', n_ff_waves, '\n')
  stopifnot(n_primary == n_ff_waves)
  cat('✓ PASS: Primary rows == full-field waves\n')
"

# View report
cat analysis/02_transform_report_*.md
```

**Artifacts:**
- `data/processed/polls_cleaned_YYYYmmdd_HHMMSS.csv` (all non-event rows, QC-adjusted)
- `data/processed/polls_primary_YYYYmmdd_HHMMSS.csv` (full-field only, one per wave, walden→other)
- `analysis/02_transform_report_YYYYmmdd_HHMMSS.md` (includes QC stats)
- `analysis/qc_outliers_YYYYmmdd_HHMMSS.csv` (if outliers exist)

## 3) EDA tests (R/03_eda_plots.R)

**Goal:** Validate distributions, spot irregularities, generate visual diagnostics

**Checklist:**
- [ ] Script runs with default flags (dry run)
- [ ] 12 timestamped PNGs saved under `plots/` when `--dryrun=false` (DPI=300)
- [ ] Report created under `analysis/03_eda_report_*.md`
- [ ] Neon theme applied to all plots (black background, bright grid)
- [ ] Candidate palette used consistently (mamdani=#39FF14, cuomo=#D200FF, etc.)
- [ ] Percent sum diagnostics flag any rows < 97% or > 103%
- [ ] Report includes "Implications for Modeling" section

**Commands:**
```bash
# Dry run (default) - focus on cleaned
Rscript R/03_eda_plots.R

# Execute EDA on cleaned data
Rscript R/03_eda_plots.R --dryrun=false

# Include primary for comparison
Rscript R/03_eda_plots.R \
  --input_cleaned=data/processed/polls_cleaned_20251005_162414.csv \
  --input_primary=data/processed/polls_primary_20251005_162414.csv \
  --dryrun=false
```

**Artifacts:**
- `plots/EDA_P{1-12}_*_{timestamp}.png` (12 plots, DPI=300, high-res)
- `analysis/03_eda_report_{timestamp}.md`

**Plots generated:**
1. Candidate % over time (time series, all scenarios)
2. Margins over time (mamdani-cuomo, mamdani-adams)
3. Pollster small multiples (top 8 pollsters)
4. Candidate density distributions
5. Boxplots by vote status (LV/RV/A)
6. Pre/post Adams withdrawal comparison
7. Mamdani vs Cuomo scatter (smoothed)
8. Pairwise margins hexbin
9. Percent sums histogram (by scenario type)
10. Missingness bar chart
11. Scenario variant deltas
12. Primary vs All comparison (if primary data provided)

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
