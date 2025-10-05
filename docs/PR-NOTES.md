# PR Notes - feat/transform-v1

## What Changed
- Added `R/02_clean_transform.R` with full CLI argument parsing (`--input`, `--output_processed`, `--output_primary`, `--report`, `--dryrun`)
- Implemented wave grouping via `pollster_wave_id = {pollster_slug}_{date_median}`
- Primary row selection: **full_field preference** → largest sample_size (one row per wave)
- Two CSV outputs: cleaned (all ~60 rows) + primary (~30 rows, one per wave)
- Markdown report with scenario histogram, wave counts, dropped alternates summary
- Added architecture decision record and testing documentation

## Why
- Need model-ready dataset with wave identifiers for random effects: `(1 | pollster_wave_id) + (1 | pollster)`
- Multiple scenarios per poll require deterministic selection rule for primary training data
- Timestamped outputs enable reproducible pipeline runs and drift detection
- Dryrun flag (default TRUE) prevents accidental overwrites during development
- CLI flags make pipeline flexible for batch processing and testing

## How to Test
```bash
# Dry run (default - shows what would be written)
Rscript R/02_clean_transform.R

# Execute transform
Rscript R/02_clean_transform.R --dryrun=false

# Verify outputs exist
ls -lh data/processed/polls_cleaned_*.csv
ls -lh data/processed/polls_primary_*.csv
cat analysis/02_transform_report_*.md

# Check primary rows == waves (CRITICAL ASSERTION)
Rscript -e "
  library(tidyverse)
  p <- read_csv(list.files('data/processed', pattern='primary.*csv\$', full.names=TRUE)[1], show_col_types=FALSE)
  n_rows <- nrow(p)
  n_waves <- n_distinct(p\$pollster_wave_id)
  cat('Primary rows:', n_rows, '| Unique waves:', n_waves, '\n')
  stopifnot(n_rows == n_waves)
  cat('✓ PASS: Primary rows == waves\n')
"
```

## Expected Output Summary
- Raw input: ~62 rows (2 event_marker + 60 poll rows)
- After filtering events: ~60 rows
- Waves detected: ~30-35 (based on pollster + date)
- Primary rows selected: ~30-35 (must equal #waves)
- Scenario distribution: ~32 full_field, ~8 adams_removed, ~20 head_to_head variants

## Files Modified
- `R/02_clean_transform.R` - Complete rewrite with CLI parsing and dryrun guard
- `docs/ADR-20251005-transform-v1.md` - Architecture decision record (new)
- `docs/PR-NOTES.md` - This file (new)
- `README.md` - Added Data section and CLI usage examples
- `TESTING.md` - Added Transform tests section with assertions

## Acceptance Criteria
- [x] CLI flags work: `--input`, `--output_processed`, `--output_primary`, `--report`, `--dryrun`
- [x] Dryrun default TRUE prevents writes
- [x] Primary rows == #waves (script asserts/warns)
- [x] Scenario histogram printed to console and report
- [x] Two timestamped CSVs in `data/processed/`
- [x] Vote status preserved in data (LV/RV/A)
- [x] Wave IDs use `pollster_wave_id` naming
