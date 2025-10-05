# CLAUDE.md — NYC Mayoral Election Polling Analysis

## 🧭 Project Overview
This repository models polling data for the **2025 New York City mayoral election**, focusing on candidate trends before and after Eric Adams’s withdrawal (Sept 28, 2025).  
It replicates the structure of Derek DeBellis’s 2024 U.S. Presidential Election Bayesian analysis — adapted for a city race with smaller sample sizes.

Claude Code (CC) acts as a structured, agentic coding assistant, following **Anthropic’s best-practice principles**:
- Plan before acting
- Work in small, reviewable batches
- Version-control everything
- Keep human-approved checkpoints

---

## 🧩 Repository Layout
```
predict-25-ny-mayor/
├── .claude/commands/        # optional automation triggers
├── R/                       # R scripts for each phase
│   ├── 00_setup.R
│   ├── 01_scrape_wiki_nyc.R
│   ├── 02_clean_transform.R
│   ├── 03_eda_plots.R
│   ├── 04_fit_models.R
│   ├── 05_compare_loo.R
│   ├── 06_explore_events.R
│   └── 07_forecasts.R
├── data/raw/                # timestamped raw scrapes
├── data/processed/          # cleaned datasets
├── analysis/                # markdown summaries
├── artifacts/models/        # saved model objects (.rds)
├── plots/                   # visual outputs
└── scripts/                 # helper bash/R tools
```

---

## 🦖 Operating Principles

### 1. Context Engineering
All persistent guidance lives here. CC should always:
- Re-read this file before any new task.
- Avoid depending on transient chat prompts alone.
- Maintain reproducible context via markdown + code comments.

### Data Source & Scraping Strategy
**Primary source:** Wikipedia — "2025 New York City mayoral election — General election polls"
**URL:** https://en.wikipedia.org/wiki/2025_New_York_City_mayoral_election#General_election_polls

**Key event:** Eric Adams withdrew on **2025-09-28**. Keep his data for before/after analysis.

**Candidates tracked:**
- Zohran Mamdani (D)
- Andrew Cuomo (I)
- Eric Adams (I, withdrawn)
- Curtis Sliwa (R)
- Other (combined: Jim Walden + any additional candidates)
- Undecided (for diagnostics)

**Scraping approach:**
1. Find `<h4 id="General_election_polls">` heading
2. Select first `<table class="wikitable sortable">` following it
3. Validate: First column header = "Poll source"

**Challenges:**
- **Rowspan:** Pollster/Date/Sample columns span multiple rows (same poll, different scenarios)
- **Dynamic candidates:** Jim Walden appears in some polls, not others
- **Footnotes:** Strip `<sup>` tags before parsing percentages
- **Missing values:** `—` in `<td class="table-na">` → convert to NA
- **Event markers:** Yellow rows with colspan (e.g., "Adams withdraws from the race")

### 2. Data Schemas

**Raw CSV** (`data/raw/polls_YYYYmmdd_HHMMSS.csv`):
```
scrape_timestamp, poll_source, date_string, sample_size_raw, margin_of_error,
adams, cuomo, mamdani, sliwa, walden, other, undecided,
row_type, notes
```

**Processed CSV** (`data/processed/polls_cleaned_YYYYmmdd_HHMMSS.csv`):
```
poll_id, pollster, date_start, date_end, date_median,
sample_size, vote_status, margin_of_error_pct,
mamdani_pct, cuomo_pct, adams_pct, sliwa_pct, other_pct, undecided_pct,
mamdani_n, cuomo_n, adams_n, sliwa_n, other_n, undecided_n,
scenario, pre_post_adams_withdrawal
```

### 3. Work Cycle
1. **Plan phase:** Propose clear plan, stop for approval.
2. **Implement phase:** Run code in isolated batch (≤ 2 files).
3. **Review phase:** Summarize diff, artifacts, and logs.
4. **Commit phase:** Commit locally with message like
   `feat(scrape): initial wiki parser with header matching`.
5. **Pause phase:** Wait for human approval before next gate.

### 4. Git Workflow
- Branch naming: `feat/`, `fix/`, `refactor/`, `exp/`
- Commit often; no >150 LOC per batch.
- Each batch must compile and lint cleanly.
- Human merges via PR only.

### 5. Data Handling
- Always timestamp outputs (`ymd_hms`)
- Never overwrite prior scrapes or processed data.
- Raw → Processed transformations must be deterministic and scriptable.
- When scraping, store both `raw_html` snippet (debug) and parsed table.

### 6. Modeling Expectations
- Multinomial family (via **brms**)  
- Random intercepts: `(1 | pollster)` and `(1 | vote_status)`  
- Time trend: `day_of_year` (linear and splined)  
- Compare df = 3, 4, 5 splines via **loo()**  
- Priors explicitly stated; log divergent transitions and ESS.

### 7. Output Conventions
| Type | Directory | Format |
|------|------------|---------|
| Raw data | `data/raw/` | `polls_YYYYmmdd_HHMMSS.csv` |
| Clean data | `data/processed/` | `polls_cleaned_YYYYmmdd_HHMMSS.csv` |
| Model summaries | `analysis/` | Markdown |
| Plots | `plots/` | PNG (600 dpi) |
| Models | `artifacts/models/` | RDS |

### 8. Interaction Etiquette
- CC must **ask before running** long computations (>10 min)  
- CC must **display intended file paths** before writing or overwriting  
- CC must **log errors, warnings, and unknown column headers**

---

## 🥮 Setup Notes
Primary dependencies installed via `R/00_setup.R`:

```r
packages <- c(
  "tidyverse", "rvest", "xml2", "lubridate", "janitor",
  "brms", "cmdstanr", "splines", "loo",
  "broom", "broom.mixed", "modelr", "here", "glue",
  "patchwork", "scales"
)
```

---

## 🔒 Safety & Review
- CC must confirm diffs before any `git push`.  
- CC must ask before deleting or rewriting files.  
- CC should stop and request clarification if schema drift or unknown HTML detected.

---

## 🪼 Suggested Command Sequence

| Step | Command | Purpose |
|------|----------|----------|
| 1 | `/init-skeleton` | Create base structure + setup |
| 2 | `/scrape` | Pull and log raw Wikipedia table |
| 3 | `/clean` | Normalize and join raw data |
| 4 | `/eda` | Quick exploratory plots |
| 5 | `/fit` | Fit multinomial models |
| 6 | `/loo` | Compare via LOO-CV |
| 7 | `/events` | Pre/post-Adams-withdrawal comparisons |
| 8 | `/forecast` | Predict near/future distributions |

---

## 🗾 Attribution
Created by **Derek DeBellis**, 2025.  
Claude Code operationalized with Anthropic’s *Agentic Coding Best Practices* (2024).  
Repository: [BrightsizeLife/predict-25-ny-mayor](https://github.com/BrightsizeLife/predict-25-ny-mayor)

