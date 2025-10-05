#!/usr/bin/env Rscript
# ==============================================================================
# 02_clean_transform.R
# Clean and transform raw polling data
# Creates ALL rows file and PRIMARY rows file (one per poll wave)
# ==============================================================================

# Parse CLI arguments
args <- commandArgs(trailingOnly = TRUE)
input_file <- NULL
output_processed <- NULL
output_primary <- NULL
report_file <- NULL
dryrun <- TRUE

if (length(args) > 0) {
  for (arg in args) {
    if (grepl("^--input=", arg)) {
      input_file <- sub("^--input=", "", arg)
    } else if (grepl("^--output_processed=", arg)) {
      output_processed <- sub("^--output_processed=", "", arg)
    } else if (grepl("^--output_primary=", arg)) {
      output_primary <- sub("^--output_primary=", "", arg)
    } else if (grepl("^--report=", arg)) {
      report_file <- sub("^--report=", "", arg)
    } else if (grepl("^--dryrun=", arg)) {
      dryrun <- as.logical(sub("^--dryrun=", "", arg))
    }
  }
}

# Load dependencies
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(glue)
  library(here)
  library(readr)
})

STAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Set defaults if not provided
if (is.null(input_file)) {
  raw_files <- list.files(here("data", "raw"), pattern = "^polls_.*\\.csv$", full.names = TRUE)
  if (length(raw_files) == 0) {
    stop("ERROR: No raw polling CSV files found in data/raw/")
  }
  input_file <- raw_files[order(file.info(raw_files)$mtime, decreasing = TRUE)][1]
}
if (is.null(output_processed)) output_processed <- glue("data/processed/polls_cleaned_{STAMP}.csv")
if (is.null(output_primary)) output_primary <- glue("data/processed/polls_primary_{STAMP}.csv")
if (is.null(report_file)) report_file <- glue("analysis/02_transform_report_{STAMP}.md")

cat("=== NYC Mayoral Polling Data Transform ===\n")
cat("Timestamp:", STAMP, "\n")
cat("Dry run:", dryrun, "\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Parse date range to start, end, median
#' Handles: "September 18-22, 2025", "September 16-18, 2025", "September 28, 2025"
parse_date_range <- function(date_str) {
  if (is.na(date_str)) {
    return(list(start = NA_Date_, end = NA_Date_, median = NA_Date_))
  }

  # Clean string
  date_str <- str_trim(date_str)

  # Try to parse as single date first
  single_date <- suppressWarnings(mdy(date_str))
  if (!is.na(single_date)) {
    return(list(start = single_date, end = single_date, median = single_date))
  }

  # Try range format: "Month DD-DD, YYYY"
  range_match <- str_match(date_str, "([A-Za-z]+)\\s+(\\d+)-(\\d+),?\\s+(\\d{4})")
  if (!is.na(range_match[1, 1])) {
    month <- range_match[1, 2]
    day1 <- range_match[1, 3]
    day2 <- range_match[1, 4]
    year <- range_match[1, 5]

    start_date <- mdy(paste(month, day1, year, sep = " "))
    end_date <- mdy(paste(month, day2, year, sep = " "))
    median_date <- start_date + floor(as.numeric(end_date - start_date) / 2)

    return(list(start = start_date, end = end_date, median = median_date))
  }

  # Fallback: NA
  list(start = NA_Date_, end = NA_Date_, median = NA_Date_)
}

#' Extract vote status from sample string: (LV), (RV), (A)
extract_vote_status <- function(sample_str) {
  if (is.na(sample_str)) return(NA_character_)
  m <- str_match(sample_str, "\\(([A-Z]+)\\)")
  if (!is.na(m[1, 2])) m[1, 2] else NA_character_
}

#' Strip percentage sign and convert to numeric (vectorized)
strip_pct <- function(x) {
  ifelse(is.na(x), NA_real_, {
    num_str <- str_remove(x, "%") %>% str_trim()
    as.numeric(num_str)
  })
}

#' Clean pollster name
clean_pollster <- function(x) {
  if (is.na(x)) return(NA_character_)
  x %>%
    str_remove_all("\\([DR]\\)") %>%  # Remove (D), (R) party affiliations
    str_remove_all("\\[[^]]+\\]") %>%  # Remove footnotes
    str_replace_all("/", "_") %>%       # Replace / with _
    str_trim() %>%
    str_squish()
}

#' Determine which candidates are present in each row
get_candidates_in_poll <- function(row) {
  core <- c("mamdani", "cuomo", "adams", "sliwa")
  present <- core[!is.na(row[paste0(core, "_pct")])]
  paste(sort(present), collapse = "+")
}

#' Rank scenarios for primary selection (full_field preferred)
scenario_rank <- function(scenario_type) {
  ifelse(scenario_type == "full_field", 1, 0)
}

# ==============================================================================
# Load Most Recent Raw Data
# ==============================================================================

cat("Loading raw data...\n")
cat("Input file:", basename(input_file), "\n\n")

raw <- read_csv(input_file, col_types = cols(.default = "c"), show_col_types = FALSE)

cat("Raw data dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")

# ==============================================================================
# Filter and Clean
# ==============================================================================

cat("\nFiltering and cleaning...\n")

# Remove event_marker rows for modeling purposes
clean <- raw %>%
  filter(row_type != "event_marker")

cat("After removing event markers:", nrow(clean), "rows\n")

# Parse dates
dates <- map(clean$date_string, parse_date_range) %>% bind_rows()

clean <- clean %>%
  mutate(
    date_start = dates$start,
    date_end = dates$end,
    date_median = dates$median
  )

# Calculate days since first poll
first_poll_date <- min(clean$date_median, na.rm = TRUE)
clean <- clean %>%
  mutate(days_since_first_poll = as.integer(date_median - first_poll_date))

cat("Date range:", as.character(min(clean$date_median, na.rm = TRUE)), "to",
    as.character(max(clean$date_median, na.rm = TRUE)), "\n")

# Parse sample and vote status
clean <- clean %>%
  mutate(
    sample_size = parse_number(sample_size_raw),
    vote_status = map_chr(sample_size_raw, extract_vote_status)
  )

# Convert candidate percentages to numeric and calculate counts
cand_cols <- c("adams", "cuomo", "mamdani", "sliwa", "walden", "other", "undecided")

clean <- clean %>%
  mutate(across(all_of(cand_cols), strip_pct, .names = "{.col}_pct"))

# Calculate counts
for (col in cand_cols) {
  pct_col <- paste0(col, "_pct")
  n_col <- paste0(col, "_n")
  clean[[n_col]] <- round(clean$sample_size * clean[[pct_col]] / 100, 0)
}

# Build identifiers with proper wave grouping
# IMPORTANT: make_clean_names() on vectors adds suffixes for duplicates
# We need to create a lookup table for unique pollsters first
pollster_lookup <- tibble(
  pollster_clean = unique(map_chr(clean$poll_source, clean_pollster))
) %>%
  mutate(pollster_slug = make_clean_names(pollster_clean))

clean <- clean %>%
  mutate(pollster_clean = map_chr(poll_source, clean_pollster)) %>%
  left_join(pollster_lookup, by = "pollster_clean")

# Create provisional grouping key for fill-down
clean <- clean %>%
  mutate(
    g_key = paste(pollster_slug, date_start, date_end, sep = "|")
  )

# Fill-down vote_status and sample_size within each g_key group
# Prefer rows with non-NA vote_status; for sample_size, use max non-NA
clean <- clean %>%
  group_by(g_key) %>%
  mutate(
    vote_status_filled = coalesce(vote_status, first(na.omit(vote_status))),
    sample_size_filled = coalesce(sample_size, max(sample_size, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    vote_status = vote_status_filled,
    sample_size = sample_size_filled
  ) %>%
  select(-vote_status_filled, -sample_size_filled)

# Build final pollster_wave_id: pollster_slug + date_median + vote_status
clean <- clean %>%
  mutate(
    pollster_wave_id = glue("{pollster_slug}_{format(date_median, '%Y%m%d')}_{vote_status}"),
    pollster = pollster_clean  # Alias for backward compatibility
  ) %>%
  select(-g_key)  # Remove provisional key

# Determine which candidates are present in each row
clean <- clean %>%
  rowwise() %>%
  mutate(candidates_in_poll = get_candidates_in_poll(cur_data())) %>%
  ungroup()

# Count number of major candidates (core four)
clean <- clean %>%
  mutate(
    wave_num_candidates = rowSums(!is.na(select(., adams_pct, cuomo_pct, mamdani_pct, sliwa_pct)))
  )

# Mark rows with maximum candidates within each wave
clean <- clean %>%
  group_by(pollster_wave_id) %>%
  mutate(wave_max_candidates = (wave_num_candidates == max(wave_num_candidates, na.rm = TRUE))) %>%
  ungroup()

# Map row_type to scenario_type
clean <- clean %>%
  mutate(
    scenario_type = case_when(
      str_starts(row_type, "head_to_head") ~ "head_to_head",
      row_type == "adams_removed" ~ "adams_removed",
      row_type == "full_field" ~ "full_field",
      TRUE ~ row_type
    )
  )

# Add pre/post Adams withdrawal flag
clean <- clean %>%
  mutate(pre_post_adams_withdrawal = date_median >= as.Date("2025-09-28"))

# ==============================================================================
# Create PRIMARY Rows File (One per Wave)
# ==============================================================================

cat("\nSelecting primary rows (one per poll wave)...\n")

# Selection criteria:
# 1. Highest wave_num_candidates
# 2. Prefer full_field scenario
# 3. Tie-breaker: largest sample_size

primary <- clean %>%
  mutate(scenario_rank = scenario_rank(scenario_type)) %>%
  group_by(pollster_wave_id) %>%
  arrange(desc(wave_num_candidates), desc(scenario_rank), desc(sample_size)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-scenario_rank)

n_waves <- n_distinct(clean$pollster_wave_id)
n_primary <- nrow(primary)

cat("Total waves:", n_waves, "\n")
cat("Primary rows selected:", n_primary, "\n")

# CRITICAL ASSERTION
if (n_primary != n_waves) {
  warning("WARNING: PRIMARY ROWS != WAVES: ", n_primary, " vs ", n_waves)
  cat("\n!!! ASSERTION FAILED: Primary rows should equal wave count !!!\n\n")
} else {
  cat("✓ PASS: Primary rows == wave count\n")
}

# Mark is_primary in clean dataset
# Use exact matching on multiple keys to identify primary rows uniquely
clean <- clean %>%
  left_join(
    primary %>%
      select(pollster_wave_id, scenario_type, wave_num_candidates, sample_size, mamdani_pct, cuomo_pct, adams_pct, sliwa_pct) %>%
      mutate(is_primary_flag = TRUE),
    by = c("pollster_wave_id", "scenario_type", "wave_num_candidates", "sample_size", "mamdani_pct", "cuomo_pct", "adams_pct", "sliwa_pct")
  ) %>%
  mutate(
    is_primary = coalesce(is_primary_flag, FALSE)
  ) %>%
  select(-is_primary_flag)

# ==============================================================================
# Logging: Show Dropped Alternates
# ==============================================================================

cat("\nAlternate scenarios dropped:\n")

alternates <- clean %>%
  anti_join(primary, by = c("pollster_wave_id", "date_string", "sample_size_raw", "scenario_type"))

if (nrow(alternates) > 0) {
  cat("Total alternates dropped:", nrow(alternates), "\n\n")

  # Show top 5 waves with dropped alternates
  dropped_summary <- alternates %>%
    count(pollster_wave_id, pollster, date_string, name = "n_dropped") %>%
    arrange(desc(n_dropped)) %>%
    head(5)

  cat("Top 5 waves with most alternates:\n")
  print(dropped_summary)

  # Detailed view
  cat("\nSample dropped rows:\n")
  alternates %>%
    select(pollster, date_string, scenario_type, vote_status, sample_size, wave_num_candidates) %>%
    head(10) %>%
    print()
} else {
  cat("No alternates dropped (all waves have exactly 1 row)\n")
}

# ==============================================================================
# Wave ID Consistency Tests
# ==============================================================================

cat("\n=== Wave ID Consistency Tests ===\n")

# Test 1: Each (pollster_clean, date_start, date_end, vote_status) should have ONE unique pollster_wave_id
wave_id_check <- clean %>%
  group_by(pollster_clean, date_start, date_end, vote_status) %>%
  summarise(
    n_unique_ids = n_distinct(pollster_wave_id),
    unique_ids = paste(unique(pollster_wave_id), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_unique_ids > 1)

if (nrow(wave_id_check) > 0) {
  cat("\n⚠️  WARNING: Multiple wave IDs for same (pollster, date, vote_status):\n")
  print(wave_id_check)
} else {
  cat("✓ PASS: Each (pollster, date, vote_status) has unique wave ID\n")
}

# Test 2: American Pulse example (August 14-19, 2025 LV rows)
cat("\nAmerican Pulse Research & Polling August 14-19, 2025 LV rows:\n")
american_pulse_example <- clean %>%
  filter(
    str_detect(pollster_clean, regex("American Pulse", ignore_case = TRUE)),
    date_start == as.Date("2025-08-14"),
    date_end == as.Date("2025-08-19"),
    vote_status == "LV"
  ) %>%
  select(pollster_wave_id, scenario_type, wave_num_candidates, is_primary, sample_size,
         mamdani_pct, cuomo_pct, adams_pct, sliwa_pct)

if (nrow(american_pulse_example) > 0) {
  print(american_pulse_example)

  n_unique_ids_ap <- n_distinct(american_pulse_example$pollster_wave_id)
  n_primary_ap <- sum(american_pulse_example$is_primary)

  cat("\n  Unique wave IDs:", n_unique_ids_ap, "\n")
  cat("  Primary rows:", n_primary_ap, "\n")

  if (n_unique_ids_ap == 1 && n_primary_ap == 1) {
    cat("  ✓ PASS: All rows share same wave ID; exactly 1 is primary\n")
  } else {
    cat("  ⚠️  FAIL: Expected 1 unique ID and 1 primary row\n")
  }
} else {
  cat("  (No American Pulse August 14-19 LV rows found)\n")
}

# Test 3: Primary rows == #waves
cat("\nPrimary row count validation:\n")
cat("  Unique wave IDs:", n_waves, "\n")
cat("  Primary rows:", n_primary, "\n")
cat("  Sum of is_primary:", sum(clean$is_primary), "\n")

if (n_primary == n_waves && sum(clean$is_primary) == n_primary) {
  cat("  ✓ PASS: Primary rows == #waves == sum(is_primary)\n")
} else {
  cat("  ⚠️  FAIL: Counts don't match\n")
}

# ==============================================================================
# Summary Statistics
# ==============================================================================

cat("\n=== Summary Statistics ===\n")

cat("\nScenario type distribution (ALL rows):\n")
scenario_all <- table(clean$scenario_type)
print(scenario_all)

cat("\nScenario type distribution (PRIMARY rows):\n")
scenario_primary <- table(primary$scenario_type)
print(scenario_primary)

cat("\nVote status distribution (PRIMARY rows):\n")
vote_primary <- table(primary$vote_status, useNA = "ifany")
print(vote_primary)

cat("\nPre/Post Adams withdrawal (PRIMARY rows):\n")
adams_primary <- table(primary$pre_post_adams_withdrawal)
print(adams_primary)

# ==============================================================================
# Write Markdown Summary
# ==============================================================================

md_content <- glue("
# NYC Mayoral Polling Data Transform Summary

**Timestamp:** {STAMP}

## Input

- **Source:** `{basename(input_file)}`
- **Raw rows:** {nrow(raw)}
- **After filtering event markers:** {nrow(clean)}

## Date Range

- **First poll:** {min(clean$date_median, na.rm = TRUE)}
- **Last poll:** {max(clean$date_median, na.rm = TRUE)}
- **Days span:** {max(clean$days_since_first_poll, na.rm = TRUE)}

## Poll Waves

- **Total waves:** {n_waves}
- **Primary rows selected:** {n_primary}
- **Alternates dropped:** {nrow(alternates)}
- **Assertion:** Primary rows == waves? **{if (n_primary == n_waves) 'PASS ✓' else 'FAIL ✗'}**
- **Wave ID consistency:** {if (nrow(wave_id_check) == 0) 'PASS ✓ - Each (pollster, date, vote_status) has unique wave ID' else glue('FAIL ✗ - {nrow(wave_id_check)} groups have multiple IDs')}

## Scenario Types (PRIMARY rows)

```
{paste(capture.output(print(scenario_primary)), collapse = '\n')}
```

## Vote Status (PRIMARY rows)

```
{paste(capture.output(print(vote_primary)), collapse = '\n')}
```

## Pre/Post Adams Withdrawal (PRIMARY rows)

```
{paste(capture.output(print(adams_primary)), collapse = '\n')}
```

## Top Pollsters (PRIMARY rows)

```
{paste(capture.output(head(sort(table(primary$pollster), decreasing = TRUE), 5)), collapse = '\n')}
```

## Dropped Alternates Summary

{if (nrow(alternates) > 0) {
  glue('
**Total alternates dropped:** {nrow(alternates)}

**Top waves with most alternates:**
```
{paste(capture.output(print(dropped_summary)), collapse = \"\\n\")}
```
')
} else {
  'No alternates dropped (all waves have exactly 1 row)'
}}

## Output Files

- **ALL rows:** `{output_processed}` ({nrow(clean)} rows)
- **PRIMARY rows:** `{output_primary}` ({nrow(primary)} rows)

## Notes

- Event marker rows excluded from modeling data
- PRIMARY selection: full_field preference → max sample size
- `pollster_wave_id` = pollster_slug + date_median (YYYYmmdd)
- `candidates_in_poll` kept for diagnostics only (not for fixed effects)
- `vote_status` (LV/RV/A) preserved for analysis

---

Generated by `R/02_clean_transform.R`
")

# ==============================================================================
# Write Output Files (guarded by dryrun)
# ==============================================================================

if (dryrun) {
  cat("\n=== DRY RUN MODE ===\n")
  cat("Would write:\n")
  cat("  - Processed CSV:", output_processed, "(", nrow(clean), "rows )\n")
  cat("  - Primary CSV:", output_primary, "(", nrow(primary), "rows )\n")
  cat("  - Report:", report_file, "\n")
  cat("\nRe-run with --dryrun=false to write files.\n")
} else {
  cat("\n=== Writing output files ===\n")

  # Create output directories
  dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("analysis"), recursive = TRUE, showWarnings = FALSE)

  # Write ALL rows file
  write_csv(clean, here(output_processed), na = "")
  cat("✓ ALL rows:", output_processed, "(", nrow(clean), "rows )\n")

  # Write PRIMARY rows file
  write_csv(primary, here(output_primary), na = "")
  cat("✓ PRIMARY rows:", output_primary, "(", nrow(primary), "rows )\n")

  # Write markdown summary
  writeLines(md_content, here(report_file))
  cat("✓ Summary:", report_file, "\n")

  cat("\n✓ Transform complete!\n")
}
