#!/usr/bin/env Rscript
# ==============================================================================
# 02_clean_transform.R
# Clean and transform raw polling data
# Creates ALL rows file and PRIMARY rows file (one per poll wave)
# ==============================================================================

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

cat("=== NYC Mayoral Polling Data Transform ===\n")
cat("Timestamp:", STAMP, "\n\n")

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

#' Strip percentage sign and convert to numeric
strip_pct <- function(x) {
  if (is.na(x)) return(NA_real_)
  num_str <- str_remove(x, "%") %>% str_trim()
  as.numeric(num_str)
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

# ==============================================================================
# Load Most Recent Raw Data
# ==============================================================================

cat("Loading raw data...\n")

# Find most recent polls CSV
raw_files <- list.files(here("data", "raw"), pattern = "^polls_.*\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) {
  stop("ERROR: No raw polling CSV files found in data/raw/")
}

latest_file <- raw_files[order(file.info(raw_files)$mtime, decreasing = TRUE)][1]
cat("Using:", basename(latest_file), "\n\n")

raw <- read_csv(latest_file, col_types = cols(.default = "c"), show_col_types = FALSE)

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

# Build identifiers
clean <- clean %>%
  mutate(
    pollster = map_chr(poll_source, clean_pollster),
    pollster_slug = make_clean_names(pollster),
    poll_wave_id = glue("{pollster_slug}_{format(date_median, '%Y%m%d')}")
  )

# Determine which candidates are present in each row
get_candidates_in_poll <- function(row) {
  core <- c("mamdani", "cuomo", "adams", "sliwa", "other", "undecided")
  present <- core[!is.na(row[paste0(core, "_pct")])]
  paste(sort(present), collapse = "+")
}

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
  group_by(poll_wave_id) %>%
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
# 2. Tie-breaker: vote_status (LV > RV > A)
# 3. Tie-breaker: largest sample_size

vote_status_rank <- function(vs) {
  case_when(
    vs == "LV" ~ 3,
    vs == "RV" ~ 2,
    vs == "A" ~ 1,
    TRUE ~ 0
  )
}

primary <- clean %>%
  mutate(vote_rank = vote_status_rank(vote_status)) %>%
  group_by(poll_wave_id) %>%
  arrange(desc(wave_num_candidates), desc(vote_rank), desc(sample_size)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-vote_rank)

n_waves <- n_distinct(clean$poll_wave_id)
n_primary <- nrow(primary)

cat("Total waves:", n_waves, "\n")
cat("Primary rows selected:", n_primary, "\n")

# ==============================================================================
# Logging: Show Dropped Alternates
# ==============================================================================

cat("\nAlternate scenarios dropped:\n")

alternates <- clean %>%
  anti_join(primary, by = c("poll_wave_id", "date_string", "sample_size_raw", "scenario_type"))

if (nrow(alternates) > 0) {
  cat("Total alternates dropped:", nrow(alternates), "\n\n")

  # Show top 5 waves with dropped alternates
  dropped_summary <- alternates %>%
    count(poll_wave_id, pollster, date_string, name = "n_dropped") %>%
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
# Summary Statistics
# ==============================================================================

cat("\n=== Summary Statistics ===\n")

cat("\nScenario type distribution (ALL rows):\n")
print(table(clean$scenario_type))

cat("\nScenario type distribution (PRIMARY rows):\n")
print(table(primary$scenario_type))

cat("\nVote status distribution (PRIMARY rows):\n")
print(table(primary$vote_status, useNA = "ifany"))

cat("\nPre/Post Adams withdrawal (PRIMARY rows):\n")
print(table(primary$pre_post_adams_withdrawal))

# ==============================================================================
# Write Output Files
# ==============================================================================

cat("\n=== Writing output files ===\n")

# Create output directory
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("analysis"), recursive = TRUE, showWarnings = FALSE)

# Write ALL rows file
all_output <- glue("data/processed/polls_cleaned_{STAMP}.csv")
write_csv(clean, here(all_output), na = "")
cat("✓ ALL rows:", all_output, "(", nrow(clean), "rows )\n")

# Write PRIMARY rows file
primary_output <- glue("data/processed/polls_primary_{STAMP}.csv")
write_csv(primary, here(primary_output), na = "")
cat("✓ PRIMARY rows:", primary_output, "(", nrow(primary), "rows )\n")

# ==============================================================================
# Write Markdown Summary
# ==============================================================================

summary_md <- glue("analysis/transform_summary_{STAMP}.md")

md_content <- glue("
# NYC Mayoral Polling Data Transform Summary

**Timestamp:** {STAMP}

## Input

- **Source:** `{basename(latest_file)}`
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

## Scenario Types (PRIMARY rows)

```
{paste(capture.output(table(primary$scenario_type)), collapse = '\n')}
```

## Vote Status (PRIMARY rows)

```
{paste(capture.output(table(primary$vote_status, useNA = 'ifany')), collapse = '\n')}
```

## Pre/Post Adams Withdrawal (PRIMARY rows)

```
{paste(capture.output(table(primary$pre_post_adams_withdrawal)), collapse = '\n')}
```

## Top Pollsters (PRIMARY rows)

```
{paste(capture.output(head(sort(table(primary$pollster), decreasing = TRUE), 5)), collapse = '\n')}
```

## Output Files

- **ALL rows:** `{all_output}` ({nrow(clean)} rows)
- **PRIMARY rows:** `{primary_output}` ({nrow(primary)} rows)

## Notes

- Event marker rows excluded from modeling data
- PRIMARY selection: max candidates → LV > RV > A → max sample size
- `poll_wave_id` = pollster_slug + date_median
- `candidates_in_poll` kept for diagnostics only (not for fixed effects)

---

Generated by `R/02_clean_transform.R`
")

writeLines(md_content, here(summary_md))
cat("✓ Summary:", summary_md, "\n")

cat("\n✓ Transform complete!\n")
