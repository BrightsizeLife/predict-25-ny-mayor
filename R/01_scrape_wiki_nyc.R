#!/usr/bin/env Rscript
# ==============================================================================
# 01_scrape_wiki_nyc.R (v3)
# Scrape Wikipedia "2025 New York City mayoral election — General election polls"
# Uses html_table(fill=TRUE) + poll grouping + fill-down to handle rowspan
# ==============================================================================

# Parse CLI arguments
args <- commandArgs(trailingOnly = TRUE)
dryrun <- TRUE
url <- "https://en.wikipedia.org/wiki/2025_New_York_City_mayoral_election"
snapshot <- TRUE

if (length(args) > 0) {
  for (arg in args) {
    if (grepl("^--dryrun=", arg)) {
      dryrun <- as.logical(sub("^--dryrun=", "", arg))
    } else if (grepl("^--url=", arg)) {
      url <- sub("^--url=", "", arg)
    } else if (grepl("^--snapshot=", arg)) {
      snapshot <- as.logical(sub("^--snapshot=", "", arg))
    }
  }
}

# Load dependencies
suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(tidyverse)
  library(janitor)
  library(glue)
  library(digest)
})

STAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

cat("=== NYC Mayoral Polling Scraper v3 ===\n")
cat("URL:", url, "\n")
cat("Dry run:", dryrun, "\n")
cat("Snapshot on drift:", snapshot, "\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Normalize text: unicode whitespace and dashes
norm_txt <- function(x) {
  x %>%
    str_replace_all("\u00A0", " ") %>%           # NBSP
    str_replace_all("[\u2013\u2014]", "-") %>%   # em-dash, en-dash
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

#' Strip footnotes: <sup> tags and [m], [316] brackets
strip_footnotes <- function(x) {
  x %>%
    str_remove_all("<sup>.*?</sup>") %>%
    str_remove_all("\\[[[:alnum:]]+\\]") %>%
    str_trim()
}

#' Clean cell: normalize + strip + convert dashes to NA
clean_cell <- function(x) {
  x <- as.character(x) %>% norm_txt() %>% strip_footnotes()
  ifelse(x %in% c("", "-", "—", "–", "-%", "—%", "–%"), NA_character_, x)
}

#' Extract numeric percent from text
pct_num <- function(x) {
  if (is.na(x)) return(NA_real_)
  m <- str_match(x, "([0-9]+(?:\\.[0-9]+)?)%?")
  as.numeric(m[, 2])
}

#' Extract vote status (LV/RV/A) from sample string
vote_status_from_sample <- function(x) {
  if (is.na(x)) return(NA_character_)
  m <- str_match(x, "\\(([A-Za-z]+)\\)")
  toupper(coalesce(m[, 2], NA_character_))
}

#' Check if any candidate column has a value
has_any_candidate <- function(df) {
  cand_cols <- c("adams", "cuomo", "mamdani", "sliwa", "walden", "other", "undecided")
  rowSums(!is.na(df[intersect(cand_cols, names(df))])) > 0
}

# ==============================================================================
# Fetch and Parse Table
# ==============================================================================

cat("Fetching page...\n")
page <- tryCatch({
  read_html(url)
}, error = function(e) {
  stop("ERROR: Failed to fetch page: ", e$message)
})

cat("Locating polling section...\n")

# Robust XPath: try multiple strategies
table_node <- tryCatch({
  # Strategy 1: Direct element with id, following-sibling table
  xpath1 <- "//*[@id='General_election_polls']/following-sibling::table[contains(@class,'wikitable')][1]"
  tables <- xml_find_all(page, xpath1)

  # Strategy 2: span with id inside heading
  if (length(tables) == 0) {
    xpath2 <- "//span[@id='General_election_polls']/ancestor::*[self::h2 or self::h3 or self::h4][1]/following-sibling::table[contains(@class,'wikitable')][1]"
    tables <- xml_find_all(page, xpath2)
  }

  # Strategy 3: Any element with id, then following table
  if (length(tables) == 0) {
    xpath3 <- "//*[@id='General_election_polls']/following::table[contains(@class,'wikitable')][1]"
    tables <- xml_find_all(page, xpath3)
  }

  if (length(tables) == 0) {
    stop("Section #General_election_polls table not found with any selector strategy")
  }

  tables[[1]]
}, error = function(e) {
  stop("ERROR: ", e$message)
})

cat("✓ Table found\n\n")

# ==============================================================================
# Process Table with html_table + Post-processing
# ==============================================================================

cat("Parsing table with html_table(fill=TRUE)...\n")

# Use html_table to handle rowspan/colspan
df_raw <- html_table(table_node, fill = TRUE) %>% as_tibble()

cat("Raw table dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "columns\n")

# Clean column names
names(df_raw) <- make_clean_names(names(df_raw))

# Clean all cells
df <- df_raw %>% mutate(across(everything(), clean_cell))

# Keep row-wide text to detect event bars
row_text <- df %>% unite("_row_text_all_", everything(), sep = " | ", na.rm = TRUE)
df$row_text_all <- row_text$`_row_text_all_`

# Fuzzy locate important columns
col_poll <- names(df)[str_detect(names(df), "poll|source")][1]
col_date <- names(df)[str_detect(names(df), "date")][1]
col_sample <- names(df)[str_detect(names(df), "sample")][1]
col_moe <- names(df)[str_detect(names(df), "margin|error")][1]

cat("\nColumn mapping:\n")
cat("  poll_source:", col_poll, "\n")
cat("  date_string:", col_date, "\n")
cat("  sample_size_raw:", col_sample, "\n")
cat("  margin_of_error:", col_moe, "\n")

# Candidate columns
cand_cols <- c(
  adams = names(df)[str_detect(names(df), "adams")][1],
  cuomo = names(df)[str_detect(names(df), "cuomo")][1],
  mamdani = names(df)[str_detect(names(df), "mamdani")][1],
  sliwa = names(df)[str_detect(names(df), "sliwa")][1],
  walden = names(df)[str_detect(names(df), "walden")][1],
  other = names(df)[str_detect(names(df), "^other$|other_")][1],
  undec = names(df)[str_detect(names(df), "undecided")][1]
)
cand_cols <- compact(cand_cols)

cat("  Candidate columns:", paste(names(cand_cols), collapse = ", "), "\n\n")

# Build canonical tibble
sub <- tibble(
  poll_source = coalesce(df[[col_poll]], NA_character_),
  date_string = coalesce(df[[col_date]], NA_character_),
  sample_size_raw = coalesce(df[[col_sample]], NA_character_),
  margin_of_error = coalesce(df[[col_moe]], NA_character_),
  adams = if (!is.null(cand_cols["adams"])) df[[cand_cols["adams"]]] else NA_character_,
  cuomo = if (!is.null(cand_cols["cuomo"])) df[[cand_cols["cuomo"]]] else NA_character_,
  mamdani = if (!is.null(cand_cols["mamdani"])) df[[cand_cols["mamdani"]]] else NA_character_,
  sliwa = if (!is.null(cand_cols["sliwa"])) df[[cand_cols["sliwa"]]] else NA_character_,
  walden = if (!is.null(cand_cols["walden"])) df[[cand_cols["walden"]]] else NA_character_,
  other = if (!is.null(cand_cols["other"])) df[[cand_cols["other"]]] else NA_character_,
  undecided = if (!is.null(cand_cols["undec"])) df[[cand_cols["undec"]]] else NA_character_,
  row_text_all = df$row_text_all
)

# ==============================================================================
# Poll Grouping and Fill-Down
# ==============================================================================

cat("Detecting poll groups...\n")

# Identify event rows
is_event <- str_detect(sub$row_text_all, regex("withdraws from the race", ignore_case = TRUE))

# Identify new poll starts (revised rule)
# New poll if: poll_source present OR date_string present OR event row OR sample has (LV/RV/A)
has_vote_status <- !is.na(sub$sample_size_raw) & str_detect(sub$sample_size_raw, "\\([A-Za-z]+\\)")
is_new <- !is.na(sub$poll_source) | !is.na(sub$date_string) | is_event | has_vote_status

group_id <- cumsum(coalesce(is_new, FALSE))
sub$group_id <- group_id

n_groups <- max(group_id, na.rm = TRUE)
cat("Detected", n_groups, "poll groups\n\n")

# Fill down metadata within each group
cat("Filling down metadata within groups...\n")
sub <- sub %>%
  group_by(group_id) %>%
  fill(poll_source, date_string, sample_size_raw, margin_of_error, .direction = "down") %>%
  ungroup()

# Show first 3 groups for eyeball check
cat("First 3 groups (eyeball fill-down):\n")
sub %>%
  filter(group_id <= 3) %>%
  select(group_id, poll_source, date_string, sample_size_raw, adams, cuomo, mamdani) %>%
  print(n = 20)

# ==============================================================================
# Row Classification
# ==============================================================================

cat("\nClassifying row types...\n")

# Helper: check if exactly 2 of 4 major candidates present
two_of_four <- function(df) {
  major <- c("adams", "cuomo", "mamdani", "sliwa")
  rowSums(!is.na(df[major])) == 2
}

# Helper: get names of present candidates
get_candidate_names <- function(row) {
  candidates <- c("adams", "cuomo", "mamdani", "sliwa")
  present <- candidates[!is.na(row[candidates])]
  if (length(present) == 2) {
    paste0("head_to_head_", paste(sort(present), collapse = "_vs_"))
  } else {
    NA_character_
  }
}

# Classify
sub <- sub %>%
  rowwise() %>%
  mutate(
    row_type = case_when(
      str_detect(row_text_all, regex("withdraws from the race", ignore_case = TRUE)) ~ "event_marker",
      sum(!is.na(c(adams, cuomo, mamdani, sliwa))) == 2 ~ get_candidate_names(cur_data()),
      is.na(adams) & sum(!is.na(c(mamdani, cuomo, sliwa))) >= 2 ~ "adams_removed",
      TRUE ~ "full_field"
    )
  ) %>%
  ungroup()

# ==============================================================================
# Filter Junk Rows
# ==============================================================================

cat("Filtering junk rows...\n")

# Drop rows where ALL candidates AND margin_of_error are NA (unless event_marker)
all_cand_na <- rowSums(!is.na(sub[c("adams", "cuomo", "mamdani", "sliwa", "walden", "other", "undecided")])) == 0
moe_na <- is.na(sub$margin_of_error)
is_junk <- all_cand_na & moe_na & sub$row_type != "event_marker"

n_before <- nrow(sub)
sub <- sub[!is_junk, ]
n_after <- nrow(sub)

cat("Dropped", n_before - n_after, "junk rows\n")

# ==============================================================================
# Deduplication
# ==============================================================================

cat("\nDeduplicating...\n")

# Create dedupe key
sub <- sub %>%
  mutate(
    dedupe_key = paste(
      poll_source, date_string, sample_size_raw, margin_of_error,
      adams, cuomo, mamdani, sliwa, walden, other, undecided, row_type,
      sep = "|"
    )
  )

n_before_dedup <- nrow(sub)
duplicates <- sum(duplicated(sub$dedupe_key))
sub <- sub %>% distinct(dedupe_key, .keep_all = TRUE)
n_after_dedup <- nrow(sub)

cat("Duplicates dropped:", duplicates, "\n")

# ==============================================================================
# Finalize Output
# ==============================================================================

cat("\nFinalizing output...\n")

out <- sub %>%
  mutate(scrape_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) %>%
  select(
    scrape_timestamp, poll_source, date_string, sample_size_raw, margin_of_error,
    adams, cuomo, mamdani, sliwa, walden, other, undecided,
    row_type
  )

# ==============================================================================
# Summary and Output
# ==============================================================================

cat("\n=== Processing Summary ===\n")
cat("Groups detected:", n_groups, "\n")
cat("Junk rows dropped:", n_before - n_after, "\n")
cat("Duplicates dropped:", duplicates, "\n")
cat("Final rows:", nrow(out), "\n\n")

cat("Row type distribution:\n")
row_type_table <- out %>% count(row_type, name = "count")
print(row_type_table)

cat("\n=== Sample (first 5 rows) ===\n")
out %>%
  select(poll_source, date_string, sample_size_raw, mamdani, cuomo, adams, row_type) %>%
  head(5) %>%
  print()

if (dryrun) {
  cat("\n✓ DRY RUN complete. No file written.\n")
  cat("  Run with --dryrun=false to save CSV.\n")
} else {
  # Write to timestamped CSV
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  output_path <- glue("data/raw/polls_{STAMP}.csv")

  write_csv(out, output_path, na = "")

  cat("\n✓ SUCCESS: Scraped data written to", output_path, "\n")
  cat("  Rows:", nrow(out), "\n")
  cat("  Columns:", ncol(out), "\n")
}

cat("\n")
