#!/usr/bin/env Rscript
# ==============================================================================
# 01_scrape_wiki_nyc.R
# Scrape Wikipedia "2025 New York City mayoral election — General election polls"
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
  library(lubridate)
  library(glue)
  library(digest)
})

cat("=== NYC Mayoral Polling Scraper ===\n")
cat("URL:", url, "\n")
cat("Dry run:", dryrun, "\n")
cat("Snapshot on drift:", snapshot, "\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Clean HTML text: strip footnotes, normalize dashes, remove non-breaking spaces
clean_html_text <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)

  # Convert to UTF-8
  text <- enc2utf8(as.character(text))

  # Define unicode characters as hex codes to avoid encoding issues
  nbsp <- intToUtf8(0x00A0)     # non-breaking space
  emdash <- intToUtf8(0x2014)   # em-dash
  endash <- intToUtf8(0x2013)   # en-dash

  text <- text %>%
    # Remove footnote brackets like [m], [316], etc.
    str_remove_all("\\[[[:alnum:]]+\\]") %>%
    # Remove non-breaking spaces
    str_replace_all(nbsp, " ") %>%
    # Normalize em-dash and en-dash to simple dash
    str_replace_all(emdash, "-") %>%
    str_replace_all(endash, "-") %>%
    # Remove percentage signs (will parse later)
    str_remove_all("%") %>%
    # Trim whitespace
    str_trim()

  # Convert dashes and empty strings to NA
  if (text %in% c("-", "", "NA")) {
    return(NA_character_)
  }

  text
}

#' Detect row type based on content
detect_row_type <- function(row_data) {
  # Convert to character vector for checking
  row_str <- paste(unlist(row_data), collapse = " ")

  # Event marker: "Adams withdraws"
  if (str_detect(row_str, regex("adams.*withdraw", ignore_case = TRUE))) {
    return("event_marker")
  }

  # Check which major candidates have values
  has_mamdani <- !is.na(row_data$mamdani) && row_data$mamdani != ""
  has_cuomo <- !is.na(row_data$cuomo) && row_data$cuomo != ""
  has_adams <- !is.na(row_data$adams) && row_data$adams != ""
  has_sliwa <- !is.na(row_data$sliwa) && row_data$sliwa != ""

  candidate_count <- sum(c(has_mamdani, has_cuomo, has_adams, has_sliwa))

  # Full field: all 4 major candidates present
  if (candidate_count == 4) {
    return("full_field")
  }

  # Adams removed: Adams is NA but others present
  if (!has_adams && candidate_count >= 2) {
    return("adams_removed")
  }

  # Head-to-head: only 2 candidates
  if (candidate_count == 2) {
    candidates <- c()
    if (has_mamdani) candidates <- c(candidates, "mamdani")
    if (has_cuomo) candidates <- c(candidates, "cuomo")
    if (has_adams) candidates <- c(candidates, "adams")
    if (has_sliwa) candidates <- c(candidates, "sliwa")
    return(paste0("head_to_head_", paste(candidates, collapse = "_")))
  }

  # Default
  return("partial")
}

#' Fill down metadata columns (poll_source, date_string, sample_size_raw)
fill_down_metadata <- function(df) {
  df %>%
    mutate(
      poll_source = if_else(is.na(poll_source) | poll_source == "",
                            NA_character_, poll_source),
      date_string = if_else(is.na(date_string) | date_string == "",
                            NA_character_, date_string),
      sample_size_raw = if_else(is.na(sample_size_raw) | sample_size_raw == "",
                                NA_character_, sample_size_raw)
    ) %>%
    fill(poll_source, date_string, sample_size_raw, .direction = "down")
}

# ==============================================================================
# Main Scraping Logic
# ==============================================================================

cat("Fetching page...\n")
page <- tryCatch({
  read_html(url)
}, error = function(e) {
  stop("ERROR: Failed to fetch page: ", e$message)
})

cat("Locating polling section...\n")

# Use XPath to find the anchor, then the following table
table_node <- tryCatch({
  # Find the General_election_polls anchor
  anchor <- page %>%
    html_nodes(xpath = "//*[@id='General_election_polls']")

  if (length(anchor) == 0) {
    stop("Section #General_election_polls not found")
  }

  # Find first wikitable following the anchor
  table <- page %>%
    html_nodes(xpath = "//*[@id='General_election_polls']/following::table[contains(@class,'wikitable')][1]")

  if (length(table) == 0) {
    stop("No wikitable found after #General_election_polls")
  }

  table[[1]]
}, error = function(e) {
  stop("ERROR: ", e$message)
})

cat("✓ Table found\n\n")

# Parse table
cat("Parsing table...\n")
raw_table <- table_node %>%
  html_table(fill = TRUE, header = TRUE)

cat("Raw dimensions:", nrow(raw_table), "rows x", ncol(raw_table), "columns\n")

# Log headers
headers <- colnames(raw_table)
header_hash <- digest(headers, algo = "md5")

cat("\nDiscovered headers:\n")
print(headers)
cat("\nHeader hash:", header_hash, "\n")

# Expected headers (flexible matching)
expected_core <- c("Poll source", "Date", "Sample", "Margin")
has_expected <- sum(str_detect(headers, regex(paste(expected_core, collapse = "|"),
                                               ignore_case = TRUE)))

if (has_expected < 3) {
  warning("WARN: Schema drift detected - fewer than 3 expected core columns found")

  if (snapshot) {
    snapshot_path <- glue("data/raw/html_snapshot_{format(Sys.time(), '%Y%m%d_%H%M%S')}.html")
    dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
    write_html(page, snapshot_path)
    cat("INFO: HTML snapshot saved to", snapshot_path, "\n")
  }
}

# ==============================================================================
# Clean and Transform
# ==============================================================================

cat("\nCleaning table data...\n")

# Standardize column names
df <- raw_table %>%
  janitor::clean_names()

# Identify key columns by pattern matching (case-insensitive)
col_map <- list()

for (col in names(df)) {
  if (str_detect(col, regex("poll.*source", ignore_case = TRUE))) {
    col_map$poll_source <- col
  } else if (str_detect(col, regex("date", ignore_case = TRUE))) {
    col_map$date_string <- col
  } else if (str_detect(col, regex("sample", ignore_case = TRUE))) {
    col_map$sample_size_raw <- col
  } else if (str_detect(col, regex("margin", ignore_case = TRUE))) {
    col_map$margin_of_error <- col
  } else if (str_detect(col, regex("mamdani", ignore_case = TRUE))) {
    col_map$mamdani <- col
  } else if (str_detect(col, regex("cuomo", ignore_case = TRUE))) {
    col_map$cuomo <- col
  } else if (str_detect(col, regex("adams", ignore_case = TRUE))) {
    col_map$adams <- col
  } else if (str_detect(col, regex("sliwa", ignore_case = TRUE))) {
    col_map$sliwa <- col
  } else if (str_detect(col, regex("walden", ignore_case = TRUE))) {
    col_map$walden <- col
  } else if (str_detect(col, regex("^other$", ignore_case = TRUE))) {
    col_map$other <- col
  } else if (str_detect(col, regex("undecided", ignore_case = TRUE))) {
    col_map$undecided <- col
  }
}

# Rename columns using the map
df_renamed <- df
for (new_name in names(col_map)) {
  old_name <- col_map[[new_name]]
  if (old_name %in% names(df_renamed)) {
    names(df_renamed)[names(df_renamed) == old_name] <- new_name
  }
}

# Ensure all expected columns exist
required_cols <- c("poll_source", "date_string", "sample_size_raw", "margin_of_error",
                   "mamdani", "cuomo", "adams", "sliwa", "other", "undecided")

for (col in required_cols) {
  if (!col %in% names(df_renamed)) {
    df_renamed[[col]] <- NA_character_
  }
}

# If walden exists, combine with other
if ("walden" %in% names(df_renamed)) {
  df_renamed <- df_renamed %>%
    mutate(
      other = if_else(!is.na(walden) & walden != "", walden, other)
    ) %>%
    select(-walden)
}

# Select only required columns
df_selected <- df_renamed %>%
  select(poll_source, date_string, sample_size_raw, margin_of_error,
         mamdani, cuomo, adams, sliwa, other, undecided)

# Clean all text fields
df_clean <- df_selected %>%
  mutate(across(everything(), ~ map_chr(.x, clean_html_text)))

# Fill down metadata (handles rowspan)
df_filled <- fill_down_metadata(df_clean)

# Detect row types
cat("Detecting row types...\n")
df_typed <- df_filled %>%
  rowwise() %>%
  mutate(row_type = detect_row_type(pick(everything()))) %>%
  ungroup()

# Add scrape timestamp and notes placeholder
df_final <- df_typed %>%
  mutate(
    scrape_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    notes = NA_character_
  ) %>%
  select(scrape_timestamp, poll_source, date_string, sample_size_raw,
         margin_of_error, adams, cuomo, mamdani, sliwa, other, undecided,
         row_type, notes)

# ==============================================================================
# Summary and Output
# ==============================================================================

cat("\n=== Processing Summary ===\n")
cat("Total rows:", nrow(df_final), "\n")
cat("Row type distribution:\n")
print(table(df_final$row_type))

cat("\n=== Sample (first 5 rows) ===\n")
print(df_final %>% select(poll_source, date_string, mamdani, cuomo, adams, row_type) %>% head(5))

if (dryrun) {
  cat("\n✓ DRY RUN complete. No file written.\n")
  cat("  Run with --dryrun=false to save CSV.\n")
} else {
  # Write to timestamped CSV
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  output_path <- glue("data/raw/polls_{format(Sys.time(), '%Y%m%d_%H%M%S')}.csv")

  write_csv(df_final, output_path, na = "")

  cat("\n✓ SUCCESS: Scraped data written to", output_path, "\n")
  cat("  Rows:", nrow(df_final), "\n")
  cat("  Columns:", ncol(df_final), "\n")
}

cat("\n")
