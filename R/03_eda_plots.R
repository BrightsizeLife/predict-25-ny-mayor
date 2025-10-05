#!/usr/bin/env Rscript
# ==============================================================================
# 03_eda_plots.R
# Exploratory Data Analysis with neon theme
# Generates 12 high-resolution plots + markdown report
# ==============================================================================

# Parse CLI arguments
args <- commandArgs(trailingOnly = TRUE)
input_cleaned <- NULL
input_primary <- NULL
out_dir <- "plots"
report_file <- NULL
dryrun <- TRUE

if (length(args) > 0) {
  for (arg in args) {
    if (grepl("^--input_cleaned=", arg)) {
      input_cleaned <- sub("^--input_cleaned=", "", arg)
    } else if (grepl("^--input_primary=", arg)) {
      input_primary <- sub("^--input_primary=", "", arg)
    } else if (grepl("^--out_dir=", arg)) {
      out_dir <- sub("^--out_dir=", "", arg)
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
  library(here)
  library(glue)
  library(scales)
  library(ggridges)
})

STAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Set defaults if not provided
if (is.null(input_cleaned)) {
  cleaned_files <- list.files(here("data", "processed"),
                               pattern = "^polls_cleaned_.*\\.csv$",
                               full.names = TRUE)
  if (length(cleaned_files) == 0) {
    stop("ERROR: No cleaned CSV files found in data/processed/")
  }
  input_cleaned <- cleaned_files[order(file.info(cleaned_files)$mtime, decreasing = TRUE)][1]
}

if (is.null(input_primary)) {
  primary_files <- list.files(here("data", "processed"),
                               pattern = "^polls_primary_.*\\.csv$",
                               full.names = TRUE)
  if (length(primary_files) > 0) {
    input_primary <- primary_files[order(file.info(primary_files)$mtime, decreasing = TRUE)][1]
  }
}

if (is.null(report_file)) report_file <- glue("analysis/03_eda_report_{STAMP}.md")

cat("=== NYC Mayoral Polling EDA ===\n")
cat("Timestamp:", STAMP, "\n")
cat("Dry run:", dryrun, "\n\n")

# ==============================================================================
# Load Theme and Data
# ==============================================================================

cat("Loading theme and data...\n")
source(here("R", "theme_cs.R"))

cleaned <- read_csv(input_cleaned, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  mutate(
    date_median = as.Date(date_median),
    across(ends_with("_pct"), as.numeric),
    sample_size = as.numeric(sample_size),
    pre_post_adams_withdrawal = as.logical(pre_post_adams_withdrawal)
  )

primary <- if (!is.null(input_primary)) {
  read_csv(input_primary, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(
      date_median = as.Date(date_median),
      across(ends_with("_pct"), as.numeric),
      sample_size = as.numeric(sample_size)
    )
} else {
  NULL
}

cat("Cleaned data:", nrow(cleaned), "rows\n")
if (!is.null(primary)) cat("Primary data:", nrow(primary), "rows\n")

# ==============================================================================
# Summary Statistics
# ==============================================================================

cat("\nComputing summary statistics...\n")

n_rows_cleaned <- nrow(cleaned)
n_waves <- n_distinct(cleaned$pollster_wave_id)
min_date <- min(cleaned$date_median, na.rm = TRUE)
max_date <- max(cleaned$date_median, na.rm = TRUE)
days_span <- as.integer(max_date - min_date)

scenario_type_counts <- cleaned %>%
  count(scenario_type, sort = TRUE)

vote_status_counts <- cleaned %>%
  count(vote_status, sort = TRUE)

top_pollsters <- cleaned %>%
  count(pollster_clean, sort = TRUE) %>%
  head(8)

# Waves with scenario variants
waves_with_alternates <- cleaned %>%
  group_by(pollster_wave_id) %>%
  summarise(n_scenarios = n(), .groups = "drop") %>%
  filter(n_scenarios > 1)

n_waves_with_alternates <- nrow(waves_with_alternates)
n_waves_single <- n_waves - n_waves_with_alternates

# Missingness rates
missingness <- cleaned %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "pct_missing") %>%
  filter(pct_missing > 0) %>%
  arrange(desc(pct_missing))

# Percent sum diagnostics
cand_cols <- c("mamdani_pct", "cuomo_pct", "adams_pct", "sliwa_pct", "other_pct", "undecided_pct")
cleaned <- cleaned %>%
  mutate(row_sum = rowSums(select(., all_of(cand_cols)), na.rm = TRUE))

pct_sum_mean <- mean(cleaned$row_sum, na.rm = TRUE)
pct_sum_sd <- sd(cleaned$row_sum, na.rm = TRUE)
pct_sum_min <- min(cleaned$row_sum, na.rm = TRUE)
pct_sum_max <- max(cleaned$row_sum, na.rm = TRUE)
n_low_sum <- sum(cleaned$row_sum < 97, na.rm = TRUE)
n_high_sum <- sum(cleaned$row_sum > 103, na.rm = TRUE)
pct_low_sum <- round(n_low_sum / n_rows_cleaned * 100, 1)
pct_high_sum <- round(n_high_sum / n_rows_cleaned * 100, 1)

# Pre/post Adams
pre_post_counts <- cleaned %>%
  count(pre_post_adams_withdrawal)

# ==============================================================================
# Prepare Long Format for Plotting
# ==============================================================================

cat("Preparing long format data...\n")

cleaned_long <- cleaned %>%
  select(pollster_wave_id, pollster_clean, date_median, scenario_type, vote_status,
         sample_size, pre_post_adams_withdrawal,
         mamdani_pct, cuomo_pct, adams_pct, sliwa_pct, other_pct, undecided_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "candidate",
    values_to = "pct"
  ) %>%
  mutate(
    candidate = str_remove(candidate, "_pct"),
    candidate = factor(candidate, levels = names(cs_palette))
  ) %>%
  filter(!is.na(pct))

# ==============================================================================
# Plot 1: Candidate % over Time (Time Series)
# ==============================================================================

cat("Generating Plot 1: Candidate % over time...\n")

p1 <- cleaned_long %>%
  ggplot(aes(x = date_median, y = pct, color = candidate)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 0.7, linewidth = 1.2) +
  scale_color_manual(values = cs_palette) +
  labs(
    title = "Candidate Support Over Time (All Scenarios)",
    subtitle = glue("N={n_rows_cleaned} rows | {min_date} to {max_date}"),
    x = "Poll Date (Median)",
    y = "Support (%)",
    color = "Candidate"
  ) +
  facet_wrap(~candidate, ncol = 3, scales = "free_y")

# ==============================================================================
# Plot 2: Margins over Time
# ==============================================================================

cat("Generating Plot 2: Margins over time...\n")

margins <- cleaned %>%
  filter(!is.na(mamdani_pct) & !is.na(cuomo_pct)) %>%
  mutate(
    mamdani_cuomo_margin = mamdani_pct - cuomo_pct,
    mamdani_adams_margin = if_else(!is.na(adams_pct), mamdani_pct - adams_pct, NA_real_)
  ) %>%
  select(date_median, scenario_type, mamdani_cuomo_margin, mamdani_adams_margin) %>%
  pivot_longer(
    cols = ends_with("_margin"),
    names_to = "margin_type",
    values_to = "margin"
  ) %>%
  filter(!is.na(margin)) %>%
  mutate(
    margin_type = str_remove(margin_type, "_margin"),
    margin_type = str_replace_all(margin_type, "_", " "),
    margin_type = str_to_title(margin_type)
  )

p2 <- margins %>%
  ggplot(aes(x = date_median, y = margin, color = margin_type)) +
  geom_hline(yintercept = 0, color = "#B0B0B0", linetype = "dashed", linewidth = 0.8) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 0.7, linewidth = 1.2) +
  scale_color_manual(values = c("Mamdani Cuomo" = "#73FF6B", "Mamdani Adams" = "#E6FF00")) +
  labs(
    title = "Polling Margins Over Time",
    subtitle = "Mamdani lead vs Cuomo and Adams",
    x = "Poll Date (Median)",
    y = "Margin (percentage points)",
    color = "Margin Type"
  )

# ==============================================================================
# Plot 3: Pollster Small Multiples
# ==============================================================================

cat("Generating Plot 3: Pollster small multiples...\n")

p3 <- cleaned_long %>%
  filter(pollster_clean %in% top_pollsters$pollster_clean[1:8]) %>%
  ggplot(aes(x = date_median, y = pct, color = candidate)) +
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.7, size = 1.5) +
  scale_color_manual(values = cs_palette) +
  labs(
    title = "Candidate Support by Top 8 Pollsters",
    subtitle = "Time series for each pollster",
    x = "Poll Date",
    y = "Support (%)",
    color = "Candidate"
  ) +
  facet_wrap(~pollster_clean, ncol = 4, scales = "free")

# ==============================================================================
# Plot 4: Candidate Density
# ==============================================================================

cat("Generating Plot 4: Candidate density...\n")

p4 <- cleaned_long %>%
  ggplot(aes(x = pct, fill = candidate)) +
  geom_density(alpha = 0.6, color = NA) +
  scale_fill_manual(values = cs_palette) +
  labs(
    title = "Distribution of Candidate Support",
    subtitle = "Density plots across all scenarios",
    x = "Support (%)",
    y = "Density",
    fill = "Candidate"
  ) +
  facet_wrap(~candidate, ncol = 3, scales = "free")

# ==============================================================================
# Plot 5: Boxplots by Vote Status
# ==============================================================================

cat("Generating Plot 5: Boxplots by vote status...\n")

p5 <- cleaned_long %>%
  filter(!is.na(vote_status)) %>%
  ggplot(aes(x = vote_status, y = pct, fill = candidate)) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.5) +
  scale_fill_manual(values = cs_palette) +
  labs(
    title = "Candidate Support by Vote Status",
    subtitle = "LV = Likely Voters, RV = Registered Voters, A = Adults",
    x = "Vote Status",
    y = "Support (%)",
    fill = "Candidate"
  ) +
  facet_wrap(~candidate, ncol = 3, scales = "free_y")

# ==============================================================================
# Plot 6: Pre/Post Adams Withdrawal
# ==============================================================================

cat("Generating Plot 6: Pre/Post Adams withdrawal...\n")

p6 <- cleaned_long %>%
  filter(!is.na(pre_post_adams_withdrawal)) %>%
  mutate(period = if_else(pre_post_adams_withdrawal, "Post-Withdrawal (≥2025-09-28)", "Pre-Withdrawal (<2025-09-28)")) %>%
  ggplot(aes(x = pct, y = period, fill = candidate)) +
  geom_density_ridges(alpha = 0.6, scale = 1.5) +
  scale_fill_manual(values = cs_palette) +
  labs(
    title = "Candidate Support: Pre vs Post Adams Withdrawal",
    subtitle = "Ridgeline plots showing distribution shift",
    x = "Support (%)",
    y = "Period",
    fill = "Candidate"
  ) +
  facet_wrap(~candidate, ncol = 2)

# ==============================================================================
# Plot 7: Mamdani vs Cuomo Scatter
# ==============================================================================

cat("Generating Plot 7: Mamdani vs Cuomo scatter...\n")

p7 <- cleaned %>%
  filter(!is.na(mamdani_pct) & !is.na(cuomo_pct) & !is.na(vote_status)) %>%
  ggplot(aes(x = cuomo_pct, y = mamdani_pct, color = vote_status, size = sample_size)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, alpha = 0.3) +
  scale_color_manual(values = c("LV" = "#73FF6B", "RV" = "#D200FF", "A" = "#00E5FF")) +
  scale_size_continuous(range = c(2, 10)) +
  labs(
    title = "Mamdani vs Cuomo Support",
    subtitle = "Scatter with linear smoothing | Size = sample size",
    x = "Cuomo Support (%)",
    y = "Mamdani Support (%)",
    color = "Vote Status",
    size = "Sample Size"
  )

# ==============================================================================
# Plot 8: Pairwise Margins
# ==============================================================================

cat("Generating Plot 8: Pairwise margins...\n")

p8 <- cleaned %>%
  filter(!is.na(mamdani_pct) & !is.na(cuomo_pct) & !is.na(adams_pct)) %>%
  mutate(
    mamdani_cuomo = mamdani_pct - cuomo_pct,
    mamdani_adams = mamdani_pct - adams_pct
  ) %>%
  ggplot(aes(x = mamdani_cuomo, y = mamdani_adams)) +
  geom_hex(bins = 20) +
  geom_abline(slope = 1, intercept = 0, color = "#73FF6B", linetype = "dashed", linewidth = 0.8) +
  scale_fill_gradient(low = "#1a1a1a", high = "#73FF6B") +
  labs(
    title = "Pairwise Margins: Mamdani Lead",
    subtitle = "Mamdani-Cuomo vs Mamdani-Adams margins | 45\u00b0 reference line",
    x = "Mamdani - Cuomo Margin",
    y = "Mamdani - Adams Margin",
    fill = "Count"
  )

# ==============================================================================
# Plot 9: Percent Sums Histogram
# ==============================================================================

cat("Generating Plot 9: Percent sums histogram...\n")

p9 <- cleaned %>%
  ggplot(aes(x = row_sum)) +
  geom_histogram(bins = 30, fill = "#73FF6B", alpha = 0.6, color = "#73FF6B") +
  geom_vline(xintercept = 100, color = "#D200FF", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = c(96, 104), color = "#FF6B00", linetype = "dotted", linewidth = 0.8) +
  labs(
    title = "Percent Sum Distribution (All Rows)",
    subtitle = glue("Rows < 96%: {sum(cleaned$row_sum < 96, na.rm = TRUE)} | Rows > 104%: {sum(cleaned$row_sum > 104, na.rm = TRUE)} | QC band: [96, 104]"),
    x = "Row Sum (%)",
    y = "Count"
  ) +
  facet_wrap(~scenario_type, scales = "free_y")

# ==============================================================================
# Plot 10: Missingness Heatmap
# ==============================================================================

cat("Generating Plot 10: Missingness...\n")

p10 <- if (nrow(missingness) > 0) {
  missingness %>%
    mutate(column = fct_reorder(column, pct_missing)) %>%
    ggplot(aes(x = pct_missing, y = column)) +
    geom_col(fill = "#73FF6B", alpha = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", pct_missing)),
              hjust = -0.1, color = "#73FF6B", size = 3) +
    labs(
      title = "Missingness by Column",
      subtitle = "Percentage of NA values per column",
      x = "% Missing",
      y = "Column"
    ) +
    xlim(0, max(missingness$pct_missing) * 1.2)
} else {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "No missingness detected",
             color = "#73FF6B", size = 8) +
    theme_void() +
    theme(plot.background = element_rect(fill = "black"))
}

# ==============================================================================
# Plot 11: Scenario Deltas (Alternates vs Primary)
# ==============================================================================

cat("Generating Plot 11: Scenario deltas...\n")

scenario_comparison <- cleaned %>%
  filter(pollster_wave_id %in% waves_with_alternates$pollster_wave_id) %>%
  mutate(is_primary = as.logical(is_primary)) %>%
  select(pollster_wave_id, scenario_type, is_primary,
         mamdani_pct, cuomo_pct, adams_pct, sliwa_pct) %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "candidate",
    values_to = "pct"
  ) %>%
  mutate(candidate = str_remove(candidate, "_pct")) %>%
  filter(!is.na(pct))

p11 <- if (nrow(scenario_comparison) > 0) {
  scenario_comparison %>%
    group_by(pollster_wave_id, candidate) %>%
    mutate(
      primary_pct = pct[is_primary],
      delta = pct - primary_pct
    ) %>%
    ungroup() %>%
    filter(!is_primary) %>%
    ggplot(aes(x = delta, y = scenario_type, color = candidate)) +
    geom_vline(xintercept = 0, color = "#73FF6B", linetype = "dashed") +
    geom_point(alpha = 0.6, size = 3, position = position_jitter(height = 0.2)) +
    scale_color_manual(values = cs_palette) +
    labs(
      title = "Scenario Variant Deltas vs Primary",
      subtitle = "Difference from primary selection (full_field with max candidates)",
      x = "Delta from Primary (%)",
      y = "Scenario Type",
      color = "Candidate"
    )
} else {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "No scenario variants to compare",
             color = "#73FF6B", size = 8) +
    theme_void() +
    theme(plot.background = element_rect(fill = "black"))
}

# ==============================================================================
# Plot 12: Primary vs All (if PRIMARY data available)
# ==============================================================================

cat("Generating Plot 12: Primary vs All comparison...\n")

p12 <- if (!is.null(primary)) {
  primary_long <- primary %>%
    select(pollster_wave_id, date_median,
           mamdani_pct, cuomo_pct, adams_pct, sliwa_pct) %>%
    pivot_longer(
      cols = ends_with("_pct"),
      names_to = "candidate",
      values_to = "pct"
    ) %>%
    mutate(
      candidate = str_remove(candidate, "_pct"),
      source = "Primary"
    )

  cleaned_comp <- cleaned %>%
    select(pollster_wave_id, date_median,
           mamdani_pct, cuomo_pct, adams_pct, sliwa_pct) %>%
    pivot_longer(
      cols = ends_with("_pct"),
      names_to = "candidate",
      values_to = "pct"
    ) %>%
    mutate(
      candidate = str_remove(candidate, "_pct"),
      source = "All (Cleaned)"
    )

  combined <- bind_rows(cleaned_comp, primary_long) %>%
    filter(!is.na(pct))

  combined %>%
    ggplot(aes(x = pct, fill = source)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Primary" = "#39FF14", "All (Cleaned)" = "#D200FF")) +
    labs(
      title = "Primary Selection vs All Scenarios",
      subtitle = "Overlaid density plots comparing distributions",
      x = "Support (%)",
      y = "Density",
      fill = "Data Source"
    ) +
    facet_wrap(~candidate, ncol = 3, scales = "free")
} else {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Primary data not provided",
             color = "#39FF14", size = 8) +
    theme_void() +
    theme(plot.background = element_rect(fill = "black"))
}

# ==============================================================================
# Generate Irregularities and Modeling Implications
# ==============================================================================

irregularities <- c()
if (n_low_sum > 0) {
  irregularities <- c(irregularities, glue("- {n_low_sum} rows ({pct_low_sum}%) have percent sums < 97%"))
}
if (n_high_sum > 0) {
  irregularities <- c(irregularities, glue("- {n_high_sum} rows ({pct_high_sum}%) have percent sums > 103%"))
}
if (nrow(missingness) > 0) {
  top_missing <- missingness %>% head(3)
  for (i in 1:nrow(top_missing)) {
    irregularities <- c(irregularities,
                       glue("- Column `{top_missing$column[i]}`: {sprintf('%.1f', top_missing$pct_missing[i])}% missing"))
  }
}
if (length(irregularities) == 0) {
  irregularities <- c("- No major irregularities detected")
}

modeling_implications <- c(
  glue("- **Scenario variants:** {n_waves_with_alternates} waves have multiple scenarios; consider modeling strategy (separate models vs fixed effects)"),
  glue("- **Vote status:** LV ({sum(vote_status_counts$n[vote_status_counts$vote_status == 'LV'], na.rm=TRUE)}) vs RV ({sum(vote_status_counts$n[vote_status_counts$vote_status == 'RV'], na.rm=TRUE)}) vs A ({sum(vote_status_counts$n[vote_status_counts$vote_status == 'A'], na.rm=TRUE)}) - consider including as random effect or filtering to LV only"),
  "- **Percent sums:** Rows with sums outside [97, 103] may need investigation or exclusion",
  "- **Pre/post Adams:** Only 2 polls post-withdrawal; limited data for event analysis",
  "- **Primary selection:** full_field scenarios prioritized; alternates preserved for sensitivity analysis"
)

irregularities_bullets <- paste(irregularities, collapse = "\n")
modeling_implications_bullets <- paste(modeling_implications, collapse = "\n")

# ==============================================================================
# Write Markdown Report
# ==============================================================================

md_content <- glue("
# EDA Report - NYC Mayoral Polling Data

**Timestamp:** {STAMP}
**Input CLEANED:** `{basename(input_cleaned)}`
{if (!is.null(input_primary)) glue('**Input PRIMARY (comparison):** `{basename(input_primary)}`') else ''}

## Summary Statistics

### Data Overview
- **Total rows (CLEANED):** {n_rows_cleaned}
- **Unique waves:** {n_waves}
- **Date range:** {min_date} to {max_date}
- **Days span:** {days_span}

### Scenario Type Distribution (CLEANED)
```
{paste(capture.output(print(scenario_type_counts)), collapse = '\n')}
```

### Vote Status Distribution (CLEANED)
```
{paste(capture.output(print(vote_status_counts)), collapse = '\n')}
```

### Top Pollsters (by row count)
```
{paste(capture.output(print(top_pollsters)), collapse = '\n')}
```

### Waves with Scenario Variants
- **Waves with alternates:** {n_waves_with_alternates}
- **Waves with 1 scenario only:** {n_waves_single}

### Missingness Rates (CLEANED)
{if (nrow(missingness) > 0) {
  glue('```\n{paste(capture.output(print(missingness)), collapse = \"\\n\")}\n```')
} else {
  'No missing values detected.'
}}

### Percent Sum Diagnostics (CLEANED)
- **Mean row sum:** {sprintf('%.2f', pct_sum_mean)}
- **SD:** {sprintf('%.2f', pct_sum_sd)}
- **Min:** {sprintf('%.2f', pct_sum_min)}
- **Max:** {sprintf('%.2f', pct_sum_max)}
- **Rows < 97%:** {n_low_sum} ({pct_low_sum}%)
- **Rows > 103%:** {n_high_sum} ({pct_high_sum}%)

### Pre/Post Adams Withdrawal (2025-09-28)
```
{paste(capture.output(print(pre_post_counts)), collapse = '\n')}
```

## Plots

### Time Series (CLEANED)
![Candidate % over time](../plots/EDA_P1_candidate_pct_time_{STAMP}.png)
![Margins over time](../plots/EDA_P2_margins_time_{STAMP}.png)
![Pollster multiples](../plots/EDA_P3_pollster_multiples_{STAMP}.png)

### Distributions (CLEANED)
![Candidate density](../plots/EDA_P4_candidate_density_{STAMP}.png)
![By vote status](../plots/EDA_P5_candidate_by_vote_status_{STAMP}.png)
![Pre/post Adams](../plots/EDA_P6_pre_post_adams_{STAMP}.png)

### Bivariate (CLEANED)
![Mamdani vs Cuomo](../plots/EDA_P7_mamdani_vs_cuomo_scatter_{STAMP}.png)
![Pairwise margins](../plots/EDA_P8_pairwise_margins_{STAMP}.png)

### Sanity Checks (CLEANED)
![Percent sums](../plots/EDA_P9_percent_sums_histogram_{STAMP}.png)
![Missingness](../plots/EDA_P10_missingness_{STAMP}.png)
![Scenario deltas](../plots/EDA_P11_scenario_deltas_{STAMP}.png)
{if (!is.null(input_primary)) glue('![Primary vs All](../plots/EDA_P12_primary_vs_all_{STAMP}.png)') else ''}

## Irregularities Noted

{irregularities_bullets}

## Implications for Modeling

{modeling_implications_bullets}

---

Generated by `R/03_eda_plots.R`
")

# ==============================================================================
# Save Plots and Report (guarded by dryrun)
# ==============================================================================

if (dryrun) {
  cat("\n=== DRY RUN MODE ===\n")
  cat("Would generate 12 plots in:", out_dir, "\n")
  cat("Would write report:", report_file, "\n")
  cat("\nSummary:\n")
  cat("  Cleaned rows:", n_rows_cleaned, "\n")
  cat("  Unique waves:", n_waves, "\n")
  cat("  Scenario types:", paste(scenario_type_counts$scenario_type, collapse = ", "), "\n")
  cat("  Vote status:", paste(vote_status_counts$vote_status, collapse = ", "), "\n")
} else {
  cat("\n=== Writing outputs ===\n")

  # Create output directories
  dir.create(here(out_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("analysis"), recursive = TRUE, showWarnings = FALSE)

  # Save plots (DPI=300, high quality)
  plots <- list(
    p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6 = p6,
    p7 = p7, p8 = p8, p9 = p9, p10 = p10, p11 = p11, p12 = p12
  )

  plot_names <- c(
    "candidate_pct_time", "margins_time", "pollster_multiples",
    "candidate_density", "candidate_by_vote_status", "pre_post_adams",
    "mamdani_vs_cuomo_scatter", "pairwise_margins",
    "percent_sums_histogram", "missingness", "scenario_deltas", "primary_vs_all"
  )

  for (i in 1:12) {
    filename <- here(out_dir, glue("EDA_P{i}_{plot_names[i]}_{STAMP}.png"))
    ggsave(
      filename = filename,
      plot = plots[[i]],
      width = 12,
      height = 7,
      dpi = 300,
      bg = "black"
    )
    cat("✓ Saved:", basename(filename), "\n")
  }

  # Write report
  writeLines(md_content, here(report_file))
  cat("✓ Report:", report_file, "\n")

  cat("\n✓ EDA complete!\n")
  cat("  Plots:", length(plots), "PNG files\n")
  cat("  Report:", report_file, "\n")
}
