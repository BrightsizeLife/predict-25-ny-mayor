#!/usr/bin/env Rscript
# ==============================================================================
# 04_fit_models.R
# Fit 4 baseline multinomial models (M0-M3) with brms
# Reproducible seeds, parallel chains, full diagnostics, LOO comparison
# ==============================================================================

# Parse CLI arguments
args <- commandArgs(trailingOnly = TRUE)
input_file <- NULL
out_dir <- "artifacts/models"
report_file <- NULL
chains <- 4
iter <- 4000
warmup <- 2000
seed <- 1234
smoke <- FALSE
cores <- NULL
reloo <- FALSE

if (length(args) > 0) {
  for (arg in args) {
    if (grepl("^--input=", arg)) {
      input_file <- sub("^--input=", "", arg)
    } else if (grepl("^--out_dir=", arg)) {
      out_dir <- sub("^--out_dir=", "", arg)
    } else if (grepl("^--report=", arg)) {
      report_file <- sub("^--report=", "", arg)
    } else if (grepl("^--chains=", arg)) {
      chains <- as.integer(sub("^--chains=", "", arg))
    } else if (grepl("^--iter=", arg)) {
      iter <- as.integer(sub("^--iter=", "", arg))
    } else if (grepl("^--warmup=", arg)) {
      warmup <- as.integer(sub("^--warmup=", "", arg))
    } else if (grepl("^--seed=", arg)) {
      seed <- as.integer(sub("^--seed=", "", arg))
    } else if (grepl("^--smoke=", arg)) {
      smoke <- as.logical(sub("^--smoke=", "", arg))
    } else if (grepl("^--cores=", arg)) {
      cores <- as.integer(sub("^--cores=", "", arg))
    } else if (grepl("^--reloo=", arg)) {
      reloo <- as.logical(sub("^--reloo=", "", arg))
    }
  }
}

# Load dependencies
suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(here)
  library(glue)
  library(parallel)
})

STAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Set defaults
if (is.null(input_file)) {
  primary_files <- list.files(here("data", "processed"),
                               pattern = "^polls_primary_.*\\.csv$",
                               full.names = TRUE)
  if (length(primary_files) == 0) {
    stop("ERROR: No primary CSV files found in data/processed/")
  }
  input_file <- primary_files[order(file.info(primary_files)$mtime, decreasing = TRUE)][1]
}

if (is.null(report_file)) report_file <- glue("analysis/04_model_diagnostics_{STAMP}.md")

if (is.null(cores)) {
  cores <- max(1, detectCores() - 2)
}

# Smoke test overrides
if (smoke) {
  chains <- 2
  iter <- 1000
  warmup <- 500
  cat("=== SMOKE TEST MODE ===\n")
}

cat("=== NYC Mayoral Polling - Multinomial Models ===\n")
cat("Timestamp:", STAMP, "\n")
cat("Input:", basename(input_file), "\n")
cat("Seed:", seed, "\n")
cat("Chains:", chains, "| Iter:", iter, "| Warmup:", warmup, "\n")
cat("Cores:", cores, "\n")
cat("Smoke:", smoke, "\n\n")

# Set seeds
set.seed(seed)

# ==============================================================================
# Load and Validate Data
# ==============================================================================

cat("Loading PRIMARY data...\n")
primary <- read_csv(input_file, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  mutate(
    date_median = as.Date(date_median),
    across(ends_with("_pct"), as.numeric),
    across(ends_with("_n"), as.integer),
    sample_size = as.integer(sample_size)
  )

cat("PRIMARY rows:", nrow(primary), "\n")

# Validate FULL-FIELD
cat("\nValidating FULL-FIELD definition (top-4 + other/undecided)...\n")
validation <- primary %>%
  mutate(
    has_top4 = !is.na(mamdani_pct) & !is.na(cuomo_pct) & !is.na(adams_pct) & !is.na(sliwa_pct),
    has_other_undecided = !is.na(other_pct) | !is.na(undecided_pct)
  ) %>%
  summarise(
    n_full_field = sum(has_top4 & has_other_undecided),
    n_missing_top4 = sum(!has_top4),
    n_missing_other_undecided = sum(has_top4 & !has_other_undecided)
  )

cat("Full-field compliant:", validation$n_full_field, "/", nrow(primary), "\n")
if (validation$n_missing_top4 > 0) {
  stop("ERROR: ", validation$n_missing_top4, " rows missing top-4 candidates")
}
if (validation$n_missing_other_undecided > 0) {
  stop("ERROR: ", validation$n_missing_other_undecided, " rows missing other/undecided")
}
cat("✓ All PRIMARY rows are strict FULL-FIELD\n\n")

# Build counts (ensure correct ordering for refcat)
cat("Building count columns...\n")
primary <- primary %>%
  mutate(
    # Recalculate if needed
    num_mamdani = if_else(is.na(mamdani_n), as.integer(round(sample_size * mamdani_pct / 100)), mamdani_n),
    num_cuomo = if_else(is.na(cuomo_n), as.integer(round(sample_size * cuomo_pct / 100)), cuomo_n),
    num_adams = if_else(is.na(adams_n), as.integer(round(sample_size * adams_pct / 100)), adams_n),
    num_sliwa = if_else(is.na(sliwa_n), as.integer(round(sample_size * sliwa_pct / 100)), sliwa_n),
    num_other = if_else(is.na(other_n), as.integer(round(sample_size * other_pct / 100)), other_n),
    num_undecided = if_else(is.na(undecided_n), as.integer(round(sample_size * undecided_pct / 100)), undecided_n)
  )

# Replace NA counts with 0
primary <- primary %>%
  mutate(across(starts_with("num_"), ~coalesce(., 0L)))

# Verify trials = sum of events, adjust if needed due to rounding
primary <- primary %>%
  mutate(
    count_sum = num_mamdani + num_cuomo + num_adams + num_sliwa + num_other + num_undecided,
    diff = sample_size - count_sum
  )

# Adjust largest category to absorb rounding error
if (any(primary$diff != 0)) {
  cat("Adjusting", sum(primary$diff != 0), "rows for rounding errors\n")
  primary <- primary %>%
    rowwise() %>%
    mutate(
      # Find which category has the largest count to adjust
      max_cat = which.max(c(num_mamdani, num_cuomo, num_adams, num_sliwa, num_other, num_undecided)),
      num_mamdani = if_else(max_cat == 1, num_mamdani + diff, num_mamdani),
      num_cuomo = if_else(max_cat == 2, num_cuomo + diff, num_cuomo),
      num_adams = if_else(max_cat == 3, num_adams + diff, num_adams),
      num_sliwa = if_else(max_cat == 4, num_sliwa + diff, num_sliwa),
      num_other = if_else(max_cat == 5, num_other + diff, num_other),
      num_undecided = if_else(max_cat == 6, num_undecided + diff, num_undecided)
    ) %>%
    ungroup() %>%
    select(-count_sum, -diff, -max_cat)
} else {
  primary <- primary %>% select(-count_sum, -diff)
}

cat("✓ Counts built\n\n")

# Data summary
min_date <- min(primary$date_median, na.rm = TRUE)
max_date <- max(primary$date_median, na.rm = TRUE)
n_waves <- n_distinct(primary$pollster_wave_id)

cat("Data summary:\n")
cat("  Waves:", n_waves, "\n")
cat("  Date range:", as.character(min_date), "to", as.character(max_date), "\n")
cat("  Pollsters:", n_distinct(primary$pollster), "\n")
cat("  Vote status:", paste(unique(primary$vote_status), collapse = ", "), "\n\n")

# ==============================================================================
# Define Models
# ==============================================================================

cat("Defining models...\n")

# Reference category: num_mamdani (first in cbind order)
# Family: multinomial with refcat specified
family_spec <- multinomial(refcat = "num_mamdani")

# Formulas
formula_M0 <- bf(
  cbind(num_mamdani, num_cuomo, num_adams, num_sliwa, num_other, num_undecided) | trials(sample_size) ~ 1,
  family = family_spec
)

formula_M1 <- bf(
  cbind(num_mamdani, num_cuomo, num_adams, num_sliwa, num_other, num_undecided) | trials(sample_size) ~ 1 + (1 | pollster),
  family = family_spec
)

formula_M2 <- bf(
  cbind(num_mamdani, num_cuomo, num_adams, num_sliwa, num_other, num_undecided) | trials(sample_size) ~ 1 + (1 | pollster) + (1 | vote_status),
  family = family_spec
)

formula_M3 <- bf(
  cbind(num_mamdani, num_cuomo, num_adams, num_sliwa, num_other, num_undecided) | trials(sample_size) ~ 1 + (1 | pollster) + (1 | vote_status) + (1 | pollster_wave_id),
  family = family_spec
)

models <- list(
  m00_intercept = formula_M0,
  m01_ri_pollster = formula_M1,
  m02_ri_pollster_vstatus = formula_M2,
  m03_ri_pollster_vstatus_wave = formula_M3
)

cat("✓ Models defined (m00-m03)\n\n")

# ==============================================================================
# Fit Models
# ==============================================================================

cat("Fitting models...\n\n")

# Backend: cmdstanr if available
backend <- "rstan"  # Force rstan due to macOS SDK mismatch with cmdstanr
cmdstan_version <- NULL
cat("Using rstan backend (cmdstanr disabled due to macOS SDK compilation issues)\n")

# Control settings
control_settings <- list(
  adapt_delta = 0.99,
  max_treedepth = 12
)

# Storage
fits <- list()
diagnostics <- list()
warnings_list <- list()

for (model_name in names(models)) {
  cat(glue("=== Fitting {model_name} ===\n"))

  # Capture warnings/messages
  warnings_captured <- character(0)

  fit <- tryCatch({
    withCallingHandlers({
      brm(
        formula = models[[model_name]],
        data = primary,
        chains = chains,
        iter = iter,
        warmup = warmup,
        cores = cores,
        seed = seed,
        backend = backend,
        control = control_settings,
        refresh = 500  # Show progress every 500 iterations
      )
    }, warning = function(w) {
      warnings_captured <<- c(warnings_captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }, error = function(e) {
    cat(glue("ERROR fitting {model_name}: {conditionMessage(e)}\n\n"))
    return(NULL)
  })

  if (is.null(fit)) {
    cat(glue("✗ {model_name} FAILED\n\n"))
    next
  }

  # Save fit
  fit_path <- here(out_dir, glue("{STAMP}_{model_name}.rds"))
  dir.create(here(out_dir), recursive = TRUE, showWarnings = FALSE)
  saveRDS(fit, fit_path)
  cat(glue("✓ Saved: {fit_path}\n"))

  fits[[model_name]] <- fit
  warnings_list[[model_name]] <- warnings_captured

  # Diagnostics
  summ <- summary(fit)
  rhat_max <- max(summ$fixed[, "Rhat"], na.rm = TRUE)
  ess_bulk_min <- min(summ$fixed[, "Bulk_ESS"], na.rm = TRUE)
  ess_tail_min <- min(summ$fixed[, "Tail_ESS"], na.rm = TRUE)

  # Divergences
  np <- nuts_params(fit)
  n_divergent <- sum(np$divergent__)

  diagnostics[[model_name]] <- list(
    rhat_max = rhat_max,
    ess_bulk_min = ess_bulk_min,
    ess_tail_min = ess_tail_min,
    n_divergent = n_divergent,
    warnings = warnings_captured
  )

  cat(glue("  Rhat (max): {round(rhat_max, 3)}\n"))
  cat(glue("  Bulk ESS (min): {round(ess_bulk_min, 0)}\n"))
  cat(glue("  Tail ESS (min): {round(ess_tail_min, 0)}\n"))
  cat(glue("  Divergences: {n_divergent}\n\n"))

  # Random intercepts plots (if applicable)
  if (model_name != "m00_intercept") {
    cat(glue("  Generating random intercepts plot for {model_name}...\n"))

    ri_plot_path <- here("plots", glue("{STAMP}_ri_{model_name}.png"))
    dir.create(here("plots"), recursive = TRUE, showWarnings = FALSE)

    tryCatch({
      p <- mcmc_plot(fit, type = "intervals", regex_pars = "^r_")
      ggsave(ri_plot_path, plot = p, width = 10, height = 8, dpi = 300)
      cat(glue("  ✓ Saved RI plot: {ri_plot_path}\n"))
    }, error = function(e) {
      cat(glue("  Warning: Could not generate RI plot: {conditionMessage(e)}\n"))
    })
  }

  cat("\n")
}

# Check if any fits succeeded
if (length(fits) == 0) {
  stop("ERROR: All model fits failed")
}

cat(glue("✓ {length(fits)}/{length(models)} models fitted successfully\n\n"))

# ==============================================================================
# LOO Comparison
# ==============================================================================

cat("=== LOO Comparison ===\n\n")

loo_objects <- list()
loo_warnings <- list()

for (model_name in names(fits)) {
  cat(glue("Computing LOO for {model_name}...\n"))

  loo_warnings_captured <- character(0)

  loo_obj <- tryCatch({
    withCallingHandlers({
      loo(fits[[model_name]], reloo = reloo)
    }, warning = function(w) {
      loo_warnings_captured <<- c(loo_warnings_captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }, error = function(e) {
    cat(glue("ERROR computing LOO for {model_name}: {conditionMessage(e)}\n\n"))
    return(NULL)
  })

  if (!is.null(loo_obj)) {
    loo_objects[[model_name]] <- loo_obj
    loo_warnings[[model_name]] <- loo_warnings_captured

    # Check for problematic observations
    if (!is.null(loo_obj$diagnostics)) {
      n_bad <- sum(loo_obj$diagnostics$pareto_k > 0.7, na.rm = TRUE)
      if (n_bad > 0) {
        cat(glue("  Warning: {n_bad} observations with Pareto k > 0.7\n"))
      }
    }

    cat(glue("  ✓ LOO computed\n\n"))
  }
}

# LOO comparison table
if (length(loo_objects) > 1) {
  cat("LOO Comparison Table:\n")
  loo_comp <- loo_compare(loo_objects)
  print(loo_comp)
  cat("\n")
} else {
  cat("Only one model succeeded; no comparison possible\n\n")
  loo_comp <- NULL
}

# ==============================================================================
# Write Report
# ==============================================================================

cat("Writing diagnostics report...\n")

# Capture session info
si <- sessionInfo()
r_version <- paste(si$R.version$major, si$R.version$minor, sep = ".")
brms_version <- as.character(packageVersion("brms"))

md_lines <- c(
  "# Multinomial Model Diagnostics",
  "",
  glue("**Timestamp:** {STAMP}"),
  glue("**Input:** `{basename(input_file)}`"),
  "",
  "## Computational Environment",
  "",
  glue("- **R version:** {r_version}"),
  glue("- **brms version:** {brms_version}"),
  glue("- **Backend:** {backend}"),
  if (!is.null(cmdstan_version)) glue("- **cmdstan version:** {cmdstan_version}") else NULL,
  glue("- **Cores:** {cores}"),
  glue("- **Chains:** {chains}"),
  glue("- **Iterations:** {iter} (warmup: {warmup})"),
  glue("- **Seed:** {seed}"),
  glue("- **Smoke mode:** {smoke}"),
  "",
  "## Data Summary",
  "",
  glue("- **Waves:** {n_waves}"),
  glue("- **Date range:** {min_date} to {max_date}"),
  glue("- **Pollsters:** {n_distinct(primary$pollster)}"),
  glue("- **Vote status:** {paste(unique(primary$vote_status), collapse = ', ')}"),
  "",
  "## Models Fitted",
  "",
  "- **m00_intercept:** `~ 1` (intercept only)",
  "- **m01_ri_pollster:** `~ 1 + (1 | pollster)`",
  "- **m02_ri_pollster_vstatus:** `~ 1 + (1 | pollster) + (1 | vote_status)`",
  "- **m03_ri_pollster_vstatus_wave:** `~ 1 + (1 | pollster) + (1 | vote_status) + (1 | pollster_wave_id)`",
  "",
  "**Family:** `multinomial(refcat = 1)` (mamdani as reference)",
  "",
  "**Outcome:** `cbind(num_mamdani, num_cuomo, num_adams, num_sliwa, num_other, num_undecided) | trials(sample_size)`",
  "",
  "## Per-Model Diagnostics",
  ""
)

for (model_name in names(diagnostics)) {
  diag <- diagnostics[[model_name]]

  md_lines <- c(md_lines,
    glue("### {model_name}"),
    "",
    glue("- **Rhat (max):** {round(diag$rhat_max, 3)}"),
    glue("- **Bulk ESS (min):** {round(diag$ess_bulk_min, 0)}"),
    glue("- **Tail ESS (min):** {round(diag$ess_tail_min, 0)}"),
    glue("- **Divergences:** {diag$n_divergent}"),
    ""
  )

  if (length(diag$warnings) > 0) {
    md_lines <- c(md_lines,
      "**Warnings:**",
      "",
      "```",
      diag$warnings,
      "```",
      ""
    )
  }
}

# LOO comparison
if (!is.null(loo_comp)) {
  md_lines <- c(md_lines,
    "## LOO Comparison",
    "",
    "```",
    capture.output(print(loo_comp)),
    "```",
    ""
  )

  # LOO warnings
  if (length(loo_warnings) > 0) {
    md_lines <- c(md_lines,
      "### LOO Warnings",
      ""
    )
    for (model_name in names(loo_warnings)) {
      if (length(loo_warnings[[model_name]]) > 0) {
        md_lines <- c(md_lines,
          glue("**{model_name}:**"),
          "",
          "```",
          loo_warnings[[model_name]],
          "```",
          ""
        )
      }
    }
  }
}

# Artifact paths
md_lines <- c(md_lines,
  "## Saved Artifacts",
  "",
  "**Model fits:**",
  ""
)

for (model_name in names(fits)) {
  fit_path <- glue("{out_dir}/{STAMP}_{model_name}.rds")
  md_lines <- c(md_lines, glue("- `{fit_path}`"))
}

md_lines <- c(md_lines,
  "",
  "**Random intercepts plots:**",
  ""
)

for (model_name in names(fits)) {
  if (model_name != "m00_intercept") {
    ri_path <- glue("plots/{STAMP}_ri_{model_name}.png")
    md_lines <- c(md_lines, glue("- `{ri_path}`"))
  }
}

md_lines <- c(md_lines,
  "",
  "---",
  "",
  "Generated by `R/04_fit_models.R`"
)

dir.create(here("analysis"), recursive = TRUE, showWarnings = FALSE)
writeLines(md_lines, here(report_file))

cat(glue("✓ Report: {report_file}\n\n"))

cat("✓ Model fitting complete!\n")
