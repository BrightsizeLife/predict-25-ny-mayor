# ==============================================================================
# 00_setup.R
# Package installation and loading for NYC Mayoral Polling Analysis
# ==============================================================================

# Required packages
packages <- c(
  # Data wrangling & scraping
  "tidyverse",      # dplyr, tidyr, ggplot2, readr, purrr, stringr
  "rvest",          # HTML scraping
  "xml2",           # XML parsing support
  "lubridate",      # Date parsing
  "janitor",        # clean_names(), etc.

  # Modeling
  "brms",           # Bayesian multinomial models
  "cmdstanr",       # Stan backend (preferred)
  # "rstan",        # Fallback if cmdstanr unavailable
  "splines",        # bs() basis functions
  "loo",            # LOO-CV model comparison

  # Utilities
  "broom",          # Tidy model outputs
  "broom.mixed",    # Tidy mixed models
  "modelr",         # Data grids for predictions
  "here",           # Path management
  "glue",           # String interpolation

  # Plotting
  "patchwork",      # Multi-panel plots
  "scales"          # Axis formatting
)

# Check which packages are not installed
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

# Install missing packages
if (length(new_packages) > 0) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}

# Load packages
message("\nLoading packages...")
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))

# Check for cmdstanr installation (requires special setup)
if (!require("cmdstanr", quietly = TRUE)) {
  message("\nNote: cmdstanr not found. To install:")
  message("  install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))")
  message("  cmdstanr::install_cmdstan()")
  message("\nFalling back to rstan if available.")

  if (!require("rstan", quietly = TRUE)) {
    warning("Neither cmdstanr nor rstan are available. Install one to fit models.")
  }
}

# Set options
options(
  mc.cores = parallel::detectCores() - 1,  # Use all but one core
  brms.backend = "cmdstanr",               # Prefer cmdstanr
  scipen = 999,                            # Disable scientific notation
  digits = 4                               # Display precision
)

# Print session info for reproducibility
message("\n=== Session Info ===")
message("R version: ", R.version.string)
message("Platform: ", R.version$platform)
message("Date: ", Sys.Date())
message("\nPackage versions:")
print(sapply(packages[packages %in% loadedNamespaces()],
             function(pkg) as.character(packageVersion(pkg))))

message("\n✓ Setup complete. Ready to run analysis scripts.")
