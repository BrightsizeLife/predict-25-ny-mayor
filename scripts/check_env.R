#!/usr/bin/env Rscript
# ==============================================================================
# check_env.R
# Environment smoke test for NYC Mayoral Polling Analysis
# ==============================================================================

cat("=== Environment Smoke Test ===\n\n")

# Source setup script
cat("Sourcing R/00_setup.R...\n")
source("R/00_setup.R")

cat("\n=== Package Versions ===\n")
packages <- c(
  "tidyverse", "rvest", "xml2", "lubridate", "janitor",
  "brms", "cmdstanr", "splines", "loo",
  "broom", "broom.mixed", "modelr", "here", "glue",
  "patchwork", "scales"
)

loaded_packages <- packages[packages %in% loadedNamespaces()]
if (length(loaded_packages) > 0) {
  versions <- sapply(loaded_packages, function(pkg) as.character(packageVersion(pkg)))
  version_df <- data.frame(
    Package = names(versions),
    Version = unname(versions),
    stringsAsFactors = FALSE
  )
  print(version_df, row.names = FALSE)
} else {
  cat("No packages loaded from setup script.\n")
}

cat("\n=== Stan Backend Check ===\n")

# Check cmdstanr
cmdstan_available <- FALSE
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  tryCatch({
    cmdstan_ver <- cmdstanr::cmdstan_version()
    cat(" cmdstanr available\n")
    cat("  Version:", cmdstan_ver, "\n")
    cmdstan_available <- TRUE
  }, error = function(e) {
    cat("  cmdstanr package installed but CmdStan not found\n")
    cat("  Install with: cmdstanr::install_cmdstan()\n")
  })
} else {
  cat("  cmdstanr not installed\n")
  cat("  Install with: install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))\n")
}

# Check rstan as fallback
if (!cmdstan_available) {
  if (requireNamespace("rstan", quietly = TRUE)) {
    cat(" rstan available as fallback\n")
    cat("  Version:", as.character(packageVersion("rstan")), "\n")
  } else {
    cat("  rstan not available\n")
    cat("  Install with: install.packages('rstan')\n")
  }
}

cat("\n=== System Info ===\n")
cat("R version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("CPU cores available:", parallel::detectCores(), "\n")
cat("mc.cores option:", getOption("mc.cores", 1), "\n")
cat("brms.backend option:", getOption("brms.backend", "rstan"), "\n")

cat("\n=== Timestamp ===\n")
cat("Test run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")

cat("\n=== Summary ===\n")
if (length(loaded_packages) > 0 && (cmdstan_available || requireNamespace("rstan", quietly = TRUE))) {
  cat(" PASS: Environment is ready for analysis\n")
} else {
  cat("  WARN: Some components missing; review warnings above\n")
}

cat("\n")
