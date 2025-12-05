# imports.R

# Load the custom functions
source("R/prepare_data.R")
source("R/compute_effects.R")
source("R/model_diagnostics.R")
source("R/plots.R")

# Toggle: install missing packages automatically
INSTALL_MISSING <- TRUE

pkgs <- c(
  "dplyr",
  "ggplot2",
  "bartCause",
  "dbarts",
  "yaml",
  "tidyverse",
  "mice",
  "coda",
  "knitr",
  "officer",
  "flextable",
  "kableExtra",
  "patchwork",
  "forcats",
  "iml",
  "tibble",
  "purrr",
  "ggbeeswarm"
  )

# Pick a CRAN mirror if none set (avoids interactive prompt)
if (isTRUE(INSTALL_MISSING) && (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Install any missing packages (quietly)
if (INSTALL_MISSING) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    install.packages(missing_pkgs, dependencies = TRUE)
  }
}

# Load packages without startup chatter
suppressPackageStartupMessages({
  invisible(lapply(pkgs, library, character.only = TRUE))
})

invisible(TRUE)