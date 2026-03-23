# imports.R

# Source project functions
source("R/prepare_data.R")
source("R/compute_effects.R")
source("R/model_diagnostics.R")
source("R/plots.R")

# Toggle automatic installation of missing packages
INSTALL_MISSING <- TRUE

# Required packages
pkgs <- c(
  "tidyverse",
  "bartCause",
  "dbarts",
  "yaml",
  "mice",
  "coda",
  "knitr",
  "officer",
  "flextable",
  "kableExtra",
  "patchwork",
  "iml",
  "ggbeeswarm"
)

# Set a CRAN mirror if needed
if (isTRUE(INSTALL_MISSING)) {
  repos <- getOption("repos")
  if (is.null(repos) || identical(repos["CRAN"], "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
}

# Install missing packages
if (isTRUE(INSTALL_MISSING)) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, dependencies = TRUE)
  }
}

# Load packages quietly
suppressPackageStartupMessages({
  for (pkg in pkgs) {
    library(pkg, character.only = TRUE)
  }
})

invisible(TRUE)