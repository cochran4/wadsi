# Wisconsin Autism Data Science Initiative

This repository contains code and public-facing materials from the Wisconsin
Autism Data Science Initiative (WADSI) at the University of Wisconsin–Madison.
The project develops and applies data-science methods to better understand
autism and the factors that shape autistic people's lives.

## Study 1

The `study_1/` directory contains the developing analysis workflow for the
first study. The workflow uses Bayesian additive regression trees (BART) to
estimate individual- and population-level measures of association and
attributable fractions. It also contains sensitivity analyses examining BART
implementation, MCMC configuration, predictive performance, and unmeasured
confounding.

> **Pilot status:** The currently rendered reports use public NHANES data with
> diabetes as the outcome to test the analysis workflow. They do not use SEED
> data and do not report results about autism. The language describing the
> scientific aims anticipates the future SEED analysis.

Rendered reports:

- [Study 1 analysis](https://cochran4.github.io/wadsi/reports/study_1.html)
- [Study 1 sensitivity analyses](https://cochran4.github.io/wadsi/reports/study_1_sensitivity.html)

The corresponding Quarto source files are
[`study_1/study_1.qmd`](study_1/study_1.qmd) and
[`study_1/study_1_sensitivity.qmd`](study_1/study_1_sensitivity.qmd).

## Repository structure

- `study_1/`: analysis documents, R functions, configuration, and rendered
  output for Study 1
- `docs/`: source for the WADSI website and the GitHub Pages copies of the
  rendered reports
- `old/`: archived project files

SEED study data and fitted models derived from those data are not included in
the repository. The current rendered reports contain results from the public
NHANES pilot analysis and can be viewed without downloading the repository or
running the analyses.

## Website

Visit the [WADSI website](https://cochran4.github.io/wadsi/) for project news
and background information.

## License

Code and other repository materials are available under the terms in
[`LICENSE`](LICENSE).
