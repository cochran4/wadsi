# Prepare one genuinely binary focal predictor for the proxy analysis.
#
# X is the focal predictor, Y is the outcome, and Z contains every remaining
# variable in the analytic dataset. Multi-level and continuous predictors are
# deliberately excluded from this first implementation.
prepare_binary_proxy_data <- function(analytic_df, outcome_var, x_var) {
  x <- analytic_df[[x_var]]

  if (is.factor(x)) {
    x_levels <- levels(droplevels(x))
  } else {
    x_levels <- sort(unique(x))
  }

  if (length(x_levels) != 2L) {
    stop("The proxy analysis currently requires a genuinely binary X.")
  }

  # Match the primary PRR convention: use the most common observed level as
  # the reference and encode the other level as the alternative.
  x_ref <- names(sort(table(x), decreasing = TRUE))[1]
  x_alt <- setdiff(x_levels, x_ref)
  x_binary <- as.integer(x == x_alt)
  z_vars <- setdiff(names(analytic_df), c(outcome_var, x_var))

  list(
    x = x_binary,
    y = analytic_df[[outcome_var]],
    z = analytic_df[, z_vars, drop = FALSE],
    z_vars = z_vars,
    x_ref = x_ref,
    x_alt = x_alt,
    contrast = paste(x_alt, "vs", x_ref)
  )
}


# Standardize a sampled latent U so that it has mean zero and unit variance.
# This fixes the location and scale used by the two BART models containing U.
standardize_proxy_u <- function(u) {
  drop(scale(u))
}


# Integrate outcome risks over U(x) and U(x') for one posterior iteration.
#
# u_mean_ref and u_mean_alt are the fitted means from U | X, Z after setting X
# to the reference and alternative values. u_sd is the residual standard
# deviation from Gaussian BART. The same normal residual draws are used for
# both interventions to reduce Monte Carlo noise, but U(x) and U(x') generally
# differ because their conditional means differ.
integrate_proxy_u <- function(
    u_mean_ref,
    u_mean_alt,
    u_sd,
    predict_y_ref,
    predict_y_alt,
    n_u_draws = 100L
) {
  n <- length(u_mean_ref)
  risk_ref <- numeric(n)
  risk_alt <- numeric(n)

  for (draw in seq_len(n_u_draws)) {
    residual <- rnorm(n) * u_sd
    u_ref <- u_mean_ref + residual
    u_alt <- u_mean_alt + residual

    risk_ref <- risk_ref + predict_y_ref(u_ref)
    risk_alt <- risk_alt + predict_y_alt(u_alt)
  }

  list(
    risk_ref = risk_ref / n_u_draws,
    risk_alt = risk_alt / n_u_draws
  )
}


# Convert U-integrated outcome probabilities into one population relative risk.
# This is the same case-control population reconstruction used by the primary
# and unmeasured-confounding analyses.
compute_proxy_prr <- function(
    risk_ref,
    risk_alt,
    y,
    population_priors,
    eps = 1e-6
) {
  risk_ref <- pmin(pmax(risk_ref, eps), 1 - eps)
  risk_alt <- pmin(pmax(risk_alt, eps), 1 - eps)

  odds_ratio <-
    (risk_alt / (1 - risk_alt)) /
    (risk_ref / (1 - risk_ref))

  sample_p1 <- mean(y == 1)
  numerator <- risk_ref * population_priors$pY1 / sample_p1
  weights <- numerator / (
    numerator +
      (1 - risk_ref) * population_priors$pY0 / (1 - sample_p1)
  )

  sum(odds_ratio * weights) / sum(weights)
}


# Describe the three-model Gibbs sampler used by the binary proxy analysis.
#
# The executable sampler will update:
#   1. X | Z with probit BART;
#   2. U | X, Z with Gaussian BART, with U standardized to unit variance;
#   3. Y | X, U with probit BART.
#
# During fitting, U is updated using each participant's observed X. During
# prediction, separate U(0) and U(1) values are drawn and integrated before the
# PRR is calculated. This function deliberately stops until that joint sampler
# is implemented; it never returns placeholder estimates.
fit_binary_proxy_model <- function(
    analytic_df,
    outcome_var,
    x_var,
    population_priors,
    nchain = 8L,
    burn = 150L,
    keep = 250L,
    n_u_draws = 100L,
    seed = 20260322L
) {
  model_data <- prepare_binary_proxy_data(
    analytic_df,
    outcome_var,
    x_var
  )

  stop(
    "The binary proxy-model data and PRR integration are ready, ",
    "but the joint BART Gibbs sampler is not yet implemented."
  )
}
