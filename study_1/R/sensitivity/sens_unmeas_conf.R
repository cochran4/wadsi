# Evaluate one binary observation under a probit model.
#
# If Pr(R = 1) = Phi(eta), the likelihood contribution is
#
#   R = 1: Phi(eta)
#   R = 0: 1 - Phi(eta).
#
# We calculate the log probabilities directly to avoid numerical problems in
# the tails of the normal distribution.
log_probit_likelihood <- function(r, eta) {
  ifelse(
    r == 1,
    pnorm(eta, log.p = TRUE),
    pnorm(eta, lower.tail = FALSE, log.p = TRUE)
  )
}


# Sample the binary unmeasured confounder U for every participant.
#
# For each person, compare the posterior kernel under U = 0 with the kernel
# under U = 1:
#
#   Pr(U = u | X, Y, Z) is proportional to
#   Pr(U = u) Pr(X | Z, U = u) Pr(Y | X, Z, U = u).
#
# The calculation is performed on the log scale. The predictor likelihood can
# come from either a binary probit model or a continuous Gaussian model.
sample_u <- function(
    y,
    f_y,
    beta_x,
    beta_y,
    pi_u,
    x,
    f_x,
    x_sigma,
    x_model,
    x_rows
) {
  # Start each participant's X likelihood at log(1) = 0. For a categorical
  # predictor, entries outside the requested contrast remain zero under both
  # U = 0 and U = 1 and therefore cancel from the update of U.
  log_x_u0 <- numeric(length(y))
  log_x_u1 <- numeric(length(y))

  if (x_model == "binary") {
    # For a categorical contrast, x is 1 for the alternative level and 0 for
    # the reference level. beta_x shifts the latent probit mean when U = 1.
    log_x_u0[x_rows] <- log_probit_likelihood(x, f_x)
    log_x_u1[x_rows] <- log_probit_likelihood(x, f_x + beta_x)
  } else {
    # For a continuous predictor, x is standardized. The Gaussian BART model
    # supplies its conditional mean f_x and residual standard deviation.
    log_x_u0[x_rows] <- dnorm(x, mean = f_x, sd = x_sigma, log = TRUE)
    log_x_u1[x_rows] <- dnorm(
      x,
      mean = f_x + beta_x,
      sd = x_sigma,
      log = TRUE
    )
  }

  # Posterior kernel for U = 0: the prior probability of U = 0, the observed
  # predictor likelihood, and the observed outcome likelihood.
  log_prob_u0 <-
    log1p(-pi_u) +
    log_x_u0 +
    log_probit_likelihood(y, f_y)

  # Posterior kernel for U = 1. beta_y shifts the latent probit outcome mean,
  # just as beta_x shifts the predictor model above.
  log_prob_u1 <-
    log(pi_u) +
    log_x_u1 +
    log_probit_likelihood(y, f_y + beta_y)

  # With only two possible values of U, the difference between the log kernels
  # can be converted directly into Pr(U = 1 | X, Y, Z).
  prob_u1 <- plogis(log_prob_u1 - log_prob_u0)

  # Draw one U value per participant for the next Gibbs iteration.
  u <- rbinom(length(y), size = 1, prob = prob_u1)

  # Keep both the sampled U and its conditional probability. Averaging the
  # probabilities across iterations gives a less noisy estimate of each
  # participant's posterior probability of U = 1.
  list(
    u = u,
    prob_u1 = prob_u1
  )
}


# Prepare the predictor model for either a categorical contrast or a continuous
# predictor. Observations outside a categorical contrast do not contribute to
# the X likelihood when U is updated.
prepare_sensitivity_data <- function(
    analytic_df,
    y_var,
    x_var,
    x_ref,
    x_alt,
    predictor_stats_df
) {
  # Z contains every analysis variable except the focal predictor and outcome.
  z_vars <- setdiff(names(analytic_df), c(y_var, x_var))

  # Keep the outcome on its original 0/1 scale for the outcome likelihood.
  y <- analytic_df[[y_var]]

  if (is.factor(analytic_df[[x_var]])) {
    # Keep only participants whose observed X is one of the two levels in the
    # requested contrast.
    x_rows <- which(analytic_df[[x_var]] %in% c(x_ref, x_alt))

    # Encode the alternative level as 1 and the reference level as 0 for the
    # probit BART predictor model.
    x <- as.integer(analytic_df[[x_var]][x_rows] == x_alt)
    x_model <- "binary"

    # Centering and scaling apply only to continuous predictors.
    x_center <- NA_real_
    x_scale <- NA_real_
  } else {
    # Every participant contributes to a continuous predictor model.
    x_rows <- seq_len(nrow(analytic_df))

    # Use summaries calculated from the observed data before imputation. This
    # prevents the imputed analytic dataset from redefining the predictor's
    # center, scale, or sensitivity-parameter interpretation.
    stats_row <- predictor_stats_df[predictor_stats_df$predictor == x_var, ]
    x_center <- stats_row$mean
    x_scale <- stats_row$sd
    x <- (analytic_df[[x_var]] - x_center) / x_scale
    x_model <- "continuous"
  }

  # Construct the data for the predictor model. It contains the prepared X
  # response and Z, but not the outcome Y.
  x_data <- analytic_df[x_rows, z_vars, drop = FALSE]
  x_data[[x_var]] <- x

  list(
    x = x,
    y = y,
    x_rows = x_rows,
    x_model = x_model,
    x_center = x_center,
    x_scale = x_scale,
    z_vars = z_vars,
    x_data = x_data
  )
}


# Extract one chain from the reference outcome model.
#
# dbarts::bart() stores all chains in one mutable sampler. The sensitivity
# workflow needs one independent outcome sampler per joint chain because each
# chain has its own U vector and therefore its own offset.
#
# We copy the final state of the selected reference chain. The retained
# reference draws are not copied: they remain available in reference_model for
# the primary analysis but are not needed to initialize the sensitivity chain.
extract_reference_chain <- function(reference_model, chain_id) {
  reference_sampler <- reference_model$fit

  # The chain states were copied to R with storeState() before the reference
  # model was saved. Use those stored states directly after readRDS().

  # Create a one-chain sampler with the same model and training data.
  control <- reference_sampler$control
  control@n.chains <- 1L
  control@n.samples <- 1L
  control@keepTrees <- FALSE

  sampler_class <- methods::getRefClass(
    "dbartsSampler",
    where = asNamespace("dbarts")
  )
  sampler <- sampler_class$new(
    control,
    reference_sampler$model,
    reference_sampler$data
  )

  # Keep the current trees but discard the saved posterior-tree history.
  chain_state <- reference_sampler$state[[chain_id]]
  chain_state@savedTrees <- integer(0)

  # dbarts stores bookkeeping as attributes on the list of chain states.
  state <- list(chain_state)
  attributes(state) <- attributes(reference_sampler$state)
  attr(state, "currentNumSamples") <- 1L
  attr(state, "currentSampleNum") <- 0L

  sampler$setState(state)
  sampler
}


# Construct the two counterfactual datasets used for one PRR contrast.
#
# Both datasets contain one row per participant and the same variables used by
# the reference outcome model. They differ only in whether the focal predictor
# is set to the reference value x' or the alternative value x.
prepare_prr_data <- function(
    analytic_df,
    x_var,
    x_ref,
    x_alt,
    reference_model
) {
  # Recover the original variable names used by the reference outcome model,
  # then arrange those variables exactly as they appear in the analytic data.
  outcome_vars <- attr(reference_model$fit$data@x, "term.labels")
  outcome_data <- analytic_df[, outcome_vars, drop = FALSE]

  # Begin both counterfactual datasets from the same observed covariate
  # profiles. This holds all variables other than the focal predictor fixed.
  ref_data <- outcome_data
  alt_data <- outcome_data

  if (is.factor(analytic_df[[x_var]])) {
    # Preserve the original factor levels so that dbarts constructs the same
    # indicator columns used when fitting the reference outcome model.
    x_levels <- levels(analytic_df[[x_var]])

    # Set every participant to x' in the reference dataset and to x in the
    # alternative dataset.
    ref_data[[x_var]] <- factor(x_ref, levels = x_levels)
    alt_data[[x_var]] <- factor(x_alt, levels = x_levels)
  } else {
    # Continuous interventions remain on the predictor's original scale. For
    # the primary contrasts, x_ref and x_alt are the pre-imputation 10th and
    # 90th percentiles, respectively.
    ref_data[[x_var]] <- x_ref
    alt_data[[x_var]] <- x_alt
  }

  # Return the two datasets and a label describing the direction of contrast.
  list(
    ref = ref_data,
    alt = alt_data,
    contrast = paste0(x_alt, " vs ", x_ref)
  )
}


# Compute one population relative-risk draw.
#
# The calculation uses one current draw of the outcome BART model and one
# current vector of U values. Because U is a pre-exposure confounder, the same
# U_i is used for person i under both interventions. The case-control weighting
# calculation is the same as in the primary analysis.
compute_prr_draw <- function(
    y_sampler,
    prr_data,
    u,
    beta_y,
    p_y1_sample,
    population_priors,
    eps = 1e-6
) {
  # Add beta_y U_i to the latent probit outcome function for every person.
  offset <- beta_y * u

  # Predict each person's latent probit outcome under X = x' and X = x, using
  # the same U offset in both counterfactual datasets. Convert the two latent
  # predictions to probabilities with the standard normal CDF.
  p_ref <- pnorm(
    y_sampler$predict(prr_data$ref, offset.test = offset)
  )
  p_alt <- pnorm(
    y_sampler$predict(prr_data$alt, offset.test = offset)
  )

  # Keep probabilities away from exactly zero and one before forming odds.
  p_ref <- pmin(pmax(p_ref, eps), 1 - eps)
  p_alt <- pmin(pmax(p_alt, eps), 1 - eps)

  # Form the profile-specific odds-ratio approximation used for the causal
  # relative risk in the primary analysis.
  odds_ratio <-
    (p_alt / (1 - p_alt)) /
      (p_ref / (1 - p_ref))

  # Recover the population weights from the sampled-data reference risks, the
  # outcome prevalence in the case-control sample, and the known population
  # outcome prevalence.
  p_y0_sample <- 1 - p_y1_sample
  weight_numerator <-
    p_ref * population_priors$pY1 / p_y1_sample
  weights <- weight_numerator /
    (
      weight_numerator +
      (1 - p_ref) * population_priors$pY0 / p_y0_sample
    )

  # Average the profile-specific contrast using the reconstructed population
  # weights. This produces one PRR value for the current posterior iteration.
  sum(odds_ratio * weights) / sum(weights)
}


# Fit one complete sensitivity chain.
#
# The predictor model is new because the reference analysis only fitted the
# outcome model. The outcome sampler begins at the final state of one reference
# chain. The joint burn-in then allows both models and U to adapt to the chosen
# beta_x and beta_y values.
fit_one_proxy_sensitivity_model <- function(
    analytic_df,
    y_var,
    x_var,
    beta_x,
    beta_y,
    reference_model,
    population_priors,
    predictor_stats_df,
    x_ref,
    x_alt,
    reference_chain = 1L,
    pi_u = 0.5,
    x_burn = 150L,
    inner_burn = 150L,
    inner_keep = 250L,
    inner_thin = 1L
) {
  model_data <- prepare_sensitivity_data(
    analytic_df,
    y_var,
    x_var,
    x_ref,
    x_alt,
    predictor_stats_df
  )
  n_obs <- length(model_data$y)
  p_y1_sample <- mean(model_data$y == 1)

  prr_data <- prepare_prr_data(
    analytic_df,
    x_var,
    x_ref,
    x_alt,
    reference_model
  )

  # Create the initial predictor sampler without U. The bart() interface uses
  # probit BART when its response is 0/1 and Gaussian BART otherwise. Its
  # sampler-only form lets us update the model one Gibbs iteration at a time.
  # We retain the bart() default k = 2 used by the reference outcome model.
  x_sampler <- dbarts::bart(
    x.train = model_data$x_data[, model_data$z_vars, drop = FALSE],
    y.train = model_data$x,
    nskip = 0L,
    ndpost = 1L,
    nchain = 1L,
    verbose = FALSE,
    keeptrees = FALSE,
    keeptrainfits = TRUE,
    sampleronly = TRUE
  )
  x_sampler$run(x_burn, 1L)

  # Start the outcome model at the corresponding reference-model chain.
  y_sampler <- extract_reference_chain(reference_model, reference_chain)

  # Initialize U from its assumed marginal prevalence.
  u <- rbinom(n_obs, size = 1, prob = pi_u)

  # Retain only the population prevalence of U and the PRR at each iteration.
  # The current participant-level U vector is still used by the Gibbs sampler
  # but is overwritten rather than stored after every update.
  u_prevalence_draws <- numeric(inner_keep)
  prr_draws <- numeric(inner_keep)

  total_iterations <- inner_burn + inner_keep * inner_thin
  saved_draw <- 0L

  # One systematic-scan Gibbs iteration updates f_x, then f_y, then U.
  for (iter in seq_len(total_iterations)) {
    # 1. Update the predictor model given the current U:
    #
    #      Pr(X = 1 | Z, U) = Phi{f_x(Z) + beta_x U}.
    #
    # dbarts returns f_x plus the offset, so subtract the offset afterward.
    x_offset <- beta_x * u[model_data$x_rows]
    x_sampler$setOffset(x_offset)
    x_update <- x_sampler$run(0L, 1L)
    f_x <- drop(x_update$train) - x_offset
    x_sigma <- if (model_data$x_model == "continuous") {
      drop(x_update$sigma)
    } else {
      1
    }

    # 2. Update the outcome model given the same current U:
    #
    #      Pr(Y = 1 | X, Z, U) = Phi{f_y(X, Z) + beta_y U}.
    y_offset <- beta_y * u
    y_sampler$setOffset(y_offset)
    y_update <- y_sampler$run(0L, 1L)
    f_y <- drop(y_update$train) - y_offset

    # 3. Update each U_i given the observed X_i and Y_i and current functions.
    u_update <- sample_u(
      model_data$y,
      f_y,
      beta_x,
      beta_y,
      pi_u,
      model_data$x,
      f_x,
      x_sigma,
      model_data$x_model,
      model_data$x_rows
    )
    u <- u_update$u

    # After joint burn-in, retain every inner_thin-th iteration.
    if (iter > inner_burn && (iter - inner_burn) %% inner_thin == 0L) {
      saved_draw <- saved_draw + 1L

      # Average the full-conditional probabilities rather than the sampled 0/1
      # values to obtain a less noisy estimate of the prevalence of U.
      u_prevalence_draws[saved_draw] <- mean(u_update$prob_u1)

      prr_draws[saved_draw] <- compute_prr_draw(
        y_sampler,
        prr_data,
        u,
        beta_y,
        p_y1_sample,
        population_priors
      )
    }
  }

  list(
    summary = data.frame(
      predictor = x_var,
      beta_x = beta_x,
      beta_y = beta_y,
      pi_u = pi_u,
      posterior_u_prevalence = mean(u_prevalence_draws)
    ),
    draws = list(
      u_prevalence = u_prevalence_draws,
      prr = prr_draws
    ),
    settings = list(
      predictor = x_var,
      outcome = y_var,
      beta_x = beta_x,
      beta_y = beta_y,
      pi_u = pi_u,
      contrast = prr_data$contrast,
      predictor_model = model_data$x_model,
      predictor_center = model_data$x_center,
      predictor_scale = model_data$x_scale,
      reference_chain = reference_chain,
      x_burn = x_burn,
      inner_burn = inner_burn,
      inner_keep = inner_keep,
      inner_thin = inner_thin
    )
  )
}


# Run multiple independent chains for one sensitivity setting.
#
# The number of sensitivity chains comes from the reference model. Sensitivity
# chain c begins from reference-model chain c, so the two sets of chains remain
# aligned. The chains run in parallel and remain separate for later convergence
# assessment. Because all chains retain the same number of iterations, their
# posterior U probabilities receive equal weight.
fit_proxy_sensitivity_model_chains <- function(
    analytic_df,
    y_var,
    x_var,
    beta_x,
    beta_y,
    reference_model,
    population_priors,
    predictor_stats_df,
    x_ref,
    x_alt,
    pi_u = 0.5,
    seed = NULL,
    x_burn = 100L,
    inner_burn = 100L,
    inner_keep = 100L,
    inner_thin = 1L
) {
  nchain <- reference_model$fit$control@n.chains

  chain_seeds <- if (is.null(seed)) {
    rep(NA_integer_, nchain)
  } else {
    as.integer(seed) + seq_len(nchain) - 1L
  }

  chains <- parallel::mclapply(seq_len(nchain), function(chain_id) {
    # Use a distinct but reproducible seed for the predictor model and U.
    if (!is.na(chain_seeds[chain_id])) {
      set.seed(chain_seeds[chain_id])
    }

    fit <- fit_one_proxy_sensitivity_model(
      analytic_df = analytic_df,
      y_var = y_var,
      x_var = x_var,
      beta_x = beta_x,
      beta_y = beta_y,
      reference_model = reference_model,
      population_priors = population_priors,
      predictor_stats_df = predictor_stats_df,
      x_ref = x_ref,
      x_alt = x_alt,
      reference_chain = chain_id,
      pi_u = pi_u,
      x_burn = x_burn,
      inner_burn = inner_burn,
      inner_keep = inner_keep,
      inner_thin = inner_thin
    )

    fit$summary$chain <- chain_id
    fit$settings$chain <- chain_id
    fit$settings$seed <- chain_seeds[chain_id]
    fit
  }, mc.cores = nchain, mc.set.seed = FALSE)
  names(chains) <- paste0("chain_", seq_len(nchain))

  summary <- do.call(rbind, lapply(chains, `[[`, "summary"))
  rownames(summary) <- NULL

  prr_draws <- unlist(
    lapply(chains, function(chain) chain$draws$prr),
    use.names = FALSE
  )

  prr_by_chain <- do.call(
    cbind,
    lapply(chains, function(chain) chain$draws$prr)
  )
  u_prevalence_by_chain <- do.call(
    cbind,
    lapply(chains, function(chain) chain$draws$u_prevalence)
  )

  u_prevalence_draws <- unlist(
    lapply(chains, function(chain) chain$draws$u_prevalence),
    use.names = FALSE
  )

  diagnostics <- data.frame(
    quantity = c("PRR", "U prevalence"),
    rhat = c(
      posterior::rhat(prr_by_chain),
      posterior::rhat(u_prevalence_by_chain)
    ),
    bulk_ess = c(
      posterior::ess_bulk(prr_by_chain),
      posterior::ess_bulk(u_prevalence_by_chain)
    ),
    tail_ess = c(
      posterior::ess_tail(prr_by_chain),
      posterior::ess_tail(u_prevalence_by_chain)
    )
  )

  list(
    summary = summary,
    posterior = list(
      u_prevalence_draws = u_prevalence_draws,
      posterior_u_prevalence = mean(u_prevalence_draws)
    ),
    prr = list(
      draws = prr_draws,
      summary = data.frame(
        predictor = x_var,
        contrast = chains[[1]]$settings$contrast,
        beta_x = beta_x,
        beta_y = beta_y,
        posterior_mean = mean(prr_draws),
        posterior_median = median(prr_draws),
        ci_lower = as.numeric(quantile(prr_draws, 0.025)),
        ci_upper = as.numeric(quantile(prr_draws, 0.975))
      )
    ),
    diagnostics = diagnostics,
    chains = chains,
    settings = list(
      predictor = x_var,
      outcome = y_var,
      beta_x = beta_x,
      beta_y = beta_y,
      pi_u = pi_u,
      contrast = chains[[1]]$settings$contrast,
      nchain = nchain,
      chain_seeds = chain_seeds,
      x_burn = x_burn,
      inner_burn = inner_burn,
      inner_keep = inner_keep,
      inner_thin = inner_thin
    )
  )
}


# Evaluate one predictor's PRR over a grid of sensitivity parameters.
#
# Each grid cell runs the complete set of chains inherited from the reference
# model. The returned summary has one row per (beta_x, beta_y) combination;
# the full fitted objects remain available for any later diagnostics.
fit_proxy_sensitivity_grid <- function(
    analytic_df,
    y_var,
    x_var,
    sensitivity_settings,
    reference_model,
    population_priors,
    predictor_stats_df,
    x_ref,
    x_alt,
    pi_u = 0.5,
    seed = NULL,
    x_burn = 100L,
    inner_burn = 100L,
    inner_keep = 100L,
    inner_thin = 1L
) {
  grid <- sensitivity_settings

  fits <- lapply(seq_len(nrow(grid)), function(i) {
    fit_proxy_sensitivity_model_chains(
      analytic_df = analytic_df,
      y_var = y_var,
      x_var = x_var,
      beta_x = grid$beta_x[i],
      beta_y = grid$beta_y[i],
      reference_model = reference_model,
      population_priors = population_priors,
      predictor_stats_df = predictor_stats_df,
      x_ref = x_ref,
      x_alt = x_alt,
      pi_u = pi_u,
      seed = seed,
      x_burn = x_burn,
      inner_burn = inner_burn,
      inner_keep = inner_keep,
      inner_thin = inner_thin
    )
  })

  summary <- do.call(
    rbind,
    lapply(fits, function(fit) fit$prr$summary)
  )
  rownames(summary) <- NULL

  diagnostics <- do.call(rbind, lapply(seq_along(fits), function(i) {
    data.frame(
      predictor = x_var,
      contrast = fits[[i]]$settings$contrast,
      beta_x = grid$beta_x[i],
      beta_y = grid$beta_y[i],
      fits[[i]]$diagnostics
    )
  }))
  rownames(diagnostics) <- NULL

  list(
    summary = summary,
    diagnostics = diagnostics,
    fits = fits,
    grid = grid
  )
}


# Run the unmeasured-confounding analysis for every PRR contrast reported in
# the primary analysis. Contrasts and predictors are processed sequentially;
# the chains within each sensitivity setting run in parallel.
fit_all_proxy_sensitivity_models <- function(
    analytic_df,
    y_var,
    config,
    predictor_stats_df,
    sensitivity_settings,
    reference_model,
    pi_u = 0.5,
    seed = NULL,
    x_burn = 100L,
    inner_burn = 100L,
    inner_keep = 100L,
    inner_thin = 1L
) {
  contrasts <- define_prr_contrasts(
    analytic_df,
    config,
    predictor_stats_df
  )

  fits <- lapply(contrasts, function(contrast) {
    fit_proxy_sensitivity_grid(
      analytic_df = analytic_df,
      y_var = y_var,
      x_var = contrast$predictor,
      sensitivity_settings = sensitivity_settings,
      reference_model = reference_model,
      population_priors = config$population_priors,
      predictor_stats_df = predictor_stats_df,
      x_ref = contrast$ref,
      x_alt = contrast$alt,
      pi_u = pi_u,
      seed = seed,
      x_burn = x_burn,
      inner_burn = inner_burn,
      inner_keep = inner_keep,
      inner_thin = inner_thin
    )
  })

  names(fits) <- vapply(
    contrasts,
    function(contrast) paste(contrast$predictor, contrast$contrast, sep = ": "),
    character(1)
  )

  summary <- do.call(rbind, lapply(fits, `[[`, "summary"))
  diagnostics <- do.call(rbind, lapply(fits, `[[`, "diagnostics"))
  rownames(summary) <- NULL
  rownames(diagnostics) <- NULL

  list(
    summary = summary,
    diagnostics = diagnostics,
    fits = fits,
    contrasts = contrasts
  )
}


# Plot the PRRs from the unmeasured-confounding analysis.
#
# Each row is one contrast from the primary analysis. Points are posterior
# medians and horizontal lines are 95% credible intervals. A logarithmic PRR
# axis gives proportional increases and decreases the same visual weight.
plot_proxy_sensitivity_prrs <- function(summary, config) {
  plot_data <- summary

  # Replace variable names and coded factor levels with reader-facing labels.
  predictor_labels <- unlist(config$predictor_labels)
  plot_data$predictor_label <- predictor_labels[plot_data$predictor]
  plot_data$contrast_label <- plot_data$contrast
  plot_data$contrast_label[
    plot_data$predictor == "smoker_current" & plot_data$contrast == "1 vs 0"
  ] <- "Yes vs No"
  plot_data$contrast_label[
    plot_data$predictor == "smoker_current" &
      plot_data$contrast == "Missing vs 0"
  ] <- "Missing vs No"
  plot_data$contrast_label[
    plot_data$predictor == "bp_treated" & plot_data$contrast == "0 vs 1"
  ] <- "No vs Yes"

  # Continuous contrasts may contain more decimal places than are useful in a
  # figure. Display them as the rounded values reported in the primary table.
  continuous_rows <- plot_data$predictor %in% c("sbp", "hdl_c", "tot_chol")
  plot_data$contrast_label[continuous_rows] <- vapply(
    strsplit(plot_data$contrast[continuous_rows], " vs ", fixed = TRUE),
    function(values) paste(round(as.numeric(values)), collapse = " vs "),
    character(1)
  )

  plot_data$label <- paste(
    plot_data$predictor_label,
    plot_data$contrast_label,
    sep = "\n"
  )

  # Give the three sensitivity settings concise labels and retain their
  # intended order in the legend and within each contrast.
  plot_data$scenario <- factor(
    paste(plot_data$beta_x, plot_data$beta_y),
    levels = c("0 0", "0.5 0.5", "0.5 -0.5"),
    labels = c(
      "No unmeasured confounding",
      "Same direction",
      "Opposite direction"
    )
  )

  # Reverse the observed contrast order so the first reported contrast appears
  # at the top of the figure.
  plot_data$label <- factor(
    plot_data$label,
    levels = rev(unique(plot_data$label))
  )

  dodge <- ggplot2::position_dodge(width = 0.65)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = posterior_median,
      y = label,
      color = scenario
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 1,
      color = "grey55",
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      position = dodge,
      orientation = "y",
      width = 0.18,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(position = dodge, size = 2.6) +
    ggplot2::scale_x_log10(
      breaks = c(0.25, 0.5, 1, 2, 4),
      limits = c(0.12, 4),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_color_manual(
      values = c("#333333", "#0072B2", "#D55E00")
    ) +
    ggplot2::labs(
      x = "Population relative risk",
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
