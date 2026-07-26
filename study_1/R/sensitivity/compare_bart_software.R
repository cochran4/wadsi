# Compare probit BART implementations using population relative risks.
#
# BART and dbarts receive the same numeric design matrix. flexBART receives the
# original data frame because its defining feature is direct treatment of
# categorical predictors. The models use the same probit link and sampling
# effort, while retaining each function's ordinary model defaults. In
# particular, do not set the number of trees or model-prior arguments here.


# Create the indicator-expanded design matrix used by BART and dbarts.
prepare_bart_design <- function(analytic_df, outcome_var) {
  predictors <- analytic_df[, setdiff(names(analytic_df), outcome_var), drop = FALSE]

  # Save the terms object so new counterfactual data receive the same columns.
  design_terms <- terms(~ . - 1, data = predictors)

  list(
    matrix = model.matrix(design_terms, predictors),
    terms = design_terms
  )
}


# Fit BART, dbarts::bart2, and flexBART for comparison with the saved
# dbarts::bart reference model.
fit_probit_bart_comparison <- function(
    analytic_df,
    outcome_var,
    nchain,
    nskip,
    ndpost,
    thin = 2L,
    seed = 20260322L
) {
  design <- prepare_bart_design(analytic_df, outcome_var)
  y <- analytic_df[[outcome_var]]

  # flexBART requires a binary outcome stored specifically as an integer.
  flexbart_data <- analytic_df
  flexbart_data[[outcome_var]] <- as.integer(y)

  # Run the eight independent BART chains in parallel.
  bart_time <- system.time({
    bart_fits <- parallel::mclapply(
      seq_len(nchain),
      function(chain) {
        set.seed(seed + chain - 1L)
        BART::pbart(
          x.train = design$matrix,
          y.train = y,
          nskip = nskip,
          ndpost = ndpost,
          keepevery = thin,
          nkeeptreedraws = ndpost,
          printevery = max(nskip + ndpost, 1L)
        )
      },
      mc.cores = nchain,
      mc.set.seed = FALSE
    )
  })[["elapsed"]]

  failed_bart_chains <- which(vapply(
    bart_fits,
    inherits,
    logical(1),
    "try-error"
  ))
  if (length(failed_bart_chains) > 0L) {
    stop(
      "BART chain(s) failed: ",
      paste(failed_bart_chains, collapse = ", "),
      ". First error: ",
      as.character(bart_fits[[failed_bart_chains[1]]])
    )
  }

  # Fit the formula-interface version of dbarts with its ordinary model
  # defaults, including 75 trees and its family-specific default for k. The
  # arguments below change only sampling, parallelization, and saved output.
  dbarts_bart2_time <- system.time({
    dbarts_bart2_fit <- dbarts::bart2(
      formula = reformulate(".", response = outcome_var),
      data = flexbart_data,
      n.chains = nchain,
      n.threads = nchain,
      n.burn = nskip,
      n.samples = ndpost * thin,
      n.thin = thin,
      combineChains = TRUE,
      keepTrees = TRUE,
      verbose = FALSE,
      seed = seed
    )
  })[["elapsed"]]

  # flexBART runs chains sequentially within one fit, so run eight independent
  # one-chain fits in parallel instead.
  flexbart_time <- system.time({
    flexbart_fits <- parallel::mclapply(
      seq_len(nchain),
      function(chain) {
        set.seed(seed + chain - 1L)
        flexBART::flexBART(
          formula = reformulate("bart(.)", response = outcome_var),
          train_data = flexbart_data,
          family = binomial(link = "probit"),
          nest_v = FALSE,
          nest_c = FALSE,
          n.chains = 1L,
          burn = nskip,
          nd = ndpost,
          thin = thin,
          save_samples = FALSE,
          save_trees = TRUE,
          verbose = FALSE
        )
      },
      mc.cores = nchain,
      mc.set.seed = FALSE
    )
  })[["elapsed"]]

  failed_flexbart_chains <- which(vapply(
    flexbart_fits,
    inherits,
    logical(1),
    "try-error"
  ))
  if (length(failed_flexbart_chains) > 0L) {
    stop(
      "flexBART chain(s) failed: ",
      paste(failed_flexbart_chains, collapse = ", "),
      ". First error: ",
      as.character(flexbart_fits[[failed_flexbart_chains[1]]])
    )
  }

  list(
    BART = bart_fits,
    dbarts_bart2 = dbarts_bart2_fit,
    flexBART = flexbart_fits,
    design_terms = design$terms,
    outcome_var = outcome_var,
    timing = data.frame(
      software = c("BART", "dbarts::bart2", "flexBART"),
      elapsed_seconds = c(bart_time, dbarts_bart2_time, flexbart_time)
    ),
    settings = list(
      nchain = nchain,
      nskip = nskip,
      ndpost = ndpost,
      thin = thin,
      seed = seed
    )
  )
}


# Return predictions from each package as a draws-by-person probability matrix.
predict_probit_bart <- function(fits, reference_model, software, newdata) {
  if (software == "BART") {
    # Apply the design terms created at fitting so the columns remain aligned.
    x <- model.matrix(
      fits$design_terms,
      newdata[, setdiff(names(newdata), fits$outcome_var), drop = FALSE]
    )

    return(do.call(
      rbind,
      lapply(fits$BART, function(fit) predict(fit, x)$prob.test)
    ))
  }

  if (software == "dbarts_bart") {
    return(predict(reference_model, newdata = newdata))
  }

  if (software == "dbarts_bart2") {
    return(predict(fits$dbarts_bart2, newdata = newdata))
  }

  # The remaining implementation is flexBART. Combine the one-chain fits in
  # chain order.
  flexbart_newdata <- newdata
  flexbart_newdata[[fits$outcome_var]] <- as.integer(
    flexbart_newdata[[fits$outcome_var]]
  )

  do.call(
    rbind,
    lapply(fits$flexBART, predict, newdata = flexbart_newdata)
  )
}


# Compare convergence and predictive diagnostics across the implementations.
comparison_model_diagnostics <- function(
    fits,
    reference_model,
    analytic_df,
    outcome_var,
    save_path = "outputs/diagnostics/software_comparison",
    reference_save_path = "outputs/diagnostics/reference"
) {
  nchain <- fits$settings$nchain
  ndpost <- fits$settings$ndpost

  bart_draws <- do.call(rbind, lapply(fits$BART, `[[`, "prob.train"))
  bart_diagnostics <- model_diagnostics(
    probability_draws = bart_draws,
    analytic_df = analytic_df,
    outcome_var = outcome_var,
    save_path = file.path(save_path, "BART"),
    nchain = nchain,
    ndpost = ndpost
  )
  rm(bart_draws)

  dbarts_bart_diagnostics <- model_diagnostics(
    bart_fit = reference_model,
    analytic_df = analytic_df,
    outcome_var = outcome_var,
    save_path = reference_save_path,
    nchain = nchain,
    ndpost = ndpost
  )

  dbarts_bart2_diagnostics <- model_diagnostics(
    bart_fit = fits$dbarts_bart2,
    analytic_df = analytic_df,
    outcome_var = outcome_var,
    save_path = file.path(save_path, "dbarts_bart2"),
    nchain = nchain,
    ndpost = ndpost
  )

  flexbart_draws <- predict_probit_bart(
    fits,
    reference_model,
    "flexBART",
    analytic_df
  )
  flexbart_diagnostics <- model_diagnostics(
    probability_draws = flexbart_draws,
    analytic_df = analytic_df,
    outcome_var = outcome_var,
    save_path = file.path(save_path, "flexBART"),
    nchain = nchain,
    ndpost = ndpost
  )
  rm(flexbart_draws)

  diagnostic_list <- list(
    BART = bart_diagnostics,
    dbarts_bart = dbarts_bart_diagnostics,
    dbarts_bart2 = dbarts_bart2_diagnostics,
    flexBART = flexbart_diagnostics
  )

  summary <- do.call(rbind, lapply(names(diagnostic_list), function(software) {
    diagnostics <- diagnostic_list[[software]]
    software_label <- c(
      BART = "BART",
      dbarts_bart = "dbarts::bart",
      dbarts_bart2 = "dbarts::bart2",
      flexBART = "flexBART"
    )[[software]]
    data.frame(
      software = software_label,
      median_individual_rhat = median(diagnostics$rhat_values),
      proportion_below_1.10 = mean(diagnostics$rhat_values < 1.10),
      proportion_below_1.20 = mean(diagnostics$rhat_values < 1.20),
      maximum_individual_rhat = max(diagnostics$rhat_values),
      global_rhat = diagnostics$ess_summary$rhat,
      bulk_ess = diagnostics$ess_summary$ess_bulk,
      tail_ess = diagnostics$ess_summary$ess_tail,
      brier_score = diagnostics$brier_score,
      negative_log_probability = diagnostics$negative_log_probability
    )
  }))

  list(summary = summary, diagnostics = diagnostic_list)
}


# Reproduce the predictor contrasts used in the primary analysis.
define_prr_contrasts <- function(analytic_df, config, predictor_stats_df) {
  contrasts <- list()

  for (predictor in config$predictors) {
    var <- predictor$name

    if (predictor$type == "continuous") {
      # Continuous predictors compare the observed 90th and 10th percentiles.
      stats_row <- predictor_stats_df[predictor_stats_df$predictor == var, ]

      contrasts[[length(contrasts) + 1L]] <- list(
        predictor = var,
        contrast = "90th vs. 10th percentile",
        ref = stats_row$q10,
        alt = stats_row$q90
      )
    } else {
      # The most common observed category is the reference. Missing remains a
      # possible alternative level but is not eligible to be the reference.
      values <- factor(analytic_df[[var]])
      levels <- levels(values)
      observed_levels <- setdiff(levels, "Missing")
      reference <- names(sort(
        table(values[values %in% observed_levels]),
        decreasing = TRUE
      ))[1]

      for (alternative in setdiff(levels, reference)) {
        contrasts[[length(contrasts) + 1L]] <- list(
          predictor = var,
          contrast = paste0(alternative, " vs ", reference),
          ref = reference,
          alt = alternative
        )
      }
    }
  }

  contrasts
}


# Construct the two intervention datasets for one predictor contrast.
make_counterfactual_data <- function(analytic_df, contrast) {
  ref_data <- analytic_df
  alt_data <- analytic_df
  var <- contrast$predictor

  # Preserve factor levels so prediction uses the columns learned at fitting.
  if (is.factor(analytic_df[[var]])) {
    levels <- levels(analytic_df[[var]])
    ref_data[[var]] <- factor(contrast$ref, levels = levels)
    alt_data[[var]] <- factor(contrast$alt, levels = levels)
  } else {
    ref_data[[var]] <- contrast$ref
    alt_data[[var]] <- contrast$alt
  }

  list(ref = ref_data, alt = alt_data)
}


# Convert posterior predictions into the primary-analysis PRR summary.
summarize_prr <- function(
    p_ref,
    p_alt,
    y,
    population_priors,
    eps = 1e-6
) {
  # Keep the odds finite when posterior probabilities approach zero or one.
  p_ref <- pmin(pmax(p_ref, eps), 1 - eps)
  p_alt <- pmin(pmax(p_alt, eps), 1 - eps)

  # Form the odds-scale individual contrast used by the primary-analysis PRR
  # calculation; conditional odds ratios are invariant to case-control sampling.
  odds_ratio <-
    (p_alt / (1 - p_alt)) /
    (p_ref / (1 - p_ref))

  # Reweight each person's reference risk from the sample outcome prevalence
  # to the configured population prevalence, then average the individual RRs.
  p_y1_sample <- mean(y == 1)
  p_y0_sample <- 1 - p_y1_sample
  numerator <- p_ref * population_priors$pY1 / p_y1_sample
  weights <- numerator /
    (numerator + (1 - p_ref) * population_priors$pY0 / p_y0_sample)

  draws <- rowSums(odds_ratio * weights) / rowSums(weights)

  c(
    posterior_mean = mean(draws),
    posterior_median = median(draws),
    ci_lower = unname(quantile(draws, 0.025)),
    ci_upper = unname(quantile(draws, 0.975))
  )
}


# Compute every predictor contrast for each probit BART implementation.
compare_bart_prrs <- function(
    fits,
    reference_model,
    analytic_df,
    config,
    predictor_stats_df
) {
  contrasts <- define_prr_contrasts(
    analytic_df,
    config,
    predictor_stats_df
  )
  software_names <- c("BART", "dbarts_bart", "dbarts_bart2", "flexBART")
  results <- list()

  # Counterfactual predictions can be large, so compute and summarize one
  # software-by-contrast pair at a time.
  for (software in software_names) {
    for (contrast in contrasts) {
      counterfactual <- make_counterfactual_data(analytic_df, contrast)
      p_ref <- predict_probit_bart(
        fits, reference_model, software, counterfactual$ref
      )
      p_alt <- predict_probit_bart(
        fits, reference_model, software, counterfactual$alt
      )

      prr <- summarize_prr(
        p_ref,
        p_alt,
        analytic_df[[config$outcome$name]],
        config$population_priors
      )

      results[[length(results) + 1L]] <- data.frame(
        software = c(
          BART = "BART",
          dbarts_bart = "dbarts::bart",
          dbarts_bart2 = "dbarts::bart2",
          flexBART = "flexBART"
        )[[software]],
        predictor = contrast$predictor,
        contrast = contrast$contrast,
        posterior_mean = prr["posterior_mean"],
        posterior_median = prr["posterior_median"],
        ci_lower = prr["ci_lower"],
        ci_upper = prr["ci_upper"]
      )

      rm(p_ref, p_alt)
    }
  }

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  result
}


# Plot the PRRs from the software comparison.
#
# Each panel is one contrast. Points are posterior medians and vertical lines
# are 95% credible intervals. The displayed PRR axis is limited to 0.1--3 so
# that the unusually wide bart2 intervals do not compress the other results.
plot_bart_comparison_prrs <- function(prrs, config) {
  plot_data <- prrs

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

  plot_data$label <- paste(
    plot_data$predictor_label,
    plot_data$contrast_label,
    sep = "\n"
  )
  plot_data$label <- factor(
    plot_data$label,
    levels = unique(plot_data$label)
  )
  plot_data$software <- factor(
    plot_data$software,
    levels = c("BART", "dbarts::bart", "dbarts::bart2", "flexBART")
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = software,
      y = posterior_median,
      color = software
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "grey55",
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.20,
      linewidth = 0.7
    ) +
    # Mark intervals that continue above or below the displayed axis range.
    ggplot2::geom_segment(
      data = plot_data[plot_data$ci_upper > 3, ],
      ggplot2::aes(
        x = software,
        xend = software,
        y = posterior_median,
        yend = 3
      ),
      arrow = grid::arrow(
        length = grid::unit(0.08, "inches"),
        type = "closed"
      ),
      linewidth = 0.7
    ) +
    ggplot2::geom_segment(
      data = plot_data[plot_data$ci_lower < 0.1, ],
      ggplot2::aes(
        x = software,
        xend = software,
        y = posterior_median,
        yend = 0.1
      ),
      arrow = grid::arrow(
        length = grid::unit(0.08, "inches"),
        type = "closed"
      ),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::facet_wrap(
      ggplot2::vars(label),
      ncol = 2
    ) +
    ggplot2::scale_y_log10(
      breaks = c(0.1, 0.25, 0.5, 1, 2, 3),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::coord_cartesian(ylim = c(0.1, 3)) +
    ggplot2::scale_color_manual(
      values = c(
        "flexBART" = "#CC79A7",
        "dbarts::bart2" = "#D55E00",
        "dbarts::bart" = "#0072B2",
        "BART" = "#333333"
      ),
      guide = "none"
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Population relative risk"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
}
