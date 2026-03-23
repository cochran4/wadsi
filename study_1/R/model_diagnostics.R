# -----------------------------------------------------------------------------
# model_diagnostics.R
#
# Description:
#   Produces basic diagnostics for a fitted dbarts model.
#   Includes:
#     * Traceplot of the mean latent fitted value
#     * Autocorrelation plot of the mean latent fitted value
#     * Effective sample size of the mean latent fitted value
#     * Sampled R-hat values for selected fitted values
#     * Calibration table and plot based on posterior mean predicted probabilities
#     * Brier score
#
#   Optionally saves plots and summaries to disk.
#
# Inputs:
#   - bart_fit: fitted dbarts model object
#   - analytic_df: training data used to fit the model
#   - outcome_var: name of the binary outcome variable
#   - save_path: optional directory for saved outputs
#   - nchain: number of chains used in dbarts::bart()
#   - ndpost: number of retained posterior draws per chain
#   - n_rhat_check: number of fitted values to sample for R-hat checks
#
# Output:
#   A list containing:
#     * ess_summary: effective sample size summary
#     * rhat_summary: summary of sampled R-hat values
#     * rhat_values: sampled R-hat values
#     * brier_score: Brier score
#     * calib_summary: calibration table
#     * plots: diagnostic plots
#
# -----------------------------------------------------------------------------

model_diagnostics <- function(
    bart_fit,
    analytic_df,
    outcome_var,
    save_path = NULL,
    nchain = 8L,
    ndpost = 8000L
) {
  message("Running model diagnostics...")
  
  # Colorblind-friendly palette
  cb_palette <- c(
    "blue"   = "#0072B2",
    "orange" = "#E69F00",
    "green"  = "#009E73",
    "pink"   = "#CC79A7",
    "yellow" = "#F0E442",
    "red"    = "#D55E00",
    "purple" = "#56B4E9",
    "gray"   = "#999999"
  )
  
  # Posterior draws of latent fitted values
  yhat_train <- bart_fit$yhat.train
  
  # Check draw dimensions against chain settings
  total_draws <- nrow(yhat_train)
  expected_draws <- as.integer(nchain) * as.integer(ndpost)
  
  # Observed outcome
  y_obs <- analytic_df[[outcome_var]]
  
  # ---------------------------------------------------------------------------
  # Global MCMC diagnostics
  # ---------------------------------------------------------------------------
  
  # Mean latent fitted value at each posterior draw
  draw_mean <- rowMeans(yhat_train)
  
  # Reshape into iterations x chains
  draw_mean_mat <- matrix(draw_mean, ncol = nchain)
  
  # Build mcmc.list object
  draw_mean_mcmc <- coda::mcmc.list(
    lapply(seq_len(nchain), function(j) coda::mcmc(draw_mean_mat[, j]))
  )
  
  # Effective sample size across chains
  ess_mean <- coda::effectiveSize(draw_mean_mcmc)
  
  # Effective sample size by chain
  ess_mean_by_chain <- unlist(lapply(draw_mean_mcmc, coda::effectiveSize))
  names(ess_mean_by_chain) <- paste0("chain_", seq_along(ess_mean_by_chain))
  
  # R-hat for the same summary
  rhat_mean <- coda::gelman.diag(draw_mean_mcmc, autoburnin = FALSE)$psrf[1, 1]
  
  ess_summary <- data.frame(
    quantity = "mean_latent_fitted_value",
    ess_total = as.numeric(ess_mean),
    ess_min_chain = min(ess_mean_by_chain),
    ess_median_chain = median(ess_mean_by_chain),
    nchain = nchain,
    ndpost = ndpost,
    ess_ratio = as.numeric(ess_mean) / (nchain * ndpost),
    rhat = as.numeric(rhat_mean)
  )
  
  # Construct a long-format data frame with one row per iteration per chain
  trace_df <- do.call(
    rbind,
    lapply(seq_len(nchain), function(j) {
      data.frame(
        iter = seq_len(ndpost),
        value = draw_mean_mat[, j],
        chain = factor(paste("Chain", j))
      )
    })
  )
  
  # Initialize traceplot: map iteration to x, value to y, and color by chain
  p_trace <- ggplot2::ggplot(
    trace_df,
    ggplot2::aes(x = iter, y = value, color = chain)
  ) +
    # Plot the trajectory of the summary statistic within each chain
    ggplot2::geom_line(linewidth = 0.6, alpha = 0.9) +
    
    # Separate panels by chain to assess mixing and overlap
    ggplot2::facet_wrap(~ chain, ncol = 2) +
    
    # Use the predefined colorblind-friendly palette
    ggplot2::scale_color_manual(values = unname(cb_palette[seq_len(nchain)])) +
    
    # Label axes (titles handled elsewhere)
    ggplot2::labs(
      x = "Iteration",
      y = "Mean latent fitted value"
    ) +
    
    # Apply minimal theme with larger, modern fonts
    ggplot2::theme_minimal(base_size = 16, base_family = "Helvetica") +
    
    # Emphasize axis labels and clean up legend
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(face = "bold", size = 16),
      axis.text = ggplot2::element_text(size = 14)
    )
  
  # Compute autocorrelation within each chain and stack results into one data frame
  acf_df <- do.call(
    rbind,
    lapply(seq_len(nchain), function(j) {
      # Autocorrelation of the summary statistic for chain j
      acf_vals <- stats::acf(draw_mean_mat[, j], plot = FALSE)
      
      # Store lag and autocorrelation values along with chain label
      data.frame(
        lag = as.numeric(acf_vals$lag),
        acf = as.numeric(acf_vals$acf),
        chain = factor(paste("Chain", j))
      )
    })
  )
  
  # Initialize ACF plot (lag vs autocorrelation)
  p_acf <- ggplot2::ggplot(
    acf_df,
    ggplot2::aes(x = lag, y = acf, fill = chain)
  ) +
    # Bar plot emphasizes decay of autocorrelation over lags
    ggplot2::geom_col(alpha = 0.9) +
    
    # Separate panels by chain to assess dependence within each chain
    ggplot2::facet_wrap(~ chain, ncol = 2) +
    
    # Apply colorblind-friendly palette across chains
    ggplot2::scale_fill_manual(values = unname(cb_palette[seq_len(nchain)])) +
    
    # Axis labels only (titles handled elsewhere)
    ggplot2::labs(
      x = "Lag",
      y = "Autocorrelation"
    ) +
    
    # Apply consistent visual styling
    ggplot2::theme_minimal(base_size = 16, base_family = "Helvetica") +
    
    # Clean up legend and emphasize axes
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(face = "bold", size = 16),
      axis.text = ggplot2::element_text(size = 14)
    )
  
  # ---------------------------------------------------------------------------
  # Sampled R-hat checks
  # ---------------------------------------------------------------------------
  
  # Determine how many fitted values to check
  # Loop over all fitted values
  rhat_values <- sapply(seq_len(ncol(yhat_train)), function(i) {
    # Posterior draws for fitted value i
    draw_i <- yhat_train[, i]
    
    # Reshape combined draws into iterations x chains
    draw_i_mat <- matrix(draw_i, ncol = nchain)
    
    # Build mcmc.list object for Gelman-Rubin diagnostic
    draw_i_mcmc <- coda::mcmc.list(
      lapply(seq_len(nchain), function(j) coda::mcmc(draw_i_mat[, j]))
    )
    
    # Extract point estimate of R-hat
    unname(coda::gelman.diag(draw_i_mcmc, autoburnin = FALSE)$psrf[1, 1])
  })
  
  # Summarize the sampled R-hat values
  rhat_summary <- data.frame(
    quantity = c("min", "q1", "median", "mean", "q3", "max"),
    value = as.numeric(summary(rhat_values))
  )
  
  # Create histogram of sampled R-hat values
  p_rhat <- ggplot2::ggplot(
    data.frame(rhat = rhat_values),
    ggplot2::aes(x = rhat)
  ) +
    # Histogram of sampled R-hat values
    ggplot2::geom_histogram(
      bins = 10,
      fill = cb_palette["green"],
      color = "white",
      alpha = 0.9
    ) +
    
    # Reference line at a common informal threshold
    ggplot2::geom_vline(
      xintercept = 1.05,
      linetype = "dashed",
      color = cb_palette["red"],
      linewidth = 0.8
    ) +
    
    # Axis labels only (titles handled elsewhere)
    ggplot2::labs(
      x = "R-hat",
      y = "Persons"
    ) +
    
    # Apply consistent visual styling
    ggplot2::theme_minimal(base_size = 16, base_family = "Helvetica") +
    
    # Emphasize axes
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold", size = 16),
      axis.text = ggplot2::element_text(size = 14)
    )
  
  # ---------------------------------------------------------------------------
  # Predictive checks
  # ---------------------------------------------------------------------------
  
  # Convert latent posterior draws to posterior mean predicted probabilities
  p_hat <- colMeans(stats::pnorm(yhat_train))
  
  # Summarize overall predictive accuracy with the Brier score
  brier_score <- mean((p_hat - y_obs)^2, na.rm = TRUE)
  
  # Create a data frame with predicted probabilities and observed outcomes
  calib_df <- data.frame(
    p_hat = p_hat,
    y = y_obs
  )
  
  # Group observations into deciles of predicted risk
  calib_df$bin <- dplyr::ntile(calib_df$p_hat, 10)
  
  # Summarize predicted and observed event rates within each risk decile
  calib_summary <- calib_df |>
    dplyr::group_by(bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_pred = mean(p_hat, na.rm = TRUE),
      mean_obs = mean(y, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Initialize calibration plot
  p_calib <- ggplot2::ggplot(
    calib_summary,
    ggplot2::aes(x = mean_pred, y = mean_obs)
  ) +
    # Plot observed versus predicted event rates by risk decile
    ggplot2::geom_point(size = 3.5, color = cb_palette["green"]) +
    
    # Add 45-degree reference line for perfect calibration
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = cb_palette["gray"],
      linewidth = 0.8
    ) +
    
    # Use equal scales on both axes
    ggplot2::coord_equal() +
    
    # Axis labels only (titles handled elsewhere)
    ggplot2::labs(
      x = "Predicted probability",
      y = "Observed event rate"
    ) +
    
    # Apply consistent visual styling
    ggplot2::theme_minimal(base_size = 16, base_family = "Helvetica") +
    
    # Emphasize axes
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold", size = 16),
      axis.text = ggplot2::element_text(size = 14)
    )
  # ---------------------------------------------------------------------------
  # Save results
  # ---------------------------------------------------------------------------
  
  if (!is.null(save_path)) {
    dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
    
    ggplot2::ggsave(
      file.path(save_path, "traceplot.png"),
      p_trace, width = 6, height = 6, dpi = 600
    )
    ggplot2::ggsave(
      file.path(save_path, "acf_plot.png"),
      p_acf, width = 6, height = 6, dpi = 600
    )
    ggplot2::ggsave(
      file.path(save_path, "rhat_histogram.png"),
      p_rhat, width = 6, height = 4, dpi = 600
    )
    ggplot2::ggsave(
      file.path(save_path, "calibration_plot.png"),
      p_calib, width = 6, height = 4, dpi = 600
    )
    
    readr::write_csv(ess_summary, file.path(save_path, "ess_summary.csv"))
    readr::write_csv(rhat_summary, file.path(save_path, "rhat_summary.csv"))
    readr::write_csv(
      data.frame(obs_index = seq_along(rhat_values),
                 rhat = rhat_values),
      file.path(save_path, "rhat_values.csv")
    )
    readr::write_csv(calib_summary, file.path(save_path, "calibration_summary.csv"))
    readr::write_csv(
      data.frame(metric = "brier_score", value = brier_score),
      file.path(save_path, "predictive_metrics.csv")
    )
    
    message("Saved diagnostics to ", normalizePath(save_path))
  }
  
  list(
    ess_summary = ess_summary,
    rhat_summary = rhat_summary,
    rhat_values = rhat_values,
    brier_score = brier_score,
    calib_summary = calib_summary,
    plots = list(
      trace = p_trace,
      acf = p_acf,
      rhat = p_rhat,
      calib = p_calib
    )
  )
}