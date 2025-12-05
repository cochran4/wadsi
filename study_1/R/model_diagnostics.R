# -----------------------------------------------------------------------------
# model_diagnostics.R
#
# Description:
#   Produces Bayesian model diagnostics for a fitted dbarts model.
#   Includes:
#     * Traceplot of mean latent fitted value
#     * Autocorrelation plot
#     * Calibration table and plot (predicted vs observed)
#   Optionally saves figures and calibration table to disk.
#
# Inputs:
#   - bart_fit: dbarts model object (with yhat.train)
#   - analytic_df: data frame used for training
#   - outcome_var: name of outcome variable
#   - save_path: optional directory path to save results (default NULL)
#
# Outputs:
#   - List containing ESS summary, calibration summary, and file paths (if saved)
#
# Dependencies:
#   ggplot2, dplyr, coda, knitr, officer, flextable
# -----------------------------------------------------------------------------

model_diagnostics <- function(bart_fit, analytic_df, outcome_var, save_path = NULL) {
  
  message("Running model diagnostics...")
  
  # ---- Define colorblind-friendly palette ----
  cbPalette <- c(
    "blue"   = "#0072B2",
    "orange" = "#E69F00",
    "green"  = "#009E73",
    "pink"   = "#CC79A7",
    "yellow" = "#F0E442",
    "red"    = "#D55E00",
    "purple" = "#56B4E9",
    "gray"   = "#999999"
  )
  
  
  # ---- Extract posterior draws ----
  yhat_train <- bart_fit$yhat.train
  train_mu   <- rowMeans(yhat_train)
  
  # ---- Effective sample size ----
  ess_mu <- effectiveSize(train_mu)
  ess_summary <- data.frame(
    ess_value = ess_mu,
    n_samples = nrow(yhat_train)
  )
  
  # ---- Traceplot ----
  trace_df <- data.frame(iter = seq_along(train_mu), value = train_mu)
  p_trace <- ggplot(trace_df, aes(x = iter, y = value)) +
    geom_line(color = cbPalette["blue"], linewidth = 0.7) +
    labs(
      title = "Traceplot of Mean Fitted Value",
      subtitle = sprintf("Effective sample size ≈ %.0f", ess_mu),
      x = "Iteration", y = "Mean Latent Fitted Value"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  
  # ---- Autocorrelation plot ----
  acf_vals <- acf(train_mu, plot = FALSE)
  acf_df <- data.frame(lag = acf_vals$lag, acf = acf_vals$acf)
  p_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_col(fill = cbPalette["orange"]) +
    labs(
      title = "Autocorrelation of Mean Fitted Value",
      x = "Lag", y = "Autocorrelation"
    ) +
    theme_minimal(base_size = 13)
  
  # ---- Calibration table ----
  p_hat <- colMeans(pnorm(yhat_train))
  calib_df <- data.frame(p_hat = p_hat, y = analytic_df[[outcome_var]])
  calib_df$bin <- cut(
    calib_df$p_hat,
    breaks = quantile(calib_df$p_hat, probs = seq(0, 1, 0.1), na.rm = TRUE),
    include.lowest = TRUE
  )
  
  calib_summary <- calib_df |>
    group_by(bin) |>
    summarise(
      n = n(),
      mean_pred = mean(p_hat, na.rm = TRUE),
      mean_obs  = mean(y, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Calibration plot ----
  p_calib <- ggplot(calib_summary, aes(x = mean_pred, y = mean_obs)) +
    geom_point(size = 3, color = cbPalette["red"]) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = cbPalette["gray"]) +
    coord_equal() +
    labs(
      title = "Calibration Plot",
      subtitle = "Predicted vs. Observed Event Rates",
      x = "Predicted Probability",
      y = "Observed Probability"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  
  # ---- Save results ----
  if (!is.null(save_path)) {
    dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
    
    ggsave(file.path(save_path, "traceplot.png"), p_trace, width = 6, height = 4, dpi = 300)
    ggsave(file.path(save_path, "acf_plot.png"), p_acf, width = 6, height = 4, dpi = 300)
    ggsave(file.path(save_path, "calibration_plot.png"), p_calib, width = 6, height = 4, dpi = 300)
    
    # Save calibration table to Word
    ft <- flextable(calib_summary)
    ft <- autofit(ft)
    doc <- read_docx() |> body_add_flextable(ft)
    print(doc, target = file.path(save_path, "calibration_table.docx"))
    
    message("Saved diagnostics to ", normalizePath(save_path))
  }
  
  list(
    ess_summary = ess_summary,
    calib_summary = calib_summary,
    plots = list(trace = p_trace, acf = p_acf, calib = p_calib)
  )
}