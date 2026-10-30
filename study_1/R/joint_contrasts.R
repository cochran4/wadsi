# =============================================================================
# compute_joint_relative_risks.R
#
# Description:
#   Estimates population relative risks for groups of predictors
#   changed together, using posterior predictions from the fitted BART model.
#   These CAB-motivated contrasts were specified after protocol acceptance.
#   They provide additional interpretations of the same model; no model is refit.
#
#   For each group listed in config$joint_contrasts:
#     * Continuous: compares 90th vs. 10th percentiles from pre-imputation summaries
#     * Binary categorical: compares the other non-missing level to the most
#       common non-missing level, as in the individual relative-risk calculation
#
#   Each contrast is oriented using the earlier population PRR median:
#   reverse the original settings when that median is below 1.
#   All selected predictors change together. Remaining variables, including
#   missingness indicators, retain their observed values. The odds contrasts,
#   reference-scenario weights, and posterior summaries follow
#   compute_causal_relative_risks(). The protocol's assumptions and justification
#   for interpreting the odds contrasts as relative risks still apply.
#
# Inputs:
#   - bart_fit: fitted dbarts::bart model
#   - analytic_df: analytic data set used for model fitting
#   - config: configuration containing predictors, outcome, population_priors,
#             and joint_contrasts (e.g., a group containing sbp and tot_chol)
#   - prr_summary: earlier irr_results$weighted_summary, used to orient settings
#   - contrasts: groups of predictor names; defaults to config$joint_contrasts
#   - predictor_stats_df: pre-imputation q10 and q90 for continuous predictors
#
# Output:
#   A list containing:
#     * weighted_summary: population posterior summaries and 95% credible intervals
# =============================================================================
compute_joint_relative_risks <- function(bart_fit, analytic_df, config, prr_summary,
                                       contrasts = config$joint_contrasts,
                                       predictor_stats_df = NULL) {

  # --- Setup ---------------------------------------------------------------
  # Outcome variable and observed outcomes in the analytic sample
  outcome_var <- config$outcome$name
  y_vec <- analytic_df[[outcome_var]]

  # Outcome probabilities in the sample and in the target population
  pY1_S1 <- mean(y_vec == 1, na.rm = TRUE)
  pY0_S1 <- 1 - pY1_S1
  pY1 <- config$population_priors$pY1
  pY0 <- config$population_priors$pY0

  # Obtain predicted probabilities and clamp them away from 0 and 1
  get_probs <- function(fit, newdata, eps = 1e-6) {
    p <- stats::predict(fit, newdata = newdata)
    pmin(pmax(p, eps), 1 - eps)
  }

  # Compute the weights used to move from the sample to the target population
  compute_weights <- function(p_ref) {
    num <- p_ref * pY1 / pY1_S1
    denom <- p_ref * pY1 / pY1_S1 + (1 - p_ref) * pY0 / pY0_S1
    num / denom
  }

  # Predictor definitions from the configuration
  predictors <- config$predictors
  predictor_names <- vapply(predictors, `[[`, character(1), "name")

  # Collect population summaries across joint contrasts
  weighted_summary_all <- list()

  # --- Main loop -----------------------------------------------------------
  for (vars in contrasts) {
    vars <- unlist(vars, use.names = FALSE)
    contrast <- paste(vars, collapse = " + ")
    message("Processing joint contrast: ", contrast)

    # Begin with two copies of the analytic data. Only the selected predictors
    # are overwritten; all other predictors and adjustment variables stay fixed.
    x_alt <- analytic_df
    x_ref <- analytic_df
    comparison_labels <- character()
    reference_labels <- character()

    for (var in vars) {
      pred <- predictors[[match(var, predictor_names)]]
      type <- pred$type

      if (type == "continuous") {
        # --- Continuous predictor ------------------------------------------
        # Use the same pre-imputation percentiles as the individual contrasts
        stats_row <- predictor_stats_df[predictor_stats_df$predictor == var, ]
        high <- stats_row$q90
        low <- stats_row$q10

        alt <- high
        ref <- low

        original_contrast <- "90th vs. 10th percentile"
        alt_label <- format(high, digits = 6, trim = TRUE)
        ref_label <- format(low, digits = 6, trim = TRUE)

      } else if (type == "categorical") {
        # --- Binary categorical predictor ----------------------------------
        # Choose the most common non-missing level as the reference
        vals <- factor(analytic_df[[var]])
        levs <- levels(vals)
        valid_levels <- setdiff(levs, "Missing")
        ref <- names(sort(table(vals[vals %in% valid_levels]), decreasing = TRUE))[1]
        alt <- setdiff(valid_levels, ref)

        original_contrast <- paste0(alt, " vs ", ref)
        # Show the actual category codes as well as Yes/No for binary 0/1 data.
        alt_label <- alt
        ref_label <- ref
        if (all(valid_levels %in% c("0", "1"))) {
          alt_label <- paste0(ifelse(alt == "1", "Yes", "No"), " [", alt, "]")
          ref_label <- paste0(ifelse(ref == "1", "Yes", "No"), " [", ref, "]")
        }
      }

      # Retrieve the earlier population PRR for this predictor and contrast.
      # Match the contrast as well as the name to exclude Missing-category rows.
      prr <- prr_summary$posterior_median[
        prr_summary$predictor == var &
          prr_summary$contrast == original_contrast
      ]

      # Orient the comparison toward the individually estimated higher-risk
      # setting. Reverse the values and their labels when the earlier PRR < 1.
      if (prr < 1) {
        original_alt <- alt
        alt <- ref
        ref <- original_alt

        original_alt_label <- alt_label
        alt_label <- ref_label
        ref_label <- original_alt_label
      }

      # Set the selected predictor in both joint scenarios.
      if (type == "continuous") {
        x_alt[[var]] <- alt
        x_ref[[var]] <- ref
      } else if (type == "categorical") {
        x_alt[[var]] <- factor(alt, levels = levs, ordered = is.ordered(vals))
        x_ref[[var]] <- factor(ref, levels = levs, ordered = is.ordered(vals))
      }

      # Record both settings explicitly for the population results table.
      predictor_label <- config$predictor_labels[[var]]
      if (is.null(predictor_label)) predictor_label <- var
      comparison_labels <- c(comparison_labels, paste0(predictor_label, " = ", alt_label))
      reference_labels <- c(reference_labels, paste0(predictor_label, " = ", ref_label))
    }

    # Predict under the two joint settings for each posterior draw
    p_alt <- get_probs(bart_fit, x_alt)
    p_ref <- get_probs(bart_fit, x_ref)

    # Convert predicted probabilities to odds and form the individual contrast
    odds_alt <- p_alt / (1 - p_alt)
    odds_ref <- p_ref / (1 - p_ref)
    irr_hat_draws <- odds_alt / odds_ref

    # Weight by the reference scenario, as in the individual-predictor PRRs
    weights <- compute_weights(p_ref)
    weighted_irr <- rowSums(irr_hat_draws * weights) / rowSums(weights)

    # Store posterior summaries of the population-level contrast
    weighted_summary_all[[length(weighted_summary_all) + 1]] <- data.frame(
      contrast = contrast,
      comparison = paste(comparison_labels, collapse = "; "),
      reference = paste(reference_labels, collapse = "; "),
      posterior_mean = mean(weighted_irr),
      posterior_median = median(weighted_irr),
      ci_lower = as.numeric(stats::quantile(weighted_irr, 0.025)),
      ci_upper = as.numeric(stats::quantile(weighted_irr, 0.975))
    )

  }

  # --- Combine and return --------------------------------------------------
  list(
    weighted_summary = dplyr::bind_rows(weighted_summary_all)
  )
}

# =============================================================================
# plot_joint_relative_risks.R
#
# Description:
#   Plots population posterior medians and 95% credible intervals for the joint
#   contrasts. Rows follow the YAML order, matching the accompanying table.
#   The table gives the exact comparison and reference settings for each row.
#
# Inputs:
#   - weighted_summary: joint_rr_results$weighted_summary
#   - config: configuration containing predictor display labels
#   - save_path: optional path for the figure
#
# Output:
#   A ggplot object, returned invisibly.
# =============================================================================
plot_joint_relative_risks <- function(weighted_summary, config, save_path = NULL) {

  # --- Prepare labels ------------------------------------------------------
  # Use the display labels from YAML instead of raw variable names.
  plot_df <- weighted_summary
  plot_df$contrast_label <- plot_df$contrast
  for (i in seq_len(nrow(plot_df))) {
    vars <- strsplit(plot_df$contrast[i], " + ", fixed = TRUE)[[1]]
    for (j in seq_along(vars)) {
      label <- config$predictor_labels[[vars[j]]]
      if (!is.null(label)) vars[j] <- label
    }
    plot_df$contrast_label[i] <- paste(vars, collapse = " + ")
  }

  # Wrap display labels and retain the same row order as the results table.
  plot_df$contrast_label <- stringr::str_wrap(plot_df$contrast_label, width = 45)
  plot_df$contrast_label <- factor(
    plot_df$contrast_label,
    levels = rev(plot_df$contrast_label)
  )

  # --- Population relative-risk plot ---------------------------------------
  # Points show posterior medians; horizontal lines show 95% credible intervals.
  # The dashed line marks a relative risk of 1 (equal risk in both scenarios).
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = posterior_median, y = contrast_label)
  ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_segment(
      ggplot2::aes(x = ci_lower, xend = ci_upper, yend = contrast_label)
    ) +
    ggplot2::geom_point(size = 3, colour = "#3B4CC0") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Population relative risk (95% credible interval)",
      y = NULL,
      caption = "Comparison versus reference settings are specified in the accompanying table."
    ) +
    ggplot2::theme_minimal(base_size = 12)

  # --- Save and return -----------------------------------------------------
  if (!is.null(save_path)) {
    ggplot2::ggsave(
      save_path, p, width = 11,
      height = max(4, 1.6 * nrow(plot_df)), dpi = 300
    )
  }

  invisible(p)
}
