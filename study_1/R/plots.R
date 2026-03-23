# -----------------------------------------------------------------------------
# plot_individual_rr.R
#
# Description:
#   Creates two panels:
#     Top/Left    = Continuous predictors
#     Bottom/Right = Categorical predictors
#
#   Each dot represents one person's posterior mean individual relative risk.
#   The x-axis is shown on a log2 scale. Dots are white-filled circles with
#   colored outlines, and violins summarize the distribution within each
#   predictor-contrast pair.
#
# Inputs:
#   irr_summary_df : data.frame with columns
#       - id, predictor, contrast, posterior_mean_irr
#       - type (optional; inferred if missing)
#   save_path          : optional path to save the horizontal plot
#   save_path_vertical : optional path to save the vertical plot
#
# Output:
#   A patchwork plot object, returned invisibly.
# -----------------------------------------------------------------------------
plot_individual_rr <- function(irr_summary_df, config, save_path = NULL, save_path_vertical = NULL) {
  
  
  # Extract human-readable predictor labels from config (if provided)
  label_map <- if (!is.null(config$predictor_labels)) {
    unlist(config$predictor_labels)
  } else {
    NULL
  }
  
  # Build a lookup table for predictor type from config
  predictor_type_df <- dplyr::bind_rows(config$predictors) %>%
    dplyr::select(name, type) %>%
    dplyr::rename(predictor = name)
  
  # Add predictor type to the plotting data
  irr_summary_df <- irr_summary_df %>%
    dplyr::left_join(predictor_type_df, by = "predictor")
  
  # Convert simple 0/1 labels to No/Yes for readability
  clean_contrast <- function(x) {
    x <- gsub("\\b1\\b", "Yes", x)
    x <- gsub("\\b0\\b", "No", x)
    x
  }
  
  # Create display labels:
  #   - Replace raw variable names with readable labels when available
  #   - For continuous predictors, show only the variable name
  #   - For categorical predictors, append the contrast
  irr_summary_df <- irr_summary_df %>%
    dplyr::mutate(
      predictor_label = dplyr::if_else(
        !is.null(label_map) & predictor %in% names(label_map),
        unname(label_map[predictor]),
        predictor
      ),
      contrast_clean = clean_contrast(contrast),
      display_label = dplyr::case_when(
        type == "continuous" ~ predictor_label,
        TRUE ~ paste0(predictor_label, "\n", contrast_clean)
      )
    )

  # --- Colorblind-friendly Okabe-Ito palette ---
  cb_palette <- c(
    "#3B4CC0", "#E69F00", "#009E73", "#CC79A7",
    "#D55E00", "#56B4E9", "#F0E442", "#999999"
  )
  
  make_panel <- function(df) {
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        # X-axis: posterior mean individual relative risk (log scale applied below)
        x = posterior_mean_irr,
        
        # Y-axis: cleaned display label (reversed for top-to-bottom ordering)
        y = forcats::fct_rev(factor(display_label)),
        
        # Color grouping by readable predictor label
        fill = predictor_label
      )
    ) +
      
      # Reference line at no effect (relative risk = 1)
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      
      # Violin shows distribution of individual effects within each displayed contrast
      ggplot2::geom_violin(
        ggplot2::aes(group = display_label),
        fill = "gray90",
        color = "gray70",
        width = 0.6,
        scale = "width",
        alpha = 0.6
      ) +
      
      # Points show individual posterior means (jittered for visibility)
      ggplot2::geom_point(
        shape = 21,
        color = "white",
        stroke = 0.2,
        size = 1.0,
        alpha = 0.25,
        position = ggplot2::position_jitter(width = 0.01, height = 0.17)
      ) +
      
      # Log2 scale for interpretability (e.g., doubling/halving)
      ggplot2::scale_x_continuous(
        trans = "log2",
        breaks = c(0.25, 0.5, 0.67, 1, 1.5, 2, 4),
        labels = c("0.25", "0.5", "0.67", "1", "1.5", "2", "4")
      ) +
      
      # Consistent color palette across predictors
      ggplot2::scale_fill_manual(values = cb_palette) +
      
      # Axis labels
      ggplot2::labs(x = "Individual relative risk", y = NULL) +
      
      # Minimal theme with emphasis on readability
      
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        
        # Y-axis labels (predictor + contrast)
        axis.text.y = ggplot2::element_text(size = 12, face = "bold"),
        
        # X-axis title
        axis.title.x = ggplot2::element_text(face = "bold", size = 17),
        
        # X-axis tick labels
        axis.text.x = ggplot2::element_text(size = 12),
        
        legend.position = "none",
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )
  }
  
  # --- Build panels by predictor type ---
  p_cont <- irr_summary_df %>%
    dplyr::filter(type == "continuous") %>%
    make_panel()
  
  p_cat <- irr_summary_df %>%
    dplyr::filter(type == "categorical") %>%
    make_panel()
  
  # --- Add panel labels ---
  p_cont <- p_cont +
    ggplot2::labs(tag = "A") +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(face = "bold", size = 16, family = "sans")
    )
  
  p_cat <- p_cat +
    ggplot2::labs(tag = "B") +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(face = "bold", size = 16, family = "sans")
    )
  
  # --- Combine panels for different outputs ---
  
  # Horizontal layout (for saving)
  plot_horizontal <- p_cont | p_cat
  plot_horizontal <- plot_horizontal + plot_annotation()
  
  # Vertical layout (for Quarto display)
  plot_vertical <- p_cont / p_cat
  plot_vertical <- plot_vertical + plot_annotation()
    
  # --- Save ---
  if (!is.null(save_path)) {
    ggsave(save_path, plot_horizontal, width = 16, height = 11, dpi = 600)
  }
   
   # Save vertical version
   if (!is.null(save_path_vertical)) {
     ggplot2::ggsave(
       filename = save_path_vertical,
       plot     = plot_vertical,
       width    = 8,
       height   = 16,
       dpi      = 600
     )
   }
   
   # Return vertical for interactive use
   invisible(plot_vertical)

}


plot_shapley_beeswarm <- function(af_shapley,
                                  analytic_df,
                                  predictor_stats_df,
                                  save_path = NULL,
                                  save_path_vertical = NULL) {
  
  # ---- Colorblind-friendly palettes ----------------------------------------
  cbContinuous <- c(
    low  = "#3B4CC0",  # blue
    mid  = "white",
    high = "#D55E00"   # reddish
  )
  
  cbDiscrete <- c(
    "#3B4CC0", "#E69F00", "#009E73", "#CC79A7",
    "#D55E00", "#56B4E9", "#F0E442", "#999999"
  )
  
  # ---- Order features by mean |Shapley| ------------------------------------
  feature_order <- af_shapley |>
    dplyr::group_by(feature) |>
    dplyr::summarise(
      mean_abs = mean(abs(shapley_mean), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(mean_abs)
  
  features <- unique(af_shapley$feature)
  
  # ---- Long form: feature values from analytic_df --------------------------
  long_vals <- analytic_df |>
    tibble::rownames_to_column("id") |>
    dplyr::mutate(id = as.character(id)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(features), as.character)) |>
    dplyr::select(id, dplyr::all_of(features)) |>
    tidyr::pivot_longer(
      cols      = -id,
      names_to  = "feature",
      values_to = "feature_value"
    )
  
  # ---- Lookup of mean/SD for continuous predictors -------------------------
  stats_lookup <- predictor_stats_df |>
    dplyr::select(
      feature = predictor,
      mean,
      sd
    )
  
  # ---- Join shapley + feature values + stats -------------------------------
  af_plot <- af_shapley |>
    dplyr::mutate(id = as.character(id)) |>
    dplyr::inner_join(long_vals, by = c("id", "feature")) |>
    dplyr::left_join(stats_lookup, by = "feature") |>
    dplyr::mutate(
      feature = factor(feature, levels = feature_order$feature),
      is_continuous = !is.na(mean) & !is.na(sd) & sd > 0
    ) |>
    dplyr::group_by(feature) |>
    dplyr::mutate(
      raw_num = suppressWarnings(as.numeric(feature_value)),
      raw_num = dplyr::if_else(
        is.na(raw_num),
        as.numeric(as.factor(feature_value)),
        raw_num
      ),
      feature_value_scaled = dplyr::if_else(
        is_continuous,
        (raw_num - mean) / sd,
        as.numeric(scale(raw_num))
      ),
      cat_level = dplyr::if_else(
        is_continuous,
        NA_character_,
        feature_value
      )
    ) |>
    dplyr::ungroup()
  
  # Split into continuous vs categorical
  af_cont <- af_plot |> dplyr::filter(is_continuous)
  af_cat  <- af_plot |> dplyr::filter(!is_continuous)
  
  # --- Shared x-axis limits (based on shapley_mean) ---
  x_limits <- range(af_plot$shapley_mean, na.rm = TRUE)
  
  # ---- Panel builders ------------------------------------------------------
  make_cont_panel <- function(df) {
    ggplot2::ggplot(df, ggplot2::aes(
      x = shapley_mean,
      y = feature,
      color = feature_value_scaled
    )) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
      ggbeeswarm::geom_quasirandom(
        orientation = "y", width = 0.4,
        alpha = 0.9, size = 1.6, stroke = 0.4
      ) +
      ggplot2::scale_color_gradient2(
        low = cbContinuous["low"], mid = cbContinuous["mid"], high = cbContinuous["high"],
        midpoint = 0, name = "Standardized value\n(low → high)"
      ) +
      ggplot2::scale_x_continuous(limits = x_limits, expand = ggplot2::expansion(mult = 0.05)) +
      ggplot2::labs(x = "Posterior mean Shapley value", y = NULL) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_text(size = 10),
        axis.title.x = ggplot2::element_text(face = "bold", size = 13),
        legend.position = "right",
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )
  }
  
  make_cat_panel <- function(df) {
    df <- df |> dplyr::mutate(cat_level = factor(cat_level))
    ggplot2::ggplot(df, ggplot2::aes(
      x = shapley_mean,
      y = feature,
      color = cat_level
    )) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
      ggbeeswarm::geom_quasirandom(
        orientation = "y", width = 0.4,
        alpha = 0.8, size = 1.8, stroke = 0.4
      ) +
      ggplot2::scale_color_manual(values = cbDiscrete, guide = "none") +
      ggplot2::scale_x_continuous(limits = x_limits, expand = ggplot2::expansion(mult = 0.05)) +
      ggplot2::labs(x = "Posterior mean Shapley value", y = NULL) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_text(size = 10),
        axis.title.x = ggplot2::element_text(face = "bold", size = 13),
        plot.margin  = ggplot2::margin(10, 10, 10, 10)
      )
  }
  
  # ---- Build panels --------------------------------------------------------
  p_cont <- make_cont_panel(af_cont) + ggplot2::labs(tag = "A")
  p_cat  <- make_cat_panel(af_cat)  + ggplot2::labs(tag = "B")
  
  # ---- Combine -------------------------------------------------------------
  plot_horizontal <- p_cont | p_cat
  plot_vertical   <- p_cont / p_cat
  
  # ---- Save ---------------------------------------------------------------
  if (!is.null(save_path))
    ggplot2::ggsave(save_path, plot_horizontal, width = 15, height = 6.5, dpi = 600)
  if (!is.null(save_path_vertical))
    ggplot2::ggsave(save_path_vertical, plot_vertical, width = 8, height = 11, dpi = 600)
  
  invisible(plot_vertical)
}

# =============================================================================
# plot_individual_af.R
#
# Description:
#   Publication-quality histogram of individual attributable fractions.
#   Displays the posterior distribution of AF estimates across individuals.
#   Uses a colorblind-safe, high-contrast palette suitable for Science/Nature.
# =============================================================================

plot_individual_af <- function(af_summary_df, save_path = NULL) {
  
  # --- Compute summary statistics -------------------------------------------
  af_mean_global <- mean(af_summary_df$af_mean, na.rm = TRUE)
  
  # --- Create plot ----------------------------------------------------------
  p <- ggplot2::ggplot(af_summary_df, ggplot2::aes(x = af_mean)) +
    # Histogram of AF values
    ggplot2::geom_histogram(
      aes(y = after_stat(density)),
      bins = 35,
      fill = "#3B4CC0",   # deep blue (Okabe–Ito inspired)
      color = "white",
      alpha = 0.95
    ) +
    # Smoothed density curve
    ggplot2::geom_density(
      linewidth = 1,
      color = "#E69F00",  # rich orange contrast
      alpha = 1
    ) +
    ggplot2::labs(
      x = "Individual attributable fraction",
      y = "Count, persons"
    ) +
    ggplot2::theme_minimal(base_family = "Helvetica", base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 16,
        hjust = 0.5,
        family = "Helvetica"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 12,
        family = "Helvetica"
      ),
      axis.title.x = ggplot2::element_text(face = "bold", size = 13),
      axis.title.y = ggplot2::element_text(face = "bold", size = 13),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  
  # --- Save high-resolution figure ------------------------------------------
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, p, width = 7, height = 5, dpi = 600)
  }
  
  return(p)
}