# -----------------------------------------------------------------------------
# plot_conditional_clr.R
#
# Description:
#   Creates two horizontally aligned panels:
#     Left  = Continuous predictors
#     Right = Categorical predictors
#
#   Each dot = one person's posterior mean causal likelihood ratio (CLR).
#   X-axis = log₂ CLR; dots are white-filled circles with colored outlines.
#   Both panels share a similar look and scale, with titles above each.
#
# Inputs:
#   clr_summary_df : data.frame with columns
#       - id, predictor, contrast, clr_mean
#       - type (optional; inferred if missing)
#   save_path      : optional path to save the plot (e.g. "outputs/CLR.png")
#
# Output:
#   A ggplot patchwork object, optionally saved as PNG.
# -----------------------------------------------------------------------------
plot_conditional_clr <- function(clr_summary_df, save_path = NULL, save_path_vertical = NULL) {
  
  # --- Infer type if missing ---
  clr_summary_df <- clr_summary_df %>%
    mutate(
      type = case_when(
        grepl("1SD", contrast, ignore.case = TRUE) ~ "continuous",
        TRUE ~ "categorical"
      )
  )

  # --- Colorblind-friendly Okabe–Ito palette ---
  cbPalette <- c(
    "#3B4CC0", "#E69F00", "#009E73", "#CC79A7",
    "#D55E00", "#56B4E9", "#F0E442", "#999999"
  )
  
  make_panel <- function(df) {
    ggplot(df, aes(
      x = clr_mean,
      y = fct_rev(factor(paste(predictor, contrast, sep = ": "))),
      fill = predictor
    )) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_violin(
        aes(group = paste(predictor, contrast)),
        fill = "gray90", color = "gray70",
        width = 0.6, scale = "width", alpha = 0.6
      ) +
      geom_point(
        aes(fill = predictor),
        shape = 21,
        color = "white",  # white outline
        stroke = 0.2,
        size = 1.0,
        alpha = 0.25,               # <-- half transparent points
        position = position_jitter(width = 0.01, height = 0.17)
      ) +
      scale_x_continuous(
        trans = "log2",
        breaks = c(0.25, 0.5, 0.67, 0.8, 1, 1.25, 1.5, 2, 4),
        labels = c("0.25", "0.5", "0.67", "0.8", "1", "1.25", "1.5", "2", "4")
      ) +
      scale_fill_manual(values = cbPalette) +
      labs(x = "Individual likelihood ratio", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 13),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)
      )
  }
  
  # --- Build panels ---
  p_cont <- clr_summary_df %>%
    filter(type == "continuous") %>%
    make_panel()
  
  p_cat <- clr_summary_df %>%
    filter(type == "categorical") %>%
    make_panel()
  
  # --- Apply bold tag theme to each subplot individually ---
  p_cont <- p_cont +
    labs(tag = "A") +
    theme(plot.tag = element_text(face = "bold", size = 16, family = "sans"))
  
  p_cat <- p_cat +
    labs(tag = "B") +
    theme(plot.tag = element_text(face = "bold", size = 16, family = "sans"))
  
  # --- Combine panels for different outputs ---
  
  # Horizontal layout (for saving)
   plot_horizontal <- p_cont | p_cat
   plot_horizontal <- plot_horizontal + plot_annotation()
  
   # Vertical layout (for Quarto display)
   plot_vertical <- p_cont / p_cat
   plot_vertical <- plot_vertical + plot_annotation()
    
  # --- Save ---
  if (!is.null(save_path)) {
    ggsave(save_path, plot_horizontal, width = 13, height = 11, dpi = 600)
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
    ggplot2::ggsave(save_path, plot_horizontal, width = 13, height = 6.5, dpi = 600)
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