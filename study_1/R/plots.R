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
plot_individual_rr <- function(irr_summary_df,
                               config,
                               predictor_stats_df = NULL,
                               save_path = NULL,
                               save_path_vertical = NULL) {
  
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
  
  # Add predictor type
  irr_summary_df <- irr_summary_df %>%
    dplyr::left_join(predictor_type_df, by = "predictor")
  
  # Add predictor mean/sd for continuous predictors, if supplied
  if (!is.null(predictor_stats_df)) {
    irr_summary_df <- irr_summary_df %>%
      dplyr::left_join(
        predictor_stats_df %>%
          dplyr::select(predictor, mean, sd),
        by = "predictor"
      )
  } else {
    irr_summary_df <- irr_summary_df %>%
      dplyr::mutate(mean = NA_real_, sd = NA_real_)
  }
  
  # --- Identify truly binary categorical predictors ---
  # A predictor is treated as binary if its contrast labels contain only 0/1
  # values and do not contain any larger category labels like 2 or 3.
  binary_predictors <- irr_summary_df %>%
    dplyr::filter(type == "categorical") %>%
    dplyr::mutate(
      contrast_trim = trimws(as.character(contrast))
    ) %>%
    dplyr::group_by(predictor) %>%
    dplyr::summarise(
      is_binary =
        all(grepl("\\b[01]\\b", contrast_trim)) &&
        !any(grepl("\\b[2-9]\\b", contrast_trim)),
      .groups = "drop"
    )
  
  
    # --- Attach binary indicator back to main data ---
    irr_summary_df <- irr_summary_df %>%
    dplyr::left_join(binary_predictors, by = "predictor") %>%  # add is_binary flag
    
    # Replace NA (non-categorical predictors) with FALSE
    # so downstream logic does not accidentally treat them as binary
    dplyr::mutate(is_binary = dplyr::coalesce(is_binary, FALSE))
  
  # --- Clean contrast labels for categorical predictors ---
  # For binary predictors only, convert 0/1 labels to No/Yes.
  # For all other categorical predictors, leave the label unchanged.
    clean_categorical_contrast <- function(x, is_binary) {
      x <- trimws(as.character(x))
      if (is_binary) {
        x <- gsub("\\b1\\b", "Yes", x)
        x <- gsub("\\b0\\b", "No", x)
      }
      x
    }
  
  
  # --- Clean contrast labels for continuous predictors ---
  # When predictor means and SDs are available, replace generic labels
  # like "-1 SD vs +1 SD" with the corresponding numeric values.
  clean_continuous_contrast <- function(contrast, mean = NA_real_, sd = NA_real_) {
    
    # If mean and SD are available, show the implied low/high values
    if (!is.na(mean) && !is.na(sd)) {
      low  <- round(mean - sd)
      high <- round(mean + sd)
      return(paste0(high, " vs ", low))
    }
    
    # Otherwise fall back to the original contrast label
    as.character(contrast)
  }
  
  # --- Build display labels for plotting ---
  # Create readable predictor and contrast labels for the y-axis
  irr_summary_df <- irr_summary_df %>%
    dplyr::rowwise() %>%  # operate row-by-row (needed for custom label functions)
    dplyr::mutate(
      
      # Replace raw predictor names with human-readable labels (if provided)
      predictor_label = dplyr::if_else(
        !is.null(label_map) && predictor %in% names(label_map),
        unname(label_map[predictor]),
        predictor
      ),
      
      # Wrap long predictor names to avoid overly wide labels
      predictor_label = stringr::str_wrap(predictor_label, width = 20),
      
      # Clean contrast labels:
      #   - continuous: use mean ± SD (or fallback)
      #   - categorical: clean 0/1 → No/Yes only when appropriate
      contrast_clean = dplyr::case_when(
        type == "continuous" ~ clean_continuous_contrast(contrast, mean, sd),
        TRUE ~ clean_categorical_contrast(contrast, is_binary)
      ),
      
      # Wrap contrast labels for readability
      contrast_clean = stringr::str_wrap(contrast_clean, width = 18),
      
      # Combine predictor and contrast into a single multi-line label
      display_label = paste0(predictor_label, "\n", contrast_clean)
    ) %>%
    dplyr::ungroup()  # return to standard (non-rowwise) behavior
  
  
  # --- Color palette (Okabe-Ito; colorblind-friendly) ---
  # Ensures consistent coloring across predictors
  cb_palette <- c(
    "#3B4CC0", "#E69F00", "#009E73", "#CC79A7",
    "#D55E00", "#56B4E9", "#F0E442", "#999999"
  )
  # --- Define a common x-axis for both panels ---
  # Compute the overall range of individual relative risks across all predictors
  x_range <- range(irr_summary_df$posterior_mean_irr, na.rm = TRUE)
  
  # Add a small padding so points/violins do not sit on the plot boundaries
  pad_factor <- 0.05
  x_limits <- c(
    x_range[1] / (1 + pad_factor),  # slightly below minimum
    x_range[2] * (1 + pad_factor)   # slightly above maximum
  )
  
  # --- Define interpretable tick marks on log2 scale ---
  # These correspond to halving/doubling and common intermediate values
  x_breaks <- c(0.25, 0.5, 0.67, 1, 1.5, 2, 4)
  
  # Keep only breaks that fall within the computed limits
  # (ensures consistent but not cluttered axes across panels)
  x_breaks <- x_breaks[
    x_breaks >= x_limits[1] & x_breaks <= x_limits[2]
  ]
  
  make_panel <- function(df, panel_tag = NULL, title = NULL) {
    
    # --- Base plot: map data to aesthetics ---
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = posterior_mean_irr,                          # x-axis: individual relative risk
        y = forcats::fct_rev(factor(display_label)),     # y-axis: predictor + contrast (top-to-bottom)
        fill = predictor_label                           # fill used for grouping (not shown in legend)
      )
    ) +
      
      # --- Reference line (no effect) ---
      ggplot2::geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = "gray50",
        linewidth = 0.6
      ) +
      
      # --- Distribution layer (violin) ---
      # Shows spread of individual relative risks within each predictor/contrast
      ggplot2::geom_violin(
        ggplot2::aes(group = display_label),
        fill = "gray90",
        color = "gray70",
        width = 0.7,
        scale = "width",
        alpha = 0.7
      ) +
      
      # --- Individual estimates (points) ---
      # Each dot is one person's posterior mean IRR
      ggplot2::geom_point(
        shape = 21,
        color = "white",
        stroke = 0.25,
        size = 1.4,
        alpha = 0.30,
        position = ggplot2::position_jitter(width = 0.01, height = 0.17)
      ) +
      
      # --- X-axis scaling (log2) ---
      # Allows interpretation in terms of doubling / halving
      ggplot2::scale_x_continuous(
        trans = "log2",
        limits = x_limits,
        breaks = x_breaks,
        labels = scales::label_number()
      ) +
      
      # --- Color palette ---
      ggplot2::scale_fill_manual(values = cb_palette) +
      
      # --- Labels and titles ---
      ggplot2::labs(
        x = "Individual relative risk",
        y = NULL,
        title = title
      ) +
      
      # --- Theme and styling ---
      ggplot2::theme_minimal(base_size = 20) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        
        # Axis text
        axis.text.y = ggplot2::element_text(size = 18, face = "bold"),
        axis.text.x = ggplot2::element_text(size = 18),
        
        # Axis title
        axis.title.x = ggplot2::element_text(face = "bold", size = 19),
        
        # Panel title (Continuous / Categorical)
        plot.title = ggplot2::element_text(face = "bold", size = 22, hjust = 0.5),
        
        legend.position = "none",
        plot.margin = ggplot2::margin(12, 12, 12, 12)
      )
  }
  
  # --- Build panels by predictor type ---
  
  # Continuous predictors panel
  p_cont <- irr_summary_df %>%
    dplyr::filter(type == "continuous") %>%
    make_panel(title = "Continuous predictors")

  # Categorical predictors panel
  p_cat <- irr_summary_df %>%
    dplyr::filter(type == "categorical") %>%
    make_panel(title = "Categorical predictors")
  
  # --- Combine panels into final layouts ---
  
  # Horizontal layout (side-by-side; useful for manuscripts)
  #plot_horizontal <- p_cont | p_cat
  plot_horizontal <- (p_cont | p_cat) 
  #+
  #  patchwork::plot_annotation(tag_levels = "A") &
  #  ggplot2::theme(
  #    plot.tag = ggplot2::element_text(face = "bold", size = 18),
  #    plot.tag.position = c(0, 1)
  #  )
  
  # Vertical layout (stacked; useful for presentations / Quarto)
  plot_vertical <- p_cont / p_cat
  
  
  # --- Save outputs if paths are provided ---
  
  # Save horizontal version
  if (!is.null(save_path)) {
    ggplot2::ggsave(
      save_path,
      plot_horizontal,
      width = 24,
      height = 12,
      dpi = 600
    )
  }
  
  # Save vertical version
  if (!is.null(save_path_vertical)) {
    ggplot2::ggsave(
      save_path_vertical,
      plot_vertical,
      width = 12,
      height = 18,
      dpi = 600
    )
  }
  
  # --- Return vertical plot for interactive use ---
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