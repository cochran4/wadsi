# =============================================================================
# compute_causal_relative_risks.R
#
# Description:
#   Estimates individual and population causal relative risks using posterior
#   predictions from a fitted BART model.
#
#   For each predictor:
#     * Continuous: compares 90th vs. 10th percentiles, using pre-imputation summaries
#     * Categorical: compares each non-reference level to a reference level
#
#   For each contrast, the function:
#     1. Constructs two counterfactual datasets with the predictor set to x and x'
#     2. Predicts Pr(Y = 1 | X, Z, S = 1) under each scenario across posterior draws
#     3. Forms a model-based odds ratio for each individual and draw
#     4. Uses these individual contrasts, together with π(Z), to construct a
#        population-level summary
#
#   The justification for using this odds ratio to approximate the causal
#   relative risk, including the required assumptions, is given in the protocol.
#
# Inputs:
#   - bart_fit: fitted dbarts::bart model
#   - analytic_df: analytic data set used for model fitting
#   - config: configuration list containing predictors, outcome, and
#             population_priors
#   - predictor_stats_df: pre-imputation mean and SD for continuous predictors
#
# Output:
#   A list containing:
#     * irr_summary_df: per-person posterior mean individual relative risk
#     * weighted_summary: posterior mean and 95% interval for the
#                         population-level summary
# =============================================================================
compute_causal_relative_risks <- function(bart_fit, analytic_df, config, predictor_stats_df) {

  # --- Setup ---------------------------------------------------------------
  # Outcome variable and observed outcomes in the analytic sample
  outcome_var <- config$outcome$name
  y_vec <- analytic_df[[outcome_var]]
  
  # Outcome probabilities in the sample (S = 1) and in the target population
  pY1_S1 <- mean(y_vec == 1, na.rm = TRUE)
  pY0_S1 <- 1 - pY1_S1
  pY1 <- config$population_priors$pY1
  pY0 <- config$population_priors$pY0
  
  # Obtain predicted probabilities and clamp them away from 0 and 1
  get_probs <- function(fit, newdata, eps = 1e-6) {
    p <- predict(fit, newdata = newdata)
    pmin(pmax(p, eps), 1 - eps)
  }
  
  # Compute π(Z), the weight used to move from the sampled data to the target population
  compute_weights <- function(p_ref) {
    num <- p_ref * pY1 / pY1_S1
    denom <- p_ref * pY1 / pY1_S1 + (1 - p_ref) * pY0 / pY0_S1
    num / denom
  }
  
  # Use row index as a unique identifier for individuals
  id_vec <- seq_len(nrow(analytic_df))
  
  # Predictor definitions from the configuration
  predictors <- config$predictors
  
  # Collect individual- and population-level summaries across predictors
  irr_summary_all <- list()
  weighted_summary_all <- list()
  
  # --- Main loop ------------------------------------------------------------
  for (pred in predictors) {
    var <- pred$name
    type <- pred$type
    message("Processing predictor: ", var, " (", type, ")")

    # For each predictor, define a contrast x versus x' and evaluate the
    # fitted model under both settings, holding all other variables fixed.
    # We then form a model-based odds ratio for each individual and posterior draw.
    
    if (type == "continuous") {
      # --- Continuous predictor ---------------------------------------------
      
      # Retrieve the pre-imputation mean and standard deviation
      stats_row <- predictor_stats_df[predictor_stats_df$predictor == var, ]
      
      # Define the contrast: 90th vs. 10th percentile
      high <- stats_row$q90
      low  <- stats_row$q10
      contrast <- "90th vs. 10th percentile"
      
      # Create two counterfactual data sets that differ only in this predictor
      x_high <- analytic_df
      x_high[[var]] <- high
      
      x_low <- analytic_df
      x_low[[var]] <- low
      
      # Predict Pr(Y = 1 | X, Z, S = 1) under each counterfactual setting
      p_high <- get_probs(bart_fit, x_high)
      p_low  <- get_probs(bart_fit, x_low)
      
      # Convert predicted probabilities to odds and form the model-based contrast
      odds_high <- p_high / (1 - p_high)
      odds_low  <- p_low  / (1 - p_low)
      irr_hat_draws <- odds_high / odds_low
      
      # Weight by π(Z), evaluated under the reference condition
      weights <- compute_weights(p_low)
      weighted_irr <- rowSums(irr_hat_draws * weights) / rowSums(weights)
      
      # Store posterior mean individual relative risk for each person
      irr_summary_all[[length(irr_summary_all) + 1]] <- data.frame(
        id = id_vec,
        predictor = var,
        contrast = contrast,
        posterior_mean_irr = colMeans(irr_hat_draws),
        posterior_median_irr = apply(irr_hat_draws, 2, median)
      )
      
      # Store posterior summaries of the population-level contrast
      weighted_summary_all[[length(weighted_summary_all) + 1]] <- data.frame(
        predictor = var,
        contrast = contrast,
        posterior_mean = mean(weighted_irr),
        posterior_median = median(weighted_irr),
        ci_lower = as.numeric(stats::quantile(weighted_irr, 0.025)),
        ci_upper = as.numeric(stats::quantile(weighted_irr, 0.975))
      )
      
    } else if (type == "categorical") {
      # --- Categorical variable ---------------------------------------------
      
      # Observed levels of the predictor
      vals <- factor(analytic_df[[var]])
      levs <- levels(vals)
      
      # Choose the most common non-missing level as the reference, if possible
      valid_levels <- setdiff(levs, c("Missing"))
      ref <- if (length(valid_levels) > 0) {
        names(sort(table(vals[vals %in% valid_levels]), decreasing = TRUE))[1]
      } else {
        names(sort(table(vals), decreasing = TRUE))[1]
      }

      # Compare each non-reference level to the reference
      others <- setdiff(levs, ref)
      
      # Loop over contrasts (each non-reference category vs reference)
      for (alt in others) {
        contrast <- paste0(alt, " vs ", ref)
        
        # Construct counterfactual data sets that differ only in this predictor
        x_ref <- analytic_df
        x_ref[[var]] <- factor(ref, levels = levs)
        x_alt <- analytic_df
        x_alt[[var]] <- factor(alt, levels = levs)

        # Predict Pr(Y = 1 | X, Z, S = 1) under each category assignment
        p_ref <- get_probs(bart_fit, x_ref)
        p_alt <- get_probs(bart_fit, x_alt)
        
        # Convert predicted probabilities to odds and form the model-based contrast
        odds_ref <- p_ref / (1 - p_ref)
        odds_alt <- p_alt / (1 - p_alt)
        irr_hat_draws <- odds_alt / odds_ref
        
        # Weight by π(x', Z), evaluated under the reference condition
        weights <- compute_weights(p_ref)
        weighted_irr <- rowSums(irr_hat_draws * weights) / rowSums(weights)
        
        # Store posterior mean individual relative risk for each person
        irr_summary_all[[length(irr_summary_all) + 1]] <- data.frame(
          id = id_vec,
          predictor = var,
          contrast = contrast,
          posterior_mean_irr = colMeans(irr_hat_draws),
          posterior_median_irr = apply(irr_hat_draws, 2, median)
        )
        
        # Store posterior summaries of the population-level contrast
        weighted_summary_all[[length(weighted_summary_all) + 1]] <- data.frame(
          predictor = var,
          contrast = contrast,
          posterior_mean = mean(weighted_irr),
          posterior_median = median(weighted_irr),
          ci_lower = as.numeric(stats::quantile(weighted_irr, 0.025)),
          ci_upper = as.numeric(stats::quantile(weighted_irr, 0.975))
        )
      }
    }
  }
  
  # --- Combine and return ---------------------------------------------------
  list(
    irr_summary_df = dplyr::bind_rows(irr_summary_all),
    weighted_summary = dplyr::bind_rows(weighted_summary_all)
  )
}


# =============================================================================
# compute_attributable_fractions.R
#
# Description:
#   Estimate individual and population attributable fractions (AF, PAF)
#   using a fitted BART model, where the reference exposure pattern x*
#   is constructed from low-risk predictor values implied by the
#   estimated causal relative risks.
#
#   Steps:
#     1. Use BART to get posterior draws of Pr(Y = 1 | X, Z, S = 1)
#        for each person under their observed predictors (p_obs).
#     2. Construct a reference exposure pattern x* using low-risk values
#        implied by the estimated causal relative risks:
#          - Continuous predictors: choose the 10th or 90th percentile,
#            depending on the estimated direction of risk.
#          - Categorical predictors: choose the level with the lowest
#            estimated risk.
#     3. Build a counterfactual dataset where all predictors X are set
#        to these reference values, keeping Z fixed for each person.
#     4. Use BART to get Pr(Y = 1 | X = x*, Z, S = 1) (p_ref).
#     5. For each posterior draw, compute
#          AF_i^(m) = 1 - p_ref_i^(m) / p_obs_i^(m).
#     6. Summarize:
#          - Individual AF: posterior mean and posterior median AF_i across draws.
#          - Population AF: average AF_i^(m) over cases (Y = 1),
#            summarized by the posterior mean, posterior median, and 95% credible interval.
#
# Inputs:
#   bart_fit    : fitted dbarts::bart model (binary outcome, probit link).
#   analytic_df : data.frame used to fit BART (after preprocessing/imputation).
#   config      : list with at least:
#       - outcome$name      : name of outcome variable (0/1).
#       - predictors        : list of predictors (each with $name and $type).
#
# Output:
#   list(
#     af_summary_df = data.frame(id, af_mean, af_median),
#     paf_summary   = data.frame(posterior_mean, posterior_median, ci_lower, ci_upper),
#     ref_predictors = data.frame with constructed reference predictor values
#     p_ref          = probabilities under reference predictors
#   )
# =============================================================================
compute_attributable_fractions <- function(bart_fit, analytic_df, config, irr_results, predictor_stats_df) {
  
  # --- Basic setup ----------------------------------------------------------
  outcome_var <- config$outcome$name
  y_vec <- analytic_df[[outcome_var]]
  
  # Helper probability function
  get_probs <- function(fit, newdata, eps = 1e-6) {
    p <- predict(fit, newdata = newdata)
    pmin(pmax(p, eps), 1 - eps)
  }
  
  # Predictor names and types
  pred_names <- vapply(config$predictors, `[[`, character(1), "name")
  pred_types <- vapply(config$predictors, `[[`, character(1), "type")
  names(pred_types) <- pred_names
  
  # Predictors that have missingness indicators
  base_missing_vars <- config$missingness_indicators
  if (is.null(base_missing_vars)) base_missing_vars <- character(0)
  
  # --- Step 1: probabilities under observed predictors ----------------------
  p_obs <- get_probs(bart_fit, analytic_df)
  
  # --- Step 2: construct counterfactual data with X = x* --------------------
  rr_df <- irr_results$weighted_summary
  x_ref_df <- analytic_df
  
  # Initialize reference values
  ref_predictors <- x_ref_df[1, pred_names, drop = FALSE]
  
  for (vn in pred_names) {
    
    # Skip missingness indicators themselves
    if (grepl("^miss_", vn)) next
    
    message("Processing predictor: ", vn)
    
    # Type of present predictor
    pred_type <- pred_types[[vn]]
   
    # --- Continuous predictors ---
    if (pred_type == "continuous") {
      # Use the estimated causal relative risk (posterior median)
      # to determine the direction of association:
      #   - RR > 1: higher values increase risk → use lower (10th percentile)
      #   - RR < 1: higher values decrease risk → use higher (90th percentile)
      rr_val  <- rr_df$posterior_median[rr_df$predictor == vn][1]
      
      # Pre-imputation percentiles defining the contrast
      q10_val <- predictor_stats_df$q10[predictor_stats_df$predictor == vn]
      q90_val <- predictor_stats_df$q90[predictor_stats_df$predictor == vn]
      
      # Choose the lower-risk value to construct the reference exposure
      ref_val <- if (rr_val > 1) q10_val else q90_val      
      
      # Replace values
      x_ref_df[[vn]] <- ref_val
      
      # If this predictor has a missingness indicator, set it to 0 under the
      # intervention so the counterfactual predictor profile is coherent:
      # the predictor now has a defined value and is no longer treated as missing.
      if (vn %in% base_missing_vars) {
        miss_var <- paste0("miss_", vn)
        x_ref_df[[miss_var]] <- 0
        ref_predictors[[miss_var]] <- 0
      }
      
      # Store predictor value
      ref_predictors[[vn]] <- ref_val
    }
    
    # --- Categorical predictors ---
    if (pred_type == "categorical") {
      
      # Extract relative risk summaries for this predictor
      pred_rr <- rr_df[rr_df$predictor == vn, , drop = FALSE]
      
      # Will store a mapping: level → relative risk (vs reference)
      level_rr <- c()
      
      # Loop over each contrast (e.g., "levelA vs levelB")
      for (j in seq_len(nrow(pred_rr))) {
        
        # Parse contrast string into "left vs right"
        parts <- strsplit(trimws(pred_rr$contrast[j]), "\\s+vs\\s+")[[1]]
        if (length(parts) != 2) next
        
        left  <- parts[1]   # comparison level
        right <- parts[2]   # reference level
        rr    <- pred_rr$posterior_median[j]
        
        # Assign RR = 1 to the reference level (if not already set)
        if (!(right %in% names(level_rr))) level_rr[right] <- 1
        
        # Assign RR for the comparison level relative to the reference
        level_rr[left] <- rr
        
      }
      
      # Remove "Missing" level if present (not a meaningful intervention target)
      level_rr <- level_rr[!names(level_rr) %in% "Missing"]
      
      # Choose the lowest-risk category as the reference exposure (x')
      ref_level <- names(level_rr)[which.min(level_rr)]
      
      # Assign the lowest-risk category to everyone
      x_ref_df[[vn]] <- factor(ref_level, levels = levels(x_ref_df[[vn]]))

      # Store reference value
      ref_predictors[[vn]] <- factor(ref_level, levels = levels(x_ref_df[[vn]]))      
    }
  }
  
  # --- Step 3: probabilities under reference predictors ---------------------
  p_ref <- get_probs(bart_fit, x_ref_df)
  
  # --- Step 4: individual AF per draw ---------------------------------------
  af_sN <- 1 - (p_ref / p_obs)
  af_mean <- colMeans(af_sN)
  af_median <- apply(af_sN, 2, median)
  
  # --- Step 5: population AF (cases only) -----------------------------------
  case_idx <- which(y_vec == 1)
  paf_draws <- rowMeans(af_sN[, case_idx, drop = FALSE])
  
  # Summarize population attributable fraction across posterior draws
  paf_summary <- data.frame(
    posterior_mean   = mean(paf_draws),           # average effect across draws
    posterior_median = median(paf_draws),         # robust central tendency
    ci_lower         = quantile(paf_draws, 0.025),# lower bound of 95% credible interval
    ci_upper         = quantile(paf_draws, 0.975) # upper bound of 95% credible interval
  )
  
  list(
    af_mean        = af_mean,        # per-individual posterior mean AF
    af_median      = af_median,      # per-individual posterior median AF
    paf_summary    = paf_summary,    # population-level AF summary
    ref_predictors = ref_predictors, # reference exposure pattern x' used in analysis
    p_ref          = p_ref           # probabilities under reference predictors
  )
}

# =============================================================================
# compute_shapley_af.R
#
# Description:
#   For each sampled individual i, define the coalition value
#
#       v_i(S) = E_donor[ 1 - p(x^*, Z_i) / p(X_{i,S}, X_{-S}^{donor}, Z_i) ],
#
#   where predictors in S are set to the individual's observed values,
#   predictors not in S are taken from donor rows sampled from the empirical
#   distribution of X, and Z_i is held fixed.
#
#   The function computes a permutation-based Shapley decomposition of
#
#       v_i(full) - v_i(empty),
#
#   so each predictor's contribution represents the average change in the
#   attributable-fraction functional when that predictor is moved from the
#   donor baseline to the individual's observed value.
#
#   For each sampled individual i:
#     1. Draw random permutations of the substantive predictors in X.
#     2. For each permutation, draw donor rows from the empirical distribution
#        of X.
#     3. Starting from the empty coalition, progressively overwrite donor
#        columns with the individual's observed predictor values according
#        to the permutation order.
#     4. At each step, compute the change in the value function
#            v_i(S) = E_donor[ AF_i(X_S observed, X_-S donor, Z_i) ].
#     5. Average marginal contributions over permutations to approximate
#        Shapley contributions for each posterior draw.
#
#   Population-level Shapley values:
#     - For autism cases only, average posterior Shapley draws across cases.
#     - Then summarize across posterior draws.
#
# Inputs:
#   bart_fit      : fitted dbarts::bart model
#   analytic_df   : data frame used to train the model (after imputation)
#   config        : list with $outcome, $predictors, etc.
#   p_ref         : counterfactual predictions for the reference profile x^*
#   n_individuals : number of individuals for Shapley computation
#   n_samples     : number of random permutations per individual
#   n_donors      : number of donor rows used to approximate the expectation
#                   over unassigned predictors
#
# Output:
#   list with elements:
#     - individual_feature_shapley
#     - individual_af_components
#     - population_feature_shapley
#     - population_af_components
# =============================================================================
compute_shapley_af <- function(bart_fit,
                               analytic_df,
                               config,
                               p_ref,
                               n_individuals = 5,
                               n_samples = 100,
                               n_donors  = 50,
                               n_cores   = 1L) {
  
  # ---------------------------------------------------------------------------
  # 1. Basic setup
  # ---------------------------------------------------------------------------
  # Outcome name
  outcome_var <- config$outcome$name
  
  # All predictor columns used by the model, including any miss_* indicators
  all_X_names <- vapply(config$predictors, `[[`, character(1), "name")
  
  # Shapley players are the substantive predictors (exclude miss_* indicators)
  player_names <- all_X_names[!grepl("^miss_", all_X_names)]
   
  # Z variables are everything not in the model predictors or outcome.
  Z_names <- setdiff(names(analytic_df), c(all_X_names, outcome_var))
  
  # Number of substantive predictors
  n_pred <- length(player_names)

  # Number of observations/persons
  n_obs  <- nrow(analytic_df)
  
  # Helper: posterior predicted probabilities, clipped away from 0 and 1
  # to avoid instability in ratios such as p_ref / p_obs.
  get_probs <- function(fit, newdata, eps = 1e-6) {
    p <- predict(fit, newdata = newdata)   # draws x nrow(newdata)
    pmin(pmax(p, eps), 1 - eps)
  }
  
  # Split data in X, Z, and outcome
  X_df <- analytic_df[, all_X_names, drop = FALSE]
  Z_df <- analytic_df[, Z_names, drop = FALSE]
  y_vec <- analytic_df[[outcome_var]]

  # ---------------------------------------------------------------------------
  # 2. Sample individuals first
  # ---------------------------------------------------------------------------

  idx <- sample(seq_len(n_obs), size = min(n_individuals, n_obs), replace = FALSE)
  
  # ---------------------------------------------------------------------------
  # 3. Reference scenario p(x^*, Z) for selected individuals only
  # ---------------------------------------------------------------------------

  p_ref_draws_sel <- p_ref[, idx, drop = FALSE]
  n_draws <- nrow(p_ref_draws_sel)
  
  # ---------------------------------------------------------------------------
  # 4. Helper: compute Shapley quantities for one sampled individual
  # ---------------------------------------------------------------------------
  #
  # This helper returns all quantities needed later:
  #   - feature-level Shapley summaries
  #   - individual-level baseline/full summaries
  #   - full posterior draw objects needed for population-level aggregation
  #
  # The argument k indexes the sampled subset:
  #   - i = idx[k] is the row index in the original analytic data
  #   - p_ref_draws_sel[, k] are the corresponding reference probabilities
  # ---------------------------------------------------------------------------
  
  compute_one_individual <- function(k) {
    
    # i is the row index in the original data.
    # k is the position within the sampled subset.
    i <- idx[k]
    
    # Reference probabilities for this sampled individual only.
    p_ref_i <- p_ref_draws_sel[, k]
    
    # Observed predictor and covariate values for individual i.
    x_i <- X_df[i, , drop = FALSE]
    z_i <- Z_df[i, , drop = FALSE]
    
    # Repeat observed values across donor rows for easy column replacement.
    x_i_rep <- x_i[rep.int(1L, n_donors), , drop = FALSE]
    z_i_rep <- z_i[rep.int(1L, n_donors), , drop = FALSE]
    
    # Posterior Shapley draws for this individual:
    #   rows    = posterior draws
    #   columns = predictors
    phi_mat <- matrix(0, nrow = n_draws, ncol = n_pred)
    colnames(phi_mat) <- player_names
    
    # Store baseline and full values.
    # These accumulate across permutations and are averaged later.
    v_empty_avg <- numeric(n_draws)
    v_full_avg  <- numeric(n_draws)
    
    # -------------------------------------------------------------------------
    # 4a. Monte Carlo over random permutations
    # -------------------------------------------------------------------------
    
    for (s in seq_len(n_samples)) {
      
      # Random order in which predictors are added
      perm <- sample(player_names, size = n_pred, replace = FALSE)
      
      # Draw donor rows from the empirical distribution of X
      donor_idx <- sample(seq_len(n_obs), size = n_donors, replace = TRUE)
      donors <- X_df[donor_idx, , drop = FALSE]
      
      # -----------------------------------------------------------------------
      # Value at the empty coalition
      # -----------------------------------------------------------------------
      #
      # At S = empty set, all predictors in X come from donor rows, while Z is
      # fixed at the observed value for individual i.
      # -----------------------------------------------------------------------
      
      df_work <- cbind(donors, z_i_rep)
      p_obs_empty <- get_probs(bart_fit, df_work)   # draws x n_donors
      
      # Value function for empty set
      v_prev_draw <- rowMeans(1 - p_ref_i / p_obs_empty)
      
      # Accumulate empty-set value for this permutation
      v_empty_avg <- v_empty_avg + v_prev_draw
      
      # -----------------------------------------------------------------------
      # Walk through the permutation
      # -----------------------------------------------------------------------
      
      for (j in seq_along(perm)) {
        
        nm <- perm[j]
        
        # Once nm enters the coalition, replace that donor column with the
        # individual's observed value and keep it fixed thereafter.
        df_work[[nm]] <- x_i_rep[[nm]]
        
        # If nm has a corresponding miss_* indicator, move it together
        miss_var <- paste0("miss_", nm)
        if (miss_var %in% names(df_work)) {
          df_work[[miss_var]] <- x_i_rep[[miss_var]]
        }
        
        # Current coalition data: assigned predictors use x_i, unassigned
        # predictors use donors, and Z stays fixed at z_i.
        p_obs_curr <- get_probs(bart_fit, df_work)   # draws x n_donors
        
        # Average AF across donor rows for each posterior draw.
        v_curr_draw <- rowMeans(1 - p_ref_i / p_obs_curr)
        
        # Marginal contribution of nm in this permutation.
        phi_mat[, nm] <- phi_mat[, nm] + (v_curr_draw - v_prev_draw)
        
        # Update previous draw value function
        v_prev_draw <- v_curr_draw
      }
      
      # Store value of full set
      v_full_avg <- v_full_avg + v_prev_draw
    }
    
    # Average over permutations to obtain the Monte Carlo Shapley approximation.
    phi_mat <- phi_mat / n_samples
    v_empty_avg <- v_empty_avg / n_samples
    v_full_avg  <- v_full_avg  / n_samples
    
    # -------------------------------------------------------------------------
    # 5. Summarize individual-level Shapley values
    # -------------------------------------------------------------------------
    
    shapley_df <- data.frame(
      id             = i,
      feature        = player_names,
      shapley_mean   = colMeans(phi_mat),
      shapley_median = apply(phi_mat, 2, median)
    )
    
    # Store individual-level baseline values
    value_df <- data.frame(
      id              = i,
      v_empty_mean    = mean(v_empty_avg),
      v_empty_median  = median(v_empty_avg),
      v_full_mean     = mean(v_full_avg),
      v_full_median   = median(v_full_avg)
    )
    
    # Return everything needed downstream.
    list(
      id          = i,
      is_case     = (y_vec[i] == 1),
      phi_mat     = phi_mat,
      v_empty_avg = v_empty_avg,
      v_full_avg  = v_full_avg,
      shapley_df  = shapley_df,
      value_df    = value_df
    )
  }
  
  # ---------------------------------------------------------------------------
  # 6. Compute individual results, optionally in parallel
  # ---------------------------------------------------------------------------
  #
  # Each sampled individual can be processed independently, so the natural
  # place to parallelize is over k = 1, ..., length(idx).
  #
  # On macOS, parallel::mclapply() works and is convenient. When n_cores = 1,
  # we fall back to lapply() for easier debugging and reproducibility.
  # ---------------------------------------------------------------------------
  
  if (n_cores > 1L) {
    results_list <- parallel::mclapply(
      X = seq_along(idx),
      FUN = compute_one_individual,
      mc.cores = n_cores
    )
  } else {
    results_list <- lapply(seq_along(idx), compute_one_individual)
  }
  
  # ---------------------------------------------------------------------------
  # 7. Combine individual-level results
  # ---------------------------------------------------------------------------
  
  shap_individual <- dplyr::bind_rows(lapply(results_list, `[[`, "shapley_df"))
  shap_individual_values <- dplyr::bind_rows(lapply(results_list, `[[`, "value_df"))
  
  # ---------------------------------------------------------------------------
  # 8. Population-level summaries
  # ---------------------------------------------------------------------------
  #
  # Population-level quantities are computed by averaging posterior draw
  # objects across sampled autism cases only.
  # ---------------------------------------------------------------------------
  
  case_results <- Filter(function(x) isTRUE(x$is_case), results_list)
  n_cases_used <- length(case_results)
  
  if (n_cases_used > 0) {
    
    pop_phi_mat <- Reduce(`+`, lapply(case_results, `[[`, "phi_mat")) / n_cases_used
    pop_v_empty <- Reduce(`+`, lapply(case_results, `[[`, "v_empty_avg")) / n_cases_used
    pop_v_full  <- Reduce(`+`, lapply(case_results, `[[`, "v_full_avg")) / n_cases_used
    
    shap_population <- data.frame(
      feature        = player_names,
      mean_shapley   = colMeans(pop_phi_mat),
      median_shapley = apply(pop_phi_mat, 2, median),
      lower          = apply(pop_phi_mat, 2, stats::quantile, probs = 0.025),
      upper          = apply(pop_phi_mat, 2, stats::quantile, probs = 0.975),
      n_cases_used   = n_cases_used
    )
    
    population_values <- data.frame(
      mean_v_empty    = mean(pop_v_empty),
      median_v_empty  = median(pop_v_empty),
      lower_v_empty   = stats::quantile(pop_v_empty, probs = 0.025),
      upper_v_empty   = stats::quantile(pop_v_empty, probs = 0.975),
      mean_v_full     = mean(pop_v_full),
      median_v_full   = median(pop_v_full),
      lower_v_full    = stats::quantile(pop_v_full, probs = 0.025),
      upper_v_full    = stats::quantile(pop_v_full, probs = 0.975),
      n_cases_used    = n_cases_used
    )
    
  } else {
    
    shap_population <- data.frame(
      feature        = player_names,
      mean_shapley   = NA_real_,
      median_shapley = NA_real_,
      lower          = NA_real_,
      upper          = NA_real_,
      n_cases_used   = 0L
    )
    
    population_values <- data.frame(
      mean_v_empty    = NA_real_,
      median_v_empty  = NA_real_,
      lower_v_empty   = NA_real_,
      upper_v_empty   = NA_real_,
      mean_v_full     = NA_real_,
      median_v_full   = NA_real_,
      lower_v_full    = NA_real_,
      upper_v_full    = NA_real_,
      n_cases_used    = 0L
    )
  }
  
  # ---------------------------------------------------------------------------
  # 8. Return
  # ---------------------------------------------------------------------------
  
  list(
    individual_feature_shapley = shap_individual,
    individual_af_components   = shap_individual_values,
    population_feature_shapley = shap_population,
    population_af_components   = population_values
  )
}