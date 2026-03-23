# =============================================================================
# compute_causal_relative_risks.R
#
# Description:
#   Estimates individual and population causal relative risks using posterior
#   predictions from a fitted BART model.
#
#   For each predictor:
#     * Continuous: compares +1 SD vs -1 SD, using pre-imputation summaries
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
    
    # Assert that predictions are numeric, non-missing, and on the probability scale
    if (!is.numeric(p)) {
      stop("BART predictions are not numeric.")
    }
    if (any(is.na(p))) {
      stop("BART predictions contain missing values.")
    }
    if (any(p < -1e-8 | p > 1 + 1e-8)) {
      stop("BART predictions are not on the probability scale.")
    }
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
      mu <- stats_row$mean
      sd <- stats_row$sd
      
      # Define the contrast: one standard deviation above versus below the mean
      high <- mu + sd
      low  <- mu - sd
      contrast <- "+1SD vs -1SD"
      
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
        posterior_mean_irr = colMeans(irr_hat_draws)
      )
      
      # Store posterior summaries of the population-level contrast
      weighted_summary_all[[length(weighted_summary_all) + 1]] <- data.frame(
        predictor = var,
        contrast = contrast,
        posterior_mean = mean(weighted_irr),
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

      # Choose a reference level (skip Missing/Unknown/NA if possible)
      valid_levels <- setdiff(levs, c("Missing", "Unknown", "NA"))
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
          posterior_mean_irr = colMeans(irr_hat_draws)
        )
        
        # Store posterior summaries of the population-level contrast
        weighted_summary_all[[length(weighted_summary_all) + 1]] <- data.frame(
          predictor = var,
          contrast = contrast,
          posterior_mean = mean(weighted_irr),
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
#   using a fitted BART model, where the reference exposure pattern x'
#   is taken from the person with the lowest model-based risk.
#
#   Steps:
#     1. Use BART to get posterior draws of Pr(Y = 1 | X, Z, S = 1)
#        for each person under their observed predictors (p_obs).
#     2. Identify the "reference person" as the individual with the
#        lowest posterior mean risk.
#     3. Build a counterfactual dataset where all predictors X are set
#        to that person's predictor values, keeping Z fixed for each person.
#     4. Use BART to get Pr(Y = 1 | X = x', Z, S = 1) (p_ref).
#     5. For each posterior draw, compute
#          AF_i^(m) = 1 - p_ref_i^(m) / p_obs_i^(m).
#     6. Summarize:
#          - Individual AF: posterior mean AF_i across draws.
#          - Population AF: average AF_i^(m) over cases (Y = 1),
#            with posterior mean and 95% credible interval.
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
#     af_summary_df = data.frame(id, af_mean),
#     paf_summary   = data.frame(posterior_mean, ci_lower, ci_upper),
#     ref_index     = index of reference person,
#     ref_predictors = named vector of that person's predictor values
#   )
# =============================================================================

compute_attributable_fractions <- function(bart_fit, analytic_df, config) {
  # --- Basic setup ----------------------------------------------------------
  outcome_var <- config$outcome$name
  y_vec <- analytic_df[[outcome_var]]
  
  # Helper: convert latent probit-scale output to probabilities,
  # and clip away from 0/1 to avoid division issues.
  get_probs <- function(fit, newdata, eps = 1e-6) {
    p <- predict(fit, newdata = newdata)  # S x N matrix
    pmin(pmax(p, eps), 1 - eps)
  }
  
  # IDs (if available)
  id_vec <- if ("person_id" %in% names(analytic_df)) {
    analytic_df$person_id
  } else {
    seq_len(nrow(analytic_df))
  }
  
  # Predictor names (we will treat all of these as X; Z are whatever remains)
  pred_names <- vapply(config$predictors, `[[`, character(1), "name")
  
  # --- Step 1: probabilities under observed predictors ----------------------
  # p_obs is S x N: rows = posterior draws, cols = individuals
  p_obs <- get_probs(bart_fit, analytic_df)
  
  # Posterior mean risk for each individual (average over draws)
  p_obs_mean <- colMeans(p_obs)
  
  # --- Step 2: identify reference person -----------------------------------
  # Reference = person with the lowest posterior mean risk
  ref_index <- which.min(p_obs_mean)
  
  # Extract that person's predictor values as the reference pattern x'
  ref_row <- analytic_df[ref_index, pred_names, drop = FALSE]
  ref_predictors <- as.list(ref_row[1, , drop = TRUE])
  
  # --- Step 3: construct counterfactual data with X = x' --------------------
  x_ref_df <- analytic_df
  for (vn in pred_names) {
    x_ref_df[[vn]] <- ref_row[[vn]]
  }
  
  # --- Step 4: probabilities under reference predictors ---------------------
  p_ref <- get_probs(bart_fit, x_ref_df)  # S x N
  
  cat("\n--- Diagnostics ---\n")
  cat("Range of predicted risks (observed):", range(p_obs_mean), "\n")
  cat("Reference person's risk:",
      round(p_obs_mean[ref_index], 6), "\n")
  
  p_ref_mean <- colMeans(p_ref)
  cat("Range of predicted risks (reference):", range(p_ref_mean), "\n")
  
  cat("Mean ratio p_ref/p_obs:",
      mean(p_ref_mean / p_obs_mean), "\n")
  
  # --- Step 5: individual AF per draw ---------------------------------------
  # AF_i^(m) = 1 - p_ref_i^(m) / p_obs_i^(m)
  af_sN <- 1 - (p_ref / p_obs)  # same S x N dimension
  
  # Posterior mean AF per person (average over draws)
  af_mean <- colMeans(af_sN)
  
  af_summary_df <- data.frame(
    id      = id_vec,
    af_mean = af_mean
  )
  
  # --- Step 6: population AF (cases only) -----------------------------------
  # For each draw m, average AF_i^(m) over cases (Y = 1)
  case_idx <- which(y_vec == 1)
  if (length(case_idx) == 0) {
    warning("No cases (Y = 1) in analytic_df; population AF undefined.")
    paf_draws <- NA_real_
  } else {
    # subset to columns of cases, then average across individuals for each draw
    paf_draws <- rowMeans(af_sN[, case_idx, drop = FALSE])
  }
  
  paf_summary <- data.frame(
    posterior_mean = mean(paf_draws),
    ci_lower       = quantile(paf_draws, 0.025),
    ci_upper       = quantile(paf_draws, 0.975)
  )
  
  # --- Return ---------------------------------------------------------------
  list(
    af_summary_df  = af_summary_df,
    paf_summary    = paf_summary,
    ref_index      = ref_index,
    ref_predictors = ref_predictors
  )
}


# =============================================================================
# compute_shapley_af.R  (updated)
#
# Description:
#   Decompose the attributable fraction
#       f(X) = 1 - p_ref(Z) / p_obs(X, Z)
#   into Shapley contributions for each predictor X, keeping Z fixed.
#
#   For each selected individual i:
#     - We compute a matrix phi_mat (n_draws x p), where each row is a
#       posterior draw and each column is a predictor.
#     - We then:
#         * take the column means of phi_mat as that individual's posterior
#           mean Shapley values (for reporting / plotting).
#         * (for autism cases only) accumulate phi_mat into a running
#           population-level matrix so we can recover the posterior
#           distribution of population Shapley values.
#
#   Population Shapley values:
#     - For each posterior draw m, we average phi_mat[m, ] across autism cases,
#       giving a population Shapley value per draw and predictor.
#     - We then summarize across draws to get posterior mean and 95% credible
#       intervals for each predictor.
#
# Inputs:
#   bart_fit       : fitted dbarts::bart model
#   analytic_df    : dataframe used to train model (after imputation)
#   config         : list with $outcome, $predictors, etc.
#   ref_index      : index of reference individual used to build p_ref
#   n_individuals  : number of individuals for Shapley computation
#   n_samples      : number of random permutations per individual
#   n_donors       : number of donor rows used when sampling remaining features
#
# Output: list with
#   - individual: tibble(id, feature, shapley_mean)
#   - population: tibble(feature, mean_shapley, lower, upper, n_cases_used)
# =============================================================================

compute_shapley_af <- function(bart_fit,
                               analytic_df,
                               config,
                               ref_index = NULL,
                               n_individuals = 5,
                               n_samples = 100,
                               n_donors  = 50) {
  # -----------------------------
  # Setup
  # -----------------------------
  outcome_var <- config$outcome$name
  X_names <- vapply(config$predictors, `[[`, character(1), "name")
  Z_names <- setdiff(names(analytic_df), c(X_names, outcome_var))
  
  # Helper: probit-scale predictions -> probabilities (draws x N)
  get_probs_draws <- function(fit, newdata, eps = 1e-6) {
    p <- predict(fit, newdata = newdata)   # ndpost x N
    p[p < eps]     <- eps
    p[p > 1 - eps] <- 1 - eps
    p
  }
  
  # -----------------------------
  # Reference scenario p_ref
  # -----------------------------
  if (is.null(ref_index)) {
    stop("ref_index must be supplied (index of reference individual).")
  }
  
  ref_row <- analytic_df[ref_index, X_names, drop = FALSE]
  x_ref_df <- analytic_df
  for (vn in X_names) {
    x_ref_df[[vn]] <- ref_row[[vn]]
  }
  
  # Posterior draws for reference probabilities: (draws x N)
  p_ref_draws <- get_probs_draws(bart_fit, x_ref_df)
  n_draws     <- nrow(p_ref_draws)
  p           <- length(X_names)
  
  # -----------------------------
  # Sample individuals for Shapley
  # -----------------------------
  set.seed(2025)
  all_idx <- seq_len(nrow(analytic_df))
  idx <- sample(all_idx, size = min(n_individuals, length(all_idx)))
  
  # Prepare storage:
  indiv_list <- vector("list", length(idx))  # for individual posterior means
  
  # For population-level Shapley:
  pop_phi_mat <- matrix(0, nrow = n_draws, ncol = p)
  colnames(pop_phi_mat) <- X_names
  n_cases_used <- 0L
  
  # For quick outcome lookup
  y_vec <- analytic_df[[outcome_var]]
  
  # -----------------------------
  # Main loop over individuals
  # -----------------------------
  for (k in seq_along(idx)) {
    i <- idx[k]
    
    x_i <- analytic_df[i, X_names, drop = FALSE]
    z_i <- analytic_df[i, Z_names, drop = FALSE]
    
    # Matrix of Shapley contributions per draw (rows) and predictor (cols)
    phi_mat <- matrix(0, nrow = n_draws, ncol = p)
    colnames(phi_mat) <- X_names
    
    # --- v(empty set): expected AF when all X replaced by random donors -----
    donors0 <- analytic_df[
      sample(seq_len(nrow(analytic_df)), n_donors),
      X_names, drop = FALSE
    ]
    Z_mat0 <- z_i[rep(1, nrow(donors0)), , drop = FALSE]
    df0    <- cbind(donors0, Z_mat0)
    
    p_obs0 <- get_probs_draws(bart_fit, df0)      # draws x n_donors
    AF_md_empty <- 1 - (p_ref_draws[, i] / p_obs0)  # recycle col i across donors
    v_empty_draw <- rowMeans(AF_md_empty)         # length = n_draws
    
    # --- Monte Carlo KernelSHAP-style approximation -------------------------
    for (s in seq_len(n_samples)) {
      perm <- sample(X_names)
      x_temp <- as.data.frame(matrix(NA, ncol = p, nrow = 1))
      colnames(x_temp) <- X_names
      
      # start each permutation from v(empty set)
      v_prev_draw <- v_empty_draw
      
      for (j in seq_along(perm)) {
        # include predictor perm[j] in the subset
        x_temp[[perm[j]]] <- x_i[[perm[j]]]
        
        # draw donor rows and fill in unspecified predictors
        donors <- analytic_df[
          sample(seq_len(nrow(analytic_df)), n_donors),
          X_names, drop = FALSE
        ]
        for (nm in X_names) {
          if (!is.na(x_temp[[nm]])) {
            donors[[nm]] <- x_temp[[nm]]
          }
        }
        
        # combine with fixed Z for this individual
        Z_mat <- z_i[rep(1, nrow(donors)), , drop = FALSE]
        df    <- cbind(donors, Z_mat)
        
        # posterior draws of p_obs for this subset
        p_obs_draws <- get_probs_draws(bart_fit, df)  # draws x n_donors
        
        # AF for each draw m and donor d
        AF_md <- 1 - (p_ref_draws[, i] / p_obs_draws) # draws x n_donors
        
        # value function v(S) for this subset (per draw)
        v_curr_draw <- rowMeans(AF_md)  # length = n_draws
        
        # marginal contribution of feature perm[j] for each draw
        phi_mat[, perm[j]] <- phi_mat[, perm[j]] + (v_curr_draw - v_prev_draw)
        
        # update baseline for next feature in permutation
        v_prev_draw <- v_curr_draw
      }
    }
    
    # average over permutations
    phi_mat <- phi_mat / n_samples   # still draws x predictors
    
    # posterior mean Shapley value per feature for this individual
    phi_mean <- colMeans(phi_mat)
    
    indiv_list[[k]] <- tibble::tibble(
      id           = i,
      feature      = X_names,
      shapley_mean = phi_mean
    )
    
    # accumulate into population matrix if this individual is a case
    if (y_vec[i] == 1) {
      pop_phi_mat  <- pop_phi_mat + phi_mat
      n_cases_used <- n_cases_used + 1L
    }
  }
  
  # -----------------------------
  # Combine individual results
  # -----------------------------
  shap_individual <- dplyr::bind_rows(indiv_list)
  
  # -----------------------------
  # Population Shapley distribution
  # -----------------------------
  if (n_cases_used > 0) {
    # average phi_mat over cases for each draw
    pop_phi_mat <- pop_phi_mat / n_cases_used   # draws x predictors
    
    # summarize across draws for each predictor
    mean_shapley <- colMeans(pop_phi_mat)
    lower <- apply(pop_phi_mat, 2, stats::quantile, probs = 0.025)
    upper <- apply(pop_phi_mat, 2, stats::quantile, probs = 0.975)
    
    shap_population <- tibble::tibble(
      feature      = X_names,
      mean_shapley = mean_shapley,
      lower        = lower,
      upper        = upper,
      n_cases_used = n_cases_used
    )
  } else {
    shap_population <- tibble::tibble(
      feature      = X_names,
      mean_shapley = NA_real_,
      lower        = NA_real_,
      upper        = NA_real_,
      n_cases_used = 0L
    )
  }
  
  # -----------------------------
  # Return
  # -----------------------------
  list(
    individual = shap_individual,
    population = shap_population
  )
}