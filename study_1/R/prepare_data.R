# -----------------------------------------------------------------------------
# coerce_variable_types.R
#
# Description:
#   This function prepares an analytic dataset from a raw data frame (`raw_df`)
#   and a configuration object (`config`). It:
#     1. Keeps only predictors, covariates, and the outcome.
#     2. Coerces variables to the correct type (numeric or factor).
#     3. Converts the outcome to 0/1 only if categorical (for modeling).
#     4. Computes mean and SD for continuous predictors.
#     5. Returns both the processed dataset and summary statistics.
#
# Design choice:
#   Categorical predictors and covariates are kept as factors to preserve
#   labeling and ensure correct handling by R’s modeling functions. Only the
#   outcome is binarized to 0/1 when needed for logistic or binary models.
#
# Inputs:
#   - raw_df:   Original data frame containing all study variables.
#   - config:   A list with:
#       * predictors: list of lists with elements 'name' and 'type'
#       * covariates: list of lists with elements 'name' and 'type'
#       * outcome: list with elements 'name', 'type', and 'reference' (if categorical)
#
# Output:
#   A list containing:
#       * coerced_df: coerced analytic dataset
#       * predictor_stats_df: mean and SD for continuous predictors
#
# Example usage:
#   result <- coerce_variable_types(raw_df, config)
#   result$coerced_df
#   result$predictor_stats_df
#
# -----------------------------------------------------------------------------

coerce_variable_types <- function(raw_df, config) {

  # Variables used in the analysis
  vars_to_keep <- c(
    vapply(config$predictors, `[[`, character(1), "name"),
    vapply(config$covariates, `[[`, character(1), "name"),
    config$outcome$name
  )
  
  # Subset data
  coerced_df <- raw_df[, vars_to_keep, drop = FALSE]
  
  # Map variable names to types
  type_map <- c(
    setNames(
      vapply(config$predictors, `[[`, character(1), "type"),
      vapply(config$predictors, `[[`, character(1), "name")
    ),
    setNames(
      vapply(config$covariates, `[[`, character(1), "type"),
      vapply(config$covariates, `[[`, character(1), "name")
    ),
    setNames(config$outcome$type, config$outcome$name)
  )
  
  # Coerce variables
  for (v in names(type_map)) {
    coerced_df[[v]] <- switch(
      type_map[[v]],
      continuous  = as.numeric(coerced_df[[v]]),
      categorical = as.factor(coerced_df[[v]]),
      stop("Unsupported type for ", v, ": ", type_map[[v]])
    )
  }
  
  # Convert binary categorical outcome to 0/1
  if (config$outcome$type == "categorical") {
    outcome_name <- config$outcome$name
    ref <- as.character(config$outcome$reference)
    f <- relevel(factor(as.character(coerced_df[[outcome_name]])), ref = ref)
    coerced_df[[outcome_name]] <- as.integer(f) - 1
  }
  
  # Collect the continuous variable names
  continuous_predictors <- vapply(
    config$predictors,
    function(x) x$name,
    character(1)
  )
  types <- vapply(
    config$predictors,
    function(x) x$type,
    character(1)
  )
  continuous_predictors <- continuous_predictors[types == "continuous"]
  
  # Compute mean and SD for continuous predictors
  predictor_stats_df <- if (length(continuous_predictors) > 0) {
    do.call(rbind, lapply(continuous_predictors, function(v) {
      data.frame(
        predictor = v,
        mean = mean(coerced_df[[v]], na.rm = TRUE),
        sd = sd(coerced_df[[v]], na.rm = TRUE)
      )
    }))
  } else {
    data.frame(predictor = character(), mean = numeric(), sd = numeric())
  }
  rownames(predictor_stats_df) <- NULL
  
  # Return processed data and predictor statistics
  list(
    coerced_df = coerced_df,
    predictor_stats_df = predictor_stats_df
  )
}


# -----------------------------------------------------------------------------
# add_missingness_indicators.R
#
# Description:
#   This function handles missingness for selected variables listed in
#   config$missingness_indicators.
#
#   For continuous variables, it:
#     1. Creates a binary missingness indicator named miss_<var>
#     2. Replaces missing values with 0
#
#   For categorical variables, it:
#     1. Adds "Missing" as an explicit factor level
#
#   The configuration object is updated only for continuous variables,
#   since their new missingness indicators must be included in downstream
#   modeling.
#
# Inputs:
#   - coerced_df: data frame after type coercion
#   - config: configuration list containing predictors, covariates, and
#             missingness_indicators
#
# Output:
#   A list containing:
#     * indicated_df: updated data frame
#     * config: updated configuration
#
# -----------------------------------------------------------------------------
add_missingness_indicators <- function(coerced_df, config) {

  # Copy input data
  indicated_df <- coerced_df
  
  # Nothing to do
  if (is.null(config$missingness_indicators) || length(config$missingness_indicators) == 0) {
    message("No missingness indicators specified in config.")
    return(list(
      indicated_df = indicated_df,
      config = config
    ))
  }
  
  # Process each selected variable
  for (var in config$missingness_indicators) {
    
    # Determine whether the variable is a predictor or covariate, and get its type
    var_type <- NULL
    var_group <- NULL
    
    for (group in c("predictors", "covariates")) {
      var_names <- vapply(config[[group]], `[[`, character(1), "name")
      match_idx <- which(var_names == var)
      
      if (length(match_idx) > 0) {
        var_type <- config[[group]][[match_idx[1]]]$type
        var_group <- group
        break
      }
    }
    
    # Continuous variable: add indicator and replace missing values with 0
    if (var_type == "continuous") {
      
      # Create name for new missingness indicator
      miss_name <- paste0("miss_", var)
      
      # Add in missingness indicator
      indicated_df[[miss_name]] <- as.integer(is.na(indicated_df[[var]]))
      indicated_df[[var]][is.na(indicated_df[[var]])] <- 0
      
      # Add indicator to the same section of the config
      entry <- list(name = miss_name, type = "categorical")
      config[[var_group]] <- append(config[[var_group]], list(entry))
      
      # Categorical variable: add explicit Missing level
    } else if (var_type == "categorical") {
      indicated_df[[var]] <- forcats::fct_na_value_to_level(
        indicated_df[[var]],
        level = "Missing"
      )
      
    } else {
      warning("Unsupported variable type for ", var, ": ", var_type)
    }
  }
  
  # ---- Return updated dataset and config ----
  list(
    indicated_df = indicated_df,
    config = config
  )
}

# -----------------------------------------------------------------------------
# impute_missing_data.R
#
# Description:
#   Performs single imputation using the `mice` package. The outcome is removed
#   before imputation and added back afterward, so only predictors and
#   covariates are imputed.
#
#   Designed to be used after adding missingness indicators.
#
# Inputs:
#   - indicated_df: data frame after missingness handling
#   - config: configuration list containing at least:
#       * outcome: list with element 'name'
#
# Output:
#   - imputed data frame with outcome reattached
#
# Notes:
#   - Uses single imputation (m = 1).
#   - Relies on `mice` default methods for each variable type.
#
# -----------------------------------------------------------------------------

impute_missing_data <- function(indicated_df, config) {

  # Copy data
  imputed_df <- indicated_df
  
  # Grab outcome name
  outcome_name <- config$outcome$name
  
  # Remove outcome before imputation
  outcome <- imputed_df[[outcome_name]]
  imputed_df[[outcome_name]] <- NULL
  
  # If nothing is missing, return early
  if (!anyNA(imputed_df)) {
    imputed_df[[outcome_name]] <- outcome
    return(imputed_df)
  }
  
  # Perform single imputation
  mice_result <- mice::mice(imputed_df, m = 1, printFlag = FALSE)
  imputed_df <- mice::complete(mice_result, 1)
  
  # Re-attach outcome
  imputed_df[[outcome_name]] <- outcome
  
  # Return imputed data
  imputed_df
}

# -----------------------------------------------------------------------------
# finalize_analytic_dataset.R
#
# Description:
#   Prepares the final analytic dataset by removing rows with missing outcome
#   values.
#
# Inputs:
#   - imputed_df: data frame output from impute_missing_data()
#   - config: configuration list containing at least:
#       * outcome: list with element 'name'
#
# Output:
#   - analytic data frame with non-missing outcomes
#
# Example:
#   analytic_df <- finalize_analytic_dataset(imputed_df, config)
#   summary(analytic_df)
#
# -----------------------------------------------------------------------------
finalize_analytic_dataset <- function(imputed_df, config) {

  # Copy data
  analytic_df <- imputed_df
  
  # Drop rows with missing outcomes
  outcome_name <- config$outcome$name
  n_missing <- sum(is.na(analytic_df[[outcome_name]]))
  
  # Print number of rows with missing outcome
  if (n_missing > 0) {
    analytic_df <- analytic_df[!is.na(analytic_df[[outcome_name]]), ]
    cat(sprintf(
      "Dropped %d row%s with missing outcome (%s).\n",
      n_missing,
      ifelse(n_missing == 1, "", "s"),
      outcome_name
    ))
  }
  
  # ---- Return final dataset and updated config ----
  analytic_df
}