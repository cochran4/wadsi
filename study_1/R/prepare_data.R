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
  # Start with a copy of the raw data
  coerced_df <- raw_df
  
  # Keep only variables listed in config
  vars_to_keep <- c(
    unlist(lapply(config$predictors, function(x) x$name)),
    unlist(lapply(config$covariates, function(x) x$name)),
    config$outcome$name
  )
  coerced_df <- coerced_df[, intersect(vars_to_keep, names(coerced_df)), drop = FALSE]
  
  # Build a named vector mapping variable names to their intended types
  type_map <- c(
    setNames(vapply(config$predictors, `[[`, character(1), "type"),
             vapply(config$predictors, `[[`, character(1), "name")),
    setNames(vapply(config$covariates, `[[`, character(1), "type"),
             vapply(config$covariates, `[[`, character(1), "name")),
    setNames(config$outcome$type, config$outcome$name)
  )
  
  # Coerce each variable to its specified type
  for (v in names(type_map)) {
    if (!v %in% names(coerced_df)) next
    coerced_df[[v]] <- switch(
      type_map[[v]],
      continuous  = as.numeric(coerced_df[[v]]),
      categorical = as.factor(coerced_df[[v]]),
      stop("Unsupported type: ", type_map[[v]])
    )
  }
  
  # Convert outcome to 0/1 if categorical
  if (config$outcome$type == "categorical") {
    f <- relevel(factor(as.character(coerced_df[[config$outcome$name]])),
                 ref = as.character(config$outcome$reference))
    coerced_df[[config$outcome$name]] <- as.integer(f) - 1
  }
  
  # Identify continuous predictors for summary stats
  continuous_predictors <- vapply(
    config$predictors,
    function(x) if (x$type == "continuous") x$name else NA_character_,
    character(1)
  )
  continuous_predictors <- na.omit(continuous_predictors)
  
  # Compute mean and SD for continuous predictors
  predictor_stats <- lapply(continuous_predictors, function(var) {
    vals <- coerced_df[[var]]
    data.frame(
      predictor = var,
      mean = mean(vals, na.rm = TRUE),
      sd   = sd(vals, na.rm = TRUE)
    )
  })
  predictor_stats_df <- do.call(rbind, predictor_stats)
  
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
#   This function handles missing data by:
#     * Adding explicit "Missing" levels to categorical (factor) variables.
#     * Creating binary missingness indicators (miss_<var>) for continuous variables.
#   It also updates the configuration object to include new indicator variables
#   for continuous features only (categorical variables are modified in place).
#
# Inputs:
#   - coerced_df: data.frame output from process_and_summarize()
#   - config: configuration list containing:
#       * predictors: list of variables with 'name' and 'type'
#       * covariates: list of variables with 'name' and 'type'
#       * missingness_indicators: character vector of variable names to process
#
# Output:
#   - updated dataset with missingness indicators included
#
# Example:
#   indicated_df <- add_missingness_indicators(result$coerced_df, config)
#   summary(indicated_df)
#
# -----------------------------------------------------------------------------

add_missingness_indicators <- function(coerced_df, config) {
  # Copy input data
  indicated_df <- coerced_df
  
  # Check that we have variables to process
  if (is.null(config$missingness_indicators) || length(config$missingness_indicators) == 0) {
    message("No missingness indicators specified in config.")
    return(list(
      data = coerced_df,
      config = config,
      data_summary = summary(coerced_df)
    ))
  }
  
  # ---- Process each variable listed ----
  for (var in config$missingness_indicators) {
    if (!var %in% names(indicated_df)) {
      warning(paste("Variable", var, "not found in dataset. Skipping."))
      next
    }
    
    # Find its type from config
    var_type <- NULL
    for (group in c("predictors", "covariates")) {
      match <- vapply(config[[group]], `[[`, character(1), "name") == var
      if (any(match)) {
        var_type <- config[[group]][[which(match)]]$type
        break
      }
    }
    
    # Skip if type could not be determined
    if (is.null(var_type)) {
      warning(paste("Variable", var, "not found in predictors or covariates. Skipping."))
      next
    }
    
    # --- Continuous variable: create binary indicator + impute 0 ---
    if (var_type == "continuous") {
      miss_name <- paste0("miss_", var)
      indicated_df[[miss_name]] <- as.integer(is.na(indicated_df[[var]]))
      indicated_df[[var]][is.na(indicated_df[[var]])] <- 0
      
      # Add to config under the same section
      entry <- list(name = miss_name, type = "binary")
      if (var %in% sapply(config$predictors, `[[`, "name")) {
        config$predictors <- append(config$predictors, list(entry))
      } else if (var %in% sapply(config$covariates, `[[`, "name")) {
        config$covariates <- append(config$covariates, list(entry))
      }
      
      # --- Categorical variable: add "Missing" level ---
    } else if (var_type == "categorical") {
      indicated_df[[var]] <- forcats::fct_na_value_to_level(indicated_df[[var]], level = "Missing")
    } else {
      warning(paste("Unsupported variable type for", var))
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
#   This function performs single imputation of missing data using the `mice`
#   package. It removes the outcome temporarily (so it is not imputed), converts
#   any character variables to factors, performs imputation, and then re-adds
#   the outcome to the completed dataset.
#
#   The function is designed to be used after adding missingness indicators,
#   so the dataset (`indicated_df`) should already have appropriate structure.
#
# Inputs:
#   - indicated_df: data.frame output from add_missingness_indicators()
#   - config: configuration list containing at least:
#       * outcome: list with element 'name'
#
# Output:
#   - imputed dataset with the outcome re-attached
#
# Notes:
#   - Uses a single imputation (m = 1) for simplicity.
#   - Set `print = FALSE` to silence MICE output if desired.
#   - You can easily modify to return all m imputations if needed.
#
# Example:
#   imputed_df <- impute_missing_data(indicated_df, config)
#   summary(imputed_df)
#
# -----------------------------------------------------------------------------

impute_missing_data <- function(indicated_df, config) {
  # Ensure required package is available
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("The 'mice' package is required. Please install it with install.packages('mice').")
  }
  
  # Copy data
  imputed_df <- indicated_df
  
  # ---- Temporarily remove outcome ----
  outcome_name <- config$outcome$name
  if (!outcome_name %in% names(imputed_df)) {
    stop("Outcome variable not found in dataset.")
  }
  
  outcome <- imputed_df[[outcome_name]]
  imputed_df[[outcome_name]] <- NULL
  
  # ---- Convert character columns to factors ----
  char_vars <- sapply(imputed_df, is.character)
  if (any(char_vars)) {
    imputed_df[char_vars] <- lapply(imputed_df[char_vars], factor)
  }
  
  # ---- Perform imputation ----
  # m = 1: single imputation
  # printFlag controls whether mice progress output is shown
  mice_result <- mice::mice(imputed_df, m = 1, printFlag = TRUE)
  imputed_df <- mice::complete(mice_result)
  
  # ---- Add outcome back ----
  imputed_df[[outcome_name]] <- outcome
  
  # ---- Return imputed data and summary ----
  imputed_df
}

# -----------------------------------------------------------------------------
# finalize_analytic_dataset.R
#
# Description:
#   This function prepares the final analytic dataset for modeling.
#   It removes any rows with missing outcome values and updates the
#   configuration object accordingly.
#
# Inputs:
#   - imputed_df: data.frame output from impute_missing_data()
#   - config: configuration list containing at least:
#       * outcome: list with 'name'
#       * predictors: list of variables with 'name' and 'type'
#       * covariates: list of variables with 'name' and 'type'
#
# Output:
#   A list containing:
#       * data: cleaned analytic dataset ready for modeling
#       * config: updated configuration (categorical vars kept as factors)
#       * data_summary: summary of the final dataset
#
# Example:
#   analytic_df <- finalize_analytic_dataset(imputed_df, config)
#   summary(analytic_df)
#
# -----------------------------------------------------------------------------

finalize_analytic_dataset <- function(imputed_df, config) {
  analytic_df <- imputed_df
  
  # ---- Drop rows with missing outcomes ----
  outcome_name <- config$outcome$name
  n_missing <- sum(is.na(analytic_df[[outcome_name]]))
  
  if (n_missing > 0) {
    analytic_df <- analytic_df[!is.na(analytic_df[[outcome_name]]), ]
    cat(sprintf("Dropped %d row%s with missing outcome (%s).\n",
                n_missing,
                ifelse(n_missing == 1, "", "s"),
                outcome_name))
  }
  
  # ---- Return final dataset and updated config ----
  analytic_df
}