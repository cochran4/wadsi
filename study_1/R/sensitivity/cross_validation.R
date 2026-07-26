# Assign outcome-stratified cross-validation folds.
#
# Stratification keeps approximately the same number of cases and controls in
# every fold, which stabilizes held-out log-loss in the case-control sample.
make_stratified_folds <- function(y, group, k, seed) {
  # Make the random fold assignments reproducible.
  set.seed(seed)

  # Allocate one fold number for every row in the analytic dataset.
  fold <- integer(length(y))

  # Assign cases and controls separately so that every fold has approximately
  # the same outcome composition.
  for (outcome_value in sort(unique(y))) {
    # Identify distinct original records with this outcome. One original
    # sandbox record can occur more than once after resampling.
    groups <- unique(group[y == outcome_value])

    # Distribute distinct records approximately evenly across folds, then
    # randomly permute the assignments.
    group_fold <- sample(rep(seq_len(k), length.out = length(groups)))

    # Give every resampled copy of the same original record the same fold.
    fold[y == outcome_value] <- group_fold[
      match(group[y == outcome_value], groups)
    ]
  }

  fold
}


# Run K-fold cross-validation for the reference dbarts::bart model.
#
# Each fold uses the same BART settings as the primary reference model. Folds
# run sequentially, while the chains within a fold run in parallel. Only the
# posterior mean held-out probabilities are retained; the fitted fold models
# are discarded after prediction.
reference_model_cross_validation <- function(
    analytic_df,
    outcome_var,
    group = seq_len(nrow(analytic_df)),
    k = 5L,
    nchain = 8L,
    nthread = 8L,
    nskip = 30000L,
    ndpost = 1000L,
    thin = 12L,
    seed = 20260322L
) {
  # Separate the binary outcome from the predictors used by BART.
  y <- analytic_df[[outcome_var]]
  x <- analytic_df[, setdiff(names(analytic_df), outcome_var), drop = FALSE]

  # Assign each observation to one reproducible, outcome-stratified fold.
  fold <- make_stratified_folds(y, group, k, seed)

  # Store one held-out prediction table and one performance row per fold.
  heldout_predictions <- vector("list", k)
  fold_metrics <- vector("list", k)

  # Process folds sequentially to avoid running several eight-chain models at
  # once. The chains within each dbarts fit run in parallel through nthread.
  for (fold_id in seq_len(k)) {
    message("Starting cross-validation fold ", fold_id, " of ", k, ".")

    # The current fold is held out; all remaining folds form the training set.
    test_rows <- which(fold == fold_id)
    train_rows <- which(fold != fold_id)

    # Refit the reference probit BART model using only the training rows. The
    # post-burn chain runs for ndpost * thin iterations and retains every
    # thin-th draw, leaving ndpost draws per chain.
    fit <- dbarts::bart(
      x.train = x[train_rows, , drop = FALSE],
      y.train = y[train_rows],
      x.test = x[test_rows, , drop = FALSE],
      keeptrees = FALSE,
      keeptrainfits = FALSE,
      verbose = FALSE,
      nchain = nchain,
      nthread = nthread,
      nskip = nskip,
      ndpost = ndpost * thin,
      keepevery = thin,
      seed = seed + fold_id - 1L
    )

    # yhat.test contains held-out draws on the latent probit scale. Convert
    # each draw to a probability, then average over draws for each person.
    probability <- colMeans(pnorm(fit$yhat.test))

    # Avoid log(0) when calculating held-out negative log-loss.
    probability <- pmin(pmax(probability, 1e-12), 1 - 1e-12)

    # Calculate the mean Bernoulli negative log-likelihood in this held-out
    # fold. Smaller values indicate better predictive performance.
    log_loss <- -mean(
      y[test_rows] * log(probability) +
        (1 - y[test_rows]) * log(1 - probability)
    )

    # Retain one posterior mean probability for every held-out observation.
    heldout_predictions[[fold_id]] <- data.frame(
      row = test_rows,
      fold = fold_id,
      outcome = y[test_rows],
      probability = probability
    )

    # Retain the fold size and its single held-out performance estimate.
    fold_metrics[[fold_id]] <- data.frame(
      fold = fold_id,
      heldout_n = length(test_rows),
      negative_log_loss = log_loss
    )

    message(
      "Finished fold ", fold_id,
      "; negative log-loss = ", round(log_loss, 4), "."
    )

    # The fitted fold model is no longer needed after held-out prediction.
    rm(fit)
    gc()
  }

  # Combine the five fold-specific objects into two ordinary data frames.
  heldout_predictions <- do.call(rbind, heldout_predictions)
  fold_metrics <- do.call(rbind, fold_metrics)
  rownames(heldout_predictions) <- NULL
  rownames(fold_metrics) <- NULL

  # The protocol's cross-validation summary is the unweighted mean of the five
  # fold-specific negative log-loss values.
  summary <- data.frame(
    folds = k,
    mean_negative_log_loss = mean(fold_metrics$negative_log_loss)
  )

  # Return the overall summary, fold details, held-out predictions, and the
  # sampling settings needed to reproduce the calculation.
  list(
    summary = summary,
    fold_metrics = fold_metrics,
    heldout_predictions = heldout_predictions,
    settings = list(
      k = k,
      nchain = nchain,
      nthread = nthread,
      nskip = nskip,
      ndpost = ndpost,
      thin = thin,
      seed = seed
    )
  )
}
