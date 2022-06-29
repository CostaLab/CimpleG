
param_checkup <- function(
  k_folds,
  param_p,
  quantile_threshold,
  grid_n
){

  # Check cv params
  assertthat::assert_that(is.numeric(k_folds))
  assertthat::assert_that(k_folds > 0)
  # Check CimpleG params
  assertthat::assert_that(is.numeric(param_p))
  assertthat::assert_that(is.numeric(quantile_threshold))
  assertthat::assert_that(
    param_p > 0 & param_p %% 2 == 0,
    msg = "param_p is not a positive even integer."
  )
  assertthat::assert_that(quantile_threshold > 0 && quantile_threshold < 1)
  # Check ML params
  assertthat::assert_that(grid_n > 0)

  return(NULL)
}

method_param_checkup <- function(method,pred_type,rank_method){

  # Check method params
  selected_method <- match.arg(
    method,
    choices = c(
      # simple models
      "CimpleG", "CimpleG_parab", "CimpleG_unscaled", "brute_force", "oner",
      # complex models
      "logistic_reg", "decision_tree", "boost_tree", "mlp", "rand_forest",
      "null_model"
    )
  )

  selected_pred_type <- match.arg(
    pred_type, choices = c("both", "hypo", "hyper")
  )
  selected_rank_method <- match.arg(
    rank_method, choices = c("ac_rank", "a_rank", "c_rank")
  )

  return(
    list(
      method = selected_method,
      pred_type = selected_pred_type,
      rank_method = selected_rank_method
    )
  )
}

