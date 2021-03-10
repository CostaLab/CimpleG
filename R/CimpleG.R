#' Train a classification model using (CpGs) as features
#' for the given target data.
#'
#' @param train_data Training dataset.
#'  A matrix (s x f) with methylation data (Beta values)
#'  that will be used to train/find the predictors.
#'  Samples (s) must be in rows while features/CpGs (f) must be in columns.
#'
#' @param train_targets A data frame with the training target samples one-hot encoded.
#'  A data frame with at least 1 column,
#'  with as many rows and in the same order as `train_data`.
#'  Target columms need to be one-hot encoded, meaning that, for that column
#'  the target samples should be encoded as `1` while every other sample
#'  should be encoded as `0`.
#'
#' @param targets A string specifying the name of the column in `train_targets`
#'  to be used for training.
#'  Can be a character vector if there are several columns in `train_targets`
#'  to be used for training.
#'  If this argument is a character vector, CimpleG will search for the
#'  best predictors for each target sequentially or in parallel depending on the
#'  value of `run_parallel`
#'
#' @param test_data Testing dataset.
#'  A matrix (s x f) with methylation data (Beta values)
#'  that will be used to test the performance of the found predictors.
#'  Samples (s) must be in rows while features/CpGs (f) must be in columns.
#'  If `test_data` *OR* `test_targets` are NULL, CimpleG will generate a
#'  stratified test dataset based on `train_targets` by removing 25% of the
#'  samples from `train_data` and `train_targets`.
#'
#' @param test_targets A data frame with the testing target samples one-hot encoded.
#'  A data frame with at least 1 column,
#'  with as many rows and in the same order as `test_data`.
#'  Target columms need to be one-hot encoded, meaning that, for that column
#'  the target samples should be encoded as `1` while every other sample
#'  should be encoded as `0`.
#'  If `test_data` *OR* `test_targets` are NULL, CimpleG will generate a
#'  stratified test dataset based on `train_targets` by removing 25% of the
#'  samples from `train_data` and `train_targets`.
#'
#' @param method A string specifying the method or type of machine learning
#'  model/algorithm to be used for training.
#'  These are divided in two main groups.
#'  * The simple models (classifiers that use a single feature),
#'  `parab_scale` (default), `adhoc`, `parab` or `oner`;
#'  * the complex models (classifiers that use several features),
#'  `logistic_reg`, `decision_tree`, `boost_tree`, `mlp` or `rand_forest`.
#'
#' @param pred_type A string specifying the type of predictor/CpG to be
#'  searched for during training. Only used for simple models.
#'  One of `both` (default), `hypo` or `hyper`.
#'  If `hypo`, only hypomethylated predictors will be considered.
#'  If `hyper`, only hypermethylated predictors will be considered.
#'
#' @param engine A string specifying the
#'  machine learning engine behind `method`. Only used for complex models.
#'  Currently not in use.
#'
#' @param k_folds An integer specifying the number of folds (K) to be used
#'  in training for the stratified K-fold cross-validation procedure.
#'
#' @param n_repeats An integer specifying the number of repeats (N) to be used
#'  in training for the stratified N-repeats K-fold cross-validation procedure.
#'  Currently not in use.
#'
#' @param run_parallel A boolean, if `FALSE`, the default, it will search
#'  for predictors for multiple targets sequentially.
#'  If `TRUE` it will search for predictors for multiple targets
#'  at the same time (parallel processing) in order to save
#'  in computational time. You need to set up `future::plan()` before running
#'  this function.
#'
#' @examples
#' library("CimpleG")
#'
#' # read data
#' train_d <- readRDS(file.path("data","mock_train.RDS"))
#' test_d <- readRDS(file.path("data","mock_test.RDS"))
#' train_target <- readRDS(file.path("data","mock_train_targets.RDS"))
#' test_target <- readRDS(file.path("data","mock_test_targets.RDS"))
#'
#' # run CimpleG
#' cimpleg_result <- CimpleG(
#'   train_data = train_d,
#'   train_targets = train_target,
#'   test_data = test_d,
#'   test_targets = test_target,
#'   targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS")
#' )
#'
#' # check results
#' cimpleg_result$results
#'
#' @importFrom dplyr %>%
#' @export
CimpleG <- function(
  train_data,
  train_targets,
  targets,
  test_data=NULL,
  test_targets=NULL,
  method = c(
    "parab_scale",
    "adhoc",
    "parab",
    "oner",
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest",
    "null_model"
  ),
  pred_type = c("both","hypo","hyper"),
  engine=c("glmnet","xgboost","nnet","ranger"),
  k_folds = 10,
  grid_n=10,
  n_repeats = 1,
  run_parallel=FALSE
) {
  # TODO make some diagnostic plots

  if(is.null(test_data) | is.null(test_targets)){
    message("'test_data' or 'test_targets' is NULL.")
    message("'train_data' will be partioned to create 'test_data'.")

    split_data <- make_train_test_split(
      train_d=train_data,
      train_targets=train_targets,
      targets=targets
    )
    train_data <- split_data$train_data
    train_targets <- split_data$train_targets
    test_data <- split_data$test_data
    test_targets <- split_data$test_targets
    rm(split_data)
  }

  # Check train data
  assertthat::assert_that(all(targets %in% colnames(train_targets)))
  assertthat::are_equal(nrow(train_data), nrow(train_targets))

  # Check cv params
  assertthat::assert_that(is.numeric(k_folds))
  assertthat::assert_that(k_folds > 0)
  assertthat::assert_that(is.numeric(n_repeats))

  # Check test data
  assertthat::assert_that(all(targets %in% colnames(test_targets)))
  assertthat::are_equal(nrow(test_data), nrow(test_targets))

  # Check parallel params
  assertthat::assert_that(is.numeric(n_cores))
  assertthat::assert_that(n_cores > 0)

  # Check method params
  selected_method <- match.arg(
    method,
    choices = c(
      # simple models
      "adhoc", "parab", "parab_scale", "oner",
      # complex models
      "logistic_reg", "decision_tree", "boost_tree", "mlp", "rand_forest",
      "null_model"
    )
  )
  selected_pred_type <- match.arg(pred_type, choices = c("both","hypo","hyper"))
  assertthat::assert_that(grid_n > 0)


  is_simple_method <- selected_method %in% c("adhoc", "parab", "parab_scale", "oner")


  work_helper <- function(target) {
    train_target_vec <- factor(ifelse(
      train_targets[, target] == 1,
      "positive_class",
      "negative_class"
    ), levels = c("positive_class", "negative_class"))
    test_target_vec <- factor(ifelse(
      test_targets[, target] == 1,
      "positive_class",
      "negative_class"
    ), levels = c("positive_class", "negative_class"))

    train_data$target <- train_target_vec
    test_data$target <- test_target_vec
    message(
      paste0(
        "Training for target '",target,
        "' with '",selected_method," is starting..."
      )
    )
    if(is_simple_method){
      train_res <- do_cv(
        train_data = train_data,
        method = selected_method,
        k_folds = k_folds,
        n_repeats = n_repeats,
        pred_type = selected_pred_type,
        target_name=target
      )

      test_res <- eval_test_data(
        test_data = test_data,
        final_model = train_res$model,
        method = selected_method
      )
    }else{
      train_res <- train_general_model(
        train_data = train_data,
        k_folds = k_folds,
        model_type = selected_method,
        engine = engine,
        grid_n = grid_n,
        target_name = target
      )

      test_res <- eval_general_model(
        test_data = test_data,
        final_model = train_res
      )
    }

    return(list(
      train_res = train_res,
      test_perf = test_res
    ))
  }

  if(run_parallel){
    res <- future.apply::future_lapply(
      X = targets,
      FUN = work_helper,
      future.seed = TRUE
    ) %>% magrittr::set_names(targets)
  }else{
    res <- purrr::map(
      .x=targets,
      .f=work_helper
    ) %>% magrittr::set_names(targets)
  }

  if(is_simple_method){
    signatures = purrr::map_chr(
      res,
      function(cg_res){
        cg_res$train_res$CpG
      }
    )
    final_res <- list(signatures=signatures,results=res)
  }else{
    final_res <- list(results=res)
  }

  class(final_res) <- "CimpleG"
  return(final_res)
}
