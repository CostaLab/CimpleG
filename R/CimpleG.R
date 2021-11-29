#' Find simple CpG (CimpleG) signatures.
#'
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
#'  `CimpleG` (default), `brute_force`, `CimpleG_unscaled` or `oner`;
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
#' @param grid_n An integer specifying the number of hyperparameter combinations
#'  to train for.
#'
#' @param run_parallel A boolean, if `FALSE`, the default, it will search
#'  for predictors for multiple targets sequentially.
#'  If `TRUE` it will search for predictors for multiple targets
#'  at the same time (parallel processing) in order to save
#'  in computational time. You need to set up `future::plan()` before running
#'  this function.
#'
#' @param save_dir If defined it will save the resulting model to the given directory.
#'  Default is \code{NULL}.
#'
#' @param save_format Only used if \code{save_dir} is not \code{NULL}.
#'  One of "zstd", "lz4", "gzip", "bzip2","xz", "nocomp".
#'  \code{zstd} is the best option, fast compression and loading times, low space usage.
#'
#' @param verbose How verbose you want CimpleG to be while it is running.
#'  At 0, no message is displayed, at 3 every message is displayed.
#'  Default is \code{1}.
#'
#' @return A CimpleG object with the results per target class.
#'
#' @examples
#' library("CimpleG")
#'
#' # read data
#' data(train_data)
#' data(train_targets)
#' data(test_data)
#' data(test_targets)
#'
#' # run CimpleG
#' cimpleg_result <- CimpleG(
#'   train_data = train_data,
#'   train_targets = train_targets,
#'   test_data = test_data,
#'   test_targets = test_targets,
#'   method = "CimpleG",
#'   targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS")
#' )
#'
#' # check results
#' cimpleg_result$results
#'
#' @export
CimpleG <- function(
  train_data,
  train_targets = NULL,
  targets,
  test_data = NULL,
  test_targets = NULL,
  method = c(
    "CimpleG",
    "brute_force",
    "CimpleG_unscaled",
    "oner",
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest",
    "null_model"
  ),
  pred_type = c("both", "hypo", "hyper"),
  engine = c("glmnet", "xgboost", "nnet", "ranger"),
  k_folds = 10,
  grid_n = 10,
  n_repeats = 1,
  run_parallel = FALSE,
  save_dir = NULL,
  save_format = c("zstd", "lz4", "gzip", "bzip2","xz", "nocomp"),
  verbose=1
) {

  if(inherits(train_data, "SummarizedExperiment")){
    prep_train <- prep_data(train_data,targets)
    train_data <- prep_train$beta_mat
    train_targets <- prep_train$df_targets
    rm(prep_train)

    if(!is.null(test_data) && inherits(test_data, "SummarizedExperiment")){
      prep_test <- prep_data(test_data,targets)
      test_data <- prep_test$beta_mat
      test_targets <- prep_test$df_targets
      rm(prep_test)
    }
  }
  
  # Check train data
  assertthat::assert_that(length(targets) > 0)
  assertthat::assert_that(all(targets %in% colnames(train_targets)))
  assertthat::assert_that(
    assertthat::are_equal(nrow(train_data), nrow(train_targets))
  )

  if(nrow(train_data) > ncol(train_data)){
    warning(
      paste0(
        "There are more samples (", nrow(train_data), ")",
        " than features (", ncol(train_data), ").", "\n",
        "This might be a sign that you forgot to transpose your train_data."
      )
    )
  }

  # Make sure samples for the given target exist for training (!=0)
  purrr::walk(targets, function(target){
    samples_in_target <- sum(as.integer(train_targets[, target]))
    assertthat::assert_that(
      samples_in_target > 0,
      msg = paste0(
        "Target class: '", target, "' has 0 samples to train for.", "\n",
        "Make sure samples for the given target exist for training"
      )
    )
    return(NULL)
  })

  # Check cv params
  assertthat::assert_that(is.numeric(k_folds))
  assertthat::assert_that(k_folds > 0)
  assertthat::assert_that(is.numeric(n_repeats))
  # Check method params
  selected_method <- match.arg(
    method,
    choices = c(
      # simple models
      "CimpleG", "CimpleG_unscaled", "brute_force", "oner",
      # complex models
      "logistic_reg", "decision_tree", "boost_tree", "mlp", "rand_forest",
      "null_model"
    )
  )
  selected_pred_type <- match.arg(
    pred_type, choices = c("both", "hypo", "hyper")
  )
  assertthat::assert_that(grid_n > 0)

  if(is.null(test_data) | is.null(test_targets)){
    if(verbose>=2){
      message("'test_data' or 'test_targets' is NULL.") 
      message("'train_data' will be partioned to create 'test_data'.")
    }

    split_data <- make_train_test_split(
      train_d = train_data,
      train_targets = train_targets,
      targets = targets
    )
    train_data <- split_data$train_data
    train_targets <- split_data$train_targets
    test_data <- split_data$test_data
    test_targets <- split_data$test_targets
    rm(split_data)
  }

  # Check test data
  assertthat::assert_that(all(targets %in% colnames(test_targets)))
  assertthat::assert_that(
    assertthat::are_equal(nrow(test_data), nrow(test_targets))
  )

  is_simple_method <- selected_method %in% c(
    "CimpleG", "brute_force", "CimpleG_unscaled", "oner"
  )

  train_data <- as.data.frame(train_data)
  test_data <- as.data.frame(test_data)


  work_helper <- function(target) {

    start_time <- Sys.time()

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

    rv_tbl <- table(train_data[, "target"])

    if (k_folds > rv_tbl[which.min(rv_tbl)]) {
      k_folds <- rv_tbl[which.min(rv_tbl)]
      warning(paste0("Too few samples for set K in cross-validation for target ",target_name))
      warning(paste0("K folds reset to k=", k_folds))
    }

    if(is_simple_method){
      train_res <- do_cv(
        train_data = train_data,
        method = selected_method,
        k_folds = k_folds,
        n_repeats = n_repeats,
        pred_type = selected_pred_type,
        target_name = target,
        verbose = verbose
      )

      test_res <- eval_test_data(
        test_data = test_data,
        final_model = train_res$model,
        method = selected_method,
        verbose = verbose
      )
    }else{
      train_res <- train_general_model(
        train_data = train_data,
        k_folds = k_folds,
        model_type = selected_method,
        engine = engine,
        grid_n = grid_n,
        target_name = target,
        verbose = verbose
      )

      test_res <- eval_general_model(
        test_data = test_data,
        final_model = train_res,
        verbose = verbose
      )
    }

    elapsed_time <- Sys.time() - start_time

    return(list(
      train_res = train_res,
      test_perf = test_res,
      elapsed_time = elapsed_time
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
      .x = targets,
      .f = work_helper
    ) %>% magrittr::set_names(targets)
  }

  if(is_simple_method){
    signatures <- purrr::map_chr(
      res,
      function(cg_res){
        cg_res$train_res$CpG
      }
    )
    final_res <- list(signatures = signatures, results = res)
  }else{
    final_res <- list(results = res)
  }

  class(final_res) <- "CimpleG"

  if(!is.null(save_dir)){

    target_name <- ifelse(length(targets) > 1, "multitargets", targets)
    model_name <- method
    time_tag <- format(Sys.time(), "%Y%m%d-%H%M%S")
    f_name <- paste0(
      "CimpleG_results_",
      "target-", target_name,"_",
      "model-", model_name,"_",
      "t-", time_tag
    )

    save_object(
      object = final_res,
      file_name = file.path(save_dir, f_name),
      file_format = save_format
    )
  }

  return(final_res)
}

prep_data <- function(data, targets){

  is_minfi_class <- 
    class(data) %in% c("MethylSet", "GenomicMethylSet", "RGChannelSet", "GenomicRatioSet")

  is_sumexp_class <- is(data) %in% "SummarizedExperiment"

  if(is_minfi_class & requireNamespace("minfi", quietly = TRUE)){

    beta_mat <- minfi::getBeta(data)
    df_targets <- minfi::pData(data)

  }else if(is_sumexp_class & requireNamespace("SummarizedExperiment", quietly = TRUE)){

    is_beta_in_assays <- tolower(SummarizedExperiment::assayNames(data)) == "beta"

    assertthat::assert_that(any(is_beta_in_assays))

    # samples as columns
    beta_mat  <- SummarizedExperiment::assays(data)[which(is_beta_in_assays)][[1]]
    df_targets <- SummarizedExperiment::colData(data)
  }else{
    abort(
      paste0(
        "Class of data provided ('",class(data),"'), is not supported.\n",
        "Please provide a SummarizedExperiment object or a ",
        "simple matrix/data frame with your data."
      )
    )
  }

  # make samples as rows
  beta_mat <- t(beta_mat)

  # make DataFrame into data.frame
  df_targets <- as.data.frame(df_targets)

  assertthat::assert_that(all(targets %in% colnames(df_targets)))
  
  # check which cols don't have their values as 0 or 1
  # these will be the ones to edit
  # cols_to edit is a named vector
  cols_to_edit <- which(sapply(
    df_targets[targets],
    function(dcols){!all(dcols %in% c(0,1))}
  ))

  # make cols to edit factor
  df_targets[names(cols_to_edit)] <- lapply(
    df_targets[names(cols_to_edit)],factor
  )

  # find in each col to edit (targets that are not 0/1) which of the values
  # is in a smaller proportion, these values will become 1
  # fetch_col will be a named vector where the values are the minority "class"
  # and the names are the name of the corresponding column
  fetch_col <- sapply(
    df_targets[names(cols_to_edit)],
    function(x){levels(x)[which.min(tabulate(x))]}
  )

  # make cols one-hot encoded
  df_targets <- as.data.frame(
    mltools::one_hot(
      dt=data.table::as.data.table(df_targets),
      cols=names(cols_to_edit),
      dropCols=FALSE
    )
  )
  # for each col that needs to be edited, fetch it and the corresponding name
  # created by one_hot
  df_targets[,names(fetch_col)] <- lapply(
    names(fetch_col),
    function(x){df_targets[,paste0(x,"_",fetch_col[x])]}
  )

  return(list(beta_mat = beta_mat, df_targets = df_targets))
}
