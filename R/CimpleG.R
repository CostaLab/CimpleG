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
#' @param target_columns A string specifying the name of the column in `train_targets`
#'  to be used for training.
#'  Can be a character vector if there are several columns in `train_targets`
#'  to be used for training.
#'  If this argument is a character vector, CimpleG will search for the
#'  best predictors for each target sequentially or in parallel depending on the
#'  value of `run_parallel`
#'
#' @param targets DEPRECATED use `target_columns`.
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
#' @param param_p An even number in `sigma / (delta^param_p)`. Tunes how much weight will be
#'  given to delta when doing feature selection. Default is \code{2}.
#'
#' @param quantile_threshold A number between 0 and 1.
#'  Determines how many features will be kept. Default is \code{0.005}.
#'
#' @param train_only A boolean, if TRUE, CimpleG will only train (find predictors)
#'  but not test them against a test dataset.
#'
#' @param engine A string specifying the
#'  machine learning engine behind `method`. Only used for complex models.
#'  Currently not in use.
#'
#' @param k_folds An integer specifying the number of folds (K) to be used
#'  in training for the stratified K-fold cross-validation procedure.
#'
#' @param grid_n An integer specifying the number of hyperparameter combinations
#'  to train for.
#'
#' @param rank_method A string specifying the ranking strategy to rank the features during training.
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
#' @param deconvolution_reference A boolean, if `TRUE`, it will create 
#'  a deconvolution reference matrix based on the training data.
#'  This can later be used to perform deconvolution. Default is `FALSE`.
#'
#' @param split_data A boolean, if `TRUE`, it will subset the train data provided,
#'  creating a smaller test set that will be used to test the models after training.
#'  This parameter is experimental. Default is `FALSE`.
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
#'   target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS")
#' )
#'
#' # check signatures
#' cimpleg_result$signatures
#'
#' @export
CimpleG <- function(
  train_data,
  train_targets = NULL,
  target_columns = NULL,
  test_data = NULL,
  test_targets = NULL,
  method = c(
    "CimpleG",
    "CimpleG_parab",
    "brute_force",
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest"
  ),
  pred_type = c("both", "hypo", "hyper"),
  engine = c("glmnet", "xgboost", "nnet", "ranger"),
  rank_method = c("ac_rank","a_rank","c_rank"),
  k_folds = 10,
  grid_n = 10,
  param_p = 2,
  quantile_threshold = 0.005,
  train_only = FALSE,
  split_data = FALSE,
  run_parallel = FALSE,
  deconvolution_reference = TRUE,
  save_dir = NULL,
  save_format = c("zstd", "lz4", "gzip", "bzip2","xz", "nocomp"),
  verbose=1,
  targets=NULL
) {
  UseMethod("CimpleG")
}

#' @export
CimpleG.matrix <- function(
  train_data,
  train_targets = NULL,
  target_columns = NULL,
  test_data = NULL,
  test_targets = NULL,
  method = c(
    "CimpleG",
    "CimpleG_parab",
    "brute_force",
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest"
  ),
  pred_type = c("both", "hypo", "hyper"),
  engine = c("glmnet", "xgboost", "nnet", "ranger"),
  rank_method = c("ac_rank","a_rank","c_rank"),
  k_folds = 10,
  grid_n = 10,
  param_p = 2,
  quantile_threshold = 0.005,
  train_only = FALSE,
  split_data = FALSE,
  run_parallel = FALSE,
  deconvolution_reference = TRUE,
  save_dir = NULL,
  save_format = c("zstd", "lz4", "gzip", "bzip2","xz", "nocomp"),
  verbose=1,
  targets=NULL
){

  if(!is.null(targets) & is.null(target_columns)){
    warning("Parameter `targets` is deprecated, please use `target_columns` instead.")
    target_columns <- targets
  }

  param_checkup(
    k_folds = k_folds, param_p = param_p, grid_n = grid_n,
    quantile_threshold = quantile_threshold
  )

  selected_method_params <- method_param_checkup(
    method = method,
    pred_type = pred_type,
    rank_method = rank_method
  )

  input_data <- input_data_check_prep(
    train_data = train_data, train_targets = train_targets,
    test_data = test_data, test_targets = test_targets,
    target_columns = target_columns,
    train_only = train_only, split_data = split_data
  )

  CimpleG_main(
    train_data = input_data$train_data,
    train_targets = input_data$train_targets,
    test_data = input_data$test_data,
    test_targets = input_data$test_targets,
    target_columns = input_data$target_columns,
    method = selected_method_params$method,
    pred_type = selected_method_params$pred_type,
    rank_method = selected_method_params$rank_method,
    k_folds = k_folds,
    grid_n = grid_n,
    param_p = param_p,
    quantile_threshold = quantile_threshold,
    train_only = train_only,
    run_parallel = run_parallel,
    deconvolution_reference = deconvolution_reference,
    save_dir = save_dir,
    save_format = save_format,
    verbose = verbose
  )
}

#' @export
CimpleG.data.frame <- function(
  train_data,
  train_targets = NULL,
  target_columns = NULL,
  test_data = NULL,
  test_targets = NULL,
  method = c(
    "CimpleG",
    "CimpleG_parab",
    "brute_force",
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest"
  ),
  pred_type = c("both", "hypo", "hyper"),
  engine = c("glmnet", "xgboost", "nnet", "ranger"),
  rank_method = c("ac_rank","a_rank","c_rank"),
  k_folds = 10,
  grid_n = 10,
  param_p = 2,
  quantile_threshold = 0.005,
  train_only = FALSE,
  split_data = FALSE,
  run_parallel = FALSE,
  deconvolution_reference = TRUE,
  save_dir = NULL,
  save_format = c("zstd", "lz4", "gzip", "bzip2","xz", "nocomp"),
  verbose=1,
  targets=NULL
){

  if(!is.null(targets) & is.null(target_columns)){
    warning("Parameter `targets` is deprecated, please use `target_columns` instead.")
    target_columns <- targets
  }

  param_checkup(
    k_folds = k_folds, param_p = param_p, grid_n = grid_n,
    quantile_threshold = quantile_threshold
  )

  selected_method_params <- method_param_checkup(
    method = method,
    pred_type = pred_type,
    rank_method = rank_method
  )
  input_data <- input_data_check_prep(
    train_data = train_data, train_targets = train_targets,
    test_data = test_data, test_targets = test_targets,
    target_columns = target_columns,
    train_only = train_only, split_data = split_data
  )


  CimpleG_main(
    train_data = input_data$train_data,
    train_targets = input_data$train_targets,
    test_data = input_data$test_data,
    test_targets = input_data$test_targets,
    target_columns = input_data$target_columns,
    method = selected_method_params$method,
    pred_type = selected_method_params$pred_type,
    rank_method = selected_method_params$rank_method,
    k_folds = k_folds,
    grid_n = grid_n,
    param_p = param_p,
    quantile_threshold = quantile_threshold,
    train_only = train_only,
    run_parallel = run_parallel,
    deconvolution_reference = deconvolution_reference,
    save_dir = save_dir,
    save_format = save_format,
    verbose = verbose
  )
}

#' @export
CimpleG.SummarizedExperiment <- function(
  train_data,
  train_targets = NULL,
  target_columns = NULL,
  test_data = NULL,
  test_targets = NULL,
  method = c(
    "CimpleG",
    "CimpleG_parab",
    "brute_force",
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest"
  ),
  pred_type = c("both", "hypo", "hyper"),
  engine = c("glmnet", "xgboost", "nnet", "ranger"),
  rank_method = c("ac_rank","a_rank","c_rank"),
  k_folds = 10,
  grid_n = 10,
  param_p = 2,
  quantile_threshold = 0.005,
  train_only = FALSE,
  split_data = FALSE,
  run_parallel = FALSE,
  deconvolution_reference = TRUE,
  save_dir = NULL,
  save_format = c("zstd", "lz4", "gzip", "bzip2","xz", "nocomp"),
  verbose=1,
  targets=NULL
){

  if(!is.null(targets) & is.null(target_columns)){
    warning("Parameter `targets` is deprecated, please use `target_columns` instead.")
    target_columns <- targets
  }
  param_checkup(
    k_folds = k_folds, param_p = param_p, grid_n = grid_n,
    quantile_threshold = quantile_threshold
  )

  selected_method_params <- method_param_checkup(
    method = method,
    pred_type = pred_type,
    rank_method = rank_method
  )

  # if(inherits(train_data, "SummarizedExperiment")){
  prep_train <- prep_summarizedexp_data(train_data, target_columns)
  train_data <- prep_train$beta_mat
  train_targets <- prep_train$df_targets
  rm(prep_train)

  if(!is.null(test_data) && inherits(test_data, "SummarizedExperiment") && !train_only){
    prep_test <- prep_summarizedexp_data(test_data,target_columns)
    test_data <- prep_test$beta_mat
    test_targets <- prep_test$df_targets
    rm(prep_test)
  }
  # }
  input_data <- input_data_check_prep(
    train_data = train_data, train_targets = train_targets,
    test_data = test_data, test_targets = test_targets,
    target_columns = target_columns,
    train_only = train_only, split_data = split_data
  )

  CimpleG_main(
    train_data = input_data$train_data,
    train_targets = input_data$train_targets,
    test_data = input_data$test_data,
    test_targets = input_data$test_targets,
    target_columns = input_data$target_columns,
    method = selected_method_params$method,
    pred_type = selected_method_params$pred_type,
    rank_method = selected_method_params$rank_method,
    k_folds = k_folds,
    grid_n = grid_n,
    param_p = param_p,
    quantile_threshold = quantile_threshold,
    train_only = train_only,
    run_parallel = run_parallel,
    deconvolution_reference = deconvolution_reference,
    save_dir = save_dir,
    save_format = save_format,
    verbose = verbose
  )
}


CimpleG_main <- function(
  train_data,
  train_targets = NULL,
  target_columns = NULL,
  test_data = NULL,
  test_targets = NULL,
  method = c(
    "CimpleG",
    "CimpleG_parab",
    "brute_force",
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest"
  ),
  pred_type = c("both", "hypo", "hyper"),
  engine = c("glmnet", "xgboost", "nnet", "ranger"),
  rank_method = c("ac_rank","a_rank","c_rank"),
  k_folds = 10,
  grid_n = 10,
  param_p = 2,
  quantile_threshold = 0.005,
  train_only = FALSE,
  run_parallel = FALSE,
  deconvolution_reference = TRUE,
  save_dir = NULL,
  save_format = c("zstd", "lz4", "gzip", "bzip2","xz", "nocomp"),
  verbose=1
) {

  start_o_time <- Sys.time()

  # due to NSE notes in R CMD check
  id <- train_rank <- NULL

  #TODO: make the "new" method the main one!
  is_simple_method <- method %in% c(
    "CimpleG_parab","brute_force"
  )
  is_cimpleg <- method %in% "CimpleG"

  if(is_cimpleg){
    # TODO: ensure other methods can run off of data.table so that I can remove is_cimpleg
    if(is.matrix(train_data)){
      train_data <- as.data.frame(train_data)
      data.table::setDT(train_data)
    }else if(is.data.frame(train_data)){
      data.table::setDT(train_data)
    }
    if(!train_only){
      if(is.matrix(test_data)){
        test_data <- as.data.frame(test_data)
        data.table::setDT(test_data)
      }else if(is.data.frame(test_data)){
        data.table::setDT(test_data)
      }
    }
  }else{
    train_data <- as.data.frame(train_data)
    if(!train_only) test_data <- as.data.frame(test_data)
  }

  work_helper <- function(target) {

    start_time <- Sys.time()

    train_target_vec <- factor(ifelse(
      train_targets[, target] == 1,
      "positive_class",
      "negative_class"
    ), levels = c("positive_class", "negative_class"))
    if(!train_only){
      test_target_vec <- factor(ifelse(
          test_targets[, target] == 1,
          "positive_class",
          "negative_class"
          ), levels = c("positive_class", "negative_class"))
    }

    if(is_cimpleg){
      train_data[,target := train_target_vec]
      if(!train_only) test_data[,target := test_target_vec]
    }else{
      train_data$target <- train_target_vec
      if(!train_only) test_data$target <- test_target_vec
    }

    rv_tbl <- table(train_data[, "target"])

    if (k_folds > rv_tbl[which.min(rv_tbl)]) {
      k_folds <- rv_tbl[which.min(rv_tbl)]
      warning(paste0("Too few samples for set K in cross-validation for target ",target))
      warning(paste0("K folds reset to k=", k_folds))
    }

    test_res <- NULL

    if(is_simple_method){
      train_res <- do_cv(
        train_data = train_data,
        method = method,
        k_folds = k_folds,
        pred_type = pred_type,
        target_name = target,
        verbose = verbose
      )
      if(!train_only){
        test_res <- eval_test_data(
          test_data = test_data,
          final_model = train_res$train_results,
          method = method,
          verbose = verbose
        )
      }
    }else if(is_cimpleg){
      train_res <- cv_loop(
        train_data = train_data,
        target_name = target,
        k_folds = k_folds,
        pred_type = pred_type,
        param_p = param_p,
        q_threshold = quantile_threshold,
        rank_method = rank_method,
        run_parallel = run_parallel,
        verbose = verbose
      )

      if(!train_only){
        test_res <- eval_test(
          test_data = test_data,
          train_results = train_res$train_results
        )
      }
    }else{
      train_res <- train_general_model(
        train_data = train_data,
        k_folds = k_folds,
        model_type = method,
        engine = engine,
        grid_n = grid_n,
        target_name = target,
        verbose = verbose
      )

      if(!train_only){
        test_res <- eval_general_model(
          test_data = test_data,
          final_model = train_res,
          verbose = verbose
        )
      }
    }

    elapsed_time <- Sys.time() - start_time

    return(list(
      train_res = train_res,
      test_perf = test_res,
      elapsed_time = elapsed_time
    ))
  }

  if(run_parallel){
    requireNamespace("future", quietly = FALSE)
    requireNamespace("future.apply", quietly = FALSE)
    res <- future.apply::future_lapply(
      X = target_columns,
      FUN = work_helper,
      future.seed = TRUE
    ) %>% magrittr::set_names(target_columns)
  }else{
    res <- purrr::map(
      .x = target_columns,
      .f = work_helper
    ) %>% magrittr::set_names(target_columns)
  }


  o_time <- Sys.time() - start_o_time

  if(is_cimpleg | is_simple_method){
    # cimpleg | parab | brute force
    signatures <- purrr::map_chr(
      res,
      function(cg_res){
        cg_res$train_res$train_results[train_rank == 1, id]
      }
    )
    final_res <- list(
      signatures = signatures,
      results = res,
      overall_time = o_time,
      method = method
    )
  }else{
    # ML models
    final_res <- list(
      results = res,
      overall_time = o_time,
      method = method
    )
  }

  class(final_res) <- "CimpleG"


  if(deconvolution_reference){

    non_train_samples <- which(rowSums(train_targets[,target_columns])==0)
    target_vector <- names(train_targets[,target_columns])[max.col(train_targets[,target_columns])]
    target_vector[non_train_samples] <- "others"

    if(any(grepl("target",colnames(train_data),fixed=TRUE))){
      train_data <- train_data[,1:(ncol(train_data)-1)]
    }

    ref_mat  <- make_deconv_ref_matrix(
      cimpleg_res=final_res,
      ref_data=train_data,
      ref_data_label=target_vector,
      method=method
    )
    final_res <- append(final_res,ref_mat)
    class(final_res) <- "CimpleG"
  }

  if(!is.null(save_dir)){

    target_name <- ifelse(length(target_columns) > 1, "multitargets", target_columns)
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

