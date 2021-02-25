

#' Main function of the package
#' Finds the best predictors (CpGs) for the given targets
#'
#' @importFrom dplyr %>%
#' @export
CimpleG <- function(
  train_data,
  train_targets,
  targets,
  test_data=NULL,
  test_targets=NULL,
  method = c("adhoc", "parab", "parab_scale", "oner"),
  pred_type = c("both","hypo","hyper"),
  k_folds = 10,
  n_repeats = 1,
  n_cores=1
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
  assertthat::assert_that(is.numeric(n_repeats))

  # Check test data
  assertthat::assert_that(all(targets %in% colnames(test_targets)))
  assertthat::are_equal(nrow(test_data), nrow(test_targets))

  # Check parallel params
  assertthat::assert_that(is.numeric(n_cores))

  # Check method params
  method <- match.arg(method, choices = c("adhoc", "parab", "parab_scale", "oner"))
  pred_type <- match.arg(pred_type, choices = c("both","hypo","hyper"))

  if(n_cores>1){
    future::plan(future::multisession(),workers=n_cores)
    options(future.globals.maxSize=3000 * 1024 ^ 2)#3Gb
  }else{
    future::plan(future::sequential())
  }

  res <- furrr::future_map(
    .x=targets,
    .options = furrr::furrr_options(seed = TRUE),
    .f=function(target) {
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
          "Training for target '",target,"' with '",method,"(",pred_type,")' is starting..."
        )
      )
      train_res <- do_cv(
        train_data = train_data,
        method = method,
        k_folds = k_folds,
        n_repeats = n_repeats,
        pred_type = pred_type,
        target_name=target
      )

      test_res <- eval_test_data(
        test_data = test_data,
        final_model = train_res$model,
        method = method
      )
      return(list(
        train_res = train_res,
        test_perf = test_res
      ))
    }
  ) %>% magrittr::set_names(targets)

  signatures = purrr::map_chr(
    res,
    function(cg_res){
      cg_res$train_res$CpG
    }
  )

  return(list(signatures=signatures,results=res))
}


#' Main function of the package
#' Finds the best predictors (CpGs) for the given targets
#'
#' @importFrom dplyr %>%
#' @export
CimpleG_general <- function(
  train_data,
  train_targets,
  targets,
  test_data=NULL,
  test_targets=NULL,
  model_type=c(
    "logistic_reg",
    "decision_tree",
    "boost_tree",
    "mlp",
    "rand_forest",
    "null_model"
  ),
  engine=c("glmnet","xgboost","nnet","ranger"),
  k_folds = 10,
  n_repeats = 1,
  grid_n=10,
  n_cores=1
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
  assertthat::assert_that(is.numeric(n_repeats))

  # Check test data
  assertthat::assert_that(all(targets %in% colnames(test_targets)))
  assertthat::are_equal(nrow(test_data), nrow(test_targets))

  # Check parallel params
  assertthat::assert_that(is.numeric(n_cores))

  if(n_cores>1){
    future::plan(future::multisession(),workers=n_cores)
    options(future.globals.maxSize=3000 * 1024 ^ 2)#3Gb
  }else{
    future::plan(future::sequential())
  }

  res <- furrr::future_map(
    .x=targets,
    .options = furrr::furrr_options(seed = TRUE),
    .f=function(target) {
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
          "Training for target '",target,"' with '",model_type,"' is starting..."
        )
      )
      train_res <- train_general_model(
        train_data = train_data,
        k_folds = k_folds,
        model_type = model_type,
        engine = engine,
        grid_n = grid_n,
        target_name = target
      )

      test_res <- eval_general_model(
        test_data = test_data,
        final_model = train_res
      )
      return(list(
        train_res = train_res,
        test_perf = test_res
      ))
    }
  ) %>% magrittr::set_names(targets)

  return(list(results=res))
}

#' Perform deconvolution using the results from CimpleG
#' Calculate reference matrix given reference data and targets
#' Compute deconvolution on new data
#'
#' @importFrom dplyr %>%
#' @export
CimpleG_deconvolution <- function(
  CimpleG_result,
  reference_data,
  reference_targets,
  targets,
  new_data,
  method=c("nnls","epidish","nmf")
){

  # TODO needs to also work with the general sigs
  # TODO assert that signatures exist in ref and new data
  signatures = purrr::map_chr(
    CimpleG_result,
    function(cg_res){
      cg_res$train_res$CpG
    }
  )

  assertthat::assert_that(all(signatures %in% colnames(reference_data)))
  assertthat::assert_that(all(signatures %in% colnames(new_data)))


  ref_mat = compute_deconv_reference(
    signatures=signatures,
    data=reference_data,
    target_table=reference_targets,
    targets=targets
  )

  new_mat = new_data[,signatures] %>% as.matrix()

  deconvolution_nnls <- function(
    # Non-Negative Least Squares deconvolution algorithm
    weights_mat, # Features as rows, Classes as columns
    values_mat, # Features as rows, Samples as columns
    iter=2000
  ){
    #implementation of the NNLS method with update rule of LEE

    # init mat with 1s
    deconvMat <- matrix(1, ncol = ncol(values_mat), nrow = ncol(weights_mat))

    colnames(deconvMat) <- colnames(values_mat)
    rownames(deconvMat) <- colnames(weights_mat)

    tW_V <- t(weights_mat) %*% values_mat
    tW_W <- t(weights_mat) %*% weights_mat

    for(i in 1:iter){
      tW_Wdec <- tW_W %*% deconvMat
      deconvMat <- deconvMat * (tW_V / tW_Wdec)
    }

    # Adjust proportions to sum up to 1
    p_deconvMat <- apply(
      X = deconvMat,
      MARGIN = 2,
      FUN = function(x){x/sum(x)}
    )

    if(any(is.na(p_deconvMat))){
      message("### Careful! Deconvolution result has NA values.")
      message("This is probably due to features/probes being missing in the input matrix 'values_mat'")
    }

    return(p_deconvMat)
  }

  print(ref_mat[1:5,1:5])
  print(t(new_mat)[1:5,1:5])

  deconv_res = deconvolution_nnls((ref_mat),t(new_mat))
  return(deconv_res)
}
