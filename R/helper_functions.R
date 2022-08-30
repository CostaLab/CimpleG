# Main function controlling k-fold CV training
do_cv <- function(
  train_data,
  k_folds = 10,
  method = c("CimpleG_parab", "CimpleG_unscaled", "brute_force", "oner"),
  pred_type = c("both","hypo","hyper"),
  target_name,
  verbose=1
) {

  assertthat::assert_that("target" %in% colnames(train_data))
  assertthat::assert_that(is.factor(train_data[, "target"]))


  tictoc::tic(paste0("Training for target '",target_name,"' with '",method,"' has finished."))
  # f_data <- rsample::vfold_cv(
  #   train_data,
  #   v = k_folds,
  #   strata = "target"
  # )
  # vfold_cv stratification not working properly, using caret instead
  cv_index <- caret::createFolds(
    train_data$target,
    k = k_folds,
    # if returnTrain=TRUE when k=1, returns nothing
    returnTrain = ifelse(k_folds < 2, FALSE, TRUE),
    list=TRUE
  )

  tc <- caret::trainControl(
    index = cv_index,
    indexOut = purrr::map(
      cv_index,
      function(x){setdiff(seq_len(nrow(train_data)), x)}
    ),
    method = "cv",
    number = k_folds
  )

  f_data <- rsample::caret2rsample(ctrl = tc, data = train_data)

  # adding split id within the splits
  for(i in seq_len(length(f_data$splits))){
    f_data$splits[[i]]$id <- tibble::tibble(id = sprintf("Fold%02d", i))
  }

  f_data$results <- purrr::map(
    .x = f_data$splits,
    .f = find_predictors,
    method = method,
    scale_scores = any(grepl("CimpleG_parab", method, fixed = TRUE)),
    pred_type = pred_type,
    verbose = verbose
  )
  print_timings(verbose<1)

  # FIXME pproc not working
  prroc_prauc <- yardstick::new_prob_metric(prroc_prauc, "maximize")
  class_prob_metrics <- yardstick::metric_set(
    # prroc_prauc,
    yardstick::pr_auc,
    yardstick::roc_auc,
    yardstick::accuracy,
    yardstick::f_meas
  )

  # get metrics per fold and predictor
  train_summary <- f_data$results %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(truth = stats::relevel(.data$truth, "positive_class")) %>%
    dplyr::mutate(prediction = stats::relevel(.data$prediction, "positive_class")) %>%
    dplyr::group_by(
      .data$resample,
      .data$predictor,
      .data$type,
      dplyr::across(tidyselect::any_of("parab_param")),
      dplyr::across(tidyselect::any_of("diff_means"))
    ) %>%
    class_prob_metrics(
      .data$truth,
      .data$positive_prob,
      estimate = .data$prediction
    ) %>%
    dplyr::group_by(.data$predictor, .data$type, .data$.metric)

  if(any(grepl("CimpleG",method))){
    # cimpleg parab
    train_summary <- train_summary %>%
      dplyr::summarise(
        mean_fold_performance = mean(.data$.estimate,na.rm=TRUE),
        n = length(.data$predictor),
        mean_parab_param = mean(.data$parab_param),
        diff_means = mean(.data$diff_means),
        .groups = "drop"
      )
    train_summary <- train_summary %>%
      dplyr::mutate(
        mean_fold_performance = (
          scales::rescale(abs(.data$diff_means), to=c(0.1, 1)) * .data$mean_fold_performance
        )
      ) %>%
      dplyr::mutate(
        mean_fold_performance = (
          scales::rescale(.data$n, to=c(0.1, 1)) * .data$mean_fold_performance
        )
      )
  }else{
    # brute force
    train_summary <- train_summary %>%
      dplyr::summarise(
        mean_fold_performance = mean(.data$.estimate,na.rm=TRUE),
        n = length(.data$predictor),
        .groups = "drop"
      )
  }

  train_summary <- train_summary %>%
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$mean_fold_performance))

  # sort by predictor that appears in more folds and best mean fold perf
  train_results <- train_summary %>%
    dplyr::filter(.data$.metric == "pr_auc") %>%
    dplyr::arrange(dplyr::desc(.data$n), dplyr::desc(.data$mean_fold_performance)) %>%
    dplyr::mutate(train_rank=1:nrow(.),id=.data$predictor)
  # dplyr::select(predictor,type)
  # dplyr::pull(predictor)

  #   return(list(
  #     CpG=best_pred$predictor,
  #     model=best_pred,
  #     train_summary=train_summary
  #   ))
  #   return(list(
  #       fold_id=rsample::tidy(f_data),
  #       train_summary=train_summary,
  #       dt_dmsv=dt_dmsv,
  #       train_results=train_results
  #   ))
  return(list(
    fold_id=rsample::tidy(f_data),
    train_summary=train_summary,
    train_results=data.table::as.data.table(train_results)
  ))
}

#' Evaluation of produced models on test data
#' @param test_data Test data.
#' @param final_model Model to be tested.
#' @param method Method used to train model.
#' @param verbose How verbose the logs should be.
#' @export
eval_test_data <- function(
  test_data,
  final_model,
  method = "oner",
  verbose = 1
) {
  if(verbose>=2){ message("Evaluating test data...") }

  # get performance on test data
  test_data$target <- test_data$target %>% stats::relevel("positive_class")

  predictor_name <- final_model %>% dplyr::filter(.data$train_rank==1) %>% dplyr::pull(.data$id)
  is_hypo <- final_model %>% dplyr::filter(.data$train_rank==1) %>% dplyr::pull(.data$type) %>% {. == "hypo"}

  pred_prob <- test_data %>% dplyr::pull(predictor_name)

  pred_prob <- if(is_hypo){ 1 - pred_prob }else{ pred_prob }

  pred_class <- factor(
    ifelse(
      pred_prob > 0.5,
      "positive_class",
      "negative_class"
      ),
    levels = levels(test_data$target)
  )

  acc_perf <- yardstick::accuracy_vec(
    truth = test_data$target,
    estimate = pred_class
  )

  f1_perf <- yardstick::f_meas_vec(
    truth = test_data$target,
    estimate = pred_class
  )

  aupr_perf <- yardstick::pr_auc_vec(
    truth = test_data$target,
    estimate = pred_prob
  )

  res <- data.frame(
    method = method,
    predictor = predictor_name,
    accuracy = acc_perf,
    f1 = f1_perf,
    aupr = aupr_perf
  )
  return(res)
}


eval_test <- function(
  test_data,
  train_results,
  verbose=1
){

  target <- pred_type <- id <- stat_origin <- NULL

  if(verbose>=2) message("Evaluating test data...")

  # get performance on test data
  test_data$target <- test_data$target %>% stats::relevel("positive_class")

  hyper_aupr <- test_data[,
    lapply(X = .SD, true_v = target, FUN = function(x, true_v){
      prroc_prauc_vec(estimate = x, truth = true_v)
    }),
    .SDcols = train_results[pred_type == TRUE, id]]

  hypo_aupr <- test_data[,
    lapply(X = .SD, true_v = target, FUN = function(x, true_v){
      prroc_prauc_vec(estimate = 1 - x, truth = true_v)
    }),
    .SDcols = train_results[pred_type == FALSE, id]]

  if(length(hyper_aupr) == 0){
    hyper_aupr <- data.table::data.table(
      id = character(), test_aupr = double(), key = "id"
    )
  }else{
    hyper_aupr <- data.table::transpose(hyper_aupr, keep.names = "id")
    data.table::setnames(hyper_aupr, "V1", "test_aupr")
  }
  if(length(hypo_aupr) == 0){
    hypo_aupr <- data.table::data.table(
      id = character(), test_aupr = double(), key = "id"
    )
  }else{
    hypo_aupr <- data.table::transpose(hypo_aupr, keep.names = "id")
    data.table::setnames(hypo_aupr, "V1", "test_aupr")
  }

  dt_dmsv <- merge(train_results, rbind(hypo_aupr, hyper_aupr), by = "id")
  data.table::setnames(dt_dmsv, "validation_aupr", "validation_mean_aupr")
  data.table::setnames(dt_dmsv, "train_aupr", "train_mean_aupr")
  #   dt_dmsv[, stat_origin := NULL]
  data.table::setkeyv(dt_dmsv, "train_rank")

  return(dt_dmsv)
}


predict.CimpleG <- function(
  object,
  new_data,
  meth=c("hypo","hyper")
){

  # due to NSE notes in R CMD check
  prediction_optimal <- prediction_default <- optimal_bins <- .metric <- NULL

  # assume object is cpg or CimpleG obj
  assertthat::assert_that(is.character(object) | "CimpleG" %in% is(object))
  if(!is.character(object)){
    abort("TODO: implement")
  }

  if(!data.table::is.data.table(new_data)) data.table::setDT(new_data)

  assertthat::assert_that(object %in% colnames(new_data))

  # assume last col is target
  target_name <- names(new_data)[ncol(new_data)]

  get_cols <- c(object,target_name)
  #   target_vec <- new_data[,ncol(new_data),with=FALSE]
  #   predictor_vec <- new_data[,object,with=FALSE]

  #   tbl_pred_target <- new_data[,..get_cols]
  tbl_pred_target <- new_data[,.SD,.SDcols=get_cols]

  if(meth=="hypo"){
    tbl_pred_target[,(object):= 1 - .SD, .SDcols=object]
  }

  # Class prediction with optimal binning from information gain
  tbl_pred_target[, prediction_optimal:= OneR::optbin(.SD, method="infogain")[[1]], ]
  levels(tbl_pred_target$prediction_optimal) <- c("X0","X1")
  tbl_pred_target$prediction_optimal <- stats::relevel(tbl_pred_target$prediction_optimal,"X1")
  tbl_pred_target[, optimal_bins:= OneR::optbin(.SD, method="infogain")[[1]], ]

  # Default class prediction, hypermethylated if over 0.5, hypomethylated otherwise
  tbl_pred_target[, prediction_default := factor(ifelse(.SD >= 0.5, "X1", "X0"),levels=c("X1","X0")),.SDcols=object]

  # method that we use in training is: prroc_prauc_vec(estimate = 1 - x, truth = true_v)
  class_prob_metrics <- yardstick::metric_set(
    yardstick::pr_auc,
    yardstick::roc_auc,
    yardstick::accuracy,
    yardstick::f_meas
  )

  opt_metrics <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::f_meas
  )

  res <- class_prob_metrics(
    data = tbl_pred_target,
    truth = !! rlang::enquo(target_name),
    object, ## prob
    estimate = prediction_default, ## class
    estimator = "binary"
  )[,c(".metric",".estimate")]
  res_opt <- opt_metrics(
    data = tbl_pred_target,
    truth = !! rlang::enquo(target_name),
    estimate = prediction_optimal, ## class
    estimator = "binary"
  )[,c(".metric",".estimate")]

  #   res_opt[.metric=="accuracy",.metric:="accuracy_opt"]
  #   res_opt[.metric=="f_meas",.metric:="f_meas_opt"]
  res_opt <- res_opt %>% dplyr::mutate(.metric = base::replace(.metric,.metric=="accuracy","accuracy_optimal"))
  res_opt <- res_opt %>% dplyr::mutate(.metric = base::replace(.metric,.metric=="f_meas","f_optimal"))
  res <- rbind(res,res_opt)
  res <- res[order(res$.metric),]

  data.table::setnames(tbl_pred_target,object,paste0("prob.",object))
  #   col_order <- c(paste0("prob.",object),"prediction","target")

  return(list(results=res,table=tbl_pred_target[]))
}


eval_general_model <- function(
  test_data,
  final_model,
  verbose = 1
){
  if(verbose>=2){ message("Evaluating test data...") }

  # get performance on test data
  test_data$target <- test_data$target %>% stats::relevel("positive_class")

  test_pred <- predict(
    object = final_model,
    new_data = test_data,
    type = "class"
  ) %>%
  dplyr::pull(.data$.pred_class)

  test_pred_prob <- predict(
    object = final_model,
    new_data = test_data,
    type = "prob"
   ) %>%
  as.data.frame %>%
  dplyr::pull(.data$.pred_positive_class)

  acc_perf <- yardstick::accuracy_vec(
    truth=test_data$target,
    estimate=test_pred
  )
  f1_perf <- yardstick::f_meas_vec(
    truth=test_data$target,
    estimate=test_pred
  )
  aupr_perf <- yardstick::pr_auc_vec(
    truth = test_data$target,
    estimate = test_pred_prob
  )

  res <- data.frame(
    method=final_model$fit$actions$model$spec$engine,
    accuracy=acc_perf,
    f1 = f1_perf,
    aupr = aupr_perf
  )
  return(res)
}


cv_loop <- function(
  train_data,
  target_name,
  k_folds = 10,
  pred_type = c("both","hypo","hyper"),
  param_p=NULL,
  q_threshold=NULL,
  rank_method=NULL,
  run_parallel=FALSE,
  verbose=1
){

  # due to NSE notes in R CMD check
  cpg_score <- id <- stat_origin <- mean_aupr <- target <- train_rank <- NULL
  diff_means <- fold_presence <- NULL

  # new do_cv
  assertthat::assert_that("target" %in% colnames(train_data))
  assertthat::assert_that(is.factor(train_data[,target]))

  # Only printed when finished
  tictoc::tic(paste0("Training for target '",target_name,"' with 'CimpleG' has finished."))

  cv_index <- caret::createFolds(
    train_data$target,
    k = k_folds,
    # if returnTrain=TRUE when k=1, returns nothing
    returnTrain = ifelse(k_folds < 2, FALSE, TRUE),
    list=TRUE
  )

  tc <- caret::trainControl(
    index = cv_index,
    indexOut = purrr::map(
      cv_index,
      function(x){setdiff(seq_len(nrow(train_data)),x)}
    ),
    method = 'cv',
    number = k_folds
  )

  f_data <- rsample::caret2rsample(ctrl = tc, data = train_data)

  # adding split id within the splits
  for(i in seq_len(length(f_data$splits))){
    f_data$splits[[i]]$id <- tibble::tibble(id = sprintf("Fold%02d", i))
  }

  # TODO: implement parallel cv loop
  f_data$results <- purrr::map(
    .x = f_data$splits,
    .f = find_best_predictors,
    p_type = pred_type,
    param_p = param_p,
    q_threshold = q_threshold,
    verbose = verbose
  )

  # process results from training splits
  train_summary <- f_data$results %>%
    data.table::rbindlist() %>%
    data.table::melt(
      measure.vars = c("train_aupr","validation_aupr"),
      variable.name = "stat_origin",
      value.name = "aupr"
    )

  # compute mean var_a, mean aupr and number of times a predictor was in a fold
  # discard features that were present in less than a half of the folds
  train_summary <- train_summary[,
    .(
      mean_aupr = sapply(.SD[,"aupr"], mean),
      mean_var_a = sapply(.SD[,"var_a"], mean),
      fold_presence = .N
      ),
    .SDcols = c("aupr","var_a"),
    by = .(id, stat_origin)
    ][fold_presence >= ceiling(k_folds / 2)]

  data.table::setkeyv(train_summary,"id")

  dt_dmsv <- compute_diffmeans_sumvar(
    data = train_data[, .SD, .SDcols = unique(train_summary$id)],
    target_vector = train_data$target == "positive_class"
  )
  data.table::setkeyv(dt_dmsv, "id")

  # join tables and
  # make wide format regarding training/validation aupr
  train_results <- data.table::dcast(
    train_summary[dt_dmsv],
    id + fold_presence + diff_means + sum_variance + pred_type + mean_var_a ~ stat_origin,
    value.var = "mean_aupr"
  )

  # # calculating ranking score
  # train_results[,cpg_score := ((validation_aupr * .5) + (train_aupr * .5)) * abs(diff_means) * fold_presence]

  # data.table::setkeyv(train_results, "cpg_score")
  # data.table::setorder(train_results, -cpg_score, -validation_aupr)
  rank_results(train_res=train_results, rank_method=rank_method)

  # add index as rank
  train_results[, train_rank := .I ]

  print_timings(verbose<1)

  return(list(
      fold_id=rsample::tidy(f_data),
      train_summary=train_summary,
      dt_dmsv=dt_dmsv,
      train_results=train_results
  ))
}


rank_results <- function(train_res,rank_method){

  # due to NSE notes in R CMD check
  cpg_score <- mean_var_a <- validation_aupr <- train_aupr <- fold_presence <- NULL

  switch(
    rank_method,
    a_rank = {
      train_res[,cpg_score := mean_var_a]
      data.table::setkeyv(train_res, "cpg_score")
      data.table::setorder(train_res, cpg_score)
    },
    ac_rank = {
      train_res[,cpg_score := ((mean_var_a * 0.8) + ((1-(validation_aupr * .5 + train_aupr * .5)) * 0.2)) * (1/fold_presence)]
      data.table::setkeyv(train_res, "cpg_score")
      data.table::setorder(train_res, cpg_score)
    },
    c_rank = {
      train_res[,cpg_score := ((validation_aupr * .5) + (train_aupr * .5)) * fold_presence]
      data.table::setkeyv(train_res, "cpg_score")
      data.table::setorder(train_res, -cpg_score)
    },
    {
      stop("No ranking method provided.")
    }
  )
}



find_best_predictors <- function(
  split_train_set,
  param_p,
  q_threshold=0.01,
  p_class = "positive_class",
  p_type=c("both","hypo","hyper"),
  verbose=1
){

  # due to NSE notes in R CMD check
  target <- id <- resample <- pred_type <- validation_aupr <- train_aupr <- NULL

  tictoc::tic(split_train_set$id %>% unlist())

  # Train data
  train_set <- data.table::copy(rsample::analysis(split_train_set))
  # Truth labels
  tru_v <- train_set$target == p_class

  # Compute delta sigma space (diffmeans, sumvars)
  dt_dmsv <- compute_diffmeans_sumvar(
    #     data=train_set[,-ncol(train_set)],
    data = train_set[, -ncol(train_set), with=FALSE],
    target_vector = tru_v
  )

  # If we only search for one type of probe, we can discard the rest
  dt_dmsv <- dt_dmsv[
    p_type == "both" |
    (pred_type & p_type == "hyper") |
    (!pred_type & p_type == "hypo"),
  ]

  # Get relevant features
  dt_dmsv <- get_relevant_features(
    dt_diffmean_sumvar = dt_dmsv,
    param_p = param_p,
    q_threshold = q_threshold
  )

  # Metrics on train data
  hyper_aupr <- train_set[,
    lapply(X = .SD, true_v = target, FUN = function(x, true_v) {
      prroc_prauc_vec(estimate = x, truth = true_v)
    }),
    .SDcols = dt_dmsv[pred_type == TRUE, id]]

  hypo_aupr <- train_set[,
    lapply(X = .SD, true_v = target, FUN = function(x, true_v) {
      prroc_prauc_vec(estimate = 1 - x, truth = true_v)
    }),
    .SDcols = dt_dmsv[pred_type == FALSE, id]]

  if(length(hyper_aupr) == 0){
    hyper_aupr <- data.table::data.table(
      id = character(),train_aupr = double(),key = "id"
    )
  }else{
    hyper_aupr <- data.table::transpose(hyper_aupr, keep.names="id")
    data.table::setnames(hyper_aupr, "V1", "train_aupr")
  }
  if(length(hypo_aupr) == 0){
    hypo_aupr <- data.table::data.table(
      id = character(), train_aupr = double(), key = "id"
    )
  }else{
    hypo_aupr <- data.table::transpose(hypo_aupr, keep.names = "id")
    data.table::setnames(hypo_aupr, "V1", "train_aupr")
  }

  dt_dmsv <- merge(dt_dmsv, rbind(hypo_aupr, hyper_aupr), by = "id")
  data.table::setkeyv(dt_dmsv, "train_aupr")
  data.table::setorder(dt_dmsv, -train_aupr)

  # Validation data
  holdout_set <- rsample::assessment(split_train_set)

  # Metrics on validation data
  hyper_aupr <- holdout_set[,
    lapply(X = .SD, true_v = target, FUN = function(x, true_v) {
      prroc_prauc_vec(estimate = x, truth = true_v)
    }),
    .SDcols = dt_dmsv[pred_type == TRUE, id]]

  hypo_aupr <- holdout_set[,
    lapply(X = .SD, true_v = target, FUN = function(x, true_v) {
      prroc_prauc_vec(estimate = 1 - x, truth = true_v)
    }),
    .SDcols = dt_dmsv[pred_type == FALSE, id]]

  if(length(hyper_aupr) == 0){
    hyper_aupr <- data.table::data.table(
      id = character(), validation_aupr = double(), key = "id"
    )
  }else{
    hyper_aupr <- data.table::transpose(hyper_aupr, keep.names = "id")
    data.table::setnames(hyper_aupr, "V1", "validation_aupr")
  }
  if(length(hypo_aupr) == 0){
    hypo_aupr <- data.table::data.table(
      id = character(), validation_aupr = double(), key = "id"
    )
  }else{
    hypo_aupr <- data.table::transpose(hypo_aupr, keep.names = "id")
    data.table::setnames(hypo_aupr, "V1", "validation_aupr")
  }

  dt_dmsv <- merge(dt_dmsv, rbind(hypo_aupr, hyper_aupr), by = "id")
  data.table::setkeyv(dt_dmsv, "validation_aupr")
  data.table::setorder(dt_dmsv, -validation_aupr)

  dt_dmsv[, resample := split_train_set$id %>% unlist()]
  print_timings(verbose < 2)

  return(dt_dmsv)
}

# Training/Finding the best predictors/CpGs
find_predictors <- function(
  split_train_set,
  init_step = 0.1,
  step_increment = 0.1,
  min_feat_search = 10,
  p_class = "positive_class",
  method,
  scale_scores=FALSE,
  pred_type=c("both","hypo","hyper"),
  verbose=1
) {

  tictoc::tic(split_train_set$id %>% unlist())

  train_set <- rsample::analysis(split_train_set)

  tru_v <- train_set$target == p_class

  dt_dMean_sVar <- compute_diffmeans_sumvar(
    data=train_set[,-ncol(train_set)],
    target_vector = tru_v
  )

  if(pred_type!="both"){
    if(pred_type=="hypo") dt_dMean_sVar <- dt_dMean_sVar[pred_type==FALSE]
    if(pred_type=="hyper") dt_dMean_sVar <- dt_dMean_sVar[pred_type==TRUE]
  }

  df_dMean_sVar <- as.data.frame(dt_dMean_sVar)

  if (method == "brute_force" | grepl("CimpleG",method)) {

    if (grepl("CimpleG",method)) {

      parab_res <- parabola_iter_loop(
        df_diffmean_sumvar=df_dMean_sVar,
        init_step=init_step,
        step_increment=step_increment,
        min_feat_search=min_feat_search,
        pred_type=pred_type,
        verbose=verbose
      )
      df_dMean_sVar <- parab_res$df_diffmean_sumvar

    }

    hyperM_predictors <- data.frame(.id=character(), AUPR=double())
    hypoM_predictors <- data.frame(.id=character(), AUPR=double())

    if(any(df_dMean_sVar$pred_type)){
      # get PRAUC for hyper methylated cpgs
      hyperM_predictors <- train_set %>%
        dplyr::select(
          df_dMean_sVar[which(df_dMean_sVar$pred_type), "id"]
        ) %>%
        dplyr::summarise(dplyr::across(
            .cols = tidyselect::vars_select_helpers$where(is.numeric),
            .fns = function(x,truth){
            prroc_prauc_vec(truth = truth, estimate = x)
          },
          truth = train_set$target
        )) %>% t() %>% as.data.frame %>% tibble::rownames_to_column(".id") %>%
        magrittr::set_colnames(c(".id", "AUPR")) %>%
        # attach mean diff, will be used to scale AUPR
        dplyr::mutate(pred_type = "hyper") %>%
        dplyr::left_join(
          df_dMean_sVar[,c("id","diff_means","sum_variance")],
          by=c(".id"="id")
        )


      if(scale_scores){
        hyperM_predictors$DiffScaledAUPR <- scales::rescale(
          abs(hyperM_predictors$diff_means),
          to=c(0,1)
        ) * hyperM_predictors$AUPR
      }

    }
    if(any(!df_dMean_sVar$pred_type)){
      # get PRAUC for hypo methylated cpgs
      hypoM_predictors <- train_set %>%
        dplyr::select(df_dMean_sVar[which(!df_dMean_sVar$pred_type), "id"]) %>%
        dplyr::summarise(dplyr::across(
            .cols=tidyselect::vars_select_helpers$where(is.numeric),
            .fns = function(x,truth){
            prroc_prauc_vec(truth = truth, estimate = 1 - x)
          },
          truth = train_set$target
        )) %>% t() %>% as.data.frame %>% tibble::rownames_to_column(".id")%>%
        magrittr::set_colnames(c(".id", "AUPR"))%>%
        # attach mean diff, will be used to scale AUPR
        dplyr::mutate(pred_type = "hypo") %>%
        dplyr::left_join(
          df_dMean_sVar[,c("id","diff_means","sum_variance")],
          by=c(".id"="id")
        )

    # scale AUPR by absolute mean difference between conditions/classes
      if(scale_scores){
        hypoM_predictors$DiffScaledAUPR <- scales::rescale(
          abs(hypoM_predictors$diff_means),
          to=c(0,1)
        ) * hypoM_predictors$AUPR
      }

    }

    meth_predictors <- rbind(
      hyperM_predictors,
      hypoM_predictors
    ) %>%
      dplyr::arrange(dplyr::desc(.data$AUPR))


    holdout_res <- rsample::assessment(split_train_set)
    lvls <- levels(holdout_res$target)

    apply_res = apply(
      X = meth_predictors,
      MARGIN = 1,
      FUN = function(x){

        pred_type = x["pred_type"]
        predictor = x[".id"]
        diff_means = as.double(x["diff_means"])

        if (pred_type == "hyper") {
          pred_res <- factor(ifelse(
            holdout_res[, predictor] >= .5,
            lvls[2], lvls[1]
          ), levels = lvls)
          pred_prob <- holdout_res[, predictor]
        } else {
          pred_res <- factor(ifelse(
            (1 - holdout_res[, predictor]) >= .5,
            lvls[2], lvls[1]
          ), levels = lvls)
          pred_prob <- 1 - holdout_res[, predictor]
        }
        return(data.frame(
          samples=rownames(holdout_res),
          predictor,
          truth=holdout_res$target,
          type=pred_type,
          prediction=pred_res,
          positive_prob=pred_prob,
          diff_means=diff_means,
          row.names = NULL
        ))
      }
    )%>% plyr::ldply() %>% tibble::as_tibble()

    res_df <- apply_res %>%
      dplyr::mutate(resample = split_train_set$id %>% unlist())

    if (grepl("CimpleG",method)) {
      res_df <- res_df %>%
        dplyr::mutate(parab_param = parab_res$parabola_param)
    }
  }

  if(method=="oner"){
    # oneRule model
    # Using OneRule model
    oner_predata <- OneR::optbin(target ~ ., data = train_set, method = "infogain")
    oner_mod <- OneR::OneR(oner_predata)
    oner_mod <- fix_oner_boundaries(oner_mod)

    holdout_res <- rsample::assessment(split_train_set)
    lvls <- levels(holdout_res$target)

    oner_pred <- factor(predict(oner_mod, holdout_res, type = "class"), levels = lvls)
    pred_prob <- predict(oner_mod, holdout_res, type = "prob")[, "positive_class"]

    res_df <- tibble::tibble(
      resample = split_train_set$id %>% unlist(),
      samples = rownames(holdout_res),
      predictor = oner_mod$feature,
      truth = holdout_res$target,
      type = ifelse(df_dMean_sVar[oner_mod$feature, ]$pred_type, "hyper", "hypo"),
      prediction = oner_pred,
      correct = oner_pred == holdout_res$target,
      positive_prob = pred_prob
    )
  }

  print_timings(verbose<2)
  return(res_df)
}


train_general_model <- function(
  train_data,
  k_folds,
  model_type,
  engine,
  grid_n = 10,
  target_name,
  verbose = 1
){

  tictoc::tic(paste("Training for target '",target_name,"' with ",model_type," has finished."))
  # making stratified folds, rsample doesnt seem to work properly
  cv_index = caret::createFolds(
    train_data$target,
    k = k_folds,
    returnTrain = TRUE,
    list=TRUE
  )
  tc <- caret::trainControl(
    index = cv_index,
    indexOut = purrr::map(
      cv_index,
      function(x){
        setdiff(seq_len(nrow(train_data)),x)
      }
    ),
    method = 'cv',
    number = k_folds
  )

  f_data = rsample::caret2rsample(ctrl=tc,data = train_data)

  # adding split id within the splits
  for(i in seq_len(length(f_data$splits))){
    f_data$splits[[i]]$id = tibble::tibble(id=sprintf("Fold%02d",i))
  }

  cimpleg_recipe <- recipes::recipe(
    x=head(train_data,0),
    vars=colnames(train_data),
    roles=c(rep("predictor",ncol(train_data)-1),"outcome")
  )

  if(model_type == "logistic_reg"){
    general_model <-
      # specify the model
      parsnip::logistic_reg(
        penalty=tune::tune(),
        mixture=tune::tune()
      ) %>%
      parsnip::set_engine("glmnet")
  }
  if(model_type == "decision_tree"){
    corr_filter <- cimpleg_recipe %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_lincomb(recipes::all_predictors()) %>%
      recipes::step_corr(
        recipes::all_predictors(),
        threshold = .5,
        method = "spearman"
      )

    cimpleg_recipe <- recipes::prep(
      corr_filter, training = train_data
    )

    general_model <-
      parsnip::decision_tree(
        cost_complexity=0.01, # Default
        tree_depth=tune::tune(),
        min_n=2 # Default
      ) %>%
      parsnip::set_engine(
        "rpart",
        maxsurrogate=2,
        maxcompete=1
      )
  }
  if(model_type == "null_model"){
    #FIXME crashes
    general_model <-
      # specify the model
      parsnip::null_model() %>%
      parsnip::set_engine("parsnip")
  }

  if (model_type == "boost_tree") {
    general_model <-
      # specify the model
      parsnip::boost_tree(
        tree_depth = 6, # Default
        trees = tune::tune(),
        learn_rate=0.3, # Default
        mtry = 100L, # Randomly selected predictors
        min_n = 1, # Default
        loss_reduction=0.0, # Default
        sample_size=1.0, # Default
        stop_iter = Inf # Default
      ) %>%
      parsnip::set_engine(
        "xgboost",
        objective = "binary:logistic", # Specific for binary classification
        eval_metric="aucpr", # AUPR in line with the rest of the package
        maximize=TRUE # We want to maximize AUPR
      )
  }

  if (model_type == "mlp") {

    corr_filter <- cimpleg_recipe %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_lincomb(recipes::all_predictors()) %>%
      recipes::step_corr(
        recipes::all_predictors(),
        threshold = .5,
        method = "spearman"
      )

    cimpleg_recipe <- recipes::prep(
      corr_filter, training = train_data
    )
    general_model <-
      # specify the model
      parsnip::mlp(
        hidden_units = tune::tune(),
        penalty = tune::tune(),
        epochs = 100L # Default
      ) %>%
      parsnip::set_engine(
        "nnet",
        MaxNWts=1000000
      )
  }

  if (model_type == "rand_forest") {
    corr_filter <- cimpleg_recipe %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_lincomb(recipes::all_predictors()) %>%
      recipes::step_corr(
        recipes::all_predictors(),
        threshold = .5,
        method = "spearman"
      )

    cimpleg_recipe <- recipes::prep(
      corr_filter, training = train_data
    )
    general_model <-
      # specify the model
      parsnip::rand_forest(
        trees = tune::tune(),
        min_n = 1, # Set to 1, default is 1 in ranger but 10 for classification in tidymodels
        mtry = floor(sqrt(ncol(train_data))) # Default w/ ranger
      ) %>%
      parsnip::set_engine(
        "ranger",
        oob.error = FALSE,
        respect.unordered.factors="order",
        importance = "impurity_corrected" # this should be more correct
        # importance = "impurity" # however this doesnt produce warnings
      )
  }


  general_model <- general_model %>%
    # choose either the continuous regression or binary classification mode
    parsnip::set_mode("classification")

  cimpleg_workflow <- workflows::workflow()%>%
    workflows::add_recipe(cimpleg_recipe) %>%
    workflows::add_model(general_model)

  # Training model
  # defining tuning grid
  cimpleg_res <- cimpleg_workflow %>%
    tune::tune_grid(
      resamples=f_data,
      grid=grid_n,
      metrics=yardstick::metric_set(
        yardstick::pr_auc
      )
    ) %>%
    tune::select_best(metric = "pr_auc")

  # model fitting
  cimpleg_final_model <- cimpleg_workflow %>%
    tune::finalize_workflow(cimpleg_res) %>%
    parsnip::fit(data = train_data)

  # butcher model to axe unnecessary components
  if(model_type != "boost_tree"){
    cimpleg_final_model <- cimpleg_final_model %>%
      butcher::butcher(
        #  verbose < 1
      )
  }else{
    # xgboost calls fail when axe_ctrl is executed
    cimpleg_final_model <- cimpleg_final_model %>%
      butcher::axe_call() %>%
      #       butcher::axe_ctrl() %>%
      butcher::axe_env() #%>%
      #       butcher::axe_fitted()
  }


  print_timings(verbose < 1)
  return(cimpleg_final_model)
}


# Loop iterating through parabola selecting cpgs
#
# @return list
parabola_iter_loop <- function(
  df_diffmean_sumvar,
  init_step,
  step_increment,
  min_feat_search,
  pred_type,
  verbose=1
){
  init_fs <- init_step
  final_param <- init_fs
  df_diffmean_sumvar$select_feature <- select_features(
    x = df_diffmean_sumvar$diff_means,
    y = df_diffmean_sumvar$sum_variance,
    a = init_fs
  )

  # Find features in feature space (diff_means,sum_variance)
  i_iter <- 0
  while (
    updt_selected_feats(df_diffmean_sumvar, min_feat_search,pred_type) & i_iter < 100
  ){
    df_diffmean_sumvar$select_feature <- select_features(
      x = df_diffmean_sumvar$diff_means,
      y = df_diffmean_sumvar$sum_variance,
      a = init_fs
    )
    final_param <- init_fs
    init_fs <- init_fs + step_increment
    i_iter <- i_iter + 1
  }
  if(!i_iter<100) abort("Could not find signatures.")
  if(verbose>=3){ message("Fold parabola parameter: ", final_param) }

  df_diffmean_sumvar <- df_diffmean_sumvar[which(df_diffmean_sumvar$select_feature), ]

  return(list(
    df_diffmean_sumvar=df_diffmean_sumvar,
    parabola_param=final_param
  ))
}


get_relevant_features <- function(
  dt_diffmean_sumvar,
  param_p,
  q_threshold=0.01
){

  # due to NSE notes in R CMD check
  var_a <- diff_means <- sum_variance <- NULL

  # TODO: compare efficiency of the 2 lines below
  dt_diffmean_sumvar[, var_a := compute_ax(diff_means,sum_variance,param_p)]
  #   data.table::set(dt_diffmean_sumvar, j="var_a",value=compute_ax(dt_diffmean_sumvar$diff_means,dt_diffmean_sumvar$sum_variance,param_p))

  data.table::setkeyv(dt_diffmean_sumvar, "var_a")
  dt_diffmean_sumvar <-
    dt_diffmean_sumvar[var_a < stats::quantile(var_a, q_threshold)]

  return(dt_diffmean_sumvar)
}


#' Feature selection function used in the diffmeans, sumvariance space
#' @param x, difference in means value
#' @param y, sum of variances value
#' @param a, parabola parameter, scales how open/closed the parabola is, the higher the value, the more closed the parabola is.
#' @return bool vector
#' @export
select_features <- function(x, y, a) {
  return(y < (a * x)^2)
}


#' Feature selection function used in the sigma delta space
#' @param dm, delta (difference in mean values)
#' @param sv, sigma (sum of variance values)
#' @param p, even number, the greater 'p' is the more importance will be given to sigma
#' @export
compute_ax <- function(dm, sv, p){
  return(sv / (dm ** p))
}


# Depending on pred_type:
#   Returns TRUE if:
#     less than feat_threshold hyper CpGs have been selected
#     less than feat_threshold hypo CpGs have been selected
#   Returns FALSE if:
#     more than feat_threshold hyper and hypo CpGs have been selected
updt_selected_feats <- function(
  df_diffmean_sumvar,
  feat_threshold = 10,
  pred_type=c("both","hyper","hypo")
) {
  expected_cols <- c("select_feature", "diff_means", "sum_variance","pred_type")
  if (!all(expected_cols %in% colnames(df_diffmean_sumvar))) {
    abort("Something went wrong when creating the diff_means sum_variance data.frame!")
  }
  if(pred_type=="hyper" | pred_type=="both"){
    updt_hyper <- length(
      which(df_diffmean_sumvar$pred_type & df_diffmean_sumvar$select_feature)
    ) < feat_threshold
    if(pred_type=="hyper"){
      return(updt_hyper)
    }
  }
  if(pred_type=="hypo" | pred_type=="both"){
    updt_hypo <- length(
      which(!df_diffmean_sumvar$pred_type & df_diffmean_sumvar$select_feature)
    ) < feat_threshold
    if(pred_type=="hypo"){
      return(updt_hypo)
    }
  }

  updt_cond <- updt_hyper | updt_hypo
  return(updt_cond)
}

# Fixes lower and upper boundaries (0,1) on OneR models
#
# @return OneR object
fix_oner_boundaries <- function(oner_mod){
  boundary_vals_l <- as.numeric(
    strsplit(x = gsub("\\(|\\]", "", (names(oner_mod$rules)[1])), ",", fixed = TRUE)[[1]]
  )
  boundary_vals_h <- as.numeric(
    strsplit(x = gsub("\\(|\\]", "", (names(oner_mod$rules)[2])), ",", fixed = TRUE)[[1]]
  )
  boundary_vals_l[1] <- ifelse(boundary_vals_l[1] > 0, -0.001, boundary_vals_l[1])
  boundary_vals_h[2] <- ifelse(boundary_vals_h[2] < 1, 1.001, boundary_vals_h[2])

  # fixed_interval <- levels(OneR:::CUT(0, c(boundary_vals_l, boundary_vals_h)))
  fixed_interval <- noquote(
    c(
      paste0('"(',boundary_vals_l[1],",",boundary_vals_l[2],']"'),
      paste0('"(',boundary_vals_h[1],",",boundary_vals_h[2],']"')
    )
  )
  names(oner_mod$rules) <- fixed_interval
  dimnames(oner_mod$cont_table)[[2]] <- fixed_interval
  return(oner_mod)
}

# defining PRROC PRAUC
prroc_prauc_vec <- function(
  truth, estimate, estimator = "binary", na_rm = TRUE, ...
) {
  prroc_prauc_impl <- function(truth, estimate, event_level) {
    res <- PRROC::pr.curve(
      scores.class0 = estimate,
      weights.class0 = ifelse(truth == "positive_class", 1, 0),
      curve = FALSE,
      max.compute = FALSE,
      min.compute = FALSE,
      rand.compute = FALSE,
      dg.compute = FALSE
    )$auc.integral
    return(res)
  }
  yardstick::metric_vec_template(
    metric_impl = prroc_prauc_impl,
    truth = truth,
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    ...
  )
}

#
prroc_prauc <- function(data, ...) {
  UseMethod("prroc_prauc")
}


#
prroc_prauc.data.frame <- function(
  data,
  truth,
  estimate,
  estimator = "binary",
  na_rm = TRUE,
  event_level = "first",
  ...
) {
  yardstick::metric_summarizer(
    metric_nm = "prroc_prauc",
    metric_fn = prroc_prauc_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    estimator = estimator,
    event_level = event_level,
    ...
  )
}

# timing func
print_timings <- function(quiet = FALSE) {
  tictoc::toc(quiet = quiet)
}


make_train_test_split <- function(
  train_d,
  train_targets,
  targets,
  prop=0.75
){

  if(is.null(names(targets))) names(targets) <- targets
  
  assertthat::assert_that(
    all(rowSums(train_targets[targets]) < 2),
    msg=paste0(
      "When performing the train-test split, we take into account all given targets and assume each sample belongs ",
      "to a single one of them.\n",
      "This does not seem to be the case for sample(s) ",
      paste0(which(rowSums(train_targets[targets]) > 1),collapse=", "),".\n",
      "Either fix the samples class for the given target or try to train for each of the targets independently."
    )
  )
  
  # creating a single column with all the targets available so that we can
  # do a stratified split
  # FIXME this is really ugly... only works if samples only belong to a single target
  target_strat <- purrr::map_dfr(
    .x = targets,
    .f = function(target) {
      res <- ifelse(
        train_targets[, target] == 1,
        target,
        train_targets[, target]
      )
      return(res)
    }, .id = "id"
  ) %>%
    tidyr::unite(col = "merged") %>%
    dplyr::mutate(merged = gsub("[0]+", "", .data$merged)) %>%
    dplyr::mutate(merged = gsub("^[_]+", "", .data$merged)) %>%
    dplyr::mutate(merged = gsub("[_]+$", "", .data$merged)) %>%
    dplyr::mutate(merged = ifelse(.data$merged == "", 0, .data$merged))
  # > merged
  # A
  # 0
  # 0
  # B
  # A
  # ...
  assertthat::assert_that(
    min(table(target_strat$merged)) > 1,
    msg = paste0(
      "Number of samples for one of the target classes (",
      names(which.min(table(target_strat$merged))),
      ") needs to be bigger than 1.\n",
      "Either add samples to this class or remove it from the targets."
    )
  )

  # this should ensure a training and testing data have samples
  # even for targets with a small number of samples
  min_pool <- min(
    min(table(target_strat$merged))/sum(table(target_strat$merged)),
    0.1
  )

  part_d <- rsample::initial_split(
    data = train_d %>%
      as.data.frame %>%
      dplyr::mutate(target_strat = target_strat %>% dplyr::pull(.data$merged)),
    prop = prop,
    strata = "target_strat",
    pool = min_pool
  )

  tmp_train <- rsample::training(part_d)
  tmp_test <- rsample::testing(part_d)

  new_train_targets <- tmp_train %>%
    tibble::rownames_to_column("id") %>%
    dplyr::mutate(tmp_value = 1) %>%
    tidyr::pivot_wider(
      id_cols = .data$id,
      names_from = .data$target_strat,
      values_from = .data$tmp_value
    ) %>%
    dplyr::select(.data$id, dplyr::all_of(targets)) %>%
    dplyr::mutate_all(tidyr::replace_na, 0) %>%
    tibble::column_to_rownames("id")

  # this is just so that if all samples were assigned to the training dataset,
  # for a given target, we can still add this target to the testing data
  # to avoid problems of trying to search for the target later on.
  pseudo_targets <- sapply(targets, function(x) x <- NA_real_)

  new_test_targets <- tmp_test %>%
    tibble::rownames_to_column("id") %>%
    dplyr::mutate(tmp_value = 1) %>%
    tidyr::pivot_wider(
      id_cols = .data$id,
      names_from = .data$target_strat,
      values_from = .data$tmp_value
    ) %>%
    # add col with the missing target names if theres any
    tibble::add_column(!!!pseudo_targets[setdiff(names(targets), names(.))]) %>%
    dplyr::select(.data$id, dplyr::all_of(targets)) %>%
    dplyr::mutate_all(tidyr::replace_na, 0) %>%
    tibble::column_to_rownames("id")

  tmp_train <- tmp_train %>% dplyr::select(-target_strat)
  tmp_test <- tmp_test %>% dplyr::select(-target_strat)

  return(
    list(
      train_data = tmp_train,
      test_data = tmp_test,
      train_targets = new_train_targets,
      test_targets = new_test_targets
    )
  )
}

#' Compute diff mean sum var dataframe
#' @param data Matrix with beta values that will be used to compute diffmeans sumvar data frame
#' @param target_vector boolean vector defining which samples in data are part of the target class
#' @export
compute_diffmeans_sumvar <- function(data, target_vector) {

  assertthat::assert_that(is.logical(target_vector))
  assertthat::are_equal(nrow(data),length(target_vector))

  if(is.data.frame(data)){
    if(requireNamespace("Rfast", quietly = TRUE)){
      data <- Rfast::data.frame.to_matrix(data, col.names=TRUE, row.names=TRUE)
    }else{
      warning(paste0(
          "CimpleG can run considerably faster if you have the package `Rfast` installed.\n",
          "Consider installing `Rfast` with `install.packages('Rfast')`\n"
      ))
      if(Sys.info()['sysname']=="Linux"){
        warning(paste0(
            "Since you are using a linux distribution, you might need to install the system library 'libgsl-dev'.\n",
        ))
      }
      data <- as.matrix(data)
    }
  }else if(!is.matrix(data)){
    abort("`data` must be a matrix or a data frame")
  }

  id <- colnames(data)

  diff_means <-
    matrixStats::colMeans2(data, rows=target_vector) - matrixStats::colMeans2(data, rows=!target_vector)

  sum_variance <- if(min(table(target_vector)) > 1){
    matrixStats::colVars(data, rows = target_vector) +
      matrixStats::colVars(data, rows = !target_vector)
  }else if(as.logical(names(which.min(table(target_vector)))) && max(table(target_vector)) > 1){
    # if the target class is min and the non-target class is more than 1
    warning(paste0(
      "Too few samples to properly calculate variance.\n",
      " Please consider adding more target samples to your dataset."
    ))
    # only looking at the variance of the "others" class
    (0 + matrixStats::colVars(data, rows = !target_vector))
  }else if(!as.logical(names(which.min(table(target_vector)))) && max(table(target_vector)) > 1){
    # if the non-target class is min and the target class is more than 1
    warning(paste0(
      "Too few samples to properly calculate variance.\n",
      " Please consider adding more non target samples to your dataset."
    ))
    # only looking at the variance of the "others" class
    (0 + matrixStats::colVars(data, rows = target_vector))
  } else {
    # both classes, target and non-target only have a single sample
    abort(paste0(
      "There are too few samples to calculate variance.\n",
      " Please add more samples to your target and non-target class.")
    )
  }

  dt_diffmean_sumvar <- data.table::data.table(
    "id" = id,
    "diff_means" = diff_means,
    "sum_variance" = sum_variance,
    "pred_type" = (diff_means >= 0)
  )

  return(dt_diffmean_sumvar)
}


download_geo_gsm_idat <- function(
  gse_gsm_table,
  data_dir="ncbi_geo_data",
  show_warnings=FALSE,
  run_parallel=FALSE
){

  assertthat::assert_that(
    is.data.frame(gse_gsm_table) ||
    tibble::is_tibble(gse_gsm_table) ||
    data.table::is.data.table(gse_gsm_table)
  )
  assertthat::assert_that("GSE" %in% toupper(colnames(gse_gsm_table)))
  assertthat::assert_that("GSM" %in% toupper(colnames(gse_gsm_table)))

  gse_gsm_table <- gse_gsm_table %>%
    dplyr::select(dplyr::matches("GSE"),dplyr::matches("GSM"))

  colnames(gse_gsm_table) <- c("GSE","GSM")

  gse_gsm_table <- gse_gsm_table %>%
    dplyr::mutate(GSE = trimws(.data$GSE)) %>%
    dplyr::mutate(GSM = trimws(.data$GSM))

  gse_gsm_table %>%
    dplyr::pull(.data$GSE) %>%
    unique %>%
    file.path(data_dir,.) %>%
    purrr::map_chr(.f=dir.create,recursive = TRUE,showWarnings=FALSE)

  targets = gse_gsm_table %>%
    dplyr::select(.data$GSE,.data$GSM) %>%
    split(f=seq(nrow(.)))

  work_helper = function(target){
    res = tryCatch(
      expr={
        GEOquery::getGEOSuppFiles(
          target$GSM,
          makeDirectory = TRUE,
          baseDir = file.path(data_dir,target$GSE),
          filter_regex="idat.gz$"
        )
      },
      error = function(cond) {
        message("failed to dw")
        message("Error message:")
        message(cond)
      },
      warning = function(cond) {
        message("Warning message:")
        message(cond)
      }
    )
    if (is.null(res)) warning(paste0(target$GSM, " download result was NULL."), call. = TRUE)
    return(res)
  }

  if(run_parallel){
    requireNamespace("future", quietly = FALSE)
    requireNamespace("future.apply", quietly = FALSE)
    res <- future.apply::future_lapply(
      X = targets,
      FUN = work_helper,
      future.seed = TRUE
    )
  }else{
    res <- purrr::map(
      .x=targets,
      .f=work_helper
    )
  }
  return(res)
}

is.CimpleG <- function(x) inherits(x, "CimpleG")

prediction_stats <- function(expected_values, predicted_values){
  # helper function to compute R2, RMSE and AIC when evaluating predictions vs observed values
  # correct usage and formula of R2, see https://doi.org/10.2307/2683704 and https://doi.org/10.1021%2Facs.jcim.5b00206
  r_squared <- function(vals, preds){1 - (sum((vals - preds) ^ 2) / sum((vals - mean(preds)) ^ 2))}
  rsq <- r_squared(expected_values, predicted_values)
  aic_res <- stats::AIC(stats::lm(predicted_values ~ expected_values))
  rmse <- sqrt(mean((expected_values - predicted_values)^2))
  return(list(r_squared = rsq, AIC = aic_res, rmse = rmse))
}


