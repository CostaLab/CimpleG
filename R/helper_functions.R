#' Main function controlling k-fold CV training
#'
#'
#' @importFrom dplyr %>%
do_cv <- function(
  train_data,
  k_folds = 10,
  n_repeats = 1, #FIXME not in use atm
  method = c("parab", "parab_scale", "adhoc", "oner"),
  pred_type = c("both","hypo","hyper"),
  target_name
) {
  assertthat::assert_that("target" %in% colnames(train_data))

  rv_tbl <- table(train_data[, "target"])

  if (k_folds > rv_tbl[which.min(rv_tbl)]) {
    k_folds <- rv_tbl[which.min(rv_tbl)]
    message(paste0("Too few samples for set K in cross-validation"))
    message(paste0("K folds reset to k=", k_folds))
  }

  tictoc::tic(paste0("Training for target '",target_name,"' with '",method,"(",pred_type,")' has finished."))
  # f_data <- rsample::vfold_cv(
  #   train_data,
  #   v = k_folds,
  #   repeats = n_repeats,
  #   strata = "target"
  # )
  # vfold_cv stratification not working properly, using caret instead
  cv_index = caret::createFolds(
    train_data$target,
    k = 10,
    returnTrain = TRUE,
    list=TRUE
  )
  tc <- caret::trainControl(
    index = cv_index,
    indexOut = purrr::map(
      cv_index,
      function(x){setdiff(seq_len(nrow(train_data)),x)}
    ),
    method = 'cv',
    number = 10
  )

  f_data = rsample::caret2rsample(ctrl=tc,data = train_data)

  # adding split id within the splits
  for(i in seq_len(length(f_data$splits))){
    f_data$splits[[i]]$id = tibble::tibble(id=sprintf("Fold%02d",i))
  }

  f_data$results <- purrr::map(
    .x = f_data$splits,
    .f = find_predictors,
    method = method,
    scale_scores = any(grepl("parab_scale", method, fixed = TRUE)),
    pred_type=pred_type
  )
  print_timings()

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
    dplyr::mutate(truth = relevel(truth, "positive_class")) %>%
    dplyr::mutate(prediction = relevel(prediction, "positive_class")) %>%
    dplyr::group_by(
      resample,
      predictor,
      type,
      dplyr::across(tidyselect::any_of("parab_param")),
      dplyr::across(tidyselect::any_of("diff_means"))
    ) %>%
    class_prob_metrics(
      truth,
      positive_prob,
      estimate = prediction
    ) %>%
    dplyr::group_by(predictor, type, .metric)

  if(any(grepl("parab",method))){
    train_summary <- train_summary %>%
      dplyr::summarise(
        mean_fold_performance = mean(.estimate,na.rm=TRUE),
        n = length(predictor),
        mean_parab_param = mean(parab_param),
        diff_means = mean(diff_means),
        .groups = "drop"
      )
      if(any(grepl("parab_scale",method))){
        # here is where we do all the scaling
        train_summary <- train_summary %>%
          dplyr::mutate(
            mean_fold_performance = (
              scales::rescale(abs(diff_means), to=c(0.1, 1)) * mean_fold_performance
            )
          ) %>%
          dplyr::mutate(
            mean_fold_performance = (
              scales::rescale(n, to=c(0.1, 1)) * mean_fold_performance
            )
          )
      }
  }else{
    train_summary <- train_summary %>%
      dplyr::summarise(
        mean_fold_performance = mean(.estimate,na.rm=TRUE),
        n = length(predictor),
        .groups = "drop"
      )
  }

  train_summary <- train_summary %>%
    dplyr::arrange(desc(n), desc(mean_fold_performance))

  # get predictor that appears in more folds
  best_pred <- train_summary %>%
    dplyr::filter(.metric == "pr_auc") %>%
    dplyr::arrange(desc(n), desc(mean_fold_performance)) %>%
    dplyr::slice_head() # %>%
  # dplyr::select(predictor,type)
  # dplyr::pull(predictor)

  fmod <- final_model(train_data, best_pred, train_summary=train_summary, method = method)

  return(fmod)
}

#' Evaluation of produced models on test data
#'
#' @importFrom dplyr %>%
#' @export
eval_test_data <- function(
  test_data,
  final_model,
  method = "oner"
) {
  message("Evaluating test data...")
  # get performance on test data
  test_data$target <- test_data$target %>% relevel("positive_class")

  predictor_name <- NULL

  if (identical(method, "adhoc") | identical(method, "parab") | identical(method, "parab_scale")) {

    new_data_vec <- test_data %>% dplyr::pull(final_model$predictor)

    if(final_model$type == "hypo"){

      new_data_vec <- 1 - new_data_vec
    }

    pred_class <- factor(
      ifelse(
        new_data_vec > 0.5,
        "positive_class",
        "negative_class"
      ),
      levels = levels(test_data$target)
    )
    # TODO alternatively for a better binning option we could use
    # OneR::bin(
    #   new_data_vec,
    #   nbins=2,
    #   labels=c("negative_class","positive_class")
    # )

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
      estimate = new_data_vec
    )

    predictor_name <- final_model$predictor
  }

  if (identical(method, "oner")) {

    pred_class <- predict(final_model,newdata = test_data, type = "class") %>%
      relevel("positive_class")

    pred_prob <- predict(final_model,newdata=test_data,type="prob") %>%
      as.data.frame %>% dplyr::pull(positive_class)

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

    predictor_name <- final_model$feature
  }

  res <- data.frame(
    method = method,
    predictor = predictor_name,
    accuracy = acc_perf,
    f1 = f1_perf,
    aupr = aupr_perf
  )
  return(res)
}

#' @importFrom dplyr %>%
eval_general_model <- function(
  test_data,
  final_model
){
  message("Evaluating test data...")
  # get performance on test data
  test_data$target <- test_data$target %>% relevel("positive_class")

  test_pred <- predict(
    object = final_model,
    new_data = test_data,
    type = "class"
  ) %>% dplyr::pull(.pred_class)

  test_pred_prob <- predict(
    object = final_model,
    new_data = test_data,
    type = "prob"
   ) %>% as.data.frame %>% dplyr::pull(.pred_positive_class)

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

#' Train the final model after training
#'
#' @importFrom dplyr %>%
final_model <- function(
  train_data,
  best_pred,
  train_summary,
  method = "oner"
) {
  message("Training final model...")
  if (method == "oner") {
    # OneR
    oner_predata <- OneR::optbin(
      as.formula(paste0("target~", best_pred$predictor)),
      data = train_data, method = "logreg"
    )
    oner_mod <- OneR::OneR(oner_predata)
    oner_mod <- fix_oner_boundaries(oner_mod)
    return(list(CpG=best_pred$predictor,model=oner_mod))
  }
  # if not using any of the prev methods, return best predictor
  return(list(
    CpG=best_pred$predictor,
    model=best_pred,
    train_summary=train_summary
  ))
}

#' Training/Finding the best predictors/CpGs
#'
#' @importFrom dplyr %>%
find_predictors <- function(
  split_train_set,
  init_step = 0.1,
  step_increment = 0.1,
  min_feat_search = 10,
  p_class = "positive_class",
  method,
  scale_scores=FALSE,
  pred_type=c("both","hypo","hyper")
) {

  tictoc::tic(split_train_set$id %>% unlist())

  train_set <- rsample::analysis(split_train_set)

  tru_v <- train_set$target == p_class

  df_dMean_sVar <- compute_diffmeans_sumvar(
    data=train_set[,-ncol(train_set)],
    target_vector = tru_v
  )

  if(pred_type!="both"){
    if(pred_type=="hypo") df_dMean_sVar <- df_dMean_sVar %>% dplyr::filter(!pred_type)
    if(pred_type=="hyper") df_dMean_sVar <- df_dMean_sVar %>% dplyr::filter(pred_type)
  }


  if (method == "adhoc" | grepl("parab",method)) {

    if (grepl("parab",method)) {

      parab_res <- parabola_iter_loop(
        df_diffmean_sumvar=df_dMean_sVar,
        init_step=init_step,
        step_increment=step_increment,
        min_feat_search=min_feat_search,
        pred_type=pred_type
      )
      df_dMean_sVar <- parab_res$df_diffmean_sumvar

    }

    hyperM_predictors <- data.frame(.id=character(), AUPR=double())
    hypoM_predictors <- data.frame(.id=character(), AUPR=double())

    if(any(df_dMean_sVar$pred_type)){
      # get PRAUC for hyper methylated cpgs
      hyperM_predictors <- train_set %>%
        dplyr::select(rownames(df_dMean_sVar[which(df_dMean_sVar$pred_type), ])) %>%
        dplyr::summarise(dplyr::across(.cols=tidyselect:::where(is.numeric),.fns = function(x,truth){
            prroc_prauc_vec(truth = truth, estimate = x)
          },
          truth = train_set$target
        )) %>% t() %>% as.data.frame %>% tibble::rownames_to_column(".id")%>%
        magrittr::set_colnames(c(".id", "AUPR"))%>%
        # attach mean diff, will be used to scale AUPR
        dplyr::mutate(pred_type = "hyper") %>%
        dplyr::mutate(diff_means = df_dMean_sVar[.id, ]$diff_means) %>%
        dplyr::mutate(sum_variance = df_dMean_sVar[.id, ]$sum_variance)
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
        dplyr::select(rownames(df_dMean_sVar[which(!df_dMean_sVar$pred_type), ])) %>%
        dplyr::summarise(dplyr::across(.cols=tidyselect:::where(is.numeric),.fns = function(x,truth){
            prroc_prauc_vec(truth = truth, estimate = 1 - x)
          },
          truth = train_set$target
        )) %>% t() %>% as.data.frame %>% tibble::rownames_to_column(".id")%>%
        magrittr::set_colnames(c(".id", "AUPR"))%>%
        # attach mean diff, will be used to scale AUPR
        dplyr::mutate(pred_type = "hypo") %>%
        dplyr::mutate(diff_means = df_dMean_sVar[.id, ]$diff_means) %>%
        dplyr::mutate(sum_variance = df_dMean_sVar[.id, ]$sum_variance)
        if(scale_scores){
          hypoM_predictors$DiffScaledAUPR <- scales::rescale(
            abs(hypoM_predictors$diff_means),
            to=c(0,1)
          ) * hypoM_predictors$AUPR
        }

    }

    # scale AUPR by absolute mean difference between conditions/classes
    meth_predictors <- rbind(
      hyperM_predictors,
      hypoM_predictors
    ) %>%
      dplyr::arrange(dplyr::desc(AUPR))


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

    if (grepl("parab",method)) {
      res_df <- res_df %>%
        dplyr::mutate(parab_param = parab_res$parabola_param)

      print_timings()
      return(res_df)
    }

    print_timings()
    return(res_df)
  }
  if(method=="oner"){
    # oneRule model
    # Using OneRule model
    oner_predata <- OneR::optbin(target ~ ., data = train_set, method = "naive")
    oner_mod <- OneR::OneR(oner_predata)
    oner_mod <- fix_oner_boundaries(oner_mod)

    holdout_res <- rsample::assessment(split_train_set)
    # print(table(holdout_res$target))
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
    print_timings()
    return(res_df)
  }
}


train_general_model <- function(
  train_data,
  k_folds,
  model_type,
  engine,
  grid_n = 10,
  target_name
){

  target_tbl <- table(train_data$target)

  if (k_folds > target_tbl[which.min(target_tbl)]) {
    k_folds <- target_tbl[which.min(target_tbl)]
    message(paste0("Too few samples for set K in cross-validation"))
    message(paste0("K folds reset to k=", k_folds))
  }

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

  cimpleg_recipe <- recipes::recipe(target~., data=train_data)

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
    general_model <-
      # specify the model
      parsnip::decision_tree(
        # TODO for c50 only one parameter is tuned
        #cost_complexity=tune::tune(),
        #tree_depth=tune::tune(),
        min_n=tune::tune()
      ) %>%
      parsnip::set_engine("C5.0")
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
        tree_depth = tune::tune(),
        trees = tune::tune(),
        min_n = tune::tune(),
        mtry = tune::tune()
      ) %>%
      parsnip::set_engine("xgboost")
  }

  if (model_type == "mlp") {

    #FIXME Doing filtering should be an option
    #FIXME Maximum number of weights should be an option
    #FIXME "MaxNWts"

    corr_filter <- cimpleg_recipe %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_corr(recipes::all_predictors(),threshold = .5) %>%
      recipes::step_lincomb(recipes::all_predictors())

    cimpleg_recipe <- recipes::prep(
      corr_filter, training = train_data
    )

    #FIXME need to use bake later?
    # filtered_te <- recipes::bake(filter_obj, test_data)

    general_model <-
      # specify the model
      parsnip::mlp(
        hidden_units = tune::tune(),
        penalty = tune::tune(),
        epochs = tune::tune()
      ) %>%
      parsnip::set_engine("nnet")
  }

  if (model_type == "rand_forest") {
    general_model <-
      # specify the model
      parsnip::rand_forest(
        trees = tune::tune(),
        min_n = tune::tune(),
        mtry = tune::tune()
      ) %>%
      parsnip::set_engine("ranger")
  }


  general_model <- general_model %>%
    # choose either the continuous regression or binary classification mode
    parsnip::set_mode("classification")

  cimpleg_workflow <- workflows::workflow()%>%
    workflows::add_recipe(cimpleg_recipe) %>%
    workflows::add_model(general_model)

  # Training model
  cimpleg_res <- cimpleg_workflow %>%
    tune::tune_grid(
      resamples=f_data,
      grid=grid_n,
      metrics=yardstick::metric_set(
        yardstick::pr_auc,
        yardstick::roc_auc,
        yardstick::accuracy
      )
    )

  cimpleg_final_model <- cimpleg_workflow %>%
    tune::finalize_workflow(
      tune::select_best(cimpleg_res,metric = "pr_auc")
    ) %>%
    parsnip::fit(data = train_data)

  print_timings()
  return(cimpleg_final_model)
}


#' Loop iterating through parabola selecting cpgs
#' @return list
parabola_iter_loop <- function(
  df_diffmean_sumvar,
  init_step,
  step_increment,
  min_feat_search,
  pred_type
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
  while (updt_selected_feats(df_diffmean_sumvar, min_feat_search,pred_type) & i_iter < 100) {
    df_diffmean_sumvar$select_feature <- select_features(
      x = df_diffmean_sumvar$diff_means,
      y = df_diffmean_sumvar$sum_variance,
      a = init_fs
    )
    final_param <- init_fs
    init_fs <- init_fs + step_increment
    i_iter <- i_iter + 1
  }
  message(paste0("Fold parabola parameter: ", final_param))

  df_diffmean_sumvar <- df_diffmean_sumvar[which(df_diffmean_sumvar$select_feature), ]

  return(list(
    df_diffmean_sumvar=df_diffmean_sumvar,
    parabola_param=final_param
  ))
}



#' Feature selection function
#' @return bool vector
select_features <- function(x, y, a) {
  return(y < (a * x)^2)
}

#' Depending on pred_type:
#'   Returns TRUE if:
#'     less than feat_threshold hyper CpGs have been selected
#'     less than feat_threshold hypo CpGs have been selected
#'   Returns FALSE if:
#'     more than feat_threshold hyper and hypo CpGs have been selected
updt_selected_feats <- function(
  df_diffmean_sumvar,
  feat_threshold = 10,
  pred_type=c("both","hyper","hypo")
) {
  expected_cols <- c("select_feature", "diff_means", "sum_variance","pred_type")
  if (!all(expected_cols %in% colnames(df_diffmean_sumvar))) {
    stop("Something went wrong when creating the diff_means sum_variance data.frame!")
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

#' Fixes lower and upper boundaries (0,1) on OneR models
#'
#' @return OneR object
fix_oner_boundaries <- function(oner_mod){
  boundary_vals_l <- as.numeric(
    strsplit(x = gsub("\\(|\\]", "", (names(oner_mod$rules)[1])), ",", fixed = TRUE)[[1]]
  )
  boundary_vals_h <- as.numeric(
    strsplit(x = gsub("\\(|\\]", "", (names(oner_mod$rules)[2])), ",", fixed = TRUE)[[1]]
  )
  boundary_vals_l[1] <- ifelse(boundary_vals_l[1] > 0, -0.001, boundary_vals_l[1])
  boundary_vals_h[2] <- ifelse(boundary_vals_h[2] < 1, 1.001, boundary_vals_h[2])

  fixed_interval <- levels(OneR:::CUT(0, c(boundary_vals_l, boundary_vals_h)))
  names(oner_mod$rules) <- fixed_interval
  dimnames(oner_mod$cont_table)[[2]] <- fixed_interval
  return(oner_mod)
}

#' defining PRROC PRAUC
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

#' @export prroc_prauc
prroc_prauc <- function(data, ...) {
  UseMethod("prroc_prauc")
}




#' @export
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

#' timing func
print_timings <- function(quiet = FALSE) {
  tictoc::toc(quiet = quiet)
}


make_train_test_split <- function(train_d,train_targets,targets,prop=0.75){

  if(is.null(names(targets))) names(targets) <- targets

  # FIXME this is really ugly...
  target_strat <- purrr::map_dfr(
    .x = targets,
    .f = function(target) {
      res <- ifelse(train_targets[, target] == 1, target, train_targets[, target])
      return(res)
    }, .id = "id"
  ) %>%
    tidyr::unite(col = "merged") %>%
    dplyr::mutate(merged = gsub("[0]+", "", merged)) %>%
    dplyr::mutate(merged = gsub("^[_]+", "", merged)) %>%
    dplyr::mutate(merged = gsub("[_]+$", "", merged)) %>%
    dplyr::mutate(merged = ifelse(merged == "", 0, merged))


  part_d <- rsample::initial_split(
    data=train_d%>%dplyr::mutate(target_strat=target_strat%>%dplyr::pull(merged)),
    prop=prop,
    strata="target_strat"
  )

  tmp_train <- rsample::training(part_d)
  tmp_test <- rsample::testing(part_d)

  new_train_targets <- tmp_train %>%
    tibble::rownames_to_column("id") %>%
    dplyr::mutate(tmp_value = 1) %>%
    tidyr::pivot_wider(
      id_cols = id,
      names_from = target_strat,
      values_from = tmp_value
    ) %>%
    dplyr::select(id,dplyr::all_of(targets)) %>%
    dplyr::mutate_all(tidyr::replace_na, 0) %>%
    tibble::column_to_rownames("id")


  new_test_targets <- tmp_test %>%
    tibble::rownames_to_column("id") %>%
    dplyr::mutate(tmp_value = 1) %>%
    tidyr::pivot_wider(
      id_cols = id,
      names_from = target_strat,
      values_from = tmp_value
    ) %>%
    dplyr::select(id,dplyr::all_of(targets)) %>%
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


compute_diffmeans_sumvar <- function(data, target_vector) {

  data <- as.matrix(data)

  id <- colnames(data)

  diff_means <-
    colMeans(data[target_vector, ]) - colMeans(data[!target_vector, ])

  sum_variance <- {
    matrixStats::colVars(data[target_vector, ]) +
      matrixStats::colVars(data[!target_vector, ])
  }

  df_diffmean_sumvar <- data.frame(
    "id" = id,
    "diff_means" = diff_means,
    "sum_variance" = sum_variance
  )

  df_diffmean_sumvar$pred_type <- df_diffmean_sumvar$diff_means >= 0

  return(df_diffmean_sumvar)
}
