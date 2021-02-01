


do_cv = function(
  train_data,
  target_col="target",
  k_folds=10,
  n_repeats=1
){

  if(FALSE){
    # DEBUG
    train_data = readRDS(file.path("data","mock_train.RDS"))
    target_col="target"
    k_folds=10
    n_repeats=1
  }


  assertthat::assert_that(target_col %in% colnames(train_data))

  target_vec = train_data[,target_col]

  rv_tbl <- table(target_vec)

  if(k_folds > rv_tbl[which.min(rv_tbl)]){
    k_folds <- rv_tbl[which.min(rv_tbl)]
    message(paste0("Too few samples for set K in cross-validation"))
    message(paste0("K folds reset to k=",k_folds))
  }

  # fold_list <- caret::createFolds(y=target_vec, k=k_folds, list=TRUE)

  tictoc::tic("1000")
  f_data = rsample::vfold_cv(
    train_data[,c(1:1000,ncol(train_data))],
    v=k_folds,
    repeats=n_repeats,
    strata=dplyr::all_of(target_col)
  )

  f_data$results <- purrr::map(
    .x=f_data$splits,
    .f=find_predictors,
    adhoc=FALSE
  )
  tictoc::toc()



  f_data$results %>%
    bind_rows%>%
    group_by(predictor)%>%
    accuracy(truth, prediction)
  f_data$results %>%
    bind_rows%>%
    group_by(predictor)%>%
    f_meas(truth, prediction)


  f_data$results %>%
    bind_rows%>%
    group_by(resample,predictor)%>%
    # metric_set(accuracy, kap)(truth, prediction)
    accuracy(truth, prediction) %>%
    ungroup %>%
    group_by(predictor)%>%
    summarize(
      acc = mean(.estimate),
      n=length(.estimate))


  f_data$results %>%
    bind_rows%>%
    group_by(resample,predictor)%>%
    # metric_set(accuracy, kap)(truth, prediction)
    f_meas(truth, prediction) %>%
    ungroup %>%
    group_by(predictor)%>%
    summarize(
      mean_f1 = mean(.estimate),
      n=length(.estimate))

  f_data$results %>%
    bind_rows%>%
    group_by(resample) %>%
    group_map(function(x,...){
      res=PRROC::pr.curve(
        scores.class0=x$positive_prob,
        weights.class0=ifelse(x$truth=="positive_class",1,0),
        curve=FALSE,
        max.compute=FALSE,
        min.compute=FALSE,
        rand.compute=FALSE,
        dg.compute=FALSE
      )$auc.integral
      # names(res)=unique(x$predictor)

      return(data.frame(prauc=res,predictor=unique(x$predictor)))
    }) %>%
    bind_rows %>%
    group_by(predictor)%>%
    summarise(prauc = mean(prauc))



  f_data$results %>%
    bind_rows%>%
    group_by(resample,predictor)%>%
    pr_auc(truth, positive_prob)


    data("hpc_cv")

    table(hpc_cv$Resample)

  # assessment(f_data$splits[[10]])

  # class(f_data$results)
  # str(f_data$results,1)
  # str(f_data$results[[1]],1)


  f_data$accuracy <- purrr::map_df(f_data$results, function(x){
    res=mean(x$correct)
    names(res)=unique(x$predictor)
    return(res)
  })


  f_data$AUPR <- map_df(f_data$results, function(x){
    res=PRROC::pr.curve(
      scores.class0=x$positive_prob,
      weights.class0=ifelse(x$truth=="positive_class",1,0),
      curve=FALSE,
      max.compute=FALSE,
      min.compute=FALSE,
      rand.compute=FALSE,
      dg.compute=FALSE
    )$auc.integral
    names(res)=unique(x$predictor)
    return(res)
  })


f_data$results %>% collect_metrics
  metrics(f_data)

  summary(f_data$AUPR)


}





find_predictors <- function(
  split_train_set,
  init_step=0.1,
  step_increment=0.1,
  min_feat_search=10,
  p_class = "positive_class",
  do_parab=FALSE,
  adhoc=TRUE
){

  if(FALSE){
    split_train_set = f_data$splits[[8]]
    # rsample::
    table(rsample::analysis(split_train_set)$target)
    table(rsample::assessment(split_train_set)$target)
  }


  train_set = rsample::analysis(split_train_set)

  tru_v = train_set$target == p_class

  df_dMean_sVar <- data.frame(
    "diffMeans"=(
      colMeans(train_set[tru_v,-ncol(train_set)]) -
      colMeans(train_set[!tru_v,-ncol(train_set)])
    ),
    "sumVariance"=(
      matrixStats::colVars(as.matrix(train_set[tru_v,-ncol(train_set)])) +
      matrixStats::colVars(as.matrix(train_set[!tru_v,-ncol(train_set)]))
    )
  )

  df_dMean_sVar$predType <- df_dMean_sVar$diffMeans >= 0

  if(do_parab){
    init_fs <- init_step
    final_param <- init_fs
    df_dMean_sVar$selectFeat <- select_features(
      x=df_dMean_sVar$diffMeans,
      y=df_dMean_sVar$sumVariance,
      a=init_fs
    )

    # Find features in feature space (diffMeans,sumVar)
    while(updt_selected_feats(df_dMean_sVar,min_feat_search)){

      df_dMean_sVar$selectFeat <- select_features(
        x=df_dMean_sVar$diffMeans,
        y=df_dMean_sVar$sumVariance,
        a=init_fs
      )
      final_param <- init_fs
      init_fs <- init_fs + step_increment

    }
    message(paste0("Fold param:",final_param))

    df_dMean_sVar <- df_dMean_sVar[which(df_dMean_sVar$selectFeat),]
    train_set <- train_set[,rownames(df_dMean_sVar)]
  }

  if(adhoc | do_parab){
    # get performance measures for hyper methylated cpgs
    hyperM_predictors <- apply(
      X = train_set[, rownames(df_dMean_sVar[df_dMean_sVar$predType, ])],
      MARGIN = 2,
      rv = tru_v,
      FUN = calc_performance
    ) %>% plyr::ldply(data.frame)
    # get performance measures for hypo methylated cpgs
    hypoM_predictors <- apply(
      X = 1 - train_set[, rownames(df_dMean_sVar[!df_dMean_sVar$predType, ])],
      MARGIN = 2,
      rv = tru_v,
      FUN = calc_performance
    ) %>% plyr::ldply(data.frame)


    # assign tags for easier identification
    # attach mean diff, will be used to scale AUPR
    hyperM_predictors = hyperM_predictors %>%
      mutate(predType = "hyper") %>%
      mutate(diffMeans = df_dMean_sVar[.id,]$diffMeans) %>%
      mutate(sumVariance = df_dMean_sVar[.id,]$sumVariance)

    hypoM_predictors = hypoM_predictors %>%
      mutate(predType = "hypo") %>%
      mutate(diffMeans = df_dMean_sVar[.id,]$diffMeans) %>%
      mutate(sumVariance = df_dMean_sVar[.id,]$sumVariance)

    # hypoM_predictors %>% slice_max(AUPR,n = 10)
    # hyperM_predictors %>% slice_max(AUPR,n = 10)

    # scale AUPR by absolute mean difference between conditions/classes
    meth_predictors = rbind(
        hyperM_predictors,
        hypoM_predictors
      ) %>%
      mutate(DiffScaledAUPR = abs(diffMeans) * AUPR) %>%
      tibble::column_to_rownames(".id") %>%
      arrange(desc(AUPR))

    # meth_predictors %>% arrange(desc(DiffScaledAUPR))
    # meth_predictors
    best_pred = meth_predictors %>% slice_max(AUPR)


    # TODO Scale AUPR by scaled values of diffmeans?
    # meth_predictors$DiffScaledAUPR <- scales::rescale(
    #   abs(meth_predictors$diffMeans),
    #   to=c(0,1)
    # ) * meth_predictors$AUPR

    # meth_predictors %>%
    #   slice_max(AUPR,n = 1000) %>%
    #   ggplot(.,aes(x=diffMeans,y=sumVariance,col=DiffScaledAUPR))+
    #   geom_point()+
    #   scale_colour_gradientn(
    #     colours=colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")))(10),
    #     limits=c(0,1)
    #   )+
    #   lims(x = c(-1,1), y = c(0,.25))+
    #   theme_minimal()

    holdout_res = rsample::assessment(split_train_set)
    lvls = levels(holdout_res[,target_col])


    if(best_pred$predType=="hyper"){
      pred_res = factor(ifelse(
        holdout_res[,rownames(best_pred)]>=.5,
        lvls[2],lvls[1]
      ),levels=lvls)
    }else{
      pred_res = factor(ifelse(
        1-holdout_res[,rownames(best_pred)]>=.5,
        lvls[2],lvls[1]
      ),levels=lvls)
    }



    # # merge results and sort by scaled
    # cpg_predictors <- rbind(hyperM_predictors,hypoM_predictors)
    # cpg_predictors <- cpg_predictors[order(
    #   cpg_predictors$DiffScaledAUPR,
    #   cpg_predictors$AUPR,
    #   cpg_predictors$F1,
    #   decreasing=TRUE),]
    #
    #
    # cpg_predictors$parabolaSearchParam <- final_param
    #
    # rownames(cpg_predictors) <- cpg_predictors$.id

    res_df = tibble::tibble(
      resample=split_train_set$id%>%unlist,
      samples=rownames(holdout_res),
      predictor=rownames(best_pred),
      truth=holdout_res$target,
      prediction=pred_res,
      correct=pred_res == holdout_res$target
    )


    return(res_df)

  }else{

    # rule based models
    # tree based models
    # oneRule model

    # rsample::analysis(f_data$splits[[1]]) %>% head

    # Using Recursive Partitioning and Regression Trees
    # res = rpart::rpart(target~.,data = rsample::analysis(f_data$splits[[1]]))

    # Using OneRule model
    oner_predata = OneR::optbin(target~.,data = train_set,method="naive")
    oner_mod = OneR::OneR(oner_predata)

    boundary_vals_l = as.numeric(strsplit(x = gsub("\\(|\\]","",(names(oner_mod$rules)[1])),",",fixed = TRUE)[[1]])
    boundary_vals_h = as.numeric(strsplit(x = gsub("\\(|\\]","",(names(oner_mod$rules)[2])),",",fixed = TRUE)[[1]])
    boundary_vals_l[1] = ifelse(boundary_vals_l[1]>0, -0.001,boundary_vals_l[1])
    boundary_vals_h[2] = ifelse(boundary_vals_h[2]<1, 1.001,boundary_vals_h[2])

    fixed_interval = levels(OneR:::CUT(0,c(boundary_vals_l,boundary_vals_h)))
    names(oner_mod$rules) = fixed_interval
    dimnames(oner_mod$cont_table)[[2]] = fixed_interval

    # summary(oner_mod)
    # str(oner_mod)
    # plot(oner_mod)
    holdout_res = rsample::assessment(split_train_set)
    lvls = levels(holdout_res[,target_col])

    oner_pred = factor(predict(oner_mod,holdout_res,type="class"),levels=lvls)
    pred_prob = predict(oner_mod,holdout_res,type="prob")[,"positive_class"]
    # OneR::eval_model(oner_pred,rsample::assessment(split_train_set))

    res_df = tibble::tibble(
      resample=split_train_set$id%>%unlist,
      samples=rownames(holdout_res),
      predictor=oner_mod$feature,
      truth=holdout_res$target,
      prediction=oner_pred,
      correct=oner_pred == holdout_res$target,
      positive_prob=pred_prob
    )

    return(res_df)


    if(FALSE){
      # C50 models
      c5mod = rules::C5_rules(trees = 10,min_n = length(which(tru_v)))

      c5res = c5mod %>%
        parsnip::fit(
          target ~ .,
          data = rsample::analysis(f_data$splits[[1]])
        )

      rsample::analysis(f_data$splits[[1]])$target %>% table

        predict(
          c5res,
          rsample::analysis(f_data$splits[[1]]),
          type="class"
        )%>%table

        #plot(c5res$fit)

      c5res$fit$boostResults %>% arrange(Errors)


        predict(
          c5res,
          rsample::assessment(f_data$splits[[1]]),
          type="class"
        )%>%table

      table(rsample::assessment(f_data$splits[[1]])$target)
    }
  }
}



# TODO make this parameter?
# Feature selection function
select_features <- function(x,y,a){return(y < (a*x)^2)}

updt_selected_feats <- function(df_dMean_sVar,feat_threshold=10){
  # NOTE Helper function
  # Returns TRUE if:
  #   less than feat_threshold hyper CpGs have been selected
  #   less than feat_threshold hypo CpGs have been selected
  # Returns FALSE if:
  #   more than feat_threshold hyper and hypo CpGs have been selected

  if(!all(colnames(df_dMean_sVar)%in%c("selectFeat","diffMeans","sumVariance"))){
    stop("Something went wrong when creating the diffMeans sumVariance data.frame!")
  }
  updt_cond <- (
    length(which(df_dMean_sVar[df_dMean_sVar$diffMeans >= 0,]$selectFeat)) < feat_threshold |
      length(which(df_dMean_sVar[df_dMean_sVar$diffMeans < 0,]$selectFeat)) < feat_threshold
  )
  return(updt_cond)
}


calc_performance <- function(X,rv){

  cm<-caret::confusionMatrix(
    data=factor(X>=0.5,levels=c("TRUE","FALSE")),
    reference=factor(rv,levels=c("TRUE","FALSE")),
    positive="TRUE",
    mode="everything"
  )

  pr<-PRROC::pr.curve(
    scores.class0=X,
    weights.class0=ifelse(rv,1,0),
    curve=FALSE,
    max.compute=FALSE,
    min.compute=FALSE,
    rand.compute=FALSE,
    dg.compute=FALSE
  )

  roc<-PRROC::roc.curve(
    scores.class0=X,
    weights.class0=ifelse(rv,1,0),
    curve=FALSE,
    max.compute=FALSE,
    min.compute=FALSE,
    rand.compute=FALSE
  )

  return(
    data.frame(
      cbind(
        t(cm$overall),
        t(cm$byClass),
        "AUPR"=pr$auc.integral,
        "AUROC"=roc$auc
      )
    )
  )
}
