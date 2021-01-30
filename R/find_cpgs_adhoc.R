



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

  fold_list <- caret::createFolds(y=target_vec, k=k_folds, list=TRUE)


  f_data = rsample::vfold_cv(
    train_data,
    v=k_folds,
    repeats=n_repeats,
    strata=target_col
  )

  f_data$results <- map(
    f_data$splits,
    find_predictors
  )


}





find_predictors <- function(
  split_train_set,
  response_vector,
  init_step=0.1,
  step_increment=0.1,
  min_feat_search=10,
  p_class = "positive_class",
  do_parab=FALSE
){
  # Get feature space (diffMeans,sumVar)
  # df_dMean_sVar <- getDiffMeansSumVariance(
  #   data=train_set,
  #   response_vector=response_vector,
  #   assay_name=assay_name)

  train_set = rsample::analysis(f_data$splits[[1]])
  tru_v = train_set$target == p_class
  train_set$target=NULL

  df_dMean_sVar <- data.frame(
    "diffMeans"=colMeans(train_set[tru_v,]) - colMeans(train_set[!tru_v,]),
    "sumVariance"=(
      matrixStats::colVars(as.matrix(train_set[tru_v,])) +
      matrixStats::colVars(as.matrix(train_set[!tru_v,]))
    )
  )

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
  }

  df_dMean_sVar$predType <- df_dMean_sVar$diffMeans >= 0

  train_set <- train_set[,rownames(df_dMean_sVar)]

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

  # scale AUPR by absolute mean difference between conditions/classes
  meth_predictors = rbind(
      hyperM_predictors,
      hypoM_predictors
    ) %>%
    mutate(DiffScaledAUPR = abs(diffMeans) * AUPR)

  # TODO Scale AUPR by scaled values of diffmeans?
  # meth_predictors$DiffScaledAUPR <- scales::rescale(
  #   abs(meth_predictors$diffMeans),
  #   to=c(0,1)
  # ) * meth_predictors$AUPR


  meth_predictors %>%
    slice_max(AUPR,n = 10) %>%
    ggplot(.,aes(x=diffMeans,y=sumVariance,col=AUPR))+
    geom_point()+
    scale_colour_gradientn(
      colours=colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")))(10),
      limits=c(0,1)
    )+
    lims(x = c(-1,1), y = c(0,.25))+
    theme_minimal()

  meth_predictors %>%
    slice_max(AUPR,n = 1000) %>%
    ggplot(.,aes(x=diffMeans,y=sumVariance,col=Sensitivity))+
    geom_point()+
    scale_colour_gradientn(
      colours=colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")))(10),
      limits=c(0,1)
    )+
    lims(x = c(-1,1), y = c(0,.25))+
    theme_minimal()


  train_set[,rownames(df_dMean_sVar[!df_dMean_sVar$predType,])]






  meth_predictors %>% filter(predType == "hypo")%>%
    slice_max(AUPR,n = 100)


    meth_predictors[meth_predictors$`.id`=="cg14021880",]
    meth_predictors[meth_predictors$`.id`=="cg26461417",]
    meth_predictors[meth_predictors$`.id`=="cg23922560",]
tictoc::tic("PRROC")
  PRROC::pr.curve(
    scores.class0=train_set[,c("cg14021880")],
    weights.class0=ifelse(tru_v,1,0),
    curve=FALSE,
    max.compute=FALSE,
    min.compute=FALSE,
    rand.compute=FALSE,
    dg.compute=FALSE
  )
tictoc::toc()
tictoc::tic("MLmetrics")
MLmetrics::PRAUC(
  train_set[,c("cg14021880")],ifelse(tru_v,1,0)
)
tictoc::toc()

# yardstick::metrics()
  tictoc::tic("yardstick")
    yardstick::pr_auc(
      data=data.frame(
        estimate1 = train_set[,c("cg14021880")],
        estimate2 = train_set[,c("cg23922560")],
        truth = factor(ifelse(tru_v,1,0))
      ),truth,1
    )
  tictoc::toc()



  plot(PRROC::pr.curve(
    scores.class0=train_set[,"cg23922560"],
    weights.class0=ifelse(!tru_v,1,0),
    curve=TRUE,
    max.compute=FALSE,
    min.compute=FALSE,
    rand.compute=FALSE,
    dg.compute=FALSE
  ))


  # merge results and sort by scaled
  cpg_predictors <- rbind(hyperM_predictors,hypoM_predictors)
  cpg_predictors <- cpg_predictors[order(
    cpg_predictors$DiffScaledAUPR,
    cpg_predictors$AUPR,
    cpg_predictors$F1,
    decreasing=TRUE),]


  cpg_predictors$parabolaSearchParam <- final_param

  rownames(cpg_predictors) <- cpg_predictors$.id

  return(cpg_predictors)
}

validate_predictors <- function(){
  ## TODO
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
