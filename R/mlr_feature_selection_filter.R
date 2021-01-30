

# TODO checkUsage from codetools package

# feature subset selection
mlr_feature_filter <- function(
  df_matrix,
  resp_name,
  learner_name="classif.randomForest",
  filter_method=c("aupr_custom_filter","auc"),
  ratioFeatKept=0.05,
  cv_iter=10,
  cluster_cpus=5,
  output_file_path=NULL
)
{
  t_start <- Sys.time()

  # Check args
  filter_method = match.arg(filter_method)
  if(cluster_cpus < 1) cluster_cpus = 1

  # Assertions
  assertthat::assert_that(
    class(df_matrix)%in%c("data.frame","data.frame.matrix"),
    msg="Input data needs to be a 'data.frame' or 'data.frame.matrix'"
  )


  assertthat::assert_that(assertthat::is.string(resp_name))

  if(!is.null(output_file_path)){
    assertthat::assert_that(assertthat::is.writeable(dirname(output_file_path)))
    assertthat::assert_that(
      assertthat::has_extension(path=output_file_path,ext="rds") ||
      assertthat::has_extension(path=output_file_path,ext="RDS")
    )
  }

  # reset cv iter if size of target class is too small
  if(any(table(df_matrix$Class)<cv_iter)){
    cv_iter <- table(df_matrix$Class)[which(table(df_matrix$Class)<cv_iter)]
  }

  classif_task <- mlr::makeClassifTask(
    id=resp_name,
    data=df_matrix,
    target="Class",
    positive="1"
  )

  learner <- mlr::makeFilterWrapper(
    learner=mlr::makeLearner(cl=learner_name, predict.type="prob"),
    fw.method=filter_method,
    fw.perc=ratioFeatKept
  )

  parallelMap::parallelStartMulticore(cluster_cpus)

  resampleDesc <- mlr::makeResampleDesc(
    method="CV",
    iter=cv_iter,
    stratify=TRUE
  )

  res <- mlr::resample(
    task=classif_task,
    learner=learner,
    resampling=resampleDesc,
    measures=list(aupr_custom_measure,mmce),
    show.info=TRUE,
    models=TRUE,
    keep.pred=FALSE
  )

  parallelMap::parallelStop()

  selected_feats <- sapply(res$models, mlr::getFilteredFeatures)

  tbl_feats<-table(selected_feats)
  #hist(tbl_feats)

  print(res)
  sel_feats <- names(tbl_feats)
  message(paste0("n feats selected: ",length(sel_feats)))

  message(paste0("Total runtime: ", format(as.difftime(Sys.time() - t_start, units="hours"))))

  if(!is.null(output_file_path)){
    message(paste0("Saving output to file: ",output_file_path))
    saveRDS(object=sel_feats,file=output_file_path,compress=FALSE)
  }

  return(sel_feats)
}
