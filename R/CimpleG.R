



# train
train_cimpleg = function(
  data_obj,
  method,
  k_folds,
  n_repeats,
  n_cores,
  max_features,
  param_resolution,
  output_dir
){


  #mlr3
  #mlr3viz
  #mlr3learners
  #precrec

  tsk_cimpleg = mlr3::TaskClassif$new(
    id="CimpleG",
    target="target",
    positive="positive_class",
    backend=data_obj
  )

  lrn_cimpleg = mlr3::lrn("classif.rpart",predict_type = "prob")


  pred = lrn_cimpleg$train(tsk_cimpleg)$predict(tsk_cimpleg)

  pred$confusion

  autoplot(pred, type = "roc")
  autoplot(pred, type = "prc")
  autoplot(pred)
  # merge train+test
  # resample
  # hyperparam tunning


}

# test

adhoc_learner = function(aa=.1){
  lrn_adhoc = mlr3::lrn("classif.log_reg",predict_type = "prob")
  # lrn$param_set





}



find_CpGs = function(
  train_data,
  test_data=.2,
  target="target",
  method=c("adhoc","rf","enet"),
  model_eval_method=c("CV","bootstrap"),
  k_fold=10,
  n_repeats=10,
  ...
){

  # TODO TESTS

  # if test data not provided create it as a subset of the train data
  if(!is.matrix(test_data)){

  }




}






RowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...) / (dim(x)[2] - 1)
}


library("dplyr")
data_obj = readRDS("data/cell_line_data.RDS")

set.seed(42)
random_cpgs = sample.int(nrow(data_obj),1000)
mock_train_data = data_obj[random_cpgs,data_obj$GROUP_DATA=="TRAIN"]
mock_test_data = data_obj[random_cpgs,data_obj$GROUP_DATA=="TEST"]

target = "CELL_TYPE_MSCORFIBRO"
mock_train_df = as.data.frame(t(SummarizedExperiment::assay(mock_train_data,"beta")))
mock_train_df$target = SummarizedExperiment::colData(mock_train_data)[,target]
mock_train_df$target = factor(ifelse(
  mock_train_df$target == 1,
  "positive_class",
  "negative_class"
))
mock_test_df = as.data.frame(t(SummarizedExperiment::assay(mock_test_data,"beta")))
mock_test_df$target = SummarizedExperiment::colData(mock_test_data)[,target]
mock_test_df$target = factor(ifelse(
  mock_test_df$target == 1,
  "positive_class",
  "negative_class"
))
saveRDS(mock_train_df,file=file.path("data","mock_train.RDS"))
saveRDS(mock_test_df,file=file.path("data","mock_test.RDS"))




tsk_cimpleg = mlr3::TaskClassif$new(
  id="CimpleG",
  target="target",
  positive="positive_class",
  backend=backend_data
)

lrn_cimpleg = mlr3::lrn(
  "classif.rpart",
  predict_type = "prob"
)



pred = lrn_cimpleg$train(tsk_cimpleg)$predict(tsk_cimpleg)
adhoc_pred = lrn_adhoc$train(tsk_cimpleg)$predict(tsk_cimpleg)
adhoc_pred$confusion




pred$confusion

mlr3viz::autoplot(pred, type = "roc")
mlr3viz::autoplot(pred, type = "prc")
mlr3viz::autoplot(pred)

mlr3viz::autoplot(adhoc_pred)
mlr3viz::autoplot(adhoc_pred, type = "prc")






train_cimpleg(
  data_obj=mock_train_data,
  method,
  k_folds,
  n_repeats,
  n_cores,
  max_features,
  param_resolution,
  output_dir
)
