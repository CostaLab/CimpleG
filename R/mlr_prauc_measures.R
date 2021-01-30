
# PRAUC measure to be used in mlr since mlr doesnt have it natively
measurePRAUC <- function(probabilities, truth, negative, positive){

  if (is.factor(truth)) {
    i <- as.integer(truth) == which(levels(truth) == positive)
  } else {
    i <- truth == positive
  }
  if (length(unique(i)) < 2L) {
    stop("truth vector must have at least two classes")
  }
  #MLmetrics::PRAUC y_true has to be 0/1
  i <- as.numeric(i)
  return(MLmetrics::PRAUC(y_pred=probabilities,y_true=i))
}


aupr_custom_measure <- mlr::makeMeasure(
  id = "aupr",
  name = "Area under the PR curve",
  properties = c("classif", "req.pred", "req.truth", "req.prob"),
  note="Integral over the graph that results from computing tpr and ppv for many different thresholds.",
  minimize = FALSE, best = 1, worst = 0, #FIXME worst is not 0
  fun=function(task,model,pred,feats,extra.args){
    if (Biobase::anyMissing(pred$data$response) || length(unique(pred$data$truth)) == 1L) {
      return(NA_real_)
    }
    measurePRAUC(mlr::getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  }
)

aupr_custom_filter = mlr::makeFilter(
  name = "aupr_custom_filter",
  desc = "AUPR filter for binary classification tasks",
  pkg = character(0L),
  supported.tasks = "classif",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = mlr::getTaskData(task, target.extra = TRUE)
    score = vapply(X=data$data, FUN=function(xx, yy) {
      pp<-task$task.desc$positive
      nn<-task$task.desc$negative
      yy<-as.character(yy)
      posClass<-measurePRAUC(probabilities=xx,truth=yy,negative=nn,positive=pp)
      # Note the inverted inverted probabilities for the neg class
      negClass<-measurePRAUC(probabilities=1-xx,truth=yy,negative=nn,positive=pp)
      return(max(posClass,negClass))
    }, yy = data$target, FUN.VALUE=numeric(1))
    return(score)
})

