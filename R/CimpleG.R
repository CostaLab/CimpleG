

#' @export
CimpleG <- function(
  train_data,
  test_data,
  train_targets,
  test_targets,
  targets,
  method = c("adhoc", "parab", "oner"),
  k_folds = 10,
  n_repeats = 1
) {
  # TODO make some diagnostic plots

  assertthat::assert_that(all(targets %in% colnames(train_targets)))
  assertthat::assert_that(all(targets %in% colnames(test_targets)))
  assertthat::assert_that(is.numeric(k_folds))
  assertthat::assert_that(is.numeric(n_repeats))
  assertthat::are_equal(nrow(train_data), nrow(train_targets))
  assertthat::are_equal(nrow(test_data), nrow(test_targets))

  method <- match.arg(method, choices = c("adhoc", "parab", "oner"))

  res <- purrr::map(
    targets,
    function(target) {
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

      train_res <- do_cv(
        train_data = train_data,
        method = method,
        k_folds = k_folds,
        n_repeats = n_repeats
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

  return(res)
}


CimpleG_deconvolution <- function(
  CimpleG_result,
  data
){

}
