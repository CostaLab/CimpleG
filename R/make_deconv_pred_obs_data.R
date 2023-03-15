

make_deconv_pred_obs_data <- function(
  dat, # data.frame with predictions as columns, each row should be a prediction for a given sample and given group/celltype
  true_values_col, # true values should be between 0 and 1
  predicted_cols, # predictions should be between 0 and 1
  sample_id_col,
  group_col # group col should be a factor, otherwise the function will make it a factor
  # returns tibble with nested fields
){
  dfit <- NULL

  assertthat::assert_that(is.data.frame(dat))
  assertthat::assert_that(
    all(c(group_col, predicted_cols, true_values_col) %in% colnames(dat))
  )
  assertthat::assert_that(all(dat[, c(predicted_cols, true_values_col)] >= 0))
  assertthat::assert_that(all(dat[, c(predicted_cols, true_values_col)] <= 1))

  # if grouping column is not a factor, make it a factor
  if(!is.factor(dat[[group_col]])){
    dat <- dat |> dplyr::mutate("{group_col}" := as.factor(!!dplyr::sym(group_col)))
  }
  assertthat::assert_that(is.factor(dat[[group_col]]))

  # make data tbl for each prediction
  names(predicted_cols) <- predicted_cols

  tidy_dat <-
    purrr::map_dfr(.x = predicted_cols,.id="method", .f = function(pcol){
      tidy_subset_dat <-
        dat |>
        tibble::as_tibble() |>
        tidyr::nest(data = -tidyr::all_of(group_col)) |>
        dplyr::mutate(
          # fit on the prediction values
          dfit = purrr::map(data, ~ lm(!!dplyr::sym(pcol) ~ !!dplyr::sym(true_values_col), data = .x)),
          tidied = purrr::map(dfit, broom::tidy),
          glanced = purrr::map(dfit, broom::glance),
          augmented = purrr::map(dfit, broom::augment),
          # metrics (RMSE, R2, AIC)
          metrics = purrr::map_dfr(data, ~ CimpleG:::prediction_stats(
            expected_values = .x[[true_values_col]],
            predicted_values = .x[[pcol]]
          ))
        )
      return(tidy_subset_dat)
    })
  return(tidy_dat)
}
