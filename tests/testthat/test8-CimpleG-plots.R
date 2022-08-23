
# normal execution of CimpleG
set.seed(42)
res <- CimpleG(
  train_data = train_data,
  train_targets = train_targets,
  test_data = test_data,
  test_targets = test_targets,
  method = "CimpleG",
  target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
  verbose=0
)

test_targets$targets <- paste0("CELL_TYPE_",test_targets$CELL_TYPE)

sig_vec <- c(CELL_TYPE_MSCORFIBRO="cg03369247", CELL_TYPE_NEURONS="cg24548498")
sig_list <-list(CELL_TYPE_MSCORFIBRO="cg03369247", CELL_TYPE_NEURONS="cg24548498")

test_that("signature_plot works with CimpleG, character vectors and lists", {

  plt_res <- signature_plot(res, test_data, test_targets, "GSM", "targets")
  plt_vec <- signature_plot(sig_vec, test_data, test_targets, "GSM", "targets")
  plt_list <- signature_plot(sig_list, test_data, test_targets, "GSM", "targets")

  expect_s3_class(plt_res$plot, "ggplot")
  expect_s3_class(plt_vec$plot, "ggplot")
  expect_s3_class(plt_list$plot, "ggplot")
  expect_equal(plt_res, plt_vec)
  expect_equal(plt_res, plt_list)
})

