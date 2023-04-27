
# normal execution of CimpleG
set.seed(42)
res <- CimpleG(
  train_data = train_data,
  train_targets = train_targets,
  test_data = test_data,
  test_targets = test_targets,
  method = "CimpleG",
  target_columns = c("blood_cells", "neurons"),
  verbose=0
)

sig_vec <- c(blood_cells="cg04785083", neurons="cg24548498")
sig_list <-list(blood_cells="cg04785083", neurons="cg24548498")

test_that("signature_plot works with CimpleG, character vectors and lists", {

  plt_res <- signature_plot(res, test_data, test_targets, "gsm", "cell_type")
  plt_vec <- signature_plot(sig_vec, test_data, test_targets, "gsm", "cell_type")
  plt_list <- signature_plot(sig_list, test_data, test_targets, "gsm", "cell_type")

  expect_s3_class(plt_res$plot, "ggplot")
  expect_s3_class(plt_vec$plot, "ggplot")
  expect_s3_class(plt_list$plot, "ggplot")
  expect_equal(plt_res, plt_vec)
  expect_equal(plt_res, plt_list)
})

