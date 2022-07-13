
test_that("signatures and deconv w/ ElasticNet", {
  set.seed(42)
  res <- suppressWarnings(CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "logistic_reg",
    target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
    verbose=0
  ))
  res_acc <- c(
    res$results$CELL_TYPE_MSCORFIBRO$test_perf$accuracy,
    res$results$CELL_TYPE_NEURONS$test_perf$accuracy
  )
  expect_equal(res_acc, c(0.9529412,0.9882353), tolerance=0.001)
})
