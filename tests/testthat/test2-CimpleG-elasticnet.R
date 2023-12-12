
test_that("signatures and deconv w/ ElasticNet", {
  skip_on_ci()
  set.seed(42)
  res <- suppressWarnings(CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "logistic_reg",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
  ))
  res_acc <- c(
    res$results$blood_cells$test_perf$accuracy,
    res$results$neurons$test_perf$accuracy
  )
  expect_equal(res_acc, c(1.0000000, 0.9882353), tolerance=0.001)
})
