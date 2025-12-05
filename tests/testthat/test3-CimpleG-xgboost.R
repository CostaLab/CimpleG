test_that("signatures are generated w/ xgboost", {
  skip_on_ci()
  skip_on_cran() # test might fail due to rng
  set.seed(42)
  res <- suppressWarnings(suppressMessages(CimpleG(
    train_data = train_data[, 1:100],
    train_targets = train_targets,
    test_data = test_data[, 1:100],
    test_targets = test_targets,
    method = "boost_tree",
    target_columns = c("blood_cells", "neurons"),
    verbose = 0
  )))
  res <- c(
    res$results$blood_cells$test_perf$accuracy,
    res$results$neurons$test_perf$accuracy
  )
  expect_equal(res, c(0.8941176, 0.9294118), tolerance = 0.001)
})
