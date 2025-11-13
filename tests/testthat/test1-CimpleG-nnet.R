test_that("signatures are generated w/ nnet", {
  skip_on_ci() # test might take too long to execute
  skip_on_cran() # test might fail with different seed
  set.seed(42)
  res <- suppressWarnings(CimpleG(
    train_data = train_data[, 1:100],
    train_targets = train_targets,
    test_data = test_data[, 1:100],
    test_targets = test_targets,
    method = "mlp",
    target_columns = c("blood_cells", "neurons"),
    verbose = 0
  ))
  res <- c(
    res$results$blood_cells$test_perf$accuracy,
    res$results$neurons$test_perf$accuracy
  )
  expect_equal(res, c(0.9176471, 0.9882353), tolerance = 0.001)
})
