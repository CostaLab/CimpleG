
test_that("signatures are generated w/ rand_forest", {
  set.seed(42)
  res <- suppressWarnings(suppressMessages(CimpleG(
    train_data = train_data[,1:100],
    train_targets = train_targets,
    test_data = test_data[,1:100],
    test_targets = test_targets,
    method = "rand_forest",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
  )))
  res <- c(
    res$results$blood_cells$test_perf$accuracy,
    res$results$neurons$test_perf$accuracy
  )
  expect_equal(res, c(0.8176471,0.9823529), tolerance=0.001)
})
