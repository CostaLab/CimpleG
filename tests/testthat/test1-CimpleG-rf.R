
test_that("signatures are generated w/ rand_forest", {
  set.seed(42)
  res <- suppressWarnings(suppressMessages(CimpleG(
    train_data = train_data[,1:100],
    train_targets = train_targets,
    test_data = test_data[,1:100],
    test_targets = test_targets,
    method = "rand_forest",
    target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
    verbose=0
  )))
  res <- c(
    res$results$CELL_TYPE_MSCORFIBRO$test_perf$accuracy,
    res$results$CELL_TYPE_NEURONS$test_perf$accuracy
  )
  expect_equal(res, c(0.89,0.98), tolerance=0.01)
})
