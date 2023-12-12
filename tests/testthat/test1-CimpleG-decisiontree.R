
test_that("signatures are generated w/ decision_tree", {
  skip_on_ci()
  set.seed(42)
  res <- suppressWarnings(CimpleG(
    train_data = train_data[,1:100],
    train_targets = train_targets,
    test_data = test_data[,1:100],
    test_targets = test_targets,
    method = "decision_tree",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
  ))
  res <- c(
    res$results$blood_cells$test_perf$accuracy,
    res$results$neurons$test_perf$accuracy
  )
  expect_equal(res, c(0.77,0.98), tolerance=0.01)
})
