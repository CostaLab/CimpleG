
test_that("signatures are generated w/ brute_force", {
  # skip("Skipping to speed up testing, brute_force takes too long.")
  set.seed(42)
  sigs <- suppressWarnings(CimpleG(
    train_data = train_data[,1:100],
    train_targets = train_targets,
    test_data = test_data[,1:100],
    test_targets = test_targets,
    method = "brute_force",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
    ))
  sigs <- sigs$signatures
  expect_identical(sigs,c(blood_cells="cg27309110",neurons="cg02829783"))
})
