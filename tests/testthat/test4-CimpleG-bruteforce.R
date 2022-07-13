
test_that("signatures are generated w/ brute_force", {
  # skip("Skipping to speed up testing, brute_force takes too long.")
  set.seed(42)
  sigs <- suppressWarnings(CimpleG(
    train_data = train_data[,1:100],
    train_targets = train_targets,
    test_data = test_data[,1:100],
    test_targets = test_targets,
    method = "brute_force",
    target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
    verbose=0
    ))
  sigs <- sigs$signatures
  expect_identical(sigs,c(CELL_TYPE_MSCORFIBRO="cg17786959",CELL_TYPE_NEURONS="cg10450175"))
})
