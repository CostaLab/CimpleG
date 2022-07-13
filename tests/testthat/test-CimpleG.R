
# TODO: separate tests to files, by method to ensure all are running properly

test_that("signatures and deconv w/ CimpleG", {

  # normal execution of CimpleG
  set.seed(42)
  res <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
    verbose=0
    )

  # check results
  expect_identical(res$signatures, c(CELL_TYPE_MSCORFIBRO="cg03369247", CELL_TYPE_NEURONS="cg24548498"))
})

test_that("signatures are generated when providing single target column", {
  set.seed(42)
  sigs <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = "CELL_TYPE_MSCORFIBRO",
    verbose=0
    )$signatures
  expect_identical(sigs, c(CELL_TYPE_MSCORFIBRO="cg03369247"))
})

test_that("signatures are generated when just train_only is set to TRUE", {
  set.seed(42)
  sigs <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
    verbose=0,
    train_only=TRUE
    )$signatures
  expect_identical(sigs,c(CELL_TYPE_MSCORFIBRO="cg03369247", CELL_TYPE_NEURONS="cg24548498"))
})

