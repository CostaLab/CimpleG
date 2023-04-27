
test_that("signatures are generated w/ CimpleG_parab", {
  set.seed(42)
  res <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG_parab",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
    )

  expect_identical(
    res$signatures,
    c(blood_cells="cg04785083", neurons="cg17008486")
  )
})

test_that("signatures are generated w/ CimpleG_parab when providing single target column", {
  set.seed(42)
  res <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG_parab",
    target_columns = "blood_cells",
    verbose=0
    )
  expect_identical(
    res$signatures,
    c(blood_cells="cg04785083")
  )
})

test_that("signatures are generated w/ CimpleG_parab when just train_only is set to TRUE", {
  set.seed(42)
  res <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG_parab",
    target_columns = c("blood_cells", "neurons"),
    verbose=0,
    train_only=TRUE
    )
  expect_identical(
    res$signatures,
    c(blood_cells="cg04785083", neurons="cg17008486")
  )
})
