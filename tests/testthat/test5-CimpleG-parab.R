
test_that("signatures are generated w/ CimpleG_parab", {
  set.seed(42)
  res <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG_parab",
    target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
    verbose=0
    )

  expect_identical(
    res$signatures,
    c(CELL_TYPE_MSCORFIBRO="cg24192660", CELL_TYPE_NEURONS="cg17008486")
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
    target_columns = "CELL_TYPE_MSCORFIBRO",
    verbose=0
    )
  expect_identical(
    res$signatures,
    c(CELL_TYPE_MSCORFIBRO="cg24192660")
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
    target_columns = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
    verbose=0,
    train_only=TRUE
    )
  expect_identical(
    res$signatures,
    c(CELL_TYPE_MSCORFIBRO="cg24192660", CELL_TYPE_NEURONS="cg17008486")
  )
})
