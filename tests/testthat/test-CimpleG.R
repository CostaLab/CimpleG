
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
    target_columns = c("blood_cells", "neurons"),
    verbose = 0
  )

  # check results
  expect_identical(res$signatures, c(blood_cells="cg04785083", neurons="cg24548498"))
})

test_that("CimpleG alias", {

  set.seed(42)
  alias1 <- cimpleg(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose = 0
  )
  set.seed(42)
  alias2 <- cpg(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose = 0
  )

  # check results
  expect_identical(alias1$signatures, c(blood_cells="cg04785083", neurons="cg24548498"))
  expect_identical(alias2$signatures, c(blood_cells="cg04785083", neurons="cg24548498"))
})

test_that("signatures w/ CimpleG when using SummarizedExperiment as input", {

  se_train <- SummarizedExperiment::SummarizedExperiment(
    assays = list(beta=t(train_data)),
    colData = train_targets
  )

  se_test <- SummarizedExperiment::SummarizedExperiment(
    assays = list(beta=t(test_data)),
    colData = test_targets
  )

  set.seed(42)
  res <- CimpleG(
    train_data = se_train,
    test_data = se_test,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
  )

  expect_identical(res$signatures, c(blood_cells="cg04785083", neurons="cg24548498"))
})

test_that("signatures are generated when providing single target column", {
  set.seed(42)
  sigs <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = "blood_cells",
    verbose=0
    )$signatures
  expect_identical(sigs, c(blood_cells="cg04785083"))
})

test_that("signatures are generated when just train_only is set to TRUE", {
  set.seed(42)
  sigs <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0,
    train_only=TRUE
    )$signatures
  expect_identical(sigs,c(blood_cells="cg04785083", neurons="cg24548498"))
})

test_that("only 'hyper' signatures are generated when pred_type is set to 'hyper'", {
  set.seed(42)
  res <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0,
    pred_type = "hyper"
    )
  expect_true(all(res$results$neurons$train_res$train_results$pred_type))
  expect_true(all(res$results$neurons$train_res$dt_dmsv$pred_type))
  expect_true(all(res$results$neurons$test_perf$pred_type))
  expect_true(all(res$results$neurons$train_res$train_results$diff_means > 0))
  expect_true(all(res$results$neurons$train_res$dt_dmsv$diff_means > 0))
  expect_true(all(res$results$neurons$test_perf$diff_means > 0))

  expect_true(all(res$results$blood_cells$train_res$train_results$pred_type))
  expect_true(all(res$results$blood_cells$train_res$dt_dmsv$pred_type))
  expect_true(all(res$results$blood_cells$test_perf$pred_type))
  expect_true(all(res$results$blood_cells$train_res$train_results$diff_means > 0))
  expect_true(all(res$results$blood_cells$train_res$dt_dmsv$diff_means > 0))
  expect_true(all(res$results$blood_cells$test_perf$diff_means > 0))
})

test_that("only 'hypo' signatures are generated when pred_type is set to 'hypo'", {
  set.seed(42)
  res <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0,
    pred_type = "hypo"
    )
  expect_false(all(res$results$neurons$train_res$train_results$pred_type))
  expect_false(all(res$results$neurons$train_res$dt_dmsv$pred_type))
  expect_false(all(res$results$neurons$test_perf$pred_type))
  expect_false(all(res$results$neurons$train_res$train_results$diff_means > 0))
  expect_false(all(res$results$neurons$train_res$dt_dmsv$diff_means > 0))
  expect_false(all(res$results$neurons$test_perf$diff_means > 0))

  expect_false(all(res$results$blood_cells$train_res$train_results$pred_type))
  expect_false(all(res$results$blood_cells$train_res$dt_dmsv$pred_type))
  expect_false(all(res$results$blood_cells$test_perf$pred_type))
  expect_false(all(res$results$blood_cells$train_res$train_results$diff_means > 0))
  expect_false(all(res$results$blood_cells$train_res$dt_dmsv$diff_means > 0))
  expect_false(all(res$results$blood_cells$test_perf$diff_means > 0))
})

test_that("input data is not changed regardless of input format", {

  dat_used <- train_data
  dat_unused <- train_data

  dattrg_used <- train_targets
  dattrg_unused <- train_targets

  tdat_used <- test_data
  tdat_unused <- test_data

  tdattrg_used <- test_targets
  tdattrg_unused <- test_targets

  set.seed(42)
  res <- CimpleG(
    train_data = dat_used,
    train_targets = dattrg_used,
    test_data = tdat_used,
    test_targets = tdattrg_used,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
  )

  expect_identical(dat_used,dat_unused)
  expect_identical(tdat_used,tdat_unused)
  expect_identical(dattrg_used,dattrg_unused)
  expect_identical(tdattrg_used,tdattrg_unused)

  dat_used <- as.data.frame(train_data)
  dat_unused <- as.data.frame(train_data)

  dattrg_used <- as.data.frame(train_targets)
  dattrg_unused <- as.data.frame(train_targets)

  tdat_used <- as.data.frame(test_data)
  tdat_unused <- as.data.frame(test_data)

  tdattrg_used <- as.data.frame(test_targets)
  tdattrg_unused <- as.data.frame(test_targets)

  set.seed(42)
  res <- CimpleG(
    train_data = dat_used,
    train_targets = dattrg_used,
    test_data = tdat_used,
    test_targets = tdattrg_used,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
  )

  expect_identical(dat_used,dat_unused)
  expect_identical(tdat_used,tdat_unused)
  expect_identical(dattrg_used,dattrg_unused)
  expect_identical(tdattrg_used,tdattrg_unused)


  dat_used <- SummarizedExperiment::SummarizedExperiment(
    assays = list(beta=t(train_data)),
    colData = train_targets
  )
  dat_unused <- SummarizedExperiment::SummarizedExperiment(
    assays = list(beta=t(train_data)),
    colData = train_targets
  )

  tdat_used <- SummarizedExperiment::SummarizedExperiment(
    assays = list(beta=t(test_data)),
    colData = test_targets
  )
  tdat_unused <- SummarizedExperiment::SummarizedExperiment(
    assays = list(beta=t(test_data)),
    colData = test_targets
  )

  set.seed(42)
  res <- CimpleG(
    train_data = dat_used,
    test_data = tdat_used,
    method = "CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0
  )

  expect_identical(dat_used,dat_unused)
  expect_identical(tdat_used,tdat_unused)
})




