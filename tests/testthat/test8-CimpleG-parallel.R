
# test parallel
test_that("parallel processing works",{
  skip('parallel not working properly locally')
  # allow for enough obj size to be passed to futures
  library(future)
  gbl_max <- 2048*1024^2 # 1st term in MB
  options(future.globals.maxSize = gbl_max)
  # withr::local_options(list(future.globals.maxSize = gbl_max))

  future::plan(future::multicore, workers = 2)
  set.seed(42)
  res <- cimpleg_result <- CimpleG(
    train_data = train_data,
    train_targets = train_targets,
    test_data = test_data,
    test_targets = test_targets,
    method="CimpleG",
    target_columns = c("blood_cells", "neurons"),
    verbose=0,
    run_parallel=TRUE
  )
  future::plan(future::sequential)

  expect_identical(res$signatures, c(blood_cells="cg04785083", neurons="cg24548498"))
})
future::plan(future::sequential)


