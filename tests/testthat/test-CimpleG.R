test_that("stub test works", {
  expect_equal(2 * 2, 4)
})

# normal execution of CimpleG
set.seed(42)

# check results
test_that(
  "signatures are generated",
  {expect_identical(
    CimpleG(
      train_data = train_data,
      train_targets = train_targets,
      test_data = test_data,
      test_targets = test_targets,
      method = "CimpleG",
      targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
      verbose=0
    )$signatures,
    c(CELL_TYPE_MSCORFIBRO="cg03369247",CELL_TYPE_NEURONS="cg24548498")
  )}
)

set.seed(42)
cimpleg_result <- CimpleG(
  train_data = train_data,
  train_targets = train_targets,
  test_data = test_data,
  test_targets = test_targets,
  method = "CimpleG",
  targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
  save_dir=".",
  save_format="zstd",
  verbose=0
)
date_tag  <- format(Sys.time(),"%Y%m%d")

test_that(
  "CimpleG properly saves file",
  {
    f_name <- paste0("CimpleG_results_target-multitargets_model-CimpleG_t-",date_tag,"-")
    res <- grep(
      pattern=paste0(f_name,"[0-9]{6}",".rds.zstd"),
      x=list.files(),
      value=TRUE
    )[1]
    expect_true(file.exists(res))
  }
)

f_name <- paste0("CimpleG_results_target-multitargets_model-CimpleG_t-",date_tag,"-")
f_res <- grep(
  pattern=paste0(f_name,"[0-9]{6}",".rds.zstd"),
  x=list.files(),
  value=TRUE
)[1]

test_that(
  "CimpleG result can be properly loaded from file",
  {
    res <- CimpleG:::load_object(f_res)
    expect_s3_class(res,"CimpleG")
  }
)

# cleanup file created
if(file.exists(f_res)){file.remove(f_res)}


# test parallel
test_that(
  "parallel processing works",
  {
    skip('parallel not working properly locally')
    # allow for enough obj size to be passed to futures
    library(future)
    gbl_max <- 2048*1024^2 # 1st term in MB
    options(future.globals.maxSize = gbl_max)

    future::plan(future::multicore)
    set.seed(42)
    cimpleg_result <- CimpleG(
      train_data = train_data[,1:100],
      train_targets = train_targets,
      test_data = test_data[,1:100],
      test_targets = test_targets,
      method="CimpleG",
      targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
      verbose=0,
      run_parallel=TRUE
    )
    future::plan(future::sequential)
  }
)

future::plan(future::sequential)


