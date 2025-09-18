
set.seed(42)
cimpleg_result <- CimpleG(
  train_data = train_data,
  train_targets = train_targets,
  test_data = test_data,
  test_targets = test_targets,
  method = "CimpleG",
  target_columns = c("blood_cells", "neurons"),
  save_dir=".",
  save_format="lz4",
  verbose=0
)
date_tag  <- format(Sys.time(),"%Y%m%d")

f_name <- paste0("CimpleG_results_target-multitargets_model-CimpleG_t-",date_tag,"-")
f_res <- grep(
  pattern=paste0(f_name,"[0-9]{6}",".rds.lz4"),
  x=list.files(),
  value=TRUE
)[1]

test_that("CimpleG properly saves file", {
  expect_true(file.exists(f_res))
})

test_that("CimpleG result can be properly loaded from file", {
  res <- load_object(f_res)
  expect_s3_class(res, "CimpleG")
})

test_that("save_object and load_object save and load data properly", {

  withr::with_file("mtcars.rds", {
    save_object(mtcars,file_name = "mtcars.rds",file_format = "lz4")
    expect_true(file.exists("mtcars.rds"))
    load_res <- load_object("mtcars.rds")
    expect_identical(load_res, mtcars)
  })
  withr::with_file("mtcars.rds", {
    save_object(mtcars,file_name = "mtcars.rds",file_format = "bzip2")
    expect_true(file.exists("mtcars.rds"))
    load_res <- load_object("mtcars.rds")
    expect_identical(load_res, mtcars)
  })
  withr::with_file("mtcars.rds", {
    save_object(mtcars,file_name = "mtcars.rds",file_format = "gzip")
    expect_true(file.exists("mtcars.rds"))
    load_res <- load_object("mtcars.rds")
    expect_identical(load_res, mtcars)
  })
  withr::with_file("mtcars.rds", {
    save_object(mtcars,file_name = "mtcars.rds",file_format = "xz")
    expect_true(file.exists("mtcars.rds"))
    load_res <- load_object("mtcars.rds")
    expect_identical(load_res, mtcars)
  })
  withr::with_file("mtcars.rds", {
    save_object(mtcars,file_name = "mtcars.rds",file_format = "nocomp")
    expect_true(file.exists("mtcars.rds"))
    load_res <- load_object("mtcars.rds")
    expect_identical(load_res, mtcars)
  })
})

# Cleanup files created
# Run after all tests
withr::defer(unlink(f_res), teardown_env())
