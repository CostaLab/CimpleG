
# create mock data
train_data <- readRDS(
  file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "training_matrix_assay-BETA_data_1.3.0.RDS"
  )
)
train_targets <- readRDS(
  file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "training_reference_CELL_TYPE_OneHotEnc_1.3.0.RDS"
  )
)
train_targets$GROUP_DATA <- "TRAIN"
train_targets <- as.data.frame(train_targets)


test_data <- readRDS(
  file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "testing_matrix_assay-BETA_data_1.3.0.RDS"
  )
)
test_targets <- readRDS(
  file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "testing_reference_CELL_TYPE_OneHotEnc_1.3.0.RDS"
  )
)
test_targets$GROUP_DATA <- "TEST"
test_targets <- as.data.frame(test_targets)

ncpgs <- ncol(train_data)
set.seed(42)
random_cpgs <- sample.int(ncpgs, 10000)

train_data <- train_data[,random_cpgs ]
test_data <- test_data[,random_cpgs ]

data.table::fwrite(
  as.data.frame(train_data),
  file = "data-raw/train_data.csv",
  row.names = TRUE
)
data.table::fwrite(
  as.data.frame(test_data),
  file = "data-raw/test_data.csv",
  row.names = TRUE
)
data.table::fwrite(train_targets, file = "data-raw/train_targets.csv")
data.table::fwrite(test_targets, file = "data-raw/test_targets.csv")



usethis::use_data(train_data, overwrite = TRUE)
usethis::use_data(test_data, overwrite = TRUE)
usethis::use_data(train_targets, overwrite = TRUE)
usethis::use_data(test_targets, overwrite = TRUE)
