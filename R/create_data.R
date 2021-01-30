
if(FALSE){#debug

  tm_obj = readRDS(
    file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "training_matrix_assay-BETA_data_1.3.0.RDS"
  ))
  ref_obj = readRDS(
    file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "training_reference_CELL_TYPE_OneHotEnc_1.3.0.RDS"
  ))
  ref_obj$GROUP_DATA = "TRAIN"

  tm_test_obj = readRDS(
    file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "testing_matrix_assay-BETA_data_1.3.0.RDS"
  ))
  ref_test_obj = readRDS(
    file.path(
    "/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/1.3.0",
    "testing_reference_CELL_TYPE_OneHotEnc_1.3.0.RDS"
  ))
  ref_test_obj$GROUP_DATA = "TEST"

  ref_obj_merge = rbind(ref_obj,ref_test_obj)
  tm_obj_merge = rbind(tm_obj,tm_test_obj)

  data_obj = SummarizedExperiment::SummarizedExperiment(
    assays=list(beta=t(tm_obj_merge)),
    colData=ref_obj_merge
  )

  saveRDS(object = data_obj,file = file.path("data","cell_line_data.RDS"))
  
}
