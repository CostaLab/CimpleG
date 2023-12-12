
# setup_project
usethis::proj_activate(".")

# ignore
usethis::use_build_ignore(c(
  "test_data",
  "devel",
  "update_version.sh",
  "setup_proj.R",
  "README.html",
  "temp_test.R",
  "init.r"
))

# vignettes
usethis::use_vignette("generate-signatures","Generate signatures")
usethis::use_vignette("save_load_objects","Quickly save and load (large) objects")

# misc
# usethis::use_citation()
# usethis::use_spell_check(vignettes = TRUE, lang = "en-US", error = FALSE) # run manually


# use pkg
usethis::use_package("tictoc")
usethis::use_package("rsample")
usethis::use_package("yardstick")
usethis::use_package("caret")
usethis::use_package("OneR")
usethis::use_package("dplyr")
usethis::use_package("tibble")
usethis::use_package("parsnip")
usethis::use_package("matrixStats")
usethis::use_package("magrittr")
usethis::use_package("assertthat")
usethis::use_package("purrr")
usethis::use_package("recipes")
usethis::use_package("tune")
usethis::use_package("workflows")
usethis::use_package("tidyr")
usethis::use_package("rlang")
usethis::use_package("scales")
usethis::use_package("tidyselect")
usethis::use_package("butcher")
usethis::use_package("grDevices")
usethis::use_package("data.table")
usethis::use_package("nnls")
usethis::use_package("ggsci")
usethis::use_package("patchwork")
usethis::use_package("ggExtra")
usethis::use_package("devtools")
usethis::use_package("ggplot2")
usethis::use_package("ggrepel")
usethis::use_package("gtools")
usethis::use_package("forcats")
usethis::use_package("archive")
usethis::use_package("tsutils")
usethis::use_package("stats")
usethis::use_package("methods")
usethis::use_package("vroom")
usethis::use_package("broom")

# suggested
import_tag <- "suggests" # "Imports" "suggests"
usethis::use_package("withr", import_tag)
usethis::use_package("stringr", import_tag)
usethis::use_package("reshape2", import_tag)
usethis::use_package("future", import_tag)
usethis::use_package("furrr", import_tag)
usethis::use_package("xgboost", import_tag)
usethis::use_package("glmnet", import_tag)
usethis::use_package("C50", import_tag)
usethis::use_package("ranger", import_tag)
usethis::use_package("nnet", import_tag)
usethis::use_package("circlize", import_tag)
usethis::use_package("NMF", import_tag)
usethis::use_package("RColorBrewer", import_tag)
usethis::use_package("future.apply", import_tag)
usethis::use_package("ggbeeswarm", import_tag)
usethis::use_package("ggsignif", import_tag)
usethis::use_package("plyr",import_tag) # only used in a single place, check to rm
usethis::use_package("mltools", import_tag)
usethis::use_package("Rfast",import_tag)
usethis::use_package("GEOquery",import_tag)
usethis::use_package("biomaRt",import_tag)
usethis::use_package("spelling",import_tag)

# remotes
usethis::use_dev_package("Biobase", import_tag, "Biobase")
usethis::use_dev_package("SummarizedExperiment", import_tag, remote="SummarizedExperiment")
usethis::use_dev_package("minfi", import_tag, remote="minfi")
usethis::use_dev_package("EpiDISH", import_tag, "EpiDISH")

# import from
usethis::use_package_doc()
usethis::use_import_from("grDevices", c("colorRampPalette", "dev.off", "pdf", "png"))
usethis::use_import_from("methods", "is")
usethis::use_import_from("stats", "predict")
usethis::use_import_from("utils", "head")
usethis::use_import_from("utils", "download.file")
usethis::use_import_from("rlang", "abort")
usethis::use_import_from("dplyr", "%>%")
usethis::use_import_from("rlang", ".data")
usethis::use_data_table()

usethis::use_testthat(parallel = TRUE)
usethis::use_test("CimpleG")
usethis::use_gpl_license()

# fixed standard
usethis::use_tidy_description()



