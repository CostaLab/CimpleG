
# setup_project
usethis::proj_activate(".")

# ignore
usethis::use_build_ignore(c(
  "test_data",
  "devel",
  "update_version.sh",
  "setup_proj.R"
))

# use pkg
usethis::use_package("tictoc")
usethis::use_package("rsample")
usethis::use_package("yardstick")
usethis::use_package("caret")
usethis::use_package("OneR")
usethis::use_package("dplyr")
usethis::use_package("plyr") # only used in a single place, check to rm
usethis::use_package("tibble")
usethis::use_package("parsnip")
usethis::use_package("rules")
usethis::use_package("matrixStats")
usethis::use_package("magrittr")
usethis::use_package("assertthat")
usethis::use_package("PRROC")
usethis::use_package("devtools")
usethis::use_package("purrr")
usethis::use_package("recipes")
usethis::use_package("tune")
usethis::use_package("workflows")
usethis::use_package("tidyr")
usethis::use_package("rlang")
usethis::use_package("scales")
usethis::use_package("tidyselect")

# suggested
usethis::use_package("stringr", "suggests")
usethis::use_package("reshape2", "suggests")
usethis::use_package("future", "suggests")
usethis::use_package("furrr", "suggests")
usethis::use_package("PMCMRplus", "suggests")
usethis::use_package("xgboost", "suggests")
usethis::use_package("glmnet", "suggests")
usethis::use_package("C50", "suggests")
usethis::use_package("ranger", "suggests")
usethis::use_package("nnet", "suggests")
usethis::use_package("ggplot2", "suggests")
usethis::use_package("ggExtra", "suggests")
usethis::use_package("ggrepel", "suggests")
usethis::use_package("circlize", "suggests")
usethis::use_package("NMF", "suggests")
usethis::use_package("RColorBrewer", "suggests")
usethis::use_package("forcats", "suggests")
usethis::use_package("future.apply", "suggests")
usethis::use_package("ggbeeswarm", "suggests")
usethis::use_package("ggsci", "suggests")
usethis::use_package("ggsignif", "suggests")
usethis::use_package("grDevices", "suggests")

# remotes
usethis::use_dev_package("EpiDISH", "suggests", "bioc::release/EpiDISH")
usethis::use_dev_package("Biobase", "suggests", "bioc::release/Biobase")
usethis::use_dev_package("ComplexHeatmap", "suggests", "bioc::release/ComplexHeatmap")

usethis::use_testthat()
usethis::use_test("CimpleG")
