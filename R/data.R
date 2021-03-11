
#' Cell line train data
#' @format A matrix with 1000 CpGs/features/variables as columns and 409 samples as rows
"train_data"

#' Cell line test data
#' @format A matrix with 1000 CpGs/features/variables as columns and 170 samples as rows
"test_data"

#' Cell line train data targets
#' @format A data frame with 18 variables for 409 samples as rows.
#' \describe{
#' \item{\code{GSM}}{GSM identifier of the sample}
#' \item{\code{CELL_TYPE}}{the cell type of the respective sample}
#' \item{\code{CELL_TYPE_ADIPOCYTES}}{one-hot encoded (1 or 0) column defining if a given sample is an adipocyte}
#' \item{\code{CELL_TYPE_ASTROCYTES}}{one-hot encoded (1 or 0) column defining if a given sample is an astrocyte}
#' \item{\code{CELL_TYPE_BLOOD.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is a blood cell}
#' \item{\code{CELL_TYPE_ENDOTHELIAL.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an endothelial cell}
#' \item{\code{CELL_TYPE_EPIDERMAL.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an epidermal cell}
#' \item{\code{CELL_TYPE_EPITHELIAL.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an epithelial cell}
#' \item{\code{CELL_TYPE_FIBROBLASTS}}{one-hot encoded (1 or 0) column defining if a given sample is a fibroblast}
#' \item{\code{CELL_TYPE_GLIA}}{one-hot encoded (1 or 0) column defining if a given sample is a glia cell}
#' \item{\code{CELL_TYPE_HEPATOCYTES}}{one-hot encoded (1 or 0) column defining if a given sample is an hepatocyte}
#' \item{\code{CELL_TYPE_INDUCED.PLURIPOTENT.STEM.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an iPSC}
#' \item{\code{CELL_TYPE_MESENCHYMAL.STEM.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an MSC}
#' \item{\code{CELL_TYPE_MUSCLE.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle cell}
#' \item{\code{CELL_TYPE_NEURONS}}{one-hot encoded (1 or 0) column defining if a given sample is a neuron}
#' \item{\code{CELL_TYPE_MUSCLE.STEM.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle stem cell}
#' \item{\code{CELL_TYPE_MSCORFIBRO}}{one-hot encoded (1 or 0) column defining if a given sample is an MSC or a fibroblast}
#' \item{\code{GROUP_DATA}}{to which dataset these data belong to (\code{TRAIN} or \code{TEST})}
#' }
"train_targets"

#' Cell line test data targets
#' @format A data frame with 18 variables for 170 samples as rows.
#' \describe{
#' \item{\code{GSM}}{GSM identifier of the sample}
#' \item{\code{CELL_TYPE}}{the cell type of the respective sample}
#' \item{\code{CELL_TYPE_ADIPOCYTES}}{one-hot encoded (1 or 0) column defining if a given sample is an adipocyte}
#' \item{\code{CELL_TYPE_ASTROCYTES}}{one-hot encoded (1 or 0) column defining if a given sample is an astrocyte}
#' \item{\code{CELL_TYPE_BLOOD.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is a blood cell}
#' \item{\code{CELL_TYPE_ENDOTHELIAL.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an endothelial cell}
#' \item{\code{CELL_TYPE_EPIDERMAL.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an epidermal cell}
#' \item{\code{CELL_TYPE_EPITHELIAL.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an epithelial cell}
#' \item{\code{CELL_TYPE_FIBROBLASTS}}{one-hot encoded (1 or 0) column defining if a given sample is a fibroblast}
#' \item{\code{CELL_TYPE_GLIA}}{one-hot encoded (1 or 0) column defining if a given sample is a glia cell}
#' \item{\code{CELL_TYPE_HEPATOCYTES}}{one-hot encoded (1 or 0) column defining if a given sample is an hepatocyte}
#' \item{\code{CELL_TYPE_INDUCED.PLURIPOTENT.STEM.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an iPSC}
#' \item{\code{CELL_TYPE_MESENCHYMAL.STEM.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is an MSC}
#' \item{\code{CELL_TYPE_MUSCLE.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle cell}
#' \item{\code{CELL_TYPE_NEURONS}}{one-hot encoded (1 or 0) column defining if a given sample is a neuron}
#' \item{\code{CELL_TYPE_MUSCLE.STEM.CELLS}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle stem cell}
#' \item{\code{CELL_TYPE_MSCORFIBRO}}{one-hot encoded (1 or 0) column defining if a given sample is an MSC or a fibroblast}
#' \item{\code{GROUP_DATA}}{to which dataset these data belong to (\code{TRAIN} or \code{TEST})}
#' }
"test_targets"
