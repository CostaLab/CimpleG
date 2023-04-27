
#' Cell line train data
#' @format A matrix with beta values for 1000 CpGs. Features/variables as columns and 409 samples as rows
"train_data"

#' Cell line test data
#' @format A matrix with beta values for 1000 CpGs. Features/variables as columns and 170 samples as rows
"test_data"

#' Cell line train data targets
#' @format A data frame with 18 variables for 409 samples as rows.
#' \describe{
#' \item{\code{gsm}}{GSM identifier (GEO accession number) of the sample}
#' \item{\code{cell_type}}{the cell type of the respective sample}
#' \item{\code{adipocytes}}{one-hot encoded (1 or 0) column defining if a given sample is an adipocyte}
#' \item{\code{astrocytes}}{one-hot encoded (1 or 0) column defining if a given sample is an astrocyte}
#' \item{\code{blood_cells}}{one-hot encoded (1 or 0) column defining if a given sample is a blood cell}
#' \item{\code{endothelial_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an endothelial cell}
#' \item{\code{epidermal_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an epidermal cell}
#' \item{\code{epithelial_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an epithelial cell}
#' \item{\code{fibroblasts}}{one-hot encoded (1 or 0) column defining if a given sample is a fibroblast}
#' \item{\code{glia}}{one-hot encoded (1 or 0) column defining if a given sample is a glia cell}
#' \item{\code{hepatocytes}}{one-hot encoded (1 or 0) column defining if a given sample is an hepatocyte}
#' \item{\code{ips_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an ipsc}
#' \item{\code{msc}}{one-hot encoded (1 or 0) column defining if a given sample is an msc}
#' \item{\code{muscle_cells}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle cell}
#' \item{\code{neurons}}{one-hot encoded (1 or 0) column defining if a given sample is a neuron}
#' \item{\code{muscle_sc}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle stem cell}
#' \item{\code{group_data}}{to which dataset these data belong to (\code{train} or \code{test})}
#' \item{\code{description}}{the cell type of the respective sample, in long form}
#' }
"train_targets"

#' Cell line test data targets
#' @format A data frame with 18 variables for 170 samples as rows.
#' \describe{
#' \item{\code{gsm}}{GSM identifier (GEO accession number) of the sample}
#' \item{\code{cell_type}}{the cell type of the respective sample}
#' \item{\code{adipocytes}}{one-hot encoded (1 or 0) column defining if a given sample is an adipocyte}
#' \item{\code{astrocytes}}{one-hot encoded (1 or 0) column defining if a given sample is an astrocyte}
#' \item{\code{blood_cells}}{one-hot encoded (1 or 0) column defining if a given sample is a blood cell}
#' \item{\code{endothelial_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an endothelial cell}
#' \item{\code{epidermal_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an epidermal cell}
#' \item{\code{epithelial_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an epithelial cell}
#' \item{\code{fibroblasts}}{one-hot encoded (1 or 0) column defining if a given sample is a fibroblast}
#' \item{\code{glia}}{one-hot encoded (1 or 0) column defining if a given sample is a glia cell}
#' \item{\code{hepatocytes}}{one-hot encoded (1 or 0) column defining if a given sample is an hepatocyte}
#' \item{\code{ips_cells}}{one-hot encoded (1 or 0) column defining if a given sample is an ipsc}
#' \item{\code{msc}}{one-hot encoded (1 or 0) column defining if a given sample is an msc}
#' \item{\code{muscle_cells}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle cell}
#' \item{\code{neurons}}{one-hot encoded (1 or 0) column defining if a given sample is a neuron}
#' \item{\code{muscle_sc}}{one-hot encoded (1 or 0) column defining if a given sample is a muscle stem cell}
#' \item{\code{group_data}}{to which dataset these data belong to (\code{train} or \code{test})}
#' \item{\code{description}}{the cell type of the respective sample, in long form}
#' }
"test_targets"
