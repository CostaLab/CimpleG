# Fetch CpG annotation

#' Get CpG annotation from Illumina
#'
#' @param cpg_id A character vector with the CpG IDs from Illumina to annotate.
#' @param is_epic A boolean, if TRUE, the annotation will be fetched from the EPIC array, otherwise from the 450k array. Default is TRUE.
#' @param short_annotation A boolean, if TRUE, only a small number of columns from the full annotation reference will be kept. This leads to an easier to read output. Default is TRUE.
#' @param silence_warnings A boolean, if TRUE, warnings produced during the downloading and loading of the data will be silenced. Default is TRUE.
#'
#' @return A table with the annotated CpGs in the same order as the provided signatures.
#'
#' @examples
#' \donttest{
#' library("CimpleG")
#'
#' # read data
#' signatures <- c("cg14501977", "cg24548498")
#'
#' # Get signature annotation
#' signature_annotation <- get_cpg_annotation(signatures)
#'
#' # check signature annotation
#' signature_annotation
#' }
#' @export
get_cpg_annotation <- function(
    cpg_id,
    is_epic = TRUE,
    short_annotation = TRUE,
    silence_warnings = TRUE) {
  IlmnID <- NULL

  annot_url <- short_annot_cols <- base_name <- NULL
  ANNOT_COLS_EPIC <- c("IlmnID", "CHR_hg38", "Start_hg38", "End_hg38", "UCSC_RefGene_Name", "UCSC_RefGene_Group", "UCSC_CpG_Islands_Name", "Relation_to_UCSC_CpG_Island")
  ANNOT_COLS_450K <- c("IlmnID", "CHR", "UCSC_RefGene_Name", "UCSC_CpG_Islands_Name", "Relation_to_UCSC_CpG_Island")

  if (is_epic) {
    annot_url <- "https://webdata.illumina.com/downloads/productfiles/methylationEPIC/infinium-methylationepic-v-1-0-b5-manifest-file-csv.zip"
    short_annot_cols <- ANNOT_COLS_EPIC
    base_name <- paste0("ilmn_annot_epic_")
  } else {
    annot_url <- "https://webdata.illumina.com/downloads/productfiles/humanmethylation450/humanmethylation450_15017482_v1-2.csv"
    short_annot_cols <- ANNOT_COLS_450K
    base_name <- paste0("ilmn_annot_450k_")
  }

  annot <- NULL

  tryCatch(
    expr = {
      tmp_files <- list.files(tempdir(), full.names = TRUE)
      if (length(tmp_files[grepl(base_name, tmp_files)]) == 0) {
        tmp <- tempfile(pattern = base_name)
        message("\n[CimpleG] Getting annotation manifest from Illumina.\n")
        download.file(url = annot_url, destfile = tmp)
      } else {
        tmp <- tmp_files[grep(base_name, tmp_files)]
        # there should only be one file that matches the query but just in case this fail, we select the 1st
        tmp <- tmp[1]
      }
      if (silence_warnings) {
        # regardless of platform (epic/450) need to skip 7 lines from manifest
        annot <- suppressWarnings(vroom::vroom(tmp, skip = 7, show_col_types = FALSE))
      } else {
        annot <- vroom::vroom(tmp, skip = 7, show_col_types = FALSE)
      }
    },
    error = function(cond) {
      message("Failed to download/load data.")
      message("Error:")
      message(cond)
    },
    warning = function(cond) {
      message("Warning:")
      message(cond)
    }
  )

  # keep only relevant probes
  annot <- annot |>
    dplyr::filter(IlmnID %in% cpg_id) |>
    dplyr::arrange(match(IlmnID, cpg_id))

  if (short_annotation) {
    annot <- annot |> dplyr::select(dplyr::any_of(short_annot_cols))
    return(annot)
  }

  return(annot)
}
