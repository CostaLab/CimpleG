# Fetch CpG annotation

#' @importFrom dplyr %>%
fetch_annotation <- function(
  data,
  mart_dataset="hsapiens_gene_ensembl"
){
  m_mm <- useMart("ENSEMBL_MART_ENSEMBL", dataset = mart_dataset)
  #ex_dat <- tail(rowData(m_test)[, c("Name", "pos", "chr", "Strand")])

  res_2  <- purrr::map(seq_len(nrow(ex_dat)),function(i_iter){
    cpg_pos <- ex_dat[i_iter, "pos"]
    cpg_strand <- if(ex_dat[i_iter, "Strand"] == "F"){1}
    else if(ex_dat[i_iter, "Strand"] == "R"){-1}
    cpg_chr <- ex_dat[i_iter, "chr"]

    for(i_pos in c(1e2, 1e3, 1e4, 1e5)){
      #chr:start:end:strand eg X:100:10000:-1
      cpg_range <- paste0(
        cpg_chr, ":", (cpg_pos - i_pos), ":", (cpg_pos + i_pos), ":", cpg_strand
      )

      res_db <- getBM(
        attributes = c(
          "transcript_biotype",
          "ensembl_gene_id",
          "external_gene_name", "strand", "chromosome_name",
          "start_position", "end_position"
        ),
        filters = c("chromosomal_region"),
        values = list(cpg_range),
        mart = m_mm
      )
      if(nrow(res_db) != 0){break}
    }
    return(res_db)
  })
  
  return(res_2)

}

