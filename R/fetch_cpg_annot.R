# Fetch CpG annotation
# TODO: finish implementation
fetch_annotation <- function(
  dat,
  mart_dataset="hsapiens_gene_ensembl"
){
  if(requireNamespace("biomaRt",quietly = TRUE)){
    m_mm <- biomaRt::useMart("ENSEMBL_MART_ENSEMBL", dataset = mart_dataset)
    #dat <- tail(rowData(m_test)[, c("Name", "pos", "chr", "Strand")])

    res_2  <- purrr::map(seq_len(nrow(dat)),function(i_iter){
      cpg_pos <- dat[i_iter, "pos"]
      cpg_strand <- if(dat[i_iter, "Strand"] == "F"){1}
        else if(dat[i_iter, "Strand"] == "R"){-1}
      cpg_chr <- dat[i_iter, "chr"]

      for(i_pos in c(1e2, 1e3, 1e4, 1e5)){
        #chr:start:end:strand eg X:100:10000:-1
        cpg_range <- paste0(
          cpg_chr, ":", (cpg_pos - i_pos), ":", (cpg_pos + i_pos), ":", cpg_strand
        )

        res_db <- biomaRt::getBM(
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
}

