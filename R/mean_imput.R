
mean_imput <- function(sigs, dat){

  assertthat::assert_that(
    any(sigs %in% colnames(dat)),
    msg = "At least some probes should exist in the data. Otherwise, downstream results will not make sense."
  )

  to_imput <- setdiff(sigs, colnames(dat))

  warn_msg <- ""

  if(length(to_imput) > 15){
    warn_msg <- paste0(
      warn_msg,
      "A total of ",length(to_imput), " probes are missing from the total number of signatures ",
      "(",length(sigs),")", " in the signature set.\n",
      "A simple mean imputation procedure will be applied.\n",
      "Please make sure this makes sense for your data."
    )
  }else{
    warn_msg <- paste0(
      warn_msg,
      "The following probes: ",paste0(to_imput, collapse = ", "), " are missing, from the total number of signatures ",
      "(",length(sigs),")", " in the signature set.\n",
      "A simple mean imputation procedure will be applied.\n",
      "Please make sure this makes sense for your data."
    )
  }
  warning(warn_msg)

  imput_vec <- rowMeans(dat)
  sample_id <- rownames(dat)
  newdat <- data.table::as.data.table(dat)
  newdat[,(to_imput) := imput_vec]
  if(requireNamespace("Rfast", quietly = TRUE)){
    newdat <- Rfast::data.frame.to_matrix(
      newdat,
      col.names = TRUE,
      row.names = FALSE
    )
  }else{
    newdat <- as.matrix(newdat)
  }
  rownames(newdat) <- sample_id
  return(newdat)
}
