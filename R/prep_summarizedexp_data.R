
prep_summarizedexp_data <- function(data, target_columns){

  is_minfi_class <-
    class(data) %in% c("MethylSet", "GenomicMethylSet", "RGChannelSet", "GenomicRatioSet")

  is_sumexp_class <- is(data) %in% "SummarizedExperiment"

  if(is_minfi_class & requireNamespace("minfi", quietly = TRUE)){

    beta_mat <- minfi::getBeta(data)
    df_targets <- minfi::pData(data)

  }else if(is_sumexp_class & requireNamespace("SummarizedExperiment", quietly = TRUE)){

    is_beta_in_assays <-
      tolower(SummarizedExperiment::assayNames(data)) == "beta"

    assertthat::assert_that(any(is_beta_in_assays))

    # samples as columns
    beta_mat  <- SummarizedExperiment::assays(data)[which(is_beta_in_assays)][[1]]
    df_targets <- SummarizedExperiment::colData(data)
  }else{
    abort(
      paste0(
        "Class of data provided ('",class(data),"'), is not supported.\n",
        "Please provide a SummarizedExperiment object or a ",
        "simple matrix/data frame with your data."
      )
    )
  }

  # make samples as rows
  if(requireNamespace("Rfast", quietly = TRUE)){
    sample_names <- colnames(beta_mat)
    feat_names <- rownames(beta_mat)
    beta_mat <- Rfast::transpose(beta_mat)
    rownames(beta_mat) <- sample_names
    colnames(beta_mat) <- feat_names
  }else{
    warning(paste0(
        "CimpleG can run considerably faster if you have the package `Rfast` installed.\n",
        "Consider installing `Rfast` with `install.packages('Rfast')`\n"
        ))
    if(Sys.info()['sysname']=="Linux"){
      warning(paste0(
        "Since you are using a linux distribution, you might need to install the system library 'libgsl-dev'.\n",
      ))
    }
    beta_mat <- t(beta_mat)
  }
  # make DataFrame into data.frame
  df_targets <- as.data.frame(df_targets)

  assertthat::assert_that(all(target_columns %in% colnames(df_targets)))

  # check which cols don't have their values as 0 or 1
  # these will be the ones to edit
  # cols_to edit is a named vector
  cols_to_edit <- which(sapply(
    df_targets[target_columns],
    function(dcols){!all(dcols %in% c(0,1))}
  ))

  # make cols to edit factor
  df_targets[names(cols_to_edit)] <- lapply(
    df_targets[names(cols_to_edit)],factor
  )

  # find in each col to edit (targets that are not 0/1) which of the values
  # is in a smaller proportion, these values will become 1
  # fetch_col will be a named vector where the values are the minority "class"
  # and the names are the name of the corresponding column
  fetch_col <- sapply(
    df_targets[names(cols_to_edit)],
    function(x){ levels(x)[which.min(tabulate(x))] }
  )

  # make cols one-hot encoded
  df_targets <- as.data.frame(
    mltools::one_hot(
      dt = data.table::as.data.table(df_targets),
      cols = names(cols_to_edit),
      dropCols = FALSE
    )
  )
  # for each col that needs to be edited, fetch it and the corresponding name
  # created by one_hot
  df_targets[,names(fetch_col)] <- lapply(
    names(fetch_col),
    function(x){df_targets[,paste0(x, "_", fetch_col[x])]}
  )

  return(list(beta_mat = beta_mat, df_targets = df_targets))
}

