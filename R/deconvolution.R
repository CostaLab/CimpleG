#' Perform deconvolution using the results from CimpleG
#' Calculate reference matrix given reference data and targets
#' Compute deconvolution on new data
#'
#' @param CimpleG_result Result from running CimpleG
#' @param reference_data Data to be used as reference
#' @param reference_targets Table with targets in `reference_data`
#' @param targets Targets in `reference_targets` to be used
#' @param new_data Data to deconvolve
#' @param method Deconvolution method to be used
#' @param ... Other parameters to be used by the different deconvolution methods
#' @importFrom dplyr %>%
#' @export
deconvolution <- function(
  CimpleG_result,
  reference_data,
  reference_targets,
  targets,
  new_data,
  method=c("nnls","epidish","nmf"),
  ...
){
  UseMethod("deconvolution")
}

#' @export
deconvolution.CimpleG <- function(
  CimpleG_result,
  reference_data,
  reference_targets,
  targets,
  new_data,
  method=c("nnls","epidish","nmf"),
  ...
){


  signatures <- CimpleG_result$signatures
  # TODO needs to also work with the general sigs
  # TODO assert that signatures exist in ref and new data

  assertthat::assert_that(class(CimpleG_result) %in% "CimpleG")
  assertthat::assert_that(all(signatures %in% colnames(reference_data)))
  assertthat::assert_that(all(signatures %in% colnames(new_data)))

  signatures <- CimpleG_result$signatures

  ref_mat <- compute_deconv_reference(
    signatures = signatures,
    data = reference_data,
    target_table = reference_targets,
    targets = targets
  )

  new_mat <- new_data[, signatures] %>% as.matrix()

  deconv_res <- deconvolution_nnls((ref_mat), t(new_mat))

  return(deconv_res)
}

deconvolution_nnls <- function(
  # Non-Negative Least Squares deconvolution algorithm
  weights_mat, # Features as rows, Classes as columns
  values_mat, # Features as rows, Samples as columns
  iter=2000
){
  #implementation of the NNLS method with update rule of LEE

  # init mat with 1s
  deconvMat <- matrix(1, ncol = ncol(values_mat), nrow = ncol(weights_mat))

  colnames(deconvMat) <- colnames(values_mat)
  rownames(deconvMat) <- colnames(weights_mat)

  tW_V <- t(weights_mat) %*% values_mat
  tW_W <- t(weights_mat) %*% weights_mat

  for(i in 1:iter){
    tW_Wdec <- tW_W %*% deconvMat
    deconvMat <- deconvMat * (tW_V / tW_Wdec)
  }

  # Adjust proportions to sum up to 1
  p_deconvMat <- apply(
    X = deconvMat,
    MARGIN = 2,
    FUN = function(x){x/sum(x)}
  )

  if(any(is.na(p_deconvMat))){
    message("### Careful! Deconvolution result has NA values.")
    message("This is probably due to features/probes being missing in the input matrix 'values_mat'")
  }

  return(p_deconvMat)
}

deconvolution_nmf <- function(
  weights_mat,
  values_mat,
  beta=0.01
){
  # NMF Algorithm - Sparse NMF via Alternating NNLS
  nmf_res = NMF::nmf(
    x=values_mat,
    rank=weights_mat,
    method="snmf/r",
    beta=beta
  )

  # Adjust proportions to sum up to 1
  nmf_H <- apply(
    X = nmf_res@fit@H,
    MARGIN = 2,
    FUN = function(x){x/sum(x)}
  )
  rownames(nmf_H) <- colnames(weights_mat)

  return(nmf_H)
}

deconvolution_epidish <- function(
  weights_mat,
  values_mat,
  epidish_method="CBS",
  epidish_nuv=seq(.1,1,.1),
  epidish_maxit=10000
){

  epidish_res = EpiDISH::epidish(
    beta.m=values_mat,
    ref.m=weights_mat,
    method=epidish_method,
    nu.v=epidish_nuv,
    maxit=epidish_maxit,
    constraint="equality"
  )

  return(t(epidish_res$estF))
}




#' @importFrom dplyr %>%
compute_deconv_reference <- function(
  signatures,
  data,
  target_table,
  targets
){

  data <- data[, signatures]

  ref_deconv_mat <- lapply(
    targets,
    function(target){
      apply(
        X=data,
        MARGIN=2,
        function(X){
          mean(X[unlist(target_table[, target]) %in% 1])
        }
      )
    }
  ) %>%
    magrittr::set_names(targets) %>%
    as.data.frame() %>%
    as.matrix()

  return(ref_deconv_mat)
}
