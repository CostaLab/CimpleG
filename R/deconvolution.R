#' Perform deconvolution on a new set of samples, based on the CimpleG models trained
#'
#' @param cpg_obj A CimpleG object. When creating/training CimpleG the parameter `deconvolution_reference` should be set to `TRUE`.
#' @param new_data Matrix or data.frame that should have the samples you want to perform deconvolution on.
#'  Samples should be in rows and probes/CpGs in columns.
#' @param ref_mat If the CimpleG object does not have the reference matrix, you can provide it here instead. See `make_deconv_ref_matrix`
#' @param deconvolution_method Deconvolution method to be used. One of #TODO
#' @param ... Extra parameters only used when deconvolution_method is set to `NMF`. The most relevant parameter are probably `method` and `beta`.
#' @export
run_deconvolution <- function(
  cpg_obj = NULL,
  new_data = NULL,
  ref_mat = NULL,
  deconvolution_method = c("NNLS","EpiDISH","NMF"),
  ...
){
  UseMethod("run_deconvolution")
}


#' @export
run_deconvolution.CimpleG <- function(
  cpg_obj = NULL,
  new_data = NULL,
  ref_mat = NULL,
  deconvolution_method = c("NNLS","EpiDISH","NMF"),
  ...
){

  assertthat::assert_that(is.CimpleG(cpg_obj))

  if(is.null(ref_mat)){
    ref_mat <- cpg_obj$deconvolution_reference_matrix
  }

  deconv_res <- NULL


  ml_methods <- c("logistic_reg", "decision_tree", "boost_tree","mlp","rand_forest")

  if(cpg_obj$method %in% ml_methods){
    deconv_res <- deconv_ml(
      cpg_obj = cpg_obj,
      new_data = new_data,
      ref_mat = ref_mat,
      deconvolution_method = deconvolution_method,
      ...
    )
  }else{
    deconv_res <- deconv_cpg(
      cpg_obj = cpg_obj,
      new_data = new_data,
      ref_mat = ref_mat,
      deconvolution_method = deconvolution_method,
      ...
    )
  }

  return(deconv_res)
}

#' @export
run_deconvolution.matrix <- function(
  cpg_obj = NULL,
  new_data = NULL,
  ref_mat = NULL,
  deconvolution_method = c("NNLS", "EpiDISH", "NMF"),
  ...
){

  deconv_res <- deconv_cpg(
    ref_mat = ref_mat,
    cpg_obj = cpg_obj,
    new_data = new_data,
    deconvolution_method = deconvolution_method,
    ...
  )

  return(deconv_res)
}


#' NNLS deconvolution
#'
#' @param dt A data.table with the new data with features/predictions on rows and samples on columns.
#' @param compute_cols A character vector with the columns for which the deconvolution algorithm should be ran.
#' @param ref_mat The reference matrix as created by CimpleG.
deconvolution_nnls <- function(dt, compute_cols, ref_mat){

  cell_type <- NULL # R CMD check pass

  # For each computable column, run nnls, normalize/transform into proportion 0-1, add and sort by label
  dt[, (compute_cols) := lapply(.SD, function(x) nnls::nnls(A = as.matrix(ref_mat), b = x)$x), .SDcols = compute_cols][,
    (compute_cols) := lapply(.SD, function(x) x / sum(x)),.SDcols = compute_cols][,
    cell_type := colnames(ref_mat)][gtools::mixedorder(cell_type),]

  return(dt)
}

#' NMF deconvolution
#'
#' @param weights_mat Reference matrix.
#' @param values_mat New data matrix.
#' @param ... Extra parameters to be set NMF options. Most relevant parameters are probably `method` and `beta`.
deconvolution_nmf <- function(
  weights_mat,
  values_mat,
  ...
){

  # W = weights matrix
  # V = values matrix (target)
  # H = mixture matrix (coef)
  init_H <- NMF::rmatrix(
    x = ncol(weights_mat),
    y = ncol(values_mat),
    dimnames = list(colnames(weights_mat), colnames(values_mat))
  )
  init_nmf_mod <- NMF::nmfModel(H = init_H, W = weights_mat)
  nmf_H <- NMF::nmf(x = values_mat, rank = init_nmf_mod, ...) %>% NMF::scoef()

  rownames(nmf_H) <- colnames(weights_mat)

  return(nmf_H)
}


deconvolution_nmf_annls <- function(
  weights_mat,
  values_mat,
  beta = 0.01,
  nrun = 1
){

  # W = weights matrix
  # V = values matrix (target)
  # H = mixture matrix (coef)

  # NMF Algorithm - Sparse NMF via Alternating NNLS
  nmf_H <- NMF::nmf(
    x = values_mat,
    rank = weights_mat,
    method = "snmf/r",
    beta = beta,
    nrun = nrun
    ) %>% NMF::scoef()

  # Adjust proportions to sum up to 1
  # coef(nmf_res) === nmf_res@fit@H
  # nmf_H <- apply(X = coef(nmf_res), MARGIN = 2, FUN = function(x) x / sum(x))
  rownames(nmf_H) <- colnames(weights_mat)

  return(nmf_H)
}


#' EpiDISH deconvolution
#'
#' @param ref_mat Reference matrix.
#' @param new_data New data matrix.
#' @param epidish_method One of `CBS` (Cibersort), `RPC` (Robust Partial Correlations), `CP` (Constrained Projection).
#'  Default is `CBS`. See `EpiDISH` documentation for more information.
#' @param epidish_nuv A vector of candidate values used for svm. Only used when epidish_method is set to `CBS`.
#'  See `EpiDISH` documentation for more information.
#' @param epidish_maxit Integer with the number of max iterations for IWLS (Iterative Weighted Least Squares).
#'  Only used when epidish_method is set to `RPC`.
deconvolution_epidish <- function(
  ref_mat,
  new_data,
  epidish_method="CBS",
  epidish_nuv=seq(.1,1,.1),
  epidish_maxit=10000
){

  epidish_method <- match.arg(epidish_method, choices = c("CBS", "RPC", "CP"))

  epidish_res <- EpiDISH::epidish(
    ref.m=ref_mat,
    beta.m=new_data,
    method=epidish_method,
    nu.v=epidish_nuv,
    maxit=epidish_maxit,
    constraint="equality"
  )

  return(t(epidish_res$estF))
}


#' Build deconvolution reference matrix
#'
#' @param cpg_obj A CimpleG object.
#' @param ref_data A matrix with the reference data to be used to build the reference matrix.
#' @param ref_data_labels A character vector with the true labels of the samples in the `reference_data`.
#' @param method Method used to train models in the CimpleG object. 
#'   If not provided (NULL), method will be taken from the CimpleG object.
#' @export
make_deconv_ref_matrix <- function(
  cpg_obj,
  ref_data,
  ref_data_labels,
  method=NULL
){

  assertthat::assert_that(is.CimpleG(cpg_obj))
  if(is.null(method)) method <- cpg_obj$method

  if(method %in% c("CimpleG","CimpleG_parab","brute_force")){

    ref_pred_res <-
      ref_data %>%
      as.data.frame() %>% 
      dplyr::select(as.character(cpg_obj$signatures)) %>%
      dplyr::mutate(true_class=ref_data_labels) %>% 
      dplyr::filter(.data$true_class  %in% names(cpg_obj$signatures))

    ref_mat <-
      ref_pred_res %>% dplyr::group_by(.data$true_class) %>% 
      dplyr::summarize_at(dplyr::vars(dplyr::starts_with("cg")),mean) %>% 
      # dplyr::filter(true_class %in% gsub("class_","",colnames(.))) %>% 
      tibble::column_to_rownames("true_class") %>% as.matrix() %>% t()
  }else{
    # ref_mat samples as rows, features as cols
    ref_pred_res <-
      cpg_obj$results %>%
      purrr::map_df(
        function(x){
          pres <- workflows:::predict.workflow(
            object=x$train_res,
            new_data=ref_data,
            type="prob"
            )$.pred_positive_class
          return(pres)
        },
        .id="classifier"
      )
      ref_pred_res <- 
        ref_pred_res %>%
        dplyr::rename_with(~paste0("classifier_",.x)) %>%
        dplyr::mutate(true_class=ref_data_labels)

    # summarize train/ref data into ref matrix
    ref_mat <-
      ref_pred_res %>% dplyr::group_by(.data$true_class) %>% 
      dplyr::summarize_at(dplyr::vars(dplyr::starts_with(colnames(.))),mean) %>%
      dplyr::filter(.data$true_class %in% gsub("classifier_","",colnames(.))) %>% 
      tibble::column_to_rownames("true_class") %>% as.matrix() %>% t()
  }

  return(list(deconvolution_reference_matrix=ref_mat))
}



deconv_ml <- function(
  cpg_obj = NULL,
  new_data = NULL,
  ref_mat = NULL,
  deconvolution_method = "NNLS",
  ...
){

  # predict on train/ref data
  # summarize train/ref data into ref matrix
  # predict on test/new data
  # run deconv on new data prediction using ref matrix
  # predict on train/ref data

  deconvolution_method <- match.arg(deconvolution_method, choices = c("NNLS","EpiDISH","NMF"))

  if(is.null(ref_mat)){
    assertthat::assert_that(!is.null(cpg_obj$deconvolution_reference_matrix))
    ref_mat <- cpg_obj$deconvolution_reference_matrix
  }

  # predict on test/new data
  new_pred_res <-
    cpg_obj$results %>%
    purrr::map_dfr(
      function(x){
        workflows:::predict.workflow(
          object=x$train_res,
          new_data=new_data,
          type="prob"
          )$.pred_positive_class
      },
      .id="classifier"
    )  %>%
    dplyr::rename_with(~paste0("classifier_",.x)) %>%
    dplyr::mutate(sample_label=rownames(new_data))

  pred_newdat <-
    new_pred_res %>%
    tibble::column_to_rownames("sample_label") %>% as.matrix() %>% t() 

  dt_res <- switch(
    deconvolution_method,
    "NNLS" = {
      dt_prednewdat <- pred_newdat%>%
        as.data.frame() %>% 
        tibble::rownames_to_column("model_id")  %>%
        data.table::as.data.table()

      compute_cols <- setdiff(colnames(dt_prednewdat),"model_id")

      deconvolution_nnls(dt = dt_prednewdat, compute_cols = compute_cols, ref_mat = ref_mat)
    },
    "EpiDISH" = {
      deconvolution_epidish(ref_mat = ref_mat, new_data = pred_newdat) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("cell_type") %>%
        dplyr::mutate(model_id = rownames(ref_mat)) %>%
        data.table::as.data.table()
    },
    "NMF" = {
      dt_prednewdat <- pred_newdat%>%
        as.data.frame() %>% 
        tibble::rownames_to_column("model_id")  %>% 
        data.table::as.data.table()

      compute_cols <- setdiff(colnames(dt_prednewdat),"model_id")

      deconvolution_nmf(weights_mat = ref_mat, values_mat = dt_prednewdat[,.SD,.SDcols=compute_cols],...) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("cell_type") %>%
        dplyr::mutate(model_id = rownames(ref_mat)) %>%
        data.table::as.data.table()
    }
  )

  dt_res <- data.table::melt(
    data = dt_res,
    value.name = "proportion",
    variable.name = "sample_id",
    id = c("model_id","cell_type")
  )

  return(dt_res)
}


deconv_cpg <- function(
  cpg_obj=NULL,
  new_data=NULL,
  ref_mat = NULL,
  deconvolution_method = "NNLS",
  ...
){
  deconvolution_method <- match.arg(deconvolution_method, choices = c("NNLS","EpiDISH","NMF"))

  signature_vec <- NULL

  if(is.null(ref_mat)){
    assertthat::assert_that(!is.null(cpg_obj$deconvolution_reference_matrix))
    assertthat::assert_that(all(cpg_obj$signatures %in% colnames(new_data)))

    ref_mat <- cpg_obj$deconvolution_reference_matrix
    signature_vec <- cpg_obj$signatures
  }else{
    assertthat::assert_that(all(rownames(ref_mat) %in% colnames(new_data)))
    signature_vec <- rownames(ref_mat)
  }


  subset_newdat <- t(new_data[, signature_vec])


  dt_res <- switch(
    deconvolution_method,
    "NNLS" = {
      dt_newdat <-
        subset_newdat %>%
        as.data.frame() %>%
        tibble::rownames_to_column("cpg_id") %>%
        data.table::as.data.table()

      compute_cols <- setdiff(colnames(dt_newdat), "cpg_id")

      deconvolution_nnls(dt = dt_newdat, compute_cols = compute_cols, ref_mat = ref_mat)
    },
    "EpiDISH" = {
      deconvolution_epidish(ref_mat = ref_mat, new_data = subset_newdat) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("cell_type") %>%
        dplyr::mutate(cpg_id = signature_vec) %>%
        data.table::as.data.table()
    },
    "NMF" = {
      dt_newdat <-
        subset_newdat %>%
        as.data.frame() %>%
        tibble::rownames_to_column("cpg_id") %>%
        data.table::as.data.table()

      compute_cols <- setdiff(colnames(dt_newdat), "cpg_id")

      deconvolution_nmf(weights_mat = ref_mat, values_mat = dt_newdat[,.SD,.SDcols=compute_cols],...) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("cell_type") %>%
        dplyr::mutate(cpg_id = signature_vec) %>%
        data.table::as.data.table()
    }
  )

  dt_res <- data.table::melt(
    data = dt_res,
    value.name = "proportion",
    variable.name = "sample_id",
    id = c("cpg_id","cell_type")
  )

  return(dt_res)
}


deconvolution_corrplot <- function(
  deconvoluted_data,
  meta_data,
  sample_id,
  true_label,
  color_dict = NULL,
  base_size = 14
){

  dat <- deconvoluted_data %>%
    dplyr::left_join(
      meta_data %>%
        as.data.frame() %>%
        dplyr::mutate(sample_id = !!ggplot2::sym(sample_id), true_label = !!ggplot2::sym(true_label)) %>%
        dplyr::select(.data$sample_id,true_label),
      by="sample_id"
      ) %>%
  dplyr::arrange(true_label,sample_id) %>% 
  dplyr::group_by(.data$sample_id) %>% 
  dplyr::mutate(ct=forcats::fct_reorder(.data$cell_type,order(.data$true_label))) %>% 
  dplyr::group_by(.data$ct) %>% 
  dplyr::mutate(ss=forcats::fct_reorder(.data$sample_id,order(.data$true_label,.data$sample_id))) %>%
  dplyr::mutate(true_label_value=dplyr::if_else(.data$cell_type==as.character(.data$true_label),1,0)) %>%
  dplyr::mutate(rmse=sqrt(mean((.data$true_label_value - .data$proportion)^2)))

  if(is.null(color_dict)){
    n_color <- dat$ct %>% levels %>% length
    color_dict <- if(n_color < 9L) ggsci::pal_nejm()(n_color) else ggsci::pal_ucscgb()(n_color)
    names(color_dict) <- dat$ct %>% levels() %>% sort()
  }

  plt <- dat %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$true_label_value,
        y = .data$proportion,
        group = .data$true_label,
        color = .data$ct
      )
    )+
    ggplot2::geom_smooth(
      ggplot2::aes(color=true_label),
      formula=y~x, method="lm", show.legend=FALSE
    )+
    ggplot2::geom_point()+
    ggplot2::scale_color_manual(values=color_dict) +
    ggplot2::scale_x_continuous(breaks=c(0,1)) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.25)) +
    ggplot2::facet_wrap(.~true_label,nrow=2)+
    ggplot2::theme_classic(base_size=base_size) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=1,byrow=TRUE))+
    ggplot2::theme(legend.position="bottom")

  return(list(data=dat,plot=plt))
}

#' Stacked barplot of deconvolution results
#'
#' @param deconvoluted_data Result from running `run_deconvolution`
#' @param meta_data Data.frame containing metadata from deconvoluted samples
#' @param sample_id_column Name of the column containing the sample id in the meta_data data.frame
#' @param true_label_column Name of the column containing the true labels of the samples in the meta_data data.frame
#' @param color_dict Named string featuring colors as values and labels (true labels) as names
#' @param show_x_label A boolean, if `TRUE` the sample labels in the X axis will be shown. Default is `FALSE`.
#' @param base_size An integer defining the base size of the text in the plot. Default is `14`.
#' @param ... Parameters passed to the ggplot2::theme function.
#' @return A list with the data and the ggplot2 plot object.
#' @export
deconvolution_barplot <- function(
  deconvoluted_data,
  meta_data,
  sample_id_column,
  true_label_column,
  color_dict=NULL,
  show_x_label=FALSE,
  base_size=14,
  ...
){

  set_meta <- meta_data %>%
    as.data.frame() %>%
    dplyr::mutate(
      sample_id = !!ggplot2::sym(sample_id_column),
      true_label = !!ggplot2::sym(true_label_column)
    ) %>%
    dplyr::select(.data$sample_id, .data$true_label)

  dat <- deconvoluted_data %>%
    dplyr::left_join(
      set_meta,
      by="sample_id"
    ) %>%
    dplyr::arrange(.data$true_label, .data$sample_id) %>%
    dplyr::group_by(.data$sample_id) %>%
    dplyr::mutate(ct=forcats::fct_reorder(.data$cell_type, order(.data$true_label))) %>%
    dplyr::group_by(.data$ct) %>%
    dplyr::mutate(ss=forcats::fct_reorder(.data$sample_id, order(.data$true_label, .data$sample_id)))

  if(is.null(color_dict)){
    n_color <- dat$ct %>% levels %>% length
    color_dict <- if(n_color < 9L) ggsci::pal_nejm()(n_color) else ggsci::pal_ucscgb()(n_color)
    names(color_dict) <- dat$ct %>% levels() %>% sort()
  }

  plt <- dat %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$ss, y = .data$proportion, fill = .data$ct)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_tile(ggplot2::aes(y = -0.05, fill = .data$true_label, height = 0.05)) +
    ggplot2::geom_tile(ggplot2::aes(y = 1.05, fill = .data$true_label, height = 0.05)) +
    ggplot2::scale_fill_manual(values = color_dict, na.value = "#000000") +
    ggplot2::scale_y_continuous(limits = c(-0.08, 1.08), breaks = seq(0, 1, 0.25)) +
    ggplot2::theme_classic(base_size = base_size)+
    ggplot2::labs(x = "Samples", y = "Proportion") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2,byrow = TRUE)) +
    ggplot2::theme(
      axis.text.x = if(show_x_label) ggplot2::element_text(angle = 90, vjust = 0, hjust = 0) else ggplot2::element_blank(),
      legend.position = "bottom",
      ...
    )

  return(list(data = dat, plot = plt))
}

