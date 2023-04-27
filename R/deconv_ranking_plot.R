

#' Boxplot and rankings of deconvolution metrics for deconvolution results.
#'
#' Produces data with varied deconvolution performance metrics. Produces one boxplot and one ranking plot with the for each metric.
#' @param deconv_df A data.frame with meta.data, true values and predictions for different methods as columns. Each row should be a prediction for a given sample and a given group/cell type.
#' @param true_values_col A string with the name of the column with the true values in `deconv_df`.
#' @param predicted_cols A vector of strings with the name of the columns with the predictions for different methods in `deconv_df`.
#' @param sample_id_col A string with the name of the column with the sample name or ID in `deconv_df`.
#' @param group_col A string with the name of the column containing the cell types or groups in `deconv_df`.
#' @param metrics A list with two entries, `x` and `y`, defining the limits of the x and y axis of the plot.
#' @param custom_colours A named vector with colours, where the names are the values defined in `predicted_cols`. If `NULL`, default colours will be used.
#' @export
deconv_ranking_plot <- function(
  deconv_df,
  true_values_col,
  predicted_cols,
  sample_id_col,
  group_col,
  #highlight_method = NULL,
  metrics = c("rmse", "r_squared", "adj.r.squared","AIC"), # supported metrics currently
  custom_colours = NULL # named vec (predicted_cols) with colors or NULL
){

  glanced <- method <- cdmin <- cdmax <- means <- NULL
  AIC <- NULL

  # at least 2 distinct predictions should be in the df
  assertthat::assert_that(length(predicted_cols) >= 2)
  # at least 1 metric has to be selected
  assertthat::assert_that(length(metrics) >= 1)
  # at least 2 distinct groups should be in the data in group_col
  assertthat::assert_that(length(unique(deconv_df[[group_col]])) >= 2)

  deconv_df <- as.data.frame(deconv_df)

  dec_dat <- make_deconv_pred_obs_data(
    deconv_df,
    true_values_col = true_values_col,
    predicted_cols = predicted_cols,
    sample_id_col = sample_id_col,
    group_col = group_col
  )

  if(is.null(custom_colours)){
    custom_colours  <- stats::setNames(ggsci::pal_d3()(length(unique(dec_dat[["method"]]))),unique(dec_dat[["method"]]))
  }

  assertthat::assert_that(all(predicted_cols %in% names(custom_colours)))

  plt_l <- purrr::map(
    metrics,
    function(mm){

      ptitle <- mm |> 
        gsub(pattern = "r_squared", replacement = "Rsq", fixed = TRUE) |>
        gsub(pattern = "rmse", replacement = "RMSE", fixed = TRUE) |>
        gsub(pattern = "adj.r.squared", replacement = "Rsq(fit)", fixed = TRUE)

      to_maximize <- any(mm %in% c("rmse", "AIC"))

      boxplt_dat <- dec_dat |>
        tidyr::unnest(metrics) |>
        dplyr::select(-AIC) |> # column already exists in glanced
        tidyr::unnest(glanced)

      dat_sortby <- boxplt_dat[[mm]]
      dat_sortby[which(is.nan(dat_sortby))] <- -Inf
      if(to_maximize){
        dat_sortby  <- boxplt_dat[[mm]] * -1
      }

      boxplt_dat |>
        ggplot2::ggplot(
          ggplot2::aes(
            y = forcats::fct_reorder(method, dat_sortby),
            x = .data[[mm]], 
            group = method, fill = method
          )
        ) +
        ggplot2::geom_boxplot(show.legend = FALSE, width = 0.5, outlier.size = .5) +
        ggplot2::scale_fill_manual(values = custom_colours) +
        ggplot2::labs(x=ptitle,y="") +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::theme(
          legend.position = "none",
          strip.background = ggplot2::element_blank(),#element_rect(colour="white", fill="#FFFFFF"),
          strip.text = ggplot2::element_text(size = 18),#element_blank(),
          plot.title = ggplot2::element_text(size = 12)
        )
    }
  )

  nemen_plt_l <-
    purrr::map(metrics, function(mm){

      ptitle <- mm |>
        gsub(pattern = "r_squared", replacement = "Rsq", fixed = TRUE) |>
        gsub(pattern = "rmse", replacement = "RMSE", fixed = TRUE) |>
        gsub(pattern = "adj.r.squared", replacement = "Rsq(fit)", fixed = TRUE)

      to_maximize <- any(mm %in% c("rmse","AIC"))

      nemenyi_dat <- dec_dat |>
        tidyr::unnest(metrics) |>
        dplyr::select(-AIC) |> # column already exists in glanced
        tidyr::unnest(glanced)

      if(to_maximize){
        nemenyi_dat[[mm]]  <- nemenyi_dat[[mm]] * -1
      }

      nemenyi_dat <- nemenyi_dat |>
        dplyr::select(method,!!dplyr::sym(group_col),!!dplyr::sym(mm)) |> 
        tidyr::pivot_wider(
          values_from = !!dplyr::sym(mm),
          names_from = method,
          id_cols = !!dplyr::sym(group_col)
        ) |> 
        tibble::column_to_rownames(group_col) %>%
        dplyr::mutate_all(~ifelse(is.nan(.)|is.na(.), -Inf, .)) |>
        as.matrix()

      nemenyi_dat <- nemenyi_dat |>
        tsutils::nemenyi(
          plottype = "none",
          #select = grep(highlight_method, colnames(nemenyi_dat), ignore.case = TRUE),
          conf.level = .95
        )

      nem_df <- data.frame(
        means=nemenyi_dat$means,
        critical_distance=nemenyi_dat$cd,
        cdmin = nemenyi_dat$means - (0.5 * nemenyi_dat$cd),
        cdmax = nemenyi_dat$means + (0.5 * nemenyi_dat$cd)
      ) |> tibble::rownames_to_column("method")

      nem_rank_plt <- nem_df |> 
        ggplot2::ggplot(ggplot2::aes(x=means,y=forcats::fct_reorder(method,means),color=method)) +
        # highlight cimpleg or max?
        ggplot2::geom_rect(ggplot2::aes(xmin=max(cdmin),xmax=max(cdmax),ymin=-Inf,ymax=Inf),show.legend=FALSE,alpha=0.5,fill="grey90",color=NA)+
        ggplot2::geom_point(size=4,show.legend=FALSE) +
        ggplot2::geom_pointrange(ggplot2::aes(xmin = cdmin, xmax = cdmax),show.legend=FALSE) +
        ggplot2::scale_color_manual(values=custom_colours) +
        ggplot2::scale_y_discrete(labels=stats::setNames(paste0(nem_df$method," - ",format(round(nem_df$means,2),digits=2,nsmall=2)),nem_df$method)) +
        ggplot2::labs(x=paste0(ptitle,"\nMean ranks"),y="",color="") +
        ggplot2::theme_classic(base_size=14)+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size=11))
      return(nem_rank_plt)
    })
  
  return(
    list(
      dat = dec_dat,
      perf_boxplt = plt_l,
      nemenyi_plt = nemen_plt_l
    )
  )
}
