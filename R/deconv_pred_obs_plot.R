
deconv_pred_obs_plot <- function(
  deconv_df,# data.frame with predictions as columns, each row should be a prediction for a given sample and given group/celltype
  true_values_col,
  predicted_cols,
  sample_id_col,
  group_col,
  axis_lims = list(x = c(0, 1), y = c(0, 1))
){
  # TODO customizable color palettes
  # TODO customizable limits
  augmented <- .fitted <- metrics <- glanced <- NULL
  rmse <- adj.r.squared <- r_squared <- AIC <- NULL

  dec_dat <- make_deconv_pred_obs_data(
    deconv_df,
    true_values_col=true_values_col,
    predicted_cols=predicted_cols,
    sample_id_col=sample_id_col,
    group_col = group_col
  )

  # should work even with just one group, run tests?
  ldat <- dec_dat |> split(dec_dat$method)
  lnames <- names(ldat)

  plt_list <- purrr::map2(.x = ldat, .y = lnames, .f = function(dd, nn){

    dd_plt <- dd |> tidyr::unnest(augmented)
    dd_txt <- dd |> tidyr::unnest(metrics) |> dplyr::select(-AIC) |> tidyr::unnest(glanced)

    dd_plt |>
      ggplot2::ggplot(ggplot2::aes(x=.data[[true_values_col]],y=.data[[nn]],color=.data[[group_col]])) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(y = .fitted)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 3) +
      ggplot2::labs(x="true proportion", y="predicted\nproportion", title = nn) +
      ggplot2::facet_wrap(. ~ .data[[group_col]], nrow = 1) +
      ggplot2::geom_text(
        data = dd_txt,
        ggplot2::aes(
          x = Inf, y = 0,
          label = paste0(
            "RMSE = ", round(rmse, 2),"\n",
            "Rsq(fit) = ", round(adj.r.squared, 2),"\n",
            "Rsq = ", round(r_squared, 2)
          )
        ),
        vjust = "inward", hjust = "inward", size = 5
      ) +
      ggplot2::scale_color_manual(
        # values=ggsci::pal_nejm()(length(unique(dd[[group_col]]))),unique(dd[[group_col]])
        values = ggsci::pal_ucscgb()(length(unique(dd[[group_col]]))), unique(dd[[group_col]])
      ) +
      ggplot2::lims(y = axis_lims$y, x = axis_lims$x) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position="none",
        strip.background = ggplot2::element_blank(),#element_rect(colour="white", fill="#FFFFFF"),
        strip.text = ggplot2::element_text(size=18),#element_blank(),
        plot.title = ggplot2::element_text(size=12)
      )
  })

  return(plt_list)
}

