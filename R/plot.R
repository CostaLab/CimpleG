#' Represent CpGs in the difference in means, sum of variances space.
#' This plot is often used to select CpGs that would be good classifiers.
#' These CpGs are often located on the bottom left and
#' bottom right of this plot.
#'
#' @param data Data to create difference in means, sum of variances plot.
#'  Either a data.frame with `xcol`,`ycol` and `feature_id_col` or, if
#'  `target_vector` is not `NULL` a matrix with beta values from which,
#'  given the target, the difference in means between the target and others,
#'  and the sum of variances within the target and others will be calculated.
#'
#' @param xcol Column with x-axis data
#' @param ycol Column with y-axis data
#' @param feature_id_col Column with the feature ID
#' @param is_feature_selected_col NULL or column with TRUE/FALSE for features which should be highlighted as selected
#' @param label_var1 Label of the target class
#' @param label_var2 Label of the other classes
#' @param target_vector if not NULL a vector target class assignment, see data
#' @param mean_cutoff a numeric draw mean cutoff at given position
#' @param var_cutoff a numeric draw variance cutoff at given position
#' @param threshold_func TODO
#' @param func_factor TODO
#' @param feats_to_highlight TODO
#' @param cpg_ranking_df TODO
#' @param color_all_points TODO
#' @param pltDensity TODO
#' @param density_type TODO
#' @param plot_dir TODO
#' @param id_tag TODO
#' @param file_tag TODO
#' @param custom_mods TODO
#'
#'
#' @examples
#' library("CimpleG")
#'
#' # read data
#' data(train_data)
#' data(train_targets)
#'
#' # make basic plot
#' plt <- diffmeans_sumvariance_plot(
#'   train_data,
#'   target_vector = train_targets$CELL_TYPE_MSCORFIBRO == 1
#' )
#' print(plt)
#'
#' # make plot with parabola and colored features
#' df_dmeansvar <- compute_diffmeans_sumvar(
#'   train_data,
#'   target_vector = train_targets$CELL_TYPE_MSCORFIBRO==1
#' )
#' parab_param <- .7
#' df_dmeansvar <- df_dmeansvar %>% mutate(
#'   is_selected=select_features(
#'     x = diff_means,
#'     y = sum_variance,
#'     a = parab_param
#'   )
#' )
#'
#' plt <- diffmeans_sumvariance_plot(
#'   df_dmeansvar,
#'   label_var1="MSC",
#'   color_all_points="red",
#'   threshold_func=function(x,a) (a*x)^2,
#'   is_feature_selected_col="is_selected",
#'   func_factor=parab_param
#' )
#' print(plt)
#' @export
diffmeans_sumvariance_plot <- function(
  data,
  xcol="diff_means",
  ycol="sum_variance",
  feature_id_col="id",
  is_feature_selected_col=NULL,
  label_var1="Target",
  label_var2="Others",
  target_vector=NULL,
  mean_cutoff=NULL,
  var_cutoff=NULL,
  threshold_func=NULL,
  func_factor=NULL,
  feats_to_highlight=NULL,
  cpg_ranking_df=NULL,
  color_all_points=NULL,
  pltDensity=TRUE,
  density_type=c("density", "histogram", "boxplot",
  "violin", "densigram"),
  plot_dir=NULL,
  id_tag=NULL,
  file_tag=NULL,
  custom_mods=FALSE
){

  if(!is.null(target_vector)){
    assertthat::assert_that(is.matrix(data)|is.data.frame(data))
    assertthat::assert_that(is.logical(target_vector))
    data <- compute_diffmeans_sumvar(data,target_vector = target_vector)
  }

  assertthat::assert_that(xcol%in%colnames(data))
  assertthat::assert_that(ycol%in%colnames(data))

  if(!feature_id_col%in%colnames(data)){
    data[,feature_id_col] = rownames(data)
  }
  assertthat::assert_that(feature_id_col%in%colnames(data))

  assertthat::assert_that(typeof(label_var1) == 'character' && typeof(label_var2) == 'character')
  # NULL or else
  assertthat::assert_that(is_feature_selected_col %in% colnames(data) || is.null(is_feature_selected_col))
  assertthat::assert_that(typeof(mean_cutoff) == 'double' || is.null(mean_cutoff))
  assertthat::assert_that(typeof(var_cutoff) == 'double' || is.null(var_cutoff))
  assertthat::assert_that(typeof(func_factor) == 'double' || is.null(func_factor))
  assertthat::assert_that(is.function(threshold_func) || is.null(threshold_func))

  density_type <- match.arg(density_type)

  best_cpgs_df<-NULL
  if(!is.null(cpg_ranking_df)){
    if(!all(c(".id","predType","DiffAndFoldScaledAUPR")%in%colnames(cpg_ranking_df))){
      warning(".id, predType, Rank and DiffAndFoldScaledAUPR columns not present in cpg_ranking_df. cpg_ranking_df won't be used")
    }else{
      best_cpgs_df<-cpg_ranking_df[cpg_ranking_df$Rank<=min(10,nrow(cpg_ranking_df)),]
    }
  }

  # setting colors
  points_color <- ifelse(!is.null(color_all_points),color_all_points,"black")
  lighten <- function(color, factor = 0.5) {
    if ((factor > 1) | (factor < 0)) stop("factor needs to be within [0,1]")
    col <- grDevices::col2rgb(color)
    col <- col + (255 - col)*factor
    col <- grDevices::rgb(t(col), maxColorValue=255)
    col
  }
  light_points_color <- lighten(points_color,0.7)

  #message("plt diffMean,sumVariance")
  if(!is.null(is_feature_selected_col)){
    plt_diffMeanSumVar <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x=!!ggplot2::sym(xcol),
        y=!!ggplot2::sym(ycol),
        fill=!!ggplot2::sym(is_feature_selected_col),
        color=!!ggplot2::sym(is_feature_selected_col)
      )
    )

    point_color_vec <- ifelse(
      data[,is_feature_selected_col],
      points_color,
      light_points_color
    )
    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      ggplot2::geom_point(
        colour=point_color_vec,
        alpha=0.5)
  }else{
    plt_diffMeanSumVar <- ggplot2::ggplot(
      data,
      ggplot2::aes(x=!!sym(xcol), y=!!sym(ycol))
    )
    if(!is.null(feats_to_highlight)){
      plt_diffMeanSumVar <- plt_diffMeanSumVar +
        ggplot2::geom_point(
          data=function(x){x[!x[,feature_id_col] %in% feats_to_highlight,]},
          colour=points_color,
          alpha=0.3)+
        ggplot2::geom_point(
          data=function(x){x[x[,feature_id_col] %in% feats_to_highlight,]},
          colour=ifelse(!is.null(best_cpgs_df),"orange3","red"),
          alpha=1)
    }else{
      plt_diffMeanSumVar <- plt_diffMeanSumVar +
        ggplot2::geom_point(colour=points_color,alpha=0.3)
    }
  }

  if(!is.null(best_cpgs_df)){
    plt_diffMeanSumVar <- plt_diffMeanSumVar + ggplot2::geom_point(
      data=function(x){x[x[,feature_id_col] %in% best_cpgs_df$.id,]},
      colour="red",
      alpha=1
    )

    best_label <- paste0(best_cpgs_df$Rank,"#",best_cpgs_df$.id)

    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      ggrepel::geom_label_repel(
        data = data[best_cpgs_df$.id,],
        ggplot2::aes(
          x=!!ggplot2::sym(xcol),
          y=!!ggplot2::sym(ycol),
          label=best_label
        ),
        fill="white",
        colour="darkred",
        segment.size=0.5,
        segment.color="darkred",
        direction="both",
        size=max(3,16/length(best_cpgs_df$.id)),
        box.padding=.6,
        force=5,
        max.iter=10000
      )
  }else if(
    length(feats_to_highlight)<25 &&
    length(feats_to_highlight)>2
  ){
    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      ggrepel::geom_label_repel(
        data = data[feats_to_highlight,],
        ggplot2::aes(
          x=!!ggplot2::sym(xcol),
          y=!!ggplot2::sym(ycol),
          label=feats_to_highlight
        ),
        fill="white",
        colour="darkred",
        segment.size=0.5,
        segment.color="darkred",
        direction="both",
        size=max(3,16/length(feats_to_highlight)),
        box.padding=.6,
        force=5,
        max.iter=10000
      )
  }

  if(!is.null(mean_cutoff) & !is.null(var_cutoff)){
    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      ggplot2::geom_vline(xintercept = -mean_cutoff, alpha=0.5) +
      ggplot2::geom_vline(xintercept = mean_cutoff, alpha=0.5) +
      ggplot2::geom_vline(xintercept = 0, alpha=0.5) +
      ggplot2::geom_hline(yintercept = var_cutoff, alpha=0.5)
  }

  plt_diffMeanSumVar <- plt_diffMeanSumVar +
  ggplot2::labs(
    x=expression(bar(beta)[paste(cell)] - bar(beta)[paste(others)]),
    y=expression(var(beta[paste(cell)]) + var(beta[paste(others)])),
    title=paste0(label_var1," vs ",label_var2),
    caption=unlist(ifelse(is.null(id_tag),waiver(),id_tag))
  )
  # FIXME control creation of simple_plot
  simple_plot = plt_diffMeanSumVar

  if(!is.null(threshold_func) & !is.null(func_factor)){
    funlabel <- paste0(
      "y==",
      gsub(
        pattern = "a",
        x = format(threshold_func)[2],
        replacement = func_factor
      )
    )
    label_ypos <- stats::quantile(data$sumVariance)["75%"]
    label_xpos <- 0


    if(custom_mods){ # custom mod
      funlabel <- paste0(
        "y==",
        gsub(
          pattern = "a",
          x = format(threshold_func)[2],
          replacement = "r"
        )
      )
      alt_funlabel <- gsub(
        pattern="y",
        x=funlabel,
        replacement=expression(var(beta[paste(cell)]) + var(beta[paste(others)]))
      )
      alt_funlabel <- gsub(
        pattern="x",
        x=alt_funlabel,
        replacement=expression(bar(beta)[paste(cell)] - bar(beta)[others])
      )

      funlabel<-alt_funlabel
      label_ypos <- (stats::quantile(data[,ycol])["100%"]+stats::quantile(data[,ycol])["75%"])/3
      label_xpos <- 0.

      plt_diffMeanSumVar <- plt_diffMeanSumVar +
        ggplot2::geom_text(
          inherit.aes=FALSE,
          data=data.frame(
            x=c(-1,1),
            y=c(0,0),
            txt=c(
              paste0("n[hypo]==",nrow(data[data[,xcol]<0 & data$selected_feat,])),
              paste0("n[hyper]==",nrow(data[data[,xcol]>0 & data$selected_feat,]))
            )
          ),
          ggplot2::aes(
            x=x,
            y=y,
            label=txt
          ),
          parse=TRUE,
          size=5,
          vjust = "inward",
          hjust = "inward"
        )
    }

    simple_plot = simple_plot +
      ggplot2::stat_function(
        inherit.aes=FALSE,
        data = data.frame(x=c(-1,1)),
        ggplot2::aes(x),
        fun=threshold_func,
        args=func_factor,
        size=1.5,
        geom="line",
        color="grey40"
      )

    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      ggplot2::stat_function(
        inherit.aes=FALSE,
        data = data.frame(x=c(-1,1)),
        ggplot2::aes(x),
        fun=threshold_func,
        args=func_factor,
        size=1.5,
        geom="line",
        color="grey40"
      )+
      ggplot2::annotate(
        geom="label",
        label=funlabel,
        # size=7,
        size=5,
        parse=TRUE,
        x=label_xpos,y=label_ypos,
        color="grey40"
      )

  }
  if(
    length(feats_to_highlight)<=2 &&
    length(feats_to_highlight)>0
  ){
    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      ggrepel::geom_label_repel(
        data = data[feats_to_highlight,],
        ggplot2::aes(
          x=!!ggplot2::sym(xcol),
          y=!!ggplot2::sym(ycol),
          label=feats_to_highlight
        ),
        arrow = grid::arrow(
          length = grid::unit(0.05, "npc"),
          type = "closed",
          ends = "last"
        ),
        fill="white",
        color="black",
        segment.color="black",
        # colour=points_color,
        # segment.color=lighten(points_color,0.5),
        segment.size=1.5,
        direction="both",
        size=5,
        box.padding=.6,
        point.padding=.5,
        # force=5,
        # max.iter=10000,
        xlim=c(-1.,1.),ylim=c(0.3,0.4)
      )
      simple_plot = simple_plot +
        ggrepel::geom_label_repel(
          data = data[feats_to_highlight,],
          ggplot2::aes(
            x=!!ggplot2::sym(xcol),
            y=!!ggplot2::sym(ycol),
            label=feats_to_highlight
          ),
          arrow = grid::arrow(
            length = grid::unit(0.05, "npc"),
            type = "closed",
            ends = "last"
          ),
          fill="white",
          color="black",
          segment.color="black",
          # colour=points_color,
          # segment.color=lighten(points_color,0.5),
          segment.size=1.5,
          direction="both",
          size=5,
          box.padding=.6,
          point.padding=.5,
          # force=5,
          # max.iter=10000,
          xlim=c(-1.,1.),ylim=c(0.3,0.4)
        )
  }

  ymaxlim <- ifelse(
    max(data[,ycol])<0.4,
    0.4,
    max(data[,ycol])
  )

  plt_diffMeanSumVar <- plt_diffMeanSumVar +
    #scale_color_manual(values = color_vals)+
    ggplot2::theme_minimal(base_size=18)+
    ggplot2::theme(
      legend.position="none",
      axis.title.y = ggplot2::element_text(face="bold",size=22),
      axis.title.x = ggplot2::element_text(face="bold",size=22),
      axis.text.x = ggplot2::element_text(face="bold",size=22),
      axis.text.y = ggplot2::element_text(face="bold",size=22),
      plot.caption = ggplot2::element_text(size=9)
    )+
    ggplot2::xlim(c(-1,1))+
    ggplot2::ylim(c(0,ymaxlim))
  #print(plt_diffMeanSumVar)

  simple_plot = simple_plot +
    ggplot2::theme_minimal(base_size=18)+
    ggplot2::theme(
      legend.position="none",
      axis.title.y = ggplot2::element_text(face="bold",size=22),
      axis.title.x = ggplot2::element_text(face="bold",size=22),
      axis.text.x = ggplot2::element_text(face="bold",size=22),
      axis.text.y = ggplot2::element_text(face="bold",size=22),
      plot.caption = ggplot2::element_text(size=9)
    )+
    ggplot2::xlim(c(-1,1))+
    ggplot2::ylim(c(0,ymaxlim))

  if(pltDensity){
    plt_diffMeanSumVar <- ggExtra::ggMarginal(
      p=plt_diffMeanSumVar,
      data=data,
      x=xcol,
      y=ycol,
      groupFill=!is.null(is_feature_selected_col),
      groupColour=!is.null(is_feature_selected_col),
      type=density_type,
      size=10)

    simple_plot = ggExtra::ggMarginal(
      p=simple_plot,
      data=data,
      x=xcol,
      y=ycol,
      groupFill=!is.null(is_feature_selected_col),
      groupColour=!is.null(is_feature_selected_col),
      type=density_type,
      size=10)
  }

  fname_tag <- paste0(
    "target-",
    label_var1,
    "_",
    file_tag,
    "_",
    id_tag,
    format(Sys.time(),"%Y%m%d-%H%M%S")
  )

  save_different_plot_format(
    plt = plt_diffMeanSumVar,
    plot_dir = plot_dir,
    create_plot_subdir = FALSE,
    save_device = c("ggplot"),
    type_name = "diffmean_sumvar_plot",
    name_tag = fname_tag,
    formats = c("png"),
    units = "cm",
    width = 15,
    height = 15
  )
  save_different_plot_format(
    plt = plt_diffMeanSumVar,
    plot_dir = plot_dir,
    create_plot_subdir = FALSE,
    save_device = c("ggplot"),
    type_name = "diffmean_sumvar_simpleplot",
    name_tag = fname_tag,
    formats = c("png"),
    units = "cm",
    width = 15,
    height = 15
  )
  return(plt_diffMeanSumVar)
}



plot <- function(){
  # TODO to implement
  # meth_predictors %>%
  #   slice_max(AUPR,n = 1000) %>%
  #   ggplot(.,aes(x=diffMeans,y=sumVariance))+
  #   geom_point(aes(col=DiffScaledAUPR),size=3)+
  #   geom_point(aes(col=AUPR),size=1)+
  #   scale_colour_gradientn(
  #     colours=colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")))(10),
  #     limits=c(0,1)
  #   )+
  #   lims(x = c(-1,1), y = c(0,.25))+
  #   theme_minimal()
}
