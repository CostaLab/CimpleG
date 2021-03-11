#' Deconvolution plotting functions
#' @param deconv_mat TODO
#' @param name_tag TODO
#' @param sorted_classes TODO
#' @param sort_by_class_value TODO
#' @param color_palette_df TODO
#' @param facet_by_sample_group TODO
#' @param group_split_chr TODO
#' @param group_split_pos TODO
#' @param facet_group_order TODO
#' @param subtitle TODO
#' @param plot_dir TODO
#' @param time_stamp TODO
#' @param hide_x_axis TODO
#' @param size_x_axis TODO
#' @param facet_x_space TODO
#' @param do_boxplots TODO
#' @param show_legend TODO
#' @param signif TODO
#' @importFrom dplyr %>%
plot_deconvolution <- function(
  deconv_mat,  # Classes as rows, Samples as columns
  name_tag,
  sorted_classes,
  sort_by_class_value=TRUE,
  color_palette_df=NULL,
  facet_by_sample_group=FALSE,
  group_split_chr="_",
  group_split_pos=2,
  facet_group_order=NULL,
  subtitle=NULL,
  plot_dir=NULL,
  time_stamp=TRUE,
  hide_x_axis=TRUE,
  size_x_axis=NULL,
  facet_x_space = c("free_x","fixed"),
  do_boxplots=FALSE,
  show_legend=TRUE,
  signif=TRUE
){

  message(">>> Plotting deconvoltion results")

  if(any(is.na(deconv_mat))){
    stop("'deconv_mat' has NA values.")
  }

  assertthat::assert_that(all(sorted_classes %in% rownames(deconv_mat)))
  assertthat::assert_that(all.equal(
    target=sum(matrixStats::colCumsums(deconv_mat)[nrow(deconv_mat),]),
    current=ncol(deconv_mat),
    tolerance=0.001
  ))
  assertthat::assert_that(is.character(group_split_chr))
  assertthat::assert_that(is.numeric(group_split_pos))

  facet_x_space <- match.arg(facet_x_space)

  if(is.null(color_palette_df)){
    color_palette_df <- make_color_palette(sorted_classes)
  }

  deconv_mat_long <- reshape2::melt(deconv_mat) %>%
    magrittr::set_names(c("Classes","Samples","Value"))

  deconv_mat_long$class_colors <-
    color_palette_df[match(deconv_mat_long$Classes,color_palette_df$classes),]$class_color

  color_vec <-
    color_palette_df$class_color %>%
    magrittr::set_names(color_palette_df$classes)

  if(facet_by_sample_group){
    deconv_mat_long$groups <- stringr::str_split(
      string=as.character(deconv_mat_long$Samples),
      pattern=group_split_chr,
      simplify=TRUE)[,group_split_pos]
    if(!is.null(facet_group_order)){
      deconv_mat_long$groups = factor(deconv_mat_long$groups,levels=facet_group_order)
    }
    facet_obj<-ggplot2::facet_grid(.~groups,
      scales="free_x",
      space=facet_x_space
    )
  }else{
    facet_obj<-NULL
    deconv_mat_long$groups <- ""
  }


  axis_text_x <- if(hide_x_axis){
    ggplot2::element_blank()
  }else{
    ggplot2::element_text(
      size=ifelse(!is.null(size_x_axis),size_x_axis,400/ncol(deconv_mat)),
      angle = 45,
      hjust = 1,
      vjust = 1.
    )
  }

  def_theme <- ggplot2::theme(
    axis.text.x=axis_text_x,
    axis.title.y=ggplot2::element_text(face="bold",size=22),
    axis.text.y=ggplot2::element_text(face="bold",size=22),
    legend.text=ggplot2::element_text(size = 12),
    strip.text.x=ggplot2::element_text(size = 22,face="bold",angle=90),
    legend.position=ifelse(show_legend,"bottom","none")
  )

  deconv_mat_long <- deconv_mat_long %>%
    # Sort colors inside each column
    dplyr::mutate(Classes=forcats::fct_reorder(Classes,match(Classes,sorted_classes)))

  if(sort_by_class_value){
    deconv_mat_long <- deconv_mat_long %>%
    # Sort samples by last Class value
      dplyr::group_by(Classes)%>%
      dplyr::mutate(
        Samples=forcats::fct_reorder(
          Samples,
          dplyr::filter(.,Classes%in%sorted_classes[length(sorted_classes)])%>%
          dplyr::pull(Value)
        )
      )
  }
  title_tag<-paste0("Deconvolution: ",name_tag)
  subtitle_tag = ifelse(is.null(subtitle),"",subtitle)

  deconv_plot <- deconv_mat_long %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x=Samples,
        y=Value,
        fill=Classes
      )) +
      ggplot2::geom_bar(stat='identity')+
      ggplot2::scale_fill_manual(
        values=color_vec,
        limits = names(color_vec)
      )+
      ggplot2::coord_cartesian(expand=FALSE)+
      ggplot2::labs(
        title=title_tag,
        subtitle=subtitle_tag,
        x="Samples",
        y="cell proportions",
        fill="Cell types"
      )+
      ggplot2::theme_minimal(base_size = 18)+
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))+
      def_theme+facet_obj

  res_list = list(
    data=deconv_mat_long,
    deconv_plot=deconv_plot
  )

  if(do_boxplots){
    deconv_boxplot <- deconv_mat_long %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x=paste0(Classes,"_",groups),
          y=Value,
          fill=Classes
        )
      )+
      ggplot2::geom_boxplot(outlier.alpha=0,width=0.5)+
      ggbeeswarm::geom_quasirandom(
        size=1,
        varwidth = TRUE,
        alpha=0.2
      )+
      ggplot2::labs(
        title=title_tag,
        x="Samples grouped by class",
        y="cell proportions",
        fill="Cell types"
      )+
      ggplot2::scale_fill_manual(
        values=color_vec,
        limits = names(color_vec)
      )
      if(signif){
        deconv_boxplot = deconv_boxplot + ggsignif::geom_signif(
          comparisons=lapply(
            X=levels(deconv_mat_long$Classes),
            grp=unique(deconv_mat_long$groups),
            function(lx,grp){
              return(c(paste0(lx,"_",grp)))
            }
          ),
          test="wilcox.test",
          y_position=-.1,
          map_signif_level=TRUE,
          tip_length=-0.01,
          size=1.,
          textsize=4
        )
      }
      deconv_boxplot = deconv_boxplot +
      ggplot2::theme_minimal(base_size=18)+
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))+
      def_theme+ggplot2::theme(axis.text.x=ggplot2::element_text(size=12, angle = 45, hjust = 1, vjust = 1.))

    res_list = list(
      data=deconv_mat_long,
      deconv_plot=deconv_plot,
      deconv_boxplot=deconv_boxplot
    )
  }
  file_tag <- paste0("deconvolution_",name_tag)

  if(!is.null(plot_dir)){

    time_stamp <-ifelse(time_stamp,format(Sys.time(),"%Y%m%d-%H%M%S"),"")

    f_name = paste0(file_tag,time_stamp)

    f_path_png = file.path(plot_dir,paste0(f_name,".png"))
    f_path_pdf = file.path(plot_dir,paste0(f_name,".pdf"))
    f_path_box_png = file.path(plot_dir,paste0("boxplot_",f_name,".png"))
    f_path_box_pdf = file.path(plot_dir,paste0("boxplot_",f_name,".pdf"))

    if(dir.exists(file.path(plot_dir,"png")) & dir.exists(file.path(plot_dir,"pdf"))){
      f_path_png = file.path(plot_dir,"png",paste0(f_name,".png"))
      f_path_pdf = file.path(plot_dir,"pdf",paste0(f_name,".pdf"))
      f_path_box_png = file.path(plot_dir,"png",paste0("boxplot_",f_name,".png"))
      f_path_box_pdf = file.path(plot_dir,"pdf",paste0("boxplot_",f_name,".pdf"))
    }

    ggplot2::ggsave(
      filename=f_path_png,
      plot = deconv_plot,
      device = "png",
      units = "cm",
      width = 25,
      height = 20)
    ggplot2::ggsave(
      filename=f_path_pdf,
      plot = deconv_plot,
      device = "pdf",
      units = "cm",
      width = 25,
      height = 20)
    if(do_boxplots){
      ggplot2::ggsave(
        filename=f_path_box_png,
        plot = deconv_boxplot,
        device = "png",
        units = "cm",
        width = 25,
        height = 20)
      ggplot2::ggsave(
        filename=f_path_box_pdf,
        plot = deconv_boxplot,
        device = "pdf",
        units = "cm",
        width = 25,
        height = 20)
    }
  }

  return(res_list)
}

plot_deconvolution_hm <- function(
  deconv_mat,  # Classes as rows, Samples as columns
  name_tag,
  group_split_chr="_",
  group_split_pos=2,
  plot_dir=NULL,
  ...
){
  message(">>> Plotting deconvolution results in heatmap")

  if(any(is.na(deconv_mat))){
    warning("Returning NULL. 'deconv_mat' has NA values.")
    return(NULL)
  }

  source("src/Cell_type_color_code.R")

  grps <- stringr::str_split(
    string=as.character(colnames(deconv_mat)),
    pattern=group_split_chr,
    simplify=TRUE)[,group_split_pos]

  grpcol<-grDevices::colorRampPalette(
    rev(
      RColorBrewer::brewer.pal(
        n=ifelse(length(unique(grps))<3,3,length(unique(grps))),
        name="Spectral"
      )
    )
  )(length(unique(grps)))

  names(grpcol)<-unique(grps)

  clsses <- rownames(deconv_mat)
  clsscol <- get_cell_colors(rownames(deconv_mat))
  names(clsscol)<-clsses

  hmcol<-circlize::colorRamp2(
    breaks=scales::rescale(1:256,to=c(0,1)),
    colors=grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(n=11, name="RdBu")))(256)
  )

  dec_hm <- ComplexHeatmap::Heatmap(
    deconv_mat,
    col=hmcol,
    show_column_names=FALSE,
    top_annotation = ComplexHeatmap::HeatmapAnnotation(
      Group = grps,
      col=list(Group=grpcol),
      annotation_legend_param=list(
        legend_direction = "horizontal",
        title_position = "topcenter",nrow=1
      )
    ),
    right_annotation = ComplexHeatmap::rowAnnotation(
      Class = clsses,
      col=list(Class=clsscol),
      show_legend=FALSE
    ),
    show_heatmap_legend=TRUE,
    heatmap_legend_param = list(
      title = "cell proportion",
      legend_direction = "horizontal",
      title_position = "topcenter",
      at=c(0,.5,1)
    ),
    ...
  )

  file_tag <- paste0("deconvolution_heatmap_",name_tag)

  if(!is.null(plot_dir)){
    fpath_png <- file.path(plot_dir,paste0(file_tag,".png"))
    fpath_pdf <- file.path(plot_dir,paste0(file_tag,".pdf"))

    if(dir.exists(file.path(plot_dir,"png")) & dir.exists(file.path(plot_dir,"pdf"))){
      fpath_png <- file.path(plot_dir,"png",paste0(file_tag,".png"))
      fpath_pdf <- file.path(plot_dir,"pdf",paste0(file_tag,".pdf"))
    }

    png(
      filename=fpath_png,
      units = "px",
      res = 144,
      width = 960,
      height = 960
    )
    ComplexHeatmap::draw(
      dec_hm,
      heatmap_legend_side = "bottom",
      annotation_legend_side = "bottom",
      merge_legend=FALSE
    )
    dev.off()

    pdf(file=fpath_pdf)
    ComplexHeatmap::draw(
      dec_hm,
      heatmap_legend_side = "bottom",
      annotation_legend_side = "bottom",
      merge_legend=FALSE
    )
    dev.off()

  }else{
    ComplexHeatmap::draw(
      dec_hm,
      heatmap_legend_side = "bottom",
      annotation_legend_side = "bottom",
      merge_legend=FALSE
    )
  }
}




plot_deconvolution_signature <- function(
  signature_mat, # Features as rows, Classes as columns
  name_tag,
  sort_func=min,
  plot_dir=NULL
){
  source("src/functions/deconvolution_algorithms.R")
  sorted_mat <- sort_weights_mat(signature_mat,sort_func)

  sig_hm<-ComplexHeatmap::Heatmap(
    matrix=sorted_mat,
    col=circlize::colorRamp2(
      breaks=c(0,0.5,1),
      colors=colorRampPalette(rev(RColorBrewer::brewer.pal(n=5, name="RdBu")))(3)
    ),
    row_dend_reorder=FALSE,
    column_dend_reorder=FALSE,
    cluster_rows=FALSE,
    cluster_columns=FALSE
  )

  file_tag <- paste0("signatures_heatmap_",name_tag)

  if(!is.null(plot_dir)){
    fpath_png <- file.path(plot_dir,paste0(file_tag,".png"))
    fpath_pdf <- file.path(plot_dir,paste0(file_tag,".pdf"))

    if(dir.exists(file.path(plot_dir,"png")) & dir.exists(file.path(plot_dir,"pdf"))){
      fpath_png <- file.path(plot_dir,"png",paste0(file_tag,".png"))
      fpath_pdf <- file.path(plot_dir,"pdf",paste0(file_tag,".pdf"))
    }

    png(
      filename=fpath_png,
      units = "px",
      res = 144,
      width = 960,
      height = 960
    )
    ComplexHeatmap::draw(
      sig_hm,
      heatmap_legend_side = "bottom",
      annotation_legend_side = "bottom",
      merge_legend=FALSE
    )
    dev.off()

    pdf(file=fpath_pdf)
    ComplexHeatmap::draw(
      sig_hm,
      heatmap_legend_side = "bottom",
      annotation_legend_side = "bottom",
      merge_legend=FALSE
    )
    dev.off()

  }else{
    ComplexHeatmap::draw(
      sig_hm,
      heatmap_legend_side = "bottom",
      annotation_legend_side = "bottom",
      merge_legend=FALSE
    )
  }
  return(sorted_mat)
}
