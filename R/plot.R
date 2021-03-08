
# plot diffmean sumvar function
pltDiffMeanSumVarBetaAnnot <- function(
  meanVarBeta_df,
  xcol="diffMeans",
  ycol="sumVariance",
  feature_id_col="cpg_id",
  is_feature_selected_col=NULL,
  label_var1,
  label_var2="Others",
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

  assertthat::assert_that(is.data.frame(meanVarBeta_df))

  assertthat::assert_that(xcol%in%colnames(meanVarBeta_df))
  assertthat::assert_that(ycol%in%colnames(meanVarBeta_df))

  if(!feature_id_col%in%colnames(meanVarBeta_df)){
    meanVarBeta_df[,feature_id_col] = rownames(meanVarBeta_df)
  }
  assertthat::assert_that(feature_id_col%in%colnames(meanVarBeta_df))

  assertthat::assert_that(typeof(label_var1) == 'character' && typeof(label_var2) == 'character')
  # NULL or else
  assertthat::assert_that(is_feature_selected_col %in% colnames(meanVarBeta_df) || is.null(is_feature_selected_col))
  assertthat::assert_that(typeof(mean_cutoff) == 'double' || is.null(mean_cutoff))
  assertthat::assert_that(typeof(var_cutoff) == 'double' || is.null(var_cutoff))
  assertthat::assert_that(typeof(func_factor) == 'double' || is.null(func_factor))
  assertthat::assert_that(is.function(threshold_func) || is.null(threshold_func))
  # assertthat::assert_that()

  density_type = match.arg(density_type)

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
    col <- col2rgb(color)
    col <- col + (255 - col)*factor
    col <- rgb(t(col), maxColorValue=255)
    col
  }
  light_points_color <- lighten(points_color,0.7)

  #message("plt diffMean,sumVariance")
  if(!is.null(is_feature_selected_col)){
    plt_diffMeanSumVar <- ggplot(
      meanVarBeta_df,
      aes(
        x=!!sym(xcol),
        y=!!sym(ycol),
        fill=!!sym(is_feature_selected_col),
        color=!!sym(is_feature_selected_col)
      )
    )

    point_color_vec <- ifelse(
      meanVarBeta_df[,is_feature_selected_col],
      points_color,
      light_points_color
    )
    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      geom_point(
        colour=point_color_vec,
        alpha=0.5)
  }else{
    plt_diffMeanSumVar <- ggplot(
      meanVarBeta_df,
      aes(x=!!sym(xcol), y=!!sym(ycol))
    )
    if(!is.null(feats_to_highlight)){
      plt_diffMeanSumVar <- plt_diffMeanSumVar +
        geom_point(
          data=function(x){x[!x[,feature_id_col] %in% feats_to_highlight,]},
          colour=points_color,
          alpha=0.3)+
        geom_point(
          data=function(x){x[x[,feature_id_col] %in% feats_to_highlight,]},
          colour=ifelse(!is.null(best_cpgs_df),"orange3","red"),
          alpha=1)
    }else{
      plt_diffMeanSumVar <- plt_diffMeanSumVar +
        geom_point(colour=points_color,alpha=0.3)
    }
  }

  if(!is.null(best_cpgs_df)){
    plt_diffMeanSumVar <- plt_diffMeanSumVar + geom_point(
      data=function(x){x[x[,feature_id_col] %in% best_cpgs_df$.id,]},
      colour="red",
      alpha=1
    )

    best_label <- paste0(best_cpgs_df$Rank,"#",best_cpgs_df$.id)

    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      geom_label_repel(
        data = meanVarBeta_df[best_cpgs_df$.id,],
        aes(
          x=!!sym(xcol),
          y=!!sym(ycol),
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
      geom_label_repel(
        data = meanVarBeta_df[feats_to_highlight,],
        aes(
          x=!!sym(xcol),
          y=!!sym(ycol),
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
    # Regions #BUG ggMarginal
    # plt_diffMeanSumVar <- plt_diffMeanSumVar +
    # geom_tile(data=data.frame(x=seq(0,1,0.01), y=0),
    #           inherit.aes = FALSE,
    #           aes(x=x, y=y,alpha=abs(x)),fill="red")+
    # geom_tile(data=data.frame(x=seq(-1,0,0.01), y=0),
    #           inherit.aes = FALSE,
    #           aes(x=x, y=y,alpha=abs(x)),fill="blue")+
    # scale_alpha(guide=FALSE,range = c(0.0, 0.15))


  if(!is.null(mean_cutoff) & !is.null(var_cutoff)){
    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      geom_vline(xintercept = -mean_cutoff, alpha=0.5) +
      geom_vline(xintercept = mean_cutoff, alpha=0.5) +
      geom_vline(xintercept = 0, alpha=0.5) +
      geom_hline(yintercept = var_cutoff, alpha=0.5)
  }

  plt_diffMeanSumVar <- plt_diffMeanSumVar +
  labs(
    x=expression(bar(beta)[paste(cell)] - bar(beta)[others]),
    y=expression(var(beta[paste(cell)]) + var(beta[paste(others)])),
    title=paste0(label_var1," vs ",label_var2),
    subtitle=ifelse(
      !is.null(best_cpgs_df)||!is.null(feats_to_highlight),
      "Best predictors (hyper and hypo) are annotated.",
      ""
    ),
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
    label_ypos <- quantile(meanVarBeta_df$sumVariance)["75%"]
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
      label_ypos <- (quantile(meanVarBeta_df[,ycol])["100%"]+quantile(meanVarBeta_df[,ycol])["75%"])/3
      label_xpos <- 0.

      plt_diffMeanSumVar <- plt_diffMeanSumVar +
        geom_text(
          inherit.aes=FALSE,
          data=data.frame(
            x=c(-1,1),
            y=c(0,0),
            txt=c(
              paste0("n[hypo]==",nrow(meanVarBeta_df[meanVarBeta_df[,xcol]<0 & meanVarBeta_df$selected_feat,])),
              paste0("n[hyper]==",nrow(meanVarBeta_df[meanVarBeta_df[,xcol]>0 & meanVarBeta_df$selected_feat,]))
            )
          ),
          aes(
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
      stat_function(
        inherit.aes=FALSE,
        data = data.frame(x=c(-1,1)),
        aes(x),
        fun=threshold_func,
        args=func_factor,
        size=1.5,
        geom="line",
        color="grey40"
      )

    plt_diffMeanSumVar <- plt_diffMeanSumVar +
      stat_function(
        inherit.aes=FALSE,
        data = data.frame(x=c(-1,1)),
        aes(x),
        fun=threshold_func,
        args=func_factor,
        size=1.5,
        geom="line",
        color="grey40"
      )+
      annotate(
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
      geom_label_repel(
        data = meanVarBeta_df[feats_to_highlight,],
        aes(
          x=!!sym(xcol),
          y=!!sym(ycol),
          label=feats_to_highlight
        ),
        arrow = arrow(
          length = unit(0.05, "npc"),
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
        geom_label_repel(
          data = meanVarBeta_df[feats_to_highlight,],
          aes(
            x=!!sym(xcol),
            y=!!sym(ycol),
            label=feats_to_highlight
          ),
          arrow = arrow(
            length = unit(0.05, "npc"),
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
    max(meanVarBeta_df[,ycol])<0.4,
    0.4,
    max(meanVarBeta_df[,ycol])
  )

  plt_diffMeanSumVar <- plt_diffMeanSumVar +
    #scale_color_manual(values = color_vals)+
    theme_minimal(base_size=18)+
    theme(
      legend.position="none",
      axis.title.y = element_text(face="bold",size=22),
      axis.title.x = element_text(face="bold",size=22),
      axis.text.x = element_text(face="bold",size=22),
      axis.text.y = element_text(face="bold",size=22),
      plot.caption = element_text(size=9)
    )+
    xlim(c(-1,1))+
    ylim(c(0,ymaxlim))
  #print(plt_diffMeanSumVar)

  simple_plot = simple_plot +
    theme_minimal(base_size=18)+
    theme(
      legend.position="none",
      axis.title.y = element_text(face="bold",size=22),
      axis.title.x = element_text(face="bold",size=22),
      axis.text.x = element_text(face="bold",size=22),
      axis.text.y = element_text(face="bold",size=22),
      plot.caption = element_text(size=9)
    )+
    xlim(c(-1,1))+
    ylim(c(0,ymaxlim))

  if(pltDensity){
    plt_diffMeanSumVar <- ggMarginal(
      p=plt_diffMeanSumVar,
      data=meanVarBeta_df,
      x=xcol,
      y=ycol,
      groupFill=!is.null(is_feature_selected_col),
      groupColour=!is.null(is_feature_selected_col),
      type=density_type,
      size=10)

    simple_plot = ggMarginal(
      p=simple_plot,
      data=meanVarBeta_df,
      x=xcol,
      y=ycol,
      groupFill=!is.null(is_feature_selected_col),
      groupColour=!is.null(is_feature_selected_col),
      type=density_type,
      size=10)
  }

  name_tag <- ifelse(
    is.null(file_tag),
    "_diffMeanSumVar",
    paste0("_",file_tag,"_diffMeanSumVar")
  )

  if(!is.null(plot_dir)){

    f_name = paste0(
      format(Sys.time(),"%Y%m%d-%H%M%S"),
      "_cond-",
      label_var1,
      name_tag
    )

    f_path_png = file.path(plot_dir,paste0(f_name,".png"))
    f_path_pdf = file.path(plot_dir,paste0(f_name,".pdf"))

    if(dir.exists(file.path(plot_dir,"png")) & dir.exists(file.path(plot_dir,"pdf"))){
      f_path_png = file.path(plot_dir,"png",paste0(f_name,".png"))
      f_path_pdf = file.path(plot_dir,"pdf",paste0(f_name,".pdf"))
    }

    ggsave(
      filename=f_path_png,
      plot = plt_diffMeanSumVar,
      device = "png",
      units = "cm",
      width = 20,
      height = 20)
    # ggsave(
    #   filename=f_path_pdf,
    #   plot = plt_diffMeanSumVar,
    #   device = "pdf",
    #   units = "cm",
    #   width = 5,
    #   height = 5)

    ggsave(
      filename=gsub(
        x=f_path_png,
        pattern=name_tag,
        replacement="_diffMeanSumVar_simple"
      ),
      plot = simple_plot,
      device = "png",
      units = "cm",
      width = 20,
      height = 20)
    # ggsave(
    #   filename=gsub(
    #     x=f_path_pdf,
    #     pattern=name_tag,
    #     replacement="_diffMeanSumVar_simple"
    #   ),
    #   plot = simple_plot,
    #   device = "pdf",
    #   units = "cm",
    #   width = 5,
    #   height = 5
    # )


  }
  return(plt_diffMeanSumVar)
}

if(FALSE){
  selected_features = readRDS("/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/output/feature_selection/ver2.0.0_MSCORFIBRO_rfk-0.01_M-cvglmnet_fw-aupr_DEBUG-test_selected_features.RDS")
  mean_var_df = readRDS("/home/tiago/R_projects/DNAmSignatures/ProcessGEOData/input_data/train_test_data/2.0.0/training_data_CELL_TYPE_mean_var_stats_list_2.0.0.RDS")


  resp_classes <- toupper(data.table::fread(file.path("src","selected_vars_train.txt"),header=F)[[1]])

  mean_var_df = (mean_var_df$CELL_TYPE_MSCORFIBRO)
  mean_var_df$CPG_ID = rownames(mean_var_df)

  mean_var_df$IS_SELECTED = mean_var_df$CPG_ID %in% selected_features


# is_feature_selected_col="selected_feat"
pltDiffMeanSumVarBetaAnnot(
  meanVarBeta_df = mean_var_df,
  xcol="DIFF_MEANS",
  ycol="SUM_VARIANCE",
  feature_id_col="CPG_ID",
  # is_feature_selected_col="IS_SELECTED",
  feats_to_highlight=selected_features[1:20],
  is_feature_selected_col=NULL,
  label_var1="mscfib",
  label_var2="other",
  plot_dir=NULL,
  pltDensity=TRUE,
  density_type="density"
)

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
