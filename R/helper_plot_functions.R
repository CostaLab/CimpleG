save_different_plot_format <- function(
  plt = NULL, plot_dir = NULL, create_plot_subdir = FALSE,
  save_device = c("ggplot", "grDevice", "complexheatmap"),
  type_name = "", name_tag = "", formats = c("png", "pdf", "tiff"),
  units = "cm", width = 15, height = 15, ...
) {

  if (!is.null(plot_dir) & !is.null(plt)) {
    save_device <- match.arg(save_device)

    f_name <- paste0(type_name, "-", name_tag)

    for (fmt in formats) {
      f_path_fmt <- file.path(plot_dir, paste0(f_name, ".", fmt))
      if (create_plot_subdir) {
        dir.create(file.path(plot_dir, fmt), recursive = TRUE, showWarnings = FALSE)
      }
      if (dir.exists(file.path(plot_dir, fmt))) {
        f_path_fmt <- file.path(plot_dir, fmt, paste0(f_name, ".", fmt))
      }
      if (save_device == "ggplot") {
        ggplot2::ggsave(filename = f_path_fmt, plot = plt, device = fmt, units = units, width = width, height = height, ...)
      }
      if (save_device == "complexheatmap") {
        save_f <- get(fmt)
        if (fmt == "png" | fmt == "tiff") {
          save_f(f_path_fmt, width = width, height = height, units = "in", res = 300, ...)
        } else {
          save_f(f_path_fmt, width = width, height = height)
        }
        ComplexHeatmap::draw(plt)
        grDevices::dev.off()
      }
    }
  }
}

#' Make color palette data frame
#' @param classes Vector with classes for which to create a color palette
#' @export
make_color_palette <- function(classes){
  if(requireNamespace("ggsci",quietly = TRUE)){
    color_palette_df <- data.frame(
      classes=classes,
      class_color=grDevices::colorRampPalette(ggsci::pal_ucscgb()(26))(length(classes))
    )
  }else if(requireNamespace("RColorBrewer",quietly = TRUE)){
    color_palette_df <- data.frame(
      classes=classes,
      class_color=grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,"Set1"))(length(classes))
    )
  }else{
    color_palette_df <- data.frame(
      classes=classes,
      class_color=grDevices::colorRampPalette(grDevices::rainbow(length(classes)+1))(length(classes))
    )
  }
  return(color_palette_df)
}
