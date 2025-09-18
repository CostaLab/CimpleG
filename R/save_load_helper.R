#' Save an R object to disk with fast and efficient compression algorithms.
#'
#' @param object Object to be saved to disk.
#'
#' @param file_name Name of the file where the R object is saved to.
#'
#' @param file_format One of "lz4", "gzip", "bzip2","xz", "nocomp".
#'  \code{lz4} is the best option, fast compression and loading times, low space usage.
#'  Format "lz4" is only available if package \code{archive} is installed.
#'  Format "zstd"  is not supported anymore as the library now needs to be precompiled with R.
#'
#' @export
save_object <- function(object, file_name, file_format = "lz4") {
  stopifnot(file_format %in% c("lz4", "gzip", "bzip2", "xz", "nocomp"))
  stopifnot(length(file_format) == 1)

  if (file_format %in% c("lz4")) {
    if (requireNamespace("archive", quietly = TRUE)) {
      con <- archive::file_write(file = file_name, filter = file_format)
      open(con)
      saveRDS(object = object, file = con)
      close(con)
    } else {
      warning("Package 'archive' needs to be installed to compress files in format 'lz4'.\n Saving object with default 'saveRDS()' function instead.")
      saveRDS(object = object, file = file_name, compress = TRUE)
    }
    return(invisible(NULL))
  }

  saveRDS(
    object = object,
    file = file_name,
    compress = ifelse(file_format %in% "nocomp", FALSE, file_format)
  )
}

#' Load an R object saved with CimpleG or an RDS file.
#'
#' @param file_name File name in the working directory or path to file
#'  to be loaded. Files saved with \code{CimpleG::save_object} and \code{base::saveRDS}
#'  files are supported.
#' @export
load_object <- function(file_name) {
  if (!file.exists(file_name)) stop(paste0("File '", file_name, "' not found."))

  if (requireNamespace("archive", quietly = TRUE)) {
    con <- archive::file_read(file = file_name)
    res <- readRDS(file = con)
    close(con)
    return(res)
  }

  res <- readRDS(file = file_name)
}
