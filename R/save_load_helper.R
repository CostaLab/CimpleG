#' Save an R object to disk with fast and efficient compression algorithms.
#'
#' @param object Object to be saved to disk.
#'
#' @param file_name Name of the file to be saved. Note that the extension
#'  (e.g. \code{.rds}, \code{.rds.zstd}, \code{.rds.lz4}) will be defined by
#'  \code{file_format}, so the user does not need to define it.
#'
#' @param file_format One of "zstd", "lz4", "gzip", "bzip2","xz", "nocomp".
#'  \code{zstd} is the best option, fast compression and loading times, low space usage.
#'
#' @export
save_object <- function(object, file_name, file_format=NULL){

  stopifnot(file_format %in% c("zstd", "lz4", "gzip", "bzip2", "xz", "nocomp"))

  if(file_format %in% "nocomp"){
    saveRDS(object = object, file = paste0(file_name, ".rds"), compress = FALSE)
    return(invisible(NULL))
  }

  if(file_format %in% c("zstd", "lz4")){
    con <- archive::file_write(
      file = paste0(file_name, ".rds.", file_format),
      filter = file_format
    )
    open(con)
    saveRDS(object = object, file = con)
    close(con)
  }else{
    saveRDS(object = object, file = paste0(file_name,".rds"), compress = file_format)
  }
}

#' Load an R object saved with CimpleG or an RDS file.
#'
#' @param file_name File name in the working directory or path to file
#'  to be loaded. Files saved with \code{CimpleG::save_object} and \code{.rds}
#'  files are supported. If the \code{archive} package is installed.
#' @export
load_object <- function(file_name){
  if(!file.exists(file_name)) stop(paste0("File '",file_name,"' not found."))
  con <- archive::file_read(file = file_name)
  res <- readRDS(file = con)
  close(con)
  return(res)
}

