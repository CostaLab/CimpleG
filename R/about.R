about <- function() {
  # TODO: add more info about package (purpose/objective, utilities, paper, citation)
  pkg_name <- "CimpleG"
  pkg_version <- utils::packageDescription(pkg_name, fields = "Version")

  pkg_msg <- paste0(pkg_name, " version ", pkg_version)
  sep_msg <- paste0(rep("-", nchar(pkg_msg)), collapse = "")
  pkg_msg <- paste0(sep_msg, "\n",pkg_msg, "\n", sep_msg)

  message(pkg_msg)

  msg <-
    paste0(
      "A package to find small CpG signatures."
    )

  message(msg)
  invisible(pkg_msg)
}
