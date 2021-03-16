.onAttach <- function(libname, pkgname) {
  pkg_version <- utils::packageDescription(pkgname, fields = "Version")

  pkg_msg <- paste0(pkgname, " version ", pkg_version)
  sep_msg <- paste0(rep("-", nchar(pkg_msg)), collapse = "")
  pkg_msg <- paste0(sep_msg, "\n",pkg_msg, "\n", sep_msg)

  packageStartupMessage(pkg_msg)
}
