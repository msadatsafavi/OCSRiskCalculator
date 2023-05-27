.onLoad <- function(libname, pkgname) {
  cat(paste("Welcome to",pkgname))
  evidence <<- get_evidence()
}
