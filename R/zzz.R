.onAttach <- function(libname, pkgname) {
  version <- read.dcf(
    file = system.file("DESCRIPTION", package = pkgname),
    fields = "Version"
  )[1]
  packageStartupMessage(
    paste(pkgname, version, "is still in the development phase")
  )
}
