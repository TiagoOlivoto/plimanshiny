#' @importFrom utils  globalVariables
.onAttach <- function(libname, pkgname) {
  vers <-  "0.0.0.9000"
  packageStartupMessage("|============================================================|")
  packageStartupMessage("| Shiny App for the pliman package  (plimanshiny ", vers,  ") |")
  packageStartupMessage("| Author: Tiago Olivoto                                      |")
  packageStartupMessage("| Type `citation('plimanshiny')` to know how to cite pliman  |")
  # packageStartupMessage("| Visit 'http://bit.ly/pkg_pliman' for a complete tutorial |")
  packageStartupMessage("|============================================================|")
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("geometry", "name", "value")
  )
}