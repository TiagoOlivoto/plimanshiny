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
    c("geometry", "name", "value", "plot_id", "block", "individual", "ind", "coverage_fraction", "cl",
      "label", "symptomatic", "vals", "unique_id", "unique_plot", "plot_layout", "edit_id",
      "model_results", "parms", "x", "y", "vindex", "parms", "doy", "flights", "q90", "volume")
  )
}
