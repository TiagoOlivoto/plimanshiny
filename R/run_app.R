#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel, navbarMenu))
#' @rawNamespace import(terra, except = c(panel, shift, distance))
#' @rawNamespace import(pliman, except = c(`%>%`))
#' @import shinyWidgets data.table  mapview leaflet mapedit sf
run_app <- function(...) {
  options(shiny.maxRequestSize = 5000 * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(launch.browser = TRUE)
    ),
    golem_opts = list(...)
  )
}
