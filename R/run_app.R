#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @importFrom golem with_golem_options
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom shiny shinyApp
#' @import shinyWidgets mapview leaflet mapedit sf rintrojs leafsync esquisse waiter shinycssloaders ggplot2 fresh
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel, navbarMenu))
#' @rawNamespace import(terra, except = c(panel, shift, distance))
#' @rawNamespace import(leaflet.extras2, except = c(menuItem, addSpinner))
#' @rawNamespace import(pliman, except = c(`%>%`))
run_app <- function(...) {
  options(shiny.maxRequestSize = 10000 * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(launch.browser = TRUE)
    ),
    golem_opts = list(...)
  )
}

