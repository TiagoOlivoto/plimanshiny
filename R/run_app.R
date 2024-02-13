#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param upload_size Shiny limits file uploads. Defaults to 10000 MB (10 GB).
#' @export
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @importFrom golem with_golem_options
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom shiny shinyApp
#' @importFrom tidyr unnest
#' @importFrom graphics abline
#' @importFrom stats binomial glm
#' @importFrom utils data stack
#' @importFrom exactextractr exact_extract
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @import shinyWidgets mapview leaflet mapedit sf rintrojs leafsync esquisse waiter shinycssloaders ggplot2 shinyFiles foreach
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel, navbarMenu))
#' @rawNamespace import(terra, except = c(panel, shift, distance))
#' @rawNamespace import(leaflet.extras2, except = c(menuItem, addSpinner))
#' @rawNamespace import(pliman, except = c(`%>%`))
run_app <- function(upload_size = 10000,
                    ...) {
  options(shiny.maxRequestSize = 10000 * 1024^2)
  terra::terraOptions(memfrac = 0.8)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(launch.browser = TRUE)
    ),
    golem_opts = list(...)
  )
}

