#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#' @param upload_size Shiny limits file uploads. Defaults to 10000 MB (10 GB).
#' @export
#' @importFrom golem with_golem_options
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom shiny shinyApp
#' @importFrom segmented segmented.lm seg.control intercept
#' @importFrom magick image_read image_animate image_write
#' @importFrom tidyr unnest
#' @importFrom graphics abline par title layout legend matplot
#' @importFrom stats binomial glm reorder runif dist D coef integrate lm nls nls.control optimise setNames smooth approx loess
#' @importFrom grDevices dev.off jpeg adjustcolor col2rgb colorRampPalette rgb png terrain.colors
#' @importFrom utils data stack write.csv
#' @importFrom future plan multisession
#' @importFrom doFuture `%dofuture%`
#' @import shinyWidgets mapview leaflet mapedit sf rintrojs leafsync esquisse waiter shinycssloaders ggplot2 shinyFiles foreach histoslider tidyterra datamods reactable drc
#' @rawNamespace import(bs4Dash, except = c(progressBar, insertTab, actionButton, updateTabsetPanel, column, tabsetPanel, tabPanel, navbarMenu))
#' @rawNamespace import(terra, except = c(panel, shift, distance))
#' @rawNamespace import(fields, except = c(addLegend, describe))
#' @rawNamespace import(purrr, except = c(accumulate, when))
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
    print = TRUE,
    golem_opts = list(...)
  )
}

