#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Create the reactiveValues object to store the mosaic data
  mosaic_data <- reactiveValues(mosaic = NULL)
  r <- reactiveValues(r = NULL)
  g <- reactiveValues(g = NULL)
  b <- reactiveValues(b = NULL)
  re <- reactiveValues(re = NULL)
  nir <- reactiveValues(nir = NULL)
  basemap <- reactiveValues(map = NULL)

  # Call the import mosaic module
  mod_mosaic_prepare_server("mosaic_prepare_1", mosaic_data, r, g, b, re, nir, basemap)
  # shapefile

  shapefile <- reactiveValues(shapefile = NULL)
  mod_shapefile_prepare_server("shapefile_prepare_1", mosaic_data, basemap, shapefile)

}

