#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mosaic_data <- reactiveValues()
  r <- reactiveValues(r = NULL)
  g <- reactiveValues(g = NULL)
  b <- reactiveValues(b = NULL)
  re <- reactiveValues(re = NULL)
  nir <- reactiveValues(nir = NULL)
  basemap <- reactiveValues(map = NULL)
  bmap <- reactiveValues(map = NULL)
  index <- reactiveValues(index = NULL)
  pathmosaic <- reactiveValues(path = NULL)
  mod_mosaic_prepare_server("mosaic_prepare_1", mosaic_data, r, g, b, re, nir, basemap, pathmosaic)
  shapefile <- reactiveValues()
  mod_shapefile_prepare_server("shapefile_prepare_1", mosaic_data, basemap, shapefile)
  mod_indexes_server("indexes_1", mosaic_data, r, g, b, re, nir, basemap, index, shapefile)
  mod_analyze_server("analyze_1", mosaic_data, basemap, shapefile, index, pathmosaic)
  # manipulation
  mod_crop_server("crop_1", mosaic_data, shapefile, r, g, b, basemap)
  mod_plotclip_server("plotclip_1", mosaic_data, shapefile, r, g, b, basemap)
  mod_bindlayer_server("bindlayer_1", mosaic_data)
  mod_interpolate_server("interpolate_1", mosaic_data, r, g, b, basemap)
  mod_aggregate_server("aggregate_1", mosaic_data, r, g, b, basemap)
  mod_resample_server("resample_1", mosaic_data)
  mod_segment_server("segment_1", mosaic_data, r, g, b, re, nir)

  # Image analysis
  imgdata <- reactiveValues()
  mod_imageimport_server("imageimport_1", imgdata)
  mod_imageanal_server("imageanal_1", imgdata)
}

