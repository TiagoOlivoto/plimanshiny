#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mosaic_data <- reactiveValues()
  r <- reactiveValues(r = 1)
  g <- reactiveValues(g = 2)
  b <- reactiveValues(b = 3)
  re <- reactiveValues(re = 4)
  nir <- reactiveValues(nir = 5)
  basemap <- reactiveValues(map = NULL)
  bmap <- reactiveValues(map = NULL)
  index <- reactiveValues(index = NULL)
  pathmosaic <- reactiveValues(path = NULL)
  mod_mosaic_prepare_server("mosaic_prepare_1", mosaic_data, r, g, b, re, nir, basemap, pathmosaic)
  shapefile <- reactiveValues()
  mod_shapefile_prepare_server("shapefile_prepare_1", mosaic_data, basemap, shapefile)
  mod_indexes_server("indexes_1", mosaic_data, r, g, b, re, nir, basemap, index, shapefile)
  mod_analyze_server("analyze_1", mosaic_data, basemap, shapefile, index, pathmosaic)
  mosaiclist <- reactiveValues()
  mod_timeseriesinput_server("timeseriesinput_1", shapefile, mosaiclist, r, g, b, re, nir, basemap)
  mod_cropbatch_server("cropbatch_1", shapefile, mosaiclist)

  # manipulation
  mod_crop_server("crop_1", mosaic_data, shapefile, r, g, b, basemap)
  mod_timeseriesanalysis_server("timeseriesanalysis_1", shapefile, mosaiclist, r, g, b, re, nir, basemap)
  mod_plotclip_server("plotclip_1", mosaic_data, shapefile, r, g, b, basemap)
  mod_bindlayer_server("bindlayer_1", mosaic_data)
  mod_interpolate_server("interpolate_1", mosaic_data, r, g, b, basemap)
  mod_aggregate_server("aggregate_1", mosaic_data, r, g, b, basemap)
  mod_resample_server("resample_1", mosaic_data)
  mod_segment_server("segment_1", mosaic_data, r, g, b, re, nir)
  mod_sentinel_server("sentinel_1", mosaic_data)

  # Image analysis
  imgdata <- reactiveValues()
  mod_imageimport_server("imageimport_1", imgdata)
  mod_imageanal_server("imageanal_1", imgdata)
  mod_imagesegment_server("imagesegment_1", imgdata)
  mod_imagepalette_server("imagepalette_1", imgdata)
  mod_slider_server("slider_1", imgdata)

  # Phytopathometry
  mod_measurediseaseind_server("measurediseaseind_1", imgdata)
  mod_imageimport_server("imageimport_2", imgdata)
  mod_colorpalette_server("colorpalette_1", imgdata)
  mod_measurediseasepal_server("measurediseasepal_1", imgdata)
  mod_measurediseasepick_server("measurediseasepick_1", imgdata)

  # Datasets
  dfs <- reactiveValues()
  mod_datasets_server("datasets_1", dfs)
  mod_dffilter_server("dffilter_1", dfs, shapefile)
  mod_dfedit_server("dfedit_1", dfs, shapefile)
  mod_dfupdate_server("dfupdate_1", dfs, shapefile)
  mod_dfjoin_server("dfjoin_1", dfs, shapefile)
}


