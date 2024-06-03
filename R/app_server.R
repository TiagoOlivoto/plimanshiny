#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Home
  mod_home_server("home_1")

  # Mosaic analysis
  mosaic_data <- reactiveValues()
  r <- reactiveValues(r = 1)
  g <- reactiveValues(g = 2)
  b <- reactiveValues(b = 3)
  re <- reactiveValues(re = NA)
  nir <- reactiveValues(nir = NA)
  swir <- reactiveValues(swir = NA)
  tir <- reactiveValues(tir = NA)
  quantiles <- reactiveValues(q = c(0, 1))
  maxpixel <- reactiveValues(mp = 1e6)
  basemap <- reactiveValues(map = NULL)
  bmap <- reactiveValues(map = NULL)
  index <- reactiveValues()
  pathmosaic <- reactiveValues(path = NULL)
  activemosaic <- reactiveValues(name = NULL)
  mod_mosaic_prepare_server("mosaic_prepare_1", mosaic_data, r, g, b, re, nir, swir, tir, basemap, pathmosaic, quantiles, maxpixel, activemosaic)

  # shapefile
  shapefile <- reactiveValues()
  mod_shapefile_prepare_server("shapefile_prepare_1", mosaic_data, basemap, shapefile, activemosaic,  r, g, b)
  mod_indexes_server("indexes_1", mosaic_data, r, g, b, re, nir, swir, tir, basemap, index, shapefile)
  mod_analyze_server("analyze_1", mosaic_data, basemap, shapefile, index, pathmosaic, dfs)
  mosaiclist <- reactiveValues()
  mod_cropbatch_server("cropbatch_1", shapefile, mosaiclist)

  # time series
  mod_timeseriesinput_server("timeseriesinput_1", shapefile, mosaiclist, r, g, b, re, nir,  swir, tir,  basemap)
  mod_timeseriesanalysis_server("timeseriesanalysis_1", shapefile, mosaiclist, r, g, b, re, nir, swir, tir, basemap, dfs)

  # manipulation
  mod_crop_server("crop_1", mosaic_data, shapefile, r, g, b, basemap)
  mod_plotclip_server("plotclip_1", mosaic_data, shapefile, r, g, b, basemap)
  mod_bindlayer_server("bindlayer_1", mosaic_data)
  mod_interpolate_server("interpolate_1", mosaic_data, r, g, b, basemap)
  mod_aggregate_server("aggregate_1", mosaic_data, r, g, b, basemap)
  mod_resample_server("resample_1", mosaic_data)
  mod_segment_server("segment_1", mosaic_data, r, g, b, re, nir)
  mod_sentinel_server("sentinel_1", mosaic_data)
  mod_spatjoin_server("spatjoin_1", shapefile)

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

  # Geostatistics
  mod_spatinterp_server("spatinterp_1", dfs, shapefile)

}


