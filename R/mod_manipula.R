#' manipula UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manipula_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4TabCard(
      id = "tabs",
      status = "success",
      width = 12,
      height = "790px",
      title = "Manipulation",
      selected = "Crop",
      solidHeader = FALSE,
      type = "tabs",
      icon = icon("screwdriver-wrench"),
      tabPanel(
        title = "Crop",
        icon = icon("crop"),
        mod_crop_ui("crop_1")
      ),
      tabPanel(
        title = "Crop (batch)",
        icon = icon("crop"),
        mod_cropbatch_ui("cropbatch_1")
      ),
      tabPanel(
        title = "Mask/Segment",
        icon = icon("mask"),
        mod_segment_ui("segment_1")
      ),
      tabPanel(
        title = "Plot Clips",
        icon = icon("grip"),
        mod_plotclip_ui("plotclip_1")
      ),
      tabPanel(
        title = "Bind Layers",
        icon = icon("layer-group"),
        mod_bindlayer_ui("bindlayer_1")
      ),
      tabPanel(
        title = "Aggregate",
        icon = icon("users"),
        mod_aggregate_ui("aggregate_1")
      ),
      tabPanel(
        title = "Resample",
        icon = icon("arrow-up-right-dots"),
        mod_resample_ui("resample_1")
      ),
      tabPanel(
        title = "Interpolate",
        icon = icon("chart-line"),
        mod_interpolate_ui("interpolate_1")
      ),
      tabPanel(
        title = "Sentinel2",
        icon = icon("satellite"),
        mod_sentinel_ui("sentinel_1")
      ),
      tabPanel(
        title = "Geometry operations",
        icon = icon("map"),
        mod_spatjoin_ui("spatjoin_1")
      )
    )
  )
}

#' manipula Server Functions
#'
#' @noRd
mod_manipula_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_manipula_ui("manipula_1")

## To be copied in the server
# mod_manipula_server("manipula_1")
