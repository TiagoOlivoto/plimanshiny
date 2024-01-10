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
