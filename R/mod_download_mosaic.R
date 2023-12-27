#' download_mosaic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_mosaic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadBttn(ns("downloadmosaic"),
                 label = "Download mosaic",
                 style = "pill")
  )
}

#' download_mosaic Server Functions
#'
#' @noRd
mod_download_mosaic_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$downloadmosaic <- downloadHandler(
        filename = function() {
          paste("mosaic", ".tif", sep = "")
        },
        content = function(file) {
          terra::writeRaster(data, file)
        }
      )
    }
  )
}

## To be copied in the UI
# mod_download_mosaic_ui("download_mosaic_1")

## To be copied in the server
# mod_download_mosaic_server("download_mosaic_1")
