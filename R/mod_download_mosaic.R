#' download_mosaic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_mosaic_ui <- function(id, button_label = "Download mosaic") {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_8(
        downloadBttn(ns("downloadmosaic"),
                     label = button_label,
                     style = "pill")
      ),
      col_4(
        selectInput(ns("formatmosaic"),
                    label = "Format",
                    choices = c(".tif", ".jpg"))
      )
    )
  )
}

#' download_mosaic Server Functions
#'
#' @noRd
mod_download_mosaic_server <- function(id, data, name = "mosaic") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$downloadmosaic <- downloadHandler(
        filename = function() {
          paste(name, input$formatmosaic, sep = "")
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
