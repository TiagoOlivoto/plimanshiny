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
                    choices = c(".tif", ".png"))
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
          if(nlyr(data) > 4 & input$formatmosaic == ".png"){
            sendSweetAlert(
              session = session,
              title = "Error",
              text = paste0("PNG driver doesn't support ", paste0(nlyr(data)), " bands.  Must be 1 (grey), 2 (grey+alpha), 3 (rgb) or 4 (rgba) bands"),
              type = "error"
            )
          }else{
            terra::writeRaster(data, file, gdal=c("COMPRESS=DEFLATE"))
          }
        }
      )
    }
  )
}

## To be copied in the UI
# mod_download_mosaic_ui("download_mosaic_1")

## To be copied in the server
# mod_download_mosaic_server("download_mosaic_1")
