#' download_mosaic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_mosaic_ui <- function(id, button_label = "Download") {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_5(
        downloadBttn(ns("downloadmosaic"),
                     label = button_label,
                     style = "pill")
      ),
      col_3(
        selectInput(ns("formatmosaic"),
                    label = "Format",
                    choices = c(".tif", ".png"))
      ),
      col_4(
        selectInput(ns("datatype"),
                    label = "Data type",
                    choices = c("auto", "INT1U", "INT2U", "INT2S", "INT4U", "INT8U", "INT8S", "INT4S", "FLT4S", "FLT8S"),
                    selected = "auto")
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
            if(input$datatype == "auto"){
              dty <- NULL
            } else{
              dty <- input$datatype
            }
            mosaic_export(data, file, datatype = dty)
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
