#' download_shapefile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_shapefile_ui <- function(id, label = "Download shapefile"){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_8(
        downloadBttn(ns("downloadshapefile"),
                     label = label,
                     style = "pill")
      ),
      col_4(
        selectInput(ns("formatshp"),
                    label = "Format",
                    choices = c(".rds", ".shp", ".json", ".kml", ".gml"),
                    selected = ".rds")
      )
    )
  )
}

#' download_shapefile Server Functions
#'
#' @noRd
mod_download_shapefile_server <- function(id, data, name = "shapefile"){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$downloadshapefile <- downloadHandler(
        filename = function() {
          if(input$formatshp == ".shp"){
            paste0(name, ".zip")
          } else{
            paste(name, input$formatshp, sep = "")
          }
        },
        content = function(file) {
          if(input$formatshp == ".shp"){
            write_shp(data, file)
          } else{
            terra::writeVector(data, file)
          }
        }
      )
    }
  )
}

## To be copied in the UI
# mod_download_shapefile_ui("download_shapefile_1")

## To be copied in the server
# mod_download_shapefile_server("download_shapefile_1")
