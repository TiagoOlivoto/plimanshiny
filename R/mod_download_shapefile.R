#' download_shapefile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_shapefile_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadBttn(ns("downloadshapefile"),
                 label = "Download shapefile",
                 style = "pill")
  )
}

#' download_shapefile Server Functions
#'
#' @noRd
mod_download_shapefile_server <- function(id, data){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$downloadshapefile <- downloadHandler(
        filename = function() {
          paste("shapefile", ".rds", sep = "")
        },
        content = function(file) {
          terra::writeVector(data, file)
        }
      )
    }
  )
}

## To be copied in the UI
# mod_download_shapefile_ui("download_shapefile_1")

## To be copied in the server
# mod_download_shapefile_server("download_shapefile_1")
