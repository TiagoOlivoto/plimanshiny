#' download_excel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_excel_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' download_excel Server Functions
#'
#' @noRd 
mod_download_excel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_download_excel_ui("download_excel_1")
    
## To be copied in the server
# mod_download_excel_server("download_excel_1")
