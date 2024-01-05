#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        bs4Card(
          width = 12,
          # status = "success",
          solidHeader = FALSE,
          img(src = "www/plimanshiny.png", width = "100%", height = "80%")
        )
      ),
      column(
        width = 2,
        bs4Dash::valueBox(
          value = "MAIN REFERENCE",
          subtitle = "Olivoto, T. 2022. Lights, camera, pliman! An R package for plant image analysis. Methods in Ecology and Evolution 13(4): 789\u20137898. doi: 10.1111/2041-210X.13803.",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13803",
          icon = shiny::icon("braille")
        ),
        bs4Dash::valueBox(
          value = "PAPER TROPICAL PLANT PATHOLOGY",
          subtitle = "Olivoto, T., S.M.P. Andrade, and E.M. Del Ponte. 2022. Measuring plant disease severity in R: introducing and evaluating the pliman package. Tropical Plant Pathology 1: 1\u2013710. doi: 10.1007/S40858-021-00487-5.",
          width = 12,
          color = "success",
          elevation = 3,
          href = "https://link.springer.com/article/10.1007/s40858-021-00487-5",
          icon = shiny::icon("braille")
        ),
        bs4Dash::userBox(
          width = 12,
          status = "success",
          title = userDescription(
            title = "Tiago Olivoto",
            subtitle = "Developer",
            type = 2,
            image = "https://olivoto.netlify.app/authors/admin/avatar_hu34cb0b15d8f30e702e31c964be2e3330_3157592_270x270_fill_lanczos_center_2.png",
          ),
          "Departamento de Fitotecnia", br(),
          "Centro de Ciencias Agrarias", br(),
          "Universidade Federal de Santa Catarina", br()
        )
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
