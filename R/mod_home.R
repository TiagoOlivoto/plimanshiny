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
        actionBttn(
          inputId = ns("about"),
          label = "About",
          color = "success",
          icon = icon("circle-info")
        ),
        actionBttn(
          inputId = ns("similartools"),
          label = "Similar tools",
          color = "success",
          icon = icon("screwdriver-wrench")
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

    observeEvent(input$about, {
      showModal(
        modalDialog(
          title = "About plimanshiny",
          fluidRow(
            col_7(
              img(src = "www/help_logo.png", width = "100%", height = "100%"),
            ),
            col_5(
              box(
                width = 12,
                headerBorder = FALSE,
                collapsible = TRUE,
                closable = TRUE,
                h2("About"),
                "{plimanshiny} provides an interactive Shiny-based graphical user interface for the pliman package,
                facilitating user-friendly access to advanced plant image analysis tools without the need
                for extensive programming knowledge. This package integrates a variety of functionalities
                for high-throughput phenotyping, including but not limited to orthomosaic analysis from drone
                and satellite imagery, shapefile creation and handling, time series analysis, image analysis,
                and phytopathometry, into a cohesive and intuitive application.", br(),br(),
                h2("Developer"),
                a("Prof. Dr. Tiago Olivoto", href = "https://olivoto.netlify.app/", target = "_blank"), br(),
                "Department of Plant Science", br(),
                "Federal University of Santa Catarina", br(), br(),
                h2("Contribution"),
                a("Dr. Leonardo Volpato", href = "https://www.linkedin.com/in/leonardo-volpato/", target = "_blank"), br(),br(),br(),
                fluidRow(
                  col_6(
                    shiny::actionButton(inputId= ns("gitpliman"),
                                        label="pliman",
                                        icon = icon("github"),
                                        onclick ="window.open('https://github.com/TiagoOlivoto/pliman', '_blank')"),
                  ),
                  col_6(
                    shiny::actionButton(inputId= ns("gitplimanshiny"),
                                        label="plimanshiny",
                                        icon = icon("github"),
                                        onclick ="window.open('https://github.com/TiagoOlivoto/plimanshiny', '_blank')"),
                  )
                )
              )
            )
          ),


          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )

    })

    observeEvent(input$similartools, {
      showModal(
        modalDialog(
          title = "Similar tools",
          fluidRow(
            col_2(
              img(src = "www/fire.png", width = "100%", height = "100%"),
            ),
            col_10(
              "Package with new tools to support FIELDimageR software on evaluating GIS images from agriculture field trials.",br(),
              shiny::actionButton(inputId= ns("gitfire"),
                                  label="FIELDimageR.Extra",
                                  icon = icon("github"),
                                  onclick ="window.open('https://github.com/filipematias23/FIELDimageR.Extra', '_blank')"),
            )

          ),
          fluidRow(
            col_2(
              img(src = "www/firqgis.png", width = "100%", height = "100%"),
            ),
            col_10(
              "A compilation of functions made in R to analyze orthomosaic images from research field trials from agriculture or plant breeding experiments using QGIS", br(),
              shiny::actionButton(inputId= ns("gitplimanshiny"),
                                  label="FIELDimageR-QGIS",
                                  icon = icon("github"),
                                  onclick ="window.open('https://github.com/filipematias23/FIELDimageR-QGIS', '_blank')"),
            )

          ),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )
    })

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
