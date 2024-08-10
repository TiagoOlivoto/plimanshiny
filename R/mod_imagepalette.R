#' imagepalette UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imagepalette_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Palette Settings",
          collapsible = FALSE,
          width = 12,
          height = "720px",
          hl(),
          h3("Input"),
          selectInput(ns("img_to_palette"),
                      label = "Source image",
                      choices = NULL),
          numericInput(
            inputId = ns("nclasses"),
            label = "Number of classes",
            value = 4
          ),
          prettyCheckbox(
            inputId = ns("proportional"),
            label = "Proportional",
            value = TRUE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          )
        )

      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Color Palette",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Color Palette",
            plotOutput(ns("colorpalette"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Results",
            reactable::reactableOutput(ns("resultpalette"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      )
    )
  )
}

#' imagepalette Server Functions
#'
#' @noRd
mod_imagepalette_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(imgdata$img)
      updateSelectInput(session, "img_to_palette", choices = c("Active image", setdiff(names(imgdata), "img")), selected = "Active image")
    })


    parms <- reactive({
      if(input$img_to_palette == "Active image"){
        img <- imgdata$img
      } else{
        img <- imgdata[[input$img_to_palette]]$data

      }
      image_palette(img,
                    npal = input$nclasses,
                    proportional = input$proportional)
    })

    output$colorpalette <- renderPlot({
      req(parms())
      plot(parms()$joint)
    })

    output$resultplottab <- reactable::renderReactable(
      parms()$proportions |>
        as.data.frame() |>
        roundcols(digits = 3) |>
        render_reactable()
    )

  })
}

## To be copied in the UI
# mod_imagepalette_ui("imagepalette_1")

## To be copied in the server
# mod_imagepalette_server("imagepalette_1")
