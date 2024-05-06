#' colorpalette UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_colorpalette_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Palette Settings",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          pickerInput(ns("imagepalette"),
                      label = "Image",
                      choices = NULL),
          textInput(ns("savepalas"),
                    label = "Save the palette as",
                    value = "image_palette"),
          fluidRow(
            col_4(
              numericInput(ns("radius"),
                           label = "Radius",
                           min = 1,
                           value = 2)
            ),
            col_4(
              numericInput(ns("width"),
                           label = "Width",
                           min = 10,
                           value = 150)
            ),
            col_4(
              numericInput(ns("height"),
                           label = "Height",
                           min = 10,
                           value = 100)
            )
          ),
          pickerInput(ns("shape"),
                      label = "Shape",
                      choices = c('box', 'disc', 'diamond', 'Gaussian', 'line')),
          fluidRow(
            col_6(
              actionBttn(ns("startpaletting"),
                         label = "Start picking!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("sampleploints"),
                         label = "   Done",
                         style = "pill",
                         no_outline = FALSE,
                         icon = icon("check"),
                         color = "success")
            )
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
          selected = "Sample points",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Sample points",
            editModUI(ns("pointpicksample"), height = "740px") |> add_spinner()
          ),
          tabPanel(
            "Created palette",
            plotOutput(ns("createdpal"), height = "740px")
          )
        )
      )
    )
  )
}

#' colorpalette Server Functions
#'
#' @noRd
mod_colorpalette_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    bmap <- reactiveValues(map = NULL)
    # Define a reactiveValues object to track whether the observer has been executed
    observeOnce <- reactiveValues(once = "sim")

    observe({
      # Check if the observer hasn't been executed yet
      if (observeOnce$once == "sim") {
        avalimg <-  setdiff(names(imgdata), "img")
        req(avalimg)
        updatePickerInput(session, "imagepalette", choices = avalimg, selected = avalimg[[1]])

        # Mark that the observer has been executed
        observeOnce$once <- "nao"
      }
    })
    observe({
      req(imgdata)
      req(input$imagepalette)
      bmap$map <- image_view(imgdata[[input$imagepalette]]$data,
                             max_pixels = 500000)
    })
    lastvalue <- reactiveValues(val = "tt")
    points <- reactiveValues(val = NULL)
    observeEvent(input$startpaletting, {
      req(bmap$map)
      cpoints <- callModule(editMod, "pointpicksample", bmap$map@map , editor = "leafpm")
      observeEvent(cpoints()$finished, {
        if(lastvalue$val != "tt" & nrow(cpoints()$finished) > 1){
          points$val <-
            cpoints()$finished |>
            sf::st_transform(3857) |>
            dplyr::select(geometry) |>
            sf::st_coordinates() |>
            as.data.frame() |>
            dplyr::slice(2:nrow(cpoints()$finished))
        } else{
          points$val <-
            cpoints()$finished |>
            sf::st_transform(3857) |>
            dplyr::select(geometry) |>
            sf::st_coordinates() |>
            as.data.frame()
        }
      })
      observeEvent(input$sampleploints,{
        # Check if edits()$finished is not NULL and shapedone is FALSE
        if (!is.null(points$val)) {
          mvpoin <- points$val
          pal <-
            create_palette(imgdata[[input$imagepalette]]$data,
                           points = mvpoin,
                           width = input$width,
                           height = input$height,
                           shape = input$shape,
                           r = input$radius)

          output$createdpal <- renderPlot({
            plot(pal)
          })
          imgdata[[input$savepalas]] <- create_reactval(name = input$savepalas, data = pal)
          lastvalue$val <- "ff"
        }
      })
    })
    observeEvent(input$sampleploints,{
      sendSweetAlert(
        session = session,
        title = "Palette successfully created",
        text = glue::glue("The color palette '{input$savepalas}' has been created and is now available for further analysis in the tab 'Image'"),
        type = "success"
      )
    })
  })
}

## To be copied in the UI
# mod_colorpalette_ui("colorpalette_1")

## To be copied in the server
# mod_colorpalette_server("colorpalette_1")
