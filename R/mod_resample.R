#' resample UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_resample_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Resample Settings",
          collapsible = FALSE,
          width = 12,
          height = "620px",
          h3("Input"),
          selectInput(ns("mosaic1"),
                      label = "Mosaic 1",
                      choices = NULL),
          selectInput(ns("mosaic2"),
                      label = "Mosaic 2",
                      choices = NULL),
          hl(),
          h3("Output"),
          textInput(ns("new_resampled"),
                    label = "New object",
                    value = NULL),
          actionBttn(ns("resample"),
                     label = "Resample!",
                     style = "pill",
                     no_outline = FALSE,
                     icon = icon("arrow-up-right-dots"),
                     color = "success")
        )
      ),
      col_9(
        bs4Card(
          title = "Aggregation Results",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          fluidRow(
            col_6(
              h3("Mosaic 1"),
              plotOutput(ns("mosaicoriginal1"), height = "640px") |> add_spinner()

            ),
            col_6(
              h3("Mosaic Resampled"),
              plotOutput(ns("mosaicresampled"), height = "640px") |> add_spinner()
            )
          )
        )
      )
    )
  )
}

#' resample Server Functions
#'
#' @noRd
mod_resample_server <- function(id, mosaic_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic1", choices = setdiff(names(mosaic_data), "mosaic"), selected = NULL)
      updateSelectInput(session, "mosaic2", choices = setdiff(names(mosaic_data), "mosaic"), selected = NULL)
      updateTextInput(session, "new_resampled", value = "mosaic_resampled")

    })

    observeEvent(input$resample, {
      req(input$mosaic1)
      req(input$mosaic2)
      myresampled <- terra::resample(mosaic_data[[input$mosaic1]]$data,
                                     mosaic_data[[input$mosaic2]]$data)
      output$mosaicoriginal1 <- renderPlot({
        terra::plot(mosaic_data[[input$mosaic1]]$data[[1]])
      })
      output$mosaicresampled <- renderPlot({
        terra::plot(myresampled[[1]])
      })

      # Update mosaic_data$mosaic when input$cropmosaic is clicked
      mosaic_data[[input$new_resampled]] <- create_reactval(name = input$new_resampled, data = myresampled)
      sendSweetAlert(
        session = session,
        title = "Mosaic successfully resampled!!",
        text = "The mosaic has been successfully resampled and is now available for further analysis.",
        type = "success"
      )


    })
  })
}

## To be copied in the UI
# mod_resample_ui("resample_1")

## To be copied in the server
# mod_resample_server("resample_1")
