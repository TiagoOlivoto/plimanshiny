#' mask UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mask_ui <-  function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mask Settings",
          collapsible = FALSE,
          width = 12,
          height = "620px",
          footer = "To start cropping a mosaic, click on 'Start!' and use the
          'Draw Polygon, or 'Draw Rectangle' tools. To crop the mosaic,
          click on 'Crop'",
          selectInput(ns("mosaic_to_mask"),
                      label = "Mosaic to be masked",
                      choices = NULL),
          textInput(ns("new_mask"),
                    label = "New object",
                    value = NULL),

          fluidRow(
            col_6(
              actionBttn(ns("startmask"),
                         label = "Start masking!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("maskmosaic"),
                         label = "mask!",
                         style = "pill",
                         no_outline = FALSE,
                         icon = icon("scissors"),
                         color = "success")
            )
          )
        )
      ),
      col_9(
        bs4Card(
          title = "Crop Results",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          fluidRow(
            col_6(h3("Original mosaic"),
                  editModUI(ns("mosaic_mask"), height = "640px") |> add_spinner()),
            col_6(h3("Cropped mosaic"),
                  leafletOutput(ns("mosaicmasked"), height = "640px") |> add_spinner())
          )
        )
      )
    )
  )
}

#' mask Server Functions
#'
#' @noRd
mod_mask_server <- function(id, mosaic_data, r, g, b){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_mask", choices = setdiff(names(mosaic_data), "mosaic"))
      updateTextInput(session, "new_mask", value = paste0(input$mosaic_to_mask, "_masked"))
    })


    # Observe event for mosaic crop action
    observeEvent(input$startmask, {
      sendSweetAlert(
        session = session,
        title = "Cropping a mosaic",
        text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to crop the mosaic. The mosaic will be cropped to the extend of the created shapes.",
        type = "info"
      )
      # Reactive expression to store the cropped mosaic
      cropped_mosaic <- reactiveVal(NULL)
      mapcrop <-
        mosaic_view(
          mosaic_data[[input$mosaic_to_mask]]$data,
          r = as.numeric(r$r),
          g = as.numeric(g$g),
          b = as.numeric(b$b),
          max_pixels = 300000
        )@map
      # # Attempt to get edits
      edits <- callModule(editMod, "mosaic_mask", mapcrop, editor = "leafpm")
      #
      observe({
        # Check if edits()$finished is not NULL
        if (!is.null(edits()$finished)) {
          grids <-
            edits()$finished |>
            sf::st_transform(sf::st_crs(mosaic_data$mosaic))
          mosaiccr <- terra::crop(mosaic_data$mosaic, grids, mask = TRUE)

          # Update the reactiveVal with the cropped mosaic
          cropped_mosaic(mosaiccr)
        }
      })

      output$mosaicmasked <- renderLeaflet({
        req(cropped_mosaic())  # Ensure cropped_mosaic is not NULL
        # Print the updated mosaic_data$mosaic
        # Render the cropped mosaic
        bcrop <-
          mosaic_view(
            cropped_mosaic(),
            r = as.numeric(r$r),
            g = as.numeric(g$g),
            b = as.numeric(b$b)
          )
        bcrop@map
      })

      # Observe event for mosaic crop action
      observeEvent(input$maskmosaic, {
        # Update mosaic_data$mosaic when input$cropmosaic is clicked
        mosaic_data[[input$new_cropped]] <- create_reactval(name = input$new_cropped, data = cropped_mosaic())
        sendSweetAlert(
          session = session,
          title = "Mosaic successfully cropped!!",
          text = "The mosaic has been successfully cropped and is now available for further analysis.",
          type = "success"
        )
      })


    })
  })
}

## To be copied in the UI
# mod_mask_ui("mask_1")

## To be copied in the server
# mod_mask_server("mask_1")
