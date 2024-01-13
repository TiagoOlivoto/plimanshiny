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
          footer = "To start masking a mosaic, click on 'Start!' and use the
          'Draw Polygon, or 'Draw Rectangle' tools. To crop the mosaic,
          click on 'Crop'",
          hl(),
          h3("Input"),
          selectInput(ns("mosaic_to_mask"),
                      label = "Mosaic to be masked",
                      choices = NULL),
          prettyCheckbox(
            inputId = ns("shapemanipula"),
            label = "Crop using a shapefile?",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          conditionalPanel("input.shapemanipula == true",  ns = ns,
                           selectInput(ns("shape_to_crop"),
                                       label = "Shapefile",
                                       choices = NULL),
          ),
          prettyCheckbox(
            inputId = ns("invertmask"),
            label = "Invert the mask?",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          hl(),
          h3("Output"),
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
            col_6(
              conditionalPanel("input.shapemanipula != true",  ns = ns,
                               h3("Original mosaic"),
                               editModUI(ns("mosaic_mask"), height = "640px") |> add_spinner()
              ),
              conditionalPanel("input.shapemanipula == true",  ns = ns,
                               h3("Original mosaic and shapefile"),
                               leafletOutput(ns("mosaic_maskshp"), height = "640px") |> add_spinner()
              ),

            ),
            col_6(
              h3("Cropped mosaic"),
              leafletOutput(ns("mosaicmasked"), height = "640px") |> add_spinner()
            )
          )
        )
      )
    )
  )
}

#' mask Server Functions
#'
#' @noRd
mod_mask_server <- function(id, mosaic_data, shapefile, r, g, b){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_mask", choices = setdiff(names(mosaic_data), "mosaic"), selected = input$mosaic_to_mask)
      updateSelectInput(session, "shape_to_crop", choices = setdiff(names(shapefile), "shapefile"))
      updateTextInput(session, "new_mask", value = paste0(input$mosaic_to_mask, "_masked"))
    })


    # Observe event for mosaic crop action
    observeEvent(input$startmask, {
      # Reactive expression to store the cropped mosaic
      cropped_mosaic <- reactiveVal(NULL)
      if(input$shapemanipula){

          shptocrop <- shapefile[[input$shape_to_crop]]$data
          output$mosaic_maskshp <- renderLeaflet({
            bcrop <-
              mosaic_view(
                mosaic_data[[input$mosaic_to_mask]]$data,
                r = as.numeric(r$r),
                g = as.numeric(g$g),
                b = as.numeric(b$b),
                max_pixels = 500000
              )

            (bcrop + shapefile_view(shptocrop))@map
          })

          mosaiccr <- terra::mask(mosaic_data$mosaic, shptocrop, inverse = input$invertmask)
          cropped_mosaic(mosaiccr)
      } else{
        sendSweetAlert(
          session = session,
          title = "Masking a mosaic",
          text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to create the mask. The mosaic will be cropped to the extend of the created shapes.",
          type = "info"
        )
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
            mosaiccr <- terra::mask(mosaic_data$mosaic, grids, inverse = input$invertmask)

            # Update the reactiveVal with the cropped mosaic
            cropped_mosaic(mosaiccr)
          }
        })
      }

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
        mosaic_data[[input$new_mask]] <- create_reactval(name = input$new_mask, data = cropped_mosaic())
        sendSweetAlert(
          session = session,
          title = "Mosaic successfully masked!!",
          text = "The mosaic has been successfully masked and is now available for further analysis.",
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
