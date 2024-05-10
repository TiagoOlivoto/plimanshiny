#' aggregate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_aggregate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mask Settings",
          collapsible = FALSE,
          width = 12,
          height = "620px",
          h3("Input"),
          selectInput(ns("mosaic_to_aggr"),
                      label = "Mosaic to aggregate",
                      choices = NULL),
          selectInput(ns("aggregatefun"),
                      label = "Resampling function",
                      choices = c('nearest', 'average', 'rms', 'bilinear', 'cubic', 'cubicspline', 'lanczos', 'mode'),
                      selected = "nearest"),
          numericInput(ns("aggregatefct"),
                       label = "Fraction of input raster",
                       value = 50),
          hl(),
          h3("Output"),
          textInput(ns("new_aggr"),
                    label = "New object",
                    value = NULL),
          fluidRow(
            col_6(
              actionBttn(ns("startaggr"),
                         label = "Start Aggregation!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("aggregate"),
                         label = "Aggregate!",
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
          title = "Aggregation Results",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          fluidRow(
            col_6(
              h3("Original mosaic"),
              leafletOutput(ns("mosaicori"), height = "640px") |> add_spinner()

            ),
            col_6(
              h3("Aggregated mosaic"),
              leafletOutput(ns("mosaicaggr"), height = "640px") |> add_spinner()
            )
          )
        )
      )
    )
  )
}

#' aggregate Server Functions
#'
#' @noRd
mod_aggregate_server <- function(id, mosaic_data, r, g, b, basemap){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_aggr", choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")), selected = "Active mosaic")
    })
    observe({
      updateTextInput(session, "new_aggr", value = paste0(input$mosaic_to_aggr, "_aggregated"))
    })
    observeEvent(input$startaggr, {
      output$mosaicori <- renderLeaflet({
        if(input$mosaic_to_aggr == "Active mosaic"){
          bcrop <- basemap$map
        } else{
          bcrop <-
            mosaic_view(
              mosaic_data[[input$mosaic_to_aggr]]$data,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              max_pixels = 500000
            )
        }
        bcrop@map
      })
    })
    observeEvent(input$aggregate, {
      myaggr <- mosaic_aggregate(mosaic_data[[input$mosaic_to_aggr]]$data,
                                 pct = chrv2numv(input$aggregatefct),
                                 fun = input$aggregatefun)
      output$mosaicaggr <- renderLeaflet({
        bcrop2 <-
          mosaic_view(
            myaggr,
            r = suppressWarnings(as.numeric(r$r)),
            g = suppressWarnings(as.numeric(g$g)),
            b = suppressWarnings(as.numeric(b$b))
          )

        bcrop2@map
      })

        # Update mosaic_data$mosaic when input$cropmosaic is clicked
        mosaic_data[[input$new_aggr]] <- create_reactval(name = input$new_aggr, data = myaggr)
        sendSweetAlert(
          session = session,
          title = "Mosaic successfully aggregated!!",
          text = "The mosaic has been successfully aggregated and is now available for further analysis.",
          type = "success"
        )


    })
  })
}

## To be copied in the UI
# mod_aggregate_ui("aggregate_1")

## To be copied in the server
# mod_aggregate_server("aggregate_1")
