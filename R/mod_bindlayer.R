#' bindlayer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bindlayer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Bind Layers Settings",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          h3("Input"),
          pickerInput(ns("layerstobind"),
                      label = "Layers to bind",
                      choices = NULL,
                      multiple = TRUE),
          textInput(ns("savebindto"),
                    label = "Store binded layers as",
                    value = "mosaic_binded"),
          textInput(ns("newmosaicnames"),
                    label = "Optional layer names (comma separated)",
                    value = NA),
          actionBttn(
            ns("bindlayers"),
            label = "Bind!",
            style = "pill",
            color = "success",
            icon = icon("layer-group")
          ),
          hl(),
          mod_download_mosaic_ui(ns("downloadlayersbind"))
        )
      ),
      col_9(
        bs4Card(
          title = "Output",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          plotOutput(ns("plotbind"), height = "640px")
        )
      )
    )
  )
}

#' bindlayer Server Functions
#'
#' @noRd
mod_bindlayer_server <- function(id, mosaic_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updatePickerInput(session, "layerstobind", choices = setdiff(names(mosaic_data), "mosaic"), selected = NULL)
    })

    observeEvent(input$bindlayers, {
      req(input$layerstobind)
      layers <- lapply(input$layerstobind, function(x) {
        mosaic_data[[x]]$data
      })

      tmp <- try(terra::rast(layers))

      if (inherits(tmp, "try-error")) {
        show_alert("Ops, an error occurred.",
                   text = "Raster extents do not match",
                   type = "error")
      } else {
        # Generate updated layer names dynamically
        updated_layer_names <- if (input$newmosaicnames == "") {
          input$layerstobind
        } else {
          str_split(input$newmosaicnames)
        }
        if(nlyr(tmp) != length(updated_layer_names)){
          show_alert("Ops, an error occurred.",
                     text = "Incorrect number of names",
                     type = "error")
        } else{
          names(tmp) <- updated_layer_names

          output$plotbind <- renderPlot({
            req(tmp)
            terra::plot(tmp)
          })

          # Update the layer names in mosaic_data
          for (i in seq_along(input$layerstobind)) {
            mosaic_data[[input$layerstobind[i]]]$name <- updated_layer_names[i]
          }
          mosaic_data[[input$savebindto]] <- create_reactval(name = input$savebindto, data = tmp)
        }

      }
    })

    mod_download_mosaic_server("downloadmosaic", mosaic_data[[input$savebindto]]$data)

  })
}

## To be copied in the UI
# mod_bindlayer_ui("bindlayer_1")

## To be copied in the server
# mod_bindlayer_server("bindlayer_1")
