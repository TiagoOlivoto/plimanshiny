#' interpolate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_interpolate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Interpolation Settings",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          h3("Input"),
          pickerInput(ns("mosaictointerpolate"),
                      label = "Mosaic to interpolate",
                      choices = NULL),
          pickerInput(ns("interpolatemethod"),
                      label = "Method",
                      choices = c("bilinear", "loess", "idw"),
                      selected = "idw"),
          textInput(ns("saveinterpolateto"),
                    label = "Store interpolated mosaic as",
                    value = "mosaic_interpolated"),
          fluidRow(
            col_6(
              actionBttn(ns("startinterpolating"),
                         label = "Start interpolating!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("interpolatemosaic"),
                         label = "Interpolate!",
                         style = "pill",
                         no_outline = FALSE,
                         icon = icon("scissors"),
                         color = "success")
            )
          ),
          prettyCheckbox(
            inputId = ns("interpolationdone"),
            label = "Interpolation finished!",
            value = FALSE,
            status = "info",
            icon = icon("thumbs-up"),
            plain = TRUE,
            outline = TRUE,
            animation = "rotate"
          ),
          hl(),
          mod_download_mosaic_ui(ns("downloadinterpolated"), button_label = "Download")
        )
      ),
      col_9(
        bs4Card(
          title = "Output",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          fluidRow(
            col_7(
              editModUI(ns("plotinterpol"), height = "660px") |> add_spinner()
            ),
            col_5(
              leafletOutput(ns("plotinterpolated"), height = "660px") |> add_spinner()
            )
          )
        )
      )
    )
  )
}

#' interpolate Server Functions
#'
#' @noRd
mod_interpolate_server <- function(id, mosaic_data, r, g, b, basemap){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data$mosaic)
      updatePickerInput(session, "mosaictointerpolate", choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")), selected = "Active mosaic")
    })
    observeEvent(input$startinterpolating, {
      req(mosaic_data)
      req(input$mosaictointerpolate)
      if(input$mosaictointerpolate == "Active mosaic"){
        bmap <- basemap$map
      } else{
        bmap <- mosaic_view(mosaic_data[[input$mosaictointerpolate]]$data,
                            r = suppressWarnings(as.numeric(r$r)),
                            g = suppressWarnings(as.numeric(g$g)),
                            b = suppressWarnings(as.numeric(b$b)),
                            max_pixels = 500000)
      }
      req(bmap)
      cpoints <- callModule(editMod, "plotinterpol", bmap@map , editor = "leafpm")

      observeEvent(input$interpolatemosaic ,{

        observeEvent(cpoints()$finished, {
          if(!is.null(cpoints()$edited)){
            cpo <- cpoints()$edited |> sf::st_transform(sf::st_crs(mosaic_data[[input$mosaictointerpolate]]$data))
          } else{
            cpo <- cpoints()$finished |> sf::st_transform(sf::st_crs(mosaic_data[[input$mosaictointerpolate]]$data))
          }
          # Check if edits()$finished is not NULL and shapedone is FALSE
          if (!is.null(cpoints()$finished)) {
            moint <- mosaic_interpolate(mosaic_data[[input$mosaictointerpolate]]$data,
                                        points = cpo,
                                        method = input$interpolatemethod)
            output$plotinterpolated <- renderLeaflet({
              mosaic_view(moint)@map
            })
            observeEvent(input$interpolationdone, {

              mosaic_data[[input$saveinterpolateto]] <- create_reactval(name = input$saveinterpolateto, data = moint)

            })
          }
        })
      })
    })
  })
}

## To be copied in the UI
# mod_interpolate_ui("interpolate_1")

## To be copied in the server
# mod_interpolate_server("interpolate_1")
