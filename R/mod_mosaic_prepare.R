#' mosaic_prepare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils read.csv
mod_mosaic_prepare_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    col_3(
      bs4Card(
        title = "Mosaic input",
        color = "success",
        actionButton(
          inputId = ns("guidemosaic"),
          label = tagList(
            icon = icon("question-circle", verify_fa = FALSE), "Guide"
          ),
          style = "color: white ; background-color: #dd4b39",
          class = "btn-danger"
        ),
        width = 12,
        status = "success",
        footer = "Here, you can configure the mosaic for further analysis. First to import, you can choose the correct order of bands (layers), the maximum number of pixels to be rendered, and the upper and lower quantiles used for color stretching.",
        fluidRow(
          col_2(
            div(class = "prep1",
                dropdownButton(
                  tags$h3("Bands"),
                  selectInput(
                    inputId = ns("r_band"),
                    label = "R",
                    choices = 1:5,
                    selected = 1
                  ),
                  selectInput(
                    inputId = ns("g_band"),
                    label = "G",
                    choices = 1:5,
                    selected = 2,
                  ),
                  selectInput(
                    inputId = ns("b_band"),
                    label = "B",
                    choices = 1:5,
                    selected = 3,
                  ),
                  selectInput(
                    inputId = ns("re_band"),
                    label = "RE",
                    choices = 1:5,
                    selected = 4,
                  ),
                  selectInput(
                    inputId = ns("nir_band"),
                    label = "NIR",
                    choices = 1:5,
                    selected = 5,
                  ),
                  circle = FALSE,
                  status = "primary",
                  icon = icon("gear"),
                  width = "100px",
                  tooltip = tooltipOptions(title = "Click to configure the bands!")
                )
            )
          ),
          col_5(
            div(class = "prep2",
                sliderInput(ns("quantileplot"),
                            label = "Quantiles",
                            min = 0,
                            max = 1,
                            value = c(0, 1))
            )
          ),
          col_5(
            div(class = "prep3",
                numericInput(ns("maxpixels"),
                             label = "Maximum Pixels",
                             value = 1e6)
            )
          )
        ),
        div(class = "prep4",
            fileInput(ns("import_mosaic"),
                      "Browse the mosaic file (.tif, .tiff .jpg)",
                      accept = c('.tif','.tiff','.jpg'))
        ),
        hl(),
        div(class = "prep5",
            awesomeRadio(
              inputId = ns("showmosaic"),
              label = "Show",
              choices = c("rgb", "mapview", "bands"),
              selected = "rgb",
              inline = TRUE
            )
        ),
        hl(),
        div(class = "prep6",
            materialSwitch(
              inputId = ns("mosaiccrop"),
              label = "Crop the mosaic?",
              value = FALSE,
              status = "danger"
            )
        ),
        conditionalPanel(
          condition = "input.mosaiccrop == true", ns = ns,
          actionBttn(ns("cropmosaic"),
                     label = "Crop the mosaic!",
                     status = "danger")
        ),
        div(class = "prep7",
            mod_download_mosaic_ui(ns("downloadmosaic"))
        )

      )
    ),
    col_9(
      bs4Card(
        width = 12,
        height = "780px",
        title = "Mosaic view",
        color = "success",
        status = "success",
        maximizable = TRUE,
        conditionalPanel(
          condition = "input.mosaiccrop != true & input.showmosaic == 'rgb' | input.mosaiccrop != true & input.showmosaic == 'bands'", ns = ns,
          plotOutput(ns("mosaic_plot"), height = "740px") |> add_spinner()
        ),
        conditionalPanel(
          condition = "input.showmosaic == 'mapview' & input.mosaiccrop != true", ns = ns,
          leafletOutput(ns("mosaic_mapview"), height = "740px") |> add_spinner()
        ),
        conditionalPanel(
          condition = "input.mosaiccrop == true", ns = ns,
          fluidRow(
            col_6(h3("Original mosaic"),
                  editModUI(ns("mosaic_crop"), height = "740px") |> add_spinner()),
            col_6(h3("Cropped mosaic"),
                  leafletOutput(ns("mosaiccropped"), height = "740px") |> add_spinner())
          )
        )
      )
    )
  )
}


helpmo <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  poorman::filter(type == "mosaic")
#' mosaic_prepare Server Functions
#'
#' @noRd
mod_mosaic_prepare_server <- function(id, mosaic_data, r, g, b, re, nir, basemap) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$guidemosaic, introjs(session,
                                            options = list("nextLabel"="Next",
                                                           "prevLabel"="Previous",
                                                           "skipLabel"="Skip",
                                                           steps = helpmo),
                                            events = list("oncomplete"=I('alert("Hope it helped!")'))))
    # GUIA

    observe({
      # Update reactiveValues for color bands
      r$r <- input$r_band
      g$g <- input$g_band
      b$b <- input$b_band
      re$re <- input$re_band
      nir$nir <- input$nir_band
    })

    observeEvent(input$import_mosaic, {
      # Update mosaic_data$mosaic when import_mosaic changes
      mosaic_data$mosaic <- mosaic_input(input$import_mosaic$datapath)
      req(mosaic_data$mosaic)  # Ensure mosaic_data$mosaic is not NULL

      mosaic_info(mosaic_data$mosaic, path = input$import_mosaic$datapath)

      # Reactive dependencies on r$r, g$g, and b$b
      reactive_r <- reactive({
        as.numeric(r$r)
      })

      reactive_g <- reactive({
        as.numeric(g$g)
      })

      reactive_b <- reactive({
        as.numeric(b$b)
      })

      observe({
        # Update basemap$map when r$r, g$g, or b$b changes
        bm <- mosaic_view(
          mosaic_data$mosaic,
          r = reactive_r(),
          g = reactive_g(),
          b = reactive_b(),
          quantiles = input$quantileplot,
          max_pixels = input$maxpixels
        )
        basemap$map <- bm
      })
    })

    output$mosaic_plot <- renderPlot({
      req(mosaic_data$mosaic)  # Ensure mosaic_data$mosaic is not NULL
      if (input$showmosaic == "rgb") {
        terra::plotRGB(
          mosaic_data$mosaic,
          r = as.numeric(r$r),
          g = as.numeric(g$g),
          b = as.numeric(b$b)
        )
      }
      if (input$showmosaic == "bands") {
        nl <- terra::nlyr(mosaic_data$mosaic)
        terra::plot(mosaic_data$mosaic, nc = ifelse(nl < 4, 3, ceiling(sqrt(nl))))
      }
    })

    output$mosaic_mapview <- renderLeaflet({
      req(basemap$map)  # Ensure mosaic_data$mosaic is not NULL
      basemap$map@map
    })

    # Observe event for mosaic crop action
    observeEvent(input$mosaiccrop, {
      if (input$mosaiccrop == TRUE) {
        sendSweetAlert(
          session = session,
          title = "Cropping a mosaic",
          text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to crop the mosaic. The mosaic will be cropped to the extend of the created shapes.",
          type = "info"
        )
        # Reactive expression to store the cropped mosaic
        cropped_mosaic <- reactiveVal(NULL)

        # Attempt to get edits
        edits <- callModule(editMod, "mosaic_crop", basemap$map@map, editor = "leafpm")

        observe({
          # Check if edits()$finished is not NULL
          if (!is.null(edits()$finished)) {
            grids <-
              edits()$finished |>
              sf::st_transform(sf::st_crs(mosaic_data$mosaic)) |>
              terra::vect() |>
              terra::ext()
            mosaiccr <- terra::crop(mosaic_data$mosaic, grids)

            # Update the reactiveVal with the cropped mosaic
            cropped_mosaic(mosaiccr)
          }
        })

        output$mosaiccropped <- renderLeaflet({
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
        observeEvent(input$cropmosaic, {
          # Update mosaic_data$mosaic when input$cropmosaic is clicked
          mosaic_data$mosaic <- cropped_mosaic()
          updateCheckboxInput(session, "mosaiccrop", value = FALSE)
          sendSweetAlert(
            session = session,
            title = "Mosaic successfully cropped!!",
            text = "The mosaic has been successfully cropped and is now available for further analysis.",
            type = "success"
          )
        })

      }
    })
    mod_download_mosaic_server("downloadmosaic", mosaic_data$mosaic)
  })
}


## To be copied in the UI
# mod_mosaic_prepare_ui("mosaic_prepare_1")

## To be copied in the server
# mod_mosaic_prepare_server("mosaic_prepare_1")
