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
        fluidRow(
          col_6(
            actionButton(
              inputId = ns("guidemosaic"),
              label = tagList(
                icon = icon("question-circle", verify_fa = FALSE), "Guide"
              ),
              style = "color: white ; background-color: #dd4b39",
              class = "btn-danger"
            )
          ),
          col_6(
            actionButton(
              inputId = ns("mosaicinfomosaic"),
              label = tagList(
                icon = icon("circle-info", verify_fa = FALSE), "Mosaic Info"
              ),
              status = "info"
            )
          )
        ),
        width = 12,
        status = "success",
        footer = "Here, you can configure the mosaic for further analysis. First to import, you can choose the correct order of bands (layers), the maximum number of pixels to be rendered, and the upper and lower quantiles used for color stretching.",
        br(),
        fluidRow(
          col_2(
            div(class = "prep1",
                dropdown(
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
                  actionBttn(
                    ns("donebands"),
                    label = "Done",
                    no_outline = FALSE,
                    icon = icon("check"),
                    color = "success"
                  ),
                  circle = FALSE,
                  status = "success",
                  style = "unite",
                  width = "170px",
                  icon = icon("layer-group"),
                  animate = animateOptions(enter = "fadeInLeft", exit = "fadeOutRight", duration = 1),
                  tooltip = tooltipOptions(title = "Configure the bands")
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
            shinyFilesButton(id=ns("filemosaic"),
                             label="Raster file(s)",
                             title="Raster file(s)",
                             buttonType = "primary",
                             multiple = TRUE,
                             class = NULL,
                             icon = icon("magnifying-glass"),
                             style = NULL),
            fluidRow(
              textInput(
                ns("filemosaicpath"),
                label = "Choosen file(s)",
                value = "",
                width = "100%"
              )
            ),
            conditionalPanel(
              condition = "input.filemosaicpath != ''", ns = ns,
              fluidRow(
                actionBttn(ns("importmosaic"),
                           label = "Import the choosen file(s)",
                           no_outline = FALSE,
                           icon = icon("file-import"),
                           style = "material-flat",
                           color = "primary")
              )
            )
        ),
        br(),
        div(class = "prep5",
            selectInput(ns("mosaictoanalyze"),
                        label = "Active Mosaic",
                        choices = NULL)
        ),
        hl(),
        div(class = "prep6",
            awesomeRadio(
              inputId = ns("showmosaic"),
              label = "Show",
              choices = c("rgb", "mapview", "bands"),
              selected = "rgb",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.showmosaic == 'mapview'", ns = ns,
              selectInput(ns("howtoplot"),
                          label = "How to plot",
                          choices = NULL)
            )
        ),
        hl(),
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
          condition = "input.showmosaic == 'rgb' | input.showmosaic == 'bands'", ns = ns,
          plotOutput(ns("mosaic_plot"), height = "740px") |> add_spinner()
        ),
        conditionalPanel(
          condition = "input.showmosaic == 'mapview'", ns = ns,
          leafletOutput(ns("mosaic_mapview"), height = "740px") |> add_spinner()
        )
      )
    )
  )
}


helpmo <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "mosaic")
#' mosaic_prepare Server Functions
#'
#' @noRd
mod_mosaic_prepare_server <- function(id, mosaic_data, r, g, b, re, nir, basemap, pathmosaic) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$guidemosaic, introjs(session,
                                            options = list("nextLabel"="Next",
                                                           "prevLabel"="Previous",
                                                           "skipLabel"="Skip",
                                                           steps = helpmo),
                                            events = list("oncomplete"=I('alert("Hope it helped!")'))))
    # GUIA

    observeEvent(input$donebands, {
      # Update reactiveValues for color bands
      r$r <- input$r_band
      g$g <- input$g_band
      b$b <- input$b_band
      re$re <- input$re_band
      nir$nir <- input$nir_band
    })

    input_file_selected <- reactiveValues(paths = NULL)
    observe({
      shinyFileChoose(input, "filemosaic",
                      root = getVolumes()(),
                      filetypes = c('tif', 'jp2', 'tiff', 'jpeg'),
                      session = session)
      if(!is.null(input$filemosaic)){
        input_file_selected$paths <- parseFilePaths(getVolumes()(), input$filemosaic)
        if(length(input_file_selected$paths$datapath) != 0){
          updateTextInput(session, "filemosaicpath", value = paste0(input_file_selected$paths$datapath, collapse = ", "))
        }

      }
    })

    observeEvent(input$importmosaic, {
      if(length(input_file_selected$paths$datapath) != 0){
        new_mosaic_name <- sapply(input_file_selected$paths$datapath, file_name)
        pathmosaic$path <- input_file_selected$paths$datapath
        # Check if the mosaic already exists in mosaic_data
        if (any(new_mosaic_name %in% names(mosaic_data))) {
          # If it exists, update the existing reactiveValues
          moname <- new_mosaic_name[new_mosaic_name %in% names(mosaic_data)]
          ask_confirmation(
            inputId = "confirmmosaicname",
            type = "warning",
            title = "Mosaic already imported",
            text = paste0("The object '", paste0(moname, collapse = ", "), "' is already available in the list of imported mosaics. Do you really want to overwrite it?"),
            btn_labels = c("Nope", "Yep"),
            btn_colors = c("#FE642E", "#04B404")
          )
          observe({
            if (!is.null(input$confirmmosaicname)) {
              if (input$confirmmosaicname) {
                for (i in 1:length(new_mosaic_name)) {
                  mosaic_data[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], mosaic_input(input_file_selected$paths$datapath[[i]], info = FALSE))
                }
              } else {
                return()
              }
            }
          })
        } else {
          # If it doesn't exist, create a new reactiveValues and add it to mosaic_data
          for (i in 1:length(new_mosaic_name)) {
            mosaic_data[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], mosaic_input(input_file_selected$paths$datapath[[i]], info = FALSE))
          }
        }

        observe({
          mosaicnames <-  setdiff(names(mosaic_data), "mosaic")
          # Update selectInput choices
          updateSelectInput(session, "mosaictoanalyze",
                            choices = mosaicnames,
                            selected = mosaicnames[[1]])
        })
      }
    })


    observe({
      # Check if a mosaic is selected
      req(input$mosaictoanalyze)
      selected_mosaic <- mosaic_data[[input$mosaictoanalyze]]
      # # Check if the selected_mosaic is not NULL and has the 'data' field
      if ('data' %in% names(selected_mosaic)) {
        mosaic_data$mosaic <- selected_mosaic$data
        mosaic_info(selected_mosaic$data)
      }
      updateSelectInput(session, "howtoplot",
                        choices = c("RGB", names(selected_mosaic$data)))
    })

    observeEvent(input$mosaicinfomosaic, {
      req(mosaic_data$mosaic)
      mosaic_info(mosaic_data$mosaic)
    })

    output$mosaic_plot <- renderPlot({
      req(mosaic_data$mosaic)  # Ensure mosaic_data$mosaic is not NULL
      if (input$showmosaic == "rgb") {
        if(nlyr(mosaic_data$mosaic) < 3){
          show_alert("Ops, too few bands",
                     text = "The current mosaic has too few bands and an RGB image cannot be rendered. Plotting a raster image",
                     type = "warning")
          terra::plot(mosaic_data$mosaic)
        } else{
          terra::plotRGB(
            mosaic_data$mosaic,
            r = as.numeric(r$r),
            g = as.numeric(g$g),
            b = as.numeric(b$b),
            stretch = "lin"
          )
        }

      }
      if (input$showmosaic == "bands") {
        nl <- terra::nlyr(mosaic_data$mosaic)
        terra::plot(mosaic_data$mosaic, nc = ifelse(nl < 4, 3, ceiling(sqrt(nl))))
      }
    })

    #
    observe({
      req(mosaic_data$mosaic)
      req(input$howtoplot)
      if(input$howtoplot == "RGB"){
        bmtmp <- mosaic_view(
          mosaic_data$mosaic,
          r = as.numeric(r$r),
          g = as.numeric(g$g),
          b = as.numeric(b$b),
          quantiles = input$quantileplot,
          max_pixels = input$maxpixels
        )
      } else{
        # mosaictmp <- mosaic_data[[input$mosaictoanalyze]]
        # aggr <- find_aggrfact(mosaictmp)
        # if(aggr > 0){
        #   magg <- mosaic_aggregate(mosaictmp, round(100 / aggr))
        # } else{
        #   magg <- mosaictmp
        # }
        # print(magg)
        bmtmp <-
          mosaic_view(mosaic_data$mosaic[input$howtoplot],
                      show = "index",
                      color_regions  = scales::brewer_pal(palette = "RdYlGn")(8),
                      max_pixels = input$maxpixels,
                      na.color = "transparent")
      }

        basemap$map <- bmtmp
    })



    output$mosaic_mapview <- renderLeaflet({
      req(basemap$map)  # Ensure mosaic_data$mosaic is not NULL
      basemap$map@map
    })

    mod_download_mosaic_server("downloadmosaic", mosaic_data$mosaic)
  })
}


## To be copied in the UI
# mod_mosaic_prepare_ui("mosaic_prepare_1")

## To be copied in the server
# mod_mosaic_prepare_server("mosaic_prepare_1")
