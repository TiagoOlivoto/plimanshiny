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
          col_4(
            actionButton(
              inputId = ns("guidemosaic"),
              label = tagList(
                icon = icon("question-circle", verify_fa = FALSE), "Guide"
              ),
              style = "color: white ; background-color: #dd4b39",
              class = "btn-danger"
            )
          ),
          col_3(
            div(class = "prep1",
                dropdown(
                  tags$h3("Settings"),
                  fluidRow(
                    col_3(
                      selectInput(
                        inputId = ns("r_band"),
                        label = "R",
                        choices = 1:5,
                        selected = 1
                      )
                    ),
                    col_3(
                      selectInput(
                        inputId = ns("g_band"),
                        label = "G",
                        choices = 1:5,
                        selected = 2,
                      )
                    ),
                    col_3(
                      selectInput(
                        inputId = ns("b_band"),
                        label = "B",
                        choices = 1:5,
                        selected = 3,
                      )
                    ),
                    col_3(
                      selectInput(
                        inputId = ns("re_band"),
                        label = "RE",
                        choices = NA
                      )
                    )
                  ),
                  fluidRow(
                    col_4(
                      selectInput(
                        inputId = ns("nir_band"),
                        label = "NIR",
                        choices = NA
                      )
                    ),
                    col_4(
                      selectInput(
                        inputId = ns("swir_band"),
                        label = "SWIR",
                        choices = NA
                      )
                    ),
                    col_4(
                      selectInput(
                        inputId = ns("tir_band"),
                        label = "TIR",
                        choices = NA
                      )
                    )
                  ),
                  div(class = "prep2",
                      sliderInput(ns("quantileplot"),
                                  label = "Quantiles",
                                  min = 0,
                                  max = 1,
                                  value = c(0, 1),
                                  step = 0.001)
                  ),
                  div(class = "prep3",
                      numericInput(ns("maxpixels"),
                                   label = "Maximum Pixels",
                                   value = 1e6)
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
                  width = "420px",
                  icon = icon("gear"),
                  animate = animateOptions(enter = "fadeInLeft", exit = "fadeOutRight", duration = 1),
                  tooltip = tooltipOptions(title = "Configure the bands")
                )
            )
          ),
          col_5(
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
        br(),
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
        fluidRow(
          col_6(
            awesomeRadio(
              inputId = ns("showmosaic"),
              label = "Show",
              choices = c("rgb", "bands", "hist"),
              selected = "rgb",
              inline = FALSE
            )
          ),
          col_6(
            selectInput(
              ns("stretch"),
              label = "Stretch",
              choices = c("none", "lin", "hist")
            )
          )
        ),
        prettyCheckbox(
          inputId = ns("intmap"),
          label = "Create an interative base map?",
          value = TRUE,
          icon = icon("check"),
          status = "success",
          animation = "rotate"
        ),
        conditionalPanel(
          condition = "input.intmap == true", ns = ns,
          selectInput(ns("howtoplot"),
                      label = "How to plot",
                      choices = NULL)
        ),
        conditionalPanel(
          condition = "input.intmap == false", ns = ns,
          sliderInput(
            ns("gammacorr"),
            label = "Gamma correction",
            min = -5,
            max = 5,
            value = 1,
            step = 0.1
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
          condition = "(input.showmosaic == 'rgb' | input.showmosaic == 'bands' | input.showmosaic == 'hist') & input.intmap == false", ns = ns,
          plotOutput(ns("mosaic_plot"), height = "740px") |> add_spinner()
        ),
        conditionalPanel(
          condition = "input.intmap == true", ns = ns,
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
mod_mosaic_prepare_server <- function(id, mosaic_data, r, g, b, re, nir, swir, tir, basemap, pathmosaic, quantiles, maxpixel, activemosaic) {
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
      swir$swir <- input$swir_band
      tir$tir <- input$tir_band
      quantiles$q <- input$quantileplot
      maxpixel$mp <- input$maxpixels
    })

    input_file_selected <- reactiveValues(paths = NULL)
    observe({
      shinyFileChoose(input, "filemosaic",
                      root = getVolumes()(),
                      filetypes = c('tif', 'jp2', 'tiff', 'jpeg', "dat"),
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
                  mosaic_data[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], terra::rast(input_file_selected$paths$datapath[[i]]))
                }
              } else {
                return()
              }
            }
          })
        } else {
          # If it doesn't exist, create a new reactiveValues and add it to mosaic_data
          for (i in 1:length(new_mosaic_name)) {
            mosaic_data[[new_mosaic_name[[i]]]] <- create_reactval(new_mosaic_name[[i]], terra::rast(input_file_selected$paths$datapath[[i]]))
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
      req(input$mosaictoanalyze)
      activemosaic$name <- input$mosaictoanalyze
      nl <- terra::nlyr(mosaic_data[[input$mosaictoanalyze]]$data)
      updateSelectInput(session, "r_band",
                        choices = 1:nl,
                        selected = "1")
      updateSelectInput(session, "g_band",
                        choices = 1:nl,
                        selected = "2")
      updateSelectInput(session, "b_band",
                        choices = 1:nl,
                        selected = "3")
      updateSelectInput(session, "re_band",
                        choices = paste0(c("NA", paste(1:nl))))
      updateSelectInput(session, "nir_band",
                        choices = paste0(c("NA", paste(1:nl))))
      updateSelectInput(session, "swir_band",
                        choices = paste0(c("NA", paste(1:nl))))
      updateSelectInput(session, "tir_band",
                        choices = paste0(c("NA", paste(1:nl))))
    })

    observe({
      # Check if a mosaic is selected
      req(input$mosaictoanalyze)
      updateSelectInput(session, "howtoplot",
                        choices = c("RGB", names(mosaic_data[[input$mosaictoanalyze]]$data)))
    })
    observe({
      req(input$mosaictoanalyze)
      crsmo <- terra::crs(mosaic_data[[input$mosaictoanalyze]]$data) != ""
      if(crsmo && terra::is.lonlat(mosaic_data[[input$mosaictoanalyze]]$data)){
        eps <- mosaic_epsg(mosaic_data[[input$mosaictoanalyze]]$data)
        text <- paste0("The current raster is in the lat/lon coordinate system, which may result in processing errors when trying to segment individuals in the `mosaic_analyze()` function. It is highly suggested to reproject the raster using mosaic_project() with ", eps)
        show_alert("CRS is on Lon/Lat format.",
                   text = text,
                   type = "warning")
      }
    })
    observeEvent(input$mosaicinfomosaic, {
      req(mosaic_data[[input$mosaictoanalyze]]$data)
      mosaic_info(mosaic_data[[input$mosaictoanalyze]]$data)
    })

    output$mosaic_plot <- renderPlot({
      req(input$mosaictoanalyze)
      if (input$showmosaic == "rgb") {
        if(nlyr(mosaic_data[[input$mosaictoanalyze]]$data) < 3){
          show_alert("Ops, too few bands",
                     text = "The current mosaic has too few bands and an RGB image cannot be rendered. Plotting a raster image",
                     type = "warning")
          terra::plot(mosaic_data[[input$mosaictoanalyze]]$data)
        } else{
          if(input$stretch == "none"){
            terra::plotRGB(
              mosaic_data[[input$mosaictoanalyze]]$data ^input$gammacorr,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              maxcell = 1e6
            )

          } else{
            terra::plotRGB(
              mosaic_data[[input$mosaictoanalyze]]$data ^ input$gammacorr,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              stretch = input$stretch,
              maxcell = 1e6
            )
          }
        }
      }
      if (input$showmosaic == "bands") {
        terra::plot(mosaic_data[[input$mosaictoanalyze]]$data)
      }
      if (input$showmosaic == "hist") {
        terra::hist(mosaic_data[[input$mosaictoanalyze]]$data)
      }
    })

    #
    observe({
      if(input$intmap){
        req(input$mosaictoanalyze)
        req(input$howtoplot)
        if(input$howtoplot == "RGB"){
          if(nlyr(mosaic_data[[input$mosaictoanalyze]]$data) >= 3){
            bmtmp <- mosaic_view(
              mosaic_data[[input$mosaictoanalyze]]$data,
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              quantiles = quantiles$q,
              max_pixels = maxpixel$mp
            )
          } else{
            show_alert("Ops, too few bands",
                       text = "The current mosaic has too few bands and an RGB image cannot be rendered. Plotting the first layer of the raster image. Change `Show` to `bands` to choose which band to plot.",
                       type = "warning")
            bmtmp <- mosaic_view(
              mosaic_data[[input$mosaictoanalyze]]$data[[1]],
              color_regions  = scales::brewer_pal(palette = "RdYlGn")(8),
              max_pixels = maxpixel$mp,
              na.color = "transparent"
            )
          }

        } else{
          bmtmp <-
            mosaic_view(mosaic_data[[input$mosaictoanalyze]]$data[input$howtoplot],
                        show = "index",
                        color_regions  = scales::brewer_pal(palette = "RdYlGn")(8),
                        max_pixels = maxpixel$mp,
                        na.color = "transparent")
        }

        basemap$map <- bmtmp
        output$mosaic_mapview <- renderLeaflet({
          req(bmtmp)
          bmtmp@map
        })
      }
    })

    mod_download_mosaic_server("downloadmosaic", mosaic_data[[input$mosaictoanalyze]]$data)
  })
}


## To be copied in the UI
# mod_mosaic_prepare_ui("mosaic_prepare_1")

## To be copied in the server
# mod_mosaic_prepare_server("mosaic_prepare_1")
