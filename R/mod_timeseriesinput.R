#' timeseriesinput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_timeseriesinput_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mosaic input",
          color = "success",
          fluidRow(
            col_3(
              col_4(
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
              )
            ),
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
          col_8(
            numericInput(ns("maxpixels"),
                         label = "Maximum Pixels",
                         value = 1e6)
          ),
          hl(),
          h3("Raster files"),
          fluidRow(
            col_6(
              shinyFilesButton(id=ns("filemosaic"),
                               label="Raster files",
                               title="Raster file(s)",
                               buttonType = "primary",
                               multiple = TRUE,
                               class = NULL,
                               icon = icon("magnifying-glass"),
                               style = NULL)
            ),
            col_6(
              actionButton(
                inputId = ns("importrasters"),
                label = "Import",
                icon = icon("share-from-square"),
                status = "success",
                gradient = TRUE,
                width = "130px"
              )
            )
          ),
          selectizeInput(
            inputId = ns("mosaics"),
            label = "Selected raster files",
            choices = NULL,
            multiple=TRUE,
            options = list('plugins' = list('remove_button'),
                           'create' = TRUE,
                           'persist'= FALSE)
          ),
          fluidRow(
            col_6(
              awesomeRadio(
                inputId = ns("showmosaic"),
                label = "Show",
                choices = c("rgb", "bands"),
                selected = "rgb",
                inline = TRUE
              )
            ),
            col_6(
              conditionalPanel(
                condition = "input.showmosaic == 'bands'", ns = ns,
                selectInput(
                  ns("bandnumber"),
                  label = "Band number",
                  choices = NULL
                )
              )
            )
          ),
          hl(),
          h3("Shapefile"),
          fluidRow(
            col_6(
              shinyFilesButton(id=ns("shapefileinput"),
                               label="Shapefile",
                               title="Shapefile",
                               buttonType = "primary",
                               multiple = TRUE,
                               class = NULL,
                               icon = icon("magnifying-glass"),
                               style = NULL)
            ),
            col_6(
              actionButton(
                inputId = ns("importshapefile"),
                label = "Import",
                icon = icon("share-from-square"),
                status = "success",
                gradient = TRUE,
                width = "130px"
              )
            )
          ),
          selectizeInput(
            inputId = ns("shapefileimported"),
            label = "Selected shapefile",
            choices = NULL,
            multiple=FALSE,
            options = list('plugins' = list('remove_button'),
                           'create' = TRUE,
                           'persist'= FALSE)
          ),
          prettyCheckbox(
            inputId = ns("croptoshape"),
            label = "Crop rasters to shapefile extend",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          prettyCheckbox(
            inputId = ns("plotshape"),
            label = "Plot the shapefile",
            value = TRUE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          )
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "780px",
          status = "success",
          title = "Time series",
          selected = "Time series",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Time series",
            plotOutput(ns("plotmosaic"), height = "650px") |> add_spinner(),
            sliderTextInput(
              inputId = ns("mosaicslider"),
              label = "Time series:",
              grid = TRUE,
              force_edges = TRUE,
              choices = NA
            )
          ),
          tabPanel(
            title = "Interactive visualization",
            leafletOutput(ns("leafletmap"), height = "720px")  |> add_spinner()
          )
        )
      )
    )
  )
}

#' timeseriesinput Server Functions
#'
#' @noRd
mod_timeseriesinput_server <- function(id, shapefile, mosaiclist, r, g, b, re, nir, basemap){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$donebands, {
      # Update reactiveValues for color bands
      r$r <- input$r_band
      g$g <- input$g_band
      b$b <- input$b_band
      re$re <- input$re_band
      nir$nir <- input$nir_band
    })

    input_file_selected <- reactiveValues(paths = NULL)
    mosaic_list <- reactiveValues(files = NULL)
    filedir <- reactiveValues(dir = NULL)
    datatimes <- reactiveValues(dates = NULL)
    observe({
      shinyFileChoose(input, "filemosaic",
                      root = getVolumes()(),
                      filetypes = c('tif', 'jp2', 'tiff', 'jpeg'),
                      session = session)
      if(!is.null(input$filemosaic)){
        input_file_selected$paths <- parseFilePaths(getVolumes()(), input$filemosaic)
        if(length(input_file_selected$paths$datapath) != 0){
          filenames <- file_name(input_file_selected$paths$datapath)
          filedir$dir <- file_dir(input_file_selected$paths$datapath)

          dates <- gsub(".*([0-9]{2}-[0-9]{2}-[0-9]{4}).*", "\\1", filenames)
          dates <- try(as.Date(dates, tryFormats = c("%d-%m-%Y", "%m-%d-%Y")))
          if(inherits(dates, "try-error")){
            sendSweetAlert(
              session = session,
              title = "Ops, invalid file name",
              text = "It was not possible to extract the data information for all files. Please, ensure that all the rasterfiles have a data pattern 'mm-dd-yyyy' in the name.",
              type = "error"
            )
          } else{
            # Get indices of dates sorted in chronological order
            chronological_order <- order(dates)
            datatimes$dates <- dates[chronological_order]
            mosaic_list$files <- paste0(filedir$dir, "/", filenames[chronological_order], ".", file_extension(input_file_selected$paths$datapath))
            updateSelectizeInput(session, "mosaics",
                                 choices = filenames[chronological_order],
                                 selected = filenames[chronological_order])
          }
        }
      }
    })
    observe({
      req(datatimes$dates)
      updateSliderTextInput(session, "mosaicslider", choices = datatimes$dates, selected = datatimes$dates[1])
    })
    # shapefile
    pathshape <- reactiveValues(file = NULL)
    observe({
      shinyFileChoose(input, "shapefileinput",
                      root = getVolumes()(),
                      filetypes = c("rds",  "shp",  "json", "kml",  "gml",  "dbf",  "sbn",  "sbx",  "shx",  "prj", "cpg"),
                      session = session)
      if(!is.null(input$shapefileinput)){
        pathshape$file <- parseFilePaths(getVolumes()(), input$shapefileinput)
        if(length(pathshape$file$datapath) != 0){
          updateSelectizeInput(session, "shapefileimported",
                               choices = file_name(pathshape$file$datapath),
                               selected = file_name(pathshape$file$datapath))
        }
      }
    })

    #import the mosaics
    observeEvent(input$importrasters, {
      req(mosaic_list$files)
      mosaic_list$files <- lapply(mosaic_list$files, terra::rast)
      names(mosaic_list$files) <- datatimes$dates
      updateSelectInput(session, "bandnumber", choices = 1:terra::nlyr(mosaic_list$files[[1]]))
      waiter_show(
        html = tagList(
          spin_google(),
          h2("Validating and configuring the raster files. Please, wait.")
        ),
        color = "#228B227F"
      )
    })

    observe({
      if(!is.null(basemap$map)){
        waiter_hide()
        # check for correct layer configuration
        if(inherits(mosaiclist$mosaics$data[[1]], "SpatRaster")){
          r <- as.numeric(r$r)
          g <- as.numeric(g$g)
          b <- as.numeric(b$b)
          re <- as.numeric(re$re)
          nir <- as.numeric(nir$nir)
          nlr <- terra::nlyr(mosaiclist$mosaics$data[[1]])
          if(any(c(r, g, b, re, nir) > nlr)){
            sendSweetAlert(
              session = session,
              title = "Ops, invalid layer configuration",
              text = glue::glue("Use the 'Configure the bands' button to correctly configure the layers. The rasters have {nlr} layers, but some of the declared bands (R,G,B,RE,NIR) is assumed to be a value greater than that."),
              type = "error"
            )
          }
        }

      }
    })


    #import the shapefile
    observeEvent(input$importshapefile, {
      req(pathshape$file$datapath[1])
      shapefile[["shape"]] <- create_reactval("shape", shapefile_input(pathshape$file$datapath[1], info = FALSE))
    })

    observe({
      if (input$croptoshape) {
        req(shapefile$shape$data)
        crss <- sapply(mosaic_list$files, function(x) {
          sf::st_crs(x) == sf::st_crs(shapefile$shape$data)
        })
        if(any(isFALSE(crss))){
          sendSweetAlert(
            session = session,
            title = "Ops, invalid files.",
            text = "The Coordinate Reference System does not match for some files...",
            type = "error"
          )
        } else{
          waiter_show(
            html = tagList(
              spin_google(),
              h2("Cropping the raster files, please wait.")
            ),
            color = "#228B227F"
          )
          shpcrop <- shapefile$shape$data |>
            terra::vect() |>
            terra::buffer(5) |>
            terra::ext()

          infiles <- sapply(mosaic_list$files, terra::sources)
          outfiles <- paste0(tempdir(), "\\", paste0(file_name(infiles), ".", file_extension(infiles)))
          for (i in seq_along(infiles)) {
            suppressWarnings(
              sf::gdal_utils(
                util = "warp",
                source = infiles[[i]],
                destination = outfiles[[i]],
                options = strsplit(paste("-te", shpcrop[1], shpcrop[3], shpcrop[2], shpcrop[4]), split = "\\s")[[1]]
              )
            )
          }
          # Assign the cropped files to a new reactive variable
          croppedfiles <- lapply(outfiles, terra::rast)
          names(croppedfiles) <- datatimes$dates
          mosaiclist[["mosaics"]] <- create_reactval("mosaics",  croppedfiles)

        }
        waiter_hide()
      } else{
        mosaiclist[["mosaics"]] <- create_reactval("mosaics",  mosaic_list$files)
      }

    })


    # plot the
    output$plotmosaic <- renderPlot({
      req(mosaiclist$mosaics$data)
      if(inherits(mosaiclist$mosaics$data[[1]], "SpatRaster")){
        if(input$showmosaic == "rgb"){
          terra::plotRGB(mosaiclist$mosaics$data[[input$mosaicslider]], stretch = "hist")
        } else{
          terra::plot(mosaiclist$mosaics$data[[input$mosaicslider]][[as.numeric(input$bandnumber)]])
        }
      }
      if(!is.null(shapefile$shape$data)){
        if(input$plotshape){
          terra::plot(shapefile_input(shapefile$shape$data, as_sf = FALSE, info = FALSE), add = TRUE, col = "red")
        }
      }
    })

    # interactive visualization
    observe({
      req(mosaiclist$mosaics$data)
      req(input$showmosaic)
      if(inherits(mosaiclist$mosaics$data[[1]], "SpatRaster")){
        if(input$showmosaic == "rgb"){
          bmtmp <- mosaic_view(
            mosaiclist$mosaics$data[[input$mosaicslider]],
            r = as.numeric(r$r),
            g = as.numeric(g$g),
            b = as.numeric(b$b),
            re = as.numeric(re$re),
            nir = as.numeric(nir$nir),
            # quantiles = input$quantileplot,
            max_pixels = input$maxpixels
          )
        } else{
          bmtmp <-
            mosaic_view(mosaiclist$mosaics$data[[input$mosaicslider]][[as.numeric(input$bandnumber)]],
                        show = "index",
                        color_regions  = scales::brewer_pal(palette = "RdYlGn")(8),
                        max_pixels = input$maxpixels,
                        na.color = "transparent")
        }
        basemap$map <- bmtmp
      }
    })

    output$leafletmap <- renderLeaflet({
      req(basemap$map)  # Ensure mosaic_data$mosaic is not NULL
      if(input$plotshape){
        if(!is.null(shapefile$shape$data)){
          (basemap$map + shapefile_view(shapefile_input(shapefile$shape$data, as_sf = FALSE, info = FALSE)))@map
        } else{
          basemap$map@map
        }
      }

    })
  })
}

## To be copied in the UI
# mod_timeseriesinput_ui("timeseriesinput_1")

## To be copied in the server
# mod_timeseriesinput_server("timeseriesinput_1")