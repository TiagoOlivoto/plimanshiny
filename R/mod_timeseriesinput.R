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
                      sliderInput(ns("quantileplot"),
                                  label = "Quantiles",
                                  min = 0,
                                  max = 1,
                                  value = c(0, 1),
                                  step = 0.001),
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
            col_4(
              awesomeRadio(
                inputId = ns("showmosaic"),
                label = "Show",
                choices = c("rgb", "bands"),
                selected = "rgb",
                inline = TRUE
              )
            ),
            col_4(
              conditionalPanel(
                condition = "input.showmosaic == 'bands'", ns = ns,
                selectInput(
                  ns("bandnumber"),
                  label = "Band number",
                  choices = NULL
                )
              )
            ),
            col_4(
              selectInput(
                ns("stretch"),
                label = "Stretch",
                choices = c("none", "lin", "hist")
              )
            )
          ),
          sliderInput(
            ns("gammacorr"),
            label = "Gamma correction",
            min = -5,
            max = 5,
            value = 1,
            step = 0.1
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
            title = "Animation",
            fluidRow(
              col_2(
                actionBttn(
                  ns("startanimation"),
                  icon = icon("film"),
                  label = "Animate!",
                  style = "pill",
                  color = "success"
                )
              ),
              col_2(
                actionBttn(
                  ns("loop"),
                  icon = icon("rotate"),
                  label = "Loop",
                  style = "pill",
                  color = "success"
                )
              ),
              col_2(
                selectInput(
                  ns("rgborind"),
                  label = "Show",
                  choices = c("rgb", "index"),
                  selected = "rgb"
                )
              ),
              col_2(
                conditionalPanel(
                  condition = "input.rgborind == 'index'", ns = ns,
                  selectInput(
                    ns("myindex"),
                    label = "Vegetation index",
                    choices = pliman::pliman_indexes(),
                    selected = "rgb"
                  )
                )
              ),
              col_2(
                selectInput(
                  ns("fps"),
                  label = "Frames per Second",
                  choices = c(0.5, 1, 2, 4, 5, 10),
                  selected = 2
                )
              ),
              col_2(
                downloadButton(ns("downloadgif"), "Download Animation"),
              )
            ),
            imageOutput(ns("animation"), height = "720px") |> add_spinner()

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
mod_timeseriesinput_server <- function(id, shapefile, mosaiclist, r, g, b, re, nir,  swir, tir,  basemap, quantiles){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
    })

    input_file_selected <- reactiveValues(paths = NULL)
    mosaic_list <- reactiveValues(files = NULL)
    filedir <- reactiveValues(dir = NULL)
    datatimes <- reactiveValues(dates = NULL)
    observe({
      shinyFileChoose(input, "filemosaic",
                      root = getVolumes()(),
                      filetypes = c('tif', 'jp2', 'tiff', 'jpeg', "dat"),
                      session = session)
      if(!is.null(input$filemosaic)){
        input_file_selected$paths <- parseFilePaths(getVolumes()(), input$filemosaic)
        if(length(input_file_selected$paths$datapath) != 0){
          filenames <- file_name(input_file_selected$paths$datapath)
          filedir$dir <- file_dir(input_file_selected$paths$datapath)
          date_match <- regmatches(filenames, regexpr("(\\d{8})|((\\d{2}-\\d{2}-\\d{4})|(\\d{4}-\\d{2}-\\d{2}))", filenames))
          dates <- try(as.Date(date_match, tryFormats =  date_format(date_match)[[1]]))
          if(inherits(dates, "try-error")){
            sendSweetAlert(
              session = session,
              title = "Ops, invalid file name",
              text = "It was not possible to extract the data information for all files. Please, ensure that all the rasterfiles have one of the following pattern names: 1) 'mm-dd-yyyy'; 2) 'dd-mm-yyyy'; or 3) 'yyyy-dd-mm'",
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


    observeEvent(input$mosaicinfomosaic, {
      req(mosaiclist$mosaics$data[[1]])
      mosaic_info(mosaiclist$mosaics$data[[1]])
    })

    observe({
      if(!is.null(basemap$map)){
        waiter_hide()
        # check for correct layer configuration
        if(inherits(mosaiclist$mosaics$data[[1]], "SpatRaster")){
          nl <- terra::nlyr(mosaiclist$mosaics$data[[1]])
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
        }
      }
    })


    #import the shapefile
    observeEvent(input$importshapefile, {
      req(pathshape$file$datapath[1])
      req(input$shapefileimported)
      shapefile[[input$shapefileimported]] <- create_reactval(input$shapefileimported, shapefile_input(pathshape$file$datapath[1], info = FALSE))
    })

    observe({
      if (input$croptoshape) {
        req(shapefile[[input$shapefileimported]]$data)
        crss <- sapply(mosaic_list$files, function(x) {
          sf::st_crs(x) == sf::st_crs(shapefile[[input$shapefileimported]]$data)
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
          shpcrop <- shapefile[[input$shapefileimported]]$data |>
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

    mosaictoplot <- reactive({
      req(mosaiclist$mosaics$data)
      aggr <- find_aggrfact(mosaiclist$mosaics$data[[input$mosaicslider]])
      if(aggr > 0){
        mosaic_aggregate(mosaiclist$mosaics$data[[input$mosaicslider]], round(100 / aggr))
      } else{
        mosaiclist$mosaics$data[[input$mosaicslider]]
      }
    })


    # plot the
    output$plotmosaic <- renderPlot({
      req(mosaiclist$mosaics$data)
      if(inherits(mosaiclist$mosaics$data[[1]], "SpatRaster")){

        if(input$showmosaic == "rgb"){
          if(input$stretch == "none"){
            terra::plotRGB(mosaictoplot() ^input$gammacorr,
                           r = suppressWarnings(as.numeric(r$r)),
                           g = suppressWarnings(as.numeric(g$g)),
                           b = suppressWarnings(as.numeric(b$b)))

          } else{
            terra::plotRGB(mosaictoplot() ^ input$gammacorr,
                           r = suppressWarnings(as.numeric(r$r)),
                           g = suppressWarnings(as.numeric(g$g)),
                           b = suppressWarnings(as.numeric(b$b)),
                           stretch = input$stretch)
          }
        } else{
          terra::plot(mosaictoplot()[[as.numeric(input$bandnumber)]])
        }
      }
      if(input$shapefileimported != ""){
        if(input$plotshape){
          req(shapefile[[input$shapefileimported]]$data)
          terra::plot(shapefile_input(shapefile[[input$shapefileimported]]$data, as_sf = FALSE, info = FALSE),
                      add = TRUE,
                      col = color_alpha("red", 0.5))
        }
      }
    })

    # interactive visualization
    # Ensure that observer runs after the plot
    observe({
      req(mosaiclist$mosaics$data)
      req(input$showmosaic)
      isolate({
        if (inherits(mosaiclist$mosaics$data[[1]], "SpatRaster")) {
          if (input$showmosaic == "rgb") {
            bmtmp <- mosaic_view(
              mosaictoplot(),
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              max_pixels = input$maxpixels,
              quantiles = quantiles$q
            )
          } else {
            bmtmp <- mosaic_view(
              mosaictoplot()[[as.numeric(input$bandnumber)]],
              show = "index",
              color_regions = scales::brewer_pal(palette = "RdYlGn")(8),
              max_pixels = input$maxpixels,
              na.color = "transparent"
            )
          }
          basemap$map <- bmtmp
        }
      })
    })

    output$leafletmap <- renderLeaflet({
      req(basemap$map)  # Ensure mosaic_data$mosaic is not NULL
      if(input$plotshape){
        if(!is.null(shapefile[[input$shapefileimported]]$data)){
          (basemap$map + shapefile_view(shapefile_input(shapefile[[input$shapefileimported]]$data, as_sf = FALSE, info = FALSE)))@map
        } else{
          basemap$map@map
        }
      } else{
        basemap$map@map
      }
    })



    # Animation
    observeEvent(input$startanimation, {
      req(mosaiclist$mosaics$data)
      # if is RGB
      progressSweetAlert(
        session = session,
        id = "myprogress",
        title = "Start",
        display_pct = TRUE,
        value = 0,
        total = length(mosaiclist$mosaics$data)
      )

      list_mo <- list()
      if(input$rgborind == "index"){
        for (i in seq_along(mosaiclist$mosaics$data)) {
          updateProgressBar(
            session = session,
            id = "myprogress",
            value = i,
            title = paste0("Computing the vegetation index. Please wait."),
            total = length(mosaiclist$mosaics$data)
          )
          tind <- try(
            mosaic_index(
              mosaiclist$mosaics$data[[i]],
              r = suppressWarnings(as.numeric(r$r)),
              g = suppressWarnings(as.numeric(g$g)),
              b = suppressWarnings(as.numeric(b$b)),
              re = suppressWarnings(as.numeric(re$re)),
              nir = suppressWarnings(as.numeric(nir$nir)),
              swir = suppressWarnings(as.numeric(swir$swir)),
              tir = suppressWarnings(as.numeric(tir$tir)),
              index = input$myindex,
              plot = FALSE
            ),
            silent = TRUE
          )
          if(!inherits(tind, "try-error")){
            list_mo[[i]] <- tind
          }
        }
        req(list_mo)
        minmax <- sapply(list_mo, terra::minmax)

        for (i in 1:length(list_mo)) {
          updateProgressBar(
            session = session,
            id = "myprogress",
            value = i,
            title = paste0("Rendering the animation. Please wait."),
            total = length(mosaiclist$mosaics$data)
          )
          tfaft <- glue::glue(system.file("app", package = "plimanshiny" ), "/www/animation{pliman::leading_zeros(i, 4)}.png")
          wdt <- dim(list_mo[[1]])[[2]]
          hei <- dim(list_mo[[1]])[[1]]
          ratio <- hei / wdt
          png(tfaft,
              width = 780,
              height = 780 * ratio)
          mosaic_plot(list_mo[[i]],
                      range = c(min(minmax), max(minmax)),
                      axes = FALSE)
          dev.off()
        }
        closeSweetAlert(session = session)
      } else{
        for (i in 1:length(mosaiclist$mosaics$data)) {
          updateProgressBar(
            session = session,
            id = "myprogress",
            value = i,
            title = paste0("Rendering the animation. Please wait."),
            total = length(mosaiclist$mosaics$data)
          )
          tfaft <- glue::glue(system.file("app", package = "plimanshiny" ), "/www/animation{pliman::leading_zeros(i, 4)}.png")
          wdt <- dim(mosaiclist$mosaics$data[[1]])[[2]]
          hei <- dim(mosaiclist$mosaics$data[[1]])[[1]]
          ratio <- hei / wdt
          png(tfaft,
              width = 780,
              height = 780 * ratio)
          try(
            {
              terra::plotRGB(mosaiclist$mosaics$data[[i]],
                             r = suppressWarnings(as.numeric(r$r)),
                             g = suppressWarnings(as.numeric(g$g)),
                             b = suppressWarnings(as.numeric(b$b)),
                             axes = FALSE)
            }
          )
          dev.off()
        }
        closeSweetAlert(session = session)
      }

      dir <- pliman::file_dir(glue::glue(system.file("app", package = "plimanshiny" ), "/www/animation{i}.png"))
      num_images <- image_read(paste0(dir, "/", list.files(dir, pattern = "animation")))
      anim <- image_animate(num_images,
                            fps = input$fps |> chrv2numv(),
                            loop = 1,
                            optimize = TRUE,
                            dispose = "previous")

      output$animation <- renderImage({
        list(src = anim |> image_write(tempfile(fileext = ".gif")), contentType = "image/jpeg")
      },
      deleteFile = TRUE)

      observeEvent(input$loop, {
        anim <- image_animate(num_images,
                              fps = input$fps |> chrv2numv(),
                              loop = 1,
                              optimize = TRUE,
                              dispose = "previous")
        output$animation <- renderImage({
          list(src = anim |> image_write(tempfile(fileext = ".gif")), contentType = "image/jpeg")
        },
        deleteFile = TRUE)
      })


      output$downloadgif <- downloadHandler(
        filename = "timeseries_animation.gif",
        content = function(file) {
          anim |> image_write(file)
        }
      )

      a <- file.remove(paste0(dir, "/", list.files(dir, pattern = "animation")))
    })
  })
}

## To be copied in the UI
# mod_timeseriesinput_ui("timeseriesinput_1")

## To be copied in the server
# mod_timeseriesinput_server("timeseriesinput_1")
