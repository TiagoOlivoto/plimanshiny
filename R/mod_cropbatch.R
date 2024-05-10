#' cropbatch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cropbatch_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "720px",
          status = "success",
          title = "Crop raster files",
          selected = "Cropping configuration",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Cropping configuration",
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
            selectizeInput(inputId = ns("mosaics"),
                           label = "Selected raster files",
                           choices = NULL,
                           multiple=TRUE,
                           options = list('plugins' = list('remove_button'),
                                          'create' = TRUE,
                                          'persist'= FALSE)
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
              multiple=TRUE,
              options = list('plugins' = list('remove_button'),
                             'create' = TRUE,
                             'persist' = FALSE)
            ),
            fluidRow(
              col_4(
                br(),
                shinyDirButton(id=ns("outfolder"),
                               label="Output folder",
                               title="Output folder",
                               buttonType = "default",
                               class = NULL,
                               icon = NULL,
                               style = NULL),
              ),
              col_8(
                textInput(ns("outdir"),
                          label = "Output folder",
                          value = "")
              ),
              numericInput(
                ns("buffer"),
                label = "Buffer",
                value = 5
              ),
              textInput(
                ns("suffix"),
                label = "Suffix",
                value = "_cropped"
              )
            ),
            actionBttn(
              ns("startcrop"),
              label = "Crop!",
              style = "pill",
              color = "success",
              icon = icon("layer-group")
            )
          )
        )
      ),
      col_8(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "720px",
          status = "success",
          title = "Cropping a mosaic",
          selected = "Cropping a mosaic",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Cropping a mosaic",
            plotOutput(ns("plotmosaic"), height = "650px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' cropbatch Server Functions
#'
#' @noRd
mod_cropbatch_server <- function(id, shapefile, mosaiclist){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
          mosaic_list$files <- paste0(filedir$dir, "/", filenames, ".", file_extension(input_file_selected$paths$datapath))

        }
      }
    })
    observe({
      volumes <- c(getVolumes()())
      shinyDirChoose(input, "outfolder",
                     roots = volumes,
                     session = session,
                     restrictions = system.file(package = "base"))
      diroutput <- parseDirPath(volumes, input$outfolder)
      req(diroutput)
      updateTextInput(session, "outdir", value = diroutput)
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
      }
    })

    #import the mosaics
    observeEvent(input$importrasters, {
      req(mosaic_list$files)
      nams <- file_name(mosaic_list$files)
      mosaic_list$files <- lapply(mosaic_list$files, terra::rast)
      names(mosaic_list$files) <- nams
      updateSelectizeInput(session, "mosaics",
                           choices = nams,
                           selected = nams)
    })

    #import the shapefile
    observeEvent(input$importshapefile, {
      req(pathshape$file$datapath[1])
      shapefile[["shape"]] <- create_reactval("shape", shapefile_input(pathshape$file$datapath[1], info = FALSE))
      if(length(pathshape$file$datapath) != 0){
        updateSelectInput(session, "shapefileimported",
                          choices = file_name(pathshape$file$datapath),
                          selected = file_name(pathshape$file$datapath))
      }
    })

    output$plotmosaic <- renderPlot({
      req(mosaic_list$files)
      req(shapefile$shape)
      if(inherits(mosaic_list$files[[1]], "SpatRaster")){
        if(terra::nlyr(mosaic_list$files[[1]] > 2)){
          terra::plotRGB(mosaic_list$files[[1]], stretch = "hist")
        } else{
          terra::plot(mosaic_list$files[[1]][[1]])
        }
      }
      terra::plot(terra::vect(shapefile$shape$data), add = TRUE, col = "red")
    })

    observeEvent(input$startcrop, {
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
        shpcrop <-
          shapefile$shape$data |>
          terra::vect() |>
          terra::buffer(input$buffer) |>
          terra::ext()

        infiles <- sapply(mosaic_list$files, terra::sources)
        outfiles <- paste0(input$outdir, "\\", paste0(file_name(infiles), input$suffix, ".", file_extension(infiles)))
        progressSweetAlert(
          session = session,
          id = "myprogress",
          title = "Start",
          display_pct = TRUE,
          value = 0,
          total = length(infiles)
        )

        for (i in seq_along(infiles)) {
          updateProgressBar(
            session = session,
            id = "myprogress",
            value = i,
            title = paste0("Cropping the raster files, Please, wait."),
            total = length(infiles)
          )
          suppressWarnings(
            sf::gdal_utils(
              util = "warp",
              source = infiles[[i]],
              destination = outfiles[[i]],
              options = strsplit(paste("-te", shpcrop[1], shpcrop[3], shpcrop[2], shpcrop[4]), split = "\\s")[[1]]
            )
          )
        }

        closeSweetAlert(session = session)
      }

    })


  })
}

## To be copied in the UI
# mod_cropbatch_ui("cropbatch_1")

## To be copied in the server
# mod_cropbatch_server("cropbatch_1")
