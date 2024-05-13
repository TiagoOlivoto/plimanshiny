#' sentinel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sentinel_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      col_3(
        bs4Card(
          title = "Settings",
          collapsible = FALSE,
          width = 12,
          height = "620px",
          shinyDirButton(id=ns("indir"),
                         label="Input/output folder",
                         title="Choose a working directory",
                         buttonType = "primary",
                         multiple = TRUE,
                         class = NULL,
                         icon = icon("file"),
                         style = NULL),
          textInput(ns("indirloc"),
                    label = "Input/output directory",
                    value = ""
          ),
          conditionalPanel(
            condition = "input.indirloc != ''", ns = ns,
            shinyFilesButton(id=ns("filePath"),
                             label="Input layers",
                             title="Choose the sentinel bands",
                             buttonType = "primary",
                             multiple = TRUE,
                             class = NULL,
                             icon = icon("file"),
                             style = NULL)
          ),
          numericInput(
            ns("spatres"),
            label = "Spatial resolution",
            value = 10
          ),
          textInput(ns("outputfile"),
                    label = "Output file",
                    value = "sentinel_bands.tif"),
          actionBttn(
            ns("bindlayers"),
            label = "Bind!",
            style = "pill",
            color = "success",
            icon = icon("layer-group")
          )
        )
      ),
      col_9(
        bs4Card(
          title = "Merged layers",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          plotOutput(ns("plotsentinel"))

        )
      )
    )
  )
}

#' sentinel Server Functions
#'
#' @noRd
mod_sentinel_server <- function(id, mosaic_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      volumes <- c("R Installation" = R.home(), getVolumes()())
      shinyDirChoose(input, "indir",
                     roots = volumes,
                     session = session,
                     restrictions = system.file(package = "base"))
      diroutput <- parseDirPath(volumes, input$indir)
      req(diroutput)
      updateTextInput(session, "indirloc", value = diroutput)

      # choose file
      shinyFileChoose(input, "filePath",
                      root = c("Input folder" = input$indirloc),
                      session = session)
      observeEvent(input$bindlayers, {
        if(!is.null(input$filePath)){
          input_file_selected <- parseFilePaths(c("Input folder" = input$indirloc), input$filePath)
          if(length(input_file_selected$datapath) != 0){
            waiter_show(
              html = tagList(
                spin_google(),
                h2("Binding the layers. Please, wait.")
              ),
              color = "#228B227F"
            )
            sentinel_to_tif(input_file_selected$datapath,
                            path = dirname(input_file_selected$datapath[[1]]),
                            destination = input$outputfile,
                            spat_res = input$spatres
            )
            waiter_hide()
            output$plotsentinel <- renderPlot({
              mosaic <- terra::rast(paste0(dirname(input_file_selected$datapath[[1]]), "/", input$outputfile))
              mosaic_data[[input$outputfile]] <- create_reactval(input$outputfile, mosaic)
              terra::plot(mosaic)
            })


            sendSweetAlert(
              session = session,
              title = "Layers successfully binded.",
              text = paste0("You can now find the binded raster on ", paste0(dirname(input_file_selected$datapath[[1]]), "/", input$outputfile)),
              type = "success"
            )
          }
        }
      })

    })


  })
}

## To be copied in the UI
# mod_sentinel_ui("sentinel_1")

## To be copied in the server
# mod_sentinel_server("sentinel_1")
