#' plotclip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plotclip_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mask Settings",
          collapsible = FALSE,
          width = 12,
          height = "620px",
          footer = "Module to clip plots. Select a mosaic, a shapefile and the output directory",
          hl(),
          h3("Input"),
          selectInput(ns("mosaic_to_clip"),
                      label = "Mosaic to be clipped",
                      choices = NULL),
          selectInput(ns("shape_to_clip"),
                      label = "Shapefile",
                      choices = NULL),
          selectInput(ns("uniqueid"),
                      label = "Unique ID",
                      choices = NULL),
          hl(),
          h3("Output"),
          shinyDirButton(id=ns("folderclip"),
                         label="Select an output folder",
                         title="Select an output folder",
                         buttonType = "default",
                         class = NULL,
                         icon = NULL,
                         style = NULL),
          fluidRow(
            col_6(
              actionBttn(ns("startclip"),
                         label = "Start clipping!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("clipmosaic"),
                         label = "Clip!",
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
          title = "Crop Results",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          fluidRow(
            col_6(
              h3("Mosaic and Shape"),
              leafletOutput(ns("mosaicandshape"), height = "640px") |> add_spinner()
            ),
            col_6(
              h3("Cropped mosaic"),
              leafletOutput(ns("mosaicmasked"), height = "640px") |> add_spinner()
            )
          )
        )
      )
    )
  )
}

#' plotclip Server Functions
#'
#' @noRd
mod_plotclip_server <- function(id, mosaic_data, shapefile, r, g, b){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_clip", choices = setdiff(names(mosaic_data), "mosaic"), selected = input$mosaic_to_clip)
      updateSelectInput(session, "shape_to_clip", choices = setdiff(names(shapefile), "shapefile"))
      updateTextInput(session, "new_mask", value = paste0(input$mosaic_to_clip, "_masked"))
    })

    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyDirChoose(input, "folderclip",
                   roots = volumes,
                   session = session,
                   restrictions = system.file(package = "base"))


    # Observe event for mosaic crop action
    observeEvent(input$startclip, {


      sendSweetAlert(
        session = session,
        title = "Clipping plots",
        text = "First, choose an output directory, then click on 'Clip plots'",
        type = "info"
      )
      shptocrop <- shapefile[[input$shape_to_clip]]$data
      mosaictocrop <- mosaic_data[[input$mosaic_to_clip]]$data
      req(shptocrop)
      req(mosaictocrop)
      updateSelectInput(session, "uniqueid", choices = names(shptocrop))

      output$mosaicandshape <- renderLeaflet({
        bcrop <-
          mosaic_view(
            mosaic_data[[input$mosaic_to_clip]]$data,
            r = as.numeric(r$r),
            g = as.numeric(g$g),
            b = as.numeric(b$b),
            max_pixels = 500000
          )

        (bcrop + shapefile_view(shptocrop))@map
      })



      # Observe event for mosaic crop action
      observeEvent(input$clipmosaic, {
        # Update mosaic_data$mosaic when input$cropmosaic is clicked
        diroutput <- parseDirPath(volumes, input$folderclip)
        req(diroutput)
        if(diroutput == "character(0)"){
          sendSweetAlert(
            session = session,
            title = "Ops, output folder not defined",
            text = "To clip the plots, first choose an output directory using the 'Select an output folder' button.",
            type = "error"
          )
        } else{

          req(input$uniqueid)
          progressSweetAlert(
            session = session, id = "myprogressclip",
            title = "Start",
            display_pct = TRUE,
            value = 0,
            total = nrow(shptocrop)
          )
          for(i in 1:nrow(shptocrop)){
            updateProgressBar(
              session = session,
              id = "myprogressclip",
              value = i,
              title = paste0("Working in progress, Please, wait."),
              total = nrow(shptocrop)
            )
            shptemp <- shptocrop[i, ]
            ncolid <- which(colnames(shptemp) == input$uniqueid)
            shpname <- shptemp |> as.data.frame() |>  poorman::select(ncolid) |> poorman::pull()
            mosaictmp <- terra::crop(mosaictocrop, shptemp) |> terra::mask(shptemp)
            terra::writeRaster(mosaictmp, paste0(diroutput, "/", shpname, ".png"), datatype='INT1U')

            # terra::plotRGB()
            # print(shpname)
          }
          filestoremove <- list.files(diroutput, pattern = "png.aux")
          # print(filestoremove)
          file.remove(paste0(diroutput, "/", filestoremove))

          sendSweetAlert(
            session = session,
            title = "Mosaic successfully masked!!",
            text = paste0("The plots have been successfully clipped and can now be found at ", diroutput, "."),
            type = "success"
          )
        }

      })


    })
  })
}

## To be copied in the UI
# mod_plotclip_ui("plotclip_1")

## To be copied in the server
# mod_plotclip_server("plotclip_1")
