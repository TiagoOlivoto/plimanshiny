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
          height = "710px",
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
          fluidRow(
            col_7(
              switchInput(
                inputId = ns("clipinparallel"),
                label = "Parallel",
                onLabel = "Yes",
                offLabel = "No",
                labelWidth = "80px"
              )
            ),
            col_5(
              conditionalPanel(
                condition = "input.clipinparallel == true",  ns = ns,
                numericInput(ns("numworkersclip"),
                             label = "Clusters",
                             value = NULL)
              )
            )
          ),
          hl(),
          h3("Output"),
          shinyDirButton(id=ns("folderclip"),
                         label="Select an output folder",
                         title="Select an output folder",
                         buttonType = "default",
                         class = NULL,
                         icon = NULL,
                         style = NULL),
          selectInput(ns("clipformat"),
                      label = "Format",
                      choices = c(".png", ".tif"),
                      selected = ".png"),
          prettyCheckbox(
            inputId = ns("seeaclippedplot"),
            label = "Show me a clipped plot",
            value = FALSE,
            status = "info",
            icon = icon("thumbs-up"),
            plain = TRUE,
            outline = TRUE,
            animation = "rotate"
          ),
          conditionalPanel(
            condition = "input.seeaclippedplot == true", ns = ns,
            selectizeInput(ns("myclippedplot"),
                           label = "Clipped Plot",
                           choices = NULL)
          ),
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
          conditionalPanel(
            condition = "input.seeaclippedplot == true", ns = ns,
            h3("Clipped Plot"),
            leafletOutput(ns("clippedplot"), height = "640px") |> add_spinner()
          ),
          conditionalPanel(
            condition = "input.seeaclippedplot == false", ns = ns,
            leafletOutput(ns("mosaicandshape"), height = "640px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' plotclip Server Functions
#'
#' @noRd
mod_plotclip_server <- function(id, mosaic_data, shapefile, r, g, b, basemap){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_clip", choices = c("Active mosaic", setdiff(names(mosaic_data), "mosaic")), selected = "Active mosaic")
      updateSelectInput(session, "shape_to_clip", choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")))
      availablecl <- parallel::detectCores()
      updateNumericInput(session, "numworkersclip", value = round(availablecl * 0.5), max = availablecl - 2)
    })
    observe({
      updateTextInput(session, "new_mask", value = paste0(input$mosaic_to_clip, "_masked"))
    })

    volumes <- c("R Installation" = R.home(), getVolumes()())
    shinyDirChoose(input, "folderclip",
                   roots = volumes,
                   session = session,
                   restrictions = system.file(package = "base"))


    # Observe event for mosaic crop action
    observeEvent(input$startclip, {

      sendSweetAlert(
        session = session,
        title = "Clipping plots",
        text = "First, choose an output directory to save the clipped plots. Select an Unique ID column to name the images, then click on 'Clip plots'",
        type = "info"
      )
      req(input$mosaic_to_clip)
      shptocrop <- shapefile[[input$shape_to_clip]]$data

      updateSelectInput(session, "uniqueid", choices = names(shptocrop))
      if(input$mosaic_to_clip == "Active mosaic"){
        mosaictocrop <- mosaic_data$mosaic
        bcrop <- basemap$map
      } else{
        bcrop <-
          mosaic_view(
            mosaic_data[[input$mosaic_to_clip]]$data,
            r = suppressWarnings(as.numeric(r$r)),
            g = suppressWarnings(as.numeric(g$g)),
            b = suppressWarnings(as.numeric(b$b)),
            max_pixels = 500000
          )
        mosaictocrop <- mosaic_data[[input$mosaic_to_clip]]$data
      }

      output$mosaicandshape <- renderLeaflet({
        req(bcrop)
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
          if(!input$clipinparallel){
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
              shpname <- shptemp |> as.data.frame() |> dplyr::pull(ncolid)
              mosaictmp <- terra::crop(mosaictocrop, shptemp) |> terra::mask(shptemp)
              terra::writeRaster(mosaictmp, paste0(diroutput, "/", shpname, input$clipformat), overwrite=TRUE)

            }
          } else{
            req(input$numworkersclip)
            nworkers <- input$numworkersclip
            future::plan(future::multisession, workers = nworkers)
            on.exit(future::plan(future::sequential))
            `%dofut%` <- doFuture::`%dofuture%`

            waiter_show(
              html = tagList(
                spin_google(),
                h2(paste0("Clipping plots using parallel processing in multiple sessions (",input$numworkersclip ,"). Please, wait."))
              ),
              color = "#228B227F"
            )
            ## declare alias for dopar command
            uniqueid <- input$uniqueid
            shptocrop <- shapefile[[input$shape_to_clip]]$data
            req(shptocrop)
            tmpterra <- tempdir()
            mosaic_export(mosaictocrop, paste0(tmpterra, "/tmpclip.tif"), overwrite = TRUE)
            format <- input$clipformat

            foreach::foreach(i = 1:nrow(shptocrop)) %dofut%{
              shptemp <- shptocrop[i, ]
              ncolid <- which(colnames(shptemp) == uniqueid)
              shpname <- shptemp |> as.data.frame()  |> dplyr::pull(ncolid)
              mosaictmp <- terra::crop(terra::rast(paste0(tmpterra, "/tmpclip.tif")), shptemp) |> terra::mask(shptemp)
              terra::writeRaster(mosaictmp, paste0(diroutput, "/", shpname, format), overwrite=TRUE)
            }
          }


          filestoremove <- list.files(diroutput, pattern = "png.aux")
          file.remove(paste0(diroutput, "/", filestoremove))
          waiter_hide()

          sendSweetAlert(
            session = session,
            title = "Mosaic successfully clipped!!",
            text = paste0("The plots have been successfully clipped and can now be found at ", diroutput, "."),
            type = "success"
          )
        }

      })


      observe({
        if(input$seeaclippedplot){
          ncolid <- which(colnames(shptocrop) == input$uniqueid)
          plots <- shptocrop |> as.data.frame() |> dplyr::pull(ncolid)

          updateSelectizeInput(session, "myclippedplot",
                               options = list(maxOptions = 20000),
                               server = TRUE,
                               choices = plots)
          req(input$myclippedplot)
          shptoplot <- shptocrop[which(plots == input$myclippedplot), ]
          motemp <- terra::crop(mosaictocrop, shptoplot) |> terra::mask(shptoplot)

          output$mosaicandshapeclipped <- renderLeaflet({
            (bcrop + shapefile_view(shptoplot))@map
          })

          output$clippedplot <- renderLeaflet({
            croppplot <-
              mosaic_view(
                motemp,
                r = suppressWarnings(as.numeric(r$r)),
                g = suppressWarnings(as.numeric(g$g)),
                b = suppressWarnings(as.numeric(b$b)),
                max_pixels = 500000
              )
            croppplot@map
          })

        }
      })

    })
  })
}

## To be copied in the UI
# mod_plotclip_ui("plotclip_1")

## To be copied in the server
# mod_plotclip_server("plotclip_1")
