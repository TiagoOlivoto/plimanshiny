#' indexes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indexes_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mosaic index",
          color = "success",
          fluidRow(
            col_6(
              actionButton(
                inputId = ns("guideindex"),
                label = tagList(
                  icon = icon("question-circle", verify_fa = FALSE), "Guide"
                ),
                style = "color: white ; background-color: #dd4b39",
                class = "btn-danger"
              )
            ),
            col_6(
              actionButton(
                inputId = ns("mosaicinfoindex"),
                label = tagList(
                  icon = icon("circle-info", verify_fa = FALSE), "Mosaic Info"
                ),
                status = "info"
              )
            )
          ),
          width = 12,
          status = "success",
          hl(),
          divclass("ind1",
                   pickerInput(
                     inputId = ns("imgbands"),
                     label = "Image Bands",
                     choices = c("RGB", "MS"),
                     selected = "RGB",
                     multiple = TRUE
                   ),
                   pickerInput(
                     inputId = ns("plotindexes"),
                     label = "Vegetation indexes",
                     choices = "",
                     multiple = TRUE
                   ),
                   textInput(ns("myindex"),
                             label = "My personalized index",
                             value = ""),
                   prettyCheckbox(
                     inputId = ns("inmemory"),
                     label = "Compute indexes in memory?",
                     value = TRUE,
                     icon = icon("check"),
                     status = "success",
                     animation = "rotate"
                   ),
                   numericInput(ns("workers"),
                                label = "Number of workers",
                                value = 1)
          ),
          actionBttn(
            inputId = ns("computeindex"),
            label = "Compute the indexes!",
            style = "pill",
            color = "primary",
            icon = icon("chart-simple")
          ),
          hl(),
          divclass("ind2",
                   selectInput(ns("indextosync"),
                               label = "Index to sync with basemap",
                               choices = NULL),
                   selectInput(ns("shapefiletoplot"),
                               label = "Shapefile",
                               choices = NULL)
          ),
          hl(),
          pickerInput(
            inputId = ns("indextodownload"),
            label = "Index to download",
            choices = "",
            multiple = TRUE
          ),
          mod_download_mosaic_ui(ns("download_indexes"), "Donwnload index")
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabsindex",
          width = 12,
          height = "790px",
          status = "success",
          title = "Vegetation Indexes",
          selected = "Plot Index (raster)",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Plot Index (raster)",
            materialSwitch(
              inputId = ns("truncateindex"),
              label = "Truncate index?",
              value = FALSE,
              status = "success"
            ),
            conditionalPanel(
              condition = "input.truncateindex == true", ns = ns,
              fluidRow(
                col_5(
                  input_histoslider(
                    id = ns("truncslider"),
                    label = "Truncate to...",
                    values = runif(50),
                    height = 350,
                  ),
                  actionBttn(
                    inputId = ns("truncindex"),
                    label = "Truncate!",
                    style = "pill",
                    color = "success",
                    icon = icon("down-left-and-up-right-to-center")
                  )
                ),
                col_7(
                  plotOutput(ns("plotindextrunc"), height = "600px")
                )
              )
            ),
            conditionalPanel(
              condition = "input.truncateindex == false", ns = ns,
              plotOutput(ns("plotindex"), height = "720px") |> add_spinner()
            )
          ),
          tabPanel(
            title = "Plot Index (density)",
            plotOutput(ns("plotindexhist"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Plot Index (shapefile)",
            fluidRow(
              col_10(
                pickerpalette(id, "palplotindex", selected = "RdYlGn"),
              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("palplotindexrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              )
            ),
            leafletOutput(ns("indexshp"), height = "680px")|> add_spinner()
          ),
          tabPanel(
            title = "Syncked maps",
            uiOutput(ns("indexsync"))|> add_spinner()
          )
        )
      )
    )
  )
}

helpind <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "index")
#' indexes Server Functions
#'
#' @noRd
mod_indexes_server <- function(id, mosaic_data, r, g, b, re, nir, basemap, index, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$guideindex, introjs(session,
                                           options = list("nextLabel"="Next",
                                                          "prevLabel"="Previous",
                                                          "skipLabel"="Skip",
                                                          steps = helpind),
                                           events = list("oncomplete"=I('alert("Hope it helped!")'))))
    mosaictmp <- reactiveValues(mosaic =NULL)
    observe({
      updateSelectInput(session, "shapefiletoplot",
                        choices = c("none", setdiff(names(shapefile), "shapefile")),
                        selected = "none")
      if(!is.null(shapefile$shapefile)){
        if((sf::st_crs(shapefile_input(shapefile$shapefile, info = FALSE)) != sf::st_crs(mosaic_data$mosaic))){
          sendSweetAlert(
            session = session,
            title = "Invalid CRS",
            text = "The Coordinate Reference System (CRS) of the shapefile does
            not match the input mosaic. Trying to set the shapefile's CRS to match the mosaic one.",
            type = "warning"
          )
        } else if(!overlaps(mosaic_data$mosaic, shapefile_input(shapefile$shapefile, as_sf = FALSE, info = FALSE))){
          sendSweetAlert(
            session = session,
            title = "Extend do not overlap",
            text = "The mosaic and shapefile extends do not overlap.",
            type = "warning"
          )
        } else{
          mosaictmp$mosaic <- terra::crop(mosaic_data$mosaic, terra::ext(shapefile_input(shapefile$shapefile, as_sf = FALSE, info = FALSE)))
        }
      } else{
        mosaictmp$mosaic <- mosaic_data$mosaic
      }
    })

    observeEvent(input$mosaicinfoindex, {
      req(mosaictmp$mosaic)
      mosaic_info(mosaictmp$mosaic)
    })
    # Creating a new option to select VIs
    observeEvent(input$imgbands, {
      # Check if imgbands is empty
      if (length(input$imgbands) == 0) {
        # Reset to default state
        updatePickerInput(session, "plotindexes",
                          choices = list(),
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE
                          ))
      } else {
        # Initialize an empty list for choices
        new_choices <- list()

        if ("RGB" %in% input$imgbands) {
          new_choices$RGB <- sort(pliman_indexes_rgb())
        }

        if ("MS" %in% input$imgbands) {
          new_choices$MULTISPECTRAL <- sort(pliman_indexes_me())
        }

        updatePickerInput(session, "plotindexes",
                          choices = new_choices,
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE
                          ))
      }
    })

    finalindex <- reactive({
      mindex <- strsplit(input$myindex, split = ",")[[1]]
      finalindex <- c(mindex, input$plotindexes)
    })

    observeEvent(input$computeindex, {
      req(mosaictmp$mosaic)
      if(is.null(mosaictmp$mosaic)){
        show_alert("Ops, an error occured.",
                   text = "You cannot compute any index without a mosaic. First use the 'Mosaic' tab to import a mosaic.",
                   type = "error")
      }
      if(length(finalindex()) == 0){
        show_alert("Ops, an error occured.",
                   text = "Select at leatest one index",
                   type = "error")
      }
      if(length(finalindex()) != 0){
        updateSelectInput(session, "indextosync", choices = finalindex())
        R <- try(mosaictmp$mosaic[[as.numeric(r$r)]], TRUE)
        G <- try(mosaictmp$mosaic[[as.numeric(g$g)]], TRUE)
        B <- try(mosaictmp$mosaic[[as.numeric(b$b)]], TRUE)
        NIR <- try(mosaictmp$mosaic[[as.numeric(nir$nir)]], TRUE)
        RE <- try(mosaictmp$mosaic[[as.numeric(re$re)]], TRUE)
        usedlayers <- pliman_indexes_eq()
        me <- pliman_indexes_me()
        nirind <- usedlayers[grep("NIR", usedlayers$Equation), 1]
        reind <- usedlayers[grep("RE", usedlayers$Equation), 1]
        if(any(finalindex() %in% nirind) & inherits(NIR, "try-error") | any(finalindex() %in% reind) & inherits(RE, "try-error")){
          show_alert("Ops, an error occured.",
                     text = "Multispectral indexes cannot be computed since needed bands are not available.",
                     type = "error")
        } else{
          # compute the indexes
          req(mosaictmp$mosaic)  # Ensure mosaictmp$mosaic is not NULL
          waiter_show(
            html = tagList(
              spin_google(),
              h2("Computing the indexes. Please, wait.")
            ),
            color = "#228B227F"
          )
          indextemp <- mosaic_index(mosaictmp$mosaic,
                                    r = as.numeric(r$r),
                                    g = as.numeric(g$g),
                                    b = as.numeric(b$b),
                                    re = as.numeric(re$re),
                                    nir = as.numeric(nir$nir),
                                    index = finalindex(),
                                    workers = input$workers,
                                    in_memory = input$inmemory,
                                    plot = FALSE)
          req(indextemp)
          updatePickerInput(session, "indextodownload",
                            choices = names(indextemp),
                            selected = names(indextemp))
          waiter_hide()
          sendSweetAlert(
            session = session,
            title = "Indexes successfully computed!!",
            text = "The vegetation indexes have been computed and are now available for further analysis.",
            type = "success"
          )
          index$index <- indextemp
          aggr <- find_aggrfact(indextemp)
          magg <- reactiveValues(agg = NULL)
          if(aggr > 0){
            magg$agg <- mosaic_aggregate(indextemp, round(100 / aggr))
          } else{
            magg$agg <- indextemp
          }
          req(magg)
          # update histslider
          observe({
            if (input$indextosync != "") {
              addPopover(
                id = "truncslider",
                options = list(
                  content = "Use the slider to truncate the raster layer to a given range. After pressing 'Truncate!', the mosaic will be updated and will be available for further analysis.",
                  title = "Truncating an index",
                  placement = "bottom",
                  trigger = "hover"
                )
              )

              tt <- magg$agg[[input$indextosync]]
              statsind <- terra::minmax(tt)
              update_histoslider(
                id = "truncslider",
                values = terra::values(tt),
                breaks = 100
              )
            }
          })
          truncated <- reactiveValues(trunc = NULL)
          wastrunc <- reactiveValues(was = 0)
          observe({
            if (input$indextosync != "") {
              if(input$truncateindex){
                tt <- magg$agg[[input$indextosync]]
                maskk <- tt > input$truncslider[[1]] & tt < input$truncslider[[2]]
                trunctemp <- terra::mask(tt,maskk, maskvalues = FALSE)
                truncated$trunc <- trunctemp
                inrange <- terra::minmax(tt)
                output$plotindextrunc <- renderPlot({
                  terra::plot(trunctemp, maxcell=100000, smooth=TRUE, range = inrange)
                })
              } else{
                output$plotindex <- renderPlot({
                  terra::plot(magg$agg[[input$indextosync]])
                })

              }
            }
          })

          observeEvent(input$truncindex, {
            indori <- index$index
            waiter_show(
              html = tagList(
                spin_google(),
                h2("Truncating the original mosaic. This may take a while for high-resolution mosaics. Please, wait.")
              ),
              color = "#228B227F"
            )
            maskori <- (indori[[input$indextosync]] > input$truncslider[[1]]) & (indori[[input$indextosync]] < input$truncslider[[2]])
            index$index <- terra::mask(indori, maskori, maskvalues = FALSE)
            updateMaterialSwitch(session,
                                 "truncateindex",
                                 value = FALSE)
            magg$agg <- truncated$trunc
            waiter_hide()
          })

          output$plotindexhist <- renderPlot({
            if (input$indextosync != "") {
              ots <- otsu(na.omit(terra::values(magg$agg[[input$indextosync]])))
              terra::density(magg$agg[[input$indextosync]],
                             main = paste0(names(magg$agg[[input$indextosync]]), " - Otsu: ", round(ots, 4)))
              abline(v = ots,
                     col = "red",
                     lty = 2,
              )
            }
          })
          output$indexsync <- renderUI({
            req(basemap$map)
            if (input$indextosync != "") {
              leafsync::sync(basemap$map@map, mosaic_view(magg$agg[[input$indextosync]],
                                                          index = input$indextosync))
            }
          })
          output$indexshp <- renderLeaflet({
            if (input$indextosync != "") {
              if(input$shapefiletoplot != "none"){
                indp <-  terra::mask(magg$agg[[input$indextosync]],
                                     shapefile[[input$shapefiletoplot]]$data |> shapefile_input(as_sf = FALSE, info = FALSE))
                # mosaic_view(indp)@map
                (basemap$map + mosaic_view(indp,
                                           max_pixels = 3000000,
                                           downsample_fun = "average",
                                           show = "index",
                                           color_regions = return_colors(input$palplotindex, reverse = input$palplotindexrev)))@map
              }
            }
          })
          indextodownload <- input$indextodownload
          mod_download_mosaic_server("download_indexes", indextemp[[input$indextodownload]], "indexes")

        }
      }
    })
  })
}

## To be copied in the UI
# mod_indexes_ui("indexes_1")

## To be copied in the server
# mod_indexes_server("indexes_1")
