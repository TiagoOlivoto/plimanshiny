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
      col_4(
        bs4TabCard(
          id = "tabsindex",
          width = 12,
          status = "success",
          title = "Vegetation indexes",
          selected = "Build",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Build",
            fluidRow(
              col_3(
                actionButton(
                  inputId = ns("guideindex"),
                  label = tagList(
                    icon = icon("question-circle", verify_fa = FALSE), "Guide"
                  ),
                  style = "color: white ; background-color: #dd4b39",
                  class = "btn-danger"
                )
              ),
              col_4(
                actionButton(
                  inputId = ns("mosaicinfoindex"),
                  label = tagList(
                    icon = icon("circle-info", verify_fa = FALSE), "Mosaic Info"
                  ),
                  status = "info"
                )
              ),
              col_5(
                actionButton(
                  inputId = ns("availableindexes"),
                  label = tagList(
                    icon = icon("list-check", verify_fa = FALSE), "Available indexes"
                  ),
                  status = "info"
                )
              )
            ),
            width = 12,
            status = "success",
            hl(),
            fluidRow(
              col_6(
                selectInput(ns("rastertocompute"),
                            label = "Raster",
                            choices = NULL)
              ),
              col_6(
                selectInput(ns("shapefiletoplot"),
                            label = "Shapefile",
                            choices = NULL)
              )
            ),
            pickerInput(
              inputId = ns("imgbands"),
              label = "Image Bands",
              choices = c("RGB", "MS"),
              selected = "RGB",
              multiple = TRUE
            ),
            fluidRow(
              col_8(
                pickerInput(
                  inputId = ns("plotindexes"),
                  label = "Vegetation indexes",
                  choices = "",
                  multiple = TRUE
                ),
              ),
              col_4(
                shiny::actionButton(inputId= ns("indexhelp"),
                                    label="Indexes' equations",
                                    icon = icon("square-root-variable"))
              )
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
            fluidRow(
              col_6(
                numericInput(ns("workers"),
                             label = "Number of workers",
                             value = 1)
              ),
              col_6(
                textInput(ns("storeas"),
                          label = "Save index as...",
                          value = "index")
              )
            ),
            actionBttn(
              inputId = ns("computeindex"),
              label = "Compute the indexes!",
              style = "pill",
              color = "primary",
              icon = icon("chart-simple")
            ),
            hl(),
            fluidRow(
              col_6(
                selectInput(ns("activeindex"),
                            label = "Active index",
                            choices = NULL),
              ),
              col_6(
                selectInput(ns("indextosync"),
                            label = "Index to sync",
                            choices = NULL)
              )
            )
          ),
          tabPanel(
            title = "Export",
            pickerInput(
              inputId = ns("indextodownload"),
              label = "Index to download",
              choices = "",
              multiple = TRUE
            ),
            mod_download_mosaic_ui(ns("download_indexes"), "Index")

          )
        )
      ),
      col_8(
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
            fluidRow(
              col_4(
                materialSwitch(
                  inputId = ns("truncateindex"),
                  label = "Truncate index?",
                  value = FALSE,
                  status = "success"
                )
              ),
              col_6(
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
              plotOutput(ns("plotindex"), height = "640px") |> add_spinner()
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
          ),
          tabPanel(
            "Index profile",
            fluidRow(
              col_6(
                actionBttn(
                  inputId = ns("startprofile"),
                  label = "Start profiling",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              ),
              col_6(
                actionBttn(
                  inputId = ns("createprofile"),
                  label = "Create profile",
                  style = "pill",
                  color = "success",
                  icon = icon("edit")
                )
              )
            ),
            editModUI(ns("indexprofile"), height = "700px") |> add_spinner()
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
mod_indexes_server <- function(id, mosaic_data, r, g, b, re, nir, swir, tir, basemap, index, shapefile){
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
      nam <- c("none", setdiff(names(shapefile), c("shapefile", "shapefileplot")))
      updateSelectInput(session, "shapefiletoplot",
                        choices = nam,
                        selected = nam[length(nam)])
    })
    observe({
      req(mosaic_data)
      namesmos <- setdiff(names(mosaic_data), "mosaic")
      updateSelectizeInput(session, "rastertocompute",
                           choices = namesmos,
                           selected = namesmos[length(namesmos)])
    })

    observe({
      req(input$rastertocompute)
      req(input$shapefiletoplot)
      if(input$shapefiletoplot != "none" && overlaps(mosaic_data[[input$rastertocompute]]$data, terra::vect(shapefile[[input$shapefiletoplot]]$data))){
        req(mosaic_data[[input$rastertocompute]]$data)
        if((sf::st_crs(shapefile_input(shapefile[[input$shapefiletoplot]]$data, info = FALSE)) != sf::st_crs(mosaic_data[[input$rastertocompute]]$data))){
          sendSweetAlert(
            session = session,
            title = "Invalid CRS",
            text = "The Coordinate Reference System (CRS) of the shapefile does
            not match the input mosaic. Trying to set the shapefile's CRS to match the mosaic one.",
            type = "warning"
          )
        }
        mosaictmp$mosaic <- terra::crop(mosaic_data[[input$rastertocompute]]$data, terra::ext(shapefile_input(shapefile[[input$shapefiletoplot]]$data, as_sf = FALSE, info = FALSE)))
      } else{
        mosaictmp$mosaic <- mosaic_data[[input$rastertocompute]]$data
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
        R <- try(mosaictmp$mosaic[[suppressWarnings(as.numeric(r$r))]], TRUE)
        G <- try(mosaictmp$mosaic[[suppressWarnings(as.numeric(g$g))]], TRUE)
        B <- try(mosaictmp$mosaic[[suppressWarnings(as.numeric(b$b))]], TRUE)
        NIR <- try(mosaictmp$mosaic[[suppressWarnings(as.numeric(nir$nir))]], TRUE)
        RE <- try(mosaictmp$mosaic[[suppressWarnings(as.numeric(re$re))]], TRUE)
        SWIR <- try(mosaictmp$mosaic[[suppressWarnings(as.numeric(swir$swir))]], TRUE)
        TIR <- try(mosaictmp$mosaic[[suppressWarnings(as.numeric(tir$tir))]], TRUE)
        usedlayers <- pliman_indexes_eq()
        me <- pliman_indexes_me()
        nirind <- usedlayers[grep("NIR", usedlayers$Equation), 1]
        reind <- usedlayers[grep("RE", usedlayers$Equation), 1]
        swirind <- usedlayers[grep("SWIR", usedlayers$Equation), 1]
        tirind <- usedlayers[grep("TIR", usedlayers$Equation), 1]
        if(any(finalindex() %in% nirind) & inherits(NIR, "try-error") |
           any(finalindex() %in% reind) & inherits(RE, "try-error") |
           any(finalindex() %in% swirind) & inherits(SWIR, "try-error") |
           any(finalindex() %in% tirind) & inherits(TIR, "try-error")){
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
                                    r = suppressWarnings(as.numeric(r$r)),
                                    g = suppressWarnings(as.numeric(g$g)),
                                    b = suppressWarnings(as.numeric(b$b)),
                                    re = suppressWarnings(as.numeric(re$re)),
                                    nir = suppressWarnings(as.numeric(nir$nir)),
                                    swir = suppressWarnings(as.numeric(swir$swir)),
                                    tir = suppressWarnings(as.numeric(tir$tir)),
                                    index = finalindex(),
                                    workers = input$workers,
                                    in_memory = input$inmemory,
                                    plot = FALSE)
          req(indextemp)
          waiter_hide()
          sendSweetAlert(
            session = session,
            title = "Indexes successfully computed!!",
            text = "The vegetation indexes have been computed and are now available for further analysis.",
            type = "success"
          )
          index[[input$storeas]] <- create_reactval(input$storeas, indextemp)

        }
      }
    })


    observe({
      updateSelectInput(session, "activeindex",
                        choices = names(index))
    })

    # Update magg based on selected index
    magg <- reactiveValues(agg = NULL)
    observe({
      req(input$activeindex, "Active index is required.")
      aggr <- find_aggrfact(index[[input$activeindex]]$data)
      if(aggr > 0){
        magg$agg <- mosaic_aggregate(index[[input$activeindex]]$data, round(100 / aggr))
      } else{
        magg$agg <- index[[input$activeindex]]$data
      }
      updateSelectInput(session, "indextosync", choices = names(magg$agg), selected = names(magg$agg)[1])
    })

    truncated <- reactiveValues(trunc = NULL)
    wastrunc <- reactiveValues(was = 0)
    tt <- reactiveValues(tt = NULL)
    # Render plot based on the selected index to sync
    # Update histoslider when indextosync changes
    observeEvent(input$indextosync, {
      req(magg$agg)
      req(input$indextosync %in% names(magg$agg))
      tt$tt <- magg$agg[[input$indextosync]]
      update_histoslider("truncslider", values = terra::values(tt$tt), breaks = 100)
    })

    # Plot the main index based on the selected indextosync
    observe({
      if(!input$truncateindex){
        if(input$indextosync %in% names(magg$agg)){
          output$plotindex <- renderPlot({
            terra::plot(magg$agg[[input$indextosync]],
                        col = return_colors(input$palplotindex, reverse = input$palplotindexrev, n = 100),
                        smooth = TRUE
            )
          })
        }
      }
    })

    # Plot the truncated index based on slider values
    observe({
      if(input$truncateindex){
        req(input$indextosync %in% names(magg$agg), "Selected index not found in data.")
        tt <- magg$agg[[input$indextosync]]
        maskk <- tt > input$truncslider[[1]] & tt < input$truncslider[[2]]
        truncated$trunc <- terra::mask(tt, maskk, maskvalues = FALSE)
        output$plotindextrunc <- renderPlot({
          terra::plot(truncated$trunc,
                      col = return_colors(input$palplotindex, reverse = input$palplotindexrev, n = 100),
                      smooth = TRUE)
        })
      }
    })

    observeEvent(input$truncindex, {
      indori <- index[[input$activeindex]]$data
      waiter_show(
        html = tagList(
          spin_google(),
          h2("Truncating the original mosaic. This may take a while for high-resolution mosaics. Please, wait.")
        ),
        color = "#228B227F"
      )
      maskori <- (indori[[input$indextosync]] > input$truncslider[[1]]) & (indori[[input$indextosync]] < input$truncslider[[2]])
      index[[input$activeindex]] <- create_reactval(input$activeindex, terra::mask(indori, maskori, maskvalues = FALSE))
      updateMaterialSwitch(session,
                           "truncateindex",
                           value = FALSE)
      magg$agg <- truncated$trunc
      waiter_hide()
    })


    output$plotindexhist <- renderPlot({
      if(input$indextosync %in% names(magg$agg)){
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
      if(input$indextosync %in% names(magg$agg)){
        req(basemap$map)
        leafsync::sync(basemap$map@map, mosaic_view(magg$agg[[input$indextosync]],
                                                    index = input$indextosync,
                                                    color_regions = return_colors(input$palplotindex, reverse = input$palplotindexrev)))
      }
    })

    # Index profile
    observeEvent(input$startprofile, {
      req(index[[input$activeindex]]$data)
      drawn <- reactiveValues()
      cpoints <- callModule(editMod, "indexprofile",
                            leafmap = basemap$map@map,
                            editor = "leafpm")
      observeEvent(c(cpoints()$finished, cpoints()$edited), {
        if(!is.null(cpoints()$finished)){
          drawn$finished <- cpoints()$finished |> dplyr::slice_tail(n = 1)
        }
        if(!is.null(cpoints()$edited) & !is.null(cpoints()$finished)){
          idedit <- cpoints()$edited |> dplyr::slice_tail(n = 1) |> dplyr::pull(edit_id)
          drawnedit <- cpoints()$finished |> dplyr::slice_tail(n = 1) |> dplyr::pull(edit_id)
          if(idedit == drawnedit){
            drawn$finished <- cpoints()$edited |> dplyr::slice_tail(n = 1)
          }
        }
        observeEvent(input$createprofile, {

          on.exit(layout(1))
          if(!is.null(drawn$finished)){
            output$plotinfop <- renderPlot({

              nlyrs <- terra::nlyr(mosaictmp$mosaic)
              polygons <- drawn$finished$geometry
              polygons_spv <-
                sf::st_transform(polygons, crs = sf::st_crs(mosaictmp$mosaic))


              if(inherits(polygons, "sfc_LINESTRING")){
                coords <-
                  polygons_spv |>
                  sf::st_coordinates()
                coordsext <- terra::vect(coords[, 1:2], "lines")
                exts <-
                  terra::vect(sf::st_transform(polygons, crs = sf::st_crs(mosaictmp$mosaic))) |>
                  terra::buffer(input$buffer) |>
                  terra::ext()
                mosaiccr <- terra::crop(mosaictmp$mosaic, exts)
                indexccr <- terra::crop(index[[input$activeindex]]$data, exts)
                polygons_ext <-  terra::vect(polygons_spv)
                vals <- terra::extractAlong(indexccr, coordsext, xy = TRUE)

                observe({
                  updatePickerInput(session, "indextoprofile",
                                    choices = names(vals)[4:ncol(vals)],
                                    selected = names(vals)[4:ncol(vals)][1])
                })

                coordsdist <- as.matrix(polygons_spv |> sf::st_coordinates())
                n <- nrow(coordsdist)
                distances <- NULL
                for (j in 1:(n - 1)) {
                  x1 <- coordsdist[j, 1]
                  y1 <- coordsdist[j, 2]
                  x2 <- coordsdist[j + 1, 1]
                  y2 <- coordsdist[j + 1, 2]
                  distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
                  distances[j] <- distance
                }
                # distances
                dists <- cumsum(distances)
                dist <- max(dists)
                if(nlyrs > 2){
                  layout(
                    matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE),
                    heights = c(3, 3)
                  )
                  if(input$stretch == "none"){
                    terra::plotRGB(
                      mosaiccr,
                      r = suppressWarnings(as.numeric(r$r)),
                      g = suppressWarnings(as.numeric(g$g)),
                      b = suppressWarnings(as.numeric(b$b)),
                      maxcell = 1e6,
                      colNA = "#00000000",
                      mar = c(2, 2, 2, 2),
                      axes = FALSE
                    )

                  } else{
                    terra::plotRGB(
                      mosaiccr,
                      r = suppressWarnings(as.numeric(r$r)),
                      g = suppressWarnings(as.numeric(g$g)),
                      b = suppressWarnings(as.numeric(b$b)),
                      stretch = input$stretch,
                      maxcell = 1e6,
                      colNA = "#00000000",
                      mar = c(2, 2, 2, 2),
                      axes = FALSE
                    )
                  }
                  terra::plot(polygons_ext, add = TRUE, col = "red")
                  text(coords[1, 1], coords[1, 2], "Start", cex = 1.5, col = "white")
                  text(coords[n, 1], coords[n, 2], "End", cex = 1.5, col = "white")

                  terra::plot(indexccr[[input$indextosync]],
                              axes = FALSE,
                              maxcell=5000000,
                              mar = c(2, 2, 2, 2),
                              col = return_colors(input$plaindex, reverse = input$revindex, n = 100),
                              smooth=TRUE)
                  terra::plot(polygons_ext, add = TRUE, col = "red")
                  text(coords[1, 1], coords[1, 2], 0, cex = 1.5)
                  text(coords[n, 1], coords[n, 2], round(max(dist), 2), cex = 1.5)

                  # Transform the data to long format
                  req(input$indextoprofile)
                  data_long <-
                    data.frame(x = seq(0, dist, length.out = nrow(vals))) |>
                    dplyr::bind_cols(vals |> dplyr::select(dplyr::all_of(input$indextoprofile)))

                  if(input$smooth){
                    data_long <-
                      data_long |>
                      dplyr::mutate(dplyr::across(2:length(input$indextoprofile), ~ {
                        fit <- smooth.spline(data_long$x, .x)
                        predict(fit, data_long$x)$y
                      }))
                  }


                  colorslin <- random_color(length(input$indextoprofile))
                  matplot(data_long[, 1],
                          data_long[, -1],
                          type = "l",
                          lty = 1,
                          col = colorslin,
                          ylab = "Vegetation indexes",
                          xlab = "Distance",
                          cex = 3,
                          lwd = 1.5,
                          xlim = c(0, dist))
                  legend("topright",
                         legend = input$indextoprofile,
                         col = colorslin,
                         lty = 1)

                } else{
                  layout(
                    matrix(c(1, 2), nrow = 2, byrow = TRUE),
                    heights = c(3, 3)
                  )
                  terra::plot(indexccr[[input$indextosync]],
                              axes = FALSE,
                              maxcell=5000000,
                              mar = c(2, 2, 2, 2),
                              col = return_colors(input$plaindex, reverse = input$revindex, n = 100),
                              smooth=TRUE)
                  lines(coords,
                        col = "red",
                        lwd = 3)

                  req(input$indextoprofile)
                  data_long <-
                    data.frame(x = seq(0, dist, length.out = nrow(vals))) |>
                    dplyr::bind_cols(vals |> dplyr::select(dplyr::all_of(input$indextoprofile)))

                  if(input$smooth){
                    data_long <-
                      data_long |>
                      dplyr::mutate(dplyr::across(2:length(input$indextoprofile), ~ {
                        fit <- smooth.spline(data_long$x, .x)
                        predict(fit, data_long$x)$y
                      }))
                  }

                  colorslin <- random_color(length(input$indextoprofile))
                  matplot(data_long[, 1],
                          data_long[, -1],
                          type = "l",
                          lty = 1,
                          col = colorslin,
                          ylab = "Vegetation indexes",
                          xlab = "Distance",
                          cex = 1.5,
                          lwd = 1.5,
                          xlim = c(0, dist))
                  legend("topright",
                         legend = input$indextoprofile,
                         col = colorslin,
                         lty = 1)

                }

                # Download handler to generate the CSV file
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste("data-", Sys.Date(), ".csv", sep="")
                  },
                  content = function(file) {
                    # Generate a sample data frame
                    data <- data_long
                    # Write the data frame to a CSV file
                    write.csv(data, file, row.names = FALSE)
                  }
                )

              }

            })
            showModal(
              modalDialog(
                title = "Index Profile",
                fluidRow(
                  col_2(
                    pickerInput(
                      inputId = ns("indextoprofile"),
                      label = "Vegetation index(es)",
                      choices = NULL,
                      options = list(
                        `actions-box` = TRUE
                      ),
                      multiple = TRUE
                    )
                  ),
                  col_2(
                    selectInput(
                      ns("stretch"),
                      label = "Stretch",
                      choices = c("none", "lin", "hist"),
                      selected = "lin"
                    )
                  ),
                  col_4(
                    pickerpalette(id, "plaindex", selected = "RdYlGn"),
                  ),
                  col_1(
                    prettyCheckbox(
                      inputId = ns("revindex"),
                      label = "Reverse",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_1(
                    prettyCheckbox(
                      inputId = ns("smooth"),
                      label = "Smooth",
                      value = TRUE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_2(
                    numericInput(ns("buffer"),
                                 label = "Buffer (area)",
                                 value = 2)
                  )
                ),
                fluidRow(
                  plotOutput(ns("plotinfop"), height = "600px")
                ),
                downloadButton(ns("downloadData"), "Download profile"),
                footer = NULL,
                easyClose = TRUE,
                size = "xl"
              )
            )
          }
        })
      })
    })



    output$indexshp <- renderLeaflet({
      if (input$indextosync  %in% names(magg$agg)) {
        if(input$shapefiletoplot != "none"){
          indp <-  terra::mask(magg$agg[[input$indextosync]],
                               shapefile[[input$shapefiletoplot]]$data |> shapefile_input(as_sf = FALSE, info = FALSE))
          (basemap$map + mosaic_view(indp,
                                     max_pixels = 3000000,
                                     downsample_fun = "average",
                                     show = "index",
                                     color_regions = return_colors(input$palplotindex, reverse = input$palplotindexrev)))@map
        }
      }
    })

    observe({
      observe({
        req(input$activeindex)
        updatePickerInput(session, "indextodownload",
                          choices = names(index[[input$activeindex]]$data),
                          selected = names(index[[input$activeindex]]$data))
      })
      mod_download_mosaic_server("download_indexes", index[[input$activeindex]]$data[[input$indextodownload]], "indexes")
    })


    # available indexes
    observeEvent(input$availableindexes, {
      output$tabavailableind <- renderReactable({
        render_reactable(pliman_indexes_ican_compute(input$listofbands),
                         defaultPageSize = 10,
                         columns = list(
                           Index = colDef(maxWidth = 100),
                           Equation = colDef(maxWidth = 850),
                           Band = colDef(maxWidth = 100)
                         ))
      })
      output$numindexes <- renderValueBox({
        valueBox(
          value = tags$p(nrow(pliman_indexes_ican_compute(input$listofbands)), style = "font-size: 200%;"),
          subtitle = "Number of available indexes",
          color = "success",
          icon = icon("arrow-down-1-9")
        )
      })

      showModal(
        modalDialog(
          title = "Search the built-in available indexes based on the available bands",
          fluidRow(
            col_6(
              selectizeInput(
                inputId = ns("listofbands"),
                label = "Selected the available bands",
                choices = c("R", "G", "B", "RE", "NIR", "SWIR", "TIR"),
                multiple=TRUE,
                options = list('plugins' = list('remove_button'),
                               'create' = TRUE,
                               'persist'= FALSE)
              ),
            ),
            col_6(
              valueBoxOutput(ns("numindexes"), width = 8),
            )
          ),
          reactable::reactableOutput(ns("tabavailableind")),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )
    })

    # index equation
    observeEvent(input$indexhelp, {

      output$tabavailableind <- renderReactable({
        render_reactable(pliman_indexes_eq(),
                         defaultPageSize = 10,
                         columns = list(
                           Index = colDef(maxWidth = 100),
                           Equation = colDef(maxWidth = 850),
                           Band = colDef(maxWidth = 100)
                         ))
      })

      showModal(
        modalDialog(
          title = "The equations for the available built-in indexes",
          shiny::actionButton(inputId= ns("equation"),
                              label="References",
                              icon = icon("lightbulb"),
                              onclick ="window.open('https://tiagoolivoto.github.io/pliman/articles/indexes.html', '_blank')"),
          reactable::reactableOutput(ns("tabavailableind")),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )
    })


  })
}

## To be copied in the UI
# mod_indexes_ui("indexes_1")

## To be copied in the server
# mod_indexes_server("indexes_1")
