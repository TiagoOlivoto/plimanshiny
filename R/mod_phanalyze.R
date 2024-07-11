#' phanalyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_phanalyze_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Canopy Height Models and more...",
          collapsible = FALSE,
          width = 12,
          icon = icon("mountain-sun"),
          height = "790px",
          prettyRadioButtons(
            inputId = ns("strategy"),
            label = "Strategy",
            choices = c("I have DSM and DTM", "Build DTM using sampling points", "Build DTM using a moving window"),
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly"
          ),
          pickerInput(
            ns("dsm"),
            label = "Digital Surface Model",
            choices = NULL
          ),
          conditionalPanel(
            condition = "input.strategy == 'I have DSM and DTM'", ns = ns,
            pickerInput(
              ns("dtm"),
              label = "Digital Terrain Model",
              choices = NULL
            )
          ),
          conditionalPanel(
            condition = "input.strategy == 'Build DTM using a moving window'", ns = ns,
            textInput(
              ns("windowsize"),
              label = "Window size (meters)",
              value = c("5,5")
            )
          ),
          conditionalPanel(
            condition = "input.strategy != 'I have DSM and DTM'", ns = ns,
            prettyRadioButtons(
              inputId = ns("interpmethod"),
              label = "Interpolation method",
              choices = c("Thin Plate Spline", "Kriging"),
              icon = icon("check"),
              bigger = TRUE,
              status = "info",
              animation = "jelly"
            )
          ),
          pickerInput(
            ns("shapefile"),
            label = "Shapefile",
            choices = NULL
          ),
          pickerInput(
            ns("maskfile"),
            label = "Mask (optional)",
            choices = NULL
          ),
          conditionalPanel(
            condition = "input.maskfile != 'none'", ns = ns,
            prettyCheckbox(
              inputId = ns("masksoil"),
              label = "Mask shows soil?",
              value = FALSE,
            )
          ),
          textInput(
            ns("savedfas"),
            label = "Save Results as...",
            value = "canopy_height_model",
          ),
          actionBttn(
            ns("computeph"),
            label = "Compute Canopy Height Model!",
            icon = icon("check")
          )
        )
      ),
      col_9(
        uiOutput(ns("resultphmodel"))
      )
    )
  )
}

#' phanalyze Server Functions
#'
#' @noRd
mod_phanalyze_server <- function(id, mosaic_data, shapefile, basemap, dfs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$resultphmodel <- renderUI({
      if(input$strategy == "I have DSM and DTM"){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Explore the field",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Explore the field",
            fluidRow(
              col_2(
                selectInput(ns("basemapplot"),
                            label = "Basemap",
                            choices = c("basemap", "DSM", "DTM", "CHM"))
              ),
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "RdYlGn"),
              ),
              col_1(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_3(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "640px")  |> add_spinner()
          ),

          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            "Plant Height Profile",
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
            editModUI(ns("indexprofile"), height = "660px") |> add_spinner()
          )
        )
      } else if(input$strategy == "Build DTM using a moving window"){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Explore the field",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Explore the field",
            fluidRow(
              col_2(
                selectInput(ns("basemapplot"),
                            label = "Basemap",
                            choices = c("basemap", "DSM", "DTM", "CHM"))
              ),
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "RdYlGn"),
              ),
              col_1(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_3(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Sampled Points",
            leafletOutput(ns("sampledsf"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Digital Terrain Model",
            plotOutput(ns("dtmplot"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            "Plant Height Profile",
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
            editModUI(ns("indexprofile"), height = "660px") |> add_spinner()
          )
        )
      } else {
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Interpolation",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Interpolation",
            actionBttn(
              inputId = ns("interpolate"),
              label = "Start sampling",
              style = "pill",
              color = "success",
              icon = icon("edit")
            ),
            editModUI(ns("pointpicksample"), height = "740px") |> add_spinner()

          ),
          tabPanel(
            title = "Explore the field",
            fluidRow(
              col_2(
                selectInput(ns("basemapplot"),
                            label = "Basemap",
                            choices = c("basemap", "DSM", "DTM", "CHM"))
              ),
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "RdYlGn"),
              ),
              col_1(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_3(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "640px")  |> add_spinner()
          ),
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
          ),
          tabPanel(
            "Plant Height Profile",
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
            editModUI(ns("indexprofile"), height = "660px") |> add_spinner()
          )
        )
      }
    })

    # update selec input
    observe({
      updatePickerInput(session, "dsm", choices = c(setdiff(names(mosaic_data), "mosaic")))
    })
    observe({
      updatePickerInput(session, "dtm", choices = c(setdiff(names(mosaic_data), "mosaic")))
    })
    observe({
      updatePickerInput(session, "maskfile", choices = c("none", setdiff(names(mosaic_data), "mosaic")),
                        selected = "none")
    })
    observe({
      updatePickerInput(session, "shapefile",
                        choices = c(setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
    })

    bmchm <- reactiveValues(map = NULL)
    chmreact <- reactiveValues(rast = NULL)
    dfres <- reactiveValues(df = NULL)
    sampledpoints <- reactiveValues(sf = NULL)

    observeEvent(input$computeph, {
      if(input$strategy == "I have DSM and DTM"){
        req(input$dsm, input$dtm)
        dsm <- mosaic_data[[input$dsm]]$data
        dtm <- mosaic_data[[input$dtm]]$data
        ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
        ch2 <- !inherits(dtm,"SpatRaster") || !terra::nlyr(dtm) == 1 || terra::is.bool(dtm) || is.list(dtm)

        if (ch1 | ch2) {
          sendSweetAlert(
            session = session,
            title = "Invalid file format",
            text = paste("DSM and DTM must be an SpatRaster object with one layer."),
            type = "error"
          )
          return()
        } else{

          waiter_show(
            html = tagList(
              spin_google(),
              h2("Building the Canopy Height Model. Please, wait.")
            ),
            color = "#228B227F"
          )

          if(input$maskfile == "none"){
            chmres <- mosaic_chm(dsm, dtm,
                                 verbose = FALSE)
          } else{
            chmres <- mosaic_chm(dsm = dsm,
                                 dtm = dtm,
                                 mask = mosaic_data[[input$maskfile]]$data,
                                 mask_soil = input$masksoil,
                                 verbose = FALSE)
          }
          chmreact$rast <- chmres$chm
        }
      }

      if(input$strategy == "Build DTM using sampling points"){
        req(input$dsm, input$shapefile)
        dsm <- mosaic_data[[input$dsm]]$data
        ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
        if (ch1) {
          sendSweetAlert(
            session = session,
            title = "Invalid file format",
            text = paste("DSM and DTM must be an SpatRaster object with one layer."),
            type = "error"
          )
          return()
        } else{
          mapsampl <- mosaic_view(dsm,
                                  alpha = 1,
                                  show = "index")
          cpoints <- callModule(editMod, "pointpicksample", mapsampl@map , editor = "leafpm")
          observeEvent(input$interpolate, {
            observeEvent(cpoints()$finished, {
              if(!is.null(cpoints()$edited)){
                cpo <- cpoints()$edited |> sf::st_transform(sf::st_crs(dsm))
              } else{
                cpo <- cpoints()$finished |> sf::st_transform(sf::st_crs(dsm))
              }
              req(cpo)

              waiter_show(
                html = tagList(
                  spin_google(),
                  h2("Building the Canopy Height Model. Please, wait.")
                ),
                color = "#228B227F"
              )

              if(input$maskfile == "none"){
                chmres <- mosaic_chm(dsm, points = cpo,
                                     verbose = FALSE)
              } else{
                chmres <- mosaic_chm(dsm = dsm,
                                     points = cpo,
                                     mask = mosaic_data[[input$maskfile]]$data,
                                     mask_soil = input$masksoil,
                                     verbose = FALSE)
              }
              chmreact$rast <- chmres$chm
            })
          })
        }
      }

      if(input$strategy == "Build DTM using a moving window"){
        req(input$dsm, input$windowsize, input$shapefile)
        dsm <- mosaic_data[[input$dsm]]$data
        ch1 <- !inherits(dsm,"SpatRaster") || !terra::nlyr(dsm) == 1 || terra::is.bool(dsm) || is.list(dsm)
        if (ch1) {
          sendSweetAlert(
            session = session,
            title = "Invalid file format",
            text = paste("DSM and DTM must be an SpatRaster object with one layer."),
            type = "error"
          )
          return()
        } else{
          waiter_show(
            html = tagList(
              spin_google(),
              h2("Building the Canopy Height Model. Please, wait.")
            ),
            color = "#228B227F"
          )

          if(input$maskfile == "none"){
            chmres <- mosaic_chm(dsm,
                                 window_size = input$windowsize |> chrv2numv(),
                                 verbose = FALSE)
          } else{
            chmres <- mosaic_chm(dsm = dsm,
                                 window_size = input$windowsize |> chrv2numv(),
                                 mask = mosaic_data[[input$maskfile]]$data,
                                 mask_soil = input$masksoil,
                                 verbose = FALSE)
          }
          chmreact$rast <- chmres$chm
          sampledpoints$sf <- chmres$sampling_points
          output$dtmplot <- renderPlot({
            terra::plot(chmres$chm[[1]], col = terrain.colors(100))
          })

        }
      }
      req(shapefile[[input$shapefile]]$data)
      req(chmreact$rast)
      req(input$basemapplot)

      dftmp <- mosaic_chm_extract(chmres, shapefile[[input$shapefile]]$data)
      dfres$df <- dftmp
      updateSelectInput(session, "plotattribute",
                        choices = colnames(dftmp),
                        selected = "q90")

      # store the results in dataset module
      dfs[[input$savedfas]] <- create_reactval(input$savedfas, dftmp |> sf::st_drop_geometry())
      shapefile[[input$savedfas]] <- create_reactval(input$savedfas, dftmp)

    })

    # plot the results
    observe({
      req(chmreact$rast)
      if(input$basemapplot == "basemap"){
        bmchm$map <- basemap$map
      } else if(input$basemapplot == "DSM"){
        req(chmreact$rast)
        bmchm$map <- mosaic_view(mosaic_data[[input$dsm]]$data,
                                 color_regions = return_colors("PlantSoil", reverse = TRUE),
                                 alpha = 1,
                                 show = "index")
      } else if(input$basemapplot == "DTM"){
        req(chmreact$rast)
        bmchm$map <- mosaic_view(chmreact$rast[[1]],
                                 color_regions = return_colors("PlantSoil", reverse = TRUE),
                                 alpha = 1,
                                 show = "index")
      } else if(input$basemapplot == "CHM"){
        req(chmreact$rast)
        bmchm$map <- mosaic_view(chmreact$rast[[2]],
                                 color_regions = return_colors("PlantSoil", reverse = TRUE),
                                 alpha = 1,
                                 show = "index")
      }
      waiter_hide()
      sendSweetAlert(
        session = session,
        title = "Canopy Height Model computed.",
        text = "The Canopy Height Model computed has been computed and the results are now available for download in the 'Datasets > Input' menu.",
        type = "success"
      )

    })

    output$sampledsf <- renderLeaflet({
      if(input$strategy == "Build DTM using a moving window"){
        req(sampledpoints$sf)
        (bmchm$map + shapefile_view(sampledpoints$sf, attribute = "dtm"))@map
      }
    })

    output$resultsplotmap <- renderLeaflet({
      req(dfres$df)
      req(input$plotattribute)
      (bmchm$map + shapefile_view(dfres$df,
                                  attribute = input$plotattribute,
                                  color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                                  alpha.regions = input$alpharesplot))@map
    })

    output$overview <- renderPlotly({
      req(dfres$df)
      dfhist <-
        dfres$df |>
        sf::st_drop_geometry() |>
        dplyr::select(q90, volume) |>
        tidyr::pivot_longer(dplyr::everything())
      p <-
        ggplot(dfhist, aes(x = value)) +
        geom_histogram(position = "identity",
                       fill = "forestgreen") +
        facet_wrap(~name, scales = "free") +
        labs(x = "Observed value",
             y = "Number of plots") +
        theme_bw(base_size = 18) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "bottom")
      plotly::ggplotly(p)
    })

    output$resultplottab <- renderReactable({
      req(dfres$df)
      dfres$df |>
        sf::st_drop_geometry() |>
        roundcols(digits = 3) |>
        render_reactable()

    })
    # send to dataset module




    # Plant Height Profile
    # Index profile
    observeEvent(input$startprofile, {
      req(chmreact$rast)
      drawn <- reactiveValues()
      cpoints <- callModule(editMod, "indexprofile",
                            leafmap = bmchm$map@map,
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
              polygons <- drawn$finished$geometry
              polygons_spv <-
                sf::st_transform(polygons, crs = sf::st_crs(chmreact$rast[[input$whatprofile]]))


              if(inherits(polygons, "sfc_LINESTRING")){
                coords <-
                  polygons_spv |>
                  sf::st_coordinates()
                coordsext <- terra::vect(coords[, 1:2], "lines")
                exts <-
                  terra::vect(sf::st_transform(polygons, crs = sf::st_crs(chmreact$rast[[input$whatprofile]]))) |>
                  terra::buffer(input$buffer) |>
                  terra::ext()
                indexccr <- terra::crop(chmreact$rast[[input$whatprofile]], exts)
                polygons_ext <-  terra::vect(polygons_spv)
                vals <- terra::extractAlong(indexccr, coordsext, xy = TRUE)

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
                layout(
                  matrix(c(1, 2), nrow = 2, byrow = TRUE),
                  heights = c(3, 3)
                )
                if(input$whatplot == "raster"){
                  terra::plot(indexccr,
                              axes = FALSE,
                              maxcell=5000000,
                              mar = c(2, 2, 2, 2),
                              col = return_colors(input$plaindex, reverse = input$revindex, n = 100),
                              smooth=TRUE)
                } else{
                  mosaic_plot_rgb(terra::crop(mosaic_data$mosaic, exts))
                }
                lines(coords,
                      col = "red",
                      lwd = 3)

                data_long <-
                  data.frame(x = seq(0, dist, length.out = nrow(vals))) |>
                  dplyr::bind_cols(vals |> dplyr::select(dplyr::all_of(input$whatprofile))) |>
                  dplyr::filter(!is.na(!!dplyr::sym(input$whatprofile)))
                if(input$smooth){
                  data_long <-
                    data_long |>
                    dplyr::mutate(dplyr::across(2, ~ {
                      fit <- smooth.spline(data_long$x, .x)
                      predict(fit, data_long$x)$y
                    }))
                }

                matplot(data_long[, 1],
                        data_long[, -1],
                        type = "l",
                        lty = 1,
                        col = "red",
                        ylab = input$whatprofile,
                        xlab = "Distance",
                        cex = 1.5,
                        lwd = 1.5,
                        xlim = c(0, dist))
                legend("topright",
                       legend = input$whatprofile,
                       col = "red",
                       lty = 1)


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
                title = "Canopy Height Profile",
                fluidRow(
                  col_3(
                    pickerpalette(id, "plaindex", selected = "PlantSoil"),
                  ),
                  col_3(
                    prettyRadioButtons(
                      inputId = ns("whatprofile"),
                      label = "Create profile for...",
                      choices = c("dtm", "height", "volume"),
                      selected = "height",
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_3(
                    prettyRadioButtons(
                      inputId = ns("whatplot"),
                      label = "Plot to show...",
                      choices = c("basemap", "raster"),
                      selected = "raster",
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_2(
                    prettyCheckbox(
                      inputId = ns("revindex"),
                      label = "Reverse",
                      value = TRUE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    prettyCheckbox(
                      inputId = ns("smooth"),
                      label = "Smooth",
                      value = TRUE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_1(
                    numericInput(ns("buffer"),
                                 label = "Buffer",
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

  })
}

## To be copied in the UI
# mod_phanalyze_ui("phanalyze_1")

## To be copied in the server
# mod_phanalyze_server("phanalyze_1")
