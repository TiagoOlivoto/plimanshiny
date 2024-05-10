#' timeseriesanalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_timeseriesanalysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          width = 12,
          icon = icon("gears"),
          status  = "success",
          type = "tabs",
          tabPanel(
            title = "Configure the analysis",
            fluidRow(
              col_6(
                actionButton(
                  inputId = ns("guideanalyze"),
                  label = tagList(
                    icon = icon("question-circle", verify_fa = FALSE), "Guide"
                  ),
                  style = "color: white ; background-color: #dd4b39",
                  class = "btn-danger"
                )
              ),
              col_6(
                actionBttn(ns("analyzemosaicts"),
                           label = "Analyze the mosaic!",
                           status = "success")
              )
            ),
            hl(),
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
            prettyCheckbox(
              inputId = ns("croptoext"),
              label = "Crop to the shapefile extend?",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            ),
            hl(),
            divclass("anal1",
                     pickerInput(
                       inputId = ns("summarizefun"),
                       label = "Summarize function(s)",
                       selected = "median",
                       choices = c("none", "min", "max", "count", "sum", "mean", "median", "quantile", "stdev", "coefficient_of_variation"),
                       options = list(
                         `actions-box` = TRUE
                       ),
                       multiple = TRUE
                     ),
                     textInput(
                       inputId = ns("quantilevalue"),
                       label = "Quantile Values (comma-separated)",
                       value = ""
                     )
            ),
            hl(),
            h3("Settings"),
            fluidRow(
              col_6(
                divclass("anal2",
                         materialSwitch(
                           inputId = ns("segmentplot"),
                           label = "Segment plot?",
                           value = FALSE,
                           status = "success"
                         )
                )
              ),
              col_6(
                divclass("anal3",
                         materialSwitch(
                           inputId = ns("segmentindividuals"),
                           label = "Segment individuals?",
                           value = FALSE,
                           status = "success"
                         )
                )
              )
            ),
            fluidRow(
              conditionalPanel(
                condition = "input.segmentindividuals == true | input.segmentplot == true", ns = ns,
                h4("Indexes and inclusion criteria"),
                fluidRow(
                  col_6(
                    switchInput(
                      inputId = ns("usemaskorind"),
                      label = "Segmentation",
                      labelWidth = "80px",
                      onLabel = "Index",
                      offLabel = "Mask",
                      value = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.usemaskorind == true", ns = ns,
                      divclass("anal4",
                               selectInput(ns("segmentindex"),
                                           label = "Index for segmentation",
                                           choices = NULL)
                      ),
                      divclass("anal6",
                               selectInput(ns("threshold"),
                                           label = "Threshold method",
                                           choices = c("Otsu", "numeric")),
                               conditionalPanel(
                                 condition = "input.threshold == 'numeric'", ns = ns,
                                 numericInput(ns("threshvalue"),
                                              label = "Threshold",
                                              value = NA)

                               )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.usemaskorind == false", ns = ns,
                      selectInput(ns("availablemasks"),
                                  label = "Mask for segmentation",
                                  choices = NULL)
                    ),
                    divclass("anal5",
                             selectInput(ns("includeif"),
                                         label = "Inclusion criteria",
                                         choices = c("centroid", "covered", "overlap", "intersect"))
                    )
                  ),
                  col_6(
                    divclass("anal7",
                             prettyCheckbox(
                               inputId = ns("croptoext"),
                               label = "Crop to Extend",
                               value = TRUE,
                               icon = icon("check"),
                               status = "success",
                               animation = "rotate"
                             )
                    ),
                    divclass("anal7",
                             prettyCheckbox(
                               inputId = ns("simplify"),
                               label = "Simplify",
                               value = FALSE,
                               icon = icon("check"),
                               status = "success",
                               animation = "rotate"
                             )
                    ),
                    divclass("anal8",
                             prettyCheckbox(
                               inputId = ns("invertindex"),
                               label = "Invert",
                               value = FALSE,
                               icon = icon("check"),
                               status = "success",
                               animation = "rotate"
                             )
                    ),
                    divclass("anal9",
                             prettyCheckbox(
                               inputId = ns("watershed"),
                               label = "Watershed",
                               value = TRUE,
                               icon = icon("check"),
                               status = "success",
                               animation = "rotate"
                             ),
                             conditionalPanel(
                               condition = "input.watershed == true", ns = ns,
                               fluidRow(
                                 col_6(
                                   numericInput(ns("tolerance"),
                                                label = "Tolerance",
                                                value = 1)
                                 ),
                                 col_6(
                                   numericInput(ns("extension"),
                                                label = "Extension",
                                                value = 1)

                                 )
                               )
                             )
                    ),
                    divclass("anal10",
                             prettyCheckbox(
                               inputId = ns("mapindividuals"),
                               label = "Map individuals",
                               value = FALSE,
                               icon = icon("check"),
                               status = "success",
                               animation = "rotate"
                             ),
                             conditionalPanel(
                               condition = "input.mapindividuals == true", ns = ns,
                               selectInput(ns("mapdirection"),
                                           label = "Direction for mapping",
                                           choices = c("horizontal", "vertical")),
                             )
                    )
                  )
                ),
                hl(),
                h4("Handling noises"),
                fluidRow(
                  divclass("anal11",
                           col_6(
                             numericInput(ns("lower_noise"),
                                          label = "Lower noise",
                                          value = 0.15)
                           )
                  ),
                  divclass("anal12",
                           col_6(
                             prettyCheckbox(
                               inputId = ns("filter"),
                               label = "Median filter",
                               value = FALSE,
                               icon = icon("check"),
                               status = "success",
                               animation = "rotate"
                             ),
                             conditionalPanel(
                               condition = "input.filter == true", ns = ns,
                               numericInput(ns("filterval"),
                                            label = "Median filter",
                                            value = 2),
                             )
                           )
                  )
                ),
                fluidRow(
                  col_3(
                    divclass("anal13",
                             numericInput(ns("lower_size"),
                                          label = "Lower size",
                                          value = NA)
                    )
                  ),
                  col_3(
                    numericInput(ns("upper_size"),
                                 label = "Upper size",
                                 value = NA)
                  ),
                  col_3(
                    divclass("anal14",
                             numericInput(ns("topn_lower"),
                                          label = "Top N lower",
                                          value = NA)
                    )
                  ),
                  col_3(
                    numericInput(ns("topn_upper"),
                                 label = "Top N upper",
                                 value = NA)
                  )
                )
              )
            ),
            hl(),
          ),
          tabPanel(
            title = "Configure the output",
            actionButton(
              inputId = ns("guideoutput"),
              label = tagList(
                icon = icon("question-circle", verify_fa = FALSE), "Guide"
              ),
              style = "color: white ; background-color: #dd4b39",
              class = "btn-danger"
            ),
            hl(),
            selectInput(ns("plotattribute"),
                        label = "Attribute",
                        choices = NULL),
            hl(),
            pickerpalette(id, "palplot", selected = "RdYlGn"),
            prettyCheckbox(
              inputId = ns("palplotrev"),
              label = "Reverse",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            ),
            sliderInput(ns("alpharesplot"),
                        label = "Fill opacity",
                        min = 0,
                        max = 1,
                        value = 0.75),
            h3("Export the results"),
            divclass("out4",
                     mod_download_shapefile_ui(ns("downresplot"), label = "Plot results")
            ),
            conditionalPanel(
              condition = "input.segmentindividuals == true", ns = ns,
              divclass("out5",
                       mod_download_shapefile_ui(ns("downresindiv"), label = "Individual results")
              )
            ),
            hl(),
            h3("Assign output to the R environment"),
            fluidRow(
              col_4(
                actionButton(
                  inputId = ns("savetoglobalenv"),
                  label = "Assign",
                  icon = icon("share-from-square"),
                  status = "success",
                  gradient = TRUE,
                  width = "150px",
                  flat = TRUE
                )
              ),
              col_8(
                textInput(ns("globalvarname"),
                          label = "Variable name",
                          value = "plimanshiny_output")
              )
            )
          )
        )
      ),
      col_8(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "780px",
          status = "success",
          title = "Results",
          selected = "Overview",
          solidHeader = FALSE,
          maximizable = TRUE,
          type = "tabs",
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("timeserieoverview"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Compare levels",
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("groupingvar"),
                  label = "Select a grouping variable:",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_6(
                pickerInput(
                  inputId = ns("levelsvar"),
                  label = "Select up to six levels to compare:",
                  choices = NULL,
                  multiple = TRUE,
                  options =  list(
                    "max-options" = 6,
                    "max-options-text" = "No more levels allowed"
                  )
                )
              )
            ),
            plotlyOutput(ns("timeseriecompare"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Evolution plot",
            fluidRow(
              col_4(
                pickerInput(
                  inputId = ns("plottoshow"),
                  label = "'unique_id' to see the evolution across time:",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_4(
                pickerInput(
                  inputId = ns("rgborattribute"),
                  label = "How to see the plot evolution",
                  choices = c("RGB", "plot attribute"),
                  multiple = FALSE
                )
              ),
              col_2(
                numericInput(
                  ns("ncols"),
                  label = "Columns",
                  value = NA
                )
              ),
              col_2(
                numericInput(
                  ns("nrows"),
                  label = "Rows",
                  value = NA
                )
              )
            ),
            plotOutput(ns("evolutionplot"), height = "640px") |> add_spinner()
          ),
          tabPanel(
            title = "Compare plots",
            pickerInput(
              inputId = ns("rgborattribute2"),
              label = "How to see the plot evolution",
              choices = c("RGB", "plot attribute"),
              multiple = FALSE
            ),
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("levelvarcp1"),
                  label = "Select one level to compare:",
                  choices = NULL,
                  multiple = FALSE
                ),
                plotOutput(ns("compplot1"), height = "540px") |> add_spinner()
              ),
              col_6(
                pickerInput(
                  inputId = ns("levelvarcp2"),
                  label = "Select one level to compare:",
                  choices = NULL,
                  multiple = FALSE
                ),
                plotOutput(ns("compplot2"), height = "540px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Map plot (single data)",
            fluidRow(
              col_4(
                pickerInput(
                  inputId = ns("filterdate"),
                  label = "Date to explore",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_4(
                pickerInput(
                  inputId = ns("groupvarsd"),
                  label = "Select a grouping variable:",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              col_4(
                pickerInput(
                  inputId = ns("levelsvarsd"),
                  label = "Select levels to filter:",
                  choices = NULL,
                  multiple = TRUE
                )
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "680px")  |> add_spinner()
          ),
          tabPanel(
            title = "Raw results",
            DT::dataTableOutput(ns("rawresults"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      )
    )
  )
}

#' timeseriesanalysis Server Functions
#'
#' @noRd
mod_timeseriesanalysis_server <- function(id, shapefile, mosaiclist, r, g, b, re, nir, swir, tir, basemap){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Update indexes based on bands
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
    observe({
      updateSelectInput(session, "segmentindex", choices = finalindex())
    })

    observeEvent(input$analyzemosaicts, {
      req(mosaiclist$mosaics$data)
      req(shapefile$shape$data)
      if(is.null(mosaiclist$mosaics$data) | is.null(shapefile$shape$data)){
        sendSweetAlert(
          session = session,
          title = "Did you skip any steps?",
          text = "To analyze the mosaic, ensure that mosaic and shapefile have been correctly imported.",
          type = "error"
        )
      }

      if(is.null(basemap$map)){
        bm <- mosaic_view(mosaiclist$mosaics$data[[1]])

      } else{
        bm <- basemap$map
      }

      if((input$segmentplot | input$segmentindividuals)){
        indcomp <- input$segmentindex
      } else{
        indcomp <- NULL
      }
      if(input$summarizefun[[1]] == "none"){
        summf <- NULL
      } else{
        summf <- input$summarizefun
      }

      if(is.na(input$lower_size)){
        lower_size <- NULL
      } else{
        lower_size <- input$lower_size
      }
      # For upper_size
      if (is.na(input$upper_size)) {
        upper_size <- NULL
      } else {
        upper_size <- input$upper_size
      }

      # For topn_lower
      if (is.na(input$topn_lower)) {
        topn_lower <- NULL
      } else {
        topn_lower <- input$topn_lower
      }

      # For topn_upper (assuming you've already adapted it)
      if (is.na(input$topn_upper)) {
        topn_upper <- NULL
      } else {
        topn_upper <- input$topn_upper
      }
      if("quantile" %in% input$summarizefun){
        quantiles <- as.numeric(unlist(strsplit(input$quantilevalue, split = ',')))
      } else{
        quantiles <- NULL
      }

      # Loop
      progressSweetAlert(
        session = session,
        id = "myprogress",
        title = "Start",
        display_pct = TRUE,
        value = 0,
        total = length(mosaiclist$mosaics$data)
      )
      bind <- list()

      for (i in seq_along(mosaiclist$mosaics$data)) {
        updateProgressBar(
          session = session,
          id = "myprogress",
          value = i,
          title = paste0("Working in progress, Please, wait."),
          total = length(mosaiclist$mosaics$data)
        )
        res <-
          mosaic_analyze(
            mosaic = mosaiclist$mosaics$data[[i]],
            shapefile = shapefile$shape$data,
            basemap = bm,
            plot = FALSE,
            crop_to_shape_ext = input$croptoext,
            r = suppressWarnings(as.numeric(r$r)),
            g = suppressWarnings(as.numeric(g$g)),
            b = suppressWarnings(as.numeric(b$b)),
            re = suppressWarnings(as.numeric(re$re)),
            nir = suppressWarnings(as.numeric(nir$nir)),
            swir = suppressWarnings(as.numeric(swir$swir)),
            tir = suppressWarnings(as.numeric(tir$tir)),
            plot_index = finalindex(),
            segment_plot = input$segmentplot,
            segment_individuals = input$segmentindividuals,
            simplify = input$simplify,
            segment_index = indcomp,
            watershed = input$watershed,
            tolerance = input$tolerance,
            extension = input$extension,
            invert = input$invertindex,
            summarize_fun = summf,
            summarize_quantiles = quantiles,
            include_if = input$includeif,
            threshold = ifelse(input$threshold == "Otsu", "Otsu", input$threshvalue),
            filter = ifelse(input$filter, input$filterval, FALSE),
            lower_noise = input$lower_noise,
            lower_size = lower_size,
            upper_size = upper_size,
            topn_lower = topn_lower,
            topn_upper = topn_upper,
            map_individuals = input$mapindividuals,
            map_direction = input$mapdirection,
            verbose = FALSE
          )
        bind[[names(mosaiclist$mosaics$data[i])]] <- res
      }
      req(bind)


      if(is.null(bind[[1]]$result_individ_map)){
        result_individ_map <- NULL
      }
      if(is.null(bind[[1]]$result_indiv)){
        result_indiv <- result_plot_summ <- NULL
      } else{
        result_indiv <- dplyr::bind_rows(
          lapply(bind, function(x){
            tmp <- x$result_indiv
            tmp
          }),
          .id = "date"
        ) |>
          dplyr::mutate(date = as.Date(date)) |>
          dplyr::arrange(date, block, plot_id) |>
          dplyr::mutate(unique_id = dplyr::row_number()) |>
          dplyr::relocate(unique_id, date, .before = block) |>
          sf::st_as_sf()

        result_plot_summ <- dplyr::bind_rows(
          lapply(bind, function(x){
            tmp <- x$result_plot_summ
            tmp
          }),
          .id = "date"
        ) |>
          dplyr::mutate(date = as.Date(date)) |>
          dplyr::arrange(date, block, plot_id) |>
          dplyr::mutate(unique_id = dplyr::row_number()) |>
          dplyr::relocate(unique_id, date, .before = block) |>
          sf::st_as_sf()
      }

      result_plot <- dplyr::bind_rows(
        lapply(bind, function(x){
          tmp <- x$result_plot
          tmp
        }),
        .id = "date"
      ) |>
        dplyr::mutate(date = as.Date(date)) |>
        dplyr::arrange(date, block, plot_id) |>
        dplyr::mutate(unique_id = dplyr::row_number()) |>
        dplyr::relocate(unique_id, date, .before = block) |>
        sf::st_as_sf()


      if("data" %in% colnames(res$result_plot)){
        if(input$segmentindividuals){
          unndata <-
            res$result_indiv |>
            dplyr::arrange(date, block, plot_id, individual) |>
            dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
            sf::st_drop_geometry() |>
            dplyr::select(unique_id, date, block, plot_id, individual, data) |>
            tidyr::unnest(cols = data) |>
            dplyr::group_by(unique_id, date, block, plot_id, individual) |>
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")

          result_indiv <- dplyr::left_join(res$result_indiv |> dplyr::select(-data), unndata, by = dplyr::join_by(unique_id, date, block, plot_id, individual))
          unndata <-
            res$result_plot |>
            dplyr::arrange(date, block, plot_id) |>
            dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
            sf::st_drop_geometry() |>
            tidyr::unnest(cols = data) |>
            dplyr::group_by(unique_id, date, block, plot_id) |>
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop") |>
            sf::st_drop_geometry()
          result_plot_summ<- dplyr::left_join(res$result_plot_summ, unndata, by = dplyr::join_by(unique_id, date, block, plot_id))

          result_plot <-
            res$result_plot |>
            dplyr::arrange(date, block, plot_id) |>
            dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
            tidyr::unnest(cols = data) |>
            dplyr::group_by(unique_id, date, block, plot_id) |>
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")
        }

        if(input$segmentplot){
          unndata <-
            res$result_plot |>
            dplyr::arrange(date, block, plot_id) |>
            dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
            sf::st_drop_geometry() |>
            tidyr::unnest(cols = data) |>
            dplyr::group_by(unique_id, date, block, plot_id) |>
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop") |>
            sf::st_drop_geometry() |>
            dplyr::select(-c(3:5))
          result_plot <- dplyr::left_join(res$result_plot |> dplyr::select(-data), unndata, by = dplyr::join_by(unique_id, date, block, plot_id))
          result_indiv <- res$result_indiv
          result_plot_summ <- res$result_plot_summ
        }
        if(!input$segmentindividuals & !input$segmentplot){
          result_plot <-
            res$result_plot |>
            dplyr::arrange(date, block, plot_id) |>
            dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
            tidyr::unnest(cols = data) |>
            dplyr::group_by(unique_id, date, block, plot_id) |>
            dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")
          result_indiv <- res$result_indiv
          result_plot_summ <- res$result_plot_summ
        }
      }

      ##### Show the results #######
      updateSelectInput(session, "plotattribute",
                        choices = names(result_plot),
                        selected = ifelse("individual" %in% colnames(result_plot), colnames(result_plot)[6], colnames(result_plot)[5]))

      # overview plot
      output$timeserieoverview <- renderPlotly({
        req(input$plotattribute)
        plot_ind <-
          result_plot |>
          as.data.frame() |>
          dplyr::select(dplyr::all_of(c("date", input$plotattribute)))
        p <-
          ggplot(plot_ind, aes(x = .data[["date"]], y = .data[[input$plotattribute]])) +
          geom_boxplot(fill = "#28a745") +
          geom_smooth() +
          theme_bw() +
          scale_x_date(date_labels = "%d/%m")
        plotly::ggplotly(p, dynamicTicks = TRUE)
      })

      # evolution plot
      # update the values
      observe({
        updatePickerInput(session, "groupingvar",
                          choices = colnames(result_plot),
                          selected = "plot_id")
      })
      observe({
        req(input$groupingvar)
        levels <- sort(unique(result_plot[[input$groupingvar]]))
        updatePickerInput(session, "levelsvar",
                          choices = levels)
      })

      observe({
        levels <- sort(unique(result_plot[["unique_id"]]))
        updatePickerInput(session, "levelvarcp1",
                          choices = levels)
        updatePickerInput(session, "levelvarcp2",
                          choices = levels)
      })


      # level comparision
      output$timeseriecompare <- renderPlotly({
        req(input$plotattribute)
        req(input$groupingvar)
        req(input$levelsvar)

        plot_ind <-
          result_plot |>
          as.data.frame() |>
          dplyr::select(dplyr::all_of(c("date", input$groupingvar, input$plotattribute))) |>
          dplyr::filter(!!dplyr::sym(input$groupingvar) %in% input$levelsvar) |>
          dplyr::group_by(date, !!dplyr::sym(input$groupingvar)) |>
          dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}))


        p2 <-
          ggplot(plot_ind, aes(x = .data[["date"]], y = .data[[input$plotattribute]],
                               group = .data[[input$groupingvar]],
                               color = .data[[input$groupingvar]])) +
          geom_point() +
          geom_smooth(se = FALSE,
                      method = 'loess',
                      formula = 'y ~ x') +
          theme_bw() +
          scale_x_date(date_labels = "%d/%m")
        plotly::ggplotly(p2, dynamicTicks = TRUE)
      })

      # plot evolution
      shp <- reactiveValues()
      if(!"unique_id" %in% colnames(shapefile$shape$data)){
        shp$shp <-
          shapefile$shape$data |>
          dplyr::mutate(unique_id = dplyr::row_number(), .before = 1)
      } else{
        shp$shp <- shapefile$shape$data
      }

      # plot evolution
      req(shp$shp)
      observe({
        levels <- sort(unique(shp$shp[["unique_id"]]))
        updatePickerInput(session, "plottoshow",
                          choices = levels)
      })

      observe({
        num_plots <- length(mosaiclist$mosaics$data)
        ncol <- ceiling(sqrt(num_plots))
        nrow <- ceiling(num_plots/ncol)
        req(ncol)
        req(nrow)
        updateNumericInput(session, "nrows",
                           value = nrow)
        updateNumericInput(session, "ncols",
                           value = ncol)
      })


      output$evolutionplot <- renderPlot({
        req(input$plottoshow)
        shapetmp <-
          shp$shp |>
          dplyr::filter(unique_id == input$plottoshow) |>
          shapefile_input(as_sf = FALSE, info = FALSE)


        if(input$rgborattribute == "RGB"){
          req(input$nrows, input$ncols)
          op <- par(mfrow = c(input$nrows, input$ncols))
          on.exit(par(op))
          for (i in seq_along(mosaiclist$mosaics$data)) {
            terra::crop(mosaiclist$mosaics$data[[i]], shapetmp, mask = TRUE) |> terra::plotRGB(stretch = "hist")
          }
          par(mfrow = c(1, 1))
        } else{

          list_mo <- list()
          tmpind <- sub("^[^.]*\\.", "", input$plotattribute)

          for (i in seq_along(mosaiclist$mosaics$data)) {
            list_mo[[i]] <-
              terra::crop(mosaiclist$mosaics$data[[i]], shapetmp, mask = TRUE) |>
              mosaic_index(
                r = suppressWarnings(as.numeric(r$r)),
                g = suppressWarnings(as.numeric(g$g)),
                b = suppressWarnings(as.numeric(b$b)),
                re = suppressWarnings(as.numeric(re$re)),
                nir = suppressWarnings(as.numeric(nir$nir)),
                swir = suppressWarnings(as.numeric(swir$swir)),
                tir = suppressWarnings(as.numeric(tir$tir)),
                index = tmpind,
                plot = FALSE
              )
          }

          list_mo <-
            lapply(seq_along(list_mo), function(i){
              terra::resample(list_mo[[i]], list_mo[[1]])
            }) |>
            terra::rast()

          req(list_mo)
          names(list_mo) <- names(mosaiclist$mosaics$data)

          ggplot() +
            tidyterra::geom_spatraster(data = list_mo,
                                       interpolate = TRUE,
                                       maxcell = 2.5e5) +
            tidyterra::scale_fill_terrain_c() +
            facet_wrap(~lyr,
                       ncol =  input$ncols,
                       nrow = input$nrows) +
            theme_void() +
            theme(legend.position = "bottom") +
            guides(fill = guide_colourbar(theme = theme(
              legend.key.width = unit(350, "pt")
            ))) +
            labs(fill = tmpind)

        }
      })

      output$compplot1 <- renderPlot({
        req(input$levelvarcp1)
        shapetmp <-
          result_plot |>
          dplyr::filter(unique_id == input$levelvarcp1)

        mosaic1 <- mosaiclist$mosaics$data[[which(names(mosaiclist$mosaics$data) == shapetmp$date)]]
        mc <- terra::crop(mosaic1, terra::vect(shapetmp), mask = TRUE)

        if(input$rgborattribute2 == "RGB"){
          terra::plotRGB(mc, stretch = "hist")
        } else{
          tmpind <- sub("^[^.]*\\.", "", input$plotattribute)
          mosaic_index(
            mc,
            r = suppressWarnings(as.numeric(r$r)),
            g = suppressWarnings(as.numeric(g$g)),
            b = suppressWarnings(as.numeric(b$b)),
            re = suppressWarnings(as.numeric(re$re)),
            nir = suppressWarnings(as.numeric(nir$nir)),
            swir = suppressWarnings(as.numeric(swir$swir)),
            tir = suppressWarnings(as.numeric(tir$tir)),
            index = tmpind
          )
        }
      })

      output$compplot2 <- renderPlot({
        req(input$levelvarcp2)
        shapetmp <-
          result_plot |>
          dplyr::filter(unique_id == input$levelvarcp2)

        mosaic1 <- mosaiclist$mosaics$data[[which(names(mosaiclist$mosaics$data) == shapetmp$date)]]
        mc <- terra::crop(mosaic1, terra::vect(shapetmp), mask = TRUE)

        if(input$rgborattribute2 == "RGB"){
          terra::plotRGB(mc, stretch = "hist")
        } else{
          tmpind <- sub("^[^.]*\\.", "", input$plotattribute)
          mosaic_index(
            mc,
            r = suppressWarnings(as.numeric(r$r)),
            g = suppressWarnings(as.numeric(g$g)),
            b = suppressWarnings(as.numeric(b$b)),
            re = suppressWarnings(as.numeric(re$re)),
            nir = suppressWarnings(as.numeric(nir$nir)),
            swir = suppressWarnings(as.numeric(swir$swir)),
            tir = suppressWarnings(as.numeric(tir$tir)),
            index = tmpind
          )
        }
      })


      # single data
      # filter by date
      # update the values
      observe({
        updatePickerInput(session, "groupvarsd",
                          choices = colnames(result_plot),
                          selected = "plot_id")
      })
      observe({
        req(input$groupvarsd)
        levels <- sort(unique(result_plot[[input$groupvarsd]]))
        updatePickerInput(session, "levelsvarsd",
                          choices = levels)
      })
      observe({
        levels <- unique(result_plot |> as.data.frame() |>  dplyr::select(date) |> dplyr::pull())
        updatePickerInput(session, "filterdate",
                          choices = levels)
      })


      output$resultsplotmap <- renderLeaflet({
        req(input$filterdate)

        dftemp <-
          result_plot |>
          dplyr::filter(date == input$filterdate)

        if(!is.null(input$levelsvarsd)){
          dftemp <-
            dftemp |>
            dplyr::filter(!!dplyr::sym(input$groupvarsd) %in% input$levelsvarsd)
        }


        if((!input$plotattribute %in% colnames(dftemp)) & !is.null(summf)){
          attrib <- paste0(input$summarizefun[[1]], ".", input$segmentindex)
        } else{
          attrib <- input$plotattribute
        }
        mshp <- shapefile_view(dftemp,
                               attribute = attrib,
                               color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                               alpha.regions = input$alpharesplot)
        (basemap$map +  mshp)@map
      })


      # raw results
      output$rawresults <- DT::renderDataTable(
        result_plot |>
          as.data.frame() |>
          dplyr::select(-geometry) |>
          roundcols(),
        extensions = 'Buttons',
        rownames = FALSE,
        options = list(
          dom = 'Blrtip',
          buttons = c('copy', 'excel'),
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "620px",
          pageLength = 15
        )
      )



      mod_download_shapefile_server("downresplot", terra::vect(result_plot), name = "plot_level_results")

      closeSweetAlert(session = session)

    })
  })
}

## To be copied in the UI
# mod_timeseriesanalysis_ui("timeseriesanalysis_1")

## To be copied in the server
# mod_timeseriesanalysis_server("timeseriesanalysis_1")
