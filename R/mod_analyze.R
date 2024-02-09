#' analyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyze_ui <- function(id){
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
                actionBttn(ns("analyzemosaic"),
                           label = "Analyze the mosaic!",
                           status = "success")
              )
            ),
            hl(),
            divclass("anal1",
                     pickerInput(
                       inputId = ns("summarizefun"),
                       label = "Summarize function(s)",
                       selected = "mean",
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
                ),
                divclass("anal3",
                         materialSwitch(
                           inputId = ns("segmentindividuals"),
                           label = "Segment individuals?",
                           value = FALSE,
                           status = "success"
                         )
                )
              ),
              col_6(
                divclass("anal3",
                         materialSwitch(
                           inputId = ns("byplot"),
                           label = "Analysis by shape?",
                           value = FALSE,
                           status = "success"
                         ),
                         conditionalPanel(
                           condition = "input.byplot == true",  ns = ns,
                           fluidRow(
                             col_7(
                               switchInput(
                                 inputId = ns("parallelanalysis"),
                                 label = "Parallel",
                                 onLabel = "Yes",
                                 offLabel = "No",
                                 labelWidth = "80px"
                               )
                             ),
                             col_5(
                               conditionalPanel(
                                 condition = "input.parallelanalysis == true",  ns = ns,
                                 numericInput(ns("numworkers"),
                                              label = "Clusters",
                                              value = NULL)
                               )
                             )
                           )
                         )
                )
              ),

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
            divclass("out1",
                     pickerInput(
                       inputId = ns("summarizefunoutput"),
                       label = "Summarize function",
                       choices = NULL
                     )
            ),
            fluidRow(
              col_6(
                divclass("out2",
                         selectInput(ns("plotattribute"),
                                     label = "Plot attribute",
                                     choices = NULL),
                         pickerpalette(id, "palplot", selected = "RdYlGn"),
                         sliderInput(ns("alpharesplot"),
                                     label = "Fill opacity",
                                     min = 0,
                                     max = 1,
                                     value = 0.75)
                )
              ),
              col_6(
                conditionalPanel(
                  condition = "input.segmentindividuals == true", ns = ns,
                  divclass("out3",
                           selectInput(ns("indivattribute"),
                                       label = "Individual attribute",
                                       choices = c("area")),
                           pickerpalette(id, "palind", selected = "set2"),
                           sliderInput(ns("alpharesindiv"),
                                       label = "Fill opacity",
                                       min = 0,
                                       max = 1,
                                       value = 0.75)
                  )
                )
              )
            ),
            hl(),
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
        uiOutput(ns("uiresults"))
      )
    )
  )
}

helpanal <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "analyze")
helpout <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "output")
#' analyze Server Functions
#'
#' @noRd
mod_analyze_server <- function(id, mosaic_data, basemap, shapefile, index, pathmosaic){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$guideanalyze, introjs(session,
                                             options = list("nextLabel"="Next",
                                                            "prevLabel"="Previous",
                                                            "skipLabel"="Skip",
                                                            steps = helpanal),
                                             events = list("oncomplete"=I('alert("Hope it helped!")'))))
    observeEvent(input$guideoutput, introjs(session,
                                            options = list("nextLabel"="Next",
                                                           "prevLabel"="Previous",
                                                           "skipLabel"="Skip",
                                                           steps = helpout),
                                            events = list("oncomplete"=I('alert("Hope it helped!")'))))

    output$uiresults <- renderUI({
      if(input$segmentindividuals){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Segmentation",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Segmentation",
            fluidRow(
              col_6(
                plotOutput(ns("previousdensity"), height = "720px") |> add_spinner()
              ),
              col_6(
                plotOutput(ns("previoussegment"), height = "720px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Mosaic and shapefile",
            uiOutput(ns("baseshapeindex")) |> add_spinner()
          ),
          tabPanel(
            title = "Summary",
            fluidRow(
              valueBoxOutput(ns("vbnplots")),
              valueBoxOutput(ns("vbnindiv")),
              valueBoxOutput(ns("vbnindivplotmean")),
              valueBoxOutput(ns("vbcoverage")),
              valueBoxOutput(ns("largerindiv")),
              valueBoxOutput(ns("smallerindiv"))
            ),
            fluidRow(
              col_6(
                h4("Index (plot)"),
                plotlyOutput(ns("boxresults"), height = "420px") |> add_spinner()
              ),
              col_3(
                h4("Attribute (plot)"),
                plotlyOutput(ns("plotatribres"), height = "420px") |> add_spinner()
              ),
              col_3(
                h4("Attribute (individual)"),
                plotlyOutput(ns("indivatribres"), height = "420px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Map plot",
            leafletOutput(ns("resultsplotmap"), height = "720px")  |> add_spinner(),
            downloadBttn(ns("downloadplotmap"),
                         label = "Take a shot",
                         style = "pill")
          ),
          tabPanel(
            title = "Results plot",
            DT::dataTableOutput(ns("resultplottab"), height = "720px", width = 980)  |> add_spinner()
          ),
          tabPanel(
            title = "Map individuals",
            leafletOutput(ns("resultsindivmap"), height = "720px")  |> add_spinner(),
            downloadBttn(ns("downloadindividmap"),
                         label = "Take a shot",
                         style = "pill")
          ),
          tabPanel(
            title = "Results individuals",
            DT::dataTableOutput(ns("resultsindivtab"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      } else if(input$segmentplot){
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "780px",
          status = "success",
          title = "Results",
          selected = "Segmentation",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Segmentation",
            fluidRow(
              col_6(
                plotOutput(ns("previousdensity"), height = "720px") |> add_spinner()
              ),
              col_6(
                plotOutput(ns("previoussegment"), height = "720px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Mosaic and shapefile",
            uiOutput(ns("baseshapeindex"))  |> add_spinner()
          ),
          tabPanel(
            title = "Summary",
            fluidRow(
              valueBoxOutput(ns("vbnplots"), width = 3),
              valueBoxOutput(ns("vbcoverage"), width = 3),
              valueBoxOutput(ns("mincoverage"), width = 3),
              valueBoxOutput(ns("maxcoverage"), width = 3)
            ),
            h4("Index (plot)"),
            plotlyOutput(ns("boxresults"), height = "420px") |> add_spinner()
          ),
          tabPanel(
            title = "Map plot",
            leafletOutput(ns("resultsplotmap"), height = "720px") |> add_spinner(),
            downloadBttn(ns("downloadplotmap"),
                         label = "Take a shot",
                         style = "pill")
          ),
          tabPanel(
            title = "Results plot",
            DT::dataTableOutput(ns("resultplottab"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      } else {
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "780px",
          status = "success",
          title = "Results",
          selected = "Mosaic and shapefile",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Mosaic and shapefile",
            uiOutput(ns("baseshapeindex"))  |> add_spinner()
          ),
          tabPanel(
            title = "Summary",
            h4("Index (plot)"),
            plotlyOutput(ns("boxresults"), height = "420px") |> add_spinner()
          ),
          tabPanel(
            title = "Map plot",
            leafletOutput(ns("resultsplotmap"), height = "720px") |> add_spinner(),
            downloadBttn(ns("downloadplotmap"),
                         label = "Take a shot",
                         style = "pill")
          ),
          tabPanel(
            title = "Results plot",
            DT::dataTableOutput(ns("resultplottab"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      }
    })

    shptemp <- reactiveValues(shapefile = NULL)
    observe({
      updateSelectInput(session, "availablemasks", choices = c("none", setdiff(names(mosaic_data), "mosaic")), selected = "none")
      updatePickerInput(session, "summarizefunoutput", choices = input$summarizefun, selected = input$summarizefunoutput[[1]])
    })
    maskval <- reactiveValues(mask = NULL)
    observe({
      req(shapefile$shapefile)
      if(!inherits(shapefile$shapefile, "list")){
        shptemp$shapefile <- list(shapefile$shapefile)
      } else{
        shptemp$shapefile <- shapefile$shapefile
      }
      req(input$availablemasks)
      if(input$availablemasks == "none"){
        maskval$mask <- NULL
      } else{
        maskval$mask <- mosaic_data[[input$availablemasks]]$data

        if(terra::crs(maskval$mask) != terra::crs(mosaic_data$mosaic)){
          sendSweetAlert(
            session = session,
            title = "Invalid CRS",
            text = "The Coordinate Reference System (CRS) of the mask does
            not match the input mosaic. Trying to set the mask's CRS to match the mosaic one.",
            type = "warning"
          )
        }

      }
      if(input$segmentplot & input$segmentindividuals){
        sendSweetAlert(
          session = session,
          title = "Ops, invalid arguments.",
          text = "Only plots OR individuals can be segmented. ",
          type = "error"
        )
        updateMaterialSwitch(session, "segmentindividuals", value = FALSE)
      }
      if(input$segmentplot | input$segmentindividuals){
        if(input$byplot){
          if(is.null(index$index)){
            updateSelectInput(session, "segmentindex", choices = pliman_indexes())
          } else{

          }
        } else{
          req(index$index)
          updateSelectInput(session, "segmentindex", choices = names(index$index))
        }

      }
      availablecl <- parallel::detectCores()
      updateNumericInput(session, "numworkers", value = round(availablecl * 0.4), max = availablecl - 2)

    })

    output$baseshapeindex <- renderUI({
      if(!input$byplot){
        req(index$index)
        req(mosaic_data$mosaic)
        req(basemap$map)
        req(input$segmentindex)
        layer <- ifelse(input$segmentindex != "", input$segmentindex, 1)
        m1 <-  basemap$map + mapview::mapview(do.call(rbind, lapply(shptemp$shapefile, function(x){x})),
                                              z.col = "plot_id",
                                              legend = FALSE)
        m2 <- mosaic_view(index$index[[layer]], show = "index")
        leafsync::sync(m1@map, m2)
      }
    })

    output$previousdensity <- renderPlot({
      if(!input$byplot){
        if(is.null(maskval$mask)){
          req(index$index)
          layer <- ifelse(input$segmentindex != "", input$segmentindex, 1)
          ots <- ifelse(input$threshold == "Otsu", otsu(na.omit(terra::values(index$index[[layer]]))), input$threshvalue)
          output$previoussegment <- renderPlot({
            if(input$invertindex){
              maskplot <- index$index[[layer]] < ots
            } else{
              maskplot <- index$index[[layer]] > ots
            }
            terra::plot(maskplot)
          })
          terra::density(index$index[[layer]],
                         main = paste0(names(index$index[[layer]]), " - Otsu: ", round(ots, 4)))
          abline(v = ots,
                 col = "red",
                 lty = 2,
          )
        } else{
          output$previoussegment <- renderPlot({
            terra::plot(maskval$mask)
          })

        }
      }
    })

    # Analyze
    observeEvent(input$analyzemosaic, {
      if(c(!input$byplot & is.null(index$index)) | is.null(mosaic_data$mosaic) | is.null(basemap$map) | is.null(shptemp$shapefile)){
        sendSweetAlert(
          session = session,
          title = "Did you skip any steps?",
          text = "To analyze the mosaic, ensure that mosaic, index, and shapefile have been correctly computed.",
          type = "error"
        )
      }

      t1 <- !is.na(input$lower_size) & !is.na(input$topn_lower)
      if(t1){
        sendSweetAlert(
          session = session,
          title = "Ops, invalid arguments.",
          text = "Only `lower_size` OR `topn_lower` can be used.",
          type = "error"
        )
      }
      t2 <- !is.na(input$upper_size) & !is.na(input$topn_upper)
      if(t2){
        sendSweetAlert(
          session = session,
          title = "Ops, invalid arguments.",
          text = "Only `upper_size` OR `topn_upper` can be used.",
          type = "error"
        )
      }

      if((input$segmentplot | input$segmentindividuals)){
        indcomp <- input$segmentindex
      } else{
        indcomp <- NULL
      }
      if(input$summarizefun == "none"){
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

      if(!t1 & !t2){

        if(!input$byplot){
          req(index$index)  # Ensure mosaic_data$mosaic is not NULL
          req(mosaic_data$mosaic)
          req(basemap$map)
          req(shptemp$shapefile)
          waiter_show(
            html = tagList(
              spin_google(),
              h2("Analyzing the mosaic. Please, wait.")
            ),
            color = "#228B227F"
          )

          res <-
            mosaic_analyze(mosaic = mosaic_data$mosaic,
                           basemap = basemap$map,
                           shapefile = shptemp$shapefile,
                           indexes = index$index,
                           mask = maskval$mask,
                           plot = FALSE,
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
                           verbose = FALSE)
        } else{
          if(!input$parallelanalysis){
            shp <- do.call(rbind,
                           lapply(shptemp$shapefile, function(x){
                             x |> dplyr::select(geometry)
                           }))
            # Analyze the mosaic by plot
            bind <- list()
            progressSweetAlert(
              session = session, id = "myprogress",
              title = "Start",
              display_pct = TRUE,
              value = 0,
              total = nrow(shp)
            )
            if(!is.null(index$index)){
              indexnull <- FALSE
            } else{
              indexnull <- TRUE
            }
            for (i in 1:nrow(shp)) {
              updateProgressBar(
                session = session,
                id = "myprogress",
                value = i,
                title = paste0("Working in progress, Please, wait."),
                total = nrow(shp)
              )
              if(indexnull){
                indexes <- NULL
              } else{
                indexes <- terra::crop(index$index, terra::vect(shp$geometry[[i]]) |> terra::ext())
              }
              bind[[paste0("P", leading_zeros(i, 4))]] <-
                mosaic_analyze(terra::crop(mosaic_data$mosaic, terra::vect(shp$geometry[[i]]) |> terra::ext()),
                               indexes = indexes,
                               mask = maskval$mask,
                               plot = FALSE,
                               shapefile = shp[i, ],
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
                               verbose = FALSE)
            }
          } else{

            req(input$numworkers)
            cl <- parallel::makeCluster(input$numworkers)
            doParallel::registerDoParallel(cl)
            waiter_show(
              html = tagList(
                spin_google(),
                h2(paste0("Analyzing the mosaic using parallel processing in multiple sessions (",input$numworkers ,"). Please, wait."))
              ),
              color = "#228B227F"
            )

            ## declare alias for dopar command
            `%dopar%` <- foreach::`%dopar%`
            tmpterra <- tempdir()
            if(!is.null(index$index)){
              mosaic_export(index$index, paste0(tmpterra, "/tmpindex.tif"), overwrite = TRUE)
              indexnull <- FALSE
            } else{
              indexnull <- TRUE
            }
            on.exit({
              parallel::stopCluster(cl)
              if(!is.null(index$index)){
                file.remove(paste0(tmpterra, "/tmpindex.tif"))
              }
            })
            shp <- reactive(shp)()
            segment_plot <- input$segmentplot
            segment_individuals <- input$segmentindividuals
            segmentplot <- input$segmentplot
            watershed <- input$watershed
            tolerance <- input$tolerance
            extension <- input$extension
            invert <- input$invertindex
            # summarize_fun <- input$summarizefun
            summarize_quantiles <- quantiles
            include_if <- input$includeif
            threshold <- ifelse(input$threshold == "Otsu", "Otsu", input$threshvalue)
            filter <- ifelse(input$filter, input$filterval, FALSE)
            lower_noise <- input$lower_noise
            simplify <- input$simplify
            pathmosaic <- pathmosaic$path

            bind <-
              foreach::foreach(i = 1:nrow(shp),
                               .packages = c("pliman")) %dopar%{
                                 if(indexnull){
                                   indexes <- NULL
                                 } else{
                                   indexes <- terra::crop(mosaic_input(paste0(tmpterra, "/tmpindex.tif")), terra::vect(shp$geometry[[i]]) |> terra::ext())
                                 }
                                 mosaic_analyze(terra::crop(mosaic_input(pathmosaic), terra::vect(shp$geometry[[i]]) |> terra::ext()),
                                                indexes = indexes,
                                                mask = maskval$mask,
                                                plot = FALSE,
                                                shapefile = shp[i, ],
                                                segment_plot = segment_plot,
                                                segment_individuals = segment_individuals,
                                                simplify = simplify,
                                                segment_index = indcomp,
                                                watershed = watershed,
                                                tolerance = tolerance,
                                                extension = extension,
                                                invert = invert,
                                                summarize_fun = summf,
                                                summarize_quantiles = summarize_quantiles,
                                                include_if = include_if,
                                                threshold = threshold,
                                                filter = filter,
                                                lower_noise = lower_noise,
                                                lower_size = lower_size,
                                                upper_size = upper_size,
                                                topn_lower = topn_lower,
                                                topn_upper = topn_upper,
                                                verbose = FALSE)
                               }

            req(bind)
            names(bind) <- paste0("P", leading_zeros(1:length(bind), 4))

          }


          if(is.null(bind[[1]]$result_individ_map)){
            result_individ_map <- NULL
          }
          if(is.null(bind[[1]]$result_indiv)){
            result_indiv <- result_plot_summ <- NULL
          } else{
            result_indiv <- dplyr::bind_rows(
              lapply(bind, function(x){
                tmp <- x$result_indiv
                tmp$plot_id <- NULL
                tmp
              }),
              .id = "plot_id"
            ) |>
              dplyr::relocate(plot_id, .after = block) |>
              sf::st_as_sf()

            result_plot_summ <- dplyr::bind_rows(
              lapply(bind, function(x){
                tmp <- x$result_plot_summ
                tmp$plot_id <- NULL
                tmp
              }),
              .id = "plot_id"
            ) |>
              dplyr::relocate(plot_id, .after = block) |>
              sf::st_as_sf()
          }

          result_plot <- dplyr::bind_rows(
            lapply(bind, function(x){
              tmp <- x$result_plot
              tmp$plot_id <- NULL
              tmp
            }),
            .id = "plot_id"
          ) |>
            dplyr::relocate(plot_id, .after = block) |>
            sf::st_as_sf()

          res <-
            list(result_plot = result_plot,
                 result_plot_summ = result_plot_summ,
                 result_indiv = result_indiv,
                 result_individ_map = NULL,
                 map_plot = NULL,
                 map_indiv = NULL)

          closeSweetAlert(session = session)
        }

        req(res)
        if("data" %in% colnames(res$result_plot)){
          if(input$segmentindividuals){
            unndata <-
              res$result_indiv |>
              sf::st_drop_geometry() |>
              dplyr::select(block, plot_id, individual, data) |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(block, plot_id, individual) |>
              dplyr::summarise(across(where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")

            result_indiv <- dplyr::left_join(res$result_indiv |> dplyr::select(-data), unndata, by = dplyr::join_by(block, plot_id, individual))
            unndata <-
              res$result_plot |>
              sf::st_drop_geometry() |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(block, plot_id) |>
              dplyr::summarise(across(where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop") |>
              sf::st_drop_geometry()
            result_plot_summ<- dplyr::left_join(res$result_plot_summ, unndata, by = dplyr::join_by(block, plot_id))

            result_plot <-
              res$result_plot |>
              # sf::st_drop_geometry() |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(block, plot_id) |>
              dplyr::summarise(across(where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")
          }

          if(input$segmentplot){
            unndata <-
              res$result_plot |>
              sf::st_drop_geometry() |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(block, plot_id) |>
              dplyr::summarise(across(where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop") |>
              sf::st_drop_geometry() |>
              dplyr::select(-c(3:5))
            result_plot <- dplyr::left_join(res$result_plot |> dplyr::select(-data), unndata, by = dplyr::join_by(block, plot_id))
            result_indiv <- res$result_indiv
            result_plot_summ <- res$result_plot_summ
          }
          if(!input$segmentindividuals & !input$segmentplot){
            result_plot <-
              res$result_plot |>
              # sf::st_drop_geometry() |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(block, plot_id) |>
              dplyr::summarise(across(where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")
            result_indiv <- res$result_indiv
            result_plot_summ <- res$result_plot_summ
          }
        } else{
          result_indiv <- res$result_indiv
          result_plot_summ <- res$result_plot_summ
          result_plot <- res$result_plot
        }

        # if(!is.null(indcomp)){
        if(input$segmentindividuals){
          updateSelectInput(session, "plotattribute", choices = names(result_plot_summ), selected = "coverage")
          updateSelectInput(session, "indivattribute", choices = names(result_indiv), selected = "diam_mean")
          output$vbnplots <- renderValueBox({
            valueBox(
              value = nrow(result_plot),
              subtitle = "Number of plots",
              color = "success",
              icon = icon("table-cells")
            )
          })
          output$vbnindiv <- renderValueBox({
            valueBox(
              value = nrow(result_indiv),
              subtitle = "Number of individuals",
              color = "success",
              icon = icon("seedling")
            )
          })
          output$vbnindivplotmean <- renderValueBox({
            valueBox(
              value = round(nrow(result_indiv) / nrow(result_plot), 3),
              subtitle = "Average number of individuals per plot",
              color = "success",
              icon = icon("seedling")
            )
          })
          output$vbcoverage <- renderValueBox({
            valueBox(
              value = round(mean(result_plot_summ$coverage), 3),
              subtitle = "Average canopy coverage",
              color = "success",
              icon = icon("chart-pie")
            )
          })
          output$largerindiv <- renderValueBox({
            valueBox(
              value = round(max(result_indiv$diam_mean), 3),
              subtitle = "Larger individual (diameter)",
              color = "success",
              icon = icon("up-right-and-down-left-from-center")
            )
          })
          output$smallerindiv <- renderValueBox({
            valueBox(
              value = round(min(result_indiv$diam_mean), 3),
              subtitle = "Smaller individual (diameter)",
              color = "success",
              icon = icon("up-right-and-down-left-from-center")
            )
          })

          output$boxresults <- renderPlotly({

            if(input$summarizefunoutput == "none"){
              plot_ind <-
                result_plot_summ |>
                as.data.frame() |>
                dplyr::select(input$plotattribute) |>
                stack() |>
                dplyr::rename(values = values, index = ind)
            } else{
              b <-
                result_plot_summ |>
                as.data.frame() |>
                dplyr::select(dplyr::contains(input$summarizefunoutput)) |>
                stack()
              b$ind <- as.character(b$ind)
              plot_ind <- pliman::separate_col(b, ind, into = c("statistic", "index"))
              rm(b)
            }
            p <-
              ggplot(plot_ind, aes(y = values)) +
              geom_boxplot(fill = "#28a745") +
              facet_wrap(~index, scales = "free_y") +
              theme_bw() +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())

            plotly::ggplotly(p, dynamicTicks = TRUE)
          })
          output$plotatribres <- renderPlotly({
            plotatt <-
              result_plot_summ |>
              as.data.frame() |>
              dplyr::select(dplyr::contains(input$plotattribute))
            patr <-
              ggplot(plotatt, aes( y = !!sym(input$plotattribute))) +
              geom_boxplot(fill = "#28a745") +
              theme_bw() +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())
            ggplotly(patr)
          })

          output$indivatribres <- renderPlotly({
            indivp <-
              result_plot_summ |>
              as.data.frame() |>
              dplyr::select(dplyr::contains(input$indivattribute))
            indat <-
              ggplot(indivp, aes( y = !!sym(input$indivattribute))) +
              geom_boxplot(fill = "#28a745") +
              theme_bw() +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())
            ggplotly(indat)
          })

          output$resultplottab <- DT::renderDataTable(
            result_plot_summ |>
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

          output$resultsindivtab <- DT::renderDataTable(
            result_indiv |>
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

          mod_download_shapefile_server("downresplot", terra::vect(result_plot_summ), name = "plot_level_results")
          mod_download_shapefile_server("downresindiv", terra::vect(result_indiv), name = "individual_results")

        } else if(input$segmentplot){
          updateSelectInput(session, "plotattribute", choices = names(result_plot), selected = "coverage")

          output$vbnplots <- renderValueBox({
            valueBox(
              value = nrow(result_plot),
              subtitle = "Number of plots",
              color = "success",
              icon = icon("table-cells")
            )
          })
          output$vbcoverage <- renderValueBox({
            valueBox(
              value = round(mean(result_plot$coverage), 3),
              subtitle = "Average canopy coverage",
              color = "success",
              icon = icon("chart-pie")
            )
          })
          output$mincoverage <- renderValueBox({
            valueBox(
              value = round(min(result_plot$coverage), 3),
              subtitle = "Minmum canopy coverage",
              color = "success",
              icon = icon("chart-pie")
            )
          })
          output$maxcoverage <- renderValueBox({
            valueBox(
              value = round(max(result_plot$coverage), 3),
              subtitle = "Maximum canopy coverage",
              color = "success",
              icon = icon("chart-pie")
            )
          })
          output$boxresults <- renderPlotly({
            if(input$summarizefunoutput == "none"){
              plot_ind <-
                result_plot |>
                as.data.frame() |>
                dplyr::select(input$plotattribute) |>
                stack() |>
                dplyr::rename(values = values, index = ind)
            } else{
              b <-
                result_plot |>
                as.data.frame() |>
                dplyr::select(dplyr::contains(input$summarizefunoutput)) |>
                stack()
              b$ind <- as.character(b$ind)
              plot_ind <- pliman::separate_col(b, ind, into = c("statistic", "index"))
              rm(b)
            }
            p <-
              ggplot(plot_ind, aes(y = values)) +
              geom_boxplot(fill = "#28a745") +
              facet_wrap(~index, scales = "free_y") +
              theme_bw() +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())

            plotly::ggplotly(p, dynamicTicks = TRUE)
          })


          output$resultplottab <- DT::renderDataTable(
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


        } else {
          updateSelectInput(session, "plotattribute", choices = names(result_plot), selected = names(result_plot)[[3]])
          output$boxresults <- renderPlotly({
            if(input$summarizefunoutput == "none"){
              plot_ind <-
                result_plot |>
                as.data.frame() |>
                dplyr::select(input$plotattribute) |>
                stack() |>
                dplyr::rename(values = values, index = ind)
            } else{
              b <-
                result_plot |>
                as.data.frame() |>
                dplyr::select(dplyr::contains(input$summarizefunoutput)) |>
                stack()
              b$ind <- as.character(b$ind)
              plot_ind <- pliman::separate_col(b, ind, into = c("statistic", "index"))
              rm(b)
            }
            p <-
              ggplot(plot_ind, aes(y = values)) +
              geom_boxplot(fill = "#28a745") +
              facet_wrap(~index, scales = "free_y") +
              theme_bw() +
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())


            plotly::ggplotly(p, dynamicTicks = TRUE)
          })

          output$resultplottab <- DT::renderDataTable(
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
        }
        # }
        waiter_hide()

        if(!is.null(summf)){
          sendSweetAlert(
            session = session,
            title = "Mosaic successfully analyzed!!",
            text = "The mosaic has been analyzed and the results can now be seen in the tabs",
            type = "success"
          )
        } else{
          sendSweetAlert(
            session = session,
            title = "Mosaic successfully analyzed!!",
            text = "To see the raw data, go to 'Configure the output' tab, and assign the results to a variable in the R environment",
            type = "success"
          )
        }
        mapshape <- reactiveValues(mapshape = NULL)
        bmshape <- reactiveValues(bmshape = NULL)
        mapindiv <- reactiveValues(mapindiv = NULL)

        # if(!is.null(summf)){
        observe({
          req(input$plotattribute)
          req(input$indivattribute)

          if(input$segmentindividuals){

            mshp <- shapefile_view(result_plot_summ,
                                   attribute = input$plotattribute,
                                   color_regions = return_colors(input$palplot),
                                   alpha.regions = input$alpharesplot)
            bmshp <- mapview::mapview(result_plot, alpha.regions = 0, legend = FALSE)
            indshp <-
              shapefile_view(result_indiv,
                             attribute = input$indivattribute,
                             color_regions = return_colors(input$palind),
                             alpha.regions = input$alpharesindiv)
          } else if(input$segmentplot){

            if((!input$plotattribute %in% colnames(result_plot)) & !is.null(summf)){
              attrib <- paste0(input$summarizefun[[1]], ".", input$segmentindex)
            } else{
              attrib <- input$plotattribute
            }
            mshp <- shapefile_view(result_plot,
                                   attribute = attrib,
                                   color_regions = return_colors(input$palplot),
                                   alpha.regions = input$alpharesplot)
            bmshp <- mapview::mapview(result_plot, alpha.regions = 0, legend = FALSE)
            indshp <- NULL
          } else{
            if((!input$plotattribute %in% colnames(result_plot)) & !is.null(summf)){
              attrib <- paste0(input$summarizefun[[1]], ".", input$segmentindex)
            } else{
              attrib <- input$plotattribute
            }
            mshp <- shapefile_view(result_plot,
                                   attribute = attrib,
                                   color_regions = return_colors(input$palplot),
                                   alpha.regions = input$alpharesplot)
            bmshp <- mapview::mapview(result_plot, alpha.regions = 0, legend = FALSE)
            indshp <- NULL

          }
          mapshape$mapshape <- mshp
          bmshape$bmshape <- bmshp
          mapindiv$mapindiv <- indshp

        })
        # plot summary
        output$resultsplotmap <- renderLeaflet({
          ((basemap$map +  bmshape$bmshape)  | mapshape$mapshape)@map
        })
        output$resultsindivmap <- renderLeaflet({
          if(input$segmentindividuals){
            ((basemap$map +  bmshape$bmshape)  | (mapshape$mapshape + mapindiv$mapindiv))@map
          }
        })

        output$downloadplotmap <- downloadHandler(
          filename = paste0( Sys.Date(), "map_plot", ".png"),
          content = function(file) {
            mapshot2(x = (basemap$map + mapshape$mapshape),
                     file = file,
                     cliprect = "viewport"
            )
          }
        )
        output$downloadindivmap <- downloadHandler(
          filename = paste0( Sys.Date(), "map_individual", ".png"),
          content = function(file) {
            mapshot2(x = (basemap$map + mapindiv$mapindiv),
                     file = file,
                     cliprect = "viewport"
            )
          }
        )
        # }
      }

      # send the results to the global environment
      observeEvent(input$savetoglobalenv, {
        if(input$segmentindividuals){
          report <-
            list(result_plot = res$result_plot,
                 result_plot_summ = res$result_plot_summ,
                 result_individ = res$result_indiv,
                 map_plot = (basemap$map + mapshape$mapshape),
                 map_individual = (basemap$map + mapindiv$mapindiv),
                 shapefile = shptemp$shapefile)
        } else if(input$segmentplot){
          report <-
            list(result_plot = res$result_plot,
                 map_plot = (basemap$map + mapshape$mapshape),
                 shapefile = shptemp$shapefile)
        } else{
          report <-
            list(result_plot = res$result_plot,
                 map_plot = (basemap$map + mapshape$mapshape),
                 shapefile = shptemp$shapefile)

        }
        if (exists(input$globalvarname, envir = globalenv())) {
          sendSweetAlert(
            session = session,
            title = "Error",
            text = paste0("The object'", input$globalvarname, "' already exists in the global environment. Please, change the name."),
            type = "success"
          )
        } else {
          assign(input$globalvarname, report, envir = globalenv())
          ask_confirmation(
            inputId = "myconfirmation",
            type = "warning",
            title = "Close the App?",
            text = paste0("The object'", input$globalvarname, "' has been created in the Global environment. To access the created object, you need first to stop the App. Do you really want to close the app now?"),
            btn_labels = c("Nope", "Yep"),
            btn_colors = c("#FE642E", "#04B404")
          )
        }
      })

      observe({
        if (!is.null(input$myconfirmation)) {
          if (input$myconfirmation) {
            stopApp()
          } else {
            # Do something else or simply return if the confirmation is false
            return()
          }
        }
      })


    })


  })
}

## To be copied in the UI
# mod_analyze_ui("analyze_1")

## To be copied in the server
# mod_analyze_server("analyze_1")
