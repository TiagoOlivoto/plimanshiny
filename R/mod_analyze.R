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
            selectInput(
              ns("activeshape"),
              label = "Shapefile",
              choices = NULL
            ),
            selectInput(
              ns("activeindex"),
              label = "Index to analyze",
              choices = NULL
            ),
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
                           prettyCheckbox(
                             inputId = ns("palindrev"),
                             label = "Reverse",
                             value = FALSE,
                             icon = icon("check"),
                             status = "success",
                             animation = "rotate"
                           ),
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
mod_analyze_server <- function(id, mosaic_data, basemap, shapefile, index, pathmosaic, dfs){
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
        if(input$mapindividuals){
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
              fluidRow(
                col_4(
                  materialSwitch(
                    inputId = ns("compareslidermap"),
                    label = "Comparison slider?",
                    value = FALSE,
                    status = "success"
                  )
                ),
                col_8(
                  downloadBttn(ns("downloadplotmap"),
                               label = "Take a shot",
                               style = "pill")
                )
              ),
              leafletOutput(ns("resultsplotmap"), height = "680px")  |> add_spinner()
            ),
            tabPanel(
              title = "Results plot",
              reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
            ),
            tabPanel(
              title = "Map individuals",
              fluidRow(
                col_4(
                  materialSwitch(
                    inputId = ns("compareslidermapind"),
                    label = "Comparison slider?",
                    value = FALSE,
                    status = "success"
                  )
                ),
                col_8(
                  downloadBttn(ns("downloadindividmap"),
                               label = "Take a shot",
                               style = "pill")
                )
              ),
              leafletOutput(ns("resultsindivmap"), height = "720px")  |> add_spinner()
            ),
            tabPanel(
              title = "Results individuals",
              reactable::reactableOutput(ns("resultsindivtab"), height = "700px")  |> add_spinner()
            ),
            tabPanel(
              title = "Distances",
              reactable::reactableOutput(ns("resultdist"), height = "700px")  |> add_spinner()
            )
          )
        } else{
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
              fluidRow(
                col_4(
                  materialSwitch(
                    inputId = ns("compareslidermap"),
                    label = "Comparison slider?",
                    value = FALSE,
                    status = "success"
                  )
                ),
                col_8(
                  downloadBttn(ns("downloadplotmap"),
                               label = "Take a shot",
                               style = "pill")
                )
              ),
              leafletOutput(ns("resultsplotmap"), height = "680px")  |> add_spinner()
            ),
            tabPanel(
              title = "Results plot",
              reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
            ),
            tabPanel(
              title = "Map individuals",
              fluidRow(
                col_4(
                  materialSwitch(
                    inputId = ns("compareslidermapind"),
                    label = "Comparison slider?",
                    value = FALSE,
                    status = "success"
                  )
                ),
                col_8(
                  downloadBttn(ns("downloadindividmap"),
                               label = "Take a shot",
                               style = "pill")
                )
              ),
              leafletOutput(ns("resultsindivmap"), height = "720px")  |> add_spinner()
            ),
            tabPanel(
              title = "Results individuals",
              reactable::reactableOutput(ns("resultsindivtab"), height = "700px")  |> add_spinner()
            )
          )
        }
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
            fluidRow(
              col_4(
                materialSwitch(
                  inputId = ns("compareslidermap"),
                  label = "Comparison slider?",
                  value = FALSE,
                  status = "success"
                )
              ),
              col_8(
                downloadBttn(ns("downloadplotmap"),
                             label = "Take a shot",
                             style = "pill")
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "680px")  |> add_spinner()
          ),
          tabPanel(
            title = "Results plot",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
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
            fluidRow(
              col_4(
                materialSwitch(
                  inputId = ns("compareslidermap"),
                  label = "Comparison slider?",
                  value = FALSE,
                  status = "success"
                )
              ),
              col_8(
                downloadBttn(ns("downloadplotmap"),
                             label = "Take a shot",
                             style = "pill")
              )
            ),
            leafletOutput(ns("resultsplotmap"), height = "680px")  |> add_spinner()
          ),
          tabPanel(
            title = "Results plot",
            reactable::reactableOutput(ns("resultplottab"), height = "700px")  |> add_spinner()
          )
        )
      }
    })

    shptemp <- reactiveValues(shapefile = NULL)
    observe({
      updateSelectInput(session, "availablemasks", choices = c("none", setdiff(names(mosaic_data), "mosaic")), selected = "none")
      updatePickerInput(session, "summarizefunoutput", choices = input$summarizefun, selected = input$summarizefunoutput[[1]])
    })
    observe({
      updateSelectInput(session, "activeindex",
                        choices = names(index))
    })
    observe({
      updateSelectInput(session, "activeshape",
                        choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")))
    })

    maskval <- reactiveValues(mask = NULL)

    observe({
      req(input$activeindex)
      req(input$activeshape)
      # shptemp$shapefile <- shapefile[[input$activeshape]]$data
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
          if(is.null(index[[input$activeindex]]$data)){
            updateSelectInput(session, "segmentindex", choices = pliman_indexes())
          } else{

          }
        } else{
          updateSelectInput(session, "segmentindex", choices = names(index[[input$activeindex]]$data))
        }

      }
      availablecl <- parallel::detectCores()
      updateNumericInput(session, "numworkers", value = round(availablecl * 0.4), max = availablecl - 2)

    })

    output$baseshapeindex <- renderUI({
      if(!input$byplot){
        req(input$activeindex)
        # req(index[[input$activeindex]]$data)
        req(mosaic_data$mosaic)
        req(basemap$map)
        req(input$segmentindex)
        layer <- ifelse(input$segmentindex != "", input$segmentindex, 1)
        m1 <-  basemap$map + mapview::mapview(shapefile[[input$activeshape]]$data |> dplyr::bind_rows(),
                                              z.col = "plot_id",
                                              legend = FALSE)
        m2 <- mosaic_view(index[[input$activeindex]]$data[[layer]], show = "index")
        leafsync::sync(m1@map, m2)
      }
    })

    output$previousdensity <- renderPlot({
      if(!input$byplot){
        if(is.null(maskval$mask)){
          req(index[[input$activeindex]]$data)
          layer <- ifelse(input$segmentindex != "", input$segmentindex, 1)
          ots <- ifelse(input$threshold == "Otsu", otsu(na.omit(terra::values(index[[input$activeindex]]$data[[layer]]))), input$threshvalue)
          output$previoussegment <- renderPlot({
            if(input$invertindex){
              maskplot <- index[[input$activeindex]]$data[[layer]] < ots
            } else{
              maskplot <- index[[input$activeindex]]$data[[layer]] > ots
            }
            terra::plot(maskplot)
          })
          terra::density(index[[input$activeindex]]$data[[layer]],
                         main = paste0(names(index[[input$activeindex]]$data[[layer]]), " - Otsu: ", round(ots, 4)))
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
      if(c(!input$byplot & is.null(index[[input$activeindex]]$data)) | is.null(mosaic_data$mosaic) | is.null(basemap$map) | is.null(shapefile[[input$activeshape]]$data)){
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

      if(!t1 & !t2){

        if(!input$byplot){
          req(index[[input$activeindex]]$data)  # Ensure mosaic_data$mosaic is not NULL
          req(mosaic_data$mosaic)
          req(basemap$map)
          req(shapefile[[input$activeshape]]$data)

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
                           shapefile = shapefile[[input$activeshape]]$data,
                           indexes = index[[input$activeindex]]$data,
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
                           map_individuals = input$mapindividuals,
                           map_direction = input$mapdirection,
                           verbose = FALSE)
          # assign("resulttt", res, envir = globalenv())
          centr <-
            suppressWarnings(
              res$result_plot |>
                sf::st_centroid() |>
                sf::st_coordinates() |>
                as.data.frame() |>
                setNames(c("x", "y"))
            )
          centr <- centr |> dplyr::bind_cols(res$result_plot |> sf::st_drop_geometry() |> dplyr::select(block, plot_id))
          res$result_plot <- res$result_plot |> dplyr::left_join(centr, by = dplyr::join_by(block, plot_id)) |> dplyr::relocate(x, y, .after = plot_id)
          if(!is.null(res$result_plot_summ)){
            res$result_plot_summ <- res$result_plot_summ |> dplyr::left_join(centr, by = dplyr::join_by(block, plot_id)) |> dplyr::relocate(x, y, .after = plot_id)
          }

        } else{
          if(!input$parallelanalysis){
            shp <- do.call(rbind,
                           lapply(shapefile[[input$activeshape]]$data, function(x){
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
            if(!is.null(index[[input$activeindex]]$data)){
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
                indexes <- terra::crop(index[[input$activeindex]]$data, terra::vect(shp$geometry[[i]]) |> terra::ext())
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
                               map_individuals = input$mapindividuals,
                               map_direction = input$mapdirection,
                               verbose = FALSE)
            }
          } else{

            req(input$numworkers)
            nworkers <- input$numworkers
            future::plan(future::multisession, workers = nworkers)
            on.exit(future::plan(future::sequential))
            `%dofut%` <- doFuture::`%dofuture%`
            waiter_show(
              html = tagList(
                spin_google(),
                h2(paste0("Analyzing the mosaic using parallel processing in multiple sessions (",input$numworkers ,"). Please, wait."))
              ),
              color = "#228B227F"
            )

            tmpterra <- tempdir()
            if(!is.null(index[[input$activeindex]]$data)){
              mosaic_export(index[[input$activeindex]]$data, paste0(tmpterra, "/tmpindex.tif"), overwrite = TRUE)
              indexnull <- FALSE
            } else{
              indexnull <- TRUE
            }
            on.exit({
              parallel::stopCluster(cl)
              if(!is.null(index[[input$activeindex]]$data)){
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
            mapindividuals = input$mapindividual
            mapdirection = input$mapdirection

            bind <-
              foreach::foreach(i = 1:nrow(shp)) %dofut%{
                if(indexnull){
                  indexes <- NULL
                } else{
                  indexes <- terra::crop(terra::rast(paste0(tmpterra, "/tmpindex.tif")), terra::vect(shp$geometry[[i]]) |> terra::ext())
                }
                mosaic_analyze(terra::crop(terra::rast(pathmosaic), terra::vect(shp$geometry[[i]]) |> terra::ext()),
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
                               map_individuals = mapindividuals,
                               map_direction = mapdirection,
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

          centr <-
            suppressWarnings(
              res$result_plot |>
                sf::st_centroid() |>
                sf::st_coordinates() |>
                as.data.frame() |>
                setNames(c("x", "y"))
            )

          res <-
            list(result_plot = result_plot |> dplyr::left_join(centr, by = dplyr::join_by(block, plot_id)) |> dplyr::relocate(x, y, .after = plot_id),
                 result_plot_summ = result_plot_summ |> dplyr::left_join(centr, by = dplyr::join_by(block, plot_id)) |> dplyr::relocate(x, y, .after = plot_id),
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
              dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
              sf::st_drop_geometry() |>
              dplyr::select(unique_id, block, plot_id, individual, data) |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(unique_id, block, plot_id, individual) |>
              dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")

            result_indiv <- dplyr::left_join(res$result_indiv |> dplyr::select(-data), unndata, by = dplyr::join_by(unique_id, block, plot_id, individual))
            unndata <-
              res$result_plot |>
              dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
              sf::st_drop_geometry() |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(unique_id, block, plot_id) |>
              dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop") |>
              sf::st_drop_geometry()
            result_plot_summ<- dplyr::left_join(res$result_plot_summ, unndata, by = dplyr::join_by(unique_id, block, plot_id))

            result_plot <-
              res$result_plot |>
              dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(unique_id, block, plot_id) |>
              dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")
          }

          if(input$segmentplot){
            unndata <-
              res$result_plot |>
              dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
              sf::st_drop_geometry() |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(unique_id, block, plot_id) |>
              dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop") |>
              sf::st_drop_geometry() |>
              dplyr::select(-c(3:5))
            result_plot <- dplyr::left_join(res$result_plot |> dplyr::select(-data), unndata, by = dplyr::join_by(unique_id, block, plot_id))
            result_indiv <- res$result_indiv
            result_plot_summ <- res$result_plot_summ
          }
          if(!input$segmentindividuals & !input$segmentplot){
            result_plot <-
              res$result_plot |>
              dplyr::mutate(unique_id = dplyr::row_number(), .before = 1) |>
              tidyr::unnest(cols = data) |>
              dplyr::group_by(unique_id, block, plot_id) |>
              dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x){mean(x, na.rm = TRUE)}), .groups = "drop")
            result_indiv <- res$result_indiv
            result_plot_summ <- res$result_plot_summ
          }
        } else{
          if(is.null(res$result_indiv)){
            result_indiv <- NULL
          } else{
            result_indiv <-
              res$result_indiv |>
              dplyr::mutate(unique_id = dplyr::row_number()) |>
              dplyr::relocate(unique_id, .before = 1)
          }
          if(is.null(res$result_plot_summ)){
            result_plot_summ <- NULL
          } else{
            result_plot_summ <-
              res$result_plot_summ |>
              dplyr::mutate(unique_id = dplyr::row_number()) |>
              dplyr::relocate(unique_id, .before = 1)
          }
          result_plot <-
            res$result_plot |>
            dplyr::mutate(unique_id = dplyr::row_number()) |>
            dplyr::relocate(unique_id, .before = 1)
        }


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
            if(input$summarizefunoutput[[1]] == "none"){
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
                dplyr::select(dplyr::contains(input$summarizefunoutput[[1]])) |>
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

          output$resultplottab <- reactable::renderReactable(
            result_plot_summ |>
              sf::st_drop_geometry() |>
              roundcols(digits = 3) |>
              render_reactable()
          )

          output$resultsindivtab <- reactable::renderReactable(
            result_indiv |>
              sf::st_drop_geometry() |>
              roundcols(digits = 3) |>
              render_reactable(
                columns = list(
                  x = colDef(minWidth = 150),
                  y = colDef(minWidth = 150)
                )
              )
          )
          if(input$mapindividuals){
            dists <- purrr::map_dfr(res[["result_individ_map"]][["distances"]], data.frame,
                                    .id = "plot_id")
            means <- data.frame(res[["result_individ_map"]][["means"]])
            means <- data.frame(plot_id = rownames(means),
                                mean_dist = means[, 1])
            cvs <- data.frame(res[["result_individ_map"]][["cvs"]])
            cvs <- data.frame(plot_id = rownames(cvs),
                              cv = cvs[, 1])
            dfdists <-
              purrr::reduce(list(dists, means, cvs), dplyr::left_join,  by = dplyr::join_by(plot_id)) |>
              tidyr::separate_wider_delim(plot_id, delim = "_", names = c("block", "plot_id")) |>
              setNames(c("block", "plot_id", "dist", "mean_dist", "cv"))
            # send to datasets
            dfs[["result_individ_map"]] <- create_reactval("result_individ_map", dfdists)
            # render the table
            output$resultdist <- reactable::renderReactable({
              dfdists |> render_reactable()
            })
          }


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
            if(input$summarizefunoutput[[1]] == "none"){
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
                dplyr::select(dplyr::contains(input$summarizefunoutput[[1]])) |>
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


          output$resultplottab <- reactable::renderReactable(
            result_plot |>
              sf::st_drop_geometry() |>
              roundcols(digits = 3) |>
              render_reactable()
          )

          mod_download_shapefile_server("downresplot", terra::vect(result_plot), name = "plot_level_results")


        } else {
          sel <- grep(input$summarizefunoutput[[1]], names(result_plot))[1]
          updateSelectInput(session, "plotattribute", choices = names(result_plot), selected = names(result_plot)[[sel]])
          output$boxresults <- renderPlotly({
            if(input$summarizefunoutput[[1]] == "none"){
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
                dplyr::select(dplyr::contains(input$summarizefunoutput[[1]])) |>
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

          output$resultplottab <- reactable::renderReactable(
            result_plot |>
              sf::st_drop_geometry() |>
              roundcols(digits = 3) |>
              render_reactable()
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

        observe({
          req(input$plotattribute)
          req(input$indivattribute)

          if(input$segmentindividuals){
            mshp <- shapefile_view(result_plot_summ,
                                   attribute = input$plotattribute,
                                   color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                                   alpha.regions = input$alpharesplot)
            bmshp <- mapview::mapview(result_plot, alpha.regions = 0, legend = FALSE)
            indshp <-
              shapefile_view(result_indiv,
                             attribute = input$indivattribute,
                             color_regions = return_colors(input$palind, reverse = input$palindrev),
                             alpha.regions = input$alpharesindiv)
          } else if(input$segmentplot){

            if((!input$plotattribute %in% colnames(result_plot)) & !is.null(summf)){
              attrib <- paste0(input$summarizefun[[1]], ".", input$segmentindex)
            } else{
              attrib <- input$plotattribute
            }
            mshp <- shapefile_view(result_plot,
                                   attribute = attrib,
                                   color_regions = return_colors(input$palplot, reverse = input$palplotrev),
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
                                   color_regions = return_colors(input$palplot, reverse = input$palplotrev),
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
          if(input$compareslidermap){
            ((basemap$map +  bmshape$bmshape)  | mapshape$mapshape)@map
          } else{
            (basemap$map +  mapshape$mapshape)@map
          }
        })



        output$resultsindivmap <- renderLeaflet({
          if(input$segmentindividuals){
            if(input$compareslidermapind){
              ((basemap$map +  bmshape$bmshape)  | (mapshape$mapshape + mapindiv$mapindiv))@map
            } else{
              (basemap$map +  mapindiv$mapindiv)@map
            }
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

      # Sent do datasets
      report <- reactive({
        req(res)
        if(input$segmentindividuals){
          list(result_plot = res$result_plot,
               result_plot_summ = res$result_plot_summ,
               result_individ = res$result_indiv,
               result_individ_map = res$result_individ_map,
               map_plot = (basemap$map + mapshape$mapshape),
               map_individual = (basemap$map + mapindiv$mapindiv),
               shapefile = shapefile[[input$activeshape]]$data)
        } else if(input$segmentplot){
          list(result_plot = res$result_plot,
               map_plot = (basemap$map + mapshape$mapshape),
               shapefile = shapefile[[input$activeshape]]$data)
        } else{
          list(result_plot = res$result_plot,
               map_plot = (basemap$map + mapshape$mapshape),
               shapefile = shapefile[[input$activeshape]]$data)

        }
      })

      observe({
        req(report())
        dfs[["result_plot"]] <- create_reactval("result_plot", report()$result_plot |> sf::st_drop_geometry())
        if(!is.null(report()$result_plot_summ)){
          dfs[["result_plot_summ"]] <- create_reactval("result_plot_summ", report()$result_plot_summ|> sf::st_drop_geometry())
        }
        if(!is.null(report()$result_individ)){
          dfs[["result_individ"]] <- create_reactval("result_individ", report()$result_individ|> sf::st_drop_geometry())
        }
      })

      # send the results to the global environment
      observeEvent(input$savetoglobalenv, {

        if (exists(input$globalvarname, envir = globalenv())) {
          sendSweetAlert(
            session = session,
            title = "Error",
            text = paste0("The object '", input$globalvarname, "' already exists in the global environment. Please, change the name."),
            type = "success"
          )
        } else {
          assign(input$globalvarname, report(), envir = globalenv())
          ask_confirmation(
            inputId = "myconfirmation",
            type = "warning",
            title = "Close the App?",
            text = paste0("The object '", input$globalvarname, "' has been created in the Global environment. To access the created object, you need first to stop the App. Do you really want to close the app now?"),
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
