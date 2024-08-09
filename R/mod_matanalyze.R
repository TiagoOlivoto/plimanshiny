#' matanalyze UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_matanalyze_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Plant Maturity and Heading",
          collapsible = FALSE,
          width = 12,
          height = "790px",
          dateInput(
            ns("sowing"),
            label = "Sowing Date",
            value = Sys.Date()
          ),
          pickerInput(
            ns("dftoedit"),
            label = "Time series data",
            choices = NULL
          ),
          pickerInput(
            ns("flightdate"),
            label = "Flight date",
            choices = NULL
          ),
          pickerInput(
            ns("vegetindex"),
            label = "Predictor Vegetation Index",
            choices = NULL
          ),
          prettyCheckbox(
            inputId = ns("usethresh"),
            label = "Threshold-based methods",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          conditionalPanel(
            condition = "input.usethresh == false", ns = ns,
            pickerInput(
              ns("method"),
              label = "Method for prediction",
              choices = c("Logistic Model L3",
                          "Logistic Model L4",
                          "Logistic Model L5")
            )
          ),
          conditionalPanel(
            condition = "input.usethresh == true", ns = ns,
            pickerInput(
              ns("method"),
              label = "Method for prediction",
              choices = c("LOESS (Volpato et al., 2021)",
                          "Segmented Regression (Volpato et al., 2021)")
            ),
            numericInput(
              ns("thresh"),
              label = "Threshold",
              value = 0
            )
          ),

          textInput(
            ns("saveto"),
            label = "Save results to...",
            value = "time_series_maturity"
          ),
          pickerInput(
            ns("shapefiletoexplore"),
            label = "Shapefile to explore",
            choices = NULL
          ),
          actionBttn(
            ns("predictmat"),
            label = "Predict!",
            icon = icon("check")
          )
        )
      ),
      col_9(
        uiOutput(ns("uimat"))
      )
    )
  )
}

#' matanalyze Server Functions
#'
#' @noRd
mod_matanalyze_server <- function(id, dfs, shapefile, basemap){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #
    observeEvent(input$details, {
      showModal(
        modalDialog(
          title = "Details about the prediction",
          box(
            width = 12,
            headerBorder = FALSE,
            collapsible = TRUE,
            closable = TRUE,
            h2("Volpato et al. (2021)"),
            "Details here...", br(),
            shiny::actionButton(inputId= ns("ref1"),
                                label="Reference",
                                icon = icon("link"),
                                onclick ="window.open('https://onlinelibrary.wiley.com/doi/abs/10.1002/ppj2.20018', '_blank')")
          ),
          footer = NULL,
          easyClose = TRUE,
          size = "xl"
        )
      )

    })

    refs <-           tabPanel(
      title = "Home",
      fluidRow(
        col_9(
          img(src = "www/logomat.png", width = "100%", height = "90%")
        ),
        col_3(
          h2("About"),
          "This module provides the implementation of several methods for estimating plant heading/maturity.
                Methods are grouped into those that uses or not a threshold value.", br(),br(),
          shiny::actionButton(inputId= ns("details"),
                              label="Method details",
                              icon = icon("circle-info")),
          h2("Disclaimer"),
          "We welcome feedback and suggestions about the usefulness of the application and make no guarantee of the correctness,
          reliability, or utility of the results if incorrect selections are made during the steps of DM estimation."
        )
      )
    )
    output$uimat <- renderUI({
      if(input$usethresh){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Overview",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          refs,
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            title = "Fitted model",
            pickerInput(
              inputId = ns("fittedmodel"),
              label = "Select a unique_id to plot the model:",
              choices = NULL,
              options = list(
                "actions-box" = TRUE,
                "live-search" = TRUE,
                "max-options" = 3,
                "max-options-text" = "No more levels allowed"
              )
            ),
            tabPanel("Fitted curve",
                     plotOutput(ns("fittedplot"), height = "550px") |> add_spinner()
            )
          ),
          tabPanel(
            title = "Results",
            reactable::reactableOutput(ns("tabresult"), height = "720px")  |> add_spinner()
          ),
          tabPanel(
            title = "Trait distribution",
            pickerInput(
              ns("histotraits"),
              label = "Select trait(s) to plot:",
              choices = NULL,
              multiple = TRUE
            ),
            plotlyOutput(ns("histograms"), height = "680px")  |> add_spinner()
          ),
          tabPanel(
            title = "Explore field",
            fluidRow(
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "RdYlGn"),

              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_4(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("map"), height = "640px") |> add_spinner()
          )
        )
      } else if(!input$usethresh){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Overview",
          selected = "Home",
          solidHeader = FALSE,
          type = "tabs",
          refs,
          tabPanel(
            title = "Overview",
            plotlyOutput(ns("overview"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            title = "Fitted model",
            pickerInput(
              inputId = ns("fittedmodel"),
              label = "Select a unique_id to plot the model:",
              choices = NULL,
              options = list(
                "actions-box" = TRUE,
                "live-search" = TRUE,
                "max-options" = 3,
                "max-options-text" = "No more levels allowed"
              )
            ),
            bs4TabCard(
              id = "tabs",
              status = "success",
              width = 12,
              height = "600px",
              title = "Results for the fitted model",
              selected = "Fitted curve",
              solidHeader = FALSE,
              tabPanel("Fitted curve",
                       plotOutput(ns("fittedplot"), height = "550px") |> add_spinner()
              ),
              tabPanel("First derivative",
                       plotOutput(ns("fderivate"), height = "550px") |> add_spinner()
              ),
              tabPanel("Second derivative",
                       plotOutput(ns("sderivate"), height = "550px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Results",
            reactable::reactableOutput(ns("tabresult"), height = "720px")  |> add_spinner()
          ),
          tabPanel(
            title = "Trait distribution",
            pickerInput(
              ns("histotraits"),
              label = "Select trait(s) to plot:",
              choices = NULL,
              multiple = TRUE
            ),
            plotlyOutput(ns("histograms"), height = "680px")  |> add_spinner()
          ),

          tabPanel(
            title = "Explore field",
            fluidRow(
              col_3(
                selectInput(ns("plotattribute"),
                            label = "Attribute",
                            choices = NULL)
              ),
              col_3(
                pickerpalette(id, "palplot", selected = "RdYlGn"),

              ),
              col_2(
                prettyCheckbox(
                  inputId = ns("palplotrev"),
                  label = "Reverse",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_4(
                sliderInput(ns("alpharesplot"),
                            label = "Fill opacity",
                            min = 0,
                            max = 1,
                            value = 0.75),
              )
            ),
            leafletOutput(ns("map"), height = "640px") |> add_spinner()
          )
        )
      }
    })
    observe({
      updatePickerInput(session, "dftoedit",
                        choices = c("none", names(dfs)))
    })


    dfactive <- reactiveValues()
    observeEvent(input$dftoedit, {
      req(input$dftoedit)
      if(input$dftoedit != "none"){
        dfactive$df <-
          dfs[[input$dftoedit]]$data |>
          mutate(unique_plot = paste0(block, "_", plot_id)) |>
          convert_numeric_cols()
      }
    })

    observe({
      req(dfactive$df)
      updatePickerInput(session, "vegetindex",
                        choices = colnames(dfactive$df),
                        selected = NA)
    })
    observe({
      req(dfactive$df)
      updatePickerInput(session, "flightdate",
                        choices = colnames(dfactive$df),
                        selected = "date")
    })
    observe({
      updatePickerInput(session, "shapefiletoexplore",
                        choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")))
    })




    output$overview <- renderPlotly({
      req(dfactive$df)
      req(input$vegetindex)
      p <-
        ggplot(dfactive$df, aes(x = .data[[input$flightdate]], y = .data[[input$vegetindex]], group = unique_plot)) +
        geom_smooth(aes(color = unique_plot),
                    show.legend = FALSE,
                    method = 'loess',
                    formula = "y ~ x",
                    se = FALSE,
                    alpha = 0.3) +
        labs(x = input$flightdate, y = input$vegetindex) +
        theme_bw(base_size = 18) +
        theme(panel.grid.minor = element_blank()) +
        scale_colour_grey(start = 0.1, end = 0.9)
      plotly::ggplotly(p)
    })



    observeEvent(input$predictmat, {
      req(dfactive$df)
      req(input$vegetindex)

      waiter_show(
        html = tagList(
          spin_google(),
          h2("Predictions in progress. Please, wait.")
        ),
        color = "#228B227F"
      )

      modl <- reactive({
        if(input$method == "Logistic Model L3"){
          # modl <-
          dfactive$df |>
            mod_L3(predictor = input$vegetindex,
                   flight_date = input$flightdate,
                   sowing_date = input$sowing)
        } else if(input$method == "Logistic Model L4"){
          # modl <-
          dfactive$df |>
            mod_L4(predictor = input$vegetindex,
                   flight_date = input$flightdate,
                   sowing_date = input$sowing)
        } else if(input$method == "Logistic Model L5"){
          # modl <-
          dfactive$df |>
            mod_L5(predictor = input$vegetindex,
                   flight_date = input$flightdate,
                   sowing_date = input$sowing)
        } else if(input$method == "LOESS (Volpato et al., 2021)"){
          # modl <-
          dfactive$df |>
            mod_loess(predictor = input$vegetindex,
                      flight_date = input$flightdate,
                      sowing_date = input$sowing,
                      threshold = input$thresh)

        } else if(input$method == "Logistic Ensamble"){
          # modl <- predict_mat_ensamble

        } else if(input$method == "Segmented Regression"){
          # modl <-
          dfactive$df |>
            mod_segmented(predictor = input$vegetindex,
                          flight_date = input$flightdate,
                          sowing_date = input$sowing,
                          threshold = input$thresh)
        }
      })



      observe({
        updateSelectInput(session, "plotattribute",
                          choices = colnames(modl()),
                          selected = NA)
      })

      observe({
        updatePickerInput(session, "histotraits",
                          choices = colnames(modl()),
                          selected = NA)
      })

      observe({
        req(modl())
        dfs[[input$saveto]] <- create_reactval(input$saveto, modl() |> dplyr::select(-parms))
        # Plot the results
        output$tabresult <- reactable::renderReactable({
          modl() |>
            dplyr::select(-parms) |>
            roundcols(digits = 3) |>
            render_reactable()

        })

      })









      # Plot the histograms
      output$histograms <- renderPlotly({
        req(input$histotraits)

        dfhist <-
          modl() |>
          dplyr::select(dplyr::all_of(input$histotraits)) |>
          dplyr::ungroup() |>
          tidyr::pivot_longer(dplyr::all_of(input$histotraits))


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


      observe({
        levels <- sort(unique(dfactive$df[["unique_plot"]]))
        updatePickerInput(session, "fittedmodel",
                          choices = levels)

      })


      observe({
        # if threshold is not used
        # models L3, L4 and L5
        if(input$usethresh){
          req(input$fittedmodel)
          dfplot <-
            dfactive$df |>
            dplyr::select(dplyr::all_of(c("unique_plot", input$flightdate, input$vegetindex))) |>
            setNames(c("unique_plot", "doy", "vindex")) |>
            dplyr::mutate(doy = as.POSIXlt(doy)$yday + 1 -  (as.POSIXlt(input$sowing)$yday + 1)) |>
            dplyr::filter(!!dplyr::sym("unique_plot") %in% input$fittedmodel)

          dfpars <-
            modl() |>
            filter(unique_plot == input$fittedmodel)

          output$fittedplot <- renderPlot({
            ggplot(dfplot, aes(x = doy, y = vindex)) +
              geom_point() +
              geom_smooth(method = 'loess',
                          formula = "y ~ x",
                          se = FALSE) +
              geom_vline(xintercept = dfpars$maturity, color = "salmon") +
              geom_hline(yintercept = input$thresh, linetype = 2, color = "salmon") +
              labs(x = "Days after sowing",
                   y = input$vegetindex) +
              theme_bw(base_size = 18) +
              theme(panel.grid.minor = element_blank(),
                    legend.position = "bottom")

          })

        } else if(!input$usethresh){
          req(input$fittedmodel)

          dfplot <-
            dfactive$df |>
            dplyr::select(dplyr::all_of(c("unique_plot", input$flightdate, input$vegetindex))) |>
            setNames(c("unique_plot", "doy", "vindex")) |>
            dplyr::mutate(doy = as.POSIXlt(doy)$yday + 1 -  (as.POSIXlt(input$sowing)$yday + 1)) |>
            dplyr::filter(!!dplyr::sym("unique_plot") %in% input$fittedmodel)

          dfpars <-
            modl() |>
            filter(unique_plot == input$fittedmodel)

          #
          # Plot the fitted models
          observe({
            output$fittedplot <- renderPlot({

              df_int <-
                dplyr::tibble(flights = seq(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax, length.out = 1000),
                              class = ifelse(flights < dfpars$heading, "Vegetative", "Reproductive")) |>
                as.data.frame()
              # print(df_int)
              ypred <- predict(dfpars$parms[[1]][[1]]$modeladj, newdata = df_int)
              # print(ypred)
              df_int <- dplyr::bind_cols(df_int, data.frame(y = ypred))


              pmod <-
                ggplot() +
                geom_point(aes(x = doy, y = vindex),
                           data = dfplot) +
                stat_function(fun = dfpars$parms[[1]][[1]]$model,
                              xlim = c(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax),
                              args = dfpars$parms[[1]][[1]]$coefs) +
                geom_ribbon(data = df_int,
                            aes(x = flights,
                                ymin = min(y),
                                ymax =  y,
                                fill = class),
                            alpha = 0.5) +
                geom_vline(aes(xintercept = dfpars$heading), color = "red") +
                geom_vline(xintercept = dfpars$maturity, color = "salmon") +
                labs(x = "Days after sowing",
                     y = input$vegetindex,
                     fill = "Phase") +
                theme_bw(base_size = 18) +
                theme(panel.grid.minor = element_blank(),
                      legend.position = "bottom")

              fd <-
                ggplot() +
                stat_function(fun = dfpars$parms[[1]][[1]]$fd,
                              xlim = c(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax),
                              args = dfpars$parms[[1]][[1]]$coefs) +
                labs(x = "Days after sowing",
                     y = "First derivative") +
                geom_vline(aes(xintercept = dfpars$heading), color = "red") +
                geom_vline(xintercept = dfpars$maturity, color = "salmon") +
                theme_bw(base_size = 18) +
                theme(panel.grid.minor = element_blank())
              #
              sd <-
                ggplot() +
                stat_function(fun = dfpars$parms[[1]][[1]]$sd,
                              xlim = c(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax),
                              args = dfpars$parms[[1]][[1]]$coefs) +
                labs(x =  "Days after sowing",
                     y = "Second derivative") +
                geom_vline(aes(xintercept = dfpars$heading), color = "red") +
                geom_vline(xintercept = dfpars$maturity, color = "salmon") +
                theme_bw(base_size = 18) +
                theme(panel.grid.minor = element_blank())

              output$fderivate <- renderPlot({
                fd
              })

              output$sderivate <- renderPlot({
                sd
              })
              pmod
            })

          })
        }


        waiter_hide()

      })

      sendSweetAlert(
        session = session,
        title = "Prediction done",
        text = "The predictions have been made and are now available for further analysis in the 'Datasets' module.",
        type = "success"
      )




      # Explore the results Map
      output$map <- renderLeaflet({
        req(shapefile[[input$shapefiletoexplore]]$data)
        req(input$plotattribute)
        if(input$plotattribute %in% colnames(shapefile[[input$shapefiletoexplore]]$data)){
          if(is.null(basemap$map)){
            shapefile_view(shapefile[[input$shapefiletoexplore]]$data,
                           attribute = input$plotattribute,
                           color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                           alpha.regions = input$alpharesplot)
          } else{
            mshp <- shapefile_view(shapefile[[input$shapefiletoexplore]]$data,
                                   attribute = input$plotattribute,
                                   color_regions = return_colors(input$palplot, reverse = input$palplotrev),
                                   alpha.regions = input$alpharesplot)
            (basemap$map +  mshp)@map
          }
        }

      })
    })
  })
}

## To be copied in the UI
# mod_matanalyze_ui("matanalyze_1")

## To be copied in the server
# mod_matanalyze_server("matanalyze_1")
