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
          pickerInput(
            ns("method"),
            label = "Method for prediction",
            choices = c("Logistic Model L3",
                        "Logistic Model L4",
                        "Logistic Model L5",
                        "Logistic Ensamble",
                        "Segmented Regression")
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
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Overview",
          selected = "Overview",
          solidHeader = FALSE,
          type = "tabs",
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
              multiple = TRUE,
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
                       plotOutput(ns("fittedplot"), height = "520px") |> add_spinner()
              ),
              tabPanel("First derivative",
                       plotOutput(ns("fderivate"), height = "520px") |> add_spinner()
              ),
              tabPanel("Second derivative",
                       plotOutput(ns("sderivate"), height = "520px") |> add_spinner()
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
            plotlyOutput(ns("histograms"), height = "720px")  |> add_spinner()
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
        theme(panel.grid.minor = element_blank())
      plotly::ggplotly(p)
    })



    observeEvent(input$predictmat, {
      waiter_show(
        html = tagList(
          spin_google(),
          h2("Predictions in progress. Please, wait.")
        ),
        color = "#228B227F"
      )
      req(dfactive$df)
      req(input$vegetindex)
      if(input$method == "Logistic Model L3"){
        modl <-
          dfactive$df |>
          mod_L3(predictor = input$vegetindex,
                 flight_date = input$flightdate,
                 sowing_date = input$sowing)
      } else if(input$method == "Logistic Model L4"){
        modl <-
          dfactive$df |>
          mod_L4(predictor = input$vegetindex,
                 flight_date = input$flightdate,
                 sowing_date = input$sowing)
      } else if(input$method == "Logistic Model L5"){
        modl <-
          dfactive$df |>
          mod_L5(predictor = input$vegetindex,
                 flight_date = input$flightdate,
                 sowing_date = input$sowing)
      } else if(input$method == "Logistic Ensamble"){
        # modl <- predict_mat_ensamble
      } else if(input$method == "Segmented Regression"){
        # modl <- predict_mat_seg
      }
      observe({
        updateSelectInput(session, "plotattribute",
                          choices = colnames(modl),
                          selected = NA)
      })

      observe({
        updatePickerInput(session, "histotraits",
                          choices = colnames(modl),
                          selected = NA)
      })

      # Plot the histograms
      output$histograms <- renderPlotly({
        req(input$histotraits)

        dfhist <-
          modl |>
          dplyr::select(all_of(input$histotraits)) |>
          dplyr::ungroup() |>
          tidyr::pivot_longer(all_of(input$histotraits))


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

      # Plot the fitted models
      observe({
        levels <- sort(unique(dfactive$df[["unique_plot"]]))
        updatePickerInput(session, "fittedmodel",
                          choices = levels)

        req(input$fittedmodel)
        dfplot <-
          dfactive$df |>
          dplyr::select(dplyr::all_of(c("unique_plot", input$flightdate, input$vegetindex))) |>
          dplyr::filter(!!dplyr::sym("unique_plot") %in% input$fittedmodel)

        dfpars <-
          modl |>
          filter(unique_plot == input$fittedmodel)

        output$fittedplot <- renderPlot({

          df_int <-
            dplyr::tibble(x = seq(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax, length.out = 1000),
                          y = dfpars$parms[[1]][[1]]$model(x, dfpars$parms[[1]][[1]]$b0, dfpars$parms[[1]][[1]]$b1, dfpars$parms[[1]][[1]]$b2),
                          class = ifelse(x < dfpars$heading, "Vegetative", "Reproductive"))



          pmod <-
            ggplot() +
            geom_point(aes(x = lubridate::yday(.data[[input$flightdate]]), y = .data[[input$vegetindex]]),
                       data = dfplot) +
            stat_function(fun = dfpars$parms[[1]][[1]]$model,
                          xlim = c(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax),
                          args = list(b0 = dfpars$parms[[1]][[1]]$b0,
                                      b1 = dfpars$parms[[1]][[1]]$b1,
                                      b2 = dfpars$parms[[1]][[1]]$b2)) +
            geom_ribbon(data = df_int,
                        aes(x = x,
                            ymin = min(y),
                            ymax =  y,
                            fill = class),
                        alpha = 0.5) +
            geom_vline(aes(xintercept = dfpars$heading), color = "red") +
            geom_vline(xintercept = dfpars$maturity, color = "salmon") +
            labs(x = "Days after sowing",
                 fill = "Phase") +
            theme_bw(base_size = 18) +
            theme(panel.grid.minor = element_blank(),
                  legend.position = "bottom")

          fd <-
            ggplot() +
            stat_function(fun = dfpars$parms[[1]][[1]]$fd,
                          xlim = c(dfpars$parms[[1]][[1]]$xmin, dfpars$parms[[1]][[1]]$xmax),
                          args = list(b0 = dfpars$parms[[1]][[1]]$b0,
                                      b1 = dfpars$parms[[1]][[1]]$b1,
                                      b2 = dfpars$parms[[1]][[1]]$b2)) +
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
                          args = list(b0 = dfpars$parms[[1]][[1]]$b0,
                                      b1 = dfpars$parms[[1]][[1]]$b1,
                                      b2 = dfpars$parms[[1]][[1]]$b2)) +
            labs(x =  "Days after sowing",
                 y = "First derivative") +
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


      # Plot the results
      output$tabresult <- reactable::renderReactable({
        modl |>
          roundcols(digits = 3) |>
          render_reactable()

      })

      dfs[[input$saveto]] <- create_reactval(input$saveto, modl)

      waiter_hide()

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
