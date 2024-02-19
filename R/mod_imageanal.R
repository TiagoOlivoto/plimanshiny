#' imageanal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imageanal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          width = 12,
          height = "720px",
          icon = icon("gears"),
          status  = "success",
          type = "tabs",
          tabPanel(
            title = "Single image",
            actionBttn(ns("analyzeimg"),
                       label = "Analyze!",
                       status = "success"),
            pickerInput(
              inputId = ns("plotindexes"),
              label = "Index to segment",
              choices = "",
              multiple = TRUE
            ),
            fluidRow(
              col_6(
                prettyCheckbox(
                  inputId = ns("invertindex"),
                  label = "Invert",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                ),
                prettyCheckbox(
                  inputId = ns("contour"),
                  label = "Contour",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_6(
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
                                   value = NULL)
                    ),
                    col_6(
                      numericInput(ns("extension"),
                                   label = "Extension",
                                   value = NULL)

                    )
                  )
                )
              )
            ),
            materialSwitch(
              inputId = ns("reference"),
              label = "Reference object?",
              value = FALSE,
              status = "success"
            ),
            conditionalPanel(
              condition = "input.reference == true", ns = ns,
              switchInput(
                inputId = ns("reftype"),
                label = "Reference",
                labelWidth = "90px",
                onLabel = "By size",
                offLabel = "By  color",
                value = TRUE
              ),
              conditionalPanel(
                condition = "input.reftype == true", ns = ns,
                pickerInput(
                  inputId = ns("largesmall"),
                  label = "Reference type",
                  choices = c("Larger object", "Smaller object"),
                  selected = "Larger object"
                ),
                numericInput(inputId = ns("refarea"),
                             label = "Reference area",
                             value = NA)
              )
            )
          ),
          tabPanel(
            title = "Batch processing",
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
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Index",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Index",
            plotOutput(ns("index"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Segmentation",
            plotOutput(ns("segment"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Summary",
            fluidRow(
              valueBoxOutput(ns("vbnindiv")),
              valueBoxOutput(ns("vbnaveragearea")),
              valueBoxOutput(ns("largerindiv")),
              valueBoxOutput(ns("smallerindiv"))
            ),
            fluidRow(
              col_4(
                h4("Area"),
                plotlyOutput(ns("boxarea"), height = "420px") |> add_spinner()
              ),
              col_4(
                h4("Length"),
                plotlyOutput(ns("boxlength"), height = "420px") |> add_spinner()
              ),
              col_4(
                h4("Width"),
                plotlyOutput(ns("boxwidth"), height = "420px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Results (plot)",
            leafletOutput(ns("resultsleafl"), height = "720px")  |> add_spinner(),
            downloadBttn(ns("downloadplotmap"),
                         label = "Take a shot",
                         style = "pill")
          ),
          tabPanel(
            title = "Results (data)",
            DT::dataTableOutput(ns("resultsindivtab"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      )
    )

  )
}

#' imageanal Server Functions
#'
#' @noRd
mod_imageanal_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updatePickerInput(session, "plotindexes",
                        choices = pliman_indexes_rgb(),
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ))
      print(input$tolerance)
    })




    output$index <- renderPlot({
      req(imgdata$img)
      req(input$plotindexes)
      image_index(imgdata$img, index = input$plotindexes)
    })

    output$segment <- renderPlot({
      req(imgdata$img)
      req(input$plotindexes)
      image_segment(imgdata$img, index = input$plotindexes)
    })


    observeEvent(input$analyzeimg, {
      if(is.na(input$tolerance)){
        tol <- NULL
      } else{
        tol <- input$tolerance
      }
      if(is.na(input$extension)){
        ext <- NULL
      } else{
        ext <- input$extension
      }

      if(input$reference & input$reftype & input$largesmall == "Larger object"){
        reflarger <- TRUE
      } else{
        reflarger <- FALSE
      }
      if(input$reference & input$reftype & input$largesmall == "Smaller object"){
        refsmaller <- TRUE
      } else{
        refsmaller <- FALSE
      }

      waiter_show(
        html = tagList(
          spin_google(),
          h2("Analyzing the image Please, wait.")
        ),
        color = "#228B227F"
      )

      res <-
        analyze_objects(imgdata$img,
                        index = input$plotindexes,
                        watershed = input$watershed,
                        tolerance = tol,
                        extension = ext,
                        reference = input$reference,
                        reference_larger = reflarger,
                        reference_smaller = refsmaller,
                        reference_area = input$refarea,
                        plot = FALSE)
      req(res)


      output$resultsleafl <- renderLeaflet({
        image_view(imgdata$img, object = res)@map
      })

      # summary
      output$vbnindiv <- renderValueBox({
        valueBox(
          value = res$statistics$value[[1]],
          subtitle = "Number objects",
          color = "success",
          icon = icon("table-cells")
        )
      })
      output$vbnaveragearea <- renderValueBox({
        valueBox(
          value = round(res$statistics$value[[3]], 3),
          subtitle = "Average area",
          color = "success",
          icon = icon("seedling")
        )
      })
      output$largerindiv <- renderValueBox({
        valueBox(
          value = round(res$statistics$value[[4]], 3),
          subtitle = "Larger individual (area)",
          color = "success",
          icon = icon("up-right-and-down-left-from-center")
        )
      })
      output$smallerindiv <- renderValueBox({
        valueBox(
          value = round(res$statistics$value[[2]], 3),
          subtitle = "Smaller individual (area)",
          color = "success",
          icon = icon("up-right-and-down-left-from-center")
        )
      })

      output$boxarea <- renderPlotly({

        p <-
          ggplot(res$results, aes(y = area)) +
          geom_boxplot(fill = "#28a745") +
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

        plotly::ggplotly(p, dynamicTicks = TRUE)
      })

      output$boxlength <- renderPlotly({
        p <-
          ggplot(res$results, aes(y = length)) +
          geom_boxplot(fill = "#28a745") +
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

        plotly::ggplotly(p, dynamicTicks = TRUE)
      })

      output$boxwidth <- renderPlotly({
        p <-
          ggplot(res$results, aes(y = width)) +
          geom_boxplot(fill = "#28a745") +
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

        plotly::ggplotly(p, dynamicTicks = TRUE)
      })



      output$resultsindivtab <- DT::renderDataTable(
        res$results |>
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
      waiter_hide()
      sendSweetAlert(
        session = session,
        title = "Image successfully analyzed!!",
        text = "The image has been analyzed and the results can now be seen in the tabs",
        type = "success"
      )




    })



  })
}

## To be copied in the UI
# mod_imageanal_ui("imageanal_1")

## To be copied in the server
# mod_imageanal_server("imageanal_1")
