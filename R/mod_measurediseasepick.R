#' measurediseasepick UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_measurediseasepick_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      fluidRow(
        col_4(
          bs4Card(
            title = "Palette Settings",
            collapsible = FALSE,
            width = 12,
            height = "710px",
            pickerInput(ns("imagepalette"),
                        label = "Image",
                        choices = NULL),
            pickerInput(ns("shape"),
                        label = "Shape",
                        choices = c('box', 'disc', 'diamond', 'Gaussian', 'line')),
            actionBttn(ns("startpaletting"),
                       label = "Start picking color samples!",
                       style = "pill",
                       color = "success"),
            fluidRow(
              col_6(
                h3("Leaf/background"),
                numericInput(
                  inputId = ns("openinglb"),
                  label = "Opening",
                  value = 10
                ),
                numericInput(
                  inputId = ns("closinglb"),
                  label = "Closing",
                  value = 0
                ),
                numericInput(
                  inputId = ns("filterlb"),
                  label = "Filter",
                  value = 0
                )
              ),
              col_6(
                h3("Healthy/disease"),
                numericInput(
                  inputId = ns("openingdh"),
                  label = "Opening",
                  value = 0
                ),
                numericInput(
                  inputId = ns("closingdh"),
                  label = "Closing",
                  value = 0
                ),
                numericInput(
                  inputId = ns("filterdh"),
                  label = "Filter",
                  value = 0
                )
              )
            ),
            hl(),
            fluidRow(
              col_6(
                prettyCheckbox(
                  inputId = ns("showmask"),
                  label = "Show a mask",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                ),
                conditionalPanel(
                  condition = "input.showmask == true", ns = ns,
                  fluidRow(
                    col_6(
                      colorPickr(
                        inputId = ns("colorleaf"),
                        label = "Healthy",
                        swatches = scales::viridis_pal()(10),
                        theme = "monolith",
                        useAsButton = TRUE,
                        selected = "forestgreen",
                      )
                    ),
                    col_6(
                      colorPickr(
                        inputId = ns("diseasecolor"),
                        label = "Disease",
                        swatches = scales::viridis_pal()(10),
                        theme = "monolith",
                        useAsButton = TRUE,
                        selected = "brown",
                      )
                    )
                  )
                )
              ),
              col_6(
                prettyCheckbox(
                  inputId = ns("showcontour"),
                  label = "Contour",
                  value = TRUE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                ),
                conditionalPanel(
                  condition = "input.showcontour == true", ns = ns,
                  fluidRow(
                    col_5(
                      colorPickr(
                        inputId = ns("colorcont"),
                        label = "Color",
                        swatches = scales::viridis_pal()(10),
                        theme = "monolith",
                        useAsButton = TRUE,
                        selected = "red",
                      )
                    ),
                    col_7(
                      sliderInput(
                        inputId = ns("sizecont"),
                        label = "Size",
                        min = 1,
                        max = 6,
                        value = 3,
                        step = 0.1
                      )
                    )
                  )
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
            selected = "Sampling image regions",
            solidHeader = FALSE,
            type = "tabs",
            tabPanel(
              "Sampling image regions",
              uiOutput(ns("uipick")),
              editModUI(ns("samplepoints"), height = "640px") |> add_spinner()
            ),
            tabPanel(
              title = "Results",
              plotOutput(ns("resultplot"), height = "570px")  |> add_spinner(),
              plotlyOutput(ns("resultbar"), height = "150px") |> add_spinner()
            )
          )
        )
      )
    )
  )
}

#' measurediseasepick Server Functions
#'
#' @noRd
mod_measurediseasepick_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    basemap <- reactiveValues(map = NULL)
    # Define a reactiveValues object to track whether the observer has been executed
    observeOnce <- reactiveValues(once = "sim")

    observe({
      # Check if the observer hasn't been executed yet
      if (observeOnce$once == "sim") {
        avalimg <-  setdiff(names(imgdata), "img")
        req(avalimg)
        updatePickerInput(session, "imagepalette", choices = avalimg, selected = avalimg[[1]])

        # Mark that the observer has been executed
        observeOnce$once <- "nao"
      }

    })
    observe({
      req(input$imagepalette)
      basemap$map <- image_view(imgdata[[input$imagepalette]]$data,
                                max_pixels = 500000)
    })


    backpoints <- reactiveValues(back = NULL)
    leafpoints <- reactiveValues(leaf = NULL)
    diseasepoint <- reactiveValues(disease = NULL)

    doneback <- reactiveValues(done = FALSE)
    doneleaf <- reactiveValues(done = FALSE)
    donedisease <- reactiveValues(done = FALSE)
    observeEvent(input$startpaletting, {
      observe({
        req(basemap$map)
        if(!doneback$done){
          output$uipick <- renderUI({
            actionBttn(ns("donaback"),
                       label = "Background-area sampling finished")

          })
          sendSweetAlert(
            session = session,
            title = "Sampling",
            text = "Use the 'Draw point' to pick up background colors",
            type = "info"
          )
          back <- callModule(editMod, "samplepoints", basemap$map@map, editor = "leafpm")
          observeEvent(input$donaback, {
            if (!is.null(back()$finished)) {
              backpoints$back <-
                back()$finished |>
                sf::st_transform(3857) |>
                dplyr::select(geometry) |>
                sf::st_coordinates() |>
                as.data.frame()
            }
            doneback$done <- TRUE
          })
        } else if(!doneleaf$done){
          output$uipick <- renderUI({
            actionBttn(ns("doneleaf"),
                       label = "Healthy-area sampling finished")

          })
          sendSweetAlert(
            session = session,
            title = "Sampling",
            text = "Use the 'Draw points' to pick up leaf colors",
            type = "info"
          )
          leaf <- callModule(editMod, "samplepoints", basemap$map@map, editor = "leafpm")
          observeEvent(input$doneleaf, {
            if (!is.null(leaf()$finished)) {
              leafpoints$leaf <-
                leaf()$finished |>
                sf::st_transform(3857) |>
                dplyr::select(geometry) |>
                sf::st_coordinates() |>
                as.data.frame()
            }
            doneleaf$done <- TRUE
          })
        } else if(!donedisease$done){
          output$uipick <- renderUI({
            actionBttn(ns("donedisease"),
                       label = "Disease-area sampling finished")

          })
          sendSweetAlert(
            session = session,
            title = "Sampling",
            text = "Use the 'Draw points' to pick up disease colors",
            type = "info"
          )
          disease <- callModule(editMod, "samplepoints", basemap$map@map, editor = "leafpm")
          observeEvent(input$donedisease, {
            if (!is.null(disease()$finished)) {
              diseasepoint$disease <-
                disease()$finished |>
                sf::st_transform(3857) |>
                dplyr::select(geometry) |>
                sf::st_coordinates() |>
                as.data.frame()
            }
            donedisease$done <- TRUE
          })

        } else{
          # do the analysis
          back <-
            create_palette(imgdata[[input$imagepalette]]$data,
                           points = backpoints$back,
                           width = 100,
                           height = 100,
                           shape = "box",
                           r = 3)
          folha <-
            create_palette(imgdata[[input$imagepalette]]$data,
                           points = leafpoints$leaf,
                           width = 100,
                           height = 100,
                           shape = "box",
                           r = 3)
          doenca <-
            create_palette(imgdata[[input$imagepalette]]$data,
                           points = diseasepoint$disease,
                           width = 100,
                           height = 100,
                           shape = "box",
                           r = 3)

          tmpf <- tempdir()
          req(back)
          req(folha)
          req(doenca)
          sev <-
            measure_disease(
              img = imgdata[[input$imagepalette]]$data,
              img_healthy = folha,
              img_background = back,
              img_symptoms = doenca,
              closing = c(input$closinglb, input$closingdh),
              opening = c(input$openinglb, input$openingdh),
              filter = c(input$filterlb, input$filterdh),
              col_leaf = input$colorleaf,
              col_lesions = input$diseasecolor,
              show_original = !input$showmask,
              contour_col = input$colorcont,
              contour_size = input$sizecont,
              save_image = TRUE,
              plot = FALSE,
              dir_processed = tmpf,
              prefix = "proc_shiny_disease"
            )
          output$resultplot <- renderPlot({
            fil <- image_import(list.files(path = tmpf, pattern = "proc_shiny_disease"),
                                path = tmpf)
            plot(fil)

          })
          output$resultbar <- renderPlotly({
            df <- data.frame(label = c("healthy", "symptomatic"),
                             vals = c(sev$severity[[1]], sev$severity[[2]]))
            p <-
              ggplot(df, aes(y = "", x = vals, fill = label)) +
              geom_bar(stat = "identity", position = "fill") +
              geom_text(aes(label = sprintf("%.2f%%", vals)), position = position_fill(vjust = 0.5)) +
              scale_fill_manual(values = c("forestgreen", "brown")) +
              labs(y = NULL,
                   x = "Proportion") +
              theme_minimal()
            plotly::ggplotly(p)
          })
          sendSweetAlert(
            session = session,
            title = "Image successfully analyzed!!",
            text = "The image has been analyzed and the results can now be seen in the tab 'Results'",
            type = "success"
          )

        }
      })
    })

  })
}

## To be copied in the UI
# mod_measurediseasepick_ui("measurediseasepick_1")

## To be copied in the server
# mod_measurediseasepick_server("measurediseasepick_1")
