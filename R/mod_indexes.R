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
                             value = "")
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
          height = "760px",
          status = "success",
          title = "Vegetation Indexes",
          selected = "Plot Index (raster)",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Plot Index (raster)",
            plotOutput(ns("plotindex"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Plot Index (density)",
            plotOutput(ns("plotindexhist"), height = "720px") |> add_spinner()
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
  poorman::filter(type == "index")
#' indexes Server Functions
#'
#' @noRd
mod_indexes_server <- function(id, mosaic_data, r, g, b, re, nir, basemap, index){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$guideindex, introjs(session,
                                           options = list("nextLabel"="Next",
                                                          "prevLabel"="Previous",
                                                          "skipLabel"="Skip",
                                                          steps = helpind),
                                           events = list("oncomplete"=I('alert("Hope it helped!")'))))

    observeEvent(input$mosaicinfoindex, {
      req(mosaic_data$mosaic)
      mosaic_info(mosaic_data$mosaic)
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

      finalindex
    })
    observeEvent(input$computeindex, {
      if(is.null(mosaic_data$mosaic)){
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
        R <- try(mosaic_data$mosaic[[as.numeric(r$r)]], TRUE)
        G <- try(mosaic_data$mosaic[[as.numeric(g$g)]], TRUE)
        B <- try(mosaic_data$mosaic[[as.numeric(b$b)]], TRUE)
        NIR <- try(mosaic_data$mosaic[[as.numeric(nir$nir)]], TRUE)
        RE <- try(mosaic_data$mosaic[[as.numeric(re$re)]], TRUE)
        me <- pliman_indexes_me()
        if(any(finalindex() %in% me) & inherits(NIR, "try-error") | any(finalindex() %in% me) & inherits(RE, "try-error")){
          show_alert("Ops, an error occured.",
                     text = "Multispectral indexes cannot be computed since needed bands are not available.",
                     type = "error")
        } else{

          # compute the indexes
          req(mosaic_data$mosaic)  # Ensure mosaic_data$mosaic is not NULL
          waiter_show(
            html = tagList(
              spin_google(),
              h2("Computing the indexes. Please, wait.")
            ),
            color = "#228B227F"
          )
          indextemp <- mosaic_index(mosaic_data$mosaic,
                                    r = as.numeric(r$r),
                                    g = as.numeric(g$g),
                                    b = as.numeric(b$b),
                                    re = as.numeric(re$re),
                                    nir = as.numeric(nir$nir),
                                    index = finalindex(),
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
          if(aggr > 0){
            magg <- terra::aggregate(indextemp, aggr)
          } else{
            magg <- indextemp
          }
          req(magg)
          output$plotindex <- renderPlot({
            if (input$indextosync != "") {
              terra::plot(magg[[input$indextosync]])
            }
          })
          output$plotindexhist <- renderPlot({
            if (input$indextosync != "") {
              ots <- otsu(na.omit(terra::values(magg[[input$indextosync]])))
              terra::density(magg[[input$indextosync]],
                             main = paste0(names(magg[[input$indextosync]]), " - Otsu: ", round(ots, 4)))
              abline(v = ots,
                     col = "red",
                     lty = 2,
                     )
            }
          })
          output$indexsync <- renderUI({
            req(basemap$map)
            if (input$indextosync != "") {
              leafsync::sync(basemap$map@map, mosaic_view(magg[[input$indextosync]],
                                                          index = input$indextosync))
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
