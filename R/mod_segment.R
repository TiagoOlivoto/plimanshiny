#' segment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segment_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Segmentation Settings",
          collapsible = FALSE,
          width = 12,
          height = "620px",
          hl(),
          h3("Input"),
          selectInput(ns("mosaic_to_segment"),
                      label = "Mosaic to be segmented",
                      choices = NULL),
          switchInput(
            inputId = ns("usemaskorind"),
            label = "Segmentation",
            labelWidth = "80px",
            onLabel = "Index",
            offLabel = "Pick",
            value = TRUE
          ),
          conditionalPanel(
            condition = "input.usemaskorind == true", ns = ns,
            divclass("anal4",
                     pickerInput(ns("segmentindex"),
                                 label = "Index for segmentation",
                                 choices = NULL,
                                 options = list(
                                   `actions-box` = TRUE,
                                   `live-search` = TRUE
                                 ))
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
            ),
            prettyCheckbox(
              inputId = ns("invertmask"),
              label = "Invert the segmentation?",
              value = FALSE,
              icon = icon("check"),
              status = "success",
              animation = "rotate"
            )
          ),
          awesomeRadio(
            inputId = ns("maskorseg"),
            label = "Return",
            choices = c("Mask", "Segmented mosaic"),
            selected = "Mask",
            inline = FALSE,
            status = "success"
          ),
          hl(),
          h3("Output"),
          textInput(ns("new_segment"),
                    label = "New object",
                    value = NULL),

          fluidRow(
            col_6(
              actionBttn(ns("startsegment"),
                         label = "Start!",
                         style = "pill",
                         color = "success")
            ),
            col_6(
              actionBttn(ns("segmentmosaic"),
                         label = "segment!",
                         style = "pill",
                         no_outline = FALSE,
                         icon = icon("scissors"),
                         color = "success")
            )
          )
        )
      ),
      col_9(
        bs4Card(
          title = "Crop Results",
          collapsible = FALSE,
          width = 12,
          height = "710px",
          # fluidRow(
          # col_6(
          conditionalPanel("input.usemaskorind == true",  ns = ns,
                           fluidRow(
                             col_6(
                               h3("Original mosaic"),
                               leafletOutput(ns("orimosaic"), height = "640px") |> add_spinner()
                             ),
                             col_6(
                               h3("Segmented mosaic"),
                               leafletOutput(ns("mosaicsegmentedind"), height = "640px") |> add_spinner()
                             )
                           )
          ),
          uiOutput(ns("uipick")),
          editModUI(ns("samplepoints"), height = "640px") |> add_spinner()

          # )
          # )
        )
      )
    )
  )
}

#' segment Server Functions
#'
#' @noRd
mod_segment_server <- function(id, mosaic_data, r, g, b, re, nir){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(mosaic_data)
      updateSelectInput(session, "mosaic_to_segment", choices = setdiff(names(mosaic_data), "mosaic"), selected = NULL)
      updateTextInput(session, "new_segment", value = paste0(input$mosaic_to_segment, "_segmented"))
      updatePickerInput(session, "segmentindex", choices = sort(pliman_indexes()))
    })


    # Observe event for mosaic crop action
    observeEvent(input$startsegment, {
      # Reactive expression to store the cropped mosaic
      segmented_mosaic <- reactiveVal(NULL)
      basemap <-
        mosaic_view(
          mosaic_data[[input$mosaic_to_segment]]$data,
          r = as.numeric(r$r),
          g = as.numeric(g$g),
          b = as.numeric(b$b)
        )
      if(input$usemaskorind){
        req(basemap)
        output$orimosaic <- renderLeaflet({
          basemap@map
        })
        seg <- mosaic_segment(mosaic_data[[input$mosaic_to_segment]]$data,
                              index = input$segmentindex,
                              r = as.numeric(r$r),
                              g = as.numeric(g$g),
                              b = as.numeric(b$b),
                              re = as.numeric(re$re),
                              nir = as.numeric(nir$nir),
                              threshold = ifelse(input$threshold == "Otsu", "Otsu", input$threshvalue),
                              invert = input$invertmask)
        req(seg)
        output$mosaicsegmentedind <- renderLeaflet({
          mosaic_view(
            seg,
            r = as.numeric(r$r),
            g = as.numeric(g$g),
            b = as.numeric(b$b)
          )@map

        })
        segmented_mosaic(seg)
      } else{

        req(basemap)
        backpoints <- reactiveValues(back = NULL)
        forepoints <- reactiveValues(fore = NULL)

        doneback <- reactiveValues(done = FALSE)
        observe({
          if(!doneback$done){
            output$uipick <- renderUI({
              actionBttn(ns("donaback"),
                         label = "Done background")

            })
            sendSweetAlert(
              session = session,
              title = "Sampling",
              text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to sample background regions",
              type = "info"
            )
            back <- callModule(editMod, "samplepoints", basemap@map, editor = "leafpm")
            observeEvent(input$donaback, {
              if (!is.null(back()$finished)) {
                backpoints$back <- back()$finished |> sf::st_transform(sf::st_crs(mosaic_data[[input$mosaic_to_segment]]$data))
              }
              doneback$done <- TRUE
            })
          } else{
            output$uipick <- renderUI({
              actionBttn(ns("donefore"),
                         label = "Done foreground")

            })
            sendSweetAlert(
              session = session,
              title = "Sampling",
              text = "Use the 'Draw Rectangle' or 'Draw Polygon' tools to sample foreground regions",
              type = "info"
            )
            fore <- callModule(editMod, "samplepoints", basemap@map, editor = "leafpm")
            observeEvent(input$donefore, {
              if (!is.null(fore()$finished)) {
                fore <- fore()$finished |> sf::st_transform(sf::st_crs(mosaic_data[[input$mosaic_to_segment]]$data))
                fore <-  fore |>  sf::st_difference(backpoints$back) |> sf_to_polygon()
                forepoints$fore <- fore
              }

              req(backpoints$back)
              req(forepoints$fore)
              back_sample <-
                exactextractr::exact_extract(mosaic_data[[input$mosaic_to_segment]]$data, backpoints$back, progress = FALSE) |>
                dplyr::bind_rows() |>
                dplyr::select(-coverage_fraction) |>
                dplyr::mutate(class = 0)

              fore_sample <-
                exactextractr::exact_extract(mosaic_data[[input$mosaic_to_segment]]$data, forepoints$fore, progress = FALSE) |>
                dplyr::bind_rows() |>
                dplyr::select(-coverage_fraction) |>
                dplyr::mutate(class = 1)
              df_train <- dplyr::bind_rows(fore_sample, back_sample)
              if(ncol(df_train) == 2){
                names(df_train)[[1]] <- names(mosaic_data[[input$mosaic_to_segment]]$data)
              }
              mod <- suppressWarnings(
                glm(class ~.,
                    data = df_train,
                    family = binomial("logit"))
              )
              mask <- terra::predict(mosaic_data[[input$mosaic_to_segment]]$data, mod, type = "response")
              mask[mask < 0.5] <- 0
              mask[mask > 0.5] <- 1
              if(input$maskorseg == 'Mask'){
                segmented_mosaic(mask)
              } else{
                segmented_mosaic(terra::mask(mosaic_data[[input$mosaic_to_segment]]$data, mask, maskvalue = TRUE, inverse = TRUE))
              }

              sendSweetAlert(
                session = session,
                title = "Sampling done",
                text = "The sampling procedure has been successfully finished. Click 'Segment' in the left panel to segment the mosaic",
                type = "info"
              )
            })
          }

        })

      }

      # Observe event for mosaic crop action
      observeEvent(input$segmentmosaic, {
        # Update mosaic_data$mosaic when input$cropmosaic is clicked
        mosaic_data[[input$new_segment]] <- create_reactval(name = input$new_segment, data = segmented_mosaic())
        sendSweetAlert(
          session = session,
          title = "Mosaic successfully segmented!!",
          text = "The mosaic has been successfully segmented and is now available for further analysis.",
          type = "success"
        )
      })
    })
  })
}

## To be copied in the UI
# mod_segment_ui("segment_1")

## To be copied in the server
# mod_segment_server("segment_1")
