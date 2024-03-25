#' imagesegment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imagesegment_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Segment Settings",
          collapsible = FALSE,
          width = 12,
          h3("Input"),
          selectInput(ns("img_to_segment"),
                      label = "Source image",
                      choices = NULL),
          awesomeRadio(
            inputId = ns("segmentmethod"),
            label = "Segmentation method",
            choices = c("Index", "k-means", "Manual"),
            selected = "Index",
            status = "success"
          ),
          conditionalPanel(
            condition = "input.segmentmethod == 'Index'", ns = ns,
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("imageindex"),
                  label = "Image index",
                  choices = "",
                  multiple = TRUE
                )
              ),
              col_6(
                textInput(ns("myindex"),
                          label = "My index",
                          value = ""),
              )
            ),
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("thresh"),
                  label = "Threshold",
                  choices = c("Otsu", "Adaptive", "Numeric")
                )
              ),
              col_6(
                conditionalPanel(
                  condition = "input.thresh == 'Numeric'", ns = ns,
                  sliderInput(ns("threshnum"),
                              label = "Threshold",
                              min = 0,
                              max = 0,
                              value = 0,
                              step = 0.005)
                ),
                conditionalPanel(
                  condition = "input.thresh == 'Adaptive'", ns = ns,
                  sliderInput(ns("windowsize"),
                              label = "Window size",
                              min = 0,
                              max = 0,
                              value = 0,
                              step = 1)
                )
              )
            ),
            fluidRow(
              col_3(
                prettyCheckbox(
                  inputId = ns("invertindex"),
                  label = "Invert",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_4(
                prettyCheckbox(
                  inputId = ns("fillhull"),
                  label = "Fill Holes",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              ),
              col_5(
                prettyCheckbox(
                  inputId = ns("naback"),
                  label = "Background NA",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                )
              )
            ),
            sliderInput(
              inputId = ns("opening"),
              label = "Opening",
              value = 0,
              min = 0,
              max = 50
            ),
            sliderInput(
              inputId = ns("closing"),
              label = "Closing",
              value = 0,
              min = 0,
              max = 50
            ),
            sliderInput(
              inputId = ns("filter"),
              label = "Median filter",
              value = 0,
              min = 0,
              max = 50
            )
          ),
          conditionalPanel(
            condition = "input.segmentmethod == 'k-means'", ns = ns,
            numericInput(
              inputId = ns("nclasses"),
              label = "Number of classes",
              value = 2
            )

          )
        )

      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Index (raster)",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Index (raster)",
            fluidRow(
              col_6(
                actionBttn(ns("createindex"),
                           label = "Create!",
                           style = "pill",
                           no_outline = FALSE,
                           icon = icon("plus"),
                           color = "success")
              ),
              col_6(
                textInput(
                  ns("new_index"),
                  label = "New image index",
                  value = NULL
                )
              )
            ),
            plotOutput(ns("index"), height = "680px") |> add_spinner()
          ),
          tabPanel(
            title = "Index (density)",
            plotOutput(ns("indexhist"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Mask",
            fluidRow(
              col_6(
                actionBttn(ns("createbinary"),
                           label = "Create!",
                           style = "pill",
                           no_outline = FALSE,
                           icon = icon("plus"),
                           color = "success")
              ),
              col_6(
                textInput(
                  ns("new_binary"),
                  label = "New binary image",
                  value = NULL
                )
              )
            ),
            plotOutput(ns("binary"), height = "680px")
          ),
          tabPanel(
            title = "Segment",
            fluidRow(
              col_6(
                actionBttn(ns("createsegment"),
                           label = "Create!",
                           style = "pill",
                           no_outline = FALSE,
                           icon = icon("plus"),
                           color = "success")
              ),
              col_6(
                textInput(
                  ns("new_segment"),
                  label = "New segmented image",
                  value = NULL
                )
              )
            ),
            plotOutput(ns("segmentation"), height = "680px")
          )
        )
      )
    )
  )
}

#' imagesegment Server Functions
#'
#' @noRd
mod_imagesegment_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(imgdata$img)
      updateSelectInput(session, "img_to_segment", choices = c("Active image", setdiff(names(imgdata), "img")), selected = "Active image")
    })

    observe({
      updatePickerInput(session, "imageindex",
                        choices = pliman_indexes_rgb(),
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ))
    })

    parms <- reactive({
      req(imgdata$img)
      mindex <- strsplit(input$myindex, split = ",")[[1]]
      if(input$thresh == "Otsu"){
        thresval <- "Otsu"
      } else if(input$thresh == "Adaptive"){
        thresval <- "adaptive"
      } else {
        req(input$threshnum)
        thresval <- input$threshnum
      }
      if(input$img_to_segment == "Active image"){
        img <- imgdata$img
      } else{
        img <- imgdata[[input$img_to_segment]]$data

      }

      list(index = c(mindex, input$imageindex),
           thresval = thresval,
           img = img,
           segmethod = input$segmentmethod)
    })


    ind <- reactiveValues(ind = NULL)
    output$index <- renderPlot({
      if(parms()$segmethod == "Index"){
        req(parms()$index)
        req(parms()$img)
        indtmp <- image_index(parms()$img, index = parms()$index, plot = FALSE)

        ots <- otsu(indtmp[[1]]@.Data[!is.infinite(indtmp[[1]]@.Data) & !is.na(indtmp[[1]]@.Data)])
        updateSliderInput(session, "threshnum",
                          min = min(indtmp[[1]]),
                          max = max(indtmp[[1]]),
                          value = ots,
                          step = 0.0005)
        nc <- ncol(indtmp[[1]])
        nr <- nrow(indtmp[[1]])
        ws <- min(dim(indtmp[[1]])) / 10
        updateSliderInput(session, "windowsize",
                          min = 3,
                          max = max(nc, nr) / 2,
                          value = ws,
                          step = 1)


        output$indexhist <- renderPlot({
          plot(indtmp, type = "density")
          abline(v = ots)
          title(sub = paste0("Otsu's threshold: ", round(ots, 4)))
        })
      } else if(parms()$segmethod == "k-means"){
        req(parms()$img)
        indtmp <- image_index(parms()$img, index = "R+G+B", plot = FALSE)
      }
      ind$ind <- indtmp
      plot(indtmp)
    })
    observeEvent(input$createindex ,{
      imgdata[[input$new_index]] <- create_reactval(name = input$new_index, data = ind$ind)
      sendSweetAlert(
        session = session,
        title = "Image index created",
        text = paste0("The image index (", input$new_index, ") has been created and is now available for further processing."),
        type = "success"
      )
    })

    bin <- reactiveValues(bin = NULL)
    output$binary <- renderPlot({
      if(parms()$segmethod == "Index"){
        req(parms()$img)
        req(parms()$index)
        bintmp <-
          image_binary(parms()$img,
                       index = parms()$index,
                       invert = input$invertindex,
                       opening = input$opening,
                       closing = input$closing,
                       filter = input$filter,
                       fill_hull = input$fillhull,
                       windowsize = input$windowsize,
                       threshold = parms()$thresval)[[1]]
      } else if(parms()$segmethod == "k-means"){
        req(parms()$img)
        bintmp <-
          image_segment_kmeans(parms()$img,
                               nclasses = input$nclasses,
                               invert = input$invertindex,
                               opening = input$opening,
                               closing = input$closing,
                               filter = input$filter,
                               fill_hull = input$fillhull)[["clusters"]]
      }
      bin$bin <- bintmp

    })
    observeEvent(input$createbinary ,{
      imgdata[[input$new_binary]] <- create_reactval(name = input$new_binary, data = bin$bin)
      sendSweetAlert(
        session = session,
        title = "Mask created",
        text = paste0("The binary image (", input$new_binary, ") has been created and is now available for further processing."),
        type = "success"
      )
    })

    seg <- reactiveValues(seg = NULL)
    output$segmentation <- renderPlot({
      req(parms()$img)
      req(parms()$index)
      segtmp <-
        image_segment(parms()$img,
                      index = parms()$index,
                      invert = input$invertindex,
                      opening = input$opening,
                      closing = input$closing,
                      filter = input$filter,
                      fill_hull = input$fillhull,
                      threshold = parms()$thresval,
                      windowsize = input$windowsize,
                      na_background = input$naback)
      seg$seg <- segtmp


    })
    observeEvent(input$createsegment ,{
      imgdata[[input$new_segment]] <- create_reactval(name = input$new_segment, data = seg$seg)
      sendSweetAlert(
        session = session,
        title = "Segmented image created",
        text = paste0("The segmented image (", input$new_segment, ") has been created and is now available for further processing."),
        type = "success"
      )

    })




  })
}

## To be copied in the UI
# mod_imagesegment_ui("imagesegment_1")

## To be copied in the server
# mod_imagesegment_server("imagesegment_1")
