#' imageindex UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imageindex_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Index Settings",
          collapsible = FALSE,
          width = 12,
          height = "720px",
          hl(),
          h3("Input"),
          selectInput(ns("img_to_index"),
                      label = "Source image",
                      choices = NULL),
          pickerInput(
            inputId = ns("imageindex"),
            label = "Image index",
            choices = "",
            multiple = TRUE
          ),
          textInput(ns("myindex"),
                    label = "My personalized index",
                    value = ""),
          hl(),
          h3("Output"),
          textInput(
            ns("new_index"),
            label = "New object",
            value = NULL
          ),
          actionBttn(ns("createindex"),
                     label = "Create!",
                     style = "pill",
                     no_outline = FALSE,
                     icon = icon("scissors"),
                     color = "success")
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
            plotOutput(ns("index"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Index (density)",
            plotOutput(ns("indexhist"), height = "720px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' imageindex Server Functions
#'
#' @noRd
mod_imageindex_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(imgdata$img)
      updateSelectInput(session, "img_to_index", choices = c("Active image", setdiff(names(imgdata), "img")), selected = "Active image")
    })

    observe({
      updatePickerInput(session, "imageindex",
                        choices = pliman_indexes_rgb(),
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ))
    })

    imgindex <- reactive({
      mindex <- strsplit(input$myindex, split = ",")[[1]]
      c(mindex, input$imageindex)

    })

    output$index <- renderPlot({
      if(input$img_to_index == "Active image"){
        img <- imgdata$img
      } else{
        img <- imgdata[[input$img_to_index]]$data

      }
      req(imgindex())
      req(img)
      ind <- image_index(img, index = imgindex(), plot = FALSE)
      output$indexhist <- renderPlot({
        ots <- otsu(ind[[1]]@.Data[!is.infinite(ind[[1]]@.Data) & !is.na(ind[[1]]@.Data)])
        plot(ind, type = "density")
        abline(v = ots)
        title(sub = paste0("Otsu's threshold: ", round(ots, 4)))
      })

      observeEvent(input$createindex ,{
        imgdata[[input$new_index]] <- create_reactval(name = input$new_index, data = ind)

      })
      plot(ind)
    })



  })
}

## To be copied in the UI
# mod_imageindex_ui("imageindex_1")

## To be copied in the server
# mod_imageindex_server("imageindex_1")
