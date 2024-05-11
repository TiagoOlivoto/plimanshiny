#' dffilter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dffilter_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Filter a dataset",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          prettyRadioButtons(
            inputId = ns("dforshape"),
            label = "Use",
            choices = c("data.frame", "shapefile"),
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            inline = TRUE
          ),
          pickerInput(
            ns("dftofilter"),
            label = "Dataset to filter",
            choices = NULL
          ),
          textInput(
            ns("suffix"),
            label = "Suffix",
            value = "_filtered"
          ),
          actionBttn(
            ns("donefiltering"),
            label = "Done filtering",
            icon = icon("check")
          )
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "760px",
          title = "Filter data",
          selected = "Filter data",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Filter data",
            filter_data_ui(ns("filtering"), max_height = "700px")
          )
        )
      )
    )
  )
}

#' dffilter Server Functions
#'
#' @noRd
mod_dffilter_server <- function(id, dfs, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      if(input$dforshape == "data.frame"){
        updatePickerInput(session, "dftofilter",
                          choices = c("none", names(dfs)))
      } else{
        updatePickerInput(session, "dftofilter",
                          choices = c("none", setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
      }
    })

    dfactive <- reactiveValues()
    observeEvent(input$dftofilter, {
      req(input$dftofilter)
      if(input$dftofilter != "none"){
        if(input$dforshape == "data.frame"){
          dfactive$df <- dfs[[input$dftofilter]]$data |> convert_numeric_cols()
        } else{
          dfactive$df <- shapefile[[input$dftofilter]]$data |> convert_numeric_cols()
        }
      }
    })

    # Filter data
    res_filter <- reactiveValues()
    res_filter$res <- filter_data_server(
      id = "filtering",
      data = reactive(dfactive$df),
      name = reactive(input$dftofilter),
      vars = reactive(names(dfactive$df)),
      widget_num = "slider",
      widget_date = "slider",
      label_na = "Missing",
      drop_ids = FALSE
    )
    observeEvent(input$donefiltering, {
      newfile <- add_suffix(input$dftofilter, input$suffix)
      if(input$dforshape == "data.frame"){
        dfs[[newfile]] <- create_reactval(newfile, res_filter$res$filtered())
      } else{
        shapefile[[newfile]] <- create_reactval(newfile, res_filter$res$filtered())
      }
      sendSweetAlert(
        session = session,
        title = "Dataset filtered",
        text = "The dataset has been successfully filtered and is now available for further analysis.",
        type = "success"
      )
    })
  })
}

## To be copied in the UI
# mod_dffilter_ui("dffilter_1")

## To be copied in the server
# mod_dffilter_server("dffilter_1")
