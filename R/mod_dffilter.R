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
            ns("startfilter"),
            label = "Start filtering",
            icon = icon("filter")
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
mod_dffilter_server <- function(id, dfs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updatePickerInput(session, "dftofilter",
                        choices = names(dfs))
    })
    dfactive <- reactiveValues()
    observe({
      req(input$dftofilter)
      req(dfs[[input$dftofilter]]$data)
      dfactive$df <- dfs[[input$dftofilter]]$data |> as.data.frame()
    })

    # Filter data
    res_filter <- reactiveValues()
    observeEvent(input$startfilter, {
      res_filter$res <- filter_data_server(
        id = "filtering",
        data = reactive(dfactive$df),
        name = reactive(input$dftofilter),
        vars = reactive(names(dfactive$df)),
        widget_num = "slider",
        widget_date = "slider",
        label_na = "Missing"
      )
    })
    observeEvent(input$donefiltering, {
      dfs[[paste0(input$dftofilter, input$suffix)]] <- create_reactval(paste0(input$dftofilter, input$suffix), res_filter$res$filtered())
      sendSweetAlert(
        session = session,
        title = "Dataset filtered",
        text = "The dataset has been successfully filtered and can now be found in the 'Input' tab.",
        type = "success"
      )
    })
  })
}

## To be copied in the UI
# mod_dffilter_ui("dffilter_1")

## To be copied in the server
# mod_dffilter_server("dffilter_1")
