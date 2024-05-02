#' dfupdate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dfupdate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Update a dataset",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          pickerInput(
            ns("dftoupdate"),
            label = "Dataset to update",
            choices = NULL
          ),
          textInput(
            ns("suffix"),
            label = "Suffix",
            value = "_updated"
          ),
          actionBttn(
            ns("startedit"),
            label = "Start updating",
            icon = icon("pencil")
          ),
          actionBttn(
            ns("doneediting"),
            label = "Done updating",
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
          title = "Update data",
          selected = "Update data",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Update data",
            update_variables_ui(ns("updating"))
          )
        )
      )
    )
  )
}

#' dfupdate Server Functions
#'
#' @noRd
mod_dfupdate_server <- function(id, dfs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updatePickerInput(session, "dftoupdate",
                        choices = names(dfs))
    })
    dfactive <- reactiveValues()
    observe({
      req(input$dftoupdate)
      req(dfs[[input$dftoupdate]]$data)
      dfactive$df <- dfs[[input$dftoupdate]]$data |> as.data.frame()
    })

    # Update data
    res_update <- reactiveValues()
    observeEvent(input$startedit, {
      res_update$res <- update_variables_server(
        id = "updating",
        data = reactive(dfactive$df),
        height = "720px"
      )
    })

    observeEvent(input$doneediting, {
      dfs[[paste0(input$dftoupdate, input$suffix)]] <- create_reactval(paste0(input$dftoupdate, input$suffix), res_update$res())
      sendSweetAlert(
        session = session,
        title = "Dataset updated",
        text = "The dataset has been successfully updated and can now be found in the 'Input' tab.",
        type = "success"
      )
    })
  })
}

## To be copied in the UI
# mod_dfupdate_ui("dfupdate_1")

## To be copied in the server
# mod_dfupdate_server("dfupdate_1")
