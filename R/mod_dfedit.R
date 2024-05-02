#' dfedit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dfedit_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Edit a dataset",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          pickerInput(
            ns("dftoedit"),
            label = "Dataset to edit",
            choices = NULL
          ),
          textInput(
            ns("suffix"),
            label = "Suffix",
            value = "_edited"
          ),
          actionBttn(
            ns("startedit"),
            label = "Start editing",
            icon = icon("pencil")
          ),
          actionBttn(
            ns("doneediting"),
            label = "Done editing",
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
          title = "Edit data",
          selected = "Edit data",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Edit data",
            edit_data_ui(ns("editing"))
          )
        )
      )
    )
  )
}

#' dfedit Server Functions
#'
#' @noRd
mod_dfedit_server <- function(id, dfs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      updatePickerInput(session, "dftoedit",
                        choices = names(dfs))
    })
    dfactive <- reactiveValues()
    observe({
      req(input$dftoedit)
      req(dfs[[input$dftoedit]]$data)
      dfactive$df <- dfs[[input$dftoedit]]$data |> as.data.frame()
    })

    # Filter data
    res_edit <- reactiveValues()
    observeEvent(input$startedit, {
      res_edit$res <- edit_data_server(
        id = "editing",
        data_r = reactive(dfactive$df)
      )
    })
    observeEvent(input$doneediting, {
      dfs[[paste0(input$dftoedit, input$suffix)]] <- create_reactval(paste0(input$dftoedit, input$suffix), res_edit$res())
      sendSweetAlert(
        session = session,
        title = "Dataset edited",
        text = "The dataset has been successfully edited and can now be found in the 'Input' tab.",
        type = "success"
      )
    })

  })
}

## To be copied in the UI
# mod_dfedit_ui("dfedit_1")

## To be copied in the server
# mod_dfedit_server("dfedit_1")
