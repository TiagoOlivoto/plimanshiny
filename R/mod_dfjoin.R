#' dfjoin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dfjoin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Join datasets",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          pickerInput(
            ns("dftojoin"),
            label = "Dataset(s) to join",
            choices = NULL,
            multiple = TRUE
          ),
          awesomeRadio(
            inputId = ns("type"),
            label = "Mutating join",
            choices = c("left", "right", "full"),
            selected = "left",
            inline = TRUE,
            status = "success"
          ),
          textInput(
            ns("newset"),
            label = "New dataset",
            value = "df_joined"
          ),
          actionBttn(
            ns("startjoining"),
            label = "Start joining",
            icon = icon("pencil")
          ),
          actionBttn(
            ns("donejoining"),
            label = "Done joining",
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
            pickerInput(ns("varstojoin"),
                        label = "Variable(s) to join by",
                        choices = NULL,
                        multiple = TRUE),
            reactableOutput(ns("joined"), height = "680px")
          )
        )
      )
    )
  )
}

#' dfjoin Server Functions
#'
#' @noRd
mod_dfjoin_server <- function(id, dfs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updatePickerInput(session, "dftojoin",
                        choices = names(dfs))
    })

    dfstojoin <- reactiveValues()
    observe({
      req(input$dftojoin)
      dfstojoin$vals <- lapply(input$dftojoin, function(x){
        dfs[[x]]$data
      })
    })

    observe({
      commvar <- Reduce(base::intersect, lapply(dfstojoin$vals, colnames))
      updatePickerInput(session, "varstojoin",
                        choices = commvar)
    })

    result <- reactiveValues()
    observeEvent(input$startjoining, {
      req(dfstojoin$vals)
      req(input$varstojoin)
      if(input$type == "left"){
        result$res <-
          Reduce(
            function(x, y) {
              dplyr::left_join(x, y, by = input$varstojoin)
            },
            dfstojoin$vals
          )
      } else if(input$type == "right"){
        result$res <-
          Reduce(
            function(x, y) {
              dplyr::right_join(x, y, by = input$varstojoin)
            },
            dfstojoin$vals
          )
      } else{
        result$res <-
          Reduce(
            function(x, y) {
              dplyr::full_join(x, y, by = input$varstojoin)
            },
            dfstojoin$vals
          )
      }

      output$joined <- reactable::renderReactable({
        req(result$res)
        reactable::reactable(
          result$res |> roundcols(),
          filterable = TRUE,
          searchable = TRUE,
          striped = TRUE,
          pagination = FALSE
        )
      })
    })

    observeEvent(input$donejoining, {
      dfs[[input$newset]] <- create_reactval(input$newset, result$res)
      sendSweetAlert(
        session = session,
        title = "Datasets joined!",
        text = "The dataset has been successfully edited and can now be found in the 'Input' tab.",
        type = "success"
      )
    })


  })
}

## To be copied in the UI
# mod_dfjoin_ui("dfjoin_1")

## To be copied in the server
# mod_dfjoin_server("dfjoin_1")
