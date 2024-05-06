#' datasets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_datasets_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Mask Settings",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          actionBttn(
            ns("importdf"),
            label = "Import dataset",
            icon = icon("file-import")
          ),
          pickerInput(
            ns("activedf"),
            label = "Active dataset",
            choices = NULL
          ),
          hl(),
          h4("Send to global environment"),
          "Coming soon..."
        )
      ),
      col_9(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "760px",
          title = "Active dataset",
          selected = "Active dataset",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Active dataset",
            edit_data_ui(ns("editing"))
            # reactableOutput(ns("resultsindivtab"), height = "700px")
          )
        )
      )
    )
  )
}

#' datasets Server Functions
#'
#' @noRd
mod_datasets_server <- function(id, dfs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$importdf, {
      import_modal(
        id = ns("import"),
        from = c("env", "file", "googlesheets", "url"),
        title = "Import data"
      )
    })
    # Import data
    tmp <- import_server("import", return_class = "tbl_df")
    observe({
      req(tmp$data())
      dfs[[tmp$name()]] <- create_reactval(tmp$name(), tmp$data())
    })

    observe({
      updatePickerInput(session, "activedf",
                        choices = names(dfs))
    })

    dfactive <- reactiveValues()
    observe({
      req(input$activedf)
      req(dfs[[input$activedf]]$data)
      dfactive$df <- dfs[[input$activedf]]$data
        res <- edit_data_server(
          id = "editing",
          file_name_export = input$activedf,
          data_r = reactive(dfs[[input$activedf]]$data),
          update = FALSE,
          delete = FALSE,
          add = FALSE,
          reactable_options = list(height = "680px",
                                   filterable = TRUE,
                                   searchable = TRUE,
                                   striped = TRUE,
                                   pagination = TRUE,
                                   defaultPageSize = 13)
        )

    })
  })
}

## To be copied in the UI
# mod_datasets_ui("datasets_1")

## To be copied in the server
# mod_datasets_server("datasets_1")
