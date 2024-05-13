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
mod_dfedit_server <- function(id, dfs, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      if(input$dforshape == "data.frame"){
        updatePickerInput(session, "dftoedit",
                          choices = c("none", names(dfs)))
      } else{
        updatePickerInput(session, "dftoedit",
                          choices = c("none", setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
      }
    })

    dfactive <- reactiveValues()
    observeEvent(input$dftoedit, {
      req(input$dftoedit)
      if(input$dftoedit != "none"){
        if(input$dforshape == "data.frame"){
          dfactive$df <- dfs[[input$dftoedit]]$data |> convert_numeric_cols()
        } else{
          dfactive$df <- shapefile[[input$dftoedit]]$data |> convert_numeric_cols()
        }
      }
    })

    # Filter data
    res_edit <- reactiveValues()
    observe({
      res_edit$res <- edit_data_server(
        id = "editing",
        data_r = reactive(dfactive$df)
      )
    })

    observeEvent(input$doneediting, {
      newfile <- add_suffix(input$dftoedit, input$suffix)
      if(input$dforshape == "data.frame"){
        dfs[[newfile]] <- create_reactval(newfile, res_edit$res())
      } else{
        shapefile[[newfile]] <- create_reactval(newfile, res_edit$res() |> sf::st_as_sf())
      }
      sendSweetAlert(
        session = session,
        title = "Dataset edited",
        text = "The dataset has been successfully edited and is now available for further analysis.",
        type = "success"
      )
    })

  })
}

## To be copied in the UI
# mod_dfedit_ui("dfedit_1")

## To be copied in the server
# mod_dfedit_server("dfedit_1")
