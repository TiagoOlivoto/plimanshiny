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
            ns("doneupdating"),
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
mod_dfupdate_server <- function(id, dfs, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      if(input$dforshape == "data.frame"){
        updatePickerInput(session, "dftoupdate",
                          choices = c("none", names(dfs)))
      } else{
        updatePickerInput(session, "dftoupdate",
                          choices = c("none", setdiff(names(shapefile), c("shapefile", "shapefileplot"))))
      }
    })

    dfactive <- reactiveValues()
    observeEvent(input$dftoupdate, {
      req(input$dftoupdate)
      if(input$dftoupdate != "none"){
        if(input$dforshape == "data.frame"){
          dfactive$df <- dfs[[input$dftoupdate]]$data |> convert_numeric_cols()
        } else{
          dfactive$df <- shapefile[[input$dftoupdate]]$data |> convert_numeric_cols()
        }
      }
    })


    # Update data
    res_update <- reactiveValues()
    res_update$res <- update_variables_server(
      id = "updating",
      data = reactive(dfactive$df),
      height = "720px"
    )

    observeEvent(input$doneupdating, {
      newfile <- paste0(file_name(input$dftoupdate), input$suffix, ".", file_extension(input$dftoupdate))
      if(input$dforshape == "data.frame"){
        dfs[[newfile]] <- create_reactval(newfile, res_update$res())
      } else{
        shapefile[[newfile]] <-  create_reactval(newfile, res_update$res())
      }
      sendSweetAlert(
        session = session,
        title = "Dataset updated",
        text = "The dataset has been successfully updated and is now available for further analysis.",
        type = "success"
      )
    })
  })
}

## To be copied in the UI
# mod_dfupdate_ui("dfupdate_1")

## To be copied in the server
# mod_dfupdate_server("dfupdate_1")
