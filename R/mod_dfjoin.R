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
          prettyRadioButtons(
            inputId = ns("dforshape"),
            label = "Join",
            choices = c("data.frames", "data.frames with a shapefile"),
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.dforshape == 'data.frames'", ns = ns,
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
            )
          ),
          conditionalPanel(
            condition = "input.dforshape == 'data.frames with a shapefile'", ns = ns,
            pickerInput(
              ns("dftojoinshp"),
              label = "Dataset",
              choices = NULL,
              multiple = FALSE
            ),
            pickerInput(
              ns("shapetojoin"),
              label = "Shapefile",
              choices = NULL,
              multiple = FALSE
            )
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
          title = "Merged data",
          selected = "Merged data",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Merged data",
            pickerInput(ns("varstojoin"),
                        label = "Variable(s) to join by",
                        choices = NULL,
                        multiple = TRUE),
            reactableOutput(ns("joined"), height = "640px")
          )
        )
      )
    )
  )
}

#' dfjoin Server Functions
#'
#' @noRd
mod_dfjoin_server <- function(id, dfs, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updatePickerInput(session, "dftojoin",
                        choices = names(dfs))
    })
    observe({
      updatePickerInput(session, "dftojoinshp",
                        choices = names(dfs),
                        selected = NA)
    })
    observe({
      updatePickerInput(session, "shapetojoin",
                        choices = setdiff(names(shapefile), c("shapefile", "shapefileplot")),
                        selected = NA)
    })


    result <- reactiveValues()
    observe({
      if(input$dforshape == "data.frames"){
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

        # observeEvent(input$startjoining, {
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
              pagination = TRUE,
              defaultPageSize = 13
            )
          })
        # })

        observeEvent(input$donejoining, {
          dfs[[input$newset]] <- create_reactval(input$newset, result$res)
          sendSweetAlert(
            session = session,
            title = "Datasets joined!",
            text = "The dataset has been successfully edited and can now be found in the 'Input' tab.",
            type = "success"
          )
        })
      } else{
        req(input$dftojoinshp)
        req(input$shapetojoin)
        observe({
          commvar <- intersect(names(shapefile[[input$shapetojoin]]$data), names(dfs[[input$dftojoinshp]]$data))
          updatePickerInput(session,
                            "varstojoin",
                            choices = commvar)
        })
        req(input$varstojoin)
        result$res <- dplyr::left_join(shapefile[[input$shapetojoin]]$data |> convert_numeric_cols(),
                                       dfs[[input$dftojoinshp]]$data |> convert_numeric_cols(),
                                       by =  dplyr::join_by(input$varstojoin))

        observeEvent(input$donejoining, {
          shapefile[[input$newset]] <- create_reactval(input$newset, result$res)
          sendSweetAlert(
            session = session,
            title = "Shapefile merged joined!",
            text = "The shapefile has been successfully merged and can now be found in the 'Input' tab.",
            type = "success"
          )
        })

      }
      output$joined <- reactable::renderReactable({
        req(result$res)
        reactable::reactable(
          result$res |> roundcols(),
          filterable = TRUE,
          searchable = TRUE,
          striped = TRUE,
          pagination = TRUE,
          defaultPageSize = 13
        )
      })

    })
  })
}

## To be copied in the UI
# mod_dfjoin_ui("dfjoin_1")

## To be copied in the server
# mod_dfjoin_server("dfjoin_1")
