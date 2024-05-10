#' spatjoin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spatjoin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "700px",
          status = "success",
          title = "Spatial joins/filters",
          selected = "Shapefiles",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Shapefiles",
            selectizeInput(inputId = ns("shapefile1"),
                           label = "Shapefile 1 (x)",
                           choices = NULL),
            selectizeInput(inputId = ns("shapefile2"),
                           label = "Shapefile 2 (y)",
                           choices = NULL
            ),
            radioGroupButtons(
              inputId = ns("jointype"),
              label = "Type of operation",
              choices = c("Geometry operations", "Geometric binary predicates"),
              status = "primary"
            ),
            conditionalPanel(
              condition = "input.jointype == 'Geometry operations'", ns = ns,
              awesomeRadio(
                inputId = ns("geometricoper"),
                label = "Join",
                choices = c("intersection", "difference", "union"),
                selected = "intersection",
                inline = TRUE,
                status = "success"
              )
            ),
            conditionalPanel(
              condition = "input.jointype == 'Geometric binary predicates'", ns = ns,
              awesomeRadio(
                inputId = ns("join"),
                label = "Join",
                choices = c("intersects", "contains", "cover", "crosses", "overlaps", "touches", "within"),
                selected = "intersects",
                inline = TRUE,
                status = "success"
              ),
              prettyCheckbox(
                inputId = ns("left"),
                label = "Left?",
                value = TRUE,
                icon = icon("check"),
                status = "success",
                animation = "rotate"
              )
            ),
            textInput(ns("newshape"),
                      label = "New shapefile",
                      value = "shape_join"),
            actionBttn(
              ns("joinshapes"),
              label = "Process",
              style = "pill",
              color = "success",
              icon = icon("layer-group")
            ),
            actionBttn(
              ns("storejoined"),
              label = "Store processed shapefile!",
              style = "pill",
              color = "success",
              icon = icon("share-from-square")
            )
          )
        ),
      ),
      col_8(
        bs4TabCard(
          id = "tabs",
          width = 12,
          height = "700px",
          status = "success",
          selected = "Original shapefiles",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Original shapefiles",
            fluidRow(
              col_6(
                leafletOutput(ns("leftshp"), height = "650px") |> add_spinner()
              ),
              col_6(
                leafletOutput(ns("rightshp"), height = "650px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Joined shapefile",
            leafletOutput(ns("joinedshape"), height = "650px") |> add_spinner()
          )
        )
      )
    )
  )
}

#' spatjoin Server Functions
#'
#' @noRd
mod_spatjoin_server <- function(id, shapefile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shpemod <- reactiveValues(res = NULL)

    # Observers for updating select inputs with shapefile names
    observe({
      choices <- setdiff(names(shapefile), c("shapefile", "shapefileplot"))
      updateSelectInput(session, "shapefile1", choices = choices, selected = "")
      updateSelectInput(session, "shapefile2", choices = choices, selected = "")
    })

    # Reactive storage for the shapefiles selected
    shp1 <- reactive({
      req(input$shapefile1)
      shapefile[[input$shapefile1]]$data
    })

    shp2 <- reactive({
      req(input$shapefile2)
      shapefile[[input$shapefile2]]$data
    })

    # Render Leaflet maps for both the left and right sides
    output$leftshp <- renderLeaflet({
      req(shp1())
      shapefile_view(shp1())@map
    })

    output$rightshp <- renderLeaflet({
      req(shp2())
      shapefile_view(shp2())@map
    })

    # Spatial join logic based on an event trigger (e.g., a button press)
    observeEvent(input$joinshapes, {
      req(shp1(), shp2())

      # Ensure shp2 has the same CRS as shp1
      shp2_transformed <- sf::st_transform(shp2(), sf::st_crs(shp1()))

      if(input$jointype == "Geometric binary predicates"){
        # Perform spatial join
        shpemod$res <- switch(input$join,
                              "intersects" = sf::st_join(shp1(), shp2_transformed, join = sf::st_intersects, left = input$left),
                              "contains" = sf::st_join(shp1(), shp2_transformed, join = sf::st_contains, left = input$left),
                              "cover" = sf::st_join(shp1(), shp2_transformed, join = sf::st_covers, left = input$left),
                              "crosses" = sf::st_join(shp1(),  shp2_transformed, join = sf::st_crosses, left = input$left),
                              "overlaps" = sf::st_join(shp1(), shp2_transformed, join = sf::st_overlaps, left = input$left),
                              "touches" = sf::st_join(shp1(), shp2_transformed, join = sf::st_touches, left = input$left),
                              "within" = sf::st_join(shp1(), shp2_transformed, join = sf::st_within, left = input$left))
      } else{
        # Perform spatial join
        shpemod$res <- switch(input$geometricoper,
                              "intersection" =  sf::st_intersection(shp1(), shp2_transformed),
                              "difference" = sf::st_join(shp1(), shp2_transformed),
                              "union" = sf::st_union(shp1(), shp2_transformed))
      }

      # Render the joined shapefile result
      output$joinedshape <- renderLeaflet({
        shapefile_view(shpemod$res)@map
      })
    })

    # # Event handler for creating a new shapefile in the list after spatial join
    observeEvent(input$storejoined, {
      shapefile[[input$newshape]] <- create_reactval(input$newshape, shpemod$res |> sf::st_as_sf())
      show_alert("Done",
                 text = "The joined shapefile can now be found in 'Shapefile' tab.",
                 type = "success")
    })

  })
}



## To be copied in the UI
# mod_spatjoin_ui("spatjoin_1")

## To be copied in the server
# mod_spatjoin_server("spatjoin_1")
