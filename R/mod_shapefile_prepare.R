#' shapefile_prepare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shapefile_prepare_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    col_3(
      bs4Card(title = "Shapefile input",
              width = 12,
              radioGroupButtons(
                inputId = ns("shapetype"),
                label = "Choose an option to create the shapefile",
                choices = c("Build", "Import"),
                checkIcon = list(
                  yes = tags$i(class = "fa fa-check-square",
                               style = "color: steelblue"),
                  no = tags$i(class = "fa fa-square-o",
                              style = "color: steelblue"))
              ),
              conditionalPanel(
                condition = "input.shapetype == 'Build'", ns = ns,
                h3("Plot configuration"),
                fluidRow(
                  col_6(
                    numericInput(ns("ncols"),
                                 label = "Number of columns",
                                 value = 1)
                  ),
                  col_6(
                    numericInput(ns("nrows"),
                                 label = "Number of rows",
                                 value = 1)
                  )
                ),
                sliderInput(ns("buffercol"),
                            label = "Column buffer",
                            value = 0,
                            min = -0.5,
                            max = 0.5,
                            step = 0.01
                ),
                sliderInput(ns("bufferrow"),
                            label = "Row buffer",
                            value = 0,
                            min = -0.5,
                            max = 0.5,
                            step = 0.01
                ),
                prettyCheckbox(
                  inputId = ns("shapedone"),
                  label = "Shapefile finished!",
                  value = FALSE,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE,
                  animation = "rotate"
                )
              ),
              conditionalPanel(
                condition = "input.shapetype == 'Import'", ns = ns,
                fileInput(ns("import_shapefile"),
                          "Import a shapefile (.shp, .rds)",
                          accept = c('.shp','.rds'))
              ),
              conditionalPanel(
                condition = "input.shapedone == true", ns = ns,
                materialSwitch(
                  inputId = ns("editplots"),
                  label = "Edit the drawn plots?",
                  value = FALSE,
                  status = "danger"
                ), br(),
                conditionalPanel(
                  condition = "input.editplots == true", ns = ns,
                  prettyCheckbox(
                    inputId = ns("editdone"),
                    label = "Edition finished!",
                    value = FALSE,
                    status = "info",
                    icon = icon("thumbs-up"),
                    plain = TRUE,
                    outline = TRUE,
                    animation = "rotate"
                  )
                ),
                mod_download_shapefile_ui(ns("downloadshapefile"))
              )
      )
    ),
    col_9(
      conditionalPanel(
        condition = "input.shapetype == 'Build' & input.shapedone == false", ns = ns,
        fluidRow(
          col_6(
            h3("Control points"),
            editModUI(ns("shapefile_build"), height = "760px")
          ),
          col_6(
            h3("Built shapefile"),
            leafletOutput(ns("createdshapes"), height = "760px")
          )
        )
      ),
      conditionalPanel(
        condition = "input.shapetype == 'Build' & input.shapedone == true", ns = ns,
        fluidRow(
          col_6(
            h3("Built shapefile"),
            leafletOutput(ns("plotshapedone"), height = "760px")
          ),
          col_6(
            conditionalPanel(
              condition = "input.editplots == true", ns = ns,
              h3("Plot edition"),
              editModUI(ns("plotedit"), height = "760px")
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.shapetype == 'Import'", ns = ns,
        leafletOutput(ns("shapefile_mapview"), height = "760px")
      )
    )
  )
}

#' shapefile_prepare Server Functions
#'
#' @noRd
mod_shapefile_prepare_server <- function(id, mosaic_data, basemap, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      if(!is.null(mosaic_data$mosaic)){
        observeEvent(input$shapetype, {
          if (input$shapetype == "Build") {
            createdshape <- reactiveValues(shp = NULL)
            cpoints <- callModule(editMod, "shapefile_build", basemap$map@map, editor = "leafpm")

            observeEvent(c(cpoints()$finished, !input$shapedone, input$ncols, input$nrows, input$buffercol, input$bufferrow), {
              if(!is.null(cpoints()$edited)){
                cpo <- cpoints()$edited
              } else{
                cpo <- cpoints()$finished
              }
              # Check if edits()$finished is not NULL and shapedone is FALSE
              if (!is.null(cpoints()$finished) && !input$shapedone) {
                shp <- shapefile_build(mosaic_data$mosaic, basemap, controlpoints = cpo,
                                       nrow = input$nrows,
                                       ncol = input$ncols,
                                       buffer_col = input$buffercol,
                                       buffer_row = input$bufferrow,
                                       verbose = FALSE)

                # Update the reactiveVal with the cropped mosaic
                createdshape$shp <- shp
              }
            })

            output$createdshapes <- renderLeaflet({
              nelem <- length(createdshape$shp)
              mapp <-
                basemap$map +
                mapview::mapview(createdshape$shp[[nelem]],
                                 layer.name = "shapes")
              mapp@map
            })
            observeEvent(input$shapedone,{
              nelem <- length(createdshape$shp)
              shapefile$shapefile <- createdshape$shp[[nelem]]

              output$plotshapedone <- renderLeaflet({
                mapp <-
                  basemap$map +
                  mapview::mapview(shapefile$shapefile,
                                   layer.name = "shapes")
                mapp@map
              })
            })
          }
        }
        )
        observeEvent(input$import_shapefile, {
          shapefile$shapefile <- terra::vect(input$import_shapefile$datapath)
          req(shapefile$shapefile)  # Ensure mosaic_data$mosaic is not NULL

          output$shapefile_mapview <- renderLeaflet({
            mapp <- basemap$map + mapview::mapview(shapefile$shapefile)
            # req(basemap$map)  # Ensure mosaic_data$mosaic is not NULL
            # mapview::mapview(shapefile$shapefile)@map
            mapp@map
          })
        })

        observeEvent(c(input$editplots, !input$editdone),{
          if(input$editplots == TRUE){
            shapes <-
              shapefile$shapefile |>
              poorman::mutate(`_leaflet_id` = 1:nrow(shapefile$shapefile),
                              feature_type = "polygon") |>
              poorman::relocate(geometry, .after = 2) |>
              sf::st_transform(crs = 4326)

            mapp <-
              basemap$map@map |>
              leaflet::addPolygons(
                data = shapes,
                weight = 3,
                opacity = 1,
                fill = FALSE,
                color = 'black',
                fillOpacity = 1,
                smoothFactor = 0.01,
                group = "editable"
              )

            editedpoints <- callModule(editMod, "plotedit",
                                  leafmap = mapp,
                                  targetLayerId = "editable")

            observeEvent(input$editdone,{
              if(input$editdone == TRUE){
                if(!is.null(editedpoints()$all)){

                  shapefile$shapefile <- editedpoints()$all |> poorman::select(geometry)
                  output$plotshapedone <- renderLeaflet({
                    mapp <-
                      basemap$map +
                      mapview::mapview(shapefile$shapefile,
                                       layer.name = "shapes")
                    mapp@map
                  })
                }
              }
            })
          }



        })
        mod_download_shapefile_server("downloadshapefile", terra::vect(shapefile$shapefile))
      }
    })

  })
}

## To be copied in the UI
# mod_shapefile_prepare_ui("shapefile_prepare_1")

## To be copied in the server
# mod_shapefile_prepare_server("shapefile_prepare_1")
