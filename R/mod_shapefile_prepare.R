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
      bs4Card(
        title = "Settings",
        status = "success",
        width = 12,
        fluidRow(
          col_6(
            divclass("shape1",
                     prettyRadioButtons(
                       inputId = ns("shapetype"),
                       label = "I want to...",
                       choices = c("Build", "Import"),
                       icon = icon("check"),
                       bigger = TRUE,
                       status = "info",
                       animation = "jelly"
                     )
            )
          ),
          col_6(
            fluidRow(
              col_4(
                divclass("shape2",
                         br(),
                         dropdown(
                           fluidRow(
                             col_6(
                               colorPickr(
                                 inputId = ns("colorfill"),
                                 label = "Fill color",
                                 swatches = scales::viridis_pal()(10),
                                 theme = "monolith",
                                 useAsButton = TRUE,
                                 selected = "#0361FC",
                               )
                             ),
                             col_6(
                               colorPickr(
                                 inputId = ns("colorstroke"),
                                 label = "Stroke color",
                                 swatches = scales::viridis_pal()(10),
                                 theme = "monolith",
                                 useAsButton = TRUE,
                                 selected = "darkred",
                               )
                             )
                           ),
                           sliderInput(ns("alphacolorfill"),
                                       label = "Fill opacity",
                                       min = 0,
                                       max = 1,
                                       value = 0.3),
                           style = "unite",
                           icon = icon("gear"),
                           status = "success",
                           width = "360px",
                           animate = animateOptions(enter = "fadeInLeft", exit = "fadeOutRight", duration = 1),
                           tooltip = tooltipOptions(title = "Settings")
                         )
                )
              ),
              col_8(
                conditionalPanel(
                  condition = "input.shapetype == 'Build'", ns = ns,
                  br(),
                  actionButton(
                    inputId = ns("guideshape"),
                    label = tagList(
                      icon = icon("question-circle", verify_fa = FALSE), "Guide"
                    ),
                    style = "color: white ; background-color: #dd4b39",
                    class = "btn-danger"
                  )
                ),
                conditionalPanel(
                  condition = "input.shapetype == 'Import'", ns = ns,
                  br(),
                  actionButton(
                    inputId = ns("guideshapeimput"),
                    label = tagList(
                      icon = icon("question-circle", verify_fa = FALSE), "Guide"
                    ),
                    style = "color: white ; background-color: #dd4b39",
                    class = "btn-danger"
                  )
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.shapetype == 'Build'", ns = ns,
          divclass("shape3",
                   h3("Grid shape"),
                   textInput(ns("shapenamebuild"),
                             label = "Shapefile Name",
                             value = "Shapefile Build"),
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
                   )
          ),
          divclass("shape4",
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
                   )
          ),
          divclass("shape5",
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
          tags$hr(),
        ),
        conditionalPanel(
          condition = "input.shapetype == 'Import'", ns = ns,
          tags$hr(),
          divclass("shapeimp1",
                   fileInput(ns("import_shapefile"),
                             "Import a shapefile (.shp, .rds)",
                             accept=c(".rds",  ".shp",  ".json", ".kml",  ".gml",  ".dbf",  ".sbn",  ".sbx",  ".shx",  ".prj", ".cpg" ), multiple=TRUE),
                   selectInput(ns("shapefiletoanalyze"),
                               label = "Active Shapefile",
                               choices = NULL)
          ),
          tags$hr(),
          divclass("shapeimp2",
                   selectInput(ns("colorshapeimport"),
                               label = "Fill color",
                               choices = NULL)
          )
        ),
        conditionalPanel(
          condition = "input.shapedone == true | input.shapetype == 'Import'", ns = ns,
          materialSwitch(
            inputId = ns("editplots"),
            label = "Edit the drawn plots?",
            value = FALSE,
            status = "danger"
          ),
          conditionalPanel(
            condition = "input.editplots == true & input.shapetype != 'Import'", ns = ns,
            divclass("shapeimp3",
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
            )
          ),
          conditionalPanel(
            condition = "input.editplots == true & input.shapetype == 'Import'", ns = ns,
            divclass("shapeimp4",
                     prettyCheckbox(
                       inputId = ns("editdoneimpo"),
                       label = "Edition finished!",
                       value = FALSE,
                       status = "info",
                       icon = icon("thumbs-up"),
                       plain = TRUE,
                       outline = TRUE,
                       animation = "rotate"
                     ), br()
            )
          ),
          tags$hr(),
          mod_download_shapefile_ui(ns("downloadshapefile"))
        )
      )
    ),
    col_9(
      bs4Card(
        width = 12,
        height = "780px",
        title = "Building the shapefile",
        color = "success",
        status = "success",
        maximizable = TRUE,
        conditionalPanel(
          condition = "input.shapetype == 'Build' & input.shapedone == false", ns = ns,
          fluidRow(
            col_8(
              h4("Control points"),
              editModUI(ns("shapefile_build"), height = "740px") |> add_spinner()
            ),
            col_4(
              h4("Built shapefile"),
              leafletOutput(ns("createdshapes"), height = "740px") |> add_spinner()
            )
          )
        ),
        conditionalPanel(
          condition = "input.shapetype == 'Build' & input.shapedone == true", ns = ns,
          fluidRow(
            col_6(
              h3("Built shapefile"),
              leafletOutput(ns("plotshapedone"), height = "740px") |> add_spinner()
            ),
            col_6(
              conditionalPanel(
                condition = "input.editplots == true & input.editdone == false", ns = ns,
                h3("Plot edition"),
                editModUI(ns("plotedit"), height = "740px") |> add_spinner()
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.shapetype == 'Import'", ns = ns,
          fluidRow(
            col_6(
              h3("Built shapefile"),
              leafletOutput(ns("shapefile_mapview"), height = "740px") |> add_spinner()
            ),
            col_6(
              conditionalPanel(
                condition = "input.editplots == true & input.editdoneimpo == false", ns = ns,
                h3("Plot edition"),
                editModUI(ns("ploteditimpo"), height = "740px") |> add_spinner()
              )
            )
          )
        )
      )
    )

  )
}

helpshp <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  poorman::filter(type == "shape")
shapeimp <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  poorman::filter(type == "shapeimp")
#' shapefile_prepare Server Functions
#'
#' @noRd
mod_shapefile_prepare_server <- function(id, mosaic_data, basemap, shapefile){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$guideshape, introjs(session,
                                           options = list("nextLabel"="Next",
                                                          "prevLabel"="Previous",
                                                          "skipLabel"="Skip",
                                                          steps = helpshp),
                                           events = list("oncomplete"=I('alert("Hope it helped!")'))))
    observeEvent(input$guideshapeimput, introjs(session,
                                                options = list("nextLabel"="Next",
                                                               "prevLabel"="Previous",
                                                               "skipLabel"="Skip",
                                                               steps = shapeimp),
                                                events = list("oncomplete"=I('alert("Hope it helped!")'))))

    # GUIA
    observe({
      # req(mosaic_data$mosaic)
      if(!is.null(mosaic_data$mosaic)){
        req(basemap$map)
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
                                 color = input$colorstroke,
                                 col.regions = input$colorfill,
                                 alpha.regions = input$alphacolorfill,
                                 layer.name = "shapes")
              mapp@map
            })

            observeEvent(input$shapedone,{
              nelem <- length(createdshape$shp)
              req(createdshape$shp[[nelem]])
              req(input$shapenamebuild)

              observe({
                shapefile[[input$shapenamebuild]] <-
                  create_reactval(input$shapenamebuild,
                                  createdshape$shp[[nelem]] |>
                                    poorman::mutate(plot_id = paste0("P", leading_zeros(1:nrow(createdshape$shp[[nelem]]), 4)),
                                                    .before = 1)
                  )


                observe({
                  shapefilenames <-  setdiff(names(shapefile), "shapefile")
                  # Update selectInput choices
                  updateSelectInput(session, "shapefiletoanalyze",
                                    choices = shapefilenames,
                                    selected = shapefilenames[[length(shapefilenames)]])
                })
                output$plotshapedone <- renderLeaflet({
                  mapp <-
                    basemap$map +
                    mapview::mapview(shapefile[[input$shapenamebuild]]$data,
                                     color = input$colorstroke,
                                     col.regions = input$colorfill,
                                     # z.col = "plot_id",
                                     legend = FALSE,
                                     alpha.regions = input$alphacolorfill,
                                     layer.name = "shapes")
                  mapp@map
                })
              })
            })


            observeEvent(c(input$editplots, !input$editdone),{
              if(input$editplots == TRUE){
                shapes <-
                  shapefile[[input$shapenamebuild]]$data |>
                  poorman::mutate(`_leaflet_id` = 1:nrow(shapefile[[input$shapenamebuild]]$data),
                                  feature_type = "polygon") |>
                  poorman::relocate(geometry, .after = 2) |>
                  sf::st_transform(crs = 4326)


                mapp <-
                  basemap$map@map |>
                  leaflet::addPolygons(
                    data = shapes,
                    weight = 3,
                    color = input$colorstroke,
                    fillColor = input$colorfill,
                    opacity = 1,
                    fillOpacity = input$alphacolorfill,
                    group = "editable"
                  )

                editedpoints <- callModule(editMod, "plotedit",
                                           leafmap = mapp,
                                           targetLayerId = "editable")

                observeEvent(input$editdone,{
                  if(input$editdone == TRUE){
                    if(!is.null(editedpoints()$all)){

                      shapefile[[input$shapenamebuild]]$data <-
                        editedpoints()$all |>
                        poorman::select(geometry) |>
                        sf::st_transform(crs = sf::st_crs(mosaic_data$mosaic)) |>
                        poorman::mutate(plot_id = paste0("P", leading_zeros(1:nrow(editedpoints()$all), 4)),
                                        .before = 1)
                      output$plotshapedone <- renderLeaflet({
                        mapp <-
                          basemap$map +
                          mapview::mapview(shapefile[[input$shapenamebuild]]$data,
                                           color = input$colorstroke,
                                           # col.regions = input$colorfill,
                                           z.col = "plot_id",
                                           alpha.regions = input$alphacolorfill,
                                           legend = FALSE,
                                           layer.name = "shapes")
                        mapp@map
                      })
                    }
                    updateMaterialSwitch(session, "editplots", value = FALSE)
                  }
                })
              }
            })


          }
        }
        )
      }
    })



    observeEvent(input$import_shapefile, {
      newshpname <- input$import_shapefile$name
      # Check if the mosaic already exists in shapefile
      if (any(newshpname %in% names(shapefile))) {
        # If it exists, update the existing reactiveValues
        moname <- newshpname[newshpname %in% names(shapefile)]
        ask_confirmation(
          inputId = "confirmashpname",
          type = "warning",
          title = "Shapefile already imported",
          text = paste0("The object '", paste0(moname, collapse = ", "), "' is already available in the list of imported shapefiles. Do you really want to overwrite it?"),
          btn_labels = c("Nope", "Yep"),
          btn_colors = c("#FE642E", "#04B404")
        )
        observe({
          if (!is.null(input$confirmashpname)) {
            if (input$confirmashpname) {
              if("shp" %in% file_extension(input$import_shapefile$datapath)){
                shapefile[[paste0(file_name(newshpname[[1]]), ".shp")]] <-
                  create_reactval(paste0(file_name(newshpname[[1]]), ".shp"), import_shp_mod(input$import_shapefile$datapath,
                                                                                             input$import_shapefile,
                                                                                             session))
              } else{
                for (i in 1:length(newshpname)) {
                  shapefile[[newshpname[[i]]]] <-
                    create_reactval(newshpname[[i]], import_shp_mod(input$import_shapefile$datapath[[i]],
                                                                    input$import_shapefile[[i]],
                                                                    session))
                }
              }
            } else {
              return()
            }
          }
        })
      } else {
        # If it doesn't exist, create a new reactiveValues and add it to mosaic_data
        if("shp" %in% file_extension(input$import_shapefile$datapath)){
          shapefile[[paste0(file_name(newshpname[[1]]), ".shp")]] <-
            create_reactval(paste0(file_name(newshpname[[1]]), ".shp"), import_shp_mod(input$import_shapefile$datapath,
                                                                                       input$import_shapefile,
                                                                                       session))
        } else{
          for (i in 1:length(newshpname)) {
            shapefile[[newshpname[[i]]]] <-
              create_reactval(newshpname[[i]], import_shp_mod(input$import_shapefile$datapath[[i]],
                                                              input$import_shapefile[[i]],
                                                              session))
          }
        }
      }

      observe({
        shapefilenames <-  setdiff(names(shapefile), "shapefile")
        # Update selectInput choices
        updateSelectInput(session, "shapefiletoanalyze",
                          choices = shapefilenames,
                          selected = shapefilenames[[length(shapefilenames)]])
      })
    })

    observe({
      # Check if a mosaic is selected
      req(input$shapefiletoanalyze)

      # Get the selected mosaic data
      selected_shp <- shapefile[[input$shapefiletoanalyze]]
      # # Check if the selected_mosaic is not NULL and has the 'data' field
      if ('data' %in% names(selected_shp)) {
        shapefile$shapefile <- selected_shp$data
      }
    })



    observe({
      req(shapefile$shapefile)  # Ensure mosaic_data$mosaic is not NULL

      updateSelectInput(session, "colorshapeimport", choices = names(shapefile$shapefile))
      if(!is.null(mosaic_data$mosaic)){
        if(sf::st_crs(shapefile$shapefile) != sf::st_crs(mosaic_data$mosaic)){
          sendSweetAlert(
            session = session,
            title = "Invalid CRS",
            text = "The Coordinate Reference System (CRS) of the shapefile does
            not match the input mosaic. Trying to set the shapefile's CRS to match the mosaic one.",
            type = "warning"
          )
          shp <- shapefile$shapefile |> sf::st_transform(crs = sf::st_crs(mosaic_data$mosaic))
          shapefile$shapefile <- shp
        }
      }
      output$shapefile_mapview <- renderLeaflet({
        if(is.null(basemap$map)){
          if(ncol(shapefile$shapefile) ==1){
            mapp <- mapview::mapview(shapefile$shapefile, layer.name = "shapes")
          } else{
            req(input$colorshapeimport)

            mapp <- mapview::mapview(shapefile$shapefile,
                                     zcol = input$colorshapeimport,
                                     color = input$colorstroke,
                                     # col.regions = input$colorfill,
                                     alpha.regions = input$alphacolorfill,
                                     layer.name = "shapes")
          }
        } else{
          if(ncol(shapefile$shapefile) ==1){
            mapp <-
              basemap$map +
              mapview::mapview(shapefile$shapefile,
                               color = input$colorstroke,
                               col.regions = input$colorfill,
                               alpha.regions = input$alphacolorfill,
                               layer.name = "shapes")

          } else{
            req(input$colorshapeimport)
            mapp <-
              basemap$map +
              mapview::mapview(shapefile$shapefile,
                               zcol = input$colorshapeimport,
                               color = input$colorstroke,
                               alpha.regions = input$alphacolorfill,
                               layer.name = "shapes")
          }
        }
        mapp@map
      })
      observeEvent(c(input$editplots, !input$editdoneimpo),{
        if(input$editplots == TRUE){
          shapes <-
            shapefile[[input$shapefiletoanalyze]]$data |>
            poorman::mutate(`_leaflet_id` = 1:nrow(shapefile[[input$shapefiletoanalyze]]$data),
                            feature_type = "polygon") |>
            poorman::relocate(geometry, .after = 2) |>
            sf::st_transform(crs = 4326)

          if(is.null(basemap$map)){
            mapp <-
              leaflet::leaflet() |>
              addTiles(options = tileOptions(minZoom = 1, maxZoom = 30)) |>
              leaflet::addPolygons(
                data = shapes,
                color = input$colorstroke,
                fillColor = input$colorfill,
                opacity = 1,
                fillOpacity = input$alphacolorfill,
                group = "editable"
              )
          } else{
            mapp <-
              basemap$map@map |>
              leaflet::addPolygons(
                data = shapes,
                color = input$colorstroke,
                fillColor = input$colorfill,
                opacity = 1,
                fillOpacity = input$alphacolorfill,
                group = "editable"
              )
          }


          editedpoints <- callModule(editMod, "ploteditimpo",
                                     leafmap = mapp,
                                     targetLayerId = "editable")

          observeEvent(input$editdoneimpo,{
            if(input$editdoneimpo == TRUE){
              if(!is.null(editedpoints()$all)){

                shapefile[[input$shapefiletoanalyze]]$data <- editedpoints()$all |> poorman::select(geometry)
                output$plotshapedone <- renderLeaflet({
                  if(is.null(basemap$map)){
                    mapp <-
                      basemap$map +
                      mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                       color = input$colorstroke,
                                       col.regions = input$colorfill,
                                       alpha.regions = input$alphacolorfill,
                                       layer.name = "shapes")
                  } else{
                    mapp <-
                      mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                       color = input$colorstroke,
                                       col.regions = input$colorfill,
                                       alpha.regions = input$alphacolorfill,
                                       layer.name = "shapes")
                  }
                  mapp@map
                })
              }
            }
          })
        }
      })


    })



    mod_download_shapefile_server("downloadshapefile", terra::vect(shapefile$shapefile))


  })
}

## To be copied in the UI
# mod_shapefile_prepare_ui("shapefile_prepare_1")

## To be copied in the server
# mod_shapefile_prepare_server("shapefile_prepare_1")
