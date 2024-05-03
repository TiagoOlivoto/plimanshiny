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
                           selectInput(
                             ns("plotlayout"),
                             label = "Plot layout",
                             choices = c("lrtb", "lrbt", "tblr", "tbrl", "rltb", "rlbt"),
                             selected = "lrtb",
                           ),
                           selectInput(
                             ns("fillid"),
                             label = "Fill color",
                             choices = c("none", "unique_id", "block", "plot_id"),
                             selected = "none",
                           ),
                           sliderInput(ns("alphacolorfill"),
                                       label = "Fill opacity",
                                       min = 0,
                                       max = 1,
                                       value = 0.7),
                           sliderInput(ns("lwdt"),
                                       label = "Line width",
                                       min = 0,
                                       max = 5,
                                       value = 2),
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
        selectInput(ns("shapefiletoanalyze"),
                    label = "Active Shapefile",
                    choices = NULL),
        conditionalPanel(
          condition = "input.shapetype == 'Build'", ns = ns,
          divclass("shape3",
                   h3("Grid shape"),
                   textInput(ns("shapenamebuild"),
                             label = "Shapefile Name",
                             value = "Shapefile Build"),
                   fluidRow(
                     col_6(
                       prettyCheckbox(
                         inputId = ns("buildshapefile"),
                         label = "Control points",
                         value = TRUE,
                         status = "info",
                         icon = icon("thumbs-up"),
                         plain = TRUE,
                         outline = TRUE,
                         animation = "rotate"
                       )
                     ),
                     col_6(
                       prettyCheckbox(
                         inputId = ns("buildblocks"),
                         label = "Blocks",
                         value = FALSE,
                         status = "info",
                         icon = icon("thumbs-up"),
                         plain = TRUE,
                         outline = TRUE,
                         animation = "rotate"
                       )
                     )
                   ),
                   fluidRow(
                     col_6(
                       textInput(ns("ncols"),
                                 label = "Number of columns",
                                 value = 1)
                     ),
                     col_6(
                       textInput(ns("nrows"),
                                 label = "Number of rows",
                                 value = 1)
                     )
                   ),
                   fluidRow(
                     col_6(
                       textInput(ns("plot_width"),
                                 label = "Plot width",
                                 value = NA)
                     ),
                     col_6(
                       textInput(ns("plot_height"),
                                 label = "Plot height",
                                 value = NA)
                     )
                   )
          ),
          divclass("shape4",
                   fluidRow(
                     col_6(
                       textInput(ns("buffercol"),
                                 label = "Column buffer",
                                 value = 0)
                     ),
                     col_6(
                       textInput(ns("bufferrow"),
                                 label = "Row buffer",
                                 value = 0)
                     )
                   )
          ),
          divclass("shape5",
                   fluidRow(
                     col_7(
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
                     col_5(
                       actionButton(
                         inputId = ns("plotinfo"),
                         label = tagList(
                           icon = icon("question-circle", verify_fa = FALSE), "Plot info"
                         ),
                         class = "btn-info"
                       )
                     )
                   ),
                   conditionalPanel(
                     condition = "input.shapedone == true", ns = ns,
                     divclass("shapeimp3",
                              materialSwitch(
                                inputId = ns("editplots"),
                                label = "Edit the drawn plots?",
                                value = FALSE,
                                status = "danger"
                              ),
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
                              )
                     )
                   )
          )
        ),
        conditionalPanel(
          condition = "input.shapetype == 'Import'", ns = ns,
          tags$hr(),
          divclass("shapeimp1",
                   fileInput(ns("import_shapefile"),
                             "Import a shapefile (.shp, .rds)",
                             accept=c(".rds",  ".shp",  ".json", ".kml",  ".gml",  ".dbf",  ".sbn",  ".sbx",  ".shx",  ".prj", ".cpg"), multiple=TRUE)
          ),
          tags$hr(),
          divclass("shapeimp2",
                   selectInput(ns("colorshapeimport"),
                               label = "Fill color",
                               choices = NULL)
          ),
          fluidRow(
            col_6(
              materialSwitch(
                inputId = ns("editplotsimpo"),
                label = "Edit Shapefile?",
                value = FALSE,
                status = "danger"
              )
            ),
            col_6(
              actionButton(
                inputId = ns("plotinfo2"),
                label = tagList(
                  icon = icon("question-circle", verify_fa = FALSE), "Plot info"
                ),
                class = "btn-info"
              )
            )
          ),
          conditionalPanel(
            condition = "input.editplotsimpo == true", ns = ns,
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
          )
        ),
        tags$hr(),
        mod_download_shapefile_ui(ns("downloadshapefile"))
      )
    ),
    col_9(
      uiOutput(ns("uishape"))
    )

  )
}

helpshp <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "shape")
shapeimp <-
  read.csv(file = system.file("app/www/helps.csv", package = "plimanshiny", mustWork = TRUE), sep = ";") |>
  dplyr::filter(type == "shapeimp")
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

    output$uishape <- renderUI({
      if(input$shapetype == "Build" & !input$editplots & !input$shapedone){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Control points",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Control points",
            editModUI(ns("shapefile_build"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            "Built shapefile",
            leafletOutput(ns("createdshapes"), height = "700px") |> add_spinner()
          )
        )
      } else if(input$shapetype == "Build" & input$shapedone & !input$editplots){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Built shapefile",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Built shapefile",
            leafletOutput(ns("plotshapedone"), height = "700px") |> add_spinner()
          )
        )
      } else if(input$shapetype == "Build" & input$editplots & !input$editdone){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Built shapefile",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            "Built shapefile",
            leafletOutput(ns("plotshapedone"), height = "700px") |> add_spinner()
          ),
          tabPanel(
            "Plot edition",
            editModUI(ns("plotedit"), height = "700px") |> add_spinner()
          )
        )

      } else if(input$shapetype == "Import" & !input$editplotsimpo){
        bs4Card(
          width = 12,
          height = "780px",
          title = "Building the shapefile",
          color = "success",
          status = "success",
          maximizable = TRUE,
          h3("Built shapefile"),
          leafletOutput(ns("shapefile_mapview"), height = "700px") |> add_spinner()
        )
      } else if(input$shapetype == "Import" & input$editplotsimpo){
        bs4Card(
          width = 12,
          height = "780px",
          title = "Building the shapefile",
          color = "success",
          status = "success",
          maximizable = TRUE,
          fluidRow(
            col_6(
              h3("Built shapefile"),
              leafletOutput(ns("shapefile_mapview"), height = "700px") |> add_spinner()
            ),
            col_6(
              conditionalPanel(
                condition = "input.editplotsimpo == true & input.editdoneimpo == false", ns = ns,
                h3("Plot edition"),
                editModUI(ns("ploteditimpo"), height = "700px") |> add_spinner()
              )
            )
          )
        )
      }

    })

    observeEvent(input$buildshapefile, {
      if(!input$buildshapefile){
        updatePrettyCheckbox(session, "shapedone", value = TRUE)
      } else{
        updatePrettyCheckbox(session, "shapedone", value = FALSE)
      }
    })

    # GUIA
    observe({
      # observeEvent(input$shapetype, {
      if(!is.null(mosaic_data$mosaic)){
        req(basemap$map)
        if (input$shapetype == "Build") {
          nclayout <- reactiveValues(nc = NULL)
          nrlayout <- reactiveValues(nr = NULL)
          createdshape <- reactiveValues(shp = NULL)
          cpoints <- callModule(editMod, "shapefile_build", basemap$map@map, editor = "leafpm")
          observeEvent(c(cpoints()$finished,
                         !input$shapedone,
                         input$ncols,
                         input$nrows,
                         input$buffercol,
                         input$bufferrow,
                         input$plot_width,
                         input$plot_height,
                         input$buildshapefile,
                         input$plotlayout), {
                           if(input$buildshapefile){
                             if(!is.null(cpoints()$edited)){
                               cpo <- cpoints()$edited
                             } else{
                               cpo <- cpoints()$finished
                             }
                             # Check if edits()$finished is not NULL and shapedone is FALSE
                             if (!is.null(cpoints()$finished) && !input$shapedone) {
                               nr <- input$nrows |> chrv2numv()
                               nc <- input$ncols |> chrv2numv()
                               bc <- input$buffercol |> chrv2numv()
                               bf <- input$bufferrow |> chrv2numv()
                               pw <- input$plot_width |> chrv2numv()
                               ph <- input$plot_height |> chrv2numv()
                               t1 <- nr == 1
                               t2 <- nc == 1
                               ps <- any(t2 & t1 == TRUE)
                               if(length(ph) == 0 | ps){
                                 ph <- NULL
                               }
                               if(length(pw) == 0 | ps){
                                 pw <- NULL
                               }
                               if(all(!is.na(nc)) & all(!is.na(nr)) & all(!is.na(bc)) & all(!is.na(bf))){
                                 shp <- shapefile_build(mosaic_data$mosaic,
                                                        basemap,
                                                        controlpoints = cpo,
                                                        nrow = nr,
                                                        ncol = nc,
                                                        layout = input$plotlayout,
                                                        buffer_col = bc,
                                                        buffer_row = bf,
                                                        plot_width = pw,
                                                        plot_height = ph,
                                                        verbose = FALSE)
                                 # plot(shp)
                                 # Update the reactiveVal with the cropped mosaic
                                 createdshape$shp <- shp
                               }
                             }
                           } else{

                             nr <- input$nrows |> chrv2numv()
                             nc <- input$ncols |> chrv2numv()
                             bc <- input$buffercol |> chrv2numv()
                             bf <- input$bufferrow |> chrv2numv()
                             pw <- input$plot_width |> chrv2numv()
                             ph <- input$plot_height |> chrv2numv()
                             t1 <- nr == 1
                             t2 <- nc == 1
                             ps <- any(t2 & t1 == TRUE)
                             if(length(ph) == 0 | ps){
                               ph <- NULL
                             }
                             if(length(pw) == 0 | ps){
                               pw <- NULL
                             }
                             shp <- shapefile_build(mosaic_data$mosaic,
                                                    basemap,
                                                    nrow = nr,
                                                    ncol = nc,
                                                    layout = input$plotlayout,
                                                    buffer_col = bf,
                                                    buffer_row = br,
                                                    plot_width = pw,
                                                    plot_height = ph,
                                                    build_shapefile = FALSE,
                                                    verbose = FALSE)
                             createdshape$shp <- shp

                           }
                         })

          output$createdshapes <- renderLeaflet({
            req(createdshape$shp)
            updateSelectInput(session, "fillid",
                              choices = c("none", names(createdshape$shp)))
            if(input$buildblocks){
              if(input$fillid == "none"){
                mapp <-
                  basemap$map +
                  mapview::mapview(createdshape$shp,
                                   color = input$colorstroke,
                                   col.regions = input$colorfill,
                                   alpha.regions = input$alphacolorfill,
                                   legend = FALSE,
                                   lwd = input$lwdt,
                                   layer.name = "shapes")
              } else{
                mapp <-
                  basemap$map +
                  mapview::mapview(createdshape$shp |> dplyr::bind_rows() |>  extract_number(block, plot_id),
                                   zcol = input$fillid,
                                   alpha.regions = input$alphacolorfill,
                                   lwd = input$lwdt,
                                   layer.name = "shapes")
              }

            } else{
              nelem <- length(createdshape$shp)

              if(input$fillid == "none"){
                mapp <-
                  basemap$map +
                  mapview::mapview(createdshape$shp[[nelem]] |> extract_number(block, plot_id),
                                   color = input$colorstroke,
                                   col.regions = input$colorfill,
                                   alpha.regions = input$alphacolorfill,
                                   legend = FALSE,
                                   lwd = input$lwdt,
                                   layer.name = "shapes")
              } else{
                mapp <-
                  basemap$map +
                  mapview::mapview(createdshape$shp[[nelem]] |> extract_number(block, plot_id),
                                   zcol = input$fillid,
                                   alpha.regions = input$alphacolorfill,
                                   lwd = input$lwdt,
                                   layer.name = "shapes")
              }
            }

            mapp@map
          })

          observeEvent(input$shapedone,{

            req(input$shapenamebuild)
            observe({
              if(input$buildblocks){
                req(createdshape$shp)
                shptemp <-
                  createdshape$shp |>
                  dplyr::bind_rows() |>
                  dplyr::mutate(unique_id = dplyr::row_number(), .before = 1)
                shptemp <- split(shptemp, shptemp$block)
                shapefile[[input$shapenamebuild]] <- create_reactval(input$shapenamebuild, shptemp)
              } else{
                req(createdshape$shp)
                nelem <- length(createdshape$shp)
                shapefile[[input$shapenamebuild]] <- create_reactval(input$shapenamebuild, createdshape$shp[[nelem]]|> dplyr::mutate(unique_id = dplyr::row_number(), .before = 1))
              }
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
                  mapview::mapview(shapefile[[input$shapenamebuild]]$data |> extract_number(block, plot_id),
                                   color = input$colorstroke,
                                   col.regions = input$colorfill,
                                   lwd = input$lwdt,
                                   legend = FALSE,
                                   alpha.regions = input$alphacolorfill,
                                   layer.name = "shapes")
                mapp@map
              })
            })
          })


          observeEvent(c(input$editplots, !input$editdone),{
            if(inherits(shapefile[[input$shapenamebuild]]$data, "list")){
              shptmp <- do.call(rbind, shapefile[[input$shapenamebuild]]$data)
            } else{
              shptmp <- shapefile[[input$shapenamebuild]]$data
            }

            if(input$editplots == TRUE){
              shapes <-
                shptmp |>
                dplyr::mutate(`_leaflet_id` = 1:nrow(shptmp),
                              feature_type = "polygon") |>
                dplyr::relocate(geometry, .after = 2) |>
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
                    centplot <- sf::st_centroid(shapes)
                    shapefile[[input$shapenamebuild]]$data <- editedpoints()$all |> sf::st_transform(sf::st_crs(mosaic_data$mosaic))
                    output$plotshapedone <- renderLeaflet({
                      mapp <-
                        basemap$map +
                        mapview::mapview(shapefile[[input$shapenamebuild]]$data,
                                         color = input$colorstroke,
                                         z.col = "plot_id",
                                         alpha.regions = input$alphacolorfill,
                                         lwd = input$lwdt,
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

          observe({
            # Check if a mosaic is selected
            req(input$shapefiletoanalyze)
            selected_shp <- shapefile[[input$shapefiletoanalyze]]
            if ('data' %in% names(selected_shp)) {
              shapefile$shapefile <- selected_shp$data

            }
          })
        }
      }
    })

    # Import a shapefile
    observeEvent(input$import_shapefile, {
      observeEvent(input$shapetype, {
        if (input$shapetype == "Import") {
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


          observe({
            # Check if a mosaic is selected
            req(input$shapefiletoanalyze)
            selected_shp <- shapefile[[input$shapefiletoanalyze]]
            if ('data' %in% names(selected_shp)) {
              shapefile$shapefile <- selected_shp$data
            }
          })



          observe({
            req(shapefile$shapefile)  # Ensure mosaic_data$mosaic is not NULL

            updateSelectInput(session, "colorshapeimport", choices = names(shapefile$shapefile))
            updateSelectInput(session, "fillid", choices = c("none", names(shapefile$shapefile)))

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
                if(input$fillid == "none"){
                  mapp <- mapview::mapview(shapefile$shapefile,
                                           color = input$colorstroke,
                                           col.regions = input$colorfill,
                                           alpha.regions = input$alphacolorfill,
                                           legend = FALSE,
                                           lwd = input$lwdt,
                                           layer.name = "shapes")
                } else {
                  mapp <-
                    mapview::mapview(shapefile$shapefile,
                                     zcol = input$fillid,
                                     alpha.regions = input$alphacolorfill,
                                     lwd = input$lwdt,
                                     layer.name = "shapes")
                }
              } else{
                if(input$fillid == "none"){
                  mapp <-
                    basemap$map +
                    mapview::mapview(shapefile$shapefile,
                                           color = input$colorstroke,
                                           col.regions = input$colorfill,
                                           alpha.regions = input$alphacolorfill,
                                           legend = FALSE,
                                           lwd = input$lwdt,
                                           layer.name = "shapes")
                } else {
                  mapp <-
                    basemap$map +
                    mapview::mapview(shapefile$shapefile,
                                     zcol = input$fillid,
                                     alpha.regions = input$alphacolorfill,
                                     lwd = input$lwdt,
                                     layer.name = "shapes")
                }
              }
              mapp@map
            })

            observeEvent(c(input$editplotsimpo, !input$editdoneimpo),{
              if(input$editplotsimpo == TRUE){
                shapes <-
                  shapefile[[input$shapefiletoanalyze]]$data |>
                  dplyr::mutate(`_leaflet_id` = 1:nrow(shapefile[[input$shapefiletoanalyze]]$data),
                                feature_type = "polygon") |>
                  dplyr::relocate(geometry, .after = 2) |>
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

                      shapefile[[input$shapefiletoanalyze]]$data <-
                        editedpoints()$all |>
                        sf::st_transform(crs = sf::st_crs(mosaic_data$mosaic)) |>
                        dplyr::select(geometry)
                      output$plotshapedone <- renderLeaflet({
                        if(is.null(basemap$map)){
                          mapp <-
                            basemap$map +
                            mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                             color = input$colorstroke,
                                             col.regions = input$colorfill,
                                             alpha.regions = input$alphacolorfill,
                                             lwd = input$lwdt,
                                             layer.name = "shapes")
                        } else{
                          mapp <-
                            mapview::mapview(shapefile[[input$shapefiletoanalyze]]$data,
                                             color = input$colorstroke,
                                             col.regions = input$colorfill,
                                             alpha.regions = input$alphacolorfill,
                                             lwd = input$lwdt,
                                             layer.name = "shapes")
                        }
                        mapp@map
                      })
                    }
                    updateMaterialSwitch(session, "editplotsimpo", value = FALSE)
                  }
                })
              }
            })
          })
        }
      })
    })

    # Plot information
    observeEvent(c(input$plotinfo, input$plotinfo2), {
      distsss <- reactiveValues()
      perim <- reactiveValues()
      area <- reactiveValues()
      wid <- reactiveValues()
      hei <- reactiveValues()
      req(shapefile$shapefile)
      updateSelectInput(session, "uniqueidinfo",
                        choices = 1:nrow(shapefile$shapefile),
                        selected = 1)
      if(input$plotinfo | input$plotinfo2){


        output$plotinfop <- renderPlot({
          shpinfo <- shapefile$shapefile[input$uniqueidinfo, ]
          npoints <- sf::st_coordinates(shpinfo) |> nrow()
          coords <- sf::st_coordinates(shpinfo)[, 1:2]
          buff <- diff(range(coords[, 1])) * 0.15
          dists <-  coords |> dist() |> as.matrix()
          seq_dists <- c()
          for (i in 1:(ncol(dists) - 1)) {
            seq_dists <- c(seq_dists, dists[i, i + 1])
          }
          # distsss$val <- dists
          perim$val <- sf::st_perimeter(shpinfo)
          area$val <- sf::st_area(shpinfo)
          ncoors <-
            do.call(
              rbind,
              lapply(1:(nrow(coords) - 1), function(i){
                p1 <- coords[i, ]
                p2 <- coords[i + 1, ]
                p1 - (p1 - p2) / 2
              })
            )
          if(!is.null(mosaic_data$mosaic)){
            mcro <- terra::crop(mosaic_data$mosaic, terra::vect(shpinfo) |> terra::buffer(buff))
            if(terra::nlyr(mcro) > 2){
              terra::plotRGB(mcro, stretch = "hist")
            } else{
              terra::plot(mcro[[1]])
            }
            shapefile_plot(shpinfo, add = TRUE, col = adjustcolor("salmon", 0.9))
          } else{
            shapefile_plot(shpinfo, col =adjustcolor("salmon", 0.9))
          }
          wid$val <- ifelse(npoints > 5, "-", paste0(round(seq_dists[2], 3), " m"))
          hei$val <- ifelse(npoints > 5, "-", paste0(round(seq_dists[1], 3), " m"))
          if(npoints < 15){
            boxtext(x =  ncoors[, 1],
                    y =  ncoors[, 2],
                    labels = paste0(round(seq_dists, 2), " m"),
                    col.bg = "salmon",
                    cex = 1.5)
          }
          boxtext(x =  mean(ncoors[, 1]),
                  y =  mean(ncoors[, 2]),
                  labels = paste0(round(area$val, 2), " m2"),
                  col.bg = "salmon",
                  cex = 1.5)
        })

        output$nplots <- renderValueBox({
          valueBox(
            value = tags$p(nrow(shapefile$shapefile), style = "font-size: 200%;"),
            subtitle = "Number of shapes",
            color = "success",
            icon = icon("list-ol")
          )
        })
        output$exparea <- renderValueBox({
          valueBox(
            value = tags$p(round(sum(sf::st_area(shapefile$shapefile)), 3), style = "font-size: 200%;"),
            subtitle = "Covered area (m2)",
            color = "success",
            icon = icon("square")
          )
        })
        output$pwidth <- renderValueBox({
          valueBox(
            value = tags$p(wid$val, style = "font-size: 200%;"),
            subtitle = "Plot width (m)",
            color = "success",
            icon = icon("arrows-left-right")
          )
        })
        output$phight <- renderValueBox({
          valueBox(
            value = tags$p(hei$val, style = "font-size: 200%;"),
            subtitle = "Plot height (m)       ",
            color = "success",
            icon = icon("arrows-up-down")
          )
        })

        output$pperimeter <- renderValueBox({
          valueBox(
            value = tags$p(round(perim$val, 3), style = "font-size: 200%;"),
            subtitle = "Perimeter (m)       ",
            color = "success",
            icon = icon("draw-polygon")
          )
        })
        output$parea <- renderValueBox({
          valueBox(
            value = tags$p(round(area$val, 3), style = "font-size: 200%;"),
            subtitle = "Area (m2)       ",
            color = "success",
            icon = icon("draw-polygon")
          )
        })

        showModal(
          modalDialog(
            title = "Plot measures",
            fluidRow(
              col_5(
                h4("Overview"),
                fluidRow(
                  col_6(
                    valueBoxOutput(ns("nplots"), width = 12)
                  ),
                  col_6(
                    valueBoxOutput(ns("exparea"), width = 12),
                  )
                ),
                h4("Individual measures"),
                fluidRow(
                  col_6(
                    valueBoxOutput(ns("pwidth"), width = 12)
                  ),
                  col_6(
                    valueBoxOutput(ns("phight"), width = 12)
                  )
                ),
                fluidRow(
                  col_6(
                    valueBoxOutput(ns("parea"), width = 12)
                  ),
                  col_6(
                    valueBoxOutput(ns("pperimeter"), width = 12)
                  )
                )
              ),
              col_7(
                selectInput(ns("uniqueidinfo"),
                            label = "Unique id",
                            choices = NULL,
                            width = "100%"),
                plotOutput(ns("plotinfop"), height = "600px")
              )
            ),
            footer = NULL,
            easyClose = TRUE,
            size = "xl"
          )
        )
      }
    })

    # shape
    # terra::crop(mosaic, terra::ext(terra::vect(shape)))
    mod_download_shapefile_server("downloadshapefile", shapefile_input(shapefile$shapefile, as_sf = FALSE))


  })
}

## To be copied in the UI
# mod_shapefile_prepare_ui("shapefile_prepare_1")

## To be copied in the server
# mod_shapefile_prepare_server("shapefile_prepare_1")
