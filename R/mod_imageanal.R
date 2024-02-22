#' imageanal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imageanal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4TabCard(
          width = 12,
          # height = "790px",
          icon = icon("gears"),
          status  = "success",
          type = "tabs",
          tabPanel(
            title = "Configure the Analysis",
            actionBttn(ns("analyzeimg"),
                       label = "Analyze!",
                       status = "success",
                       icon = icon("wand-magic")),
            awesomeRadio(
              inputId = ns("singleorbatch"),
              label = "Method of analysis",
              choices = c("Single image", "Batch processing"),
              selected = "Single image",
              inline = TRUE,
              status = "success"
            ),
            conditionalPanel(
              condition = "input.singleorbatch == 'Batch processing'", ns = ns,
              fluidRow(
                col_4(
                  br(),
                  shinyDirButton(id=ns("sourcebatch"),
                                 label="Input folder",
                                 title="Input folder",
                                 buttonType = "default",
                                 class = NULL,
                                 icon = NULL,
                                 style = NULL),
                ),
                col_8(
                  textInput(ns("indir"),
                            label = "Input folder",
                            value = "")
                )
              ),
              textInput(ns("pattern"),
                        label = "Name pattern",
                        value = ""),
              hl()
            ),
            pickerInput(
              inputId = ns("plotindexes"),
              label = "Index to segment",
              choices = "",
              multiple = TRUE
            ),
            textInput(ns("myindex"),
                      label = "My personalized index",
                      value = ""),
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("thresh"),
                  label = "Threshold",
                  choices = c("Otsu", "Numeric")
                )
              ),
              col_6(
                conditionalPanel(
                  condition = "input.thresh == 'Numeric'", ns = ns,
                  numericInput(ns("threshnum"),
                               label = "Threshold",
                               value = NULL)
                )
              )
            ),
            fluidRow(
              col_6(
                fluidRow(
                  col_6(
                    prettyCheckbox(
                      inputId = ns("invertindex"),
                      label = "Invert",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    prettyCheckbox(
                      inputId = ns("width_at"),
                      label = "Width at",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_6(
                    prettyCheckbox(
                      inputId = ns("fillhull"),
                      label = "Fill Holes",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    prettyCheckbox(
                      inputId = ns("haralick"),
                      label = "Haralick",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  )
                ),
                numericInput(
                  inputId = ns("filter"),
                  label = "Median Filter",
                  value = 0
                )
              ),
              col_6(
                prettyCheckbox(
                  inputId = ns("watershed"),
                  label = "Watershed",
                  value = TRUE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                ),
                conditionalPanel(
                  condition = "input.watershed == true", ns = ns,
                  fluidRow(
                    col_6(
                      numericInput(ns("tolerance"),
                                   label = "Tolerance",
                                   value = NULL)
                    ),
                    col_6(
                      numericInput(ns("extension"),
                                   label = "Extension",
                                   value = NULL)

                    )
                  )
                )
              )
            ),
            materialSwitch(
              inputId = ns("reference"),
              label = "Reference object?",
              value = FALSE,
              status = "success"
            ),
            conditionalPanel(
              condition = "input.reference == true", ns = ns,
              switchInput(
                inputId = ns("reftype"),
                label = "Reference",
                labelWidth = "90px",
                onLabel = "By size",
                offLabel = "By  color",
                value = TRUE
              ),
              conditionalPanel(
                condition = "input.reftype == true", ns = ns,
                fluidRow(
                  col_6(
                    pickerInput(
                      inputId = ns("largesmall"),
                      label = "Reference type",
                      choices = c("Larger object", "Smaller object"),
                      selected = "Larger object"
                    )
                  ),
                  col_6(
                    numericInput(inputId = ns("refareasiz"),
                                 label = "Reference area",
                                 value = NA)
                  )
                )
              ),
              conditionalPanel(
                condition = "input.reftype == false", ns = ns,
                fluidRow(
                  col_4(
                    textInput(
                      inputId = ns("back_fore_index"),
                      label = "Back/Fore index",
                      value = "R/(G/B)"
                    )
                  ),
                  col_4(
                    textInput(
                      inputId = ns("fore_ref_index"),
                      label = "Fore/Ref index",
                      value = "B-R"
                    )
                  ),
                  col_4(
                    numericInput(inputId = ns("refareacol"),
                                 label = "Reference area",
                                 value = NA)
                  )
                )
              )
            ),
            materialSwitch(
              inputId = ns("saveimg"),
              label = "Save the processed image(s)?",
              value = FALSE,
              status = "success"
            ),
            conditionalPanel(
              condition = "input.saveimg == true", ns = ns,
              fluidRow(
                col_4(
                  br(),
                  shinyDirButton(id=ns("outfolder"),
                                 label="Output folder",
                                 title="Output folder",
                                 buttonType = "default",
                                 class = NULL,
                                 icon = NULL,
                                 style = NULL),
                ),
                col_8(
                  textInput(ns("outdir"),
                            label = "Output folder",
                            value = "")
                )
              ),
              hl(),
              fluidRow(
                col_4(
                  pickerInput(
                    inputId = ns("marker"),
                    label = "Marker",
                    choices = c("none", "point", "area", "length", "width")
                  ),
                ),
                col_4(
                  prettyCheckbox(
                    inputId = ns("showcontour"),
                    label = "Contour",
                    value = TRUE,
                    icon = icon("check"),
                    status = "success",
                    animation = "rotate"
                  ),
                  prettyCheckbox(
                    inputId = ns("showlw"),
                    label = "Length/Width",
                    value = FALSE,
                    icon = icon("check"),
                    status = "success",
                    animation = "rotate"
                  )
                ),
                col_4(
                  prettyCheckbox(
                    inputId = ns("showchull"),
                    label = "Convex Hull",
                    value = FALSE,
                    icon = icon("check"),
                    status = "success",
                    animation = "rotate"
                  ),
                  prettyCheckbox(
                    inputId = ns("showsegment"),
                    label = "Segmentation",
                    value = FALSE,
                    icon = icon("check"),
                    status = "success",
                    animation = "rotate"
                  )
                )
              ),
              hl()

            )
          ),
          tabPanel(
            title = "Configure Output",
            h3("Assign output to the R environment"),
            fluidRow(
              col_4(
                actionButton(
                  inputId = ns("savetoglobalenv"),
                  label = "Assign",
                  icon = icon("share-from-square"),
                  status = "success",
                  gradient = TRUE,
                  width = "150px",
                  flat = TRUE
                )
              ),
              col_8(
                textInput(ns("globalvarname"),
                          label = "Variable name",
                          value = "plimanshiny_output")
              )
            )
          )
        )
      ),
      col_8(
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Index",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Index",
            plotOutput(ns("index"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Density",
            plotOutput(ns("indexhist"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Segmentation",
            plotOutput(ns("segment"), height = "720px") |> add_spinner()
          ),
          tabPanel(
            title = "Summary",
            fluidRow(
              valueBoxOutput(ns("vbnindiv")),
              valueBoxOutput(ns("vbnaveragearea")),
              valueBoxOutput(ns("largerindiv")),
              valueBoxOutput(ns("smallerindiv"))
            ),
            fluidRow(
              col_4(
                h4("Area"),
                plotlyOutput(ns("boxarea"), height = "420px") |> add_spinner()
              ),
              col_4(
                h4("Length"),
                plotlyOutput(ns("boxlength"), height = "420px") |> add_spinner()
              ),
              col_4(
                h4("Width"),
                plotlyOutput(ns("boxwidth"), height = "420px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Results (plot)",
            leafletOutput(ns("resultsleafl"), height = "720px")  |> add_spinner(),
            downloadBttn(ns("downloadplotmap"),
                         label = "Take a shot",
                         style = "pill")
          ),
          tabPanel(
            title = "Results (data)",
            DT::dataTableOutput(ns("resultsindivtab"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      )
    )

  )
}

#' imageanal Server Functions
#'
#' @noRd
mod_imageanal_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updatePickerInput(session, "plotindexes",
                        choices = pliman_indexes_rgb(),
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ))
    })



    observe({
      volumes <- c("R Installation" = R.home(), getVolumes()())
      shinyDirChoose(input, "sourcebatch",
                     roots = volumes,
                     session = session,
                     restrictions = system.file(package = "base"))
      # in folder
      dirinput <- parseDirPath(volumes, input$sourcebatch)
      req(dirinput)
      updateTextInput(session, "indir", value = dirinput)
      # out folder

    })

    observe({
      volumes <- c("R Installation" = R.home(), getVolumes()())
      shinyDirChoose(input, "outfolder",
                     roots = volumes,
                     session = session,
                     restrictions = system.file(package = "base"))
      diroutput <- parseDirPath(volumes, input$outfolder)
      req(diroutput)
      updateTextInput(session, "outdir", value = diroutput)
    })


    parms <- reactive({
      # req(input$plotindexes)
      mindex <- strsplit(input$myindex, split = ",")[[1]]


      if(is.na(input$tolerance)){
        tol <- NULL
      } else{
        tol <- input$tolerance
      }
      if(is.na(input$extension)){
        ext <- NULL
      } else{
        ext <- input$extension
      }
      if(input$reference & input$reftype & input$largesmall == "Larger object"){
        reflarger <- TRUE
      } else{
        reflarger <- FALSE
      }
      if(input$reference & input$reftype & input$largesmall == "Smaller object"){
        refsmaller <- TRUE
      } else{
        refsmaller <- FALSE
      }
      if(input$thresh == "Otsu"){
        thresval <- "Otsu"
      } else{
        req(input$threshnum)
        thresval <- input$threshnum
      }

      list(index = c(mindex, input$plotindexes),
           refsmaller = refsmaller,
           reflarger = reflarger,
           ext = ext,
           tol = tol,
           invert = input$invertindex,
           filter = input$filter,
           bfind = input$back_fore_index,
           frind = input$fore_ref_index,
           refarea =  na.omit(c(input$refareasiz, input$refareacol)),
           fillhull = input$fillhull,
           thresval = thresval)


    })

    output$index <- renderPlot({
      req(imgdata$img)
      req(parms()$index)
      ind <- image_index(imgdata$img, index = parms()$index, plot = FALSE)

      output$indexhist <- renderPlot({
        ots <- otsu(ind[[1]]@.Data[!is.infinite(ind[[1]]@.Data) & !is.na(ind[[1]]@.Data)])
        plot(ind, type = "density")
        abline(v = ots)
        title(sub = paste0("Otsu's threshold: ", round(ots, 4)))
      })
      plot(ind)
    })

    output$segment <- renderPlot({
      req(imgdata$img)
      req(parms()$index)
      image_segment(imgdata$img,
                    index = parms()$index,
                    invert = parms()$invert,
                    filter = parms()$filter,
                    fill_hull = parms()$fillhull,
                    threshold = parms()$thresval)
    })


    observeEvent(input$analyzeimg, {

      if(input$singleorbatch == "Single image"){
        req(imgdata$img)
        waiter_show(
          html = tagList(
            spin_google(),
            h2("Analyzing the image Please, wait.")
          ),
          color = "#228B227F"
        )
        if(input$marker == "none"){
          marker <- FALSE
        } else{
          marker <- input$marker
        }

        res <-
          analyze_objects(imgdata$img,
                          index = parms()$index,
                          watershed = input$watershed,
                          tolerance = parms()$tol,
                          extension = parms()$ext,
                          reference = input$reference,
                          reference_larger = parms()$reflarger,
                          reference_smaller = parms()$refsmaller,
                          reference_area = parms()$refarea,
                          fore_ref_index = parms()$frind,
                          back_fore_index = parms()$bfind,
                          invert = parms()$invert,
                          fill_hull = parms()$fillhull,
                          haralick = input$haralick,
                          width_at = input$width_at,
                          save_image = input$saveimg,
                          dir_processed = input$outdir,
                          marker = marker,
                          show_contour = input$showcontour,
                          show_chull = input$showchull,
                          show_segmentation = input$showsegment,
                          show_lw = input$showlw,
                          threshold = parms()$thresval,
                          plot = FALSE)
      }


      output$resultsleafl <- renderLeaflet({
        sf_df <- sf::st_sf(
          geometry = lapply(res$contours, function(x) {
            tmp <- x
            tmp[, 2] <- ncol(imgdata$img) - tmp[, 2]
            sf::st_polygon(list(as.matrix(tmp |> poly_close())))
          }),
          data = data.frame(get_measures(res)),
          crs = sf::st_crs("EPSG:3857")
        )
        colnames(sf_df) <- gsub("data.", "", colnames(sf_df))
        (image_view(imgdata$img) + mapview::mapview(sf_df,
                                           zcol = "area",
                                           col.regions = grDevices::colorRampPalette(c("darkred", "yellow", "darkgreen"))(3)))@map

      })

      # summary
      output$vbnindiv <- renderValueBox({
        valueBox(
          value = res$statistics$value[[1]],
          subtitle = "Number objects",
          color = "success",
          icon = icon("table-cells")
        )
      })
      output$vbnaveragearea <- renderValueBox({
        valueBox(
          value = round(res$statistics$value[[3]], 3),
          subtitle = "Average area",
          color = "success",
          icon = icon("seedling")
        )
      })
      output$largerindiv <- renderValueBox({
        valueBox(
          value = round(res$statistics$value[[4]], 3),
          subtitle = "Larger individual (area)",
          color = "success",
          icon = icon("up-right-and-down-left-from-center")
        )
      })
      output$smallerindiv <- renderValueBox({
        valueBox(
          value = round(res$statistics$value[[2]], 3),
          subtitle = "Smaller individual (area)",
          color = "success",
          icon = icon("up-right-and-down-left-from-center")
        )
      })

      output$boxarea <- renderPlotly({

        p <-
          ggplot(res$results, aes(y = area)) +
          geom_boxplot(fill = "#28a745") +
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

        plotly::ggplotly(p, dynamicTicks = TRUE)
      })

      output$boxlength <- renderPlotly({
        p <-
          ggplot(res$results, aes(y = length)) +
          geom_boxplot(fill = "#28a745") +
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

        plotly::ggplotly(p, dynamicTicks = TRUE)
      })

      output$boxwidth <- renderPlotly({
        p <-
          ggplot(res$results, aes(y = width)) +
          geom_boxplot(fill = "#28a745") +
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())

        plotly::ggplotly(p, dynamicTicks = TRUE)
      })



      output$resultsindivtab <- DT::renderDataTable(
        get_measures(res) |> as.data.frame(),
        extensions = 'Buttons',
        rownames = FALSE,
        options = list(
          dom = 'Blrtip',
          buttons = c('copy', 'excel'),
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "620px",
          pageLength = 15
        )
      )
      waiter_hide()
      sendSweetAlert(
        session = session,
        title = "Image successfully analyzed!!",
        text = "The image has been analyzed and the results can now be seen in the tabs",
        type = "success"
      )




    })



  })
}

## To be copied in the UI
# mod_imageanal_ui("imageanal_1")

## To be copied in the server
# mod_imageanal_server("imageanal_1")
