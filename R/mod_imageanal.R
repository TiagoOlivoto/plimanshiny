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
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("plotindexes"),
                  label = "Image index",
                  choices = "",
                  multiple = TRUE
                )
              ),
              col_6(
                textInput(ns("myindex"),
                          label = "My index",
                          value = ""),
              )
            ),
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("thresh"),
                  label = "Threshold",
                  choices = c("Otsu", "Adaptive", "Numeric")
                )
              ),
              col_6(
                conditionalPanel(
                  condition = "input.thresh == 'Numeric'", ns = ns,
                  sliderInput(ns("threshnum"),
                              label = "Threshold",
                              min = 0,
                              max = 0,
                              value = 0,
                              step = 0.005)
                ),
                conditionalPanel(
                  condition = "input.thresh == 'Adaptive'", ns = ns,
                  sliderInput(ns("windowsize"),
                              label = "Window size",
                              min = 0,
                              max = 0,
                              value = 0,
                              step = 1)
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
                    ),
                    prettyCheckbox(
                      inputId = ns("abangles"),
                      label = "A/B angles",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    prettyCheckbox(
                      inputId = ns("efourier"),
                      label = "Fourier",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    conditionalPanel(
                      condition = "input.efourier == true", ns = ns,
                      numericInput(
                        inputId = ns("nharm"),
                        label = "Harmonics",
                        value = 10
                      )
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
                    ),
                    prettyCheckbox(
                      inputId = ns("pcv"),
                      label = "PCV",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    prettyCheckbox(
                      inputId = ns("objectindex"),
                      label = "Object index",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    ),
                    conditionalPanel(
                      condition = "input.objectindex == true", ns = ns,
                      pickerInput(
                        inputId = ns("myobjectindex"),
                        label = "Object index",
                        choices = pliman_indexes(),
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        )
                      )
                    )
                  )
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
            fluidRow(
              col_4(
                numericInput(
                  inputId = ns("opening"),
                  label = "Opening",
                  value = 0
                )
              ),
              col_4(
                numericInput(
                  inputId = ns("closing"),
                  label = "Closing",
                  value = 0
                )
              ),
              col_4(
                numericInput(
                  inputId = ns("filter"),
                  label = "Median filter",
                  value = 0
                )
              )
            ),
            fluidRow(
              col_4(
                numericInput(
                  inputId = ns("lowernoise"),
                  label = "Lower noise",
                  value = 0.15
                )
              ),
              col_4(
                numericInput(
                  inputId = ns("lower_size"),
                  label = "Lower size",
                  value = NA
                )
              ),
              col_4(
                numericInput(
                  inputId =ns("upper_size"),
                  label = "Upper size",
                  value = NA
                )
              )
            ),
            fluidRow(
              col_6(
                materialSwitch(
                  inputId = ns("reference"),
                  label = "Reference object?",
                  value = FALSE,
                  status = "success"
                )
              ),
              col_6(
                materialSwitch(
                  inputId = ns("dpi"),
                  label = "Known resolution?",
                  value = FALSE,
                  status = "success"
                )
              )
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
            conditionalPanel(
              condition = "input.dpi", ns = ns,
              numericInput(
                inputId = ns("dpival"),
                label = "Image resolution (Dots Per Inch)",
                min = 1,
                value = NA
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
            hl(),
            numericInput(ns("maxpixel"),
                         label = "Maximum pixels",
                         value = 1e6),
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
            title = "Results (raw)",
            reactable::reactableOutput(ns("resultsindivtab"), height = "720px", width = 980)  |> add_spinner()
          ),
          tabPanel(
            title = "Results (summary)",
            reactable::reactableOutput(ns("resultssummary"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      )
    )

  )
}

#' imageanal Server Functions
#'
#' @noRd
mod_imageanal_server <- function(id, imgdata, dfs){
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
      if(input$dpi & input$reference){
        updateMaterialSwitch(session, "reference", value = FALSE)
        sendSweetAlert(
          session = session,
          title = "Ops, invalid options",
          text = "Only `reference` OR `dpi` can be selected.",
          type = "error"
        )
      }
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
      } else if(input$thresh == "Adaptive"){
        thresval <- "adaptive"
      } else {
        req(input$threshnum)
        thresval <- input$threshnum
      }

      if (is.na(input$upper_size)) {
        upper_size <- NULL
      } else {
        upper_size <- input$upper_size
      }
      if (is.na(input$lower_size)) {
        lower_size <- NULL
      } else {
        lower_size <- input$lower_size
      }
      if (is.na(input$dpival) | input$dpi == FALSE) {
        dpi <- NULL
      } else {
        dpi <- input$dpival
      }

      list(index = c(mindex, input$plotindexes),
           refsmaller = refsmaller,
           reflarger = reflarger,
           dpi = dpi,
           ext = ext,
           tol = tol,
           invert = input$invertindex,
           opening = input$opening,
           closing = input$closing,
           filter = input$filter,
           bfind = input$back_fore_index,
           frind = input$fore_ref_index,
           refarea =  na.omit(c(as.numeric(input$refareasiz), as.numeric(input$refareacol))),
           fillhull = input$fillhull,
           thresval = thresval,
           upper_size = upper_size,
           lower_size = lower_size,
           lower_noise = input$lowernoise)


    })

    output$index <- renderPlot({
      req(imgdata$img)
      req(parms()$index)
      ind <- image_index(imgdata$img, index = parms()$index, plot = FALSE)

      # ind <- image_index(parms()$img, index = parms()$index, plot = FALSE)
      ots <- otsu(ind[[1]]@.Data[!is.infinite(ind[[1]]@.Data) & !is.na(ind[[1]]@.Data)])
      updateSliderInput(session, "threshnum",
                        min = min(ind[[1]]),
                        max = max(ind[[1]]),
                        value = ots,
                        step = 0.0005)
      nc <- ncol(ind[[1]])
      nr <- nrow(ind[[1]])
      ws <- min(dim(ind[[1]])) / 10
      updateSliderInput(session, "windowsize",
                        min = 3,
                        max = max(nc, nr) / 2,
                        value = ws,
                        step = 1)


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
                    opening = parms()$opening,
                    closing = parms()$closing,
                    fill_hull = parms()$fillhull,
                    threshold = parms()$thresval,
                    windowsize = input$windowsize)
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
        if(input$objectindex){
          myobjectind <- input$myobjectindex
        } else{
          myobjectind <- NULL
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
                          windowsize = input$windowsize,
                          efourier = input$efourier,
                          nharm = input$nharm,
                          ab_angles = input$abangles,
                          object_index = myobjectind,
                          pcv = input$pcv,
                          filter = parms()$filter,
                          opening = parms()$opening,
                          closing = parms()$closing,
                          lower_noise = parms()$lower_noise,
                          lower_size = parms()$lower_size,
                          upper_size = parms()$upper_size,
                          plot = FALSE)

        waiter_hide()
        sendSweetAlert(
          session = session,
          title = "Image successfully analyzed!!",
          text = "The image has been analyzed and the results can now be seen in the tabs",
          type = "success"
        )


        output$resultsleafl <- renderLeaflet({
          sf_df <- sf::st_sf(
            geometry = lapply(res$contours, function(x) {
              tmp <- x
              tmp[, 2] <- ncol(imgdata$img) - tmp[, 2]
              sf::st_polygon(list(as.matrix(tmp |> poly_close())))
            }),
            data = data.frame(get_measures(res, dpi = parms()$dpi)),
            crs = sf::st_crs("EPSG:3857")
          )
          colnames(sf_df) <- gsub("data.", "", colnames(sf_df))
          (image_view(imgdata$img, max_pixels = input$maxpixel) + mapview::mapview(sf_df,
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



        output$resultsindivtab <- reactable::renderReactable({
          get_measures(res, dpi = parms()$dpi) |>
            as.data.frame() |>
            render_reactable()
        })
        dfs[["result_image_analysis"]] <- create_reactval("result_image_analysis",  get_measures(res, dpi = parms()$dpi) |>as.data.frame())


      } else{
        if(input$marker == "none"){
          marker <- FALSE
        } else{
          marker <- input$marker
        }
        imglist <- list.files(path = input$indir, pattern = input$pattern)
        # imglist <- list.files(path = "D:/Downloads/batch", pattern = "INDIV")
        results <- list()
        progressSweetAlert(
          session = session,
          id = "myprogress",
          title = "Start",
          display_pct = TRUE,
          value = 0,
          total = length(imglist)
        )
        if(input$objectindex){
          myobjectind <- input$myobjectindex
        } else{
          myobjectind <- NULL
        }

        for (i in seq_along(imglist)) {
          updateProgressBar(
            session = session,
            id = "myprogress",
            value = i,
            title = paste0("Analyzing image ", imglist[[i]],". Please, wait."),
            total = length(imglist)
          )

          results[[imglist[[i]]]] <-
            analyze_objects(img = file_name(imglist[[i]]),
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
                            dir_original = input$indir,
                            marker = marker,
                            show_contour = input$showcontour,
                            show_chull = input$showchull,
                            show_segmentation = input$showsegment,
                            show_lw = input$showlw,
                            threshold = parms()$thresval,
                            windowsize = input$windowsize,
                            efourier = input$efourier,
                            nharm = input$nharm,
                            ab_angles = input$abangles,
                            object_index = myobjectind,
                            pcv = input$pcv,
                            filter = parms()$filter,
                            opening = parms()$opening,
                            closing = parms()$closing,
                            lower_noise = parms()$lower_noise,
                            lower_size = parms()$lower_size,
                            upper_size = parms()$upper_size,
                            plot = FALSE)
        }
        req(results)
        waiter_hide()
        sendSweetAlert(
          session = session,
          title = "Images successfully analyzed!!",
          text = "The batch processing has been finished and the results can now be seen in the tabs",
          type = "success"
        )




        # bind the results
        stats <-
          do.call(rbind,
                  lapply(seq_along(results), function(i){
                    transform(results[[i]][["statistics"]],
                              id =  names(results[i]))[,c(3, 1, 2)]
                  })
          )

        if(input$objectindex){
          if(!is.null(results[[1]][["object_rgb"]])){
            obj_rgb <-
              do.call(rbind,
                      lapply(seq_along(results), function(i){
                        transform(results[[i]][["object_rgb"]],
                                  img =  names(results[i]))
                      })
              )
            obj_rgb <- obj_rgb[, c(ncol(obj_rgb), 1:ncol(obj_rgb) - 1)]
          } else{
            obj_rgb <- NULL
          }
          object_index <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["object_index"]],
                                img =  names(results[i]))
                    })
            )
          object_index <- object_index[, c(ncol(object_index), 1:ncol(object_index) - 1)]
        } else{
          obj_rgb <- NULL
          object_index <- NULL
        }



        if(input$efourier){
          efourier <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["efourier"]],
                                img =  names(results[i]))
                    })
            )
          efourier <- efourier[, c(ncol(efourier), 1:ncol(efourier)-1)]
          names(efourier)[2] <- "id"

          efourier_norm <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["efourier_norm"]],
                                img =  names(results[i]))
                    })
            )
          efourier_norm <- efourier_norm[, c(ncol(efourier_norm), 1:ncol(efourier_norm)-1)]
          names(efourier_norm)[2] <- "id"


          efourier_error <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["efourier_error"]],
                                img =  names(results[i]))
                    })
            )
          efourier_error <- efourier_error[, c(ncol(efourier_error), 1:ncol(efourier_error)-1)]
          names(efourier_error)[2] <- "id"

          efourier_power <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["efourier_power"]],
                                img =  names(results[i]))
                    })
            )
          efourier_power <- efourier_power[, c(ncol(efourier_power), 1:ncol(efourier_power)-1)]
          names(efourier_power)[2] <- "id"

          efourier_minharm <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["efourier_minharm"]],
                                img =  names(results[i]))
                    })
            )
          efourier_minharm <- efourier_minharm[, c(ncol(efourier_minharm), 1:ncol(efourier_minharm)-1)]
          names(efourier_minharm)[2] <- "id"

        } else{
          efourier <- NULL
          efourier_norm <- NULL
          efourier_error <- NULL
          efourier_power <- NULL
          efourier_minharm <- NULL
        }

        if(input$abangles){
          angles <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["angles"]],
                                img =  names(results[i]))
                    })
            )

          angles <- angles[, c(ncol(angles), 1:ncol(angles)-1)]
        } else{
          angles <- NULL
        }

        if(input$width_at){
          width_at <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["width_at"]],
                                img =  names(results[i]))
                    })
            )

          width_at <- width_at[, c(ncol(width_at), 1:ncol(width_at)-1)]
        } else{
          width_at <- NULL
        }

        if(input$pcv){
          pcv <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      data.frame(pcv = results[[i]][["pcv"]]) |>
                        transform(img =  names(results[i]))
                    })
            )

          pcv <- pcv[, c("img", "pcv")]
        } else{
          pcv <- NULL
        }

        results <-
          do.call(rbind,
                  lapply(seq_along(results), function(i){
                    transform(results[[i]][["results"]],
                              img =  names(results[i]))
                  })
          )

        if("img" %in% colnames(results)){
          results <- results[, c(ncol(results), 1:ncol(results) - 1)]
        }
        summ <- stats[stats$stat == "n", c(1, 3)]

        res <-
          structure(
            list(statistics = stats,
                 count = summ,
                 results = results,
                 obj_rgb = obj_rgb,
                 object_index = object_index,
                 efourier = efourier,
                 efourier_norm = efourier_norm,
                 efourier_error = efourier_error,
                 efourier_minharm = efourier_minharm,
                 veins = NULL,
                 angles = angles,
                 width_at = width_at,
                 pcv = pcv),
            class = "anal_obj_ls"
          )

        # get the measures
        rescor <- get_measures(res, dpi = parms()$dpi)

        # summary
        output$vbnindiv <- renderValueBox({
          valueBox(
            value = nrow(rescor$results),
            subtitle = "Number objects",
            color = "success",
            icon = icon("table-cells")
          )
        })
        output$vbnaveragearea <- renderValueBox({
          valueBox(
            value = round(mean(rescor$results$area), 3),
            subtitle = "Average area",
            color = "success",
            icon = icon("seedling")
          )
        })
        output$largerindiv <- renderValueBox({
          valueBox(
            value = round(max(rescor$results$area), 3),
            subtitle = "Larger individual (area)",
            color = "success",
            icon = icon("up-right-and-down-left-from-center")
          )
        })
        output$smallerindiv <- renderValueBox({
          valueBox(
            value = round(min(rescor$results$area), 3),
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


        output$resultsindivtab <- reactable::renderReactable({
          rescor$results |>
            render_reactable()
        })

        dfs[["result_image_analysis"]] <- create_reactval("result_image_analysis", rescor$results)

        ressumm <- rescor$summary
        ressumm$img <- gsub("img", "", ressumm$img)

        output$resultssummary <- reactable::renderReactable({
          ressumm |>
            render_reactable()
        })

      }


    })


    # send the results to the global environment
    observeEvent(input$savetoglobalenv, {
      if (exists(input$globalvarname, envir = globalenv())) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = paste0("The object'", input$globalvarname, "' already exists in the global environment. Please, change the name."),
          type = "success"
        )
      } else {
        assign(input$globalvarname, res, envir = globalenv())
        ask_confirmation(
          inputId = "myconfirmation",
          type = "warning",
          title = "Close the App?",
          text = paste0("The object'", input$globalvarname, "' has been created in the Global environment. To access the created object, you need first to stop the App. Do you really want to close the app now?"),
          btn_labels = c("Nope", "Yep"),
          btn_colors = c("#FE642E", "#04B404")
        )
      }
    })

    observe({
      if (!is.null(input$myconfirmation)) {
        if (input$myconfirmation) {
          stopApp()
        } else {
          # Do something else or simply return if the confirmation is false
          return()
        }
      }
    })



  })
}

## To be copied in the UI
# mod_imageanal_ui("imageanal_1")

## To be copied in the server
# mod_imageanal_server("imageanal_1")
