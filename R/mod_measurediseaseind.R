#' measurediseaseind UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_measurediseaseind_ui <- function(id){
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
              prettyCheckbox(
                inputId = ns("parallel"),
                label = "Parallel processing",
                value = FALSE,
                icon = icon("check"),
                status = "success",
                animation = "rotate"
              ),
              hl()
            ),
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("indexlb"),
                  label = "Leaf/Background index",
                  choices = "",
                  multiple = TRUE
                )
              ),
              col_6(
                pickerInput(
                  inputId = ns("indexdh"),
                  label = "Diseased/Healthy index",
                  choices = "",
                  multiple = TRUE
                )
              )
            ),
            fluidRow(
              col_6(
                pickerInput(
                  inputId = ns("threshlb"),
                  label = "Threshold",
                  choices = c("Otsu", "Numeric")
                ),
                conditionalPanel(
                  condition = "input.threshlb == 'Numeric'", ns = ns,
                  sliderInput(ns("threshnumlb"),
                              label = "Threshold",
                              min = 0,
                              max = 0,
                              value = 0,
                              step = 0.05)
                )
              ),
              col_6(
                pickerInput(
                  inputId = ns("threshdh"),
                  label = "Threshold",
                  choices = c("Otsu", "Numeric")
                ),
                conditionalPanel(
                  condition = "input.threshdh == 'Numeric'", ns = ns,
                  sliderInput(ns("threshnumdh"),
                              label = "Threshold",
                              min = 0,
                              max = 0,
                              value = 0,
                              step = 0.05)
                )
              )
            ),
            fluidRow(
              col_6(
                fluidRow(
                  col_6(
                    prettyCheckbox(
                      inputId = ns("invertindexlb"),
                      label = "Invert",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  ),
                  col_6(
                    prettyCheckbox(
                      inputId = ns("fillhull"),
                      label = "Fill holes",
                      value = FALSE,
                      icon = icon("check"),
                      status = "success",
                      animation = "rotate"
                    )
                  )

                ),
                numericInput(
                  inputId = ns("openinglb"),
                  label = "Opening",
                  value = 10
                ),
                numericInput(
                  inputId = ns("closinglb"),
                  label = "Closing",
                  value = 0
                ),
                numericInput(
                  inputId = ns("filterlb"),
                  label = "Filter",
                  value = 0
                )
              ),
              col_6(
                prettyCheckbox(
                  inputId = ns("invertindexdh"),
                  label = "Invert",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                ),
                numericInput(
                  inputId = ns("openingdh"),
                  label = "Opening",
                  value = 0
                ),
                numericInput(
                  inputId = ns("closingdh"),
                  label = "Closing",
                  value = 0
                ),
                numericInput(
                  inputId = ns("filterdh"),
                  label = "Filter",
                  value = 0
                )
              )
            ),
            hl(),
            fluidRow(
              col_6(
                prettyCheckbox(
                  inputId = ns("showmask"),
                  label = "Show a mask",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                ),
                conditionalPanel(
                  condition = "input.showmask == true", ns = ns,
                  fluidRow(
                    col_6(
                      colorPickr(
                        inputId = ns("colorleaf"),
                        label = "Healthy",
                        swatches = scales::viridis_pal()(10),
                        theme = "monolith",
                        useAsButton = TRUE,
                        selected = "forestgreen",
                      )
                    ),
                    col_6(
                      colorPickr(
                        inputId = ns("diseasecolor"),
                        label = "Disease",
                        swatches = scales::viridis_pal()(10),
                        theme = "monolith",
                        useAsButton = TRUE,
                        selected = "brown",
                      )
                    )
                  )
                )
              ),
              col_6(
                prettyCheckbox(
                  inputId = ns("showcontour"),
                  label = "Contour",
                  value = TRUE,
                  icon = icon("check"),
                  status = "success",
                  animation = "rotate"
                ),
                conditionalPanel(
                  condition = "input.showcontour == true", ns = ns,
                  fluidRow(
                    col_5(
                      colorPickr(
                        inputId = ns("colorcont"),
                        label = "Color",
                        swatches = scales::viridis_pal()(10),
                        theme = "monolith",
                        useAsButton = TRUE,
                        selected = "red",
                      )
                    ),
                    col_7(
                      sliderInput(
                        inputId = ns("sizecont"),
                        label = "Size",
                        min = 1,
                        max = 6,
                        value = 3,
                        step = 0.1
                      )
                    )
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
                col_2(
                  pickerInput(
                    inputId = ns("marker"),
                    label = "Marker",
                    choices = c("none", "point", "area", "length", "width")
                  )
                ),
                col_2(
                  prettyCheckbox(
                    inputId = ns("sad"),
                    label = "SAD",
                    value = FALSE,
                    icon = icon("check"),
                    status = "success",
                    animation = "rotate"
                  )
                ),
                col_8(
                  conditionalPanel(
                    condition = "input.sad == true", ns = ns,
                    fluidRow(
                      col_4(
                        numericInput(
                          inputId = ns("sadnumber"),
                          label = "Classes",
                          value = 6
                        )
                      ),
                      col_4(
                        numericInput(
                          inputId = ns("nrows"),
                          label = "Rows",
                          value = 2
                        )
                      ),
                      col_4(
                        numericInput(
                          inputId = ns("ncols"),
                          label = "Columns",
                          value = 4
                        )
                      )
                    )
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
        uiOutput(ns("uiresults"))
      )
    )

  )
}

#' measurediseaseind Server Functions
#'
#' @noRd
mod_measurediseaseind_server <- function(id, imgdata, dfs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$uiresults <- renderUI({
      if(input$singleorbatch == "Single image"){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Segmentation",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Segmentation",
            plotOutput(ns("analyzed"), height = "720px")
          ),
          tabPanel(
            title = "Results",
            plotOutput(ns("resultplot"), height = "570px")  |> add_spinner(),
            plotlyOutput(ns("resultbar"), height = "150px") |> add_spinner()
          )
        )
      } else if(input$singleorbatch == "Batch processing" & !input$sad){
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Segmentation",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Segmentation",
            plotOutput(ns("analyzed"), height = "720px")
          ),
          tabPanel(
            title = "Results (plot)",
            fluidRow(
              col_8(
                plotlyOutput(ns("barplot"), height = "720px") |> add_spinner()
              ),
              col_4(
                plotlyOutput(ns("histplot"), height = "720px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Results (raw)",
            reactable::reactableOutput(ns("resultsindivtab"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      } else{
        bs4TabCard(
          id = "tabs",
          status = "success",
          width = 12,
          height = "790px",
          title = "Results",
          selected = "Segmentation",
          solidHeader = FALSE,
          type = "tabs",
          tabPanel(
            title = "Segmentation",
            plotOutput(ns("analyzed"), height = "720px")
          ),
          tabPanel(
            title = "Results (plot)",
            fluidRow(
              col_8(
                plotlyOutput(ns("barplot"), height = "720px") |> add_spinner()
              ),
              col_4(
                plotlyOutput(ns("histplot"), height = "720px") |> add_spinner()
              )
            )
          ),
          tabPanel(
            title = "Standard Area Diagram",
            plotOutput(ns("sadplot"), height = "720px")
          ),
          tabPanel(
            title = "Results (raw)",
            reactable::reactableOutput(ns("resultsindivtab"), height = "720px", width = 980)  |> add_spinner()
          )
        )
      }



    })
    observe({
      updatePickerInput(session, "indexlb",
                        choices = pliman_indexes_rgb(),
                        options = list(
                          `actions-box` = TRUE,
                          `live-search` = TRUE
                        ))
    })
    observe({
      updatePickerInput(session, "indexdh",
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
    segmentations <- reactiveValues(minlb = NULL,
                                    otsulb = NULL,
                                    maxlb = NULL,
                                    mindh = NULL,
                                    otsudh = NULL,
                                    maxlb = NULL)

    observe({
      if(!is.null(input$indexlb) & is.null(input$indexdh)){
        ind <- image_index(imgdata$img, index = input$indexlb, plot = FALSE)[[1]]
        segmentations$minlb <- min(ind)
        segmentations$otsulb <- otsu(ind)
        segmentations$maxlb <- max(ind)
      } else if(is.null(input$indexlb) & !is.null(input$indexdh)){
        ind2 <- image_index(imgdata$img, index = input$indexdh, plot = FALSE)[[1]]
        segmentations$mindh <- min(ind2)
        segmentations$otsudh <- otsu(ind2)
        segmentations$maxdh <- max(ind2)
      } else if(!is.null(input$indexlb) & !is.null(input$indexdh)){
        ind <- image_index(imgdata$img, index = input$indexlb, plot = FALSE)[[1]]
        segmentations$minlb <- min(ind)
        segmentations$otsulb <- otsu(ind)
        segmentations$maxlb <- max(ind)

        segg <- image_segment(imgdata$img, input$indexlb, plot = FALSE)
        ind2 <- image_index(segg, index = input$indexdh, plot = FALSE)[[1]]
        segmentations$mindh <- min(ind2)
        segmentations$otsudh <- otsu(ind2)
        segmentations$maxdh <- max(ind2)

      } else{

      }
    })


    observe({
      if(!is.null(input$indexlb)){
        updateSliderInput(session, "threshnumlb",
                          min = segmentations$minlb,
                          max = segmentations$maxlb,
                          value = segmentations$otsulb,
                          step = 0.0005
        )
      }
      if(!is.null(input$indexdh)){
        updateSliderInput(session, "threshnumdh",
                          min = segmentations$mindh,
                          max = segmentations$maxdh,
                          value = segmentations$otsudh,
                          step = 0.0005
        )
      }
    })

    output$analyzed <- renderPlot({
      req(imgdata$img)
      if(!is.null(input$indexlb) & is.null(input$indexdh)){
        if(input$threshlb == "Otsu"){
          thresh1 <- "Otsu"
        } else{
          thresh1 <- input$threshnumlb
        }
        oldpar <- par()
        par(mfrow = c(1, 2))
        on.exit(oldpar)
        seg <-
          image_segment(imgdata$img,
                        index = input$indexlb,
                        threshold = thresh1,
                        invert = input$invertindexlb,
                        opening = input$openinglb,
                        closing = input$closinglb,
                        filter = input$filterlb,
                        fill_hull = input$fillhull,
                        plot =FALSE)
        plot(imgdata$img)
        plot(seg)

      } else if(is.null(input$indexlb) & !is.null(input$indexdh)){
        if(input$threshdh == "Otsu"){
          thresh2 <- "Otsu"
        } else{
          thresh2 <- input$threshnumdh
        }
        oldpar <- par()
        par(mfrow = c(1, 2))
        on.exit(oldpar)
        seg <-
          image_segment(imgdata$img,
                        index = input$indexdh,
                        invert = input$invertindexdh,
                        opening = input$openingdh,
                        closing = input$closingdh,
                        filter = input$filterdh,
                        plot = FALSE)
        plot(imgdata$img)
        plot(seg)
      } else if(!is.null(input$indexlb) & !is.null(input$indexdh)){
        if(input$threshlb == "Otsu"){
          thresh1 <- "Otsu"
        } else{
          thresh1 <- input$threshnumlb
        }
        if(input$threshdh == "Otsu"){
          thresh2 <- "Otsu"
        } else{
          thresh2 <- input$threshnumdh
        }
        oldpar <- par()
        par(mfrow = c(1, 3))
        on.exit(oldpar)

        seg1 <- image_segment(img = imgdata$img,
                              index = input$indexlb,
                              threshold = thresh1,
                              invert = input$invertindexlb,
                              opening = input$openinglb,
                              closing = input$closinglb,
                              filter = input$filterlb,
                              fill_hull = input$fillhull,
                              plot = FALSE)
        seg2 <- image_segment(img = seg1,
                              index = input$indexdh,
                              threshold = thresh2,
                              opening = input$openingdh,
                              closing = input$closingdh,
                              filter = input$filterdh,
                              invert = input$invertindexdh,
                              plot = FALSE)
        plot(imgdata$img)
        plot(seg1)
        plot(seg2)


      } else{

      }

    })


    sevsad <- reactiveValues(sev = NULL)

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
        req(input$indexdh)
        tmpf <- tempdir()
        if(input$threshlb == "Otsu"){
          thresh1 <- "Otsu"
        } else{
          thresh1 <- input$threshnumlb
        }
        if(input$threshdh == "Otsu"){
          thresh2 <- "Otsu"
        } else{
          thresh2 <- input$threshnumdh
        }
        sev <-
          measure_disease(
            fill_hull = input$fillhull,
            img = imgdata$img,
            index_lb = input$indexlb,
            index_dh = input$indexdh,
            closing = c(input$closinglb, input$closingdh),
            opening = c(input$openinglb, input$openingdh),
            filter = c(input$filterlb, input$filterdh),
            col_leaf = input$colorleaf,
            col_lesions = input$diseasecolor,
            show_original = !input$showmask,
            threshold = c(thresh1, thresh2),
            invert = c(input$invertindexlb, input$invertindexdh),
            contour_col = input$colorcont,
            contour_size = input$sizecont,
            save_image = TRUE,
            plot = FALSE,
            dir_processed = tmpf,
            prefix = "proc_shiny_disease"
          )

        req(sev)
        sevsad$sev <- sev
        output$resultplot <- renderPlot({
          fil <- image_import(list.files(path = tmpf, pattern = "proc_shiny_disease"),
                              path = tmpf)
          if(input$saveimg){
            dir_processed = input$outdir
            jpeg(paste0(dir_processed, "/proc_img.jpg"), width = nrow(fil), height = ncol(fil))
            plot(fil)
            dev.off()
          }
          plot(fil)
        })

        output$resultbar <- renderPlotly({
          df <- data.frame(label = c("healthy", "symptomatic"),
                           vals = c(sev$severity[[1]], sev$severity[[2]]))
          p <-
            ggplot(df, aes(y = "", x = vals, fill = label)) +
            geom_bar(stat = "identity", position = "fill") +
            geom_text(aes(label = sprintf("%.2f%%", vals)), position = position_fill(vjust = 0.5)) +
            scale_fill_manual(values = c("forestgreen", "brown")) +
            labs(y = NULL,
                 x = "Proportion") +
            theme_minimal()
          plotly::ggplotly(p)
        })

        waiter_hide()
        sendSweetAlert(
          session = session,
          title = "Image successfully analyzed!!",
          text = "The image has been analyzed and the results can now be seen in the tabs",
          type = "success"
        )

        # Multiple images
      } else{
        if(input$marker == "none"){
          marker <- FALSE
        } else{
          marker <- input$marker
        }

        if(input$threshlb == "Otsu"){
          thresh1 <- "Otsu"
        } else{
          thresh1 <- input$threshnumlb
        }
        if(input$threshdh == "Otsu"){
          thresh2 <- "Otsu"
        } else{
          thresh2 <- input$threshnumdh
        }
        if(!input$parallel){
          imglist <- list.files(path = input$indir, pattern = input$pattern)
          results <- list()
          progressSweetAlert(
            session = session,
            id = "myprogress",
            title = "Start",
            display_pct = TRUE,
            value = 0,
            total = length(imglist)
          )
          for (i in seq_along(imglist)) {
            updateProgressBar(
              session = session,
              id = "myprogress",
              value = i,
              title = paste0("Analyzing image ", imglist[[i]],". Please, wait."),
              total = length(imglist)
            )
            sev <-
              measure_disease(
                img = file_name(imglist[[i]]),
                index_lb = input$indexlb,
                index_dh = input$indexdh,
                closing = c(input$closinglb, input$closingdh),
                opening = c(input$openinglb, input$openingdh),
                filter = c(input$filterlb, input$filterdh),
                fill_hull = input$fillhull,
                col_leaf = input$colorleaf,
                col_lesions = input$diseasecolor,
                show_original = !input$showmask,
                threshold = c(thresh1, thresh2),
                invert = c(input$invertindexlb, input$invertindexdh),
                contour_col = input$colorcont,
                contour_size = input$sizecont,
                save_image = input$saveimg,
                dir_processed = input$outdir,
                dir_original = input$indir,
                plot = FALSE
              )
            results[[imglist[[i]]]] <- sev
          }
          severity <-
            do.call(rbind,
                    lapply(seq_along(results), function(i){
                      transform(results[[i]][["severity"]],
                                img =  names(results[i]))[, c(3, 1:2)]
                    })
            )

          sevsad$sev <- list(severity = severity)
          waiter_hide()
          sendSweetAlert(
            session = session,
            title = "Images successfully analyzed!!",
            text = "The batch processing has been finished and the results can now be seen in the tabs",
            type = "success"
          )
        } else{
          nimgs <- length(list.files(input$indir, pattern = input$pattern))
          waiter_show(
            html = tagList(
              spin_google(),
              h2(paste0("Analyzing ", nimgs, " images in multiple sessions. Please, wait."))
            ),
            color = "#228B227F"
          )
          sev <-
            measure_disease(
              pattern = input$pattern,
              dir_original = input$indir,
              index_lb = input$indexlb,
              index_dh = input$indexdh,
              closing = c(input$closinglb, input$closingdh),
              opening = c(input$openinglb, input$openingdh),
              filter = c(input$filterlb, input$filterdh),
              fill_hull = input$fillhull,
              col_leaf = input$colorleaf,
              col_lesions = input$diseasecolor,
              show_original = !input$showmask,
              threshold = c(thresh1, thresh2),
              invert = c(input$invertindexlb, input$invertindexdh),
              contour_col = input$colorcont,
              contour_size = input$sizecont,
              save_image = input$saveimg,
              dir_processed = input$outdir,
              parallel = TRUE,
              plot = FALSE
            )
          sevsad$sev <- sev
          waiter_hide()
          sendSweetAlert(
            session = session,
            title = "Images successfully analyzed!!",
            text = "The batch processing has been finished and the results can now be seen in the tabs",
            type = "success"
          )
        }
        p1 <-
          ggplot(sevsad$sev$severity, aes(y = symptomatic)) +
          geom_boxplot(fill = "forestgreen") +
          theme_minimal() +
          theme(axis.text.x = element_blank(),
                panel.grid.major.x = element_blank()) +
          labs(y = "Symptomatic (%)",
               y = "")
        p2 <-
          ggplot(sevsad$sev$severity, aes(y = reorder(img, -symptomatic), x = symptomatic)) +
          geom_col(fill = "forestgreen") +
          theme_minimal()+
          theme(panel.grid.major.x = element_blank()
                ) +
          labs(x = "Symptomatic (%)",
               y = "Image")
        output$barplot <- renderPlotly({
          plotly::ggplotly(p2)
        })
        output$histplot <- renderPlotly({
          plotly::ggplotly(p1)
        })

        output$resultsindivtab <- reactable::renderReactable(
          sevsad$sev$severity |>
            roundcols(digits = 3) |>
            render_reactable()
        )

      }
    })

    observe({
      if(input$saveimg & input$sad & input$singleorbatch == "Batch processing"){
        req(sevsad$sev)
        measures <-
          sevsad$sev$severity |>
          transform(rank = rank(symptomatic))
        nsamples <- input$sadnumber
        n <- nrow(measures)
        seq <- trunc(seq(1, n, length.out = nsamples))
        seq[c(1, length(seq))] <- c(1, n)
        leaves <- measures[which(measures$rank %in% seq),]
        leaves <- leaves[order(leaves$rank),]
        leaves_name <- paste0("proc_", leaves$img, ".jpg")
        sads <- image_import(leaves_name, path = input$outdir)

        output$sadplot <- renderPlot({
          image_combine(sads,
                        labels = paste0(round(leaves$symptomatic, 1), "%"),
                        ncol = input$ncols,
                        nrow = input$nrows)
        })

      }
    })

    observe({
      req(sevsad$sev$severity)
      dfs[["phytopathometry_results_ind"]] <- create_reactval("phytopathometry_results", sevsad$sev$severity)
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
        assign(input$globalvarname, sevsad$sev, envir = globalenv())
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
# mod_measurediseaseind_ui("measurediseaseind_1")

## To be copied in the server
# mod_measurediseaseind_server("measurediseaseind_1")
