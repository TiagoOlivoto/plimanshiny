#' imageimport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imageimport_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        bs4Card(
          title = "Image input",
          color = "success",
          fluidRow(
            actionButton(
              inputId = ns("guideimg"),
              label = tagList(
                icon = icon("question-circle", verify_fa = FALSE), "Guide"
              ),
              style = "color: white ; background-color: #dd4b39",
              class = "btn-danger"
            )
          ),
          width = 12,
          status = "success",
          footer = "Here, you can configure the img for further analysis. First to import, you can choose the correct order of bands (layers), the maximum number of pixels to be rendered, and the upper and lower quantiles used for color stretching.",
          br(),
          div(class = "prep4",
              fileInput(ns("import_image"),
                        "Browse img file(s) (.jpg, .png)",
                        accept = c('.jpg','.jpeg','.png'),
                        multiple = TRUE)
          ),
          div(class = "prep5",
              selectInput(ns("activeimg"),
                          label = "Active image",
                          choices = NULL)
          ),
          prettyCheckbox(
            inputId = ns("resizeimg"),
            label = "Resize",
            value = FALSE,
            icon = icon("check"),
            status = "success",
            animation = "rotate"
          ),
          conditionalPanel(
            condition = "input.resizeimg == true", ns = ns,
            fluidRow(
              col_4(
                numericInput(
                  inputId = ns("relsize"),
                  label = "Relative size",
                  value = 100
                )
              ),
              col_4(
                numericInput(
                  inputId = ns("width"),
                  label = "Width (px)",
                  value = 0
                )
              ),
              col_4(
                numericInput(
                  inputId = ns("height"),
                  label = "Height (px)",
                  value = 0
                )
              )
            ),
            actionBttn(ns("resize"),
                       label = "Resize!",
                       style = "pill",
                       no_outline = FALSE,
                       icon = icon("expand"),
                       color = "success")
          ),

          hl(),
          div(class = "prep7",
              mod_download_mosaic_ui(ns("downloadimg"))
          )

        )
      ),
      col_9(
        bs4Card(
          width = 12,
          height = "780px",
          title = "Image view",
          color = "success",
          status = "success",
          maximizable = TRUE,
          plotOutput(ns("imgplot"), height = "740px") |> add_spinner()
        )
      )
    )
  )
}

#' imageimport Server Functions
#'
#' @noRd
mod_imageimport_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$import_image, {
      new_img_name <- input$import_image$name
      # pathimg$path <- input$import_image$datapath
      # Check if the img already exists in imgdata
      if (any(new_img_name %in% names(imgdata))) {
        # If it exists, update the existing reactiveValues
        moname <- new_img_name[new_img_name %in% names(imgdata)]
        ask_confirmation(
          inputId = "confirmimgname",
          type = "warning",
          title = "img already imported",
          text = paste0("The object '", paste0(moname, collapse = ", "), "' is already available in the list of imported imgs. Do you really want to overwrite it?"),
          btn_labels = c("Nope", "Yep"),
          btn_colors = c("#FE642E", "#04B404")
        )
        observe({
          if (!is.null(input$confirmimgname)) {
            if (input$confirmimgname) {
              for (i in 1:length(new_img_name)) {
                imgdata[[new_img_name[[i]]]] <- create_reactval(new_img_name[[i]], image_import(input$import_image$datapath[[i]]))
              }
            } else {
              return()
            }
          }
        })
      } else {
        # If it doesn't exist, create a new reactiveValues and add it to imgdata
        for (i in 1:length(new_img_name)) {
          imgdata[[new_img_name[[i]]]] <- create_reactval(new_img_name[[i]], image_import(input$import_image$datapath[[i]]))
        }
      }

      observe({
        imgnames <-  setdiff(names(imgdata), "img")
        # Update selectInput choices
        updateSelectInput(session, "activeimg",
                          choices = imgnames,
                          selected = imgnames[[1]])
      })

      observe({
        # Check if a mosaic is selected
        req(input$activeimg)
        selimg <- imgdata[[input$activeimg]]
        # # Check if the selected_mosaic is not NULL and has the 'data' field
        if ('data' %in% names(selimg)) {
          imgdata$img <- selimg$data
        }
      })

      observeEvent(input$resize, {
        if(input$resizeimg){
          if((input$width == 0 | is.na(input$width)) & (input$height == 0 | is.na(input$height))){
            resized <- image_resize(imgdata$img,
                                    rel_size = input$relsize)
          }
          if((input$width == 0 | is.na(input$width)) & (input$height != 0 & !is.na(input$height))){
            resized <- image_resize(imgdata$img,
                                    height = input$height)
          }
          if((input$width != 0 & !is.na(input$width)) & (input$height == 0 | is.na(input$height))){
            resized <- image_resize(imgdata$img,
                                    width = input$width)
          }
          if((input$width != 0 & !is.na(input$width)) & (input$height != 0 & !is.na(input$height))){
            resized <- image_resize(imgdata$img,
                                    width = input$width,
                                    height = input$height)
          }
          imgdata[[paste0(input$activeimg, "_resized")]] <- create_reactval(paste0(input$activeimg, "_resized"), data = resized)

          sendSweetAlert(
            session = session,
            title = "Resize process finished",
            text = paste0("The image has been resized to ", nrow(resized), " x ", ncol(resized)),
            type = "success"
          )

        }
      })

      output$imgplot <- renderPlot({
        req(imgdata$img)  # Ensure mosaic_data$mosaic is not NULL
        if(inherits(imgdata$img, "integer")){
          image(imgdata$img)
        } else{
          plot(imgdata$img)
        }
      })

    })


  })
}

## To be copied in the UI
# mod_imageimport_ui("imageimport_1")

## To be copied in the server
# mod_imageimport_server("imageimport_1")
