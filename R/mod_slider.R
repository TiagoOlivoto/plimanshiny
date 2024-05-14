#' slider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_slider_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        col_4(
          selectInput(
            ns("beforeimg"),
            label = "Image 1",
            choices = NULL
          ),
          selectInput(
            ns("afterimg"),
            label = "Image 2",
            choices = NULL
          ),
          actionBttn(
            ns("clearcache"),
            label = "Done",
            style = "pill",
            color = "success"
          )
        ),
        col_8(
          uiOutput(ns("slider"))
        )
      )

    )
  )
}

#' slider Server Functions
#'
#' @noRd
mod_slider_server <- function(id, imgdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      updateSelectInput(session, "beforeimg", choices = setdiff(names(imgdata), "img"), selected = NA)
      updateSelectInput(session, "afterimg", choices = setdiff(names(imgdata), "img"), selected = NA)
    })
    pathslider1 <- reactiveValues(val=NULL)
    pathslider2 <- reactiveValues(val=NULL)
    output$slider <- renderUI({
      # image1
      req(input$beforeimg)
      tfbef <- paste0(system.file("app", package = "plimanshiny" ), "/www/beforeimg_",file_name(input$beforeimg),".png")
      pathslider1$val <- tfbef
      image_export(imgdata[[input$beforeimg]]$data, tfbef)

      # image2
      req(input$afterimg)
      tfaft <- paste0(system.file("app", package = "plimanshiny" ), "/www/afterimg_",file_name(input$afterimg),".png")
      pathslider2$val <- tfaft
      image_export(imgdata[[input$afterimg]]$data, tfaft)
      # Regex pattern to match everything after "app/"
      pattern <- "app/(.*)"
      # Extract the captured group
      match1 <- regmatches(tfaft, regexec(pattern, tfaft))
      match2 <- regmatches(tfbef, regexec(pattern, tfbef))
      imgafter <- sub(match2, "\\1", match1[[1]][2])
      imgabefo <- sub(match2, "\\1", match2[[1]][2])

      HTML(
        paste0(
          '<div id="comparison">
         <style>
        div#comparison {
            width: 50vw; /* Increased width */
            height: 50vw; /* Increased height */
            max-width: 800px; /* Increased maximum width */
            max-height: 800px; /* Increased maximum height */
            overflow: hidden;
        }

        div#comparison figure {
            background-image: url(',imgafter,');
            background-size: cover;
            position: relative;
            font-size: 0;
            width: 100%;
            height: 100%;
            margin: 0;
        }

        div#comparison figure > img {
            position: relative;
            width: 100%;
        }

        div#comparison figure div {
            background-image: url(',imgabefo,');
            background-size: cover;
            position: absolute;
            width: 50%;
            box-shadow: 0 5px 10px -2px rgba(0, 0, 0, 0.7);
            overflow: hidden;
            bottom: 0;
            height: 100%;
        }

        input[type=range]{
            -webkit-appearance:none;
            -moz-appearance:none;
            position: relative;
            top: -2rem;
            left: -2%;
            background-color: rgba(25,255,25,0.2);
            width: 102%;
            }
            input[type=range]:focus {
            outline: none;
            }
            input[type=range]:active {
            outline: none;
            }

            input[type=range]::-moz-range-track {
            -moz-appearance:none;
            height:45px;
            width: 98%;
            background-color: rgba(25,255,25,0.1);
            position: relative;
            outline: none;
            }
            input[type=range]::active {
            border: none;
            outline: none;
            }
            input[type=range]::-webkit-slider-thumb {
            -webkit-appearance:none;
            width: 20px;
            height: 15px;
            border-radius: 5px;
            background: rgba(25,255,25,0.8);
            }
            input[type=range]::-moz-range-thumb {
            -moz-appearance: none;
            width: 20px;
            height: 15px;
            background: #fff;
            border-radius: 15;
            }
            input[type=range]:focus::-webkit-slider-thumb {
            height: 25px;
            border-radius: 5px;
            background: rgba(0,255,70,1);
            }
            input[type=range]:focus::-moz-range-thumb {
            height:45px;
            background: rgba(25,255,25,0.05);
            }
    </style>
         <figure>
            <div id="divisor"></div>
         </figure>
         <input type="range" min="0" max="100" value="50" id="slider" oninput="moveDivisor()">
       </div>'
        )
      )
    })

    # remove temp files after finishing the section
    observe({
      if(!is.null(pathslider1$val)){
        session$onSessionEnded(function() {
          f1 <- list.files(path = paste0(system.file("app", package = "plimanshiny" ), "/www/"),
                           pattern = "beforeimg_")
          f2 <- list.files(path = paste0(system.file("app", package = "plimanshiny" ), "/www/"),
                           pattern = "afterimg_")
          if(any(c(length(f1), length(f2)) != 0)){
            tmpimages <- paste0(paste0(system.file("app", package = "plimanshiny" ), "/www/"), c(f1, f2))
            a <- sapply(tmpimages, file.remove)
          }
        })
      }

    })

  })
}
## To be copied in the UI
# mod_slider_ui("slider_1")

## To be copied in the server
# mod_slider_server("slider_1")
