#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
#' @importFrom shiny tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      tags$li
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}
#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#'
#' @return An HTML tag
#' @noRd
#'
#' @examples
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' @importFrom shiny tags tagAppendAttributes tagList
#'
list_to_p <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$p
      )
    )
  } else {
    res <- lapply(
      list,
      tags$p
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' @importFrom shiny tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y) {
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#'
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[attrs[i]]] <- NULL
  }
  tag
}

#' Hide or display a tag
#'
#' @param tag the tag
#'
#' @return a tag
#' @noRd
#'
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' @importFrom shiny tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) &&
    !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;",
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom shiny tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) &&
    grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*",
      "",
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#'
#' @param id the id of the element to hide
#'
#' @noRd
#'
#' @importFrom shiny tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#'
#' @examples
#' with_red_star("Enter your name here")
#' @importFrom shiny tags HTML
with_red_star <- function(text) {
  shiny::tags$span(
    HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red",
          "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#'
#' @examples
#' rep_br(5)
#' @importFrom shiny HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' @importFrom shiny tags
enurl <- function(url, text) {
  tags$a(href = url, text)
}

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  column(8, ...)
}

#' @importFrom shiny column
col_9 <- function(...) {
  column(9, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  column(6, ...)
}

#' @importFrom shiny column
col_7 <- function(...) {
  column(7, ...)
}


#' @importFrom shiny column
col_5 <- function(...) {
  column(5, ...)
}

#' @importFrom shiny column
col_4 <- function(...) {
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...) {
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...) {
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...) {
  column(1, ...)
}

#' @importFrom shiny div
divclass <- function(class, ...) {
  div(class = class, ...)
}


pickerpalette <- function(id, inputid, n = 50, ...){
  ns <- NS(id)
  palettePicker(
    inputId = ns(inputid),
    label = "Color palette",
    choices = list(
      "Viridis" = list(
        "viridis" = scales::viridis_pal(option = "viridis")(n),
        "magma" = scales::viridis_pal(option = "magma")(n),
        "inferno" = scales::viridis_pal(option = "inferno")(n),
        "plasma" = scales::viridis_pal(option = "plasma")(n),
        "cividis" = scales::viridis_pal(option = "cividis")(n),
        "set1" = grDevices::colorRampPalette(c("yellow", "#53CC67", "#009B95", "#00588B","#4B0055"))(n),
        "set2" = grDevices::colorRampPalette(c("darkred", "yellow", "darkgreen"))(n),
        "set3" = rev(grDevices::terrain.colors(n))
      ),
      "Brewer" = list(
        "Blues" = colorRampPalette(scales::brewer_pal(palette = "Blues")(8))(n),
        "Reds" = colorRampPalette(scales::brewer_pal(palette = "Reds")(8))(n),
        "Greens " = colorRampPalette(scales::brewer_pal(palette = "Greens")(8))(n),
        "Oranges" = colorRampPalette(scales::brewer_pal(palette = "Oranges")(8))(n),
        "Paired" = colorRampPalette(scales::brewer_pal(palette = "Paired")(8))(n)
      ),
      "Others" = list(
        "WaterSoil" = custom_palette(c("#00008B", "#D2B48C", "#8B4513"), n = n),
        "PlantSoil" = custom_palette(c("forestgreen", "#B2DF8A", "#8B4513"), n = n),
        "BrBG" = colorRampPalette(scales::brewer_pal(palette = "BrBG")(8))(n),
        "PiYG" = colorRampPalette(scales::brewer_pal(palette = "PiYG")(8))(n),
        "PRGn" = colorRampPalette(scales::brewer_pal(palette = "PRGn")(8))(n),
        "PuOr" = colorRampPalette(scales::brewer_pal(palette = "PuOr")(8))(n),
        "RdBu" = colorRampPalette(scales::brewer_pal(palette = "RdBu")(8))(n),
        "RdGy" = colorRampPalette(scales::brewer_pal(palette = "RdGy")(8))(n),
        "RdYlBu" = colorRampPalette(scales::brewer_pal(palette = "RdYlBu")(8))(n),
        "RdYlGn" =colorRampPalette(scales::brewer_pal(palette = "RdYlGn")(8))(n),
        "Spectral" = colorRampPalette(scales::brewer_pal(palette = "Spectral")(8))(n)
      ),
      "Qualitative" = list(
        "ggplot2" = c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"),
        "Accent" = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F"),
        "Dark2" = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"),
        "Paired" = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C"),
        "Pastel1" = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC"),
        "Pastel2" = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE"),
        "Brewer1" = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"),
        "Brewer2" = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"),
        "Brewer3" = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462")
      )
    ),
    textColor = c(rep("white", 5), "black", "white", rep("black", 6), rep("white", 10), rep("black", 10)),
    ...
  )
}


return_colors <- function(pal, reverse = FALSE, n = 8){
  pals <-
    switch(pal,
           "viridis" = scales::viridis_pal(option = "viridis")(n),
           "magma" = scales::viridis_pal(option = "magma")(n),
           "inferno" = scales::viridis_pal(option = "inferno")(n),
           "plasma" = scales::viridis_pal(option = "plasma")(n),
           "cividis" = scales::viridis_pal(option = "cividis")(n),
           "set1" = grDevices::colorRampPalette(c("yellow", "#53CC67", "#009B95", "#00588B","#4B0055"))(n),
           "set2" = grDevices::colorRampPalette(c("darkred", "yellow", "darkgreen"))(n),
           "set3" = rev(grDevices::terrain.colors(n)),
           "Blues" = colorRampPalette(scales::brewer_pal(palette = "Blues")(8))(n),
           "Reds" = colorRampPalette(scales::brewer_pal(palette = "Reds")(8))(n),
           "Greens " = colorRampPalette(scales::brewer_pal(palette = "Greens")(8))(n),
           "Oranges" = colorRampPalette(scales::brewer_pal(palette = "Oranges")(8))(n),
           "Paired" = colorRampPalette(scales::brewer_pal(palette = "Paired")(8))(n),
           "WaterSoil" = custom_palette(c("#00008B", "#D2B48C", "#8B4513"), n = n),
           "PlantSoil" = custom_palette(c("forestgreen", "#B2DF8A", "#8B4513"), n = n),
           "BrBG" = colorRampPalette(scales::brewer_pal(palette = "BrBG")(8))(n),
           "PiYG" = colorRampPalette(scales::brewer_pal(palette = "PiYG")(8))(n),
           "PRGn" = colorRampPalette(scales::brewer_pal(palette = "PRGn")(8))(n),
           "PuOr" = colorRampPalette(scales::brewer_pal(palette = "PuOr")(8))(n),
           "RdBu" = colorRampPalette(scales::brewer_pal(palette = "RdBu")(8))(n),
           "RdGy" = colorRampPalette(scales::brewer_pal(palette = "RdGy")(8))(n),
           "RdYlBu" = colorRampPalette(scales::brewer_pal(palette = "RdYlBu")(8))(n),
           "RdYlGn" =colorRampPalette(scales::brewer_pal(palette = "RdYlGn")(8))(n),
           "Spectral" = colorRampPalette(scales::brewer_pal(palette = "Spectral")(8))(n),
           "ggplot2" = sample(c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Accent" = sample(c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Dark2" = sample(c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Paired" = sample(c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Pastel1" = sample(c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Pastel2" = sample(c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Brewer1" = sample(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Brewer2" = sample(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"), n, replace = ifelse(n > 6, TRUE, FALSE)),
           "Brewer3" = sample(c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462"),  n, replace = ifelse(n > 6, TRUE, FALSE))
  )
  if(reverse){
    return(rev(pals))
  } else{
    return(pals)
  }
}
add_spinner <- function(x, type = 4, color = "#28a745"){
  shinycssloaders::withSpinner(x, type = type, color = color)
}
#' Make the current tag behave like an action button
#'
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param inputId Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   link <- a(href = "#", "My super link", style = "color: lightblue;")
#'
#'   ui <- fluidPage(
#'     make_action_button(link, inputId = "mylink")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$mylink, {
#'       showNotification("Pouic!")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
make_action_button <- function(tag, inputId = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) stop("Must provide a shiny tag.")
  if (!is.null(tag$attribs$class)) {
    if (grep("action-button", tag$attribs$class)) {
      stop("tag is already an action button")
    }
  }
  if (is.null(inputId) && is.null(tag$attribs$id)) {
    stop("tag does not have any id. Please use inputId to be able to
           access it on the server side.")
  }

  # handle id
  if (!is.null(inputId)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$",
          tag$attribs$id,
          "to access it from the server side. inputId will be ignored."
        )
      )
    } else {
      tag$attribs$id <- inputId
    }
  }

  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button")
  }
  # return tag
  tag
}


# UNCOMMENT AND USE
#
# attachment::att_amend_desc()
#
# To use this part of the UI
#
#' #' Include Content From a File
#' #'
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #'
#' #' @rdname includeRMarkdown
#' #' @export
#' #'
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom shiny HTML
#' includeRMarkdown <- function(path){
#'
#'   md <- tempfile(fileext = '.md')
#'
#'   on.exit(unlink(md),add = TRUE)
#'
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'
#'   Encoding(html) <- "UTF-8"
#'
#'   return(HTML(html))
#' }

