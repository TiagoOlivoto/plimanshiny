#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

find_aggrfact <- function(mosaic, max_pixels = 1e6){
  compute_downsample <- function(nr, nc, n) {
    if (n == 0) {
      invisible(nr * nc)
    } else if (n == 1) {
      invisible(ceiling(nr/2) * ceiling(nc/2))
    } else if (n > 1) {
      invisible(ceiling(nr/(n+1)) * ceiling(nc/(n+1)))
    } else {
      stop("Invalid downsampling factor. n must be a non-negative integer.")
    }
  }
  nr <- nrow(mosaic)
  nc <- ncol(mosaic)
  npixel <- nr * nc
  possible_downsamples <- 0:20
  possible_npix <- sapply(possible_downsamples, function(x){
    compute_downsample(nr, nc, x)
  })
  downsample <- which.min(abs(possible_npix - max_pixels))
  downsample <- ifelse(downsample == 1, 0, downsample)
  return(downsample)
}
sf_to_polygon <- function(shps) {
  if(inherits(shps, "list")){
    shps <- do.call(rbind, shps)
  }
  classes <- sapply(lapply(sf::st_geometry(shps$geometry), class), function(x){x[2]})
  shps[classes %in% c("POINT", "LINESTRING"), ] <-
    shps[classes %in% c("POINT", "LINESTRING"), ] |>
    sf::st_buffer(0.0000001) |>
    sf::st_cast("POLYGON") |>
    sf::st_simplify(preserveTopology = TRUE)
  return(shps)
}

roundcols <- function(df, ..., digits = 3){
  is_mat <- is.matrix(df)
  if (is_mat == TRUE) {
    df <- df %>% as.data.frame() %>% pliman::rownames_to_column()
  }
  has_rownames <- function(x) {
    Negate(is.null)(rownames(x))
  }
  if (has_rownames(df)) {
    rnames <- rownames(df)
  }
  if (missing(...)) {
    df <-
      df |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x){round(x, digits = digits)}))
  }
  else {
    df <-
      df |>
      dplyr::mutate(dplyr::across(c(...), \(x){round(x, digits = digits)}))
  }
  if (has_rownames(df)) {
    rownames(df) <- rnames
  }
  return(df)
}
import_shp <- function(shp){
  if(is.null(shp)){
    return()
  }
  previouswd <- getwd()
  uploaddirectory <- dirname(shp$datapath[1])
  setwd(uploaddirectory)
  for(i in 1:nrow(shp)){
    file.rename(shp$datapath[i], shp$name[i])
  }
  setwd(previouswd)
  filetemp <- paste(uploaddirectory, shp$name[grep(pattern="*.shp$", shp$name)], sep="/")
  shapefile_input(filetemp, info = FALSE)
}

import_shp_mod <- function(datapath, file, session){
  files <- datapath
  exts <- c(".rds",  ".shp",  ".json", ".kml",  ".gml",  ".dbf",  ".sbn",  ".sbx",  ".shx",  ".prj", ".cpg")
  if(!any(file_extension(files)  %in% sub(".", "", exts))){
    sendSweetAlert(
      session = session,
      title = "Invalid file format",
      text = paste("Invalid file format while uploading the shapefile. Ensure that the file extension are one of", paste0(exts, collapse = ", ")),
      type = "error"
    )
    return()
  } else{
    reqshp <- c("shp", "dbf", "prj", "shx")
    if(any(file_extension(files)  %in%  reqshp)){
      if (!all(reqshp %in% file_extension(files))) {
        sendSweetAlert(
          session = session,
          title = "Required files",
          text = "When importing a '.shp' file, make sure to also import the
              mandatory files companion *.dbf, *.prj, and *.shx. Select the multiple
              required files and try again.",
          type = "error"
        )
        return()
      } else{
        shp <- import_shp(file)
      }
    } else{
      shp <- shapefile_input(datapath, info = FALSE)
    }
    return(shp)
  }
}

write_shp <- function(data, file){
  temp_shp <- tempdir()
  # write shp files
  terra::writeVector(data, paste0(temp_shp, "/my_shp.shp"), overwrite = TRUE)
  # zip all the shp files
  zip_file <- file.path(temp_shp, "vector_shp.zip")
  shp_files <- list.files(temp_shp,
                          "my_shp",
                          full.names = TRUE)
  # the following zip method works for me in linux but substitute with whatever method working in your OS
  zip_command <- paste("zip -j",
                       zip_file,
                       paste(shp_files, collapse = " "))
  system(zip_command)
  # copy the zip file to the file argument
  file.copy(zip_file, file)
  # remove all the files created
  file.remove(zip_file, shp_files)
}
#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

hl <- function(){
  tags$hr()
}

mosaic_info <- function(mo){
  if(terra::crs(mo) != ""){
    crsmo <- terra::crs(mo, describe = TRUE)
    crsname <- paste0(crsmo$name, " (", paste0(paste0(crsmo$authority), ":", paste0(crsmo$code)), ")")
  } else{
    crsname <- "CRS not available"
  }
  dt <- terra::datatype(mo)[[1]]
  content <- tags$span(
    tags$h1(icon("info"), "Mosaic information", style = "color: steelblue;"),
    icon("border-all"),tags$b("Number of columns: "), paste0(terra::ncol(mo)), tags$br(),
    icon("border-all"),tags$b("Number of rows: "), paste0(terra::nrow(mo)), tags$br(),
    icon("layer-group"),tags$b("Number of layers: "), paste0(terra::nlyr(mo), " (",paste0(names(mo), collapse = ", ") , ")"), tags$br(),
    icon("ruler-combined"),tags$b("Resolution: "), paste0(paste(round(terra::res(mo), 8), collapse = ", "), " (x, y)"), tags$br(),
    icon("ruler-combined"),tags$b("Extend: "), paste0(sub("^ext\\((.*)\\)$", "\\1", paste0(round(terra::ext(mo), 2))), " (xmin, xmax, ymin, ymax)"), tags$br(),
    icon("earth-americas"),tags$b("CRS: "), paste0(crsname), tags$br(),
    icon("database"),tags$b("Data Type: "), paste0(dt), tags$br()
  )

  show_alert(
    title = NULL,
    text = div(content, style = "text-align: left; line-height: 1.5"),
    html = TRUE,
    width = 850
  )
}


# Function to create a new reactiveValues object for a mosaic
create_reactval <- function(name, data) {
  reactiveValues(name = name, data = data)
}

chrv2numv <- function(chr){
  unlist(strsplit(as.character(chr), split = ','))
  as.numeric(gsub("[[:space:]]", "", unlist(strsplit(as.character(chr), split = ','))))
}

overlaps <- function(mosaic, shape){
  !is.null(terra::intersect(terra::ext(shape), terra::ext(mosaic)))
}


extract_number <- function(.data,
                           ...,
                           pattern = NULL){
  if(missing(pattern)){
    pattern <- "[^0-9.-]+"
  }
  if (inherits(.data, c("data.frame","tbl_df", "data.table"))){
    if(missing(...)){
      results <-
        dplyr::mutate(.data, dplyr::across(dplyr::where(~!is.numeric(.)), gsub, pattern = pattern, replacement = "")) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
    } else{
      results <-
        dplyr::mutate(.data, dplyr::across(c(...), gsub, pattern = pattern, replacement = ""))  |>
        dplyr::mutate(dplyr::across(c(...), as.numeric))
    }
    return(results)
  } else{
    return(as.numeric(gsub("[^0-9.-]+", "", .data)))
  }
}

str_split <- function(string){
  gsub("[[:space:]]", "", strsplit(string, split = ',')[[1]])
}

create_palette <- function(img, points, width = 150, height = 100, shape = "box", r = 1){
  nc <- ncol(img)
  colnames(points) <- c("x", "y")
  points[, 2] <- nc - points[, 2]
  bind <- NULL
  for (i in 1:nrow(points)) {
    xrmin <- trunc(points[, 1][i]) - r
    xrmax <- trunc(points[, 1][i]) + r
    yrmin <- trunc(points[, 2][i]) - r
    yrmax <- trunc(points[, 2][i]) + r
    sqr <- xrmax - xrmin + 1
    kern <- as.logical(EBImage::makeBrush(sqr, shape = shape))
    R <- img[xrmin:xrmax, yrmin:yrmax, 1][kern]
    G <- img[xrmin:xrmax, yrmin:yrmax, 2][kern]
    B <- img[xrmin:xrmax, yrmin:yrmax, 3][kern]
    bind <- rbind(bind, cbind(R, G, B))
  }
  dim_mat <- trunc(sqrt(nrow(bind)))
  bind <- bind[sample(1:nrow(bind)), ][1:dim_mat^2, ]
  pal <-
    EBImage::Image(c(bind[, 1], bind[, 2], bind[, 3]),
                   dim = c(dim_mat, dim_mat, 3),
                   colormode = "Color") |>
    image_resize(width = width, height = height)
  return(pal)
}

# gathered from https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
                    border.bg = NA, adj = NULL, pos = NULL, offset = 0.5,
                    padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){

  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }
  }

  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)
    }
  } else {
    adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]

  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  graphics::rect(xleft = xMid - rectWidth/2,
                 ybottom = yMid - rectHeight/2,
                 xright = xMid + rectWidth/2,
                 ytop = yMid + rectHeight/2,
                 col = adjustcolor(col.bg, 0.9),
                 border = adjustcolor(border.bg, 0.9))

  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
                 adj = c(0.5, 0.5))

  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }
}


convert_numeric_cols <- function(data) {
  # Function to check if a column can be converted to numeric
  can_convert_to_numeric <- function(x) {
    is.character(x) && all(!is.na(suppressWarnings(as.numeric(x))))
  }

  # Get the names of the columns that can be converted
  numeric_col_names <- names(data)[sapply(data, can_convert_to_numeric)]

  # Convert only those columns to numeric
  data <- data  |>
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_col_names), as.numeric))

  return(data)
}


add_suffix <- function(name, suffix){
  gsub(".character\\(0\\)", "", paste0(file_name(name), suffix, ".", file_extension(name)))
}

render_reactable <- function(df,
                             filterable = TRUE,
                             searchable = TRUE,
                             striped = TRUE,
                             pagination = TRUE,
                             defaultPageSize = 15,
                             defaultColDef = colDef(
                               maxWidth = 400,
                               footer = function(values) {
                                 if (!is.numeric(values)) return()
                                 sparkline::sparkline(values, type = "box", width = 100, height = 30)
                               }),
                             theme = reactableTheme(
                               cellPadding = "8px 10px",
                               style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                               searchInputStyle = list(width = "100%")
                             ),
                             ...){


  reactable(
    df,
    filterable = filterable,
    searchable = searchable,
    striped = striped,
    pagination = pagination,
    defaultPageSize = defaultPageSize,
    defaultColDef = defaultColDef,
    theme = theme,
    ...
  )
}
color_alpha <- function(color, alpha) {
  # Convert the color to RGB
  rgb_vals <- col2rgb(color) / 255

  # Check that alpha is between 0 and 1
  if (alpha < 0 || alpha > 1) {
    stop("Alpha value must be between 0 and 1")
  }

  # Create the color with the specified alpha
  alpha_color <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha)

  return(alpha_color)
}


date_format <- function(date_vector) {
  possible_formats <- c("%Y%m%d", "%d-%m-%Y", "%m-%d-%Y", "%Y-%m-%d")

  # Helper function to check if a date matches a given format
  matches_format <- function(date_string, format) {
    if (format == "%Y%m%d") {
      return(grepl("^\\d{8}$", date_string))
    }
    if (format == "%d-%m-%Y") {
      if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_string)) {
        parts <- unlist(strsplit(date_string, "-"))
        if (as.numeric(parts[1]) <= 31 && as.numeric(parts[2]) <= 12) {
          return(TRUE)
        }
      }
    }
    if (format == "%m-%d-%Y") {
      if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_string)) {
        parts <- unlist(strsplit(date_string, "-"))
        if (as.numeric(parts[1]) <= 12 && as.numeric(parts[2]) <= 31) {
          return(TRUE)
        }
      }
    }
    if (format == "%Y-%m-%d") {
      return(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_string))
    }
    return(FALSE)
  }

  # Check all dates for each format
  valid_formats <- list()
  for (format in possible_formats) {
    if (all(sapply(date_vector, matches_format, format = format))) {
      valid_formats <- c(valid_formats, format)
    }
  }

  return(valid_formats)
}

check_cols_shpinp <- function(shpimp){
  if(!"unique_id" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(unique_id = dplyr::row_number())
  }
  if(!"block" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(block = "B01")
  }
  if(!"plot_id" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(plot_id = paste0("P", leading_zeros(1:nrow(shpimp), 3)))
  }
  if(!"row" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(row = 1)
  }
  if(!"column" %in% colnames(shpimp)){
    shpimp <- shpimp |> dplyr::mutate(column = 1)
  }
  shpimp |> dplyr::relocate(geometry, .after = dplyr::last_col())
}
# Function to calculate the UTM zone based on longitude
get_utm_zone <- function(lon) {
  return(floor((lon + 180) / 6) + 1)
}

# Function to get the bounds of the UTM zone
get_utm_bounds <- function(zone) {
  lon_min <- (zone - 1) * 6 - 180
  lon_max <- zone * 6 - 180
  return(list(lon_min = lon_min, lon_max = lon_max, lat_min = -80, lat_max = 84))
}
epsg <- function(lat, lon) {
  utm_zone <- floor((lon + 180) / 6) + 1
  hemisphere <- ifelse(lat >= 0, "N", "S")
  epsg_code <- if (hemisphere == "N") {
    32600 + utm_zone
  } else {
    32700 + utm_zone
  }
  return(paste0("EPSG:", epsg_code))
}
