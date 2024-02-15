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

