#' utmzonesel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_utmzonesel_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_2(
        bs4Card(
          title = "UTM zone selector",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          numericInput(
            ns("latitude"),
            label = "Latitude:",
            value = NA
          ),
          numericInput(
            ns("longitude"),
            label = "Longitude:",
            value = NA
          ),
          actionButton(ns("update_map"), "Update Map"),
          hl(),
          textInput(
            ns("epsg"),
            label = "EPSG code",
            value = ""
          )
        )
      ),
      col_10(
        bs4Card(
          title = "Provide the coordinates or pick a location to get the EPSG code.",
          collapsible = FALSE,
          width = 12,
          height = "760px",
          leafletOutput(ns("map"), height = "680px")
        )
      )
    )
  )
}

#' utmzonesel Server Functions
#'
#' @noRd
mod_utmzonesel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$map <- renderLeaflet({
      mapview::mapview(map.types = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"))@map |>
        setView(lng = 0, lat = 0, zoom = 2)
    })

    observeEvent(input$map_click, {
      click <- input$map_click
      lon <- click$lng
      lat <- click$lat
      updateTextInput(session, "epsg",
                      value = epsg(lat, lon))
      updateNumericInput(session, "latitude",
                         value = lat)
      updateNumericInput(session, "longitude",
                         value = lon)

      utm_zone <- get_utm_zone(lon)
      bounds <- get_utm_bounds(utm_zone)

      leafletProxy("map") |>
        clearShapes() |>
        clearMarkers() |>
        addRectangles(
          lng1 = bounds$lon_min, lat1 = bounds$lat_min,
          lng2 = bounds$lon_max, lat2 = bounds$lat_max,
          fillColor = "salmon",
          color = "salmon",
          fillOpacity = 0.5,
          weight = 2
        ) |>
        addMarkers(lng = lon, lat = lat, popup = paste("UTM Zone:", utm_zone))
    })

    observeEvent(input$update_map, {
      lon <- as.numeric(input$longitude)
      lat <- as.numeric(input$latitude)
      updateTextInput(session, "epsg",
                      value = epsg(lat, lon))


      if (!is.na(lon) & !is.na(lat)) {
        utm_zone <- get_utm_zone(lon)
        bounds <- get_utm_bounds(utm_zone)

        leafletProxy("map") |>
          clearShapes() |>
          clearMarkers() |>
          addRectangles(
            lng1 = bounds$lon_min, lat1 = bounds$lat_min,
            lng2 = bounds$lon_max, lat2 = bounds$lat_max,
            fillColor = "salmon",
            color = "salmon",
            fillOpacity = 0.5,
            weight = 2
          ) |>
          addMarkers(lng = lon, lat = lat, popup = paste("UTM Zone:", utm_zone)) |>
          setView(lng = lon, lat = lat, zoom = 8)
      }
    })
  })
}

## To be copied in the UI
# mod_utmzonesel_ui("utmzonesel_1")

## To be copied in the server
# mod_utmzonesel_server("utmzonesel_1")
