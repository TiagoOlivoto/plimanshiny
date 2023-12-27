#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4DashPage(
      title = "pliman Shiny",
      skin = NULL,
      freshTheme = NULL,
      preloader = NULL,
      options = NULL,
      fullscreen = TRUE,
      help = FALSE,
      dark = NULL,
      scrollToTop = FALSE,
      header = bs4DashNavbar(
        title = dashboardBrand(
          title = "pliman Shiny",
          color = "white",
          href = "https://mrpackages.netlify.app/",
          image = "www/beans3.png",
          opacity = 0.8
        ),
        status = "white",
        fixed = TRUE,
        "Shiny app for the pliman package",
        rightUi = bs4DropdownMenu(
          type = "messages",
          badgeStatus = "danger",
          href = "http://buymeacoffee.com/mrbean"
          # messageItem(
          #   from = "MrBean",
          #   message = "If you want to contribute...",
          #   time = "today", image = "www/beans3.png",
          #   href = "http://buymeacoffee.com/mrbean"
          # )
        )
      ),
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "success",
        elevation = 3,
        fixed = TRUE,
        bs4SidebarMenu(
          id = "tabs",
          bs4SidebarHeader("Menu"),
          bs4SidebarMenuItem(
            "Home",
            tabName = "home", icon = shiny::icon("home", verify_fa = FALSE)
          ),
          # Import data
          bs4SidebarMenuItem(
            "Mosaic",
            icon = shiny::icon("image"),
            startExpanded = F,
            bs4SidebarMenuItem(
              text = "Input",
              tabName = "mosaicimport",
              icon = shiny::icon("file-upload", verify_fa = FALSE)
            ),
            bs4SidebarMenuItem(
              text = "Shapefile",
              tabName = "shapefileimport",
              icon = shiny::icon("draw-polygon")
            ),
            bs4SidebarMenuItem(
              text = "Index",
              tabName = "mosaicindex",
              icon = shiny::icon("crop")
            )

          )
        )
      ),
      body = bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "mosaicimport",
            mod_mosaic_prepare_ui("mosaic_prepare_1")
          ),
          bs4TabItem(
            tabName = "shapefileimport",
            mod_shapefile_prepare_ui("shapefile_prepare_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "plimanshiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
