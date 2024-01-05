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
      preloader = list(html = spin_google(), color = "#228B227F"),
      title = "pliman Shiny",
      options = NULL,
      fullscreen = TRUE,
      help = TRUE,
      scrollToTop = TRUE,
      footer =  dashboardFooter(
        left = a(
          href = "https://olivoto.netlify.app/",
          target = "_blank", "@Olivoto"
        ),
        right = "2023"
      ),
      controlbar = dashboardControlbar(
        skinSelector()
      ),
      header = bs4DashNavbar(
        title = dashboardBrand(
          title = "pliman Shiny",
          color = "white",
          image = "www/pdepliman.png",
          opacity = 0.8
        ),
        status = "white",
        fixed = TRUE,
        "A Shiny app for {pliman} package",
        rightUi = bs4DropdownMenu(
          type = "messages",
          badgeStatus = "danger",
          messageItem(
            from = "Olivoto",
            message = "If you want to contact me",
            time = "today",
            icon = shiny::icon("envelope"),
            href = "https://olivoto.netlify.app/"
          ),
          messageItem(
            from = "Olivoto",
            message = "If you want to contact me",
            time = "today",
            icon = shiny::icon("envelope"),
            href = "https://olivoto.netlify.app/"
          )
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
            "Analyze orthomosaics",
            icon = shiny::icon("mountain-sun"),
            startExpanded = F,
            bs4SidebarMenuItem(
              text = "Mosaic",
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
            ),
            bs4SidebarMenuItem(
              text = "Analyze",
              tabName = "mosaicanalyze",
              icon = shiny::icon("chart-line")
            )

          )
        )
      ),
      body = bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "home",
            mod_home_ui("home_1")
          ),
          bs4TabItem(
            tabName = "mosaicimport",
            mod_mosaic_prepare_ui("mosaic_prepare_1")
          ),
          bs4TabItem(
            tabName = "shapefileimport",
            mod_shapefile_prepare_ui("shapefile_prepare_1")
          ),
          bs4TabItem(
            tabName = "mosaicindex",
            mod_indexes_ui("indexes_1")
          ),
          bs4TabItem(
            tabName = "mosaicanalyze",
            mod_analyze_ui("analyze_1")
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
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "plimanshiny"
    ),
    rintrojs::introjsUI(),
    waiter::use_waiter()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
