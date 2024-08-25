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
        right = "2024"
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
        "A Shiny app for the {pliman} package",
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
        width = "285px",
        bs4SidebarMenu(
          id = "tabs",
          bs4SidebarHeader("Menu"),
          bs4SidebarMenuItem(
            "Home",
            tabName = "home", icon = shiny::icon("home", verify_fa = FALSE)
          ),
          # Dataset
          bs4SidebarMenuItem(
            text = "Datasets",
            startExpanded = F,
            icon = shiny::icon("database", verify_fa = FALSE),
            bs4SidebarMenuSubItem(
              text = "Input",
              tabName = "datasets",
              icon = shiny::icon("file-import", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Filter",
              tabName = "filter",
              icon = shiny::icon("filter", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Edit",
              tabName = "edit",
              icon = shiny::icon("pencil", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Update",
              tabName = "update",
              icon = shiny::icon("rotate-right", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Join",
              tabName = "join",
              icon = shiny::icon("code-merge", verify_fa = FALSE)
            )
          ),
          bs4SidebarMenuItem(
            text = "Raster file(s)",
            tabName = "mosaicimport",
            icon = shiny::icon("file-upload", verify_fa = FALSE)
          ),
          bs4SidebarMenuItem(
            text = "Shapefile",
            tabName = "shapefileimport",
            icon = shiny::icon("draw-polygon")
          ),
          bs4SidebarMenuItem(
            text = "Manipulate",
            tabName = "mosaicmanipula",
            icon = shiny::icon("screwdriver-wrench", verify_fa = FALSE)
          ),
          bs4SidebarMenuItem(
            "Single orthomosaic",
            icon = shiny::icon("camera"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Index",
              tabName = "mosaicindex",
              icon = shiny::icon("crop")
            ),
            bs4SidebarMenuSubItem(
              text = "Analyze",
              tabName = "mosaicanalyze",
              icon = shiny::icon("chart-simple")
            )
          ),
          # time series
          bs4SidebarMenuItem(
            "Time series",
            icon = shiny::icon("chart-line"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Mosaics and shapefile",
              tabName = "mosaicshape",
              icon = shiny::icon("file-upload", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Analyze",
              tabName = "analyzeseries",
              icon = shiny::icon("chart-line")
            )
          ),
          ### to be included ###

          bs4SidebarMenuItem(
            "Drone traits",
            icon = shiny::icon("chart-line"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Plant Maturity",
              tabName = "matanalyzemod",
              icon = shiny::icon("calendar-check")
            ),
            bs4SidebarMenuSubItem(
              text = "Plant Measures",
              tabName = "phanalyze",
              icon = shiny::icon("ruler-combined")
            ),
            bs4SidebarMenuSubItem(
              text = "Crop Surface",
              tabName = "coveranalyze",
              icon = shiny::icon("seedling")
            ),
            bs4SidebarMenuSubItem(
              text = "Plot uniformity",
              tabName = "plotunifanalyze",
              icon = shiny::icon("layer-group")
            ),
            bs4SidebarMenuSubItem(
              text = "Plant Count",
              tabName = "plcountanalyze",
              icon = shiny::icon("leaf")
            )
          ),
          # Geostatistics
          bs4SidebarMenuItem(
            "Geostatistics",
            icon = shiny::icon("globe"),
            startExpanded = FALSE,
            bs4SidebarMenuSubItem(
              text = "Spatial interpolation",
              tabName = "spatinterpolate",
              icon = shiny::icon("chart-line")
            ),
            bs4SidebarMenuSubItem(
              text = "Spatial autocorrelation",
              tabName = "spatautocor",
              icon = shiny::icon("chart-line")
            )
          ),
          # Phytopathometry
          bs4SidebarMenuItem(
            "Phytopathometry",
            icon = shiny::icon("leaf"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Image",
              tabName = "imageimportphyt",
              icon = shiny::icon("image", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Color palettes",
              tabName = "createcolorpal",
              icon = shiny::icon("palette", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Severity (index)",
              tabName = "measurediseaseind",
              icon = shiny::icon("ruler-combined", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Severity (interactive)",
              tabName = "measurediseaseint",
              icon = shiny::icon("computer-mouse", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Severity (color palettes)",
              tabName = "measurediseasepal",
              icon = shiny::icon("palette", verify_fa = FALSE)
            )

          ),
          bs4SidebarMenuItem(
            "Image analysis",
            icon = shiny::icon("images"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "Image",
              tabName = "imageimport",
              icon = shiny::icon("image", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Index/Mask/Segment",
              tabName = "imagebinary",
              icon = shiny::icon("images", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Comparison slider",
              tabName = "slider",
              icon = shiny::icon("sliders", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Color Palette",
              tabName = "imagepalette",
              icon = shiny::icon("palette", verify_fa = FALSE)
            ),
            bs4SidebarMenuSubItem(
              text = "Analyze",
              tabName = "imageanal",
              icon = shiny::icon("magnifying-glass-chart", verify_fa = FALSE)
            )

          ),
          bs4SidebarMenuItem(
            "Toolbox",
            icon = shiny::icon("toolbox"),
            startExpanded = F,
            bs4SidebarMenuSubItem(
              text = "UTM zone selector",
              tabName = "utmzonesel",
              icon = shiny::icon("earth-americas", verify_fa = FALSE)
            )
          )
          ####

        )
      ),
      body = bs4DashBody(
        bs4TabItems(
          # Single mosaic
          bs4TabItem(
            tabName = "home",
            mod_home_ui("home_1")
          ),
          # Datasets
          bs4TabItem(
            tabName = "datasets",
            mod_datasets_ui("datasets_1")
          ),
          bs4TabItem(
            tabName = "filter",
            mod_dffilter_ui("dffilter_1")
          ),
          bs4TabItem(
            tabName = "edit",
            mod_dfedit_ui("dfedit_1")
          ),
          bs4TabItem(
            tabName = "update",
            mod_dfupdate_ui("dfupdate_1")
          ),
          bs4TabItem(
            tabName = "join",
            mod_dfjoin_ui("dfjoin_1")
          ),
          # Mosaics
          bs4TabItem(
            tabName = "mosaicimport",
            mod_mosaic_prepare_ui("mosaic_prepare_1")
          ),
          bs4TabItem(
            tabName = "mosaicmanipula",
            mod_manipula_ui("manipula_1")
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
          ),

          # Time series
          bs4TabItem(
            tabName = "mosaicshape",
            mod_timeseriesinput_ui("timeseriesinput_1")
          ),
          bs4TabItem(
            tabName = "analyzeseries",
            mod_timeseriesanalysis_ui("timeseriesanalysis_1")
          ),
          # Drone traits
          bs4TabItem(
            tabName = "matanalyzemod",
            mod_matanalyze_ui("matanalyze_1")
          ),
          bs4TabItem(
            tabName = "phanalyze",
            mod_phanalyze_ui("phanalyze_1")
          ),
          # Geostatistics
          bs4TabItem(
            tabName = "spatinterpolate",
            mod_spatinterp_ui("spatinterp_1")
          ),

          # Phytopathometry
          bs4TabItem(
            tabName = "imageimportphyt",
            mod_imageimport_ui("imageimport_2")
          ),
          bs4TabItem(
            tabName = "createcolorpal",
            mod_colorpalette_ui("colorpalette_1")
          ),
          bs4TabItem(
            tabName = "measurediseaseind",
            mod_measurediseaseind_ui("measurediseaseind_1")
          ),
          bs4TabItem(
            tabName = "measurediseasepal",
            mod_measurediseasepal_ui("measurediseasepal_1")

          ),
          bs4TabItem(
            tabName = "measurediseaseint",
            mod_measurediseasepick_ui("measurediseasepick_1")

          ),

          # Image analysis
          bs4TabItem(
            tabName = "imageimport",
            mod_imageimport_ui("imageimport_1")
          ),
          bs4TabItem(
            tabName = "imageanal",
            mod_imageanal_ui("imageanal_1")
          ),
          bs4TabItem(
            tabName = "imagebinary",
            mod_imagesegment_ui("imagesegment_1")
          ),
          bs4TabItem(
            tabName = "imagepalette",
            mod_imagepalette_ui("imagepalette_1")
          ),
          bs4TabItem(
            tabName = "slider",
            mod_slider_ui("slider_1")
          ),

          # Tools
          bs4TabItem(
            tabName = "utmzonesel",
            mod_utmzonesel_ui("utmzonesel_1")
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
