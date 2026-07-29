#' Application user interface
#'
#' @param request Internal Shiny request object.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  ids <- app_module_ids

  tagList(
    golem_add_external_resources(),
    dashboardPage(
      header = dashboardHeader(title = strong("MetaboReport")),
      sidebar = dashboardSidebar(
        sidebarMenu(
          id = "sidebarmenu",
          style = "position:fixed;overflow:visible;",
          dashboard_nav_item("Home", "home", "home"),
          hr(),
          dashboard_nav_item("Upload Data", "uploadData", "upload"),
          hr(),
          dashboard_nav_item("Preprocess Data", "preprocessData", "adjust"),
          hr(),
          dashboard_nav_item("Statistics", "viewResult", "chart-bar"),
          hr(),
          dashboard_nav_item("Download Report", "downloadReport", "download"),
          hr(),
          dashboard_nav_item(
            "Widgets", "widgets", "tools",
            menuSubItem("Duplicate Check", tabName = "repCheck", icon = icon("bolt")),
            menuSubItem("Randomizer", tabName = "randomizer", icon = icon("bolt")),
            menuSubItem("LC Tool", tabName = "lcTool", icon = icon("bolt")),
            menuSubItem("MS Tool", tabName = "msTool", icon = icon("bolt")),
            menuSubItem("Identification", tabName = "idTool", icon = icon("bolt"))
          ),
          hr(),
          dashboard_nav_item("Contact", "contact", "envelope")
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem("home", mod_01_home_ui(ids[["home"]])),
          tabItem("uploadData", mod_02_uploadData_ui(ids[["upload"]])),
          tabItem("preprocessData", mod_03_preprocess_ui(ids[["preprocess"]])),
          tabItem("viewResult", mod_04_viewResult_ui(ids[["statistics"]])),
          tabItem("downloadReport", mod_05_downloadReport_ui(ids[["report"]])),
          tabItem("repCheck", mod_06_repCheck_ui(ids[["replicate_check"]])),
          tabItem("randomizer", mod_07_randomizer_ui(ids[["randomizer"]])),
          tabItem("lcTool", mod_08_lcTool_ui(ids[["lc_tool"]])),
          tabItem("msTool", mod_09_msTool_ui(ids[["ms_tool"]])),
          tabItem("idTool", mod_11_idTool_ui(ids[["identification"]])),
          tabItem("contact", mod_10_contact_ui(ids[["contact"]]))
        )
      )
    )
  )
}

#' Add packaged resources to the application
#'
#' @importFrom golem add_resource_path favicon
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))

  tags$head(
    golem::favicon(),
    tags$title("MetaboReport"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = sprintf(
        "www/custom.css?v=%s",
        utils::packageVersion("MetaboReport")
      )
    ),
    tags$script(
      src = sprintf(
        "www/download.js?v=%s",
        utils::packageVersion("MetaboReport")
      )
    )
  )
}
