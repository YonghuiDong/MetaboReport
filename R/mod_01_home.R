#' 01_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        h2(strong(sprintf(
          "Welcome to MetaboReport (v%s)",
          utils::packageVersion("MetaboReport")
        ))),
        include_html_body(app_sys("app/www/landing.html"))
      )

))}

#' 01_home Server Functions
#'
#' @noRd
mod_01_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
