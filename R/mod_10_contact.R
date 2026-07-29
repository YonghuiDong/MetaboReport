#' 10_contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_10_contact_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 10, include_html_body(app_sys("app/www/contact.html")))
      )
  )
}

#' 10_contact Server Functions
#'
#' @noRd
mod_10_contact_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
