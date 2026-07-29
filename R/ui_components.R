# Shared UI components ---------------------------------------------------------

#' Dashboard navigation item
#'
#' @noRd
dashboard_nav_item <- function(label, tab, icon_name, ...) {
  shinydashboard::menuItem(
    text = shiny::strong(label),
    tabName = tab,
    icon = shiny::icon(icon_name),
    ...
  )
}

#' Include the body of a self-contained HTML document
#'
#' `shiny::includeHTML()` warns when given a complete document. This helper
#' keeps the existing self-contained assets while inserting only valid body
#' markup into the dashboard.
#'
#' @noRd
include_html_body <- function(path) {
  document <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  body <- sub("(?is).*<body[^>]*>", "", document, perl = TRUE)
  body <- sub("(?is)</body>.*", "", body, perl = TRUE)
  shiny::HTML(body)
}
