#' Create application state
#'
#' All analysis modules share this single reactive state object. Keeping its
#' schema in one place makes data flow explicit and easier to test.
#'
#' @noRd
new_app_state <- function() {
  shiny::reactiveValues(
    data = NULL,
    group = NULL,
    filter = NULL,
    filterNormTransform = NULL,
    clean = NULL
  )
}

#' Stable module identifiers
#'
#' @noRd
app_module_ids <- c(
  home = "home",
  upload = "upload",
  preprocess = "preprocess",
  statistics = "statistics",
  report = "report",
  replicate_check = "replicate_check",
  randomizer = "randomizer",
  lc_tool = "lc_tool",
  ms_tool = "ms_tool",
  identification = "identification",
  contact = "contact"
)
