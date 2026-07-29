#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' @param max_upload_mb Maximum accepted upload size in megabytes.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  max_upload_mb = 10000,
  ...
) {
  options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
