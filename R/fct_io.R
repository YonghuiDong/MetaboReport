#' Read a Shiny file upload
#'
#' @param upload A single value from `shiny::fileInput()`.
#' @param check_names Passed to `utils::read.csv()`.
#' @return A data frame, or `NULL` when no file was supplied.
#' @noRd
read_uploaded_table <- function(upload, check_names = FALSE) {
  if (is.null(upload)) return(NULL)

  extension <- tolower(tools::file_ext(upload$name))
  reader <- switch(
    extension,
    csv = function(path) utils::read.csv(
      path,
      header = TRUE,
      check.names = check_names
    ),
    xls = readxl::read_xls,
    xlsx = readxl::read_xlsx,
    stop("Unsupported file type: .", extension)
  )

  as.data.frame(reader(upload$datapath), check.names = check_names)
}

#' Stop a download when no usable result exists
#'
#' @noRd
has_download_result <- function(value) {
  if (is.null(value) || !length(value)) return(FALSE)
  if (is.data.frame(value) || is.matrix(value)) {
    return(nrow(value) > 0L && ncol(value) > 0L)
  }
  if (is.character(value)) {
    value <- trimws(value)
    return(any(nzchar(value)) && !all(value %in% c("Not Found", "Result Not Available")))
  }
  TRUE
}

#' Validate a result inside a Shiny download handler
#'
#' Reactive validation failures are treated as an unavailable result. Other
#' errors remain visible in the server log so genuine bugs are not hidden.
#'
#' @noRd
require_download_result <- function(value, available = NULL) {
  value <- tryCatch(
    force(value),
    shiny.silent.error = function(error) NULL
  )
  if (is.null(available)) available <- has_download_result(value)
  if (!isTRUE(available)) {
    stop(structure(
      list(message = "Result Not Available", call = NULL),
      class = c("metaboreport_download_unavailable", "error", "condition")
    ))
  }
  value
}

#' Show the unavailable-result dialog
#'
#' @noRd
show_result_not_available <- function() {
  shiny::showModal(shiny::modalDialog(
    shiny::div(
      class = "result-not-available-message",
      shiny::icon("exclamation-circle"),
      shiny::strong("Result Not Available")
    ),
    footer = shiny::modalButton("OK"),
    easyClose = TRUE,
    fade = TRUE
  ))
}

#' Contain download failures within the current Shiny session
#'
#' @noRd
safe_download_content <- function(content) {
  force(content)
  notify_and_stop <- function() {
    show_result_not_available()
    shiny::req(FALSE)
  }

  function(file) {
    tryCatch(
      content(file),
      metaboreport_download_unavailable = function(error) notify_and_stop(),
      shiny.silent.error = function(error) notify_and_stop(),
      error = function(error) {
        if (inherits(error, "shiny.silent.error")) return(notify_and_stop())
        warning("Download failed: ", conditionMessage(error), call. = FALSE)
        notify_and_stop()
      }
    )
  }
}
