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
