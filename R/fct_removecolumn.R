#' @title Delete columns
#' @description Delete columns
#' @param df a data frame
#' @param nameofthecolumn names of the columns to be deleted
#' @return a data frame.
#' @noRd
#' @examples
#' df <- copy(mtcars)
#' cols <- c("cyl", "disp", "hp")
#' removecolumn(df, cols)

removecolumn <- function(df, nameofthecolumn){
  missing_columns <- setdiff(nameofthecolumn, names(df))
  if (length(missing_columns)) {
    stop("Unknown column(s): ", paste(missing_columns, collapse = ", "))
  }
  df[setdiff(names(df), nameofthecolumn)]
}
