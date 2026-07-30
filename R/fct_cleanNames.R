#' @title Cleans strings
#' @description Trim leading and tailing white spaces and replace middle white spaces with underscore
#' @param str strings
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' str <- c("a and b", "  c and d    ", "ID_(o)")
#' str2 <- cleanNames(str)

cleanNames <- function(str) {
  gsub_trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  gsub_trim2 <- function(x) gsub("^\\_+|\\_+$", "", x)
  tem <- gsub_trim(str) # trim leading and tailing white spaces
  tem <- make.names(tem) # make syntactically valid names
  tem <- gsub_trim2(tem) # remove mostly tailing "underscores"
  return(tem)
}
