#' @title Check replicated rows in a dataframe
#' @description check replicated rows in a data frame based on multiple columns and two conditions.
#' @param df a data frame
#' @param col_all columns, the elements should be the same in rows from all of the columns
#' @param col_any columns, at least one element should be the same in rows from any of the columns
#' @return a data frame with an additional column isReplicate to indicate which rows are replicated.
#' @importFrom dplyr %>% n sym
#' @noRd
#' @examples
#' # Example 1
#' df <- data.frame(
#' a = c(1, 1, 2, 3, 4, 1, 1, 3),
#' b = c(1, 2, 2, 3, 4, 1, 1, 3),
#' d = c("x", "y", "z", "x", "x", "y", "x", "x"),
#' e = c("x", "y", "z", "x", "x", "x", "z", "x")
#' )
#' col_all <- c("a", "d")
#' col_any <- c("b", "e")
#' checkReplicates(df, col_all, col_any)
#'
#' # Example 2
#' df2 = data.frame(a = c(123, 123, 178), b = c(468, 444, 444))
#' checkReplicates(df, col_all = "a", col_any = "b")

checkReplicates <- function(df, col_all = NULL, col_any = NULL) {
  #(1) check input
  if(is.null(col_all) && is.null(col_any)) stop("At least one column should be selected")
  selected <- c(col_all, col_any)
  if(any(!selected %in% names(df))) stop("Unknown column selected")
  duplicated_group <- function(x) duplicated(x) | duplicated(x, fromLast = TRUE)

  #(2) check condition 1.
  if(is.null(col_all)){
    all_replicate <- rep(TRUE, nrow(df))
  } else{
    all_replicate <- duplicated_group(df[col_all])
  }

  #(3) check condition 2.
  if(is.null(col_any)){
    any_replicate <- rep(TRUE, nrow(df))
  } else{
    any_replicate <- rep(FALSE, nrow(df))
    eligible <- which(all_replicate)
    for(column in col_any) {
      any_replicate[eligible] <- any_replicate[eligible] |
        duplicated_group(df[[column]][eligible])
    }
  }

  #(4) check both condition 1 & 2.
  df$isReplicate <- all_replicate & any_replicate
  df[c("isReplicate", setdiff(names(df), "isReplicate"))]
}
