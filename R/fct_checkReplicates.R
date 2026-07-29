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
  if(is.null(col_all) & is.null(col_any))stop("At least one column should be selected")

  #(2) check condition 1.
  if(is.null(col_all)){
    df$allReplicate = TRUE
  } else{
    df <- df %>%
      dplyr::group_by(across(col_all)) %>%
      dplyr::mutate(allReplicate = n() > 1) %>%
      dplyr::ungroup()
    }

  #(3) check condition 2.
  if(is.null(col_any)){
    Final <- cbind.data.frame(df, anyReplicate = TRUE)
  } else{
    n <- length(col_any)
    m <- dim(df)[1]
    tem <- data.frame(matrix(ncol = n, nrow = m))
    for(i in 1:n)
      tem[, i] <- df %>%
      dplyr::group_by(allReplicate, !!sym(col_any[i])) %>% # here need to add allReplicate to make sure that no mismatch. see Example2.
      dplyr::mutate(x = n() > 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(x)
    tem2 <- tem %>%
      dplyr::mutate(anyReplicate = ifelse(rowSums(.) > 0, TRUE, FALSE)) %>%
      dplyr::select(anyReplicate)
    Final <- cbind.data.frame(df, tem2)
  }

  #(4) check both condition 1 & 2.
  Final <- Final %>%
    dplyr::rowwise() %>%
    dplyr::mutate(isReplicate = ifelse(isTRUE(allReplicate) & isTRUE(anyReplicate), TRUE, FALSE)) %>%
    dplyr::select(-c(allReplicate, anyReplicate)) %>%
    dplyr::relocate(isReplicate)
  return(Final)
}
