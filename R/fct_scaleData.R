#' @title Feature scaling
#' @description Feature scaling of a data frame
#' @param DF a data frame, row is feature, and column is sample
#' @param Method feature scaling method. "None" is without scaling;
#' @return a row normalized dataframe
#' @noRd
#' @examples
#' dat <- data.frame(S1 = c(1, 3, 4), S2 = c(1, 10, 3), S3 = c(2, 10, 20))
#' ret <- scaleData(dat, Method = "meanCenter")

scaleData <- function(DF, Method = "None") {
  tem <- switch(Method,
                "None" = DF,
                "meanCenter" = t(scale(t(DF), center = TRUE, scale = FALSE)),
                "autoScale" = t(scale(t(DF), center = TRUE, scale = TRUE)),
                "paretoScale" = t(apply(t(DF), 2, function(x) ((x - mean(x))/sqrt(sd(x))))),
                "rangeScale" = t(apply(t(DF), 2, function(x) ((x - mean(x))/(max(x) - min(x))))),
                "vastScale" = t(apply(t(DF), 2, function(x) ((x - mean(x))/sd(x) * mean(x)/sd(x)))),
                "levelScale" = t(apply(t(DF), 2, function(x) ((x - mean(x))/mean(x)))),
                stop("Unknown scaling method")
                )
  if(Method != "None") tem[is.nan(tem) | is.infinite(tem)] <- 0
  return(tem)
  }
