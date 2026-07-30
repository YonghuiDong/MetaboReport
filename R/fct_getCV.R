#' @title Calculate coefficient of variation (CV)
#' @description Calculate coefficient of variation (CV) among different sample groups
#' @param x sample ion intensity matrix, row sample, column feature.
#' @param Group sample group information
#' @importFrom stats sd
#' @return a dataframe with mean values and cv
#' @noRd
#' @examples
#' dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#' myGroup <- rep_len(LETTERS[1:2], 300)
#' ret <- getCV(dat, Group = myGroup)

getCV <- function(x, Group = NULL){
  cat ("\n- Calculating coefficient of variation (CV)...\n")
  #(1) check input
  if(is.null(Group)){stop("Please include group information")}
  Group <- as.factor(Group)
  if(length(levels(Group)) <= 1){stop("At least two sample groups should be included")}
  if(length(Group) != nrow(x)){stop("Missing group information detected")}

  # calculate CV
  x <- as.matrix(x)
  groups <- split(seq_len(nrow(x)), Group, drop = TRUE)
  cv_int <- vapply(groups, function(rows) {
    values <- x[rows, , drop = FALSE]
    means <- colMeans(values)
    sds <- if(length(rows) > 1L) {
      sqrt(colSums(sweep(values, 2, means, "-")^2) / (length(rows) - 1L))
    } else {
      rep(NA_real_, ncol(values))
    }
    sds / means * 100
  }, numeric(ncol(x)))
  if(is.null(dim(cv_int))) {
    cv_int <- matrix(cv_int, nrow = ncol(x),
                     dimnames = list(colnames(x), names(groups)))
  }
  ret <- round(cv_int, 2)
  colnames(ret) <- paste("CV_", colnames(cv_int), sep = "")
  cat(": CV calculation done!\n")
  return(ret)
}
