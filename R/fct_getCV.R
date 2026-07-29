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
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(levels(Group)) <= 1){stop("At least two sample groups should be included")}
  if(length(Group) != nrow(x)){stop("Missing group informaiton detected")}

  # calculate CV
  i <- split(1:nrow(x), Group)
  mean_int <- sapply(i, function(i){colMeans(x[i, ])})
  colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
  sd_int <- sapply(i, function(i){colSd(x[i, ])})
  cv_int <- sd_int/mean_int * 100
  ret <- round(cv_int, 2)
  colnames(ret) <- paste("CV_", colnames(cv_int), sep = "")
  cat(": CV calculation done!\n")
  return(ret)
}
