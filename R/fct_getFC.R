#' @title Calculate fold change
#' @description Calculate fold change among different samples.
#' @param x sample ion intensity matrix, row sample, column feature.
#' @param Group sample group information
#' @importFrom utils combn
#' @return a dataframe with mean values and fold changes
#' @noRd
#' @examples
#' dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#' myGroup <- rep_len(LETTERS[1:2], 300)
#' ret <- getFC(dat, Group = myGroup)

getFC <- function(x, Group = NULL){
  cat ("\n- Calculating fold changes...\n")
  #(1) check input
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(levels(Group)) <= 1){stop("At least two sample groups should be included")}
  if(length(Group) != nrow(x)){stop("Missing group informaiton detected")}

  # calculate FC
  i <- split(1:nrow(x), Group)
  mean_int <- sapply(i, function(i){colMeans(x[i, ])})
  x <- t(mean_int)
  j <- combn(levels(Group), 2)
  f_change1 <- x[j[1,],] / x[j[2,],]
  f_change2 <- x[j[2,],] / x[j[1,],]
  ## remove NaN in f_change Matrix
  f_change <- rbind(f_change1, f_change2)
  f_change[is.nan(f_change)] <- 0
  rownames(f_change) <- c(paste0("Fold_", j[1,], "_vs_", j[2,]),
                          paste0("Fold_", j[2,], "_vs_", j[1,]))
  ret <- as.data.frame(t(f_change))
  cat(": FC calculation done!\n")
  return(round(ret, 4))
}
