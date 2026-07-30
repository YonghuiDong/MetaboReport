#' @title Prepare data for k-means
#' @description repare data for k-means
#' @param DF the same data frame used for heatmap analysis. Row is sample, and column is features.
#' @param Group sample group information.
#' @return a data frame for k-means.
#' @noRd
#' @examples
#' library(dplyr)
#' library(tidyr)
#' heatmapDF <- read.csv("rawData/heatmapDF.csv", header = TRUE)
#' Group <- c(rep("Lung", 18), rep("Blood", 6), rep("QC", 6), rep("Blood", 6))
#' KMData <- prepareKMData(heatmapDF, Group)

prepareKMData <- function(DF, Group){
  #(1) Format data -------------------------------------------------------------
  if(length(Group) != nrow(DF)) stop("Missing group information detected")
  keep <- !is.na(Group) & Group != "QC"
  if(!any(keep)) stop("At least one non-QC sample group is required")
  group <- droplevels(as.factor(Group[keep]))
  values <- as.matrix(DF[keep, , drop = FALSE])
  means <- sweep(rowsum(values, group, reorder = TRUE), 1, as.vector(table(group)), "/")
  tem <- data.frame(Metabolite = colnames(DF), t(means), check.names = FALSE)
  rownames(tem) <- NULL

  #(2) Standardize data --------------------------------------------------------
  tem[-1L] <- tem[-1L] / rowSums(tem[-1L])
  tem
}
