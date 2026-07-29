#' @title Get meta data
#' @description Get metadata from processed data.
#' @param DF the processed data frame resulted from formatData function.
#' @return a data frame, which contains meta data information
#' @noRd
getMeta <- function(DF) {
  rowNameDF <- colnames(DF)
  rowNameDF <- rowNameDF[!rowNameDF %in% "ID"]
  metaData <- read.table(text = rowNameDF, sep = "_", colClasses = "character")
  colnames(metaData) <- sub("V", "Group", colnames(metaData))
  metaData <- subset(metaData, select = -ncol(metaData))
  rownames(metaData) <- rowNameDF
  return(metaData)
}
