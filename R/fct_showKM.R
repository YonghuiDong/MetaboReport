#' @title Plot K-means trend plot
#' @description Plot K-means trend plot
#' @param KMTable data frame
#' @return plot
#' @noRd
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' heatmapDF <- read.csv("rawData/heatmapDF.csv", header = TRUE)
#' Group <- c(rep("Lung", 18), rep("Blood", 6), rep("QC", 6), rep("Blood", 6))
#' KMdata <- prepareKMData(heatmapDF, Group)
#' KMResultCluster <- kmeans(dplyr::select(KMdata, -Metabolite), centers = 2)$cluster
#' KMTable <- KMdata %>% dplyr::mutate(clust = paste0("cluster", KMResultCluster))
#' showKM(KMTable)

showKM <- function(KMTable){
  p <- KMTable %>%
    tidyr::pivot_longer(cols = !c(Metabolite, clust), names_to = "Group", values_to = "normArea") %>%
    dplyr::group_by(Group) %>%
    dplyr::mutate(row_num =  1:n()) %>%
    ggplot2::ggplot(aes(x =  Group , y = normArea , group = row_num)) +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::geom_line(alpha = 0.5 , aes(col = as.character(clust))) +
    ggplot2::scale_color_manual(values = nature_palette(length(unique(KMTable$clust)))) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 14),
                   legend.position = "none",
                   axis.text.x = element_text(angle = 90 , vjust = 0.4)) +
    ggplot2::ylab("Standardized Peak Area") +
    ggplot2::facet_wrap(~clust)
  return(p)
}
