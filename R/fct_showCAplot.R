#' @title Plot correlation network.
#' @description PPlot correlation network.
#' @param PCC pearson correlation coefficient calculated from heatmapDF data frame.
#' @param Metabolite which metabolite?
#' @param Threshold the correlation coefficient threshold.
#' @param LabelSize label size.
#' @return network Plot.
#' @noRd
#' @examples
#' library(visNetwork)
#' library(dplyr)
#' heatmapDF <- read.csv("rawData/heatmapDF.csv", header = TRUE)
#' PCC <- cor(heatmapDF)
#' showCAplot(PCC, Metabolite = "ID3_Creatine", Threshold = 0.85, LabelSize = 20)


showCAplot <- function(PCC, Metabolite, Threshold = 0.9, LabelSize = 20){
  #(1) Prepare network object --------------------------------------------------
  edges <- PCC %>%
    as.data.frame() %>%
    dplyr::select(correlation = all_of(Metabolite)) %>%
    dplyr::filter(abs(correlation) >= Threshold) %>%
    dplyr::mutate(correlation = round(correlation, 3)) %>%
    dplyr::mutate(from = Metabolite, to = rownames(.))

  nodes <- data.frame(id = edges$to) %>%
    dplyr::mutate(label = edges$to) %>%
    dplyr::mutate(color.background = ifelse(edges$correlation > 0, nature_colors[[1L]], nature_colors[[2L]]))

  edges$value <- edges$correlation

  #(2) Plot network ------------------------------------------------------------
  visNetwork::visNetwork(nodes = nodes, edges[edges$from != edges$to, ], width = "100%") %>%
    visNetwork::visNodes(font = list(size = LabelSize)) %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visInteraction(navigationButtons = TRUE) %>%
    visNetwork::visIgraphLayout(randomSeed = 123)
}

