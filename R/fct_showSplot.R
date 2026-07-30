#' @title Show PLSDA score plot
#' @description perform PLSDA and show the score plot
#' @author Yonghui Dong
#' @param DF data matrix used for oplsda
#' @param oplsda oplsda result from r package ropls
#' @param size point size
#' @param interactive should the plot be interactive? default is FALSE
#' @importFrom stats cov sd
#' @importFrom ropls getVipVn
#' @importFrom ggplot2 aes
#' @importFrom dplyr %>%
#' @noRd
#' @examples
#' library(ropls)
#' data(sacurine)
#' genderFc = sacurine$sampleMetadata[, "gender"]
#' oplsda = opls(sacurine$dataMatrix, genderFc, predI = 1, orthoI = NA, fig.pdfC = "none")
#' showSplot(sacurine$dataMatrix, oplsda, interactive = TRUE)

showSplot <- function(DF, oplsda, size = 3, interactive = FALSE) {
  #(1) Suppress no visible binding for global variable notes
  Cov <- Corr <- NULL
  #(2) check input
  if(!inherits(oplsda, "opls")) {stop("Wrong oplsda object detected")}
  #(3) prepare the data
  score <- oplsda@scoreMN[, 1L]
  datasplot <- data.frame(
    Cov = drop(stats::cov(as.matrix(DF), score)),
    Corr = drop(stats::cor(as.matrix(DF), score))
  )
  rownames(datasplot) <- colnames(DF)
  VIP <- getVipVn(oplsda)
  df2 <- cbind.data.frame(datasplot, VIP = VIP)
  df3 <- df2 %>%
    dplyr::mutate(VIP = ifelse(VIP >= 1, "VIP >= 1", "VIP < 1"))
  vip_colors <- c("VIP < 1" = "#B3B3B3", "VIP >= 1" = nature_colors[[1L]])
  df3$VIP <- factor(df3$VIP, levels = names(vip_colors))

  p <- ggplot2::ggplot(df3, aes(x = Cov, y = Corr, color = VIP, text = colnames(DF))) +
    ggplot2::geom_point(size = size) +
    ggplot2::geom_hline(yintercept = 0,linetype = 2) +
    ggplot2::geom_vline(xintercept = 0,linetype = 2) +
    ggplot2::scale_color_manual(values = vip_colors) +
    ggplot2::xlab("Covariance") +
    ggplot2::ylab("Correlation") +
    ggplot2::ggtitle("S-plot") +
    ggplot2::theme_bw()
  if(isTRUE(interactive)){
    df3$feature <- rownames(df3)
    p <- plotly::plot_ly(
      data = df3,
      x = ~Cov,
      y = ~Corr,
      color = ~VIP,
      colors = vip_colors,
      text = ~feature,
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(size = size)
    ) %>%
      plotly::layout(
        title = "S-plot",
        xaxis = list(title = "Covariance"),
        yaxis = list(title = "Correlation")
      )
    }
  return(p)
}
