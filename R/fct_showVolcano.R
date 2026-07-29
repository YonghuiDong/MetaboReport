#' @title View volcano plot
#' @description View volcano plot.
#' @author Yonghui Dong
#' @param result result from doStat() function
#' @param compare_group which groups you want to compare, i.e. c("WT", "Treat1"), only two groups are allowed
#' @param FC select fold change values, default = 2
#' @param pValue select p value, default = 0.05
#' @importFrom ggplot2 aes
#' @noRd
#'@examples
#'dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#'myGroup <- rep_len(LETTERS[1:3], 300)
#'p <- getP(dat, Group = myGroup, Method = "anovaHSD")
#'FC <- getFC(dat, Group = myGroup)
#'result <- cbind.data.frame(p, FC)
#'showVolcano(result, compare_group = c("A", "B"))
#'result2 <- cbind.data.frame(AdjPvalue_A_vs_B = c(0.001, 0.003, 0.002, 0.3), Fold_A_vs_B = c(1000, 50, 10, 30), Fold_B_vs_A = c(0, 1/50, 1/10, 1/30))
#'showVolcano(result2, compare_group = c("A", "B"))
#'showVolcano(result2, compare_group = c("B", "A"))

showVolcano <- function(result, compare_group, FC = 2, pValue = 0.05, interactive = TRUE) {

  #(1)check input
  if(!is.numeric(FC)){stop("invalid calss of fold change: not numeric")}
  if(!is.numeric(pValue)){stop("invalid calss of p value: not numeric")}
  if(pValue > 1){stop("p value should between 0 and 1")}
  if(FC < 0){stop("FC value cannot be negative value ")}
  ## suppress "no visible binding for global variable" notes in ggplot2
  fold_change <- p_value <- significant <- NULL

  #(2) get index
  F_id <- paste("Fold_", compare_group[1], "_vs_", compare_group[2], sep = "")
  P_id1 <- paste("AdjPvalue_", compare_group[1], "_vs_", compare_group[2], sep = "")
  P_id2 <- paste("AdjPvalue_", compare_group[2], "_vs_", compare_group[1], sep = "")

  #(3) plot
  ## select which p value colname exist.
  if ((P_id1 %in% colnames(result)) == TRUE) {
    P_id = P_id1
    } else if ((P_id2 %in% colnames(result)) == TRUE) {
      P_id = P_id2
      } else {stop("The selected groups do not exist")}

  ## prepare data frame for plot
  F_iden <- result[, F_id]
  F_iden[F_iden == 0] <- 0.0001 # replace 0 with 0.0001 (FC round with 4 digits), see bug36
  P_iden <- result[, P_id]
  new_iden <- cbind.data.frame(fold_change = log2(F_iden), p_value = -log10(P_iden))
  colnames(new_iden) <- c("fold_change", "p_value") # to make sure colnames changed

  ## sometimes there are inf, NAN values in FC and p-value, so i replace these values with max(FC) and 0 for FC and p, respectively
  if(length(new_iden[!is.finite(new_iden$fold_change), ]$fold_change) > 0)
    {new_iden[!is.finite(new_iden$fold_change), ]$fold_change <- max(abs(new_iden[is.finite(new_iden$fold_change),]$fold_change))}
  if(length(new_iden[!is.finite(new_iden$p_value), ]$p_value) > 0)
    {new_iden[!is.finite(new_iden$p_value), ]$p_value <- 0}

  ## add color information (significance)
  new_iden$significant = "Not Significant"
  con1 <- new_iden[(new_iden$fold_change >= log2(FC) & new_iden$p_value >= -log10(pValue)),]
  if(dim(con1)[1] > 0) {
    new_iden[(new_iden$fold_change >= log2(FC) & new_iden$p_value >= -log10(pValue)),]$significant = "Significantly Up"
    }

  con2 <- new_iden[(new_iden$fold_change < -log2(FC) & new_iden$p_value >= -log10(pValue)),]
  if(dim(con2)[1] > 0) {
    new_iden[(new_iden$fold_change < -log2(FC) & new_iden$p_value >= -log10(pValue)),]$significant = "Significantly Down"
    }

  volcano_colors <- c(
    "Not Significant" = "#B3B3B3",
    "Significantly Up" = nature_colors[[1L]],
    "Significantly Down" = nature_colors[[2L]]
  )
  new_iden$significant <- factor(new_iden$significant, levels = names(volcano_colors))

  ##plot
  VP <- ggplot2::ggplot(new_iden, aes(x = fold_change, y = p_value, color = significant, text = rownames(result))) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(name = "Type", values = volcano_colors) +
    ggplot2::xlab("Log2(FC)") +
    ggplot2::ylab("-Log10(p-Value)") +
    ggplot2::ggtitle(paste("Volcano plot for sample groups:", compare_group[1], "vs", compare_group[2], sep = " ")) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = c(-log2(FC), log2(FC)), col = "grey", linetype = "longdash") +
    ggplot2::geom_hline(yintercept = -log10(pValue), col = "grey", linetype = "longdash")
  if (isTRUE(interactive)) {
    new_iden$feature <- rownames(result)
    VP <- plotly::plot_ly(
      data = new_iden,
      x = ~fold_change,
      y = ~p_value,
      color = ~significant,
      colors = volcano_colors,
      text = ~feature,
      hoverinfo = "text",
      type = "scatter",
      mode = "markers"
    ) %>%
      plotly::layout(
        title = paste("Volcano plot for sample groups:", compare_group[1], "vs", compare_group[2]),
        xaxis = list(title = "Log2(FC)"),
        yaxis = list(title = "-Log10(p-Value)")
      ) %>%
      plotly::config(toImageButtonOptions = list(format = "svg", filename = "volcanoPlot"))
  }
  return(VP)
}
