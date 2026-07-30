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
  if(!is.numeric(FC) || length(FC) != 1L || !is.finite(FC)){stop("Fold-change threshold must be a finite number.")}
  if(!is.numeric(pValue) || length(pValue) != 1L || !is.finite(pValue)){stop("P-value threshold must be a finite number.")}
  if(pValue <= 0 || pValue > 1){stop("P-value threshold must be greater than 0 and no greater than 1.")}
  if(FC < 1){stop("Fold-change threshold must be at least 1.")}
  if(length(compare_group) != 2L){stop("Exactly two groups must be selected.")}
  ## suppress "no visible binding for global variable" notes in ggplot2
  fold_change <- p_value <- significant <- NULL

  #(2) get index
  F_id <- paste("Fold_", compare_group[1], "_vs_", compare_group[2], sep = "")
  P_id1 <- paste("AdjPvalue_", compare_group[1], "_vs_", compare_group[2], sep = "")
  P_id2 <- paste("AdjPvalue_", compare_group[2], "_vs_", compare_group[1], sep = "")
  if(!F_id %in% colnames(result)){stop("Fold-change results for the selected groups do not exist")}

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
  plot_p <- P_iden
  positive_p <- plot_p[is.finite(plot_p) & plot_p > 0]
  p_floor <- if(length(positive_p)) min(positive_p) / 10 else .Machine$double.xmin
  plot_p[is.finite(plot_p) & plot_p == 0] <- p_floor
  new_iden <- cbind.data.frame(fold_change = log2(F_iden), p_value = -log10(plot_p))
  colnames(new_iden) <- c("fold_change", "p_value") # to make sure colnames changed

  finite_fc <- is.finite(new_iden$fold_change)
  fc_limit <- if(any(finite_fc)) max(abs(new_iden$fold_change[finite_fc])) else 0
  infinite_fc <- is.infinite(new_iden$fold_change)
  new_iden$fold_change[infinite_fc] <- sign(new_iden$fold_change[infinite_fc]) * fc_limit
  new_iden$fold_change[!is.finite(new_iden$fold_change)] <- 0
  new_iden$p_value[!is.finite(new_iden$p_value)] <- 0

  ## add color information (significance)
  new_iden$significant = "Not Significant"
  significant_p <- new_iden$p_value >= -log10(pValue)
  new_iden$significant[significant_p & new_iden$fold_change >= log2(FC)] <- "Significantly Up"
  new_iden$significant[significant_p & new_iden$fold_change <= -log2(FC)] <- "Significantly Down"

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
    new_iden$hover <- paste0(
      new_iden$feature,
      "<br>Fold change: ", signif(F_iden, 4),
      "<br>Adjusted p-value: ", signif(P_iden, 4),
      "<br>", new_iden$significant
    )
    VP <- plotly::plot_ly(
      data = new_iden,
      x = ~fold_change,
      y = ~p_value,
      color = ~significant,
      colors = volcano_colors,
      text = ~hover,
      hoverinfo = "text",
      type = "scatter",
      mode = "markers"
    ) %>%
      plotly::layout(
        title = paste("Volcano plot for sample groups:", compare_group[1], "vs", compare_group[2]),
        xaxis = list(title = "Log2(FC)"),
        yaxis = list(title = "-Log10(p-Value)"),
        shapes = list(
          list(type = "line", x0 = -log2(FC), x1 = -log2(FC), xref = "x", y0 = 0, y1 = 1, yref = "paper", line = list(color = "#777777", dash = "dash")),
          list(type = "line", x0 = log2(FC), x1 = log2(FC), xref = "x", y0 = 0, y1 = 1, yref = "paper", line = list(color = "#777777", dash = "dash")),
          list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = -log10(pValue), y1 = -log10(pValue), yref = "y", line = list(color = "#777777", dash = "dash"))
        )
      ) %>%
      plotly::config(toImageButtonOptions = list(format = "svg", filename = "volcanoPlot"))
  }
  return(VP)
}
