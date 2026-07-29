#' @title Show PCA score plot
#' @description perform PCA and show the score plot
#' @author Yonghui Dong
#' @param df sample ion intensity matrix, row sample, column feature.
#' @param Group sample group information.
#' @param inx PCA X axis, default is 1: PC1.
#' @param iny PCA Y axis, defult is 2: PC2.
#' @param showFrame what kind of frame should be added? Default is none, options include "none", "norm", and "polygon".
#' @param palette ColorBrewer palette name, or "Default".
#' @param interactive should the plot be interactive? default is FALSE.
#' @importFrom stats prcomp
#' @importFrom grDevices chull
#' @importFrom ggplot2 ensym aes
#' @importFrom dplyr %>%
#' @noRd
#' @examples
#' df <-  iris[1:4]
#' Group <- iris$Species
#' showPCA(df, Group, showFrame = "norm", interactive = T)

showPCA <- function(df, Group, inx = 1, iny = 2, showFrame = "none", palette = "Default", interactive = FALSE) {

  #(1) check input
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(levels(Group)) <= 1){stop("At least two sample groups should be included")}
  if(length(Group) != nrow(df)){stop("Missing group informaiton detected")}

  #(2) perform PCA
  df_pca <- prcomp(df, center = FALSE, scale. = FALSE)
  df_pcs <- data.frame(df_pca$x, Group = Group)
  percentage <-round(df_pca$sdev^2 / sum(df_pca$sdev^2) * 100, 2)
  percentage <-paste(colnames(df_pcs),"(", paste(as.character(percentage), "%", ")", sep = ""))

  #(2) plot
  nms <- names(df_pcs)
  x <- nms[inx]
  y <- nms[iny]

  Sample_Name <- rownames(df_pcs) # rename

  if(showFrame == "none"){
    p1 <- ggplot2::ggplot(df_pcs, aes(x = !!ensym(x), y = !!ensym(y), color = Group, text = Sample_Name)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::xlab(percentage[inx]) +
      ggplot2::ylab(percentage[iny]) +
      ggplot2::theme_bw()
  }

  if(showFrame == "polygon"){
    hull_group <- df_pcs %>%
      dplyr::mutate(Sample_Name = Sample_Name) %>%
      dplyr::group_by(Group) %>%
      dplyr::slice(chull(!!ensym(x), !!ensym(y)))
    p1 <- ggplot2::ggplot(data = hull_group, mapping = aes(text = Sample_Name)) +
      ggplot2::geom_polygon(mapping =  aes(x = !!ensym(x), y = !!ensym(y), fill = Group, group = Group), alpha = 0.2, inherit.aes = FALSE) +
      ggplot2::geom_point(data = df_pcs, mapping = aes(x = !!ensym(x), y = !!ensym(y), color = Group), size = 3) +
      ggplot2::xlab(percentage[inx]) +
      ggplot2::ylab(percentage[iny]) +
      ggplot2::theme_bw()
  }

  if(showFrame == "norm"){
    p1 <- ggplot2::ggplot(df_pcs, aes(text = Sample_Name)) +
      ggplot2::stat_ellipse(aes(x = !!ensym(x), y = !!ensym(y), color = Group, fill = Group), geom = "polygon", level = 0.95, alpha = 0.1, show.legend = F, inherit.aes = FALSE) +
      ggplot2::geom_point(data = df_pcs, mapping = aes(x = !!ensym(x), y = !!ensym(y), color = Group), size = 3) +
      ggplot2::xlab(percentage[inx]) +
      ggplot2::ylab(percentage[iny]) +
      ggplot2::theme_bw()
  }

  colors <- nature_palette(nlevels(Group))
  if (palette == "Default") {
    p1 <- p1 +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors)
  } else {
    p1 <- p1 +
      ggplot2::scale_color_brewer(palette = palette) +
      ggplot2::scale_fill_brewer(palette = palette)
    colors <- ggplot2::scale_color_brewer(palette = palette)$palette(nlevels(Group))
  }

  if(interactive){
    # Build the widget natively. plotly 4.10.x cannot convert ggplot2 4.x
    # objects with ggplotly() ("subscript out of bounds").
    plotData <- df_pcs
    plotData$Sample_Name <- Sample_Name
    frameData <- if (showFrame == "none") NULL else ggplot2::ggplot_build(p1)$data[[1L]]

    p1 <- plotly::plot_ly()
    if (!is.null(frameData)) {
      frameData$Group <- factor(levels(Group)[frameData$group], levels = levels(Group))
      p1 <- plotly::add_trace(
        p1,
        data = frameData,
        x = ~x,
        y = ~y,
        split = ~Group,
        color = ~Group,
        colors = colors,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        opacity = 0.2,
        hoverinfo = "skip",
        showlegend = FALSE
      )
    }

    p1 <- plotly::add_markers(
      p1,
      data = plotData,
      x = plotData[[x]],
      y = plotData[[y]],
      color = ~Group,
      colors = colors,
      text = ~Sample_Name,
      hoverinfo = "text",
      marker = list(size = 12)
    ) %>%
      plotly::layout(
        xaxis = list(title = percentage[inx]),
        yaxis = list(title = percentage[iny])
      ) %>%
      plotly::config(toImageButtonOptions = list(format = "svg", filename = "PCA"))
  }

  return(p1)
}
