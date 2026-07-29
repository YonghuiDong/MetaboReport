test_that("interactive PCA and volcano plots do not depend on ggplotly", {
  expect_equal(MetaboReport:::nature_palette(3), c("#E64B35", "#4DBBD5", "#00A087"))
  expect_length(MetaboReport:::nature_palette(12), 12)

  set.seed(1)
  data <- matrix(
    stats::rnorm(240),
    nrow = 20,
    dimnames = list(paste0("S", 1:20), paste0("F", 1:12))
  )
  group <- rep(c("Control", "Treatment"), each = 10)

  pca_result <- stats::prcomp(data, center = FALSE, scale. = FALSE)
  pca <- MetaboReport:::showPCA(data, group, pca_result = pca_result, interactive = TRUE)
  expect_s3_class(pca, "plotly")
  expect_gt(length(plotly::plotly_build(pca)$x$data), 0)

  for (frame in c("polygon", "norm")) {
    traces <- plotly::plotly_build(
      MetaboReport:::showPCA(data, group, showFrame = frame, interactive = TRUE)
    )$x$data
    expect_true(any(vapply(traces, function(x) identical(x$fill, "toself"), logical(1))))
    points <- Filter(function(x) identical(x$mode, "markers"), traces)
    expect_true(all(vapply(points, function(x) identical(x$marker$size, 12), logical(1))))
  }

  colored <- plotly::plotly_build(
    MetaboReport:::showPCA(data, group, showFrame = "polygon", palette = "Set1", interactive = TRUE)
  )$x$data
  frame_colors <- vapply(Filter(function(x) identical(x$mode, "lines"), colored), function(x) x$line$color, "")
  point_colors <- vapply(Filter(function(x) identical(x$mode, "markers"), colored), function(x) x$marker$color[[1L]], "")
  default_colors <- vapply(Filter(function(x) identical(x$mode, "markers"), plotly::plotly_build(pca)$x$data), function(x) x$marker$color[[1L]], "")
  expect_equal(frame_colors, point_colors)
  expect_false(identical(point_colors, default_colors))

  statistics <- data.frame(
    Fold_Control_vs_Treatment = c(1, 4, 0.25),
    AdjPvalue_Control_vs_Treatment = c(0.5, 0.001, 0.001),
    row.names = c("none", "up", "down")
  )
  volcano <- MetaboReport:::showVolcano(
    statistics,
    compare_group = c("Control", "Treatment"),
    interactive = TRUE
  )
  expect_s3_class(volcano, "plotly")
  volcano_traces <- plotly::plotly_build(volcano)$x$data
  volcano_shapes <- plotly::plotly_build(volcano)$x$layout$shapes
  volcano_colors <- setNames(
    vapply(volcano_traces, function(x) x$marker$color[[1L]], ""),
    vapply(volcano_traces, `[[`, "", "name")
  )
  expect_equal(
    unname(volcano_colors[c("Not Significant", "Significantly Up", "Significantly Down")]),
    c("rgba(179,179,179,1)", "rgba(230,75,53,1)", "rgba(77,187,213,1)")
  )
  expect_equal(c(volcano_shapes[[1L]]$x0, volcano_shapes[[2L]]$x0), c(-1, 1))
  expect_equal(volcano_shapes[[3L]]$y0, -log10(0.05))

  strict <- plotly::plotly_build(MetaboReport:::showVolcano(
    statistics, c("Control", "Treatment"), FC = 5, pValue = 0.0001, interactive = TRUE
  ))$x$data
  expect_equal(vapply(strict, `[[`, "", "name"), "Not Significant")
})
