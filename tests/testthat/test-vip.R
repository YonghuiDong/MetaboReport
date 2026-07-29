test_that("VIP scores match pairwise OPLS models without permutations", {
  set.seed(43)
  group <- factor(rep(c("A", "B", "C"), each = 8))
  feature <- matrix(rnorm(length(group) * 20), nrow = length(group))
  feature[group == "B", 1:5] <- feature[group == "B", 1:5] + 1.5
  feature[group == "C", 6:10] <- feature[group == "C", 6:10] + 1.5
  colnames(feature) <- paste0("F", seq_len(ncol(feature)))

  result <- MetaboReport:::getVIP(feature, group)
  pairs <- utils::combn(levels(group), 2L, simplify = FALSE)

  expect_named(result, vapply(
    pairs,
    function(pair) paste0("VIP_", paste(pair, collapse = "_vs_")),
    character(1)
  ))
  expect_equal(nrow(result), ncol(feature))

  for (index in seq_along(pairs)) {
    keep <- group %in% pairs[[index]]
    model <- ropls::opls(
      x = feature[keep, , drop = FALSE],
      y = as.character(group[keep]),
      log10L = FALSE,
      scaleC = "none",
      predI = 1,
      permI = 0,
      orthoI = 1,
      crossvalI = min(sum(keep), 7L),
      fig.pdfC = "none",
      info.txtC = "none"
    )
    expect_equal(result[[index]], unname(round(model@vipVn, 2)))
  }
})
