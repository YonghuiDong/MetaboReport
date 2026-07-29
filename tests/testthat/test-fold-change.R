test_that("fold changes preserve pair order and zero-mean behavior", {
  group <- factor(
    rep(c("A", "B", "C"), each = 2),
    levels = c("A", "B", "C", "Unused")
  )
  feature <- cbind(
    regular = rep(c(2, 4, 8), each = 2),
    all_zero = 0,
    partial_zero = rep(c(0, 1, 2), each = 2)
  )

  result <- MetaboReport:::getFC(feature, group)

  expect_named(result, c(
    "Fold_A_vs_B", "Fold_A_vs_C", "Fold_B_vs_C",
    "Fold_B_vs_A", "Fold_C_vs_A", "Fold_C_vs_B"
  ))
  expect_equal(unname(as.numeric(result["regular", ])),
               c(0.5, 0.25, 0.5, 2, 4, 2))
  expect_equal(unname(as.numeric(result["all_zero", ])), rep(0, 6))
  expect_equal(unname(as.numeric(result["partial_zero", ])),
               c(0, 0, 0.5, Inf, Inf, 2))

  one_feature <- MetaboReport:::getFC(feature[, "regular", drop = FALSE], group)
  expect_equal(dim(one_feature), c(1L, 6L))
})
