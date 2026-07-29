test_that("vectorized p-values match base R statistical tests", {
  set.seed(45)
  feature_names <- paste0("F", 1:8)

  two_group <- factor(rep(c("Control", "Treatment"), c(6, 7)))
  two_feature <- matrix(rnorm(length(two_group) * 8), nrow = length(two_group),
                        dimnames = list(NULL, feature_names))
  unpaired <- MetaboReport:::tTest(two_feature, two_group)
  expected_unpaired <- vapply(seq_len(ncol(two_feature)), function(index) {
    stats::t.test(
      two_feature[two_group == "Control", index],
      two_feature[two_group == "Treatment", index]
    )$p.value
  }, numeric(1))
  expect_equal(unpaired[[1L]], unname(expected_unpaired), tolerance = 1e-12)

  paired_group <- factor(rep(c("Before", "After"), each = 6))
  paired_feature <- matrix(rnorm(length(paired_group) * 8),
                           nrow = length(paired_group))
  paired <- MetaboReport:::tTest(paired_feature, paired_group, paired = TRUE)
  expected_paired <- vapply(seq_len(ncol(paired_feature)), function(index) {
    stats::t.test(
      paired_feature[paired_group == "After", index],
      paired_feature[paired_group == "Before", index],
      paired = TRUE
    )$p.value
  }, numeric(1))
  expect_equal(paired[[1L]], unname(expected_paired), tolerance = 1e-12)

  constant <- cbind(same = 1, different = rep(c(1, 2), each = 3))
  constant_group <- factor(rep(c("A", "B"), each = 3))
  expect_equal(
    MetaboReport:::tTest(constant, constant_group)[[1L]],
    c(1, 0)
  )

  hsd_group <- factor(rep(c("A", "B", "C"), c(5, 6, 7)))
  hsd_feature <- matrix(rnorm(length(hsd_group) * 8), nrow = length(hsd_group))
  hsd <- MetaboReport:::anovaHSD(hsd_feature, hsd_group)
  expected_hsd <- t(vapply(seq_len(ncol(hsd_feature)), function(index) {
    stats::TukeyHSD(stats::aov(hsd_feature[, index] ~ hsd_group))[[1L]][, 4L]
  }, numeric(3)))
  expect_equal(as.matrix(hsd), expected_hsd, tolerance = 1e-12,
               ignore_attr = TRUE)

  repeated_group <- factor(rep(LETTERS[1:4], each = 5))
  repeated_feature <- matrix(rnorm(length(repeated_group) * 8),
                             nrow = length(repeated_group))
  repeated <- MetaboReport:::anovaRM(repeated_feature, repeated_group)
  pairs <- utils::combn(levels(repeated_group), 2L)
  expected_repeated <- t(vapply(seq_len(ncol(repeated_feature)), function(index) {
    p <- stats::pairwise.t.test(
      repeated_feature[, index],
      repeated_group,
      paired = TRUE,
      p.adjust.method = "bonferroni"
    )$p.value
    vapply(seq_len(ncol(pairs)), function(pair_index) {
      p[pairs[2L, pair_index], pairs[1L, pair_index]]
    }, numeric(1))
  }, numeric(ncol(pairs))))
  expect_equal(ncol(repeated), choose(nlevels(repeated_group), 2L))
  expect_equal(as.matrix(repeated), expected_repeated, tolerance = 1e-12,
               ignore_attr = TRUE)
})
