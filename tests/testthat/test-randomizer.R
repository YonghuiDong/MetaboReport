test_that("instrument sequence formatting is deterministic", {
  randomized <- data.frame(
    Group = c("A", "B"),
    Sample = c("A_1", "B_1"),
    Position = c("A:A1", "A:A2")
  )

  orbitrap <- MetaboReport:::format_injection_sequence(randomized, "Orbitrap")
  waters <- MetaboReport:::format_injection_sequence(randomized, "Waters")

  expect_equal(nrow(orbitrap), 2)
  expect_identical(orbitrap$`File Name`, randomized$Sample)
  expect_identical(orbitrap$Position, randomized$Position)
  expect_identical(waters$Bottle, randomized$Position)
})

test_that("a QC frequency of one does not duplicate samples", {
  set.seed(1)
  design <- data.frame(Group = c("A", "A", "B", "B"), Rep = c(1, 2, 1, 2))
  result <- MetaboReport:::randomizeSeq(design, nQC = 1, plateRow = 8, plateCol = 12)
  samples <- result[!result$Group %in% c("QC", "Blank"), ]

  expect_equal(nrow(samples), nrow(design))
  expect_false(anyDuplicated(samples$Sample) > 0)
})
