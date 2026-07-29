test_that("feature tables are formatted without mutating the input", {
  raw <- data.frame(
    Name = c("A", "B"),
    `Area: Sample_WT_1.raw` = c(1, 2),
    `Area: Sample_Treatment_2.raw` = c(3, 4),
    check.names = FALSE
  )
  original <- raw

  result <- MetaboReport:::formatData(raw, format = "CD")

  expect_identical(raw, original)
  expect_identical(names(result), c("ID", "WT_1", "Treatment_2"))
  expect_identical(result$ID, c("ID1_A", "ID2_B"))
})

test_that("separate metadata defines stable sample names", {
  raw <- data.frame(
    Name = c("A", "B"),
    Sample_1 = c(1, 2),
    Sample_2 = c(3, 4),
    check.names = FALSE
  )
  metadata <- data.frame(
    Sample = c("S1", "S2"),
    Group = c("Control", "Treatment"),
    Batch = c("A", "A")
  )

  result <- MetaboReport:::formatData(raw, metadata, "Other")

  expect_identical(names(result), c("ID", "Control_A_1", "Treatment_A_2"))
})

test_that("all imputation methods preserve identifiers and remove missing values", {
  data <- data.frame(
    ID = c("F1", "F2", "F3"),
    S1 = c(1, NA, 3),
    S2 = c(2, 4, NA),
    S3 = c(NA, 5, 6)
  )

  for (method in c("1", "HM", "Mean", "Median", "KNN")) {
    result <- MetaboReport:::fillNA(data, method)
    expect_identical(result$ID, data$ID)
    expect_false(anyNA(result))
  }
})

test_that("column removal validates its input", {
  data <- data.frame(a = 1, b = 2, c = 3)
  expect_identical(names(MetaboReport:::removecolumn(data, c("b", "c"))), "a")
  expect_error(MetaboReport:::removecolumn(data, "missing"), "Unknown column")
})
