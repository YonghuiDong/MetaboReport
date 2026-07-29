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

test_that("Method 2 rejects sample columns without the underscore separator", {
  raw <- data.frame(Name = "A", Sample1 = 1, Sample2 = 2)

  expect_error(
    MetaboReport:::formatData(raw, format = "Other"),
    "No sample intensity columns"
  )
})

test_that("all imputation methods preserve identifiers and remove missing values", {
  data <- data.frame(
    ID = c("F1", "F2", "F3", "F4"),
    S1 = c(1, NA, 3, NA),
    S2 = c(2, 4, NA, NA),
    S3 = c(NA, 5, 6, NA)
  )

  for (method in c("1", "HM", "Mean", "Median", "KNN")) {
    result <- MetaboReport:::fillNA(data, method)
    expect_identical(result$ID, data$ID)
    expect_false(anyNA(result))
    expect_true(all(is.finite(as.matrix(result[-1]))))
  }
})

test_that("imputation is skipped when data are complete", {
  data <- data.frame(ID = c("F1", "F2"), S1 = c(1, 2), S2 = c(3, 4))
  expect_identical(MetaboReport:::fillNA(data, "KNN"), data)
  expect_error(MetaboReport:::fillNA(data, "unknown"), "Unknown missing-value method")
})

test_that("downloads distinguish usable results from empty placeholders", {
  expect_false(MetaboReport:::has_download_result(NULL))
  expect_false(MetaboReport:::has_download_result(data.frame()))
  expect_false(MetaboReport:::has_download_result("Not Found"))
  expect_true(MetaboReport:::has_download_result(data.frame(value = 1)))
})

test_that("download errors are contained inside the Shiny session", {
  shiny::testServer(function(input, output, session) {}, {
    content <- MetaboReport:::safe_download_content(function(file) stop("boom"))
    error <- tryCatch(suppressWarnings(content(tempfile())), error = identity)
    expect_s3_class(error, "shiny.silent.error")
  })
})

test_that("downloads are checked before the browser opens a save dialog", {
  script <- readLines(
    MetaboReport:::app_sys("app/www/download.js"),
    warn = FALSE
  )
  script <- paste(script, collapse = "\n")

  expect_match(script, "event.preventDefault()", fixed = TRUE)
  expect_match(script, "file.size === 0", fixed = TRUE)
})

test_that("column removal validates its input", {
  data <- data.frame(a = 1, b = 2, c = 3)
  expect_identical(names(MetaboReport:::removecolumn(data, c("b", "c"))), "a")
  expect_error(MetaboReport:::removecolumn(data, "missing"), "Unknown column")
})
