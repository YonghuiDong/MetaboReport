test_that("report validation identifies missing analysis values", {
  params <- setNames(vector("list", length(MetaboReport:::report_parameter_names)),
                     MetaboReport:::report_parameter_names)
  expect_error(
    MetaboReport:::validate_report_params(params),
    "Complete the corresponding analysis"
  )
})

test_that("custom methods are included in report markdown", {
  methods <- MetaboReport:::format_report_methods(
    sample_preparation = "Preparation text",
    sample_analysis = "Analysis text",
    data_processing = "Processing text"
  )

  expect_match(methods, "Preparation text", fixed = TRUE)
  expect_match(methods, "Analysis text", fixed = TRUE)
  expect_match(methods, "Processing text", fixed = TRUE)
  expect_false(grepl("r mySample", methods, fixed = TRUE))
})

test_that("blank report methods are labeled as not provided", {
  methods <- MetaboReport:::format_report_methods("", " ", NA_character_)
  expect_equal(lengths(regmatches(methods, gregexpr("Not provided", methods))), 3L)
})

test_that("report comparisons exclude QC and handle a single group", {
  expect_equal(
    MetaboReport:::report_group_pairs(c("B", "A", "QC")),
    list(c("A", "B"))
  )
  expect_equal(MetaboReport:::report_group_pairs(c("A", "QC")), list())
})
