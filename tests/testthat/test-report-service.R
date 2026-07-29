test_that("report validation identifies missing analysis values", {
  params <- setNames(vector("list", length(MetaboReport:::report_parameter_names)),
                     MetaboReport:::report_parameter_names)
  expect_error(
    MetaboReport:::validate_report_params(params),
    "Complete the corresponding analysis"
  )
})
