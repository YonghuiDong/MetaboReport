test_that("report validation identifies missing analysis values", {
  params <- setNames(vector("list", length(MetaboReport:::report_parameter_names)),
                     MetaboReport:::report_parameter_names)
  expect_error(
    MetaboReport:::validate_report_params(params),
    "Complete the corresponding analysis"
  )
})

test_that("report progress identifies preparation stages", {
  params <- setNames(
    as.list(rep("ready", length(MetaboReport:::report_parameter_names))),
    MetaboReport:::report_parameter_names
  )
  params$reportLanguage <- "zh"
  steps <- character()
  template_dir <- tempfile()
  dir.create(template_dir)

  expect_error(
    MetaboReport:::render_metabo_report(
      params,
      tempfile(fileext = ".html"),
      template_dir = template_dir,
      progress = function(step, detail) steps <<- c(steps, paste(step, detail))
    ),
    "Report_zh.Rmd"
  )
  expect_equal(steps, c("2 Validate report inputs", "3 Prepare report files"))
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

test_that("Chinese report keeps user-provided methods unchanged", {
  methods <- MetaboReport:::format_report_methods(
    sample_preparation = "Samples were extracted with 80% methanol.",
    sample_analysis = "采用 LC-MS 进行检测。",
    data_processing = "No additional processing.",
    language = "zh"
  )

  expect_false(grepl("# Materials and Methods", methods, fixed = TRUE))
  expect_match(methods, "Samples were extracted with 80% methanol.", fixed = TRUE)
  expect_match(methods, "采用 LC-MS 进行检测。", fixed = TRUE)
  expect_match(methods, "No additional processing.", fixed = TRUE)
})

test_that("English and Chinese report templates contain valid R code", {
  for (template in c("Report.Rmd", "Report_zh.Rmd")) {
    code <- tempfile(fileext = ".R")
    knitr::purl(MetaboReport:::app_sys("app/www", template), output = code, documentation = 0, quiet = TRUE)
    expect_silent(parse(code))
  }
})

test_that("both report templates use the institutional logo", {
  for (template in c("Report.Rmd", "Report_zh.Rmd")) {
    report <- readLines(MetaboReport:::app_sys("app/www", template), warn = FALSE)
    logo_line <- grep('src="FAFU.png"', report, fixed = TRUE)
    expect_length(logo_line, 1L)
    expect_lt(logo_line, grep("^author:", report))
  }
})

test_that("PCA notes render as blockquotes in both report templates", {
  templates <- c("Report.Rmd", "Report_zh.Rmd")
  for(template in templates) {
    lines <- readLines(MetaboReport:::app_sys("app/www", template), warn = FALSE)
    pca <- match("### PCA", lines)
    next_section <- grep("^### ", lines)[grep("^### ", lines) > pca][1]
    section <- paste(lines[pca:(next_section - 1L)], collapse = "\n")

    expect_match(section, "```\\n\\n> <span", fixed = FALSE)
  }

  chinese <- readLines(MetaboReport:::app_sys("app/www/Report_zh.Rmd"), warn = FALSE)
  expect_true(any(grepl('ggplot2::labs\\(color = "Group", fill = "Group"\\)', chinese)))
})

test_that("Chinese report plots retain English labels", {
  chinese <- paste(
    readLines(MetaboReport:::app_sys("app/www/Report_zh.Rmd"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(chinese, 'xaxis = list\\(title = "Group"\\)')
  expect_match(chinese, 'none = "Raw Peak Area"')
  expect_false(grepl('ggplot2::labs\\(title = "S 图"', chinese))
  expect_false(grepl('ggplot2::ylab\\("标准化峰面积"\\)', chinese))
  expect_false(grepl('name = "显著性"', chinese))
})

test_that("report bar tables only respond to plot legend filtering", {
  for (template in c("Report.Rmd", "Report_zh.Rmd")) {
    report <- paste(
      readLines(MetaboReport:::app_sys("app/www", template), warn = FALSE),
      collapse = "\n"
    )

    expect_match(report, 'selection = "none"', fixed = TRUE)
    expect_match(report, 'plotly::highlight(on = "plotly_selected")', fixed = TRUE)
    expect_match(report, "plotly_restyle", fixed = TRUE)
    expect_match(report, "table.column(1).search", fixed = TRUE)
  }
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
