#' Required report parameter names
#'
#' @noRd
report_parameter_names <- c(
  "myRawData", "sampleGroup", "dataGlobal3PCA", "OPLSDAGroup",
  "statTable", "VCGroup", "myStat", "myPCAPlot", "myHMPlot",
  "dataGlobal3Transform", "BPGroup", "BPTransform", "KMTrendPlot",
  "KMTable", "projectName", "authorName", "showIntro", "showMethods",
  "mySamplePrep", "mySampleAnalysis", "myDataProcessing", "reportLanguage"
)

#' Build all pairwise comparisons for non-QC groups
#'
#' @noRd
report_group_pairs <- function(group) {
  groups <- setdiff(levels(as.factor(group)), "QC")
  if (length(groups) < 2L) return(list())
  utils::combn(groups, 2L, simplify = FALSE)
}

#' Format the optional Materials and Methods section
#'
#' User-provided method text is inserted unchanged.
#'
#' @noRd
format_report_methods <- function(
  sample_preparation,
  sample_analysis,
  data_processing,
  language = "en"
) {
  language <- match.arg(language, c("en", "zh"))
  chinese <- identical(language, "zh")
  method_text <- function(x) {
    if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      return(if (chinese) "*未提供。*" else "*Not provided.*")
    }
    x
  }

  headings <- if (chinese) {
    c("材料与方法", "样品制备", "仪器分析", "数据处理")
  } else {
    c("Materials and Methods", "Sample Preparation Method", "Sample Analysis Method", "Data Processing Method")
  }
  note <- if (chinese) {
    paste0(
      "> <span style = \"color : #FF8300\">**注意：**</span>\n>\n",
      "> (1) 若以下方法信息不完整，请向负责相应实验或数据分析的人员核实。\n>\n",
      "> (2) 如用于学术论文，请结合本研究的实际流程，并按照目标期刊要求进一步修改和完善。\n\n"
    )
  } else {
    paste0(
      "> <span style = \"color : #FF8300\">**Note:**</span>\n>\n",
      "> (1) If a method is not provided below, ask the person who performed the corresponding experiment or analysis.\n>\n",
      "> (2) Adapt the text to your study and journal requirements before using it in a manuscript.\n\n"
    )
  }

  paste0(
    "# ", headings[[1L]], "\n\n",
    note,
    "## ", headings[[2L]], "\n\n",
    method_text(sample_preparation), "\n\n",
    "## ", headings[[3L]], "\n\n",
    method_text(sample_analysis), "\n\n",
    "## ", headings[[4L]], "\n\n",
    method_text(data_processing), "\n"
  )
}

#' Validate report parameters
#'
#' @param params Named list passed to the R Markdown template.
#' @noRd
validate_report_params <- function(params) {
  missing <- setdiff(report_parameter_names, names(params))
  if (length(missing)) {
    stop("Missing report parameter(s): ", paste(missing, collapse = ", "))
  }

  empty <- report_parameter_names[vapply(params[report_parameter_names], is.null, logical(1))]
  if (length(empty)) {
    stop(
      "Complete the corresponding analysis before creating the report: ",
      paste(empty, collapse = ", ")
    )
  }
  if (!params$reportLanguage %in% c("en", "zh")) {
    stop("Unsupported report language: ", params$reportLanguage)
  }
  invisible(params)
}

#' Render a self-contained HTML report
#'
#' This pure service owns all filesystem and R Markdown concerns. The Shiny
#' module only gathers reactive values and delegates to this function.
#'
#' @param params Named report parameter list.
#' @param output_file Exact destination path supplied by Shiny.
#' @param template_dir Directory containing both report templates, report CSS,
#'   and the logo image.
#' @param progress Optional function called with the current step and description.
#' @return The normalized output path, invisibly.
#' @noRd
render_metabo_report <- function(
  params,
  output_file,
  template_dir = app_sys("app/www"),
  progress = NULL
) {
  if (!is.null(progress)) progress(2, "Validate report inputs")
  validate_report_params(params)

  if (!is.null(progress)) progress(3, "Prepare report files")
  template <- if (identical(params$reportLanguage, "zh")) "Report_zh.Rmd" else "Report.Rmd"
  source_files <- c(
    report = file.path(template_dir, template),
    css = file.path(template_dir, "reportCSS", "style.css"),
    logo = file.path(template_dir, "img", "logo.png")
  )
  missing_files <- source_files[!file.exists(source_files)]
  if (length(missing_files)) {
    stop("Missing report resource(s): ", paste(basename(missing_files), collapse = ", "))
  }

  work_dir <- tempfile("MetaboReport-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  destination_files <- file.path(work_dir, c("Report.Rmd", "style.css", "logo.png"))
  copied <- file.copy(source_files, destination_files, overwrite = TRUE)
  if (!all(copied)) {
    stop("The report resources could not be prepared.")
  }

  if (!is.null(progress)) progress(4, "Render HTML report")
  rendered <- rmarkdown::render(
    input = destination_files[[1L]],
    output_file = "Report.html",
    output_dir = work_dir,
    params = params,
    envir = new.env(parent = asNamespace("MetaboReport")),
    quiet = TRUE
  )

  if (!is.null(progress)) progress(5, "Finalize download file")
  if (!file.copy(rendered, output_file, overwrite = TRUE)) {
    stop("The generated report could not be copied to the download destination.")
  }

  invisible(normalizePath(output_file, mustWork = TRUE))
}
