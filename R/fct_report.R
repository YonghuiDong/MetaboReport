#' Required report parameter names
#'
#' @noRd
report_parameter_names <- c(
  "myRawData", "sampleGroup", "dataGlobal3PCA", "OPLSDAGroup",
  "statTable", "VCGroup", "myStat", "myPCAPlot", "myHMPlot",
  "dataGlobal3Transform", "BPGroup", "BPTransform", "KMTrendPlot",
  "KMTable", "projectName", "authorName", "showIntro", "showMethods",
  "mySamplePrep", "mySampleAnalysis", "myDataProcessing"
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
#' @noRd
format_report_methods <- function(
  sample_preparation,
  sample_analysis,
  data_processing
) {
  method_text <- function(x) {
    if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
      return("*Not provided.*")
    }
    x
  }

  paste0(
    "# Materials and Methods\n\n",
    "> <span style = \"color : #FF8300\">**Note:**</span>\n>\n",
    "> (1) If a method is not provided below, ask the person who performed the corresponding experiment or analysis.\n>\n",
    "> (2) Adapt the text to your study and journal requirements before using it in a manuscript.\n\n",
    "## Sample Preparation Method\n\n",
    method_text(sample_preparation), "\n\n",
    "## Sample Analysis Method\n\n",
    method_text(sample_analysis), "\n\n",
    "## Data Processing Method\n\n",
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
  invisible(params)
}

#' Render a self-contained HTML report
#'
#' This pure service owns all filesystem and R Markdown concerns. The Shiny
#' module only gathers reactive values and delegates to this function.
#'
#' @param params Named report parameter list.
#' @param output_file Exact destination path supplied by Shiny.
#' @param template_dir Directory containing `Report.Rmd`, `reportCSS/style.css`,
#'   and `img/logo.png`.
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
  source_files <- c(
    report = file.path(template_dir, "Report.Rmd"),
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
