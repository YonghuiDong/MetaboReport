#' 05_downloadReport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_05_downloadReport_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(0) User Guide ==========================================================
      column(width = 12,
             box(width = 12,
                 title = strong("User Guide"),
                 status = "warning",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 p("1. Generate and download a self-contained HTML report from this panel."),
                 p("2. Report generation may take a few minutes. Click Generate Report once, then choose an English or Chinese report."),
                 p("3. If generation fails, restart the app session and try again.")
                 )
             ),

      #(1) Materials and Methods ===============================================
      column(width = 5,
             box(width = 12,
                 inputId = "input_card",
                 title = strong("Report Information"),
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 collapsed = FALSE,
                 closable = FALSE,
                 textInput(inputId = ns("projectName"),
                           label = "Project Name",
                           value = "",
                           placeholder = "(Optional) Name of the project"
                           ),
                 textInput(inputId = ns("authorName"),
                           label = "Author Name",
                           value = "Metabolic Profiling Unit, Weizmann Institute of Science",
                           placeholder = "(Optional) Contact name"
                           )
                 ),
             box(width = 12,
                 inputId = "input_card",
                 title = strong("Materials and Methods"),
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 collapsed = FALSE,
                 closable = FALSE,
                 radioButtons(inputId = ns("showIntro"),
                              label = "1. Include the introduction in the report?",
                              choices = list("Yes" = TRUE, "No" = FALSE),
                              selected = TRUE
                              ),
                 radioButtons(inputId = ns("showMethods"),
                              label = "2. Include Materials and Methods in the report?",
                              choices = list("Yes" = TRUE, "No" = FALSE),
                              selected = FALSE
                              ),
                 textAreaInput(inputId = ns("samplePrep"),
                               label = "2.1 Sample-preparation method",
                               value = "",
                               rows = 5,
                               placeholder = "Enter or paste the sample-preparation method."
                               ),
                 textAreaInput(inputId = ns("sampleAnalysis"),
                               label = "2.2 Sample-analysis method",
                               value = "",
                               rows = 5,
                               placeholder = "Enter or paste the sample-analysis method."
                               ),
                 textAreaInput(inputId = ns("dataProcessing"),
                               label = "2.3 Data-processing method",
                               value = "",
                               rows = 5,
                               placeholder = "Enter or paste the data-processing method."
                               ),
                 actionButton(inputId = ns("getReport"),
                              label = "Generate Report",
                              icon = icon("paper-plane"),
                              style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                              )
                 )
             ),

      #3. Download Report Panel ================================================
      column(width = 7,
             box(width = 12,
                 inputId = "report_card",
                 title = strong("Report / 报告"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note / 注意：")),
                 p(style = "color:#b2182b;", "Generating a report may take a few minutes. Click one download button only once. / 生成报告可能需要几分钟，请勿重复点击下载按钮。"),
                 uiOutput(ns("report_button"))
                 )
             )
))}


#' 05_downloadReport Server Functions
#'
#' @noRd
mod_05_downloadReport_server <- function(id, sfData, inputData, resultList) {
  moduleServer(id, function(input, output, session) {
    report_params <- reactive({
      list(
        myRawData = inputData(),
        sampleGroup = sfData$group,
        dataGlobal3PCA = resultList$dataGlobal3PCA(),
        OPLSDAGroup = resultList$OPLSDAGroup(),
        statTable = resultList$statTable(),
        VCGroup = resultList$VCGroup(),
        myStat = resultList$combinedTable(),
        myPCAPlot = resultList$PCAPlot(),
        myHMPlot = resultList$HMPlot(),
        dataGlobal3Transform = resultList$dataGlobal3Transform(),
        BPGroup = resultList$BPGroup(),
        BPTransform = resultList$BPTransform(),
        KMTrendPlot = resultList$KMTrendPlot(),
        KMTable = resultList$KMTable(),
        projectName = input$projectName,
        authorName = input$authorName,
        showIntro = input$showIntro,
        showMethods = input$showMethods,
        mySamplePrep = input$samplePrep,
        mySampleAnalysis = input$sampleAnalysis,
        myDataProcessing = input$dataProcessing
      )
    })

    output$report_button <- renderUI({
      req(input$getReport > 0)
      tagList(
        downloadButton(
          outputId = session$ns("report_en"),
          label = "Download English Report",
          style = "color:#fff;background-color:#7570b3;border-color:#7570b3"
        ),
        tags$span(style = "display:inline-block;width:10px;"),
        downloadButton(
          outputId = session$ns("report_zh"),
          label = "下载中文报告",
          style = "color:#fff;background-color:#00897b;border-color:#00897b"
        )
      )
    })

    report_download <- function(language) {
      report_language <- if (identical(language, "zh")) "Chinese" else "English"
      steps <- c("Collect analysis results", "Validate report inputs", "Prepare report files", "Render HTML report", "Finalize download file")

      downloadHandler(
        filename = function() paste0(Sys.Date(), "_Report_", toupper(language), ".html"),
        contentType = "text/html",
        content = safe_download_content(function(file) {
          params <- require_download_result(report_params())
          required <- report_parameter_names[report_parameter_names != "reportLanguage"]
          require_download_result(
            params,
            available = all(required %in% names(params)) &&
              !any(vapply(params[required], is.null, logical(1)))
          )
          params$reportLanguage <- language
          shiny::withProgress(
            message = paste("Generating", report_language, "report"),
            min = 0,
            max = 5,
            value = 0,
            {
              shiny::setProgress(1, detail = paste0("1/5 - ", steps[[1L]]))
              render_metabo_report(
                params,
                file,
                progress = function(step, detail) {
                  shiny::setProgress(step, detail = paste0(step, "/5 - ", steps[[step]]))
                }
              )
            }
          )
        })
      )
    }

    output$report_en <- report_download("en")
    output$report_zh <- report_download("zh")
  })
}
