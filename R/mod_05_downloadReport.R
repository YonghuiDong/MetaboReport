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
                 p("2. Report generation may take a few minutes. Click Generate Report once, then wait for the download button to appear."),
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
                 title = strong("Report"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
                 p(style = "color:#b2182b;", "Generating the report may take a few minutes. Click the download button only once."),
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
      downloadButton(
        outputId = session$ns("report"),
        label = "Download Report",
        style = "color:#fff;background-color:#7570b3;border-color:#7570b3"
      )
    })

    output$report <- downloadHandler(
      filename = function() paste0(Sys.Date(), "_Report.html"),
      contentType = "text/html",
      content = function(file) {
        shiny::withProgress(
          message = "Generating report",
          detail = "This may take a few minutes.",
          value = 0.4,
          render_metabo_report(report_params(), file)
        )
      }
    )
  })
}
