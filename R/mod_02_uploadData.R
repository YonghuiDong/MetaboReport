#' 02_uploadData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'


mod_02_uploadData_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(1) User Guide ==========================================================
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p("1. If your data table was prepared using Compound Discoverer, choose the", strong("Compound Discoverer"), "format; otherwise, select", strong("Other format"), "."),
               p("2. If your feature and metadata are stored in separate tables, upload both files.")
               )
      ),

      #(2) Data Input Panel ====================================================
      column(width = 4,
             ##(1) Data upload Panel -------------------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Data Input"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               selectInput(inputId = ns("fileFormat"),
                           label = "1. Select file format",
                           choices = list("Compound Discoverer (CD)" = "CD", "Other format" = "Other"),
                           selected = "CD",
                           multiple = FALSE
                           ),
               fileInput(inputId = ns("rawFile"),
                         label = "2. Upload data table (or feature table)",
                         multiple = FALSE,
                         placeholder = "Accepts CSV, XLS, or XLSX files.",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#b2182b;", "1. If there is no metadata in the file header, please upload the metadata table below."),
               p(style = "color:#b2182b;", "2. Refer to the Home page for detailed data table preparation."),
               fileInput(inputId = ns("inputMeta"),
                         label = "3. (Optional) Upload sample metadata table",
                         multiple = FALSE,
                         placeholder = "Accepts CSV, XLS, or XLSX files.",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               radioButtons(inputId = ns("showExample"),
                            label = "Use the demo data?",
                            choices = c("Yes" = "Yes", "No" = "No"),
                            selected = "No"
                            ),
               actionButton(inputId = ns("submit"),
                            label = "Submit",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
             ),

             ##(2) Outlier Removal Panel ---------------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Outlier Removal (Optional)"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#b2182b;", "Do not remove any samples unless they are identified as outliers."),
               p(style = "color:#2166ac;", shiny::icon("lightbulb"), strong("Tips: ")),
               p(style = "color:#2166ac;", "1. Proceed with your analysis without removing any samples."),
               p(style = "color:#2166ac;", "2. If you identify any outliers, come back and remove them."),
               uiOutput(outputId = ns("selectColumn")),
               column(width = 6, actionButton(inputId = ns("removeCol"), label = "Remove Selected Samples")),
               column(width = 6, actionButton(inputId = ns("undoCol"), label = "Restore Samples"))
               )
      ),

      #(3) Result Panel ========================================================
      column(width = 8,
             box(width = 12,
                 inputId = "RawData_card",
                 title = strong("Raw Data"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 shinycssloaders::withSpinner(DT::dataTableOutput(outputId = ns("rawView")), type = 5)
                 ),
             box(width = 12,
                 inputId = "meta_card",
                 title = strong("Metadata"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 verbatimTextOutput(outputId = ns("metaInfo")),
                 DT::dataTableOutput(outputId = ns("metaView"))
                 ),
             box(width = 12,
                 inputId = "sample_card",
                 title = strong("Processed Data"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 DT::dataTableOutput(outputId = ns("processView"))
                 )
      )


))}

#' uploadData Server Functions
#' @noRd
#' @importFrom dplyr %>%

mod_02_uploadData_server <- function(id, sfData){
  ns <- NS(id)
  moduleServer(id, function(input, output, session){

    #(1) Load Data =============================================================
    inputData <- reactive({
      if(input$showExample == "Yes"){return(cancerCell)}
      read_uploaded_table(input$rawFile)
    })

    inputMeta <- reactive({
      read_uploaded_table(input$inputMeta)
    })

    #(2) Format Data ===========================================================
    getProcessedData <- reactive({
      shiny::req(inputData())
      if(input$showExample == "Yes") {
        df <- formatData(DF = inputData(), format = "CD")
      } else{
        df <- formatData(DF = inputData(), metaGroup = inputMeta(), format = input$fileFormat)
        df$ID <- cleanNames(df$ID)
      }
      sfData$data <- df
      return(df)
    })

    #(3) Get Metadata ==========================================================
    observeEvent(sfData$data, {
      shiny::req(sfData$data)
      sfData$group <- getMeta(DF = sfData$data)
    })

    #(4) Show Result ===========================================================
    ##(4.1) Raw Data Overview --------------------------------------------------
    output$rawView <- DT::renderDataTable({
      shiny::req(inputData())
      DT::datatable(inputData(),
                    options = list(scrollX = TRUE,
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   fixedColumns = FALSE
                                   )
                    )
    }) |>
      bindEvent(input$submit)

    observeEvent(input$submit, {
      ##(4.2) Meta Info Overview -----------------------------------------------
      output$metaInfo <- renderPrint({
        shiny::req(getProcessedData())
        shiny::validate(need(!is.null(sfData$group), message = "Metadata not found."))
        cat(paste("Number of samples:", nrow(sfData$group), "\n"))
        cat(paste("Number of meta groups:", ncol(sfData$group), "\n"))
      })

      ##(4.3) Meta table -------------------------------------------------------
      output$metaView <- DT::renderDataTable({
        shiny::req(getProcessedData())
        DT::datatable(sfData$group,
                      caption = "Overview of Metadata Information",
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
      })

      ##(4.4) Processed Data Overview ------------------------------------------
      output$processView <- DT::renderDataTable({
        shiny::req(getProcessedData())
        DT::datatable(sfData$data,
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
      })
    })


    #(5) Remove Outlier ========================================================
    output$selectColumn <- renderUI({
      shiny::req(sfData$data)
      selectInput(inputId = ns("selectColumn"),
                  label = "Select sample(s) to remove",
                  multiple = TRUE,
                  choices = setdiff(names(sfData$data), "ID")
                  )
    })

    observeEvent(input$removeCol, {
      shiny::req(sfData$data)
      shiny::req(input$selectColumn)
      sfData$data <- removecolumn(sfData$data, input$selectColumn)
    })

    observeEvent(input$undoCol, {
      shiny::req(sfData$data)
      sfData$data <- getProcessedData()
    })

    return(inputData)
  })
}
