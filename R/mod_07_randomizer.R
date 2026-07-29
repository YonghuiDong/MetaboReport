#' 07_randomizer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

sampleMatrix <- matrix(data = NA, nrow = 1, ncol = 2, dimnames = list(NULL, c("Group", "Replicates")))
mod_07_randomizer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #1. User Guide ===========================================================
      column(width = 12,
             box(
               width = 12,
               title = strong("User Guide"),
               status = "warning",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p("1. This widget prepares an LC-MS injection sequence using block randomization."),
               p("2. Set the parameters and click", strong("Randomize"), ". The injection sequence will then be ready to download."),
               p("3. To generate a different sequence, click", strong("Randomize"), "again.")
               )
             ),

      #2. Data Input Panel =====================================================
      column(width = 4,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Enter sample metadata and replicates"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,

               shinyMatrix::matrixInput(
                 inputId = ns("sampleList"),
                 value = sampleMatrix,
                 rows = list(extend = TRUE, names = FALSE),
                 cols = list(names = TRUE),
                 class = (c("character"))
                 ),

               p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#b2182b;", "1. Please provide the sample group and replicate information in the table above.
                 Please enter the replicate information first in the table to avoid any potential bugs. If you encounter any issues entering the
                 information, refresh the page and re-enter the data."),
               p(style = "color:#b2182b;", "2. Blanks can be inserted at the start of the injection sequence and/or within the sequence after each quality control (QC)."),
               p(style = "color:#b2182b;", "3. QC will be inserted every N samples."),
               p(style = "color:#b2182b;", "4. You can reserve certain positions for other purposes, such as additional blanks or QCs.
                 These positions will not be used for regular sample placement."),
               sliderInput(inputId = ns("nBlank"),
                           label = "Number of blanks at the start of the sequence",
                           min = 0,
                           max = 20,
                           value = 0,
                           step = 1
                           ),
               sliderInput(inputId = ns("nQC"),
                           label = "QC frequency (insert one QC every N samples)",
                           min = 0,
                           max = 20,
                           value = 0,
                           step = 1,
                           ),
               sliderInput(inputId = ns("nWithinBlank"),
                           label = "Number of blanks after each QC",
                           min = 0,
                           max = 20,
                           value = 0,
                           step = 1,
                           ),
               sliderInput(inputId = ns("nEmptyCell"),
                           label = "Number of sample positions to reserve",
                           min = 0,
                           max = 10,
                           value = 0,
                           step = 1,
                           ),
               p(strong("Illustration of an LC sample plate")),
               column(width = 12, img(src='www/img/lcPlate.png', align = "left", width = "100%")),
               numericInput(inputId = ns("plateRow"),
                            label = "Number of rows in the sample plate",
                            value = 6,
                            min = 1,
                            step = 1
                            ),
               numericInput(inputId = ns("plateCol"),
                            label = "Number of columns in the sample plate",
                            value = 8,
                            min = 1,
                            step = 1
                            ),
               selectInput(inputId = ns("plateIDType"),
                           label = "Please select the sample plate ID type",
                           choices = list("Letters, such as A, B, C" = "Letter",
                                          "Numbers, such as 1, 2, 3" = "Number"
                                          ),
                           selected = "Letter"
                           ),
               selectInput(inputId = ns("outputType"),
                           label = "Please select the injection sequence type",
                           choices = list("Orbitrap", "Waters"),
                           selected = "Orbitrap"
                           ),
               actionButton(inputId = ns("randomize"),
                            label = "Randomize",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      #(3) Result Panel ========================================================
      column(width = 8,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Result"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#b2182b;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#b2182b;", "For an Orbitrap sequence, insert", strong("Bracket Type = 4"), "in the first row of the downloaded sequence file."),
               uiOutput(outputId = ns("downloadSeq")),
               hr(),
               DT::dataTableOutput(ns("randomizedList"))
               )
             )
      )
    )
  }

#' 07_randomizer Server Functions
#'
#' @noRd
mod_07_randomizer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    randomized <- eventReactive(input$randomize, {
      sample_list <- as.data.frame(input$sampleList, stringsAsFactors = FALSE)
      replicates <- suppressWarnings(as.integer(sample_list$Replicates))
      valid <-
        nrow(sample_list) > 0L &&
        all(!is.na(sample_list$Group)) &&
        all(nzchar(trimws(sample_list$Group))) &&
        all(!is.na(replicates)) &&
        all(replicates > 0L)
      validate(need(valid, "Enter a group and a positive replicate count in every row."))

      design <- data.frame(
        Group = rep(sample_list$Group, replicates),
        stringsAsFactors = FALSE
      )
      design$Rep <- ave(seq_len(nrow(design)), design$Group, FUN = seq_along)

      randomizeSeq(
        df = design,
        nBlank = input$nBlank,
        nQC = input$nQC,
        nWithinBlank = input$nWithinBlank,
        nEmptyCell = input$nEmptyCell,
        plateRow = input$plateRow,
        plateCol = input$plateCol,
        plateIDType = input$plateIDType,
        outputType = input$outputType
      )
    }, ignoreInit = TRUE)

    sequence_file <- reactive({
      req(randomized())
      format_injection_sequence(randomized(), input$outputType)
    })

    output$randomizedList <- DT::renderDataTable({
      DT::datatable(
        sequence_file(),
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          fixedColumns = FALSE,
          editable = TRUE
        )
      )
    })

    output$downloadSeq <- renderUI({
      req(sequence_file())
      downloadButton(
        outputId = session$ns("seqFile"),
        label = paste("Download", input$outputType, "Injection Sequence", sep = "_"),
        style = "color:#fff;background-color:#037e8a;border-color:#037e8a"
      )
    })

    output$seqFile <- downloadHandler(
      filename = function() paste0(input$outputType, "_Sequence_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(sequence_file(), file, row.names = FALSE)
      }
    )
  })
}
