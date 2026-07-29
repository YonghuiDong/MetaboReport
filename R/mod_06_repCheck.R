#' 06_repCheck UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'


mod_06_repCheck_ui <- function(id){
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
               p("1. Use this widget to identify duplicate rows in a data table."),
               p("2. After uploading a table, select the columns used to define duplicates."),
               p("3.", strong("Type A columns:"), "rows match only when all selected Type A values are identical.", strong("Type B columns:"), "rows match when any selected Type B value is identical. When both types are selected, both conditions must be satisfied."),
               p("4. Type A or Type B may be left empty, but at least one column must be selected."),
               p("5. Click", strong("Submit"), "to run the duplicate check."),
               p("6. The result adds an", strong("isReplicate"), "column: TRUE marks duplicate rows and FALSE marks rows without a duplicate.")
               )
             ),

      #(2) Data Input Panel ====================================================
      column(width = 5,
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
               fileInput(inputId = ns("rawFile"),
                         label = "1. Upload data table",
                         multiple = FALSE,
                         placeholder = "Accepts CSV, XLS, or XLSX files.",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               column(width = 6,
                      uiOutput(outputId = ns("selectColumnAllOut"))
                      ),
               column(width = 6,
                      uiOutput(outputId = ns("selectColumnAnyOut"))
                      ),
               actionButton(inputId = ns("submit"),
                            label = "Submit",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      #(3) Result Panel ========================================================
      column(width = 7,
             box(
               width = 12,
               inputId = "repCheck_card",
               title = strong("Duplicate Check Results"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               downloadButton(outputId = ns("downloadrepCheckTable")),
               br(),
               br(),
               DT::dataTableOutput(outputId = ns("repView"))
               )
             )
      )
    )
  }

#' 06_repCheck Server Functions
#'
#' @noRd
mod_06_repCheck_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #1.Load Data ===============================================================
    inputData <- reactive({
      read_uploaded_table(input$rawFile)
    })

    #2. Show Result ============================================================

    ##(1) Select Columns for Replicate Analysis --------------------------------
    output$selectColumnAllOut <- renderUI({
      shiny::req(inputData())
      selectInput(inputId = ns("selectColumnAll"),
                  label = "Select Type A columns",
                  multiple = TRUE,
                  choices = names(inputData())
                  )
      })

    output$selectColumnAnyOut <- renderUI({
      shiny::req(inputData())
      selectInput(inputId = ns("selectColumnAny"),
                  label = "Select Type B columns",
                  multiple = TRUE,
                  choices = names(inputData())
                  )
      })

    observe({
      if(!is.null(input$selectColumnAny)){
        updateSelectInput(session = session,
                          inputId = "selectColumnAll",
                          choices = names(inputData())[!(names(inputData()) %in% input$selectColumnAny)],
                          selected = isolate(input$selectColumnAll)
                          )
        }
      })

    observe({
      if(!is.null(input$selectColumnAll)){
        updateSelectInput(session = session,
                          inputId = "selectColumnAny",
                          choices = names(inputData())[!(names(inputData()) %in% input$selectColumnAll)],
                          selected = isolate(input$selectColumnAny)
                          )
        }
      })

    ##(2) Replicates Analysis --------------------------------------------------
    observeEvent(input$submit, {
      shiny::req(inputData())
      shiny::validate(need(
        !is.null(input$selectColumnAll) || !is.null(input$selectColumnAny),
        message = "Select at least one Type A or Type B column."
      ))
      repData <- checkReplicates(df = inputData(),
                                 col_all = input$selectColumnAll,
                                 col_any = input$selectColumnAny)

      ##(3) Show Replicate Table -----------------------------------------------
      output$repView <- DT::renderDataTable({
        shiny::validate(need(!is.null(repData), message = "No input data"))
        DT::datatable(repData,
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
        })

      ##(4) Download Replicate Table -------------------------------------------
      output$downloadrepCheckTable <- downloadHandler(
        filename = "replicatesCheck.csv",
        content = function(file){
          write.csv(repData, file, row.names = FALSE)
          }
        )

    })
  })
}
