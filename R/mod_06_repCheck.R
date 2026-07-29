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
               p("1. This widget is designed to check if any duplicate rows exist."),
               p("2. After uploading the data table, you can select the columns to define duplicate rows."),
               p("3. Two types of duplicates are defined:", strong("Type A columns:"), "rows are considered duplicates for the selected Type A columns
                 if all elements from those columns are identical. For instance, if rows with both identical molecular weight and identification, then these
                 rows are considered as duplicates. ", strong("Type B columns:"), "rows are considered duplicates for the selected Type B
                 columns if any one element from those columns is identical. For instance, if rows with either molecular weight or identification identical, these
                 rows are considered as duplicates. Finally, rows are considered duplicates for the entire data table only if when they are duplicates in both Type A and Type B."),
               p("4. You can select type A and type B columns in the follow boxes. You can also leave one of them empty according to your needs, but you cannot
                 leave both empty."),
               p("5. Click", strong("Submit"), "button to perform duplicate analysis."),
               p("6. A label", strong("true"), "will be added to duplicate rows. For rows without any duplicates, a label", strong("false"), "will be added.")
               )
             ),

      #(2) Data Input Panel ====================================================
      column(width = 5,
             ##(1) Data upload Panel -------------------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Data Input Panel"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               fileInput(inputId = ns("rawFile"),
                         label = "1. Upload Data Table:",
                         multiple = FALSE,
                         placeholder = "accepts csv, xls or xlsx format",
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
               title = strong("Replicate Check Data Panel"),
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
                  label = "Select columns type A",
                  multiple = TRUE,
                  choices = names(inputData())
                  )
      })

    output$selectColumnAnyOut <- renderUI({
      shiny::req(inputData())
      selectInput(inputId = ns("selectColumnAny"),
                  label = "Select columns type B",
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
      shiny::validate(need(!is.null(input$selectColumnAll), message = "At least one column should be selected"))
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
