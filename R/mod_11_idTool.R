#' 11_idTool UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'

mod_11_idTool_ui <- function(id){
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
               p("1. Use this widget to putatively annotate metabolites with your own database."),
               p("2. Both your sample table and database table should contain a column named", strong("mz"), "."),
               p("3. To search by retention time, ensure that both your sample table and database table have a column labeled",
                 strong("RT"), ". Retention time must be expressed in minutes."),
               p("4. Matches are candidate annotations and should be confirmed with additional evidence where possible.")
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
               fileInput(inputId = ns("sampleFile"),
                         label = "1. Upload sample table",
                         multiple = FALSE,
                         placeholder = "Accepts CSV, XLS, or XLSX files.",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               fileInput(inputId = ns("dbFile"),
                         label = "2. Upload database table",
                         multiple = FALSE,
                         placeholder = "Accepts CSV, XLS, or XLSX files.",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               sliderInput(inputId = ns("ppmTolerance"),
                           label = "3. Select the mass tolerance (ppm)",
                           min = 1 ,
                           max = 50,
                           value = 5,
                           step = 1),
               selectInput(inputId = ns("useRT"),
                           label = "4. Match by retention time?",
                           choices = list("Yes", "No"),
                           selected = "No",
                           multiple = FALSE
                           ),
               sliderInput(inputId = ns("rtTolerance"),
                           label = "4.1 If Yes, select the retention-time tolerance (min)",
                           min = 0.2 ,
                           max = 10,
                           value = 1,
                           step = 0.1),
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
               title = strong("Candidate Matches"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               uiOutput(outputId = ns("download")),
               hr(),
               DT::dataTableOutput(outputId = ns("idResult"))
               )
             )
      )
  )
}

#' 11_idTool Server Functions
#'
#' @noRd
#' @importFrom dplyr relocate %>%
#'
mod_11_idTool_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ##(1) Load Data ----------------------------------------------------------
    sampleFile <- reactive({
      read_uploaded_table(input$sampleFile, check_names = TRUE)
      })

    dbFile <- reactive({
      read_uploaded_table(input$dbFile, check_names = TRUE)
      })

    ppmTolerance <- reactive({input$ppmTolerance})
    useRT <- reactive({input$useRT})
    rtTolerance <- reactive({input$rtTolerance})

    ##(2) Search result --------------------------------------------------------
    getIdentification <- reactive({
      shiny::validate(need(!is.null(sampleFile()), message = "no sample file"))
      shiny::validate(need(!is.null(sampleFile()$mz), message = "mz column in sample file not found"))
      shiny::validate(need(!is.null(dbFile()), message = "no database file"))
      shiny::validate(need(!is.null(dbFile()$mz), message = "mz column in database file not found"))
      shiny::withProgress(
        message = "Searching in database",
        detail = "Be patient...",
        value = 0.4,
        {
          if(useRT() == "Yes"){
            shiny::validate(need(!is.null(sampleFile()$RT), message = "RT column in sample file not found"))
            shiny::validate(need(!is.null(dbFile()$RT), message = "RT column in database file not found"))
            tem <- searchDB(DF = sampleFile(), DB = dbFile(), ppm = ppmTolerance(), RT = rtTolerance(), useRT = TRUE) %>%
              dplyr::relocate("QuerryID", "Sample.mz", "Sample.RT", "DB.mz", "DB.RT", "ppm", "RT_dif")
          } else {
            tem <- searchDB(DF = sampleFile(), DB = dbFile(), ppm = ppmTolerance(), useRT = FALSE) %>%
              dplyr::relocate("QuerryID", "Sample.mz", "Sample.RT", "DB.mz", "DB.RT", "ppm")
          }
          if(is.null(tem)){tem = "Not Found"}
          return(tem)
        })
    })

    ##(3) Show result ----------------------------------------------------------
    observeEvent(input$submit, {
      output$idResult <- DT::renderDataTable({
        DT::datatable(getIdentification(),
                      rownames = FALSE,
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
        })

      #(4) download result -----------------------------------------------------
      output$download <- renderUI({
        if(!is.null(getIdentification())) {
          downloadButton(outputId = ns('idFile'),
                         label = paste('Download')
          )
        }
      })

      output$idFile <- downloadHandler(
        filename = "Identification.csv",
        content = safe_download_content(function(file) {
          result <- require_download_result(getIdentification())
          write.csv(result, file, row.names = FALSE)
        })
      )

    })

  })
}
