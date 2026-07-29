#' 08_lcTool UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_08_lcTool_ui <- function(id){
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
               p("1. This widget is designed to calculate the void volume and equilibration time."),
               p("2. The void volumn is calculated with this formula:", strong("Pi x r^2 × L × pore volume"),
                 "in which pi is 3.14, r is radius, L is the length of the column, and pore volume is 0.7 here as
                 LC column contains packed material, which occupies 65% to 70% of the column volume."),
               p("3. It typically require between 10 and 20 column volumes before the LC column is fully equilibrated and ready to use.
                 The required equilibration time will therefore vary depending on the column volumes you choose."),
               p("4. Pay attention to the unit.")
               )
             ),

      #(2) Data Input Panel ====================================================
      ## 1. Calculate void volume ----------------------------------------------
      column(width = 6,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Calculate void Volume & Equilibration Time"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,

               numericInput(inputId = ns("idColumn"),
                            label = "1. Please enter the internal diameter (mm) of the column",
                            value = NULL,
                            min = 0,
                            max = NA
                            ),

               numericInput(inputId = ns("lenColumn"),
                            label = "2. Please enter the length (mm) of the column",
                            value = NULL,
                            min = 0,
                            max = NA
                            ),

               numericInput(inputId = ns("flowRate"),
                            label = "3. Please enter the flow rate (mL/min) of your LC system",
                            value = 0,
                            min = 0,
                            max = NA
                            ),

               numericInput(inputId = ns("nColVolume"),
                            label = "4. Please enter the number of column volum",
                            value = 10,
                            min = 0,
                            max = NA
                            ),

               actionButton(inputId = ns("getVm"),
                            label = "Calculate",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),


      #(3) Result Panel ========================================================
      column(width = 6,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Calculated Void Volume & Equilibration Time"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               htmlOutput(ns("vmOutput"))
               )
             )
      )
    )
  }

#' 08_lcTool Server Functions
#'
#' @noRd
mod_08_lcTool_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #(1) Show Result ===========================================================
    observeEvent(input$getVm, {
      ##(1) Load Data ----------------------------------------------------------
      ###(1.1) get column inner diameter (mm)
      idColumn <- reactive({
        if(is.null(input$idColumn)) {return(NULL)}
        return(input$idColumn)
        })

      ###(1.2) Get column length
      lenColumn <- reactive({
        if(is.null(input$lenColumn)) {return(NULL)}
        return(input$lenColumn)
        })

      ###(1.3) Get flow rate
      flowRate <- reactive({
        if(is.null(input$flowRate)) {return(NULL)}
        return(input$flowRate)
        })

      ###(1.4) Get number of column volume
      nColVolume <- reactive({
        if(is.null(input$nColVolume)) {return(NULL)}
        return(input$nColVolume)
        })

      ##(2) Calculate void volume ----------------------------------------------
      output$vmOutput <- renderText({
        shiny::validate(need(idColumn() > 0, message = "Value of column inner diameter should be bigger than 0"))
        shiny::validate(need(lenColumn() > 0, message = "Value of column length should be bigger than 0"))
        shiny::validate(need(flowRate() >= 0, message = "LC flow rate cannot be negative value"))
        shiny::validate(need(nColVolume() > 0, message = "Value of number of column volume should be bigger than 0"))

        Vm <- round(0.7 * pi * (idColumn()/20)^2 * lenColumn()/10, 2) # cm unit
        Etime <- ifelse(flowRate() > 0, round(nColVolume() * Vm/flowRate(), 2), "unknown")
        str1 = paste0("The void volume for your column is: ", "<font color=\"#DC4028\"><b>", Vm, " (mL)", "</b></font>")
        str2 = paste0("The minimum equilibration time is: ", "<font color=\"#DC4028\"><b>", Etime, " (min)", "</b></font>")
        HTML(paste(str1, str2, sep = '<br/>'))
        })
      })
    })
  }
