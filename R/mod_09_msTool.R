#' 09_msTool UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_09_msTool_ui <- function(id){
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
               p("1. This widget is designed to calculate m/z values, and identidy mass based on m/z values."),
               p("2. You have the option to either enter a single value or upload a list of values. If uploading
                 a list, please ensure that the correct required names are used."),
               p("3. The result can be downloaded in csv format."),
               p("4. Note that when using this module for the first time, it will take a bit longer to load the database.
                 However, subsequent calculations will be significantly faster.")
               )
             ),

      #2. Data Input Panel =====================================================
      ## 1. Calculate m/z values -----------------------------------------------
      column(width = 4,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Calculate m/z Values"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               textInput(inputId = ns("formula1"),
                         label = "1. Input formula",
                         value = NULL,
                         placeholder = "e.g., C7H6O"
                         ),
               fileInput(inputId = ns("formula2"),
                         label = "1. (alternatively) Upload a list of formulas:",
                         multiple = FALSE,
                         placeholder = "should contain a column named Formula",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               numericInput(inputId = ns("nCharge"),
                            label = "Input number of charge",
                            value = 1,
                            min = 1,
                            max = NA,
                            step = 1
                            ),
               column(width = 5,
                      radioButtons(inputId = ns("ionMode1"),
                            label = "Ion mode?",
                            choices = list("Positive" = 1, "Negative" = -1),
                            selected = NA
                            )
                      ),
               column(width = 7, uiOutput(outputId = ns("adducts"))),
               column(width = 12,
                      actionButton(inputId = ns("getMZ"),
                            label = "Calculate",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
                      )
               )
             ),

      ## 2. Tentatively identify metabolites -----------------------------------
      column(width = 4,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Identify m/z Values from HMDB"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               textInput(inputId = ns("mz1"),
                         label = "1. Input mz value",
                         value = "",
                         placeholder = "e.g., 133.014"
                         ),
               fileInput(inputId = ns("mz2"),
                         label = "1. (alternatively) Upload a list of mz values:",
                         multiple = FALSE,
                         placeholder = "should contain a column named mz",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               numericInput(inputId = ns("ppmTolerance"),
                            label = "Input mass accuracy tolerance (ppm)",
                            value = 5,
                            min = 0,
                            max = NA
                            ),
               radioButtons(inputId = ns("ionMode2"),
                            label = "Ion mode?",
                            choices = list("Positive" = "+", "Negative" = "-"),
                            selected = NA
                            ),
               actionButton(inputId = ns("searchMZ"),
                            label = "Search",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      ## 3. Identify noise peak ------------------------------------------------
      column(width = 4,
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Identify Noise Peaks"),
               status = "primary",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               textInput(inputId = ns("noise1"),
                         label = "1. Input noise mz value",
                         value = "",
                         placeholder = "e.g., 33.0335"
                         ),
               fileInput(inputId = ns("noise2"),
                         label = "1. (alternatively) Upload a list of noise mz values:",
                         multiple = FALSE,
                         placeholder = "should contain a column named mz",
                         accept = c(".csv", ".xls", ".xlsx")
                         ),
               numericInput(inputId = ns("ppmTolerance2"),
                            label = "Input mass accuracy tolerance (ppm)",
                            value = 5,
                            min = 0,
                            max = NA
                            ),
               radioButtons(inputId = ns("ionMode3"),
                            label = "Ion mode?",
                            choices = list("Positive" = "+", "Negative" = "-"),
                            selected = NA
                            ),
               actionButton(inputId = ns("searchNoise"),
                            label = "Search",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                            )
               )
             ),

      #3. Result Panel =========================================================
      column(width = 4,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Calculated m/z"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               downloadButton(outputId = ns("downloadmz")),
               br(),
               br(),
               DT::dataTableOutput(ns("mzOutput"))
               )
             ),
      column(width = 4,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Identify m/z"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               downloadButton(outputId = ns("downloadDBOutput")),
               br(),
               br(),
               DT::dataTableOutput(ns("dbOutput"))
               )
             ),

      column(width = 4,
             box(
               width = 12,
               inputId = "report_card",
               title = strong("Noise Peak"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               downloadButton(outputId = ns("downloadNoiseOutput")),
               br(),
               br(),
               DT::dataTableOutput(ns("noiseOutput"))
               )
             )
      )
    )
  }

#' 09_msTool Server Functions
#'
#' @noRd
mod_09_msTool_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ############################################################################
    # Calculate m/z values======================================================
    ############################################################################

    #(1) Update adducts based on ion mode=======================================
    ## (1.1) Get ionization mode -----------------------------------------------
    ionMode1 <- reactive({
      if(is.null(input$ionMode1)) {return(NULL)}
      return(input$ionMode1)
      })
    ##(1.2) Update adducts------------------------------------------------------
    adductTypes <- reactive({
      if(is.null(ionMode1())){
        return(NULL)
      } else if((as.numeric(ionMode1()) == -1)){
        return(c("H", "CH2O2", "CH3COOH"))
      } else{
        return(c("H", "Na", "K", "H30", "NH4"))
      }
    })
    output$adducts <- renderUI({
      selectInput(inputId = ns("adducts"),
                  label = "Select adducts",
                  multiple = TRUE,
                  choices = adductTypes()
                  )
      })

    ##(2) Get and show result ==================================================
    observeEvent(input$getMZ, {
      ## (1) Load Data ---------------------------------------------------------
      ###(1.1) get formula
      formula1 <- reactive({
        if(is.null(input$formula1)) {return(NULL)}
        return(input$formula1)
        })
      formula2 <- reactive({
        read_uploaded_table(input$formula2, check_names = TRUE)
        })
      if(is.null(formula2()$Formula)) {
        formula = formula1()
        } else {
          formula = formula2()$Formula
        }

      ###(1.2) Get number of charge---------------------------------------------
      nCharge <- reactive({
        if(is.null(input$nCharge)) {return(NULL)}
        return(input$nCharge)
        })

      ###(1.3) Get result ------------------------------------------------------
      mzValue <- reactive({
        shiny::validate(need(formula != "", message = "No formula found"))
        shiny::validate(need(nCharge() > 0, message = "number of charge should be positive value"))
        shiny::validate(need(!is.null(ionMode1()), message = "Please select ion mode"))
        shiny::withProgress(
          message = "It takes time to load the database when you use this module for the first time ",
          detail = "Be patient...",
          value = 0.4,
          {
            mode = ifelse(ionMode1() == 1, "positive", "negative")
            mzValue <- getMZ(formula = formula,
                             adducts = input$adducts,
                             z = nCharge() * as.numeric(ionMode1()),
                             mode = mode
                             )
            })
        return(mzValue)
        })

      ###(1.4) Show Result -----------------------------------------------------
      output$mzOutput <- DT::renderDataTable({
        shiny::req(mzValue())
        DT::datatable(mzValue(),
                      rownames = FALSE,
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
        })
      ###(1.5) Download Result -------------------------------------------------
      output$downloadmz <- downloadHandler(
        filename = "calculatedMZ.csv",
        content = function(file){
          write.csv(mzValue(), file, row.names = FALSE)
          }
        )
      })

    ############################################################################
    # Search m/z values in DB ==================================================
    ############################################################################

    #(1)Search m/z =============================================================
    observeEvent(input$searchMZ, {
      ##(1.1) get formula ------------------------------------------------------
      mz1 <- reactive({
        if(is.null(input$mz1)) {return(NULL)}
        return(as.numeric(input$mz1))
        })
      mz2 <- reactive({
        read_uploaded_table(input$mz2, check_names = TRUE)
        })
      if(is.null(mz2()$mz)) {
        mzList = mz1()
        } else {
          mzList = as.numeric(mz2()$mz)
          }

      ##(1.2) Get mass accuracy tolerance --------------------------------------
      ppmTolerance <- reactive({
        return(input$ppmTolerance)
        })

      ##(1.3) Get ionization mode ----------------------------------------------
      ionMode2 <- reactive({
        if(is.null(input$ionMode2)) {return(NULL)}
        return(input$ionMode2)
        })

      ##(1.4) Get Result -------------------------------------------------------
      idList <- reactive({
        shiny::validate(need(mzList != "", message = "No mz value found"))
        shiny::validate(need(ppmTolerance() > 0, message = "Mass tolerance (ppm) should be positive value"))
        shiny::validate(need(!is.null(ionMode2()), message = "Please select ion mode"))
        shiny::withProgress(
          message = "Searching in HMDB database ",
          detail = "Be patient...",
          value = 0.4,
          {
            idList <- MSbox::what(myMZ = mzList,
                                  mode = ionMode2(),
                                  ppm = ppmTolerance(),
                                  useDB = "HMDB")
          })
        if(is.null(idList)){idList = data.frame(Result = "Not Found")}
        return(idList)
      })

      #(1.5) Show Result -------------------------------------------------------
      output$dbOutput <- DT::renderDataTable({
        shiny::req(idList())
        DT::datatable(idList(),
                      rownames = FALSE,
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
        })

      ##(1.6) Download Result-------------------------------------------------
      output$downloadDBOutput <- downloadHandler(
        filename = "Identification_HMDB.csv",
        content = function(file){
          write.csv(idList(), file, row.names = FALSE)
          }
        )
      })


    ############################################################################
    # 3. Identify noise m/z peaks
    ############################################################################

    #(1) Search m/z=============================================================
    observeEvent(input$searchNoise, {
      ###(1.1) get formula -----------------------------------------------------
      noise1 <- reactive({
        if(is.null(input$noise1)) {return(NULL)}
        return(as.numeric(input$noise1))
        })
      noise2 <- reactive({
        read_uploaded_table(input$noise2, check_names = TRUE)
        })
      if(is.null(noise2()$mz)) {
        noiseList = noise1()
        } else {
          noiseList = as.numeric(noise2()$mz)
          }

      ##(1.2) Get mass accuracy tolerance --------------------------------------
      ppmTolerance2 <- reactive({
        return(input$ppmTolerance2)
        })

      ##(1.3) Get ionization mode ----------------------------------------------
      ionMode3 <- reactive({
        if(is.null(input$ionMode3)) {return(NULL)}
        return(input$ionMode3)
        })

      #(2) Result ==============================================================
      ##(1) Get Result----------------------------------------------------------
      getNoiseList <- reactive({
        shiny::validate(need(noiseList != "", message = "No mz value found"))
        shiny::validate(need(ppmTolerance2() > 0, message = "Mass tolerance (ppm) should be positive value"))
        shiny::validate(need(!is.null(ionMode3()), message = "Please select ion mode"))
        shiny::withProgress(
          message = "Identifying contanminates",
          detail = "Be patient...",
          value = 0.4,
          {
            getNoiseList <- MSbox::contam(mz = noiseList,
                                          mode = ionMode3(),
                                          ppm = ppmTolerance2())
          })
        if(is.null(getNoiseList)){getNoiseList = data.frame(Result = "Not Found")}
        return(getNoiseList)
      })

      ##(1.2) Show Result ------------------------------------------------------
      output$noiseOutput <- DT::renderDataTable({
        shiny::req(getNoiseList())
        DT::datatable(getNoiseList(),
                      rownames = FALSE,
                      options = list(scrollX = TRUE,
                                     deferRender = TRUE,
                                     scroller = TRUE,
                                     fixedColumns = FALSE
                                     )
                      )
        })
      ##(1.3) Download Result-------------------------------------------------
      output$downloadNoiseOutput <- downloadHandler(
        filename = "noisePeak.csv",
        content = function(file){
          write.csv(getNoiseList(), file, row.names = FALSE)
          }
        )
      })
    })
  }
