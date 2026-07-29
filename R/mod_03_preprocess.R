#' 03_preprocess UI Function
#
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom plotly plotlyOutput

mod_03_preprocess_ui <- function(id){
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
               p("1. If your data contain missing values, choose a method in", strong("Missing Value Imputation"), "; otherwise, skip this step."),
               p("2. If your data contain pooled QC samples, use", strong("QC-based Data Filtering"), "to remove unstable features. The default CV threshold is 30%, so features with CV > 30% in pooled QCs are filtered out. Adjust the threshold to suit your study."),
               p("3. Use", strong("Data Treatment"), "to normalize, transform, and scale the data. The total-intensity bar plot, PCA score plot, and feature-distribution plots can help you choose suitable settings.")
               )
             ),

      #2. Data Cleaning Panel ==================================================
      column(width = 4,
             ##(1) Missing Data Computation Panel ------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Missing Value Imputation"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note:")),
               p(style = "color:#C70039;", "This step is automatically skipped if no missing values are detected."),
               p(strong("Select (a) or (b):")),
               p(style = "color:#C70039;", "(a) Missing Not at Random (MNAR)"),
               p(style = "color:#C70039;", "(b) Missing Completely at Random (MCAR)"),

               radioButtons(inputId = ns("missingValue"),
                            label = "1. Replace missing values with",
                            choices = c("1 (a)" = "1",
                                        "1/5 minimum of the mass feature, LOD (a)" = "HM",
                                        "k-nearest neighbors (b)" = "KNN",
                                        "Feature mean (b)" = "Mean",
                                        "Feature median (b)" = "Median"
                                        ),
                            selected = "1"
                            )
               ),

             ##(2) Data Filtering Panel ----------------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("QC-based Data Filtering"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               radioButtons(inputId = ns("QCFiltering"),
                            label = "2. Filter features using QC samples?",
                            choices = c("Yes" = "1", "No" = "0"),
                            selected = "1"
                            ),

               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#C70039;", "Features with CV values above the threshold will be filtered out."),

               sliderInput(inputId = ns("QCCV"),
                           label = "If Yes, enter the QC CV% filtering threshold",
                           value = 30,
                           min = 0,
                           max = 100
                           )
               ),

             ##(3) Data Transformation Panel -----------------------------------
             box(
               width = 12,
               inputId = "input_card",
               title = strong("Data Treatment"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,

               radioButtons(inputId = ns("normalizeMethod"),
                            label = "Sample Normalization",
                            choices = c("None" = "None",
                                        "Internal standard" = "IS",
                                        "Linear baseline normalization based on mean values" = "LBME",
                                        "Linear baseline normalization based on median values" = "LBMD",
                                        "Probabilistic quotient normalization" = "PQN",
                                        "Quantile normalization" = "QT"
                                        ),
                            selected = "None"
                            ),

               uiOutput(outputId = ns("selectIS"),
                        label = "Select the row number(s) of your internal standard(s)"
                        ),

               radioButtons(inputId = ns("transformMethod"),
                            label = "Feature Transformation",
                            choices = c("None" = "None",
                                        "Log10" = "Log10"
                                        ),
                            selected = "Log10"
                            ),

               radioButtons(inputId = ns("scaleMethod"),
                            label = "Feature Scaling",
                            choices = c("None" = "None",
                                        "Mean Centering" = "meanCenter",
                                        "Auto Scaling" = "autoScale",
                                        "Pareto Scaling" = "paretoScale",
                                        "Range Scaling" = "rangeScale",
                                        "Vast Scaling" = "vastScale",
                                        "Level Scaling" = "levelScale"
                                        ),
                            selected = "autoScale"
                            )
               )
             ),

      #3.Result Panel ==========================================================
      column(width = 8,
             ##(1) Sample Table Panel ------------------------------------------
             box(
               width = 12,
               inputId = ns("Table_card"),
               title = strong("Processed Data Table"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#C70039;", "1. If you plan to use internal standard-based normalization,
                 you can use this table to check the row numbers of your internal standard(s)."),
               p(style = "color:#C70039;", "2. You can use more than one internal standard for IS-based normalization. The mean peak area is used for normalization."),
               p(style = "color:#C70039;", "3. If only one internal standard is used, a small amount of random noise is added to its normalized peak areas to prevent errors in subsequent multivariate analyses."),
               shinycssloaders::withSpinner(DT::dataTableOutput(ns("filteredTable")), type = 5)
             ),

             ##(2) Boxplot Panel -----------------------------------------------
             box(
               width = 12,
               inputId = ns("Boxplot_card"),
               title = strong("Feature Distribution Panel"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#C70039;", "1. The box plot shows how feature distributions change with different sample-normalization methods."),
               p(style = "color:#C70039;", "2. Use the box plot to help select a sample-normalization method."),
               p(style = "color:#C70039;", "3. Scaling is not applied here, so changing the scaling method does not affect this plot."),
               varSelectInput(inputId = ns("prePCAGroup"),
                              label = "1. Select metadata (sample groups) for sample coloring",
                              data = ""
                              ),
               shinycssloaders::withSpinner(shiny::plotOutput(ns("TICPlot")), type = 5)
               ),

             ##(3) PCA Panel ---------------------------------------------------
             box(
               width = 12,
               inputId = "RawData_card",
               title = strong("PCA Panel"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 6,
                               selectInput(inputId = ns("prePCAX"),
                                           label = "Which PC on the x-axis?",
                                           choices = list(1, 2, 3, 4),
                                           selected = 1,
                                           multiple = FALSE
                                           )
                               ),

                        column(width = 6,
                               selectInput(inputId = ns("prePCAY"),
                                           label = "Which PC on the y-axis?",
                                           choices = list(1, 2, 3, 4),
                                           selected = 2,
                                           multiple = FALSE
                                           )
                               ),

                        column(width = 6,
                               radioButtons(inputId = ns("prePCAFrame"),
                                            label = "Add frame for group?",
                                            choices = c(
                                              "None" = "none",
                                              "Convex hull" = "polygon",
                                              "95% confidence ellipse" = "norm"
                                            ),
                                            selected = "none"
                                            )
                               )
                        ),
               shinycssloaders::withSpinner(plotly::plotlyOutput(ns("prePCAPlot")), type = 5)
             ),

             ##(4) Feature Panel -----------------------------------------------
             box(
               width = 12,
               inputId = "Feature_card",
               title = strong("Feature Panel"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = FALSE,
               closable = FALSE,
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#C70039;", "1. Click the button to view 30 randomly selected features."),
               p(style = "color:#C70039;", "2. Click again to sample a different set of features."),

               actionButton(inputId = ns("featureSampling"),
                            label = "Resample Features",
                            icon = icon("paper-plane"),
                            style = "color: #fff; background-color: #7570b3; border-color: #7570b3"
                            ),
               shinycssloaders::withSpinner(shiny::plotOutput(ns("featureDensityPlot")), type = 5),
               shiny::plotOutput(ns("featureBoxPlot"))
               )
             )
      )
    )
}


#' 03_preprocess Server Functions
#'
#' @noRd
#' @importFrom ggplot2 aes element_text position_jitter after_stat

mod_03_preprocess_server <- function(id, sfData){
  ns <- NS(id)
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #1. Pre-processing Parameters ==============================================
    missingValue <- reactive({
      as.character(input$missingValue)
      })

    QCFiltering <- reactive({
      as.logical(as.numeric(input$QCFiltering))
      })

    QCCV <- reactive({
      input$QCCV
      })

    normalizeMethod <- reactive({
      as.character(input$normalizeMethod)
      })

    transformMethod <- reactive({
      as.character(input$transformMethod)
      })

    scaleMethod <- reactive({
      as.character(input$scaleMethod)
      })

    prePCAGroup <- reactive({
      as.character(input$prePCAGroup)
      })

    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "prePCAGroup",
        data = sfData$group,
        selected = "Group1"
        )
      })

    prePCAX <- reactive({
      as.numeric(input$prePCAX)
      })

    prePCAY <- reactive({
      as.numeric(input$prePCAY)
    })

    prePCAFrame <- reactive({
      as.character(input$prePCAFrame)
    })


    observeEvent(input$normalizeMethod, {
      req(input$normalizeMethod == "IS")
      output$selectIS <- renderUI({
        shiny::req(sfData$data)
        selectizeInput(inputId = ns("selectIS"),
                       label = "Select the row number(s) of your internal standard(s)",
                       multiple = TRUE,
                       choices = 1:nrow(sfData$data),
                       selected = 1
                       )
        })
    })

    #2. Data Pre-processing ====================================================
    ##(1) Fill missing values---------------------------------------------------
    fillMissing <- reactive({
      shiny::req(sfData$data)
      df2 <- fillNA(sfData$data, missingValue())
      ## replace 0 with 1, for log transformation
      sampleColumns <- setdiff(names(df2), "ID")
      df2[sampleColumns][df2[sampleColumns] == 0] <- 1
      df2$ID <- cleanNames(df2$ID)
      return(df2)
      })

    ##(2) QC-based filtering----------------------------------------------------
    tFillMissing <- reactive({
      shiny::req(fillMissing())
      tem <- fillMissing() %>%
        dplyr::select(-ID) %>%
        t()
      colnames(tem) <- fillMissing()$ID
      return(tem)
      })

    dfCV <- reactive({
      shiny::req(tFillMissing())
      getCV(x = tFillMissing(), Group = sfData$group[, prePCAGroup()])
    })

    QCFilteredData <- reactive({
      shiny::req(fillMissing())
      shiny::req(dfCV())
      tem1 <- cbind.data.frame(dfCV(), fillMissing())
      if(isTRUE(QCFiltering()) & ("CV_QC" %in% names(tem1))){
        tem2 <- tem1 %>%
          dplyr::filter(CV_QC <= QCCV()) %>%
          dplyr::select(-starts_with("CV"))
        } else{
        tem2 <- fillMissing()
        }
      rownames(tem2) <- NULL
      sfData$filter <- tem2
      return(tem2)
      })

    ##(3) Data treatment--------------------------------------------------------
    treatedData <- reactive({
      shiny::req(QCFilteredData())
      normalizedData <- doNormalization(QCFilteredData() %>% dplyr::select(-ID),
                                      Method = normalizeMethod(),
                                      Inx = as.numeric(input$selectIS)
                                      )

      transformedData <- switch(transformMethod(),
                                "None" = normalizedData,
                                "Log10" = log10(normalizedData)
                                )

      sfData$filterNormTransform <- transformedData %>%
        as.data.frame() %>%
        dplyr::mutate(ID = QCFilteredData()$ID) %>%
        dplyr::relocate(ID)

      scaledData <- scaleData(transformedData, Method = scaleMethod()) %>%
        as.data.frame() %>%
        dplyr::mutate(ID = QCFilteredData()$ID) %>%
        dplyr::relocate(ID)
      sfData$clean <- scaledData
      return(scaledData)
      })

    #3.Output Plot and Table ---------------------------------------------------
    ##(1) Sample Data Table-----------------------------------------------------
    output$filteredTable <- DT::renderDataTable({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      DT::datatable(sfData$filterNormTransform,
                    options = list(scrollX = TRUE,
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   fixedColumns = FALSE
                                   )
                    )
      })

    ##(2) Feature Box Plot------------------------------------------------------
    output$TICPlot <- shiny::renderPlot({
      shiny::req(sfData$filterNormTransform, sfData$group, prePCAGroup())
      showTIC(df = sfData$filterNormTransform, Group = sfData$group[, prePCAGroup()])
    })

    ##(3) prePCA ---------------------------------------------------------------
    prePCAPlot <- reactive({
      shiny::req(treatedData())
      shiny::req(sfData$group)
      prePCAData <- treatedData() %>%
        dplyr::select(-ID) %>%
        t()
      p <- showPCA(prePCAData,
                   Group = sfData$group[, prePCAGroup()],
                   inx = prePCAX(),
                   iny = prePCAY(),
                   showFrame = prePCAFrame(),
                   interactive = TRUE
                   )
      return(p)
      })

    output$prePCAPlot <- plotly::renderPlotly({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      shiny::validate(need(prePCAX() != prePCAY(), message = "Please use different PCs"))
      prePCAPlot()
      })

    ##(4) Plot Features --------------------------------------------------------
    ## assign initial values for nFeature
    nFeature <- reactiveValues()
    nFeature$n <- 1:30
    observeEvent(input$featureSampling, {
      nFeature$n <- sample(1:dim(QCFilteredData())[1], min(30, dim(QCFilteredData())[1]))
      })

    combinedData <- reactive({
      preData <- QCFilteredData()[nFeature$n, ] %>%
        dplyr::mutate(ID = sub("_.*", "", ID)) %>%
        tidyr::pivot_longer(cols = !ID, names_to = "Sample", values_to = "Area") %>%
        dplyr::mutate(Treatment = "Before Data Treatment")

      afterData <- treatedData()[nFeature$n, ] %>%
        dplyr::mutate(ID = sub("_.*", "", ID)) %>%
        tidyr::pivot_longer(cols = !ID, names_to = "Sample", values_to = "Area") %>%
        dplyr::mutate(Treatment = "After Data Treatment")

      combinedData <- rbind.data.frame(preData, afterData) %>%
        dplyr::mutate(Treatment = factor(Treatment,
                                         levels = c("Before Data Treatment", "After Data Treatment")))
      return(combinedData)
      })

    featureDensityPlot <- reactive({
      p <- ggplot2::ggplot(combinedData(), aes(x = Area, y = after_stat(scaled), fill = Treatment)) +
        ggplot2::geom_density(alpha = 0.8) +
        ggplot2::scale_fill_manual(values = treatment_colors, guide = 'none') +
        ggplot2::facet_wrap(~ Treatment, scales = "free") +
        ggplot2::xlab("") +
        ggplot2::ylab("Density") +
        ggplot2::ggtitle(paste0("Density plots of ", length(nFeature$n), " randomly selected features")) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 16))
      return(p)
      })

    featureBoxPlot <- reactive({
      p <- ggplot2::ggplot(combinedData(), aes(x = ID, y = Area)) +
        ggplot2::geom_boxplot(aes(fill = Treatment), alpha = 0.8) +
        ggplot2::scale_fill_manual(values = treatment_colors, guide = 'none') +
        ggplot2::geom_jitter(position = position_jitter(0.2), alpha = 0.3) +
        ggplot2::facet_wrap(~ Treatment, scales = "free") +
        ggplot2::xlab("Metabolite ID") +
        ggplot2::ylab("Peak Area") +
        ggplot2::ggtitle(paste0("Box plots of ", length(nFeature$n), " randomly selected features")) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = element_text(size = 16)) +
        ggplot2::coord_flip()
      return(p)
      })

    output$featureDensityPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      featureDensityPlot()
      })

    output$featureBoxPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      featureBoxPlot()
      })

  })
}
