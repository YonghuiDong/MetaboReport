#' 04_viewResult UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom dplyr n
#'
mod_04_viewResult_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      #(1) User Guide **********************************************************
      column(width = 12,
             box(width = 12,
                 title = strong("User Guide"),
                 status = "warning",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 p("1. Perform statistical analyses and inspect the resulting tables and figures in this panel."),
                 p("2. Customize figure colors and aspect ratios before downloading them in your preferred format.")
                 )
             ),

      #(2) View Statistics Panel ***********************************************
      column(width = 4,
             box(width = 12,
                 inputId = ns("input_card"),
                 title = strong("Perform Statistics"),
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 collapsed = FALSE,
                 closable = FALSE,
                 selectInput(inputId = ns("statMethod"),
                             label = "1. What do you want to do?",
                             choices = c("Compare two unpaired groups" = "tTest",
                                         "Compare two paired groups" = "ptTest",
                                         "Compare more than two unpaired groups" = "anovaHSD",
                                         "Compare more than two paired groups" = "anovaRM"
                                         ),
                             selected = "anovaHSD",
                             multiple = FALSE
                             ),
                 selectInput(inputId = ns("pAdjMethod"),
                             label = "2. Adjust p-value by:",
                             choices = c("Don't adjust" = "none",
                                         "False Discovery Rate" = "fdr",
                                         "Bonferroni" = "bonferroni"
                                         ),
                             selected = "fdr",
                             multiple = FALSE
                             ),
                 radioButtons(inputId = ns("SigFilter"),
                              label = "3. Only consider statistically significant mass features?",
                              choices = c("Yes" = 1, "No" = 0),
                              selected = 0
                              ),
                 p(style = "color:#C70039;", shiny::icon("bell"), strong("Notes:")),
                 p(style = "color:#C70039;", "1. Only features with at least one p-value below the threshold are retained in the statistics table."),
                 p(style = "color:#C70039;", "2. This filter is not applied to PCA, OPLS-DA, or the volcano plot."),
                 sliderInput(inputId = ns("SigP"),
                             label = "If Yes, enter the p-value threshold",
                             value = 0.05,
                             min = 0,
                             max = 0.1
                             ),
                 actionButton(inputId = ns("viewStat"),
                              label = "Start",
                              icon = icon("eye"),
                              style = "color: #fff; background-color: #4daf4a; border-color: #4daf4a"
                              )
                 )
             ),

      #(3) Result Panel ********************************************************
      column(width = 8,
             ##(1) Statistics Panel --------------------------------------------
             box(width = 12,
                 inputId = "Stat_card",
                 title = strong("Statistics Panel"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 p(style = "color:#C70039;", shiny::icon("bell"), strong("Note: ")),
                 p(style = "color:#C70039;", "1. Fold change (FC) analysis is performed on missing value-filled and QC-filtered data."),
                 p(style = "color:#C70039;", "2. Univariate analysis is performed on filled, filtered, normalized and transformed data."),
                 p(style = "color:#C70039;", "3. VIP values are calculated based on filled, filtered, normalized, transformed and scaled data."),
                 fluidRow(width = 12,
                          column(width = 12,
                                 varSelectInput(inputId = ns("StatGroup"),
                                                label = "Select metadata (sample groups) for statistics",
                                                data = ""
                                                )
                                 ),
                          column(width = 12, downloadButton(outputId = ns("downloadStatTable")))
                          ),
                 br(),
                 shinycssloaders::withSpinner(DT::dataTableOutput(ns("StatResult")), type = 5)
                ),

             ##(2) PCA Panel ---------------------------------------------------
             box(width = 12,
                 inputId = "viewPCA",
                 title = strong("PCA Plot"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 closable = FALSE,
                 varSelectInput(inputId = ns("PCAGroup"),
                                label = "1. Select metadata (sample groups) for sample coloring",
                                data = ""
                                ),
                 fluidRow(width = 12,
                          column(width = 6,
                                 selectInput(inputId = ns("PCAX"),
                                             label = "Which PC on the x-axis?",
                                             choices = list(1, 2, 3, 4),
                                             selected = 1,
                                             multiple = FALSE
                                             )
                                 ),
                          column(width = 6,
                                 selectInput(inputId = ns("PCAY"),
                                             label = "Which PC on the y-axis?",
                                             choices = list(1, 2, 3, 4),
                                             selected = 2,
                                             multiple = FALSE
                                             )
                                 ),
                          column(width = 12,
                                 radioButtons(inputId = ns("PCAFrame"),
                                              label = "Add frame for group?",
                                              choices = c(
                                                "None" = "none",
                                                "Convex hull" = "polygon",
                                                "95% confidence ellipse" = "norm"
                                              ),
                                              selected = "none"
                                              )
                                 ),
                          column(width = 6,
                                 selectInput(inputId = ns("PCAColor"),
                                             label = "Color palette",
                                             choices = plot_palette_choices,
                                             selected = "Default"
                                             )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("PCAType"),
                                              label = "Select file type to download",
                                              choices = list("pdf", "tiff", "png"),
                                              selected = "pdf"
                                              )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("PCARatio"),
                                              label = "Aspect ratio of the plot (width:height)",
                                              value = 1,
                                              min = NA,
                                              max = NA
                                              )
                                 ),
                          column(width = 6,
                                 br(),
                                 downloadButton(outputId = ns("downloadPCA"))
                                 )
                          ),
                 shinycssloaders::withSpinner(plotly::plotlyOutput(ns("PCAPlot")), type = 5)
               ),

             ##(3) OPLSDA Plot Panel -------------------------------------------
             box(width = 12,
                 inputId = "viewOPLSDA",
                 title = strong("OPLS-DA Plot"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 closable = FALSE,
                 varSelectInput(inputId = ns("OPLSDAGroup"),
                                label = "1. Select metadata (sample groups)",
                                data = ""
                                ),
                 fluidRow(width = 12,
                          column(width = 6,
                                 selectInput(inputId = ns("OPLSDALevel1"),
                                             label = "2.1 Select the first sample group level",
                                             choices = ""
                                             )
                                 ),
                          column(width = 6,
                                 selectInput(inputId = ns("OPLSDALevel2"),
                                             label = "2.2 Select the second sample group level",
                                             choices = ""
                                             )
                                 ),
                          column(width = 6,
                                 selectInput(inputId = ns("OPLSDAColor"),
                                             label = "Color palette",
                                             choices = plot_palette_choices,
                                             selected = "Default"
                                             )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("OPLSDAType"),
                                              label = "Select file type to download",
                                              choices = list("pdf", "tiff", "png"),
                                              selected = "pdf"
                                              )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("OPLSDARatio"),
                                              label = "Aspect ratio of the plot (width:height)",
                                              value = 1,
                                              min = NA,
                                              max = NA
                                              )
                                 ),
                          column(width = 6, downloadButton(outputId = ns("downloadOPLSDA"))),
                          br()
                          ),
                 shinycssloaders::withSpinner(shiny::plotOutput(ns("OPLSDAPlot")), type = 5),
                 plotly::plotlyOutput(ns("SPlot"))
               ),

             ##(4) Heatmap Panel -----------------------------------------------
             box(width = 12,
                 inputId = "viewHeatmap",
                 title = strong("Heatmap"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 closable = FALSE,
                 varSelectInput(inputId = ns("HMGroup"),
                                label = "1. Select metadata (sample groups) for sample coloring",
                                data = ""
                                ),
                 fluidRow(width = 12,
                          column(width = 6,
                                 radioButtons(inputId = ns("HMColCluster"),
                                              label = "Cluster columns (mass features) in heatmap?",
                                              choices = c("Yes" = 1, "No" = 0),
                                              selected = 1
                                              )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("HMRowCluster"),
                                              label = "Cluster rows (samples) in heatmap?",
                                              choices = c("Yes" = 1, "No" = 0),
                                              selected = 1
                                              )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("HMColName"),
                                              label = "Show column names (mass feature names) in heatmap?",
                                              choices = c("Yes" = 1, "No" = 0),
                                              selected = 0
                                              )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("HMRowName"),
                                              label = "Show row names (sample names) in heatmap?",
                                              choices = c("Yes" = 1, "No" = 0),
                                              selected = 1
                                              )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("HMSplitCol"),
                                              label = "Number of column groups (features) you want to split",
                                              value = 1,
                                              min = 1,
                                              step = 1,
                                              )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("HMSplitRow"),
                                              label = "Number of row groups (samples) you want to split",
                                              value = 1,
                                              min = 1,
                                              step = 1,
                                              )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("HMQCFilter"),
                                              label = "Include QC samples (if present)?",
                                              choices = c("Yes" = 1, "No" = 0),
                                              selected = 1
                                              )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("HMType"),
                                              label = "Select file type to download",
                                              choices = list("pdf", "tiff", "png"),
                                              selected = "pdf", inline = T
                                              )
                                 ),
                          column(width = 12,
                                 numericInput(inputId = ns("HMRatio"),
                                              label = "Aspect ratio of the plot (width:height)",
                                              value = 1,
                                              min = NA,
                                              max = NA
                                              )
                                 ),
                          column(width = 6,
                                 br(),
                                 downloadButton(outputId = ns("downloadHM"))
                                 )
                          ),
                 shinycssloaders::withSpinner(shiny::plotOutput(outputId = ns("HMPlot")), type = 5)
               ),

             ##(4) Volcano Plot Panel ------------------------------------------
             box(width = 12,
                 inputId = "viewVolcano",
                 title = strong("Volcano Plot"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 closable = FALSE,
                 fluidRow(width = 12,
                          column(width = 6,
                                 selectInput(inputId = ns("VCLevel1"),
                                             label = "1.1 Select the first sample group level",
                                             choices = ""
                                             )
                                 ),
                          column(width = 6,
                                 selectInput(inputId = ns("VCLevel2"),
                                             label = "1.2 Select the second sample group level",
                                             choices = ""
                                             )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("VCFC"),
                                              label = "2. Input fold change threshold",
                                              value = 2,
                                              min = 1,
                                              max = NA
                                              )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("VCPvalue"),
                                              label = "3. Input p-value threshold",
                                              value = 0.05,
                                              min = 0,
                                              max = 1
                                              )
                                 ),
                          column(width = 6,
                                 radioButtons(inputId = ns("VCType"),
                                              label = "Select file type to download",
                                              choices = list("pdf", "tiff", "png"),
                                              selected = "pdf",
                                              inline = T
                                              )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("VCRatio"),
                                              label = "Aspect ratio of the plot (width:height)",
                                              value = 1,
                                              min = NA,
                                              max = NA
                                              )
                                 ),
                          column(width = 4, downloadButton(outputId = ns("downloadVCPlot")))
                          ),
                 shinycssloaders::withSpinner(plotly::plotlyOutput(ns("VCPlot")), type = 5)
               ),

             ##(5) K-Means Panel -----------------------------------------------
             box(width = 12,
                 inputId = "viewKMeans",
                 title = strong("K-means Cluster Analysis"),
                 status = "success",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 closable = FALSE,
                 fluidRow(width = 12,
                          column(width = 6,
                                 varSelectInput(inputId = ns("KMGroup"),
                                                label = "1. Select metadata (sample groups)",
                                                data = ""
                                                )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("KMCluster"),
                                              label = "2. Input number of clusters",
                                              value = 1,
                                              min = 1,
                                              max = NA
                                              )
                                 )
                          ),
                 fluidRow(width = 12,
                          column(width = 6,
                                 radioButtons(inputId = ns("KMType"),
                                              label = "Select plot type to download",
                                              choices = list("pdf", "tiff", "png"),
                                              selected = "pdf"
                                              )
                                 ),
                          column(width = 6,
                                 numericInput(inputId = ns("KMRatio"),
                                              label = "Aspect ratio of the plot (width:height)",
                                              value = 1,
                                              min = NA,
                                              max = NA
                                              )
                                 )
                          ),
                 fluidRow(width = 12,
                          column(width = 6,
                                 downloadButton(outputId = ns("downloadKMTrend"),
                                                label = "Download Line Plot"
                                                )
                                 ),
                          column(width = 6,
                                 downloadButton(outputId = ns("downloadKMTable"),
                                                label = "Download Table"
                                                )
                                 )
                          ),
                 br(),
                 shinycssloaders::withSpinner(shiny::plotOutput(ns("KMTrendPlot")), type = 5),
                 DT::dataTableOutput(ns("KMTable"))
               ),

             ##(6) Box Plot Panel ----------------------------------------------
             box(
               width = 12,
               inputId = "viewBoxPlot",
               title = strong("Bar/Box/Violin Plot"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 12,
                               varSelectInput(inputId = ns("BPGroup"),
                                              label = "1. Select metadata (sample groups)",
                                              data = ""
                                              )
                               ),
                        column(width = 12,
                               varSelectizeInput(inputId = ns("BPMetabolite"),
                                                 label = "2. Select a metabolite",
                                                 data = ""
                                                 )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("BPTransform"),
                                           label = "3. How to transform the data?",
                                           choices = list("none", "log2", "log10"),
                                           selected = "none"
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("BPPlotType"),
                                           label = "4. Choose plot type",
                                           choices = list("Bar plot", "Box plot", "Violin plot"),
                                           selected = "Bar plot"
                                           )
                               ),
                        column(width = 6,
                               selectInput(inputId = ns("BPPlotColor"),
                                           label = "Color palette",
                                           choices = plot_palette_choices,
                                           selected = "Default"
                                           )
                               ),
                        column(width = 6,
                               radioButtons(inputId = ns("BPType"),
                                            label = "Select plot type to download",
                                            choices = list("pdf", "tiff", "png"),
                                            selected = "pdf"
                                            )
                               ),
                        column(width = 6,
                               numericInput(inputId = ns("BPRatio"),
                                            label = "Aspect ratio of the plot (width:height)",
                                            value = 1,
                                            min = NA,
                                            max = NA
                                            )
                               ),
                        column(width = 6,
                               br(),
                               downloadButton(outputId = ns("downloadBP"),
                                              label = "Download Plot"
                                              )
                               )
                        ),
               shinycssloaders::withSpinner(shiny::plotOutput(ns("BPPlot")), type = 5)
               ),

             ##(7) Correlation analysis Panel ----------------------------------
             box(
               width = 12,
               inputId = "viewCA",
               title = strong("Correlation Analysis"),
               status = "success",
               solidHeader = FALSE,
               collapsible = TRUE,
               collapsed = TRUE,
               closable = FALSE,
               fluidRow(width = 12,
                        column(width = 12,
                               varSelectizeInput(inputId = ns("CAMetabolite"),
                                                 label = "1. Select a metabolite",
                                                 data = ""
                                                 )
                               ),
                        column(width = 12,
                               sliderInput(inputId = ns("CAThreshold"), label = "Select correlation threshold (absolute value)",
                                           min = 0.8,
                                           max = 1,
                                           value = 0.9,
                                           step = 0.001
                                           )
                               ),
                        column(width = 12,
                               downloadButton(outputId = ns("downloadCA"), label = "Download Plot")
                               )
                        ),
               br(),
               p(style = "color:#C70039;", shiny::icon("bell"), strong("Note: ")),
               p(style = "color:#C70039;", "Red indicates positive correlation, and blue indicates negative correlation."),
               shinycssloaders::withSpinner(visNetwork::visNetworkOutput(ns("CAPlot"), height = "800px"), type = 5)
               )
             )

))}


#' 04_viewResult Server Functions
#'
#' @noRd
#' @importFrom ggplot2 aes scale_color_brewer scale_fill_brewer position_dodge
mod_04_viewResult_server <- function(id, sfData){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #(1) Statistics Panel=======================================================
    ##(1) Stat parameters-------------------------------------------------------
    StatGroup <- reactive({
      as.character(input$StatGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "StatGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })

    ##(2) Perform statistics----------------------------------------------------
    analysisResult <- reactive({
      shiny::req(sfData$filter)
      shiny::req(sfData$filterNormTransform)
      shiny::req(sfData$clean)
      shiny::req(sfData$group)
      shiny::validate(need((ncol(sfData$clean) - 1) == nrow(sfData$group), message = "Please wait for data preprocessing to finish before clicking Start button."))

      shiny::withProgress(
        message = "Performing statistical analysis",
        min = 0,
        max = 8,
        value = 0,
        {
          shiny::setProgress(1, detail = "1/8 - Prepare analysis data")
          Group <- sfData$group[, StatGroup()]
          Group <- Group[Group != "QC"]
          groupCount <- nlevels(as.factor(Group))
          equalGroupSizes <- length(unique(as.integer(table(Group)))) == 1L
          if(input$statMethod %in% c("tTest", "ptTest") && groupCount != 2L) {return(NULL)}
          if(input$statMethod %in% c("ptTest", "anovaRM") && !equalGroupSizes) {return(NULL)}
          tFilter <- transformDF(sfData$filter, Group = sfData$group[, StatGroup()])
          tFilterNormTransform <- transformDF(sfData$filterNormTransform, Group = sfData$group[, StatGroup()])
          tClean <- transformDF(sfData$clean, Group = sfData$group[, StatGroup()])

          shiny::setProgress(2, detail = "2/8 - Calculate fold changes")
          statFC <- getFC(tFilter, Group = Group)

          shiny::setProgress(3, detail = "3/8 - Calculate p-values")
          statP <- getP(tFilterNormTransform, Group = Group, Method = input$statMethod)

          shiny::setProgress(4, detail = "4/8 - Adjust p-values")
          statP <- statP %>% dplyr::mutate_all(~ p.adjust(., method = input$pAdjMethod, n = length(.)))

          shiny::setProgress(5, detail = "5/8 - Calculate VIP scores")
          statVIP <- getVIP(tClean, Group = Group)

          shiny::setProgress(6, detail = "6/8 - Perform PCA")
          pcaData <- transformDF(sfData$clean, Group = NULL)
          pcaResult <- stats::prcomp(pcaData, center = FALSE, scale. = FALSE)

          shiny::setProgress(7, detail = "7/8 - Assemble statistical results")
          statTable <- cbind.data.frame(statFC, statP, statVIP)
          rownames(statTable) <- sfData$clean$ID

          filterDF <- subset(sfData$filter, select = -ID)
          names(filterDF) <- paste0("rawArea_", names(filterDF))
          cleanDF <- subset(sfData$clean, select = -ID)
          names(cleanDF) <- paste0("processedAREA_", names(cleanDF))
          combinedTable <- cbind.data.frame(ID = sfData$clean$ID, statTable, filterDF, cleanDF)
          if(isTRUE(as.logical(as.numeric(input$SigFilter)))){
            combinedTable <- combinedTable %>%
              dplyr::filter(if_any(contains("Pvalue"), ~ . < input$SigP))
          }
          rownames(combinedTable) <- NULL

          shiny::setProgress(8, detail = "8/8 - Prepare visualization data")
          heatmapTable <- combinedTable %>%
            dplyr::select(ID, starts_with("rawArea_")) %>%
            dplyr::rename_with(~ gsub("rawArea_", "", .x, fixed = TRUE))
          heatmapData <- transformDF(heatmapTable, Group = NULL)

          list(
            statTable = statTable,
            combinedTable = combinedTable,
            pcaData = pcaData,
            pcaResult = pcaResult,
            heatmapData = heatmapData
          )
        }
      )
    }) %>%
      shiny::bindCache(sfData$filter, sfData$filterNormTransform, sfData$clean, sfData$group, input$statMethod, StatGroup(), input$pAdjMethod, input$SigFilter, input$SigP) %>%
      shiny::bindEvent(input$viewStat)

    statTable <- reactive({
      result <- analysisResult()
      if(is.null(result)) return(NULL)
      result$statTable
    })

    combinedTable <- reactive({
      result <- analysisResult()
      if(is.null(result)) return(NULL)
      result$combinedTable
    })

    ##(2) Show statistical result ==============================================
    output$StatResult <- DT::renderDataTable({
      shiny::validate(
        need(!is.null(sfData$clean), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found"),
        need(!is.null(statTable()), message = "More than 2 groups detected or unequal samples found for paired test; Please choose another statistical method."),
        need(!is.null(combinedTable()), message = "No statistical result")
      )
      DT::datatable(combinedTable(),
                    extensions = c("KeyTable"),
                    options = list(scrollX = TRUE,
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   keys = TRUE
                                   )
                    )
    })

    output$downloadStatTable <- downloadHandler(
      filename = "Stat_table.csv",
      content = safe_download_content(function(file) {
        result <- require_download_result(combinedTable())
        write.csv(result, file, row.names = FALSE)
      })
    )

    #(2) PCA====================================================================
    ##(1) PCA parameters--------------------------------------------------------
    PCAGroup <- reactive({
      as.character(input$PCAGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "PCAGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    PCARatio <- reactive({
      if(input$PCARatio <=0){return(1)}
      input$PCARatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    ### sfData$clean (QC filtered data) is used for PCA
    dataGlobal3PCA <- reactive({
      result <- analysisResult()
      if(is.null(result)) return(NULL)
      result$pcaData
    })

    pcaResult <- reactive({
      result <- analysisResult()
      if(is.null(result)) return(NULL)
      result$pcaResult
    })

    PCAPlot <- reactive({
      shiny::req(dataGlobal3PCA())
      shiny::req(sfData$group)
      shiny::validate(need(nrow(dataGlobal3PCA()) == nrow(sfData$group), message = "Please click Start button to reperform statistics after deleting or recovering samples."))
      p <- showPCA(dataGlobal3PCA(),
                   Group = sfData$group[, PCAGroup()],
                   inx = as.numeric(input$PCAX),
                   iny = as.numeric(input$PCAY),
                   showFrame = input$PCAFrame,
                   palette = input$PCAColor,
                   pca_result = pcaResult(),
                   interactive = FALSE
                   ) +
        ggplot2::theme(text = element_text(size = 16))
      return(p)
    })

    ##(3) Show PCA Result ------------------------------------------------------
    output$PCAPlot <- plotly::renderPlotly({
      shiny::validate(need(!is.null(sfData$clean), message = "Input data not found."))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found."))
      showPCA(
        dataGlobal3PCA(),
        Group = sfData$group[, PCAGroup()],
        inx = as.numeric(input$PCAX),
        iny = as.numeric(input$PCAY),
        showFrame = input$PCAFrame,
        palette = input$PCAColor,
        pca_result = pcaResult(),
        interactive = TRUE
      )
    })

    output$downloadPCA <- downloadHandler(
      filename = function(){paste("PCA", input$PCAType, sep = ".")},
      content = safe_download_content(function(file) {
        plot <- require_download_result(PCAPlot())
        ggplot2::ggsave(file, plot = plot, dpi = 600, width = 20, height = 20 / PCARatio(), units = "cm", device = input$PCAType)
      })
    )

    ##(3) OPLSDA================================================================
    ##(1) OPLSDA parameters-----------------------------------------------------
    OPLSDAGroup <- reactive({
      as.character(input$OPLSDAGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "OPLSDAGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })

    ## need to use input$viewStat to update group levels when deleting or recovering samples.
    observeEvent(c(OPLSDAGroup(), input$viewStat),{
      OPLSDALevels <- sfData$group[, OPLSDAGroup()]
      updateSelectInput(inputId = "OPLSDALevel1",
                        choices = levels(as.factor(OPLSDALevels[!(OPLSDALevels %in% "QC")])),
                        selected = NULL
                        )
      updateSelectInput(inputId = "OPLSDALevel2",
                        choices = rev(levels(as.factor(OPLSDALevels[!(OPLSDALevels %in% "QC")]))),
                        selected = NULL
                        )
    })
    OPLSDARatio <- reactive({
      if(input$PCARatio <= 0){return(1)}
      input$OPLSDARatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    OPLSDA <- reactiveValues(feature = NULL, group = NULL)
    observe({
      shiny::req(dataGlobal3PCA())
      shiny::req(sfData$group)
      shiny::req(nrow(dataGlobal3PCA()) == nrow(sfData$group))
      shiny::validate(need(input$OPLSDALevel1 != input$OPLSDALevel2, message = "Please select two different groups"))
      dfOPLSDA <- cbind.data.frame(dataGlobal3PCA(), Group = sfData$group[, OPLSDAGroup()])
      dfOPLSDA <- dfOPLSDA[dfOPLSDA$Group %in% c(input$OPLSDALevel1, input$OPLSDALevel2), ]
      OPLSDA$feature <- subset(dfOPLSDA, select = -Group)
      OPLSDA$group <- as.factor(dfOPLSDA$Group)
    })

    resultOPLSDA <- reactive({
      shiny::req(OPLSDA$feature)
      shiny::req(OPLSDA$group)
      getOPLSDA(Feature = OPLSDA$feature, Group = OPLSDA$group)
    })

    OPLSDAPlot <- reactive({
      shiny::req(resultOPLSDA())
      shiny::req(OPLSDA$group)
      p <- showOPLSDA(resultOPLSDA(), Group = OPLSDA$group) +
        ggplot2::theme(text = element_text(size = 16))
      if(input$OPLSDAColor == "Default") {
        p <- p
        } else {
        p <- p +
          scale_color_brewer(palette = input$OPLSDAColor) +
          scale_fill_brewer(palette = input$OPLSDAColor)
        }
      return(p)
    })

    SPlot <- reactive({
      shiny::req(resultOPLSDA())
      shiny::req(OPLSDA$feature)
      shiny::req(nrow(dataGlobal3PCA()) == nrow(sfData$group))
      showSplot(OPLSDA$feature, resultOPLSDA()) +
        ggplot2::theme(text = element_text(size = 16))
    })

    ##(3) Show OPLSDA Result ---------------------------------------------------
    output$OPLSDAPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$clean), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      shiny::validate(need(input$OPLSDALevel1 != input$OPLSDALevel2, message = "Please select two different groups"))
      shiny::validate(need(nrow(dataGlobal3PCA()) == nrow(sfData$group), message = "Please click Start button to reperform statistics after deleting or recovering samples."))
      shiny::req(OPLSDAPlot())
      OPLSDAPlot()
    })

    output$SPlot <- plotly::renderPlotly({
      shiny::validate(need(!is.null(sfData$clean), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      shiny::validate(need(input$OPLSDALevel1 != input$OPLSDALevel2, message = "Please select two different groups"))
      shiny::req(nrow(dataGlobal3PCA()) == nrow(sfData$group))
      shiny::req(SPlot())
      showSplot(OPLSDA$feature, resultOPLSDA(), size = 6, interactive = TRUE)
    })

    output$downloadOPLSDA <- downloadHandler(
      filename = function(){paste("OPLSDA_ScorePlot", input$OPLSDAType, sep = ".")},
      content = safe_download_content(function(file) {
        plot <- require_download_result(OPLSDAPlot())
        ggplot2::ggsave(file, plot = plot, dpi = 600, width = 20, height = 20 / OPLSDARatio(), units = "cm", device = input$OPLSDAType)
      })
    )

    #(4) Heat Map===============================================================
    ##(1) Heatmap parameters----------------------------------------------------
    HMGroup <- reactive({
      as.character(input$HMGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "HMGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    HMQCFilter <- reactive({
      as.logical(as.numeric(input$HMQCFilter))
    })
    HMRatio <- reactive({
      if(input$HMRatio <=0){return(1)}
      input$HMRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    ### Non-transformed data: sfData$filter and p-values are needed
    heatmapDF <- reactive({
      result <- analysisResult()
      if(is.null(result)) return(NULL)
      result$heatmapData
    })

    HMPlot <- reactive({
      shiny::req(heatmapDF())
      shiny::req(sfData$group)
      shiny::req(nrow(heatmapDF()) == nrow(sfData$group))
      HMData <- heatmapDF() %>%
        dplyr::mutate(Group = sfData$group[, HMGroup()])
      ## Should QC be filtered?
      if(!HMQCFilter()){
        HMData <- HMData[HMData$Group != "QC", ]
      }
      annoRow <- dplyr::select(HMData, Group)
      rownames(annoRow) <- rownames(HMData)
      group_levels <- unique(as.character(HMData$Group))
      set.seed(1998)
      ComplexHeatmap::pheatmap(scale(dplyr::select(HMData, -Group), center = T, scale = T),
                               name = "ColorBar",
                               color = grDevices::colorRampPalette(c(nature_colors[[4L]], "white", nature_colors[[1L]]))(100),
                               annotation_colors = list(Group = setNames(nature_palette(length(group_levels)), group_levels)),
                               cluster_cols = as.logical(as.numeric(input$HMColCluster)),
                               cluster_rows = as.logical(as.numeric(input$HMRowCluster)),
                               annotation_row = annoRow,
                               show_rownames = as.logical(as.numeric(input$HMRowName)),
                               show_colnames = as.logical(as.numeric(input$HMColName)),
                               column_km = input$HMSplitCol,
                               row_km = input$HMSplitRow
                               )
    })

    ##(3) Show and download plot -----------------------------------------------
    output$HMPlot <- shiny::renderPlot({
      shiny::validate(
        need(!is.null(sfData$filter), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found"),
        need(nrow(heatmapDF()) == nrow(sfData$group),message = "Please click Start button to reperform statistics after deleting or recovering samples.")
      )
      HMPlot()
    })

    output$downloadHM <- downloadHandler(
      filename = function(){paste("Heatmap", input$HMType, sep = ".")},
      content = safe_download_content(function(file) {
        plot <- require_download_result(HMPlot())
        switch(input$HMType,
               "png" = png(file, width = 20, height = 20 / HMRatio(), units = "cm", res = 600),
               "pdf" = pdf(file, width = 20, height = 20 / HMRatio()),
               "tiff" = tiff(file, width = 20, height = 20 / HMRatio(), units = "cm", res = 600)
               )
        on.exit(grDevices::dev.off(), add = TRUE)
        ComplexHeatmap::draw(plot)
      })
    )

    #(3) Volcano Plot ==========================================================
    ##(1) Volcano plot parameters ----------------------------------------------

    ## need to use input$viewStat to update group levels when deleting or recovering samples.
    observeEvent(c(StatGroup(), input$viewStat), {
      VCLevels <- sfData$group[, StatGroup()]
      updateSelectInput(inputId = "VCLevel1",
                        choices = levels(as.factor(VCLevels[!(VCLevels %in% "QC")])),
                        selected = NULL
      )
      updateSelectInput(inputId = "VCLevel2",
                        choices = rev(levels(as.factor(VCLevels[!(VCLevels %in% "QC")]))),
                        selected = NULL
      )
    })
    VCRatio <- reactive({
      if(input$VCRatio <=0){return(1)}
      input$VCRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    VCPlot <- reactive({
      shiny::req(statTable())
      shiny::validate(
        need(input$VCLevel1 != input$VCLevel2, message = "Please select two different groups."),
        need(input$VCPvalue <= 1 & input$VCPvalue > 0, message = "P-value must be greater than 0 and no greater than 1."),
        need(input$VCFC >= 1, message = "Fold change threshold should be at least 1.")
      )
      p <- showVolcano(result = statTable(),
                       FC = input$VCFC,
                       pValue = input$VCPvalue,
                       compare_group = c(input$VCLevel1, input$VCLevel2),
                       interactive = FALSE
                       )
      return(p)
    })

    ##(3) Show and download plot------------------------------------------------
    output$VCPlot <- plotly::renderPlotly({
      shiny::validate(
        need(!is.null(sfData$clean), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found"),
        need(input$VCPvalue <= 1 & input$VCPvalue > 0, message = "P-value must be greater than 0 and no greater than 1."),
        need(input$VCFC >= 1, message = "Fold change threshold should be at least 1."),
        need(nrow(dataGlobal3PCA()) == nrow(sfData$group), message = "Please click Start button to reperform statistics after deleting or recovering samples.")
      )
      showVolcano(
        result = statTable(),
        FC = input$VCFC,
        pValue = input$VCPvalue,
        compare_group = c(input$VCLevel1, input$VCLevel2),
        interactive = TRUE
      )
    })

    output$downloadVCPlot <- downloadHandler(
      filename = function(){paste("VolcanoPlot", input$VCType, sep = ".")},
      content = safe_download_content(function(file) {
        plot <- require_download_result(VCPlot())
        ggplot2::ggsave(file, plot = plot, dpi = 600, width = 20, height = 20 / VCRatio(), units = "cm", device = input$VCType)
      })
    )

    #(4) K-Means================================================================
    ##(1) K-Means plot parameters-----------------------------------------------
    KMGroup <- reactive({
      as.character(input$KMGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "KMGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    KMRatio <- reactive({
      if(input$KMRatio <=0) {return(1)}
      input$KMRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    ###(2.1)prepare k-means data
    KMdata <- reactive({
      shiny::req(heatmapDF())
      shiny::validate(need(nrow(heatmapDF()) == nrow(sfData$group), message = "Please click Start button to reperform statistics after deleting or recovering samples."))
      tem <- prepareKMData(DF = heatmapDF(), Group = sfData$group[, KMGroup()])
      return(tem)
    })

    ###(2.2) calculate k-means clusters
    KMResultCluster <- reactive({
      shiny::req(KMdata())
      shiny::validate(
        need(input$KMCluster > 0, message = "Cluster number should be positive value."),
        need(input$KMCluster <= dim(KMdata())[1], message = "Cluster number should be less than the number of mass features.")
      )
      set.seed(666)
      kmeans(dplyr::select(KMdata(), -Metabolite), centers = input$KMCluster)$cluster
    })

    ###(2.3) KM Table
    KMTable <- reactive({
      shiny::req(KMResultCluster())
      data_with_cust_info <- KMdata() %>%
        dplyr::mutate(clust = paste0("cluster", KMResultCluster()))
    })

    ###(2.4) K-means trend plot
    KMTrendPlot <- reactive({
      showKM(KMTable())
    })

    ##(3) Show and download plot -----------------------------------------------
    output$KMTrendPlot <- shiny::renderPlot({
      shiny::validate(
        need(!is.null(sfData$data), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found"),
        need(input$KMCluster > 0, message = "Cluster number should be positive value."),
        need(input$KMCluster <= dim(KMdata())[1], message = "Cluster number should be less than the number of mass features.")
      )
      KMTrendPlot() + ggplot2::ggtitle("Metabolic Profile Clustering")
    })

    output$KMTable <- DT::renderDataTable({
      shiny::validate(
        need(!is.null(sfData$data), message = "Input data not found"),
        need(!is.null(sfData$group), message = "Meta data not found")
      )
      shiny::req(KMTable())
      DT::datatable(KMTable(),
                    caption = "K-means cluster table:",
                    options = list(scrollX = TRUE,
                                   deferRender = TRUE,
                                   scroller = TRUE,
                                   fixedColumns = FALSE
                                   )
                    )
    })

    output$downloadKMTrend <- downloadHandler(
      filename = function(){paste("KM_Lineplot", input$KMType, sep = ".")},
      content = safe_download_content(function(file) {
        plot <- require_download_result(KMTrendPlot())
        ggplot2::ggsave(file, plot = plot, dpi = 600, width = 20, height = 20 / KMRatio(), units = "cm", device = input$KMType)
      })
    )

    output$downloadKMTable <- downloadHandler(
      filename = "K-means_table.csv",
      content = safe_download_content(function(file) {
        result <- require_download_result(KMTable())
        write.csv(result, file, row.names = FALSE)
      })
    )

    #(5) Box Plot ==============================================================
    ##(1) Parameters -----------------------------------------------------------
    BPGroup <- reactive({
      as.character(input$BPGroup)
    })
    observeEvent(sfData$group, {
      updateVarSelectInput(
        inputId = "BPGroup",
        data = sfData$group,
        selected = "Group1"
      )
    })
    observeEvent(c(heatmapDF(), input$viewStat), {
      updateVarSelectizeInput(
        server = TRUE,
        inputId = "BPMetabolite",
        data = heatmapDF()
      )
    })
    BPTransform <- reactive({
      input$BPTransform
    })
    BPRatio <- reactive({
      if(input$BPRatio <=0){return(1)}
      input$BPRatio
    })

    ##(2) Prepare plot----------------------------------------------------------
    BPPlot <- reactive({
      shiny::req(heatmapDF())
      shiny::req(sfData$group)
      shiny::validate(need(nrow(heatmapDF()) == nrow(sfData$group), message = "Please click Start button to reperform statistics after deleting or recovering samples."))
      showBoxplot(DF = heatmapDF(), Transform = input$BPTransform, Group = sfData$group[, BPGroup()],
                  Metabolite = input$BPMetabolite, colorPalette = input$BPPlotColor, BPPlotType = input$BPPlotType
                  )
    })

    ##(3) Show and download plot------------------------------------------------
    output$BPPlot <- shiny::renderPlot({
      shiny::validate(need(!is.null(sfData$data), message = "Input data not found"))
      shiny::validate(need(!is.null(sfData$group), message = "Meta data not found"))
      BPPlot()
    })

    output$downloadBP <- downloadHandler(
      filename = function(){paste("Box_plot", input$BPType, sep = ".")},
      content = safe_download_content(function(file) {
        plot <- require_download_result(BPPlot())
        ggplot2::ggsave(file, plot = plot, dpi = 600, width = 20, height = 20 / BPRatio(), units = "cm", device = input$BPType)
      })
    )


    #(6) Correlation Analysis ==================================================
    ##(1) parameters------------------------------------------------------------
    observeEvent(input$viewStat,{
      updateVarSelectizeInput(
        server = TRUE,
        inputId = "CAMetabolite",
        data = heatmapDF()
      )
    })

    ##(2) Prepare plot----------------------------------------------------------
    PCC <- reactive({
      shiny::req(heatmapDF())
      shiny::req(sfData$group)
      shiny::validate(need(nrow(heatmapDF()) == nrow(sfData$group), message = "Please click Start button to reperform statistics after deleting or recovering samples."))
      cor(heatmapDF())
    })

    CAPlot <- reactive({
      shiny::req(PCC())
      shiny::validate(need(input$CAThreshold >= 0 & input$CAThreshold < 1, message = "The absolute correlation threshold must be at least 0 and less than 1."))
      showCAplot(PCC = PCC(), Metabolite = as.character(input$CAMetabolite), Threshold = input$CAThreshold)
    })

    output$CAPlot <- visNetwork::renderVisNetwork({
      shiny::req(CAPlot())
      CAPlot()
    })

    output$downloadCA <- downloadHandler(
      filename = function(){
        paste0("Network_", as.character(input$CAMetabolite), ".html")
      },
      content = safe_download_content(function(file) {
        plot <- require_download_result(CAPlot())
        visNetwork::visSave(graph = plot, file = file)
      })
    )

    resultList <- list(
      PCAPlot = PCAPlot,
      HMPlot = HMPlot,
      dataGlobal3PCA = dataGlobal3PCA, # data for PLSDA
      OPLSDAGroup = OPLSDAGroup, # group information for PLSDA plot
      statTable = statTable, # data for volcano plot
      VCGroup = StatGroup, # group information for volcano plot
      combinedTable = combinedTable,
      dataGlobal3Transform = heatmapDF, # data for boxplot
      BPGroup = BPGroup, # group information for boxplot
      BPTransform = BPTransform, # data transformation for boxplot
      KMTrendPlot = KMTrendPlot,
      KMTable = KMTable
      )
    return(resultList)
  })
}
