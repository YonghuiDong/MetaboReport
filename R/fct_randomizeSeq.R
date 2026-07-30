#' @title Randomize LCMS sample injection sequence
#' @description Block randomize LCMS sample injection sequence and insert Blanks and QCs.
#' @param df a dataframe used to describe the experiment design.
#' @param nBlank number of Blanks to be added at the beginning of the injection sequence
#' @param nQC frequency of QC to be inserted in the sequence, i.e., every nth QC will be inserted.
#' @param nWithinBlank number of Blanks after the QC within the sample.
#' @param nEmptyCell number of cells to be reserved in the sample plate.
#' @param plateRow number of rows of the sample plate.
#' @param plateCol number of columns of the sample plate.
#' @param plateIDType sample plate ID type: by number or by letter?
#' @param outputType the output data format. Currently support Orbitrap and Waters format.
#' @importFrom dplyr %>% bind_rows row_number
#' @return a dataframe which can be imported to LCMS instrument to build injection sequence.
#' @noRd
#' @examples
#' df <- data.frame(Group = rep(c("A", "B", "C"), each = 20), Rep = rep(1:6, times = 10))
#' result <- randomizeSeq(df, nBlank = 0, nQC = 6, nWithinBlank = 0, nEmptyCell = 2)

randomizeSeq <- function(df, nBlank = 0, nQC = 0, nWithinBlank = 0, nEmptyCell = 0, plateRow = 6, plateCol = 8, plateIDType = "Letter", outputType = "Orbitrap") {
  if(nQC < 0 || nBlank < 0 || nWithinBlank < 0 || nEmptyCell < 0) stop("Sequence counts must be non-negative")
  colnames(df) <- c("Group", "Rep")
  ## Best block is designed when the replicates are not the same among different groups
  bestBlock <- df %>%
    dplyr::count(Group) %>%
    dplyr::summarize(Fre = names(which.max(table(n)))) %>%
    as.numeric()

  df2 <- df %>%
    dplyr::mutate(Group = as.factor(Group)) %>%
    dplyr::mutate(Sample = paste(Group, Rep, sep = "_")) %>%
    dplyr::mutate(bin = as.numeric(Group)) %>%
    dplyr::group_by(bin) %>%
    dplyr::mutate(Block = rep(1:bestBlock, length.out = n())) %>%
    dplyr::group_by(Block) %>%
    dplyr::sample_n(n()) %>%
    dplyr::select(-bin, -Rep) %>%
    dplyr::ungroup()

  ## add QC
  if(nQC != 0){
    new_row <- data.frame(Group = c("QC", rep("Blank", nWithinBlank)), Sample = c("QC", rep("Blank", nWithinBlank)))

    blocks <- split(df2, (seq_len(nrow(df2)) - 1L) %/% nQC)
    df2 <- dplyr::bind_rows(lapply(blocks, function(block) dplyr::bind_rows(block, new_row))) %>%
      dplyr::group_by(Sample) %>%
      dplyr::mutate(Sample = ifelse(Sample == "QC", paste0("QC_", (row_number()+1)), Sample),
                    Sample = ifelse(Sample == "Blank", paste0("Blank_", (row_number() + nBlank)), Sample)) %>%
      dplyr::ungroup() %>%
      dplyr::add_row(Group = "QC", Sample = "QC_1", .before = 1) # add QC1 before samples
  }

  ## add Black
  if(nBlank != 0){
    df2 <- df2 %>%
      dplyr::add_row(Group = rep("Blank", nBlank), Sample = paste0("Blank_", 1:nBlank), .before = 1)
  }

  ## add sample plate position
  if(outputType == "Orbitrap"){
    combinations <- paste0(rep(LETTERS[1:26], each = plateCol), 1:plateCol)[1: (plateRow * plateCol)]
  }
  if(outputType == "Waters"){
    combinations <- paste0(rep(LETTERS[1:26], each = plateCol), ",", 1:plateCol)[1: (plateRow * plateCol)]
  }

  ## add sequence position
  if(plateIDType == "Letter"){repeated_ID = rep(LETTERS[1:26], length.out = nrow(df2))}
  if(plateIDType == "Number"){repeated_ID = rep(1:26, length.out = nrow(df2))}
  nPlate <- ceiling(nrow(df2)/(plateRow * plateCol)) # determine no. of max plate
  finalSeq <- paste(rep(repeated_ID[1:nPlate], each = length(combinations)), combinations, sep = ":") # get total sequence position
  positionHolder <- max(
    (as.numeric(nQC > 0) + as.numeric(nBlank > 0) + nEmptyCell),
    (as.numeric(nQC > 0) + as.numeric(nWithinBlank > 0) + nEmptyCell)
    ) # determine the existence of QC and Blank
  finalSeq2 <- finalSeq
  if(positionHolder > 0){finalSeq2 <- finalSeq[-(1:positionHolder)]}

  ## assign sequence position
  df2 <- df2 %>%
    dplyr::mutate(Position = rep(NA, times = nrow(df2)))

  if(nBlank ==0 & nWithinBlank == 0){
    df2 <- df2 %>%
      dplyr::mutate(Position = ifelse(Group == "QC", finalSeq[1], Position))
  } else{
    df2 <- df2 %>%
      dplyr::mutate(Position = ifelse(Group == "Blank", finalSeq[1], Position)) %>%
      dplyr::mutate(Position = ifelse(Group == "QC", finalSeq[2], Position))
  }

  suppressWarnings(df2$Position[is.na(df2$Position)] <- finalSeq2)

  return(df2)
}

#' Format an instrument injection sequence
#'
#' @param randomized A data frame returned by [randomizeSeq()].
#' @param output_type Either `"Orbitrap"` or `"Waters"`.
#' @noRd
format_injection_sequence <- function(randomized, output_type) {
  row_count <- nrow(randomized)

  if (identical(output_type, "Orbitrap")) {
    result <- as.data.frame(
      setNames(
        replicate(21L, character(row_count), simplify = FALSE),
        c(
          "Sample Type", "File Name", "Sample ID", "Path",
          "Instrument Method", "Process Method", "Calibration File",
          "Position", "Inj Vol", "Level", "Sample Wt", "Sample Vol",
          "ISTD Amt", "Dil Factor", "L1 Study", "L2 Client",
          "L3 Laboratory", "L4 Company", "L5 Phone", "Comment",
          "Sample Name"
        )
      ),
      check.names = FALSE
    )
    result$`Sample Type` <- "Unknown"
    result$`File Name` <- randomized$Sample
    result$Position <- randomized$Position
    return(result)
  }

  if (identical(output_type, "Waters")) {
    result <- as.data.frame(
      setNames(
        replicate(7L, character(row_count), simplify = FALSE),
        c(
          "File Name", "File Text", "MS File", "MS Tune File",
          "Inlet File", "Bottle", "Injection Volume"
        )
      ),
      check.names = FALSE
    )
    result$`File Name` <- randomized$Sample
    result$Bottle <- randomized$Position
    return(result)
  }

  stop("Unsupported output type: ", output_type)
}
