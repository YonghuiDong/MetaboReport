#' @title Calculate OPLSDA
#' @description calcualte OPLSDA
#' @param feature feature data frame.
#' @param group sample groups.
#' @return opls result.
#' @noRd
#' @examples
#' library(ropls)
#' data(sacurine)
#' attach(sacurine)
#' sacurine.plsda <- getOPLSDA(dataMatrix, sampleMetadata[, "gender"])

getOPLSDA <- function(Feature, Group){
  ropls::opls(x = Feature,
              y = Group,
              log10L = FALSE,
              scaleC = "none",
              predI = 1,
              permI = 20,
              orthoI = 1, # see bug #25
              crossvalI = min(length(Group), 7),
              fig.pdfC = "none",
              info.txtC = "none")
}

