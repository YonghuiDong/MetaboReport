#' @title Calculate mz values
#' @description Calculate mz values based on given formula
#' @param formula mass formula
#' @param adduct adduct ions
#' @param z charge
#' @param mode positive or negative
#' @importFrom MSbox mz mass
#' @return a dataframe, containing calculated mz values in each column
#' @noRd
#' @examples
#' formula = c("C7H6O4", "CH4")
#' adducts = c("H", "Na", "K")
#' getMZ(formula, adducts, z = 1)

getMZ <- function(formula = NULL, adducts = NULL, z = NULL, mode = "positive"){
  #(1) check input
  if(is.null(formula)){stop("Please input formula")}

  #(2) prepare dataframe
  n <- ifelse(is.null(adducts), 1, length(adducts))
  m <- length(formula)
  tem <- data.frame(matrix(ncol = n, nrow = m))
  if(is.null(adducts))(colnames(tem) = "Radical Ion")

  #(3) for positive ion mode
  if(mode == "positive"){
    if(is.null(adducts)){
      tem$`Radical Ion` <- MSbox::mz(m = formula, z = z, caseSensitive = TRUE)
      } else{
        for(i in 1:n){
          tem[, i] <- MSbox::mz(m = paste0(formula, adducts[i]), z = z, caseSensitive = TRUE)
        }
        colnames(tem) <- paste0("M+", adducts)
      }
  }

  #(4) for negative ion mode
  if(mode == "negative"){
    if(is.null(adducts)){
      tem$`Radical Ion` <- MSbox::mz(m = formula, z = z, caseSensitive = TRUE)
    } else{
      adducts[adducts == "H"] = "" ## replace "H" in negative ion mode with "", otherwise "H" will be calculated 2 times
      for(i in 1:n){
        tem[, i] <- MSbox::mz(m = paste0(formula, adducts[i]), z = z, caseSensitive = TRUE) - MSbox::mass("H1")
      }
      colnames(tem) <- paste0("M-H+", adducts)
      colnames(tem)[colnames(tem) == "M-H+"] = "M-H" # format the name for "H"
    }
  }

  #(5) output result
  return(data.frame(Formula = formula, tem, check.names = FALSE))
  }
