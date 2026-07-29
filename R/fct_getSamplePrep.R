#' @title Get sample preparation method
#' @description Get sample preparation method.
#' @param Method sample preparation method
#' @noRd
#' @example
#' myMethod <- getSamplePrep(Method = "None")

getSamplePrep <- function(Method = "None") {
  # method details
  M0 = ""
  M1 = "This is Method1"
  M2 = "This is Method2"
  M3 = "This is Method3"

  # select method
  txt <- switch(Method,
                "None" = M0,
                "Method 1" = M1,
                "Method 2" = M2,
                "Method 3" = M3
                )
  return(txt)
}
