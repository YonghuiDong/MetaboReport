#' @title calculate VIP values
#' @description calculate VIP values using among different samples groups using OPLSDA.
#' @param x sample ion intensity matrix, row sample, column feature.
#' @param Group sample group information
#' @importFrom utils combn
#' @importFrom ropls opls
#' @return a dataframe with vip values
#' @noRd
#' @examples
#' dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#' myGroup <- rep_len(LETTERS[1:3], 300)
#' out <- getVIP(dat, Group = myGroup)

getVIP <- function(x, Group = NULL){
  cat("\n- Calculating VIP values...\n")
  #(1) check input
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(levels(Group)) <= 1){stop("At least two sample groups should be included")}
  if(length(Group) != nrow(x)){stop("Missing group informaiton detected")}

  #(2) perform OPLS
  n <- levels(Group)
  dat2 <- cbind.data.frame(x, Group = Group)
  mylist <- combn(n, 2, FUN = function(x) subset(dat2, Group %in% x), simplify = FALSE)
  ## change mylist names
  listnames <- combn(n, 2, simplify = FALSE)
  names(mylist) <- lapply(listnames, function(x) paste("VIP", paste(x, collapse="_vs_"), sep = "_"))
  ## perform OPLS-DA using opls package
  oplsdafun <- function(i) {
    myVIP <- tryCatch({
      ropls::opls(x = subset(i, select = -Group),
                  y = as.character(i$Group),
                  log10L = FALSE,
                  scaleC = "none",
                  predI = 1,
                  permI = 20,
                  orthoI = 1, ## see bug #25
                  crossvalI = min(nrow(i), 7),
                  fig.pdfC = "none",
                  info.txtC = "none"
                  )
      },
    # For the following error:
    # Error: No model was built because the first predictive component was already not significant;
    # Select a number of predictive components of 1 if you want the algorithm to compute a model despite this.
    error = function(e){
      ropls::opls(x = subset(i, select = -Group),
                  y = as.character(i$Group),
                  log10L = FALSE,
                  scaleC = "none",
                  predI = 1,
                  permI = 20,
                  orthoI = 1,
                  crossvalI = min(nrow(i), 7),
                  fig.pdfC = "none",
                  info.txtC = "none"
                  )
      }
    )
    round(myVIP@vipVn, 2)
  }
  list_VIP <- lapply(mylist, oplsdafun)
  cat(": VIP value calculation done!\n")
  ## convert result to dataframe and keep the name
  return(data.frame(list_VIP))
}
