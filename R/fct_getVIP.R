#' @title Calculate pairwise VIP values
#' @description Calculate pairwise VIP scores using OPLS-DA.
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

getVIP <- function(x, Group = NULL) {
  cat("\n- Calculating VIP values...\n")

  if (is.null(Group)) stop("Please include group information")
  Group <- droplevels(factor(Group))
  if (nlevels(Group) <= 1L) stop("At least two sample groups should be included")
  if (length(Group) != nrow(x)) stop("Missing group information detected")

  x <- as.matrix(x)
  pairs <- utils::combn(levels(Group), 2L, simplify = FALSE)
  list_VIP <- lapply(pairs, function(pair) {
    keep <- Group %in% pair
    model <- ropls::opls(
      x = x[keep, , drop = FALSE],
      y = as.character(Group[keep]),
      log10L = FALSE,
      scaleC = "none",
      predI = 1,
      permI = 0,
      orthoI = 1,
      crossvalI = min(sum(keep), 7L),
      fig.pdfC = "none",
      info.txtC = "none"
    )
    round(model@vipVn, 2)
  })
  names(list_VIP) <- vapply(
    pairs,
    function(pair) paste0("VIP_", paste(pair, collapse = "_vs_")),
    character(1)
  )

  cat(": VIP value calculation done!\n")
  data.frame(list_VIP, check.names = FALSE)
}
