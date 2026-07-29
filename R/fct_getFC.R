#' @title Calculate fold change
#' @description Calculate fold change among different samples.
#' @param x sample ion intensity matrix, row sample, column feature.
#' @param Group sample group information
#' @importFrom utils combn
#' @return A data frame with pairwise fold changes.
#' @noRd
#' @examples
#' dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#' myGroup <- rep_len(LETTERS[1:2], 300)
#' ret <- getFC(dat, Group = myGroup)

getFC <- function(x, Group = NULL) {
  cat("\n- Calculating fold changes...\n")

  if (is.null(Group)) stop("Please include group information")
  x <- as.matrix(x)
  Group <- droplevels(factor(Group))
  if (nlevels(Group) <= 1L) stop("At least two sample groups should be included")
  if (length(Group) != nrow(x)) stop("Missing group information detected")

  groups <- levels(Group)
  group_sizes <- tabulate(Group, nbins = length(groups))
  group_means <- rowsum(x, as.integer(Group), reorder = TRUE) / group_sizes
  pairs <- utils::combn(seq_along(groups), 2L)
  forward <- group_means[pairs[1L, ], , drop = FALSE] /
    group_means[pairs[2L, ], , drop = FALSE]
  reverse <- group_means[pairs[2L, ], , drop = FALSE] /
    group_means[pairs[1L, ], , drop = FALSE]
  fold_change <- rbind(forward, reverse)
  fold_change[is.nan(fold_change)] <- 0
  rownames(fold_change) <- c(
    paste0("Fold_", groups[pairs[1L, ]], "_vs_", groups[pairs[2L, ]]),
    paste0("Fold_", groups[pairs[2L, ]], "_vs_", groups[pairs[1L, ]])
  )

  cat(": FC calculation done!\n")
  round(as.data.frame(t(fold_change)), 4)
}
