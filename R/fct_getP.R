#' @title Calculate p-values
#' @description Calculate feature-wise p-values using the selected test.
#' @param x sample ion intensity matrix, row is sample, column is feature.
#' @param Group sample group information.
#' @param Method the method for p-value calculation.
#' @return a data frame with p-values
#' @noRd
#' @examples
#' dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#' myGroup <- rep_len(LETTERS[1:3], 300)
#' out1 <- getP(dat, Group = myGroup, Method = "anovaHSD")

# (1) Combined function --------------------------------------------------------
getP <- function(x, Group = NULL, Method = "anovaHSD") {
  switch(
    Method,
    tTest = tTest(x, Group, paired = FALSE),
    ptTest = tTest(x, Group, paired = TRUE),
    anovaHSD = anovaHSD(x, Group),
    anovaRM = anovaRM(x, Group),
    stop("Unknown p-value method: ", Method)
  )
}

# (2) Column-wise t-test -------------------------------------------------------
column_t_test <- function(first, second, paired = FALSE) {
  if (paired) {
    if (nrow(first) != nrow(second)) stop("Paired groups must have equal sizes")
    difference <- first - second
    estimate <- colMeans(difference)
    sample_count <- nrow(difference)
    variance <- colSums(
      (difference - rep(estimate, each = sample_count))^2
    ) / (sample_count - 1L)
    standard_error <- sqrt(variance / sample_count)
    degrees_freedom <- sample_count - 1L
  } else {
    first_count <- nrow(first)
    second_count <- nrow(second)
    first_mean <- colMeans(first)
    second_mean <- colMeans(second)
    first_variance <- colSums(
      (first - rep(first_mean, each = first_count))^2
    ) / (first_count - 1L)
    second_variance <- colSums(
      (second - rep(second_mean, each = second_count))^2
    ) / (second_count - 1L)
    first_term <- first_variance / first_count
    second_term <- second_variance / second_count
    estimate <- first_mean - second_mean
    standard_error <- sqrt(first_term + second_term)
    degrees_freedom <- (first_term + second_term)^2 /
      (first_term^2 / (first_count - 1L) +
         second_term^2 / (second_count - 1L))
  }

  p_value <- 2 * stats::pt(
    abs(estimate / standard_error),
    degrees_freedom,
    lower.tail = FALSE
  )
  zero_error <- standard_error == 0
  p_value[zero_error & estimate == 0] <- 1
  p_value[zero_error & estimate != 0] <- 0
  p_value
}

# (3) Unpaired and paired t-test ----------------------------------------------
tTest <- function(x, Group = NULL, paired = FALSE) {
  cat("\n- Calculating p-values...\n")

  if (is.null(Group)) stop("Please include group information")
  x <- as.matrix(x)
  Group <- droplevels(factor(Group))
  if (nlevels(Group) != 2L) stop("Two sample groups are required for t-test")
  if (length(Group) != nrow(x)) stop("Missing group information detected")
  if (any(table(Group) < 2L)) stop("At least two samples per group are required")

  groups <- levels(Group)
  p_value <- column_t_test(
    x[Group == groups[[1L]], , drop = FALSE],
    x[Group == groups[[2L]], , drop = FALSE],
    paired = paired
  )
  output <- data.frame(p_value, check.names = FALSE)
  names(output) <- paste0("AdjPvalue_", groups[[1L]], "_vs_", groups[[2L]])
  cat(": P-value calculation done!\n")
  output
}

# (4) ANOVA with Tukey HSD -----------------------------------------------------
anovaHSD <- function(x, Group = NULL) {
  cat("\n- Calculating p-values...\n")

  if (is.null(Group)) stop("Please include group information")
  x <- as.matrix(x)
  Group <- droplevels(factor(Group))
  if (nlevels(Group) <= 1L) stop("At least two sample groups should be included")
  if (length(Group) != nrow(x)) stop("Missing group information detected")

  groups <- levels(Group)
  group_count <- length(groups)
  group_sizes <- tabulate(Group, nbins = group_count)
  group_means <- rowsum(x, as.integer(Group), reorder = TRUE) / group_sizes
  degrees_freedom <- nrow(x) - group_count
  if (degrees_freedom <= 0L) stop("Not enough samples for ANOVA")

  residuals <- x - group_means[as.integer(Group), , drop = FALSE]
  mean_square_error <- colSums(residuals^2) / degrees_freedom
  pairs <- utils::combn(seq_len(group_count), 2L)
  output <- vapply(seq_len(ncol(pairs)), function(index) {
    first <- pairs[1L, index]
    second <- pairs[2L, index]
    standard_error <- sqrt(
      (mean_square_error / 2) *
        (1 / group_sizes[[first]] + 1 / group_sizes[[second]])
    )
    difference <- group_means[second, ] - group_means[first, ]
    p_value <- stats::ptukey(
      abs(difference) / standard_error,
      group_count,
      degrees_freedom,
      lower.tail = FALSE
    )
    p_value[standard_error == 0 & difference == 0] <- 1
    p_value[standard_error == 0 & difference != 0] <- 0
    p_value
  }, numeric(ncol(x)))
  output <- matrix(output, nrow = ncol(x), ncol = ncol(pairs))
  colnames(output) <- paste0(
    "AdjPvalue_", groups[pairs[1L, ]], "_vs_", groups[pairs[2L, ]]
  )

  cat(": P-value calculation done!\n")
  as.data.frame(output)
}

# (5) Pairwise paired t-tests --------------------------------------------------
anovaRM <- function(x, Group = NULL) {
  cat("\n- Calculating p-values...\n")

  if (is.null(Group)) stop("Please include group information")
  x <- as.matrix(x)
  Group <- droplevels(factor(Group))
  if (nlevels(Group) <= 1L) stop("At least two sample groups should be included")
  if (length(Group) != nrow(x)) stop("Missing group information detected")
  if (length(unique(table(Group))) != 1L) stop("Paired groups must have equal sizes")
  if (any(table(Group) < 2L)) stop("At least two samples per group are required")

  groups <- levels(Group)
  pairs <- utils::combn(seq_along(groups), 2L)
  output <- vapply(seq_len(ncol(pairs)), function(index) {
    column_t_test(
      x[Group == groups[pairs[1L, index]], , drop = FALSE],
      x[Group == groups[pairs[2L, index]], , drop = FALSE],
      paired = TRUE
    )
  }, numeric(ncol(x)))
  output <- matrix(output, nrow = ncol(x), ncol = ncol(pairs))
  output <- pmin(output * ncol(pairs), 1)
  colnames(output) <- paste0(
    "AdjPvalue_", groups[pairs[2L, ]], "_vs_", groups[pairs[1L, ]]
  )

  cat(": P-value calculation done!\n")
  as.data.frame(output)
}
