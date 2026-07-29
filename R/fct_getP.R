#' @title get p-values
#' @description get p-values from different statistical tests
#' @param x sample ion intensity matrix, row is sample, column is feature.
#' @param Group sample group information.
#' @param Method the method for p-value calculation.
#' @importFrom stats as.formula TukeyHSD formula terms terms.formula t.test pairwise.t.test
#' @return a data frame with p-values
#' @noRd
#' @examples
#' dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#' myGroup <- rep_len(LETTERS[1:3], 300)
#' out1 <- getP(dat, Group = myGroup, Method = "anovaHSD")

#(1) Combined function----------------------------------------------------------
getP <- function(x, Group = NULL, Method = "anovaHSD"){
  pValues <- switch (Method,
                     "tTest" = tTest(x, Group, paired = FALSE),
                     "ptTest" = tTest(x, Group, paired = TRUE),
                     "anovaHSD" = anovaHSD(x, Group),
                     "anovaRM" = anovaRM(x, Group)
                     )
  return(pValues)
}

#(2) unpaired and paired T-Test-------------------------------------------------
tTest <- function(x, Group = NULL, paired = FALSE){
  cat("\n- Calculating p-values...\n")
  ##(1) check input
  x <- as.data.frame(x)
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(levels(Group)) != 2){stop("Two sample groups are required for T-test")}
  if(length(Group) != nrow(x)){stop("Missing group informaiton detected")}

  ##(2) T-test
  f <- as.factor(Group)
  j <- combn(levels(f), 2)
  out <- as.data.frame(sapply(x, function(x) t.test(x ~ Group, paired = paired, var.equal = FALSE, alternative = "two.sided")$p.value))
  colnames(out) <- paste0("AdjPvalue_", j[1,], "_vs_", j[2,])
  cat(": P-value calculation done!\n")
  return(out)
}

#(3) ANOVA with Tukey HSD-------------------------------------------------------
anovaHSD <- function(x, Group = NULL){
  cat("\n- Calculating p-values...\n")
  ##(1) check input
  x <- as.data.frame(x)
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(levels(Group)) <= 1){stop("At least two sample groups should be included")}
  if(length(Group) != nrow(x)){stop("Missing group informaiton detected")}

  ##(2) ANOVA with Tukey HSD
  response_names <- names(x)
  form <- as.formula(sprintf("cbind(%s) ~ Group", toString(response_names)))
  fit <- do.call("aov", list(formula = form, data = quote(x)))
  aov_hack <- fit
  aov_hack[c("coefficients", "fitted.values")] <- NULL
  aov_hack[c("contrasts", "xlevels")] <- NULL
  attr(aov_hack$model, "terms") <- NULL
  class(aov_hack) <- c("aov", "lm")
  ## post hoc
  N <- length(response_names)
  result <- vector("list", N)
  for (i in 1:N) {
    aov_hack$call[[2]][[2]] <- as.name(response_names[i])
    aov_hack$residuals <- as.data.frame(fit$residuals)[, i]
    aov_hack$effects <- as.data.frame(fit$effects)[, i]
    old_tm <- terms(fit)
    old_tm[[2]] <- as.name(response_names[i])
    new_tm <- terms.formula(formula(old_tm))
    aov_hack$terms <- new_tm
    aov_hack$model[1] <- data.frame(fit$model[[1]][, i])
    names(aov_hack$model)[1] <- response_names[i]
    result[[i]] <- TukeyHSD(aov_hack)$Group[, 4]
    }
  col_num <- as.numeric(summary(result)[1])
  output <- data.frame(matrix(unlist(result), ncol = col_num, byrow = TRUE))
  f <- as.factor(Group)
  j <- combn(levels(f), 2)
  colnames(output) <- paste0("AdjPvalue_", j[1,], "_vs_", j[2,])
  cat(": P-value calculation done!\n")
  return(output)
  }

#(4) Pair-wise T-Test-----------------------------------------------------------
anovaRM <- function(x, Group){
  cat("\n- Calculating p-values...\n")
  ##(1) suppress no visible binding for global variable notes
  .<- NULL
  rowname <- NULL
  name <- NULL

  ##(2) check input
  x <- as.data.frame(x)
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(Group) != nrow(x)){stop("Missing group informaiton detected")}

  ##(3) pair-wise t-Test
  N <- ncol(x)
  result <- vector("list", N)
  for(i in 1:N){
    PTT <- pairwise.t.test(x = x[, i], g = Group, paired = TRUE, alternative = "two.sided", p.adjust.method = "bonferroni")
    tem <- PTT$p.value %>%
      as.data.frame %>%
      dplyr::mutate(rowname = rownames(.)) %>%
      tidyr::pivot_longer(-rowname) %>%
      dplyr::filter(rowname != name)
    pNames <- paste0("AdjPvalue_", tem$rowname, "_vs_", tem$name)
    pValues = t(as.data.frame(tem$value))
    colnames(pValues) <- pNames
    rownames(pValues) <- NULL
    result[[i]] <- pValues
    }
  cat(": P-value calculation done!\n")
  return(as.data.frame(Reduce(rbind, result)))
}
