#' Format a metabolomics feature table
#'
#' Converts supported input tables to the internal feature-by-sample format.
#'
#' @param DF Raw feature table.
#' @param metaGroup Optional metadata table. Its first column contains sample
#'   identifiers and the remaining columns define sample groups.
#' @param format Either `"CD"` for Compound Discoverer output or `"Other"`.
#'
#' @return A data frame whose first column is `ID` and remaining columns are
#'   sample intensities.
#' @noRd
formatData <- function(DF, metaGroup = NULL, format = c("CD", "Other")) {
  format <- match.arg(format)
  data <- as.data.frame(DF, check.names = FALSE)

  if (!"Name" %in% names(data)) {
    data$Name <- "Unknown"
  }

  prefix <- if (identical(format, "CD")) "Area:" else "Sample_"
  sample_columns <- grep(prefix, names(data), value = TRUE, fixed = TRUE)
  if (!length(sample_columns)) {
    stop("No sample intensity columns were found for format '", format, "'.")
  }

  result <- data.frame(
    ID = paste0("ID", seq_len(nrow(data)), "_", data$Name),
    data[sample_columns],
    check.names = FALSE
  )

  sample_names <- sample_columns
  if (identical(format, "CD")) {
    sample_names <- sub("Area: ", "", sample_names, fixed = TRUE)
    sample_names <- sub("Sample_", "", sample_names, fixed = TRUE)
    sample_names <- sub(".raw.*", "", sample_names)
  } else {
    sample_names <- sub("Sample_", "", sample_names, fixed = TRUE)
  }
  names(result) <- c("ID", sample_names)

  if (!is.null(metaGroup)) {
    metadata <- as.data.frame(metaGroup, check.names = FALSE)
    if (ncol(metadata) < 2L) {
      stop("Metadata must contain a sample column and at least one group column.")
    }
    group_names <- do.call(paste, c(metadata[-1L], sep = "_"))
    group_names <- paste0(group_names, "_", seq_along(group_names))
    if (length(group_names) != length(sample_columns)) {
      stop("Sample number and metadata row count are not equal.")
    }
    names(result) <- c("ID", group_names)
  }

  result
}
