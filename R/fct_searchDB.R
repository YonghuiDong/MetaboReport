#' @title Search in customized database
#' @description search in customized database based on accurate m/z and RT
#' @author Yonghui Dong
#' @param DF input file, should contain at least a column named mz
#' @param DB database, should contain at least a column named mz
#' @param ppm mass tolerance, default 5ppm
#' @param RT retention time tolerance, default 0.2min
#' @param useRT should RT be considered during database search?
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @noRd
#' @examples
#' DF <- cbind.data.frame(mz = c(100.001, 100.1), RT = c(10, 11))
#' DB <- cbind.data.frame(mz = c(100.001, 100.1), RT = c(10, 12.1))
#' searchDB(DF, DB, ppm = 5, RT = 0.2, useRT = TRUE)

searchDB <- function(DF, DB, ppm = 5, RT = 0.2, useRT = FALSE){

  #(1) input check
  ## rename DF and DB
  names(DF)[toupper(names(DF)) == "MZ"] <- "Sample.mz"
  names(DF)[toupper(names(DF)) == "RT"] <- "Sample.RT"
  names(DB)[toupper(names(DB)) == "MZ"] <- "DB.mz"
  names(DB)[toupper(names(DB)) == "RT"] <- "DB.RT"
  if(!'Sample.mz' %in% colnames(DF)) {stop("Column 'mz' is not found in your data")}
  if(!'DB.mz' %in% colnames(DB)) {stop("Column 'mz' is not found in your database")}
  if(!is.numeric(DF$Sample.mz) || !is.numeric(DB$DB.mz)) {stop("m/z columns must be numeric")}
  if(!is.numeric(ppm) || length(ppm) != 1L || !is.finite(ppm) || ppm < 0 || ppm >= 1e6) {
    stop("ppm must be a finite value between 0 and 1,000,000.")
  }
  if(isTRUE(useRT) && (!'Sample.RT' %in% colnames(DF) || !'DB.RT' %in% colnames(DB))) {
    stop("Column 'rt' is not found in your data or database")
  }
  if(isTRUE(useRT) && (!is.numeric(DF$Sample.RT) || !is.numeric(DB$DB.RT))) {
    stop("RT columns must be numeric")
  }
  if(isTRUE(useRT) && (!is.numeric(RT) || length(RT) != 1L || !is.finite(RT) || RT < 0)) {
    stop("RT tolerance must be a finite non-negative value.")
  }

  #(2) sort once, then search only the matching mass range
  valid_db <- which(is.finite(DB$DB.mz) & DB$DB.mz > 0)
  db_order <- valid_db[order(DB$DB.mz[valid_db])]
  sorted_mz <- DB$DB.mz[db_order]
  tolerance <- ppm / 1e6
  Result <- vector("list", nrow(DF))

  for (i in seq_len(nrow(DF))) {
    sample_mz <- DF$Sample.mz[i]
    if(!is.finite(sample_mz) || sample_mz <= 0 || !length(sorted_mz)) next
    lower <- sample_mz / (1 + tolerance)
    upper <- sample_mz / (1 - tolerance)
    first <- findInterval(lower, sorted_mz, left.open = TRUE) + 1L
    last <- findInterval(upper, sorted_mz)
    if(first > last) next
    matches <- DB[sort(db_order[first:last]), , drop = FALSE]

    if(isTRUE(useRT)) {
      if(!is.finite(DF$Sample.RT[i])) next
      keep_rt <- is.finite(matches$DB.RT) & abs(matches$DB.RT - DF$Sample.RT[i]) < RT
      matches <- matches[keep_rt, , drop = FALSE]
    }
    if(!nrow(matches)) next

    query_row <- DF[i, , drop = FALSE]
    row.names(query_row) <- row.names(matches) <- NULL
    ppms <- (matches$DB.mz - sample_mz) * 1e6 / matches$DB.mz
    Result[[i]] <- cbind.data.frame(QuerryID = i, query_row, matches,
                                    ppm = round(ppms, 2))
    if(isTRUE(useRT)) Result[[i]]$RT_dif <- round(matches$DB.RT - DF$Sample.RT[i], 2)
    row.names(Result[[i]]) <- NULL
  }

  #(3) format and return result
  Result <- Filter(Negate(is.null), Result)
  if(!length(Result)) return(data.frame())
  do.call(rbind.data.frame, Result)
}
