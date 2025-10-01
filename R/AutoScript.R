# AS.contains <- function(x, pattern) {
#   grepl(pattern, x)
# }

#' Fixed decimal places
#'
#' Converts a numeric value to a string with a fixed number of
#' decimal places, including trailing zeros. Returns `"NaN"` if input is `NA`.
#' @param x A numeric value.
#' @param digits Number of decimal places. Default = 2.
#' @return A string representation of `x`.
#' @export
AS.fixdec <- function(x, digits = 2) {
  if (is.na(x)) return("NaN")
  output <- formatC(x, format = "f", digits = max(0, digits), flag = "#")
  return(sub("\\.$", "", output))
}

#' Row-wise maxima
#'
#' Computes the maximum value of each row in a matrix or data frame,
#' ignoring `NA`s. Returns `NA` if an entire row is missing.
#' @param input A numeric matrix or data frame.
#' @return A numeric vector of row-wise maxima.
#' @export
AS.rowmax <- function(input) {
  return(apply(input, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else max(x)
  }))
}

#' Significant figures
#'
#' Converts a numeric value to a string with the specified number of
#' significant figures, including trailing zeros. Values smaller than
#' `threshold` are displayed as `"< threshold"`.
#' @param x A numeric value.
#' @param digits Number of significant figures. Default = 2.
#' @param threshold Lower bound below which values are displayed as
#'   `"< threshold"`. Default = 0.001.
#' @return A string representation of `x`.
#' @export
AS.signif <- function(x, digits = 2, threshold = 0.001) {
  if (x < threshold) {return(paste0("< ", toString(threshold)))}
  output <- formatC(signif(x, digits), format = "fg", digits = digits, flag = "#")
  output[x == 0] <- paste0("0.", strrep("0", digits - 1)) # 0.00
  return( sub("\\.$", "", output)) # 0.
}

#' Survival outcomes
#'
#' Computes event times and statuses from start, event, and review dates.
#' @param startdate Vector of start dates.
#' @param eventdate Vector of event dates.
#' @param reviewdate Vector of last review dates for censored cases.
#' @param divisor Unit conversion factor for time. Default = 365.2425/12.
#' @return A numeric matrix with two columns: follow-up time and status (1 = event, 0 = censored).
#' @export
AS.survoutcome <- function(startdate, eventdate, reviewdate, divisor = 365.2425/12) {
  n <- length(startdate)
  output <- matrix(NA, n, 2)
  for (i in seq_len(n)) {
    if (!is.na(startdate[i])) {
      if (!is.na(eventdate[i])) {
        output[i, 1] <- (eventdate[i] - startdate[i]) / divisor
        output[i, 2] <- 1
      } else if (!is.na(reviewdate[i])) {
        output[i, 1] <- (reviewdate[i] - startdate[i]) / divisor
        output[i, 2] <- 0
      }
    }
  }
  return(output)
}

#' Write CSV with UTF-8 BOM
#'
#' Writes a CSV file that includes a UTF-8 byte order mark to
#' prevent encoding issues when opening in Microsoft Excel on Windows.
#' @param data A data frame or matrix to be written.
#' @param path File path for the output CSV file.
#' @export
AS.write.csv <- function(data, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  writeLines("\uFEFF", con)
  utils::write.csv(data, con, row.names = FALSE)
  close(con)
}
