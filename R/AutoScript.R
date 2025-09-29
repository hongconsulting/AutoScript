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

#' Summarize binary variable
#'
#' Computes a string with the count and percentage of `1`s.
#' @param x A binary (0/1) numeric or logical vector.
#' @param digits.fixed Number of decimal places for the percentage. Default = 2.
#' @return A string of the form `"count (percent%)"`.
#' @export
AS.summary.binary <- function(x, digits.fixed = 2) {
  return(paste0(sum(x), " (", AS.fixdec(100 * mean(x), digits.fixed - 1), "%)"))
}

#' Summarize time-of-day variable in "HH:MM" format.
#'
#' Computes a string with the mean and standard deviation in the form
#' `"`circular mean \eqn{\pm} circular SD`"`.
#' @param x A string vector of times-of-day in "HH:MM" format.
#' @return A string of the form `"`mean \eqn{\pm} SD`"`.
#' @export
AS.summary.HHMM <- function(x) {
  output <- paste0(HHMM::HHMM.mean(x), " \u00b1 ", HHMM::HHMM.sd(x))
  output <- gsub("-", "\u2212", output)
  return(output)
}

#' Summarize Kaplan\eqn{-}Meier time-to-event outcome
#'
#'Computes a string with the Kaplan\eqn{-}Meier median survival time and a 95%
#'confidence interval based on Greenwood's variance with a log transformation.
#' @param time Follow-up times.
#' @param status Event indicator (1 = event, 0 = censored).
#' @param digits.fixed Number of decimal places for the summary. Default = 2.
#' @return A string of the form `"median (lower to upper)"`, with `"NR"` if not reached.
#' @export
AS.summary.KM <- function(time, status, digits.fixed = 2) {
  KM <- summary(survival::survfit(survival::Surv(time, status) ~ 1))$table
  output <- paste0(AS.fixdec(KM[7], digits.fixed), " (",
                   AS.fixdec(KM[8], digits.fixed), " to ",
                   AS.fixdec(KM[9], digits.fixed), ")")
  output <- gsub("NA", "NR", output, fixed = TRUE)
  return(output)
}

#' Summarize continuous variable
#'
#' Computes a string with the mean and standard deviation in the form
#' `"`mean \eqn{\pm} SD`"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = 2.
#' @return A string of the form `"`mean \eqn{\pm} SD`"`.
#' @export
AS.summary.linear <- function(x, digits.fixed = 2) {
  output <- paste0(AS.fixdec(mean(x), digits.fixed), " \u00b1 ", AS.fixdec(stats::sd(x), digits.fixed))
  output <- gsub("-", "\u2212", output)
  return(output)
}

#' Summarize log-transformed continuous variable
#'
#' Computes a string with the geometric mean and geometric standard deviation in the form
#' `"`geometric mean \eqn{\pm} geometric SD`"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = 2.
#' @return A string of the form `"`geometric mean \eqn{\pm} geometric SD`"`.
#' @export
AS.summary.loglinear <- function(x, digits.fixed = 2) {
  output <- paste0(AS.fixdec(exp(mean(log(x))), digits.fixed), " \u00b1 ", AS.fixdec(exp(stats::sd(log(x))), digits.fixed))
  output <- gsub("-", "\u2212", output)
  return(output)
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
