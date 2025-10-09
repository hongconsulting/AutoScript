#' Summarize binary variable
#'
#' Computes a string with the count and percentage of `1`s.
#' @param x A binary (0 or 1) numeric or logical vector.
#' @param digits.fixed Number of decimal places for the percentage. Default = 2.
#' @return A string of the form `"count (percent%)"`.
#' @export
AS.summary.binary <- function(x, digits.fixed = 0) {
  return(paste0(sum(x), " (", AS.fixdec(100 * mean(x), digits.fixed), "%)"))
}

#' Summarize count variable
#'
#' Computes a string with the count and mean count per observation.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places for the mean count per observation.
#' Default = 2.
#' @return A string of the form `"count (mean count per observation)"`.
#' @export
AS.summary.count <- function(x, digits.fixed = 2) {
  return(paste0(sum(x), " (", AS.fixdec(mean(x), digits.fixed), ")"))
}

#' Summarize time-of-day variable in "HH:MM" format
#'
#' Computes a string with the mean and standard deviation in the form
#' `"`circular mean ± circular SD`"`.
#' @param x A string vector of times-of-day in "HH:MM" format.
#' @return A string of the form `"`mean ± SD`"`.
#' @export
AS.summary.HHMM <- function(x) {
  if (!requireNamespace("WHcircular", quietly = TRUE)) {stop("[AS.summary.HHMM] requires package 'WHcircular'")}
  output <- paste0(WHcircular::HHMM.mean(x), " \u00b1 ",
                   WHcircular::WH_rad_to_dHHMM(WHcircular::WH_rad_sd(WHcircular::WH_HHMM_to_rad(x))))
  output <- gsub("-", "\u2212", output)
  return(output)
}

#' Summarize Kaplan–Meier time-to-event outcome
#'
#'Computes a string with the Kaplan–Meier median survival time and a 95%
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
#' `"`mean ± SD`"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = 2.
#' @return A string of the form `"`mean ± SD`"`.
#' @export
AS.summary.linear <- function(x, digits.fixed = 2) {
  output <- paste0(AS.fixdec(mean(x), digits.fixed), " \u00b1 ", AS.fixdec(stats::sd(x), digits.fixed))
  output <- gsub("-", "\u2212", output)
  return(output)
}

#' Summarize log-transformed continuous variable
#'
#' Computes a string with the geometric mean and geometric standard deviation in the form
#' `"`geometric mean ± geometric SD`"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = 2.
#' @return A string of the form `"`geometric mean ± geometric SD`"`.
#' @export
AS.summary.loglinear <- function(x, digits.fixed = 2) {
  output <- paste0(AS.fixdec(exp(mean(log(x))), digits.fixed), " \u00b1 ", AS.fixdec(exp(stats::sd(log(x))), digits.fixed))
  output <- gsub("-", "\u2212", output)
  return(output)
}
