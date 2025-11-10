#' Summarize binary variable
#'
#' Computes a string with the count and percentage of `1`s.
#' @param x A binary (`0` or `1`) numeric or logical vector.
#' @param digits.fixed Number of decimal places for the percentage (and the
#' weighted count if `weights` are used.). Default = `0`.
#' @param weights Optional numeric vector of observation weights.
#' @return A string of the form `"count (percentage%)"`.
#' @export
AS.summary.binary <- function(x, digits.fixed = 0, weights = NULL) {
  if (is.null(weights)) {
    n <- sum(x)
    p <- mean(x)
    return(paste0(AS.fixdec(n, 0), " (", AS.fixdec(100 * p, digits.fixed), "%)"))
  } else {
    n <- sum(weights * x)
    p <- n / sum(weights)
    return(paste0(AS.fixdec(n, digits.fixed), " (", AS.fixdec(100 * p, digits.fixed), "%)"))
  }
}

#' Summarize count variable
#'
#' Computes a string with the count and mean count per observation.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places for the mean count per
#' observation. Default = `2`.
#' @return A string of the form `"count (mean count per observation)"`.
#' @export
AS.summary.count <- function(x, digits.fixed = 2) {
  return(paste0(sum(x), " (", AS.fixdec(mean(x), digits.fixed), ")"))
}

#' Summarize time-of-day variable in "HH:MM" format
#'
#' Computes a string with the mean and standard deviation in the form
#' `"circular mean ± SD"`.
#' @param x A string vector of times-of-day in "HH:MM" format.
#' @return A string of the form `"circular mean ± SD"`.
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
#' Computes a string with the Kaplan–Meier median survival time and a 95%
#' confidence interval using the Brookmeyer–Crowley¹ method with Greenwood's
#' variance² and a complementary log–log transformation³.
#' @param time Follow-up times.
#' @param status Event indicator (`1` = event, `0` = censored).
#' @param digits.fixed Number of decimal places for the summary. Default = `2`.
#' @return A string of the form `"median (lower to upper)"` or `"NR"` if the
#' median is not reached.
#' @references
#' 1. Brookmeyer, R. and Crowley, J., 1982. A confidence interval for the median
#' survival time. *Biometrics*, pp. 29–41.
#' 2. Greenwood, M., 1926. A report on the natural duration of cancer. In:
#' *Reports on Public Health and Medical Subjects*, 33, pp. 1–26. London: Her
#' Majesty’s Stationery Office, Ministry of Health.
#' 3. Klein, J.P., Logan, B., Harhoff, M. and Andersen, P.K., 2007. Analyzing
#' survival curves at a fixed point in time. *Statistics in Medicine*, 26(24),
#' pp. 4505–4519.
#' @export
AS.summary.KM <- function(time, status, digits.fixed = 2) {
  KM <- summary(survival::survfit(survival::Surv(time, status) ~ 1,
                                  conf.type = "log-log"))$table
  output <- paste0(AS.fixdec(KM[7], digits.fixed), " (",
                   AS.fixdec(KM[8], digits.fixed), " to ",
                   AS.fixdec(KM[9], digits.fixed), ")")
  output <- gsub("NA", "NR", output, fixed = TRUE)
  return(output)
}

#' Summarize continuous variable
#'
#' Computes a string with the mean and standard deviation in the form
#' `"mean ± SD"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = `2`.
#' @param weights Optional numeric vector of observation weights.
#' @return A string of the form `"mean ± SD"`.
#' @export
AS.summary.linear <- function(x, digits.fixed = 2, weights = NULL) {
  if (is.null(weights)) {
    m <- mean(x)
    s <- stats::sd(x)
  } else {
    m <- sum(weights * x) / sum(weights)
    s <- sqrt(sum(weights * (x - m)^2) / sum(weights))
  }
  output <- paste0(AS.fixdec(m, digits.fixed), " \u00b1 ", AS.fixdec(s, digits.fixed))
  output <- gsub("-", "\u2212", output)
  return(output)
}

#' Summarize log1p-transformed continuous variable
#'
#' Computes a string with the back-transformed mean and standard deviation from
#' the log(x + 1) scale in the form `"back-transformed mean ± SD"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = `2`.
#' @return A string of the form `"back-transformed mean ± SD"`.
#' @export
AS.summary.log1plinear <- function(x, digits.fixed = 2) {
  output <- paste0(AS.fixdec(exp(mean(log1p(x))) - 1, digits.fixed), " \u00b1 ", AS.fixdec(exp(stats::sd(log1p(x))), digits.fixed))
  output <- gsub("-", "\u2212", output)
  return(output)
}

#' Summarize log-transformed continuous variable
#'
#' Computes a string with the geometric mean and standard deviation in the form
#' `"geometric mean ± SD"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = `2`.
#' @return A string of the form `"geometric mean ± SD"`.
#' @export
AS.summary.loglinear <- function(x, digits.fixed = 2) {
  output <- paste0(AS.fixdec(exp(mean(log(x))), digits.fixed), " \u00b1 ", AS.fixdec(exp(stats::sd(log(x))), digits.fixed))
  output <- gsub("-", "\u2212", output)
  return(output)
}
