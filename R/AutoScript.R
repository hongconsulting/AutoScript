#' @importFrom stats anova binomial glm qnorm rnorm sd
#' @importFrom survival survfit
#' @importFrom utils write.csv

library(survival)

AS.contains <- function(x, pattern) {
  grepl(pattern, x)
}

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

#' Summarize continuous variable
#'
#' Computes a string with the mean and standard deviation in the form
#' `"`mean \eqn{\pm} sd`"`.
#' @param x A numeric vector.
#' @param digits.fixed Number of decimal places. Default = 2.
#' @return A string of the form `"`mean \eqn{\pm} sd`"`.
#' @export
AS.summary.linear <- function(x, digits.fixed = 2) {
  output <- paste0(AS.fixdec(mean(x), digits.fixed), " \u00b1 ", AS.fixdec(sd(x), digits.fixed))
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
  KM <- summary(survfit(Surv(time, status) ~ 1))$table
  output <- paste0(AS.fixdec(KM[7], digits.fixed), " (",
                   AS.fixdec(KM[8], digits.fixed), " to ",
                   AS.fixdec(KM[9], digits.fixed), ")")
  output <- gsub("NA", "NR", output, fixed = TRUE)
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
  write.csv(data, con, row.names = FALSE)
  close(con)
}

################################################################################

#' Baseline characteristics table functions
#'
#' @param basetable A list created by \code{AS.basetable.create}, containing the group
#'   assignments and current table structure.
#' @param subset.mask Optional logical vector used to restrict
#'   the analysis to a subset of observations.
#' @param p.values Logical value indicating whether to display p-values in the added row.
#' @param digits.fixed Number of decimal places for percentages in summaries.
#'   Default = 2.
#' @param digits.sig Number of significant figures for p-values. Default = 2.
#' @param sig.thresh Threshold below which p-values are displayed as
#'   \code{"< threshold"}. Default = 0.001.
#' @section Value:
#' An updated baseline table list with a new row summarizing the added variable.
#' The table itself can be accessed with \code{$table}.
#' @name AS.basetable
NULL

#' Baseline characteristics table functions
#'
#' See Details.
#' @param group Integer vector of group assignments, coded as 0, 1, or 2.
#' @param name String vector of group names. Default = \code{c("Group 0", "Group 1", "Group 2")}.
#' @return A list with two elements:
#' \itemize{
#'   \item \code{group}: an integer vector of group assignments
#'   \item \code{table}: a character matrix containing column headers and
#'     sample sizes
#' }
#' @template AS.basetable_common
#' @export
AS.basetable.create <- function(group, name = c("Group 0", "Group 1", "Group 2")) {
  output <- list()
  output$group <- group
  if (setequal(unique(group), 0)) {
    output$table <- matrix("", nrow = 2, ncol = 2)
    output$table[1, 1] <- "Name"
    output$table[1, 2] <- "Total"
    output$table[2, 2] <- paste0("n = ", length(group))
  } else if (setequal(unique(group), 0:1)) {
    output$table <- matrix("", nrow = 2, ncol = 5)
    output$table[1, 1] <- "Name"
    output$table[1, 2] <- "Total"
    output$table[1, 3] <- name[1]
    output$table[1, 4] <- name[2]
    output$table[1, 5] <- "p"
    output$table[2, 2] <- paste0("n = ", length(group))
    output$table[2, 3] <- paste0("n = ", sum(group == 0))
    output$table[2, 4] <- paste0("n = ", sum(group == 1))
  } else if (setequal(unique(group), 0:2)) {
    output$table <- matrix("", nrow = 2, ncol = 12)
    output$table[1, 1] <- "Name"
    output$table[1, 2] <- "Total"
    output$table[1, 3] <- name[1]
    output$table[1, 4] <- name[2]
    output$table[1, 5] <- name[3]
    output$table[1, 6] <- "p (0 vs 1)"
    output$table[1, 7] <- "p (0 vs 2)"
    output$table[1, 8] <- "p (1 vs 2)"
    output$table[1, 9] <- "p (0 vs 12)"
    output$table[1, 10] <- "p (1 vs 02)"
    output$table[1, 11] <- "p (2 vs 01)"
    output$table[1, 12] <- "p (LR)"
    output$table[2, 2] <- paste0("n = ", length(group))
    output$table[2, 3] <- paste0("n = ", sum(group == 0))
    output$table[2, 4] <- paste0("n = ", sum(group == 1))
    output$table[2, 5] <- paste0("n = ", sum(group == 2))
  } else {stop("[AS.basetable.create] must have 1, 2, or 3 groups")}
  return(output)
}

################################################################################

#' Baseline characteristics table functions
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param outcome Binary vector (0/1 or logical) of outcome values.
#' @inheritParams AS.basetable
#' @inheritSection AS.basetable Value
#' @template AS.basetable_common
#' @export
AS.basetable.binary <- function(name, outcome, basetable, subset.mask = NULL, p.values = T,
                                digits.fixed = 2, digits.sig = 2, sig.thresh = 0.001) {
  X <- basetable$group
  y <- outcome
  if (!is.null(subset.mask)) {
    X <- X[subset.mask]
    y <- outcome[subset.mask]
  }
  if (length(y) != length(X)) {stop("[AS.basetable.binary] inconsistent length")}
  if (any(is.na(y))) {stop("[AS.basetable.binary] outcome contains NA, use subset.mask")}
  output <- basetable
  output$table <- rbind(output$table, rep("", ncol(output$table)))
  r <- nrow(output$table)
  if (max(basetable$group) == 0) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.binary(y, digits.fixed)
  } else if (max(basetable$group) == 1) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.binary(y, digits.fixed)
    output$table[r, 3] <- AS.summary.binary(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.binary(y[X == 1], digits.fixed)
    if (p.values) {
      fit <- glm(y ~ as.factor(X), family = binomial)
      output$table[r, 5] <- AS.signif(summary(fit)$coefficients[2, 4], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 2) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.binary(y, digits.fixed)
    output$table[r, 3] <- AS.summary.binary(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.binary(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.binary(y[X == 2], digits.fixed)
    if (p.values) {
      # 0 vs 1, 0 vs 2, 1 vs 2
      fit0 <- glm(y ~ as.factor(X), family = binomial)
      fit1 <- glm(y ~ relevel(as.factor(X), ref = "1"), family = binomial)
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[3, 4], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[3, 4], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- glm(y ~ X != 0, family = binomial)
      fit1x <- glm(y ~ X != 1, family = binomial)
      fit2x <- glm(y ~ X != 2, family = binomial)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[2, 4], digits.sig, sig.thresh)
      # LR
      fitnull <- glm(y ~ 1, family = binomial)
      output$table[r, 12] <- AS.signif(anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.binary] must have 1, 2, or 3 groups")}
  return(output)
}

#' Baseline characteristics table functions
#'
#' See Details.
#' @param text A string giving the custom text to display in the first column.
#' @param basetable A list created by \code{AS.basetable.create}, containing the group
#'   assignments and current table structure.
#' @template AS.basetable_common
#' @export
AS.basetable.blank <- function(text, basetable) {
  X <- basetable$group
  output <- basetable
  output$table <- rbind(output$table, rep("", ncol(output$table)))
  r <- nrow(output$table)
  output$table[r, 1] <- text
  return(output)
}

#' Baseline characteristics table functions
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param outcome Numeric vector of continuous outcome values.
#' @inheritParams AS.basetable
#' @inheritSection AS.basetable Value
#' @template AS.basetable_common
#' @export
AS.basetable.linear <- function(name, outcome, basetable, subset.mask = NULL, p.values = T,
                                digits.fixed = 2, digits.sig = 2, sig.thresh = 0.001) {
  X <- basetable$group
  y <- outcome
  if (!is.null(subset.mask)) {
    X <- X[subset.mask]
    y <- y[subset.mask]
  }
  if (length(y) != length(X)) {stop("[AS.basetable.linear] inconsistent length")}
  if (any(is.na(y))) {stop("[AS.basetable.linear] outcome contains NA, use subset.mask")}
  output <- basetable
  output$table <- rbind(output$table, rep("", ncol(output$table)))
  r <- nrow(output$table)
  if (max(basetable$group) == 0) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.linear(y, digits.fixed)
  } else if (max(basetable$group) == 1) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.linear(y, digits.fixed)
    output$table[r, 3] <- AS.summary.linear(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.linear(y[X == 1], digits.fixed)
    if (p.values) {
      fit <- glm(y ~ as.factor(X))
      output$table[r, 5] <- AS.signif(summary(fit)$coefficients[2, 4], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 2) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.linear(y, digits.fixed)
    output$table[r, 3] <- AS.summary.linear(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.linear(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.linear(y[X == 2], digits.fixed)
    if (p.values) {
      # 0 vs 1, 0 vs 2, 1 vs 2
      fit0 <- glm(y ~ as.factor(X))
      fit1 <- glm(y ~ relevel(as.factor(X), ref = "1"))
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[3, 4], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[3, 4], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- glm(y ~ X != 0)
      fit1x <- glm(y ~ X != 1)
      fit2x <- glm(y ~ X != 2)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[2, 4], digits.sig, sig.thresh)
      # LR
      fitnull <- glm(outcome ~ 1)
      output$table[r, 12] <- AS.signif(anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.linear] must have 1, 2, or 3 groups")}
  return(output)
}

#' Baseline characteristics table functions
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param time Numeric vector of follow-up times.
#' @param status Binary vector indicating event occurrence (1 = event, 0 = censored).
#' @inheritParams AS.basetable
#' @inheritSection AS.basetable Value
#' @template AS.basetable_common
#' @export
AS.basetable.TTE <- function(name, time, status, basetable, subset.mask = NULL, p.values = T,
                             digits.fixed = 2, digits.sig = 2, sig.thresh = 0.001) {
  X <- basetable$group
  if (!is.null(subset.mask)) {
    X <- X[subset.mask]
    time <- time[subset.mask]
    status <- status[subset.mask]
  }
  if (length(time) != length(X)) {stop("[AS.basetable.TTE] inconsistent length")}
  if (length(status) != length(X)) {stop("[AS.basetable.TTE] inconsistent length")}
  if (any(is.nan(time))) {stop("[AS.basetable.TTE] time contains NA, use subset.mask")}
  if (any(is.nan(status))) {stop("[AS.basetable.TTE] status contains NA, use subset.mask")}
  output <- basetable
  output$table <- rbind(output$table, rep("", ncol(output$table)))
  r <- nrow(output$table)
  if (max(basetable$group) == 0) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.KM(time, status, digits.fixed)
  } else if (max(basetable$group) == 1) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.KM(time, status, digits.fixed)
    output$table[r, 3] <- AS.summary.KM(time[X == 0], status[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.KM(time[X == 1], status[X == 1], digits.fixed)
    if (p.values) {
      fit <- coxph(Surv(time, status) ~ as.factor(X))
      output$table[r, 5] <- AS.signif(summary(fit)$coefficients[5], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 2) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.KM(time, status, digits.fixed)
    output$table[r, 3] <- AS.summary.KM(time[X == 0], status[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.KM(time[X == 1], status[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.KM(time[X == 2], status[X == 2], digits.fixed)
    if (p.values) {
      # 0 vs 1, 0 vs 2, 1 vs 2
      fit0 <- coxph(Surv(time, status) ~ as.factor(X))
      fit1 <- coxph(Surv(time, status) ~ relevel(as.factor(X), ref = "1"))
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[1, 5], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[2, 5], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[2, 5], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- coxph(Surv(time, status) ~ X != 0)
      fit1x <- coxph(Surv(time, status) ~ X != 1)
      fit2x <- coxph(Surv(time, status) ~ X != 2)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[5], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[5], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[5], digits.sig, sig.thresh)
      # LR
      fitnull <- coxph(Surv(time, status) ~ 1)
      output$table[r, 12] <- AS.signif(anova(fitnull, fit0, test = "LRT")[2, 4], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.TTE] must have 1, 2, or 3 groups")}
  return(output)
}

################################################################################

#' Format regression results into manuscript-ready tables
#'
#' @param fit A fitted object of class \code{coxph}, \code{glm} (not yet implemented),
#' or \code{lm} (not yet implemented).
#' @param name Optional string vector of variable names.
#' @return A character matrix representing a manuscript-ready table.
#' @examples
#' library(AutoScript)
#' library(survival)
#' data <- survival::veteran
#' fit <- coxph(Surv(data$time, data$status) ~ as.factor(data$trt))
#' table2 <- AS.format(fit, name = "Treatment")
#' print(table2)
#' @export
AS.format <- function(fit, name = NULL) {
  if (inherits(fit, "coxph")) {
    summ <- summary(fit)$coefficients
    if (is.null(name)) name <- rownames(summary(fit)$coefficients)
    beta <- summ[, 1]
    SE <- summ[, 3]
    p <- summ[, 5]
    LL <- beta - qnorm(0.975) * SE
    UL <- beta + qnorm(0.975) * SE
    output <- matrix("", ncol = 3, nrow = length(beta) + 1)
    output[1, 2] <- "HR (95%CI)"
    output[1, 3] <- "p"
    for (i in 1:length(beta)) {
      output[1 + i, 1] <- name[i]
      output[1 + i, 2] <- paste0(AS.fixdec(exp(beta[i])), " (", AS.fixdec(exp(LL[i])),  " to ", AS.fixdec(exp(UL[i])), ")")
      output[1 + i, 3] <- AS.signif(p[i])
    }
  } else {stop(paste0("[AS.format] class ", class(fit)," not implemented"))}
  return(output)
}

AS.test <- function(n.group = 2, n = 100) {
  group <- sample(0:(n.group - 1), n, T)
  outcome1 <- sample(0:1, n, T)
  outcome2 <- rnorm(n)
  test <- AS.basetable.create(group)
  test <- AS.basetable.binary("Outcome 1", outcome1, test)
  test <- AS.basetable.linear("Outcome 2", outcome2, test)
  return(test)
}
