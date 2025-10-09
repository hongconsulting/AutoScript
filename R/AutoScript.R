# AS.contains <- function(x, pattern) {
#   grepl(pattern, x)
# }

#' Dispersion estimate
#'
#' Computes the ratio of residual deviance to residual degrees of freedom,
#' used to assess over-dispersion.
#' @param fit A fitted \code{glm} object.
#' @return Numeric value of the dispersion estimate.
#' @export
AS.dispersion <- function(fit) {
  return(fit$deviance/fit$df.residual)
}

AS.dmy.to.dmY <- function(input, century, pivot) {
  output <- input
  s <- strsplit(input, "/", fixed = TRUE)
  for (i in 1:length(s)) {
    if (length(s[[i]]) == 3) {
      if (nchar(s[[i]][3]) == 2) {
        y <- s[[i]][3]
        if (as.numeric(y) <= pivot) {
          s[[i]][3] <- paste0(century, y)
        } else {
          s[[i]][3] <- paste0(century - 1, y)
        }
      }
      output[i] <- paste0(s[[i]], collapse = "/")
    }
  }
  return(output)
}

#' Convert mixed dates to Microsoft Excel serial dates
#'
#' Converts string dates that may be in serial (using the Microsoft Excel offset),
#' "d/m/y" (e.g., "01/01/00"), or "d/m/Y" format (e.g., "01/01/2000") into numeric
#' serial dates. Two-digit years are expanded using a specified century and pivot year.
#' @param input String vector of dates in serial, "d/m/y" or "d/m/Y" format.
#' @param century Numeric century (e.g., 20) used for expanding two-digit years.
#' @param pivot Numeric threshold where two-digit years > \code{pivot} are expanded
#' with \code{century - 1}.
#' @return Numeric vector of serial dates using the Microsoft Excel offset.
#' @examples
#' print(AS.dmyY.to.Excel(c("01/01/00", "01/01/2000", "36526"), 20, 25))
#' @export
AS.dmyY.to.Excel <- function(input, century, pivot) {
  output <- input
  x <- AS.dmy.to.dmY(input, century, pivot)
  mask_n <- suppressWarnings(as.numeric(x))
  mask_d <- as.numeric(as.Date(x, "%d/%m/%Y")) - as.numeric(as.Date("1899-12-30"))
  output[!is.na(mask_n)] <- mask_n[!is.na(mask_n)]
  output[!is.na(mask_d)] <- mask_d[!is.na(mask_d)]
  return(suppressWarnings(as.numeric(output)))
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
#'
#' An example use case in survival analysis is determining the date of last
#' follow-up from several dates when the patient was observed.
#' @param input A numeric matrix or data frame.
#' @return A numeric vector of row-wise maxima.
#' @export
AS.rowmax <- function(input) {
  return(apply(input, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else max(x)
  }))
}

#' Row-wise minima
#'
#' Computes the minimum value of each row in a matrix or data frame,
#' ignoring `NA`s. Returns `NA` if an entire row is missing.
#'
#' An example use case in survival analysis is determining the event date for
#' progression-free survival based on the dates of progression and death.
#' @param input A numeric matrix or data frame.
#' @return A numeric vector of row-wise minima.
#' @export
AS.rowmin <- function (input) {
  return(apply(input, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA else min(x)
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

#' Convert string variable to binary variable
#'
#' Converts a string vector to a numeric vector according to the following:
#' 1. Elements exactly matching any value in \code{missing} return \code{NA}.
#' 2. Otherwise, elements matching any fixed pattern in \code{pattern} return 1.
#' 3. All other elements return 0.
#' @param input String vector to evaluate.
#' @param pattern String vector of fixed patterns to each within \code{input}.
#' @param missing String vector of exact values to treat as \code{NA}.
#' @return Numeric vector of 1 (pattern matched), 0 (no match), or \code{NA} (missing).
#' @examples
#' print(AS.string.to.binary(c("Yes", "Yes*", "No", "Missing"), "Yes", "Missing"))
#' @export
AS.string.to.binary <- function(input, pattern, missing) {
  output <- rep(NA, length(input))
  for (i in 1:length(input)) {
    if (input[i] %in% missing) {
      output[i] <- NA
    } else if (any(grepl(pattern, input[i], fixed = TRUE))) {
      output[i] <- 1
    } else {
      output[i] <- 0
    }
  }
  return(output)
}


#' Survival outcomes
#'
#' Computes event times and statuses from start, event, and review dates.
#' @param date_start Vector of start dates.
#' @param date_event Vector of event dates.
#' @param date_follow Vector of last follow-up dates for censored cases.
#' @param divisor Unit conversion factor for time. Default = 365.2425/12.
#' @return A numeric matrix with two columns: survival time and status
#' (1 = event, 0 = censored). Cases with missing start dates or with survival
#' time ≤ 0 are returned as \code{NA} in both columns.
#' @export
AS.survoutcome <- function(date_start, date_event, date_follow, divisor = 365.2425/12) {
  n <- length(date_start)
  output <- matrix(NA, n, 2)
  for (i in 1:n) {
    if (!is.na(date_start[i])) {
      if (!is.na(date_event[i])) {
        output[i, 1] <- (date_event[i] - date_start[i])/divisor
        output[i, 2] <- 1
        if (output[i, 1] <= 0) {
          output[i, 1] <- NA
          output[i, 2] <- NA
        }
      }
      else if (!is.na(date_follow[i])) {
        output[i, 1] <- (date_follow[i] - date_start[i])/divisor
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

#' Count regression
#'
#' Fits a Poisson and a negative binomial regression model, returning the model
#' with the lower Akaike information criterion¹.
#' @param formula Formula for the Poisson model, as for the \code{glm()} function.
#' @param data Optional data frame containing model variables.
#' @return A fitted object of class \code{glm} ± \code{negbin}.
#' @details Wrapper function for \code{stats::glm()} and \code{MASS:glm.nb()}.
#' @references
#' 1. Akaike, H., 1974. A new look at the statistical model identification.
#' \emph{IEEE Transactions on Automatic Control}, 19(6), pp. 716–723.
#' @examples
#' library(MASS)
#' data <- MASS::epil
#' fit <- glm.count(y ~ trt, data = data)
#' print(class(fit))
#' print(AS.format(fit, name = c("(Intercept)", "Treatment")))
#' @export
glm.count <- function(formula, data = NULL) {
  if (!requireNamespace("MASS", quietly = TRUE)) stop("[glm.count.AIC] requires package 'MASS'")
  fit0 <- stats::glm(formula, data = data, family = stats::poisson)
  fit1 <- MASS::glm.nb(formula, data = data)
  if (stats::AIC(fit0) <= stats::AIC(fit1)) return(fit0)
  return(fit1)
}

# #' Count regression
# #'
# #' Fits a Poisson regression model and falls back to a negative binomial
# #' regression model if the estimated dispersion exceeds a specified threshold.
# #' @param formula Model formula, as for the \code{glm()} function.
# #' @param thresh Numeric threshold for the dispersion estimate
# #' above which a negative binomial model is fitted. Default = 2.
# #' @param ... Additional arguments, as for the \code{glm()} function.
# #' @return A fitted object of class \code{glm} ± \code{negbin}.
# #' @details Wrapper function for \code{stats::glm()} and \code{MASS:glm.nb()}.
# #' @export
# glm.count <- function(formula, thresh = 2, ...) {
#   fit <- stats::glm(formula, family = stats::poisson, ...)
#   if (AS.dispersion(fit) > thresh) {
#     if (!requireNamespace("MASS", quietly = TRUE)) stop("[glm.count] negative binomial regression requires package 'MASS'")
#     fit <- MASS::glm.nb(formula, ...)
#   }
#   return(fit)
# }

# #' Count regression
# #'
# #' Fits a Poisson, a negative binomial, and a negative binomial mixed-effects
# #' (with random intercept terms corresponding to the fixed-effect terms)
# #' regression model¹, returning the model with the lowest Akaike information
# #' criterion.
# #' @param formula Formula for the Poisson model, as for the \code{glm()} function.
# #' @param data Optional data frame containing model variables.
# #' @return A fitted object of class \code{glm} (± \code{negbin}) or \code{glmmTMB}.
# #' @details Wrapper function for \code{stats::glm()}, \code{MASS:glm.nb()},
# #' and \code{glmmTMB::glmmTMB()}.
# #' @references
# #' 1. Payne, E.H., Hardin, J.W., Egede, L.E., Ramakrishnan, V., Selassie, A. and Gebregziabher, M., 2017.
# #' Approaches for dealing with various sources of overdispersion in modeling count data: scale adjustment versus modeling.
# #' \emph{Statistical methods in medical research}, 26(4), pp. 1802–1823.
# #' @export
# glm.count <- function(formula, data = NULL) {
#   if (!requireNamespace("glmmTMB", quietly = TRUE)) stop("[glm.count.AIC] requires package 'glmmTMB'")
#   if (!requireNamespace("MASS", quietly = TRUE)) stop("[glm.count.AIC] requires package 'MASS'")
#   fits <- list()
#   ICs <- rep(NA, 3)
#   terms <- attr(terms(formula), "term.labels")
#   fits[[1]] <- stats::glm(formula, data = data, family = stats::poisson, ...)
#   #fits[[2]] <- glmmTMB::glmmTMB(formula, data = data, family = glmmTMB::nbinom2, ...)
#   fits[[2]] <- MASS::glm.nb(formula, data = data)
#   fits[[3]] <- glmmTMB::glmmTMB(
#     as.formula(paste0(deparse(formula), "+(1|",
#                       paste0(terms, collapse = "+"), ")")),
#     data = data, family = glmmTMB::nbinom2)
#   ICs[1] <- AIC(fits[[1]])
#   ICs[2] <- AIC(fits[[2]])
#   ICs[3] <- AIC(fits[[3]])
#   return(fits[[which.min(ICs)]])
# }
