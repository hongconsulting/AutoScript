#' Dispersion estimate
#'
#' Computes the ratio of residual deviance to residual degrees of freedom, used
#' to assess over-dispersion.
#' @param fit A fitted `glm` object.
#' @return Numeric value of the dispersion estimate.
#' @export
AS.dispersion <- function(fit) {
  return(fit$deviance/fit$df.residual)
}

#' Fixed decimal places
#'
#' Converts a numeric value to a string with a fixed number of decimal places,
#' including trailing zeros. Invalid values are displayed as `"N/A"`.
#' @param x A numeric value.
#' @param digits Number of decimal places. Default = `2`.
#' @return A string representation of `x`.
#' @export
AS.fixdec <- function(x, digits = 2) {
  if (!is.numeric(x) | is.na(x)) return("N/A")
  output <- formatC(x, format = "f", digits = max(0, digits), flag = "#")
  return(sub("\\.$", "", output))
}

#' Significant figures
#'
#' Converts a numeric value to a string with the specified number of significant
#' figures, including trailing zeros. Values smaller than `threshold` are
#' displayed as `"< threshold"` and values larger than `1 - threshold` are displayed
#' as `"> 1 - threshold"`. Invalid values are displayed as `"N/A"`.
#' @param x A numeric value.
#' @param digits Number of significant figures. Default = `2`.
#' @param threshold Lower bound below which values are displayed as
#' `"< threshold"` and values larger than `1 - threshold` are displayed as
#' `"> 1 - threshold"`. Default = `0.001`.
#' @return A string representation of `x`.
#' @export
AS.signif <- function(x, digits = 2, threshold = 0.001) {
  if (!is.numeric(x) | is.na(x)) return("N/A")
  if (x < threshold) {return(paste0("< ", toString(threshold)))}
  if (x > 1 - threshold) return(paste0("> ", toString(1 - threshold)))
  output <- formatC(signif(x, digits), format = "fg", digits = digits, flag = "#")
  output[x == 0] <- paste0("0.", strrep("0", digits - 1)) # 0.00
  return( sub("\\.$", "", output)) # 0.
}

AS.trycatch <- function(expr) {
  tryCatch(expr, error = function(e) NA)
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
#' @param formula Formula for the Poisson model, as for the `glm()` function.
#' @param data Optional data frame containing model variables.
#' @return A fitted object of class `glm` ± `negbin`.
#' @details Wrapper function for `stats::glm()` and `MASS:glm.nb()`.
#' @references
#' 1. Akaike, H., 1974. A new look at the statistical model identification.
#' *IEEE Transactions on Automatic Control*, 19(6), pp. 716–723.
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
