#' Darken color
#'
#' Alpha-composites a color over a black background (`alpha * color + (1 -
#' alpha) * black`).
#' @param color A color, as accepted by `grDevices::col2rgb()`.
#' @param alpha Alpha value of `color` in the composite, from `0` (fully
#' transparent, returns black) to `1` (fully opaque, returns `color`
#' unchanged). Default = `0.5`.
#' @return A hex color string.
#' @export
AS.color.darken <- function(color, alpha = 0.5) {
  rgb_val <- grDevices::col2rgb(color) / 255
  return(grDevices::rgb(t(rgb_val * alpha), maxColorValue = 1))
}

#' Lighten color
#'
#' Alpha-composites a color over a white background (`alpha * color + (1 -
#' alpha) * white`).
#' @param color A color, as accepted by `grDevices::col2rgb()`.
#' @param alpha Alpha value of `color` in the composite, from `0` (fully
#' transparent, returns white) to `1` (fully opaque, returns `color`
#' unchanged). Default = `0.5`.
#' @return A hex color string.
#' @export
AS.color.lighten <- function(color, alpha = 0.5) {
  rgb_val <- grDevices::col2rgb(color) / 255
  return(grDevices::rgb(t(rgb_val * alpha + (1 - alpha)), maxColorValue = 1))
}

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
#' Converts a numeric vector to strings with a fixed number of decimal places,
#' including trailing zeros. Invalid values are displayed as `"N/A"`. Negative
#' numbers are represented using true minus signs (`\u2212`) instead of hyphens.
#' @param x A numeric vector.
#' @param digits Number of decimal places. Default = `2`.
#' @return A string vector representation of `x`.
#' @export
AS.fixdec <- function(x, digits = 2) {
  if (!is.numeric(x)) return(rep("N/A", length(x)))
  output <- formatC(x, format = "f", digits = max(0, digits), flag = "#")
  output <- sub("\\.$", "", output)
  output <- gsub("-", "\u2212", output, fixed = TRUE)
  output[is.na(x)] <- "N/A"
  return(output)
}

#' Interleave vectors
#'
#' Combines two vectors by alternating their elements, starting with `a`.
#' @param a A vector.
#' @param b A vector, recycled against `a` if shorter.
#' @return A vector of length `2 * length(a)` with elements from `a` and `b`
#' alternating.
#' @export
AS.interleave <- function(a, b) {
  return(c(rbind(a, b)))
}

#' Save PNG
#'
#' Saves a plot to a PNG file using the Cairo graphics device for consistent
#' font rendering.
#' @param g A plot object to be saved.
#' @param path File path of the output.
#' @param width Width of the output image.
#' @param height Height of the output image.
#' @param units Units for `width` and `height`. Can be `"cm"`, `"in"` (inches,
#' the default), `"mm"`, or `"px"` (pixels).
#' @param res Nominal resolution in pixels per inch. Default = `300`.
#' @return The value returned by `grDevices::dev.off()`.
#' @details
#' Requires Cairo support in the current R build. If Cairo is not available
#' (`capabilities("cairo") == FALSE`), the function stops with an error.
#' @export
AS.save.png <- function(g, path, width = 6, height = 5, units = "in", res = 300) {
  if (!capabilities("cairo")) stop("[OR.save.png] Cairo unavailable")
  grDevices::png(path, width = width, height = height, units = units, res = res, type = "cairo")
  print(g)
  invisible(grDevices::dev.off())
}

#' Significant figures
#'
#' Converts a numeric value to a string with the specified number of significant
#' figures, including trailing zeros. Values smaller than `threshold` are
#' displayed as `"< threshold"` and values larger than `1 - threshold` are displayed
#' as `"> 1 - threshold"`. Invalid values are displayed as `"N/A"`. Negative
#' numbers are represented using true minus signs (`\u2212`) instead of hyphens.
#' @param x A numeric vector.
#' @param digits Number of significant figures. Default = `2`.
#' @param threshold Lower bound below which values are displayed as
#' `"< threshold"` and values larger than `1 - threshold` are displayed as
#' `"> 1 - threshold"`. Default = `0.001`.
#' @return A string vector representation of `x`.
#' @export
AS.signif <- function(x, digits = 2, threshold = 0.001) {
  if (!is.numeric(x)) return(rep("N/A", length(x)))
  output <- formatC(signif(x, digits), format = "fg", digits = digits, flag = "#")
  output <- sub("\\.$", "", output)
  output[which(x == 0)] <- paste0("0.", strrep("0", digits - 1))
  output[which(x < threshold)] <- paste0("< ", toString(threshold))
  output[which(x > 1 - threshold)] <- paste0("> ", toString(1 - threshold))
  output <- gsub("-", "\u2212", output, fixed = TRUE)
  output[is.na(x)] <- "N/A"
  return(output)
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
