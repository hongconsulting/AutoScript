#' Plot observed and fitted proportions over time
#'
#' Fits a logistic regression assuming a linear effect of time on the log-odds
#' of the outcome and plots fitted and observed proportions over time.
#' Observed values are shown with Wilson 95% confidence intervals (without Yates
#' continuity correction), and fitted values with model-based pointwise 95%
#' confidence intervals.
#' @param y Binary outcome vector.
#' @param t Numeric vector representing time.
#' @param col.fitted Color for the fitted curve and its confidence limits.
#' Default = `"#C40233"`.
#' @param col.obs Color for observed proportions and their confidence intervals.
#' Default = `"#0087BD"`.
#' @param legend.pos Legend position. Default = `"bottom"`.
#' @param x.labels Labels for the x-axis ticks.
#' @param x.title Title for the x-axis. Default = `"Time"`.
#' @param y.breaks Numeric vector specifying y-axis tick locations.
#' @param y.title Title for the y-axis. Default = `"Proportion"`.
#' @return A `ggplot2::ggplot` object displaying fitted and observed proportions
#' over time, with pointwise 95% confidence intervals.
#' @export
ggproptrend <- function(y, t, col.fitted =  "#C40233", col.obs = "#0087BD",
                        legend.pos = "bottom",
                        x.labels, x.title = "Time",
                        y.breaks, y.title = "Proportion") {
  fit <- stats::glm(y~t, family = stats::binomial())
  times <- unique(t)
  output <- data.frame("t" = times)
  pred <- stats::predict(fit, newdata = output, type = "link", se.fit = TRUE)
  output$fitted_prop <- stats::plogis(pred$fit)
  output$fitted_lower <- stats::plogis(pred$fit - stats::qnorm(0.975) * pred$se.fit)
  output$fitted_upper <- stats::plogis(pred$fit + stats::qnorm(0.975) * pred$se.fit)
  output$y_prop <- NA
  output$y_lower <- NA
  output$y_upper <- NA
  for (i in 1:length(times)) {
    submask <- t == times[i]
    Wilson <- stats::prop.test(x = sum(y[submask]), n = sum(submask),
                               conf.level = 0.95, correct = FALSE)
    output$y_prop[i] <- Wilson$estimate
    output$y_lower[i] <- Wilson$conf.int[1]
    output$y_upper[i] <- Wilson$conf.int[2]
  }
  # R CMD check behavior
  fitted_prop  <- output$fitted_prop
  fitted_lower <- output$fitted_lower
  fitted_upper <- output$fitted_upper
  y_prop       <- output$y_prop
  y_lower      <- output$y_lower
  y_upper      <- output$y_upper
  g <- ggplot2::ggplot(output, ggplot2::aes(x = t)) +
    ggplot2::geom_hline(yintercept = y.breaks, color = grDevices::rgb(0.95, 0.95, 0.95)) +
    ggplot2::geom_line(ggplot2::aes(y = fitted_prop, color = "Fitted")) +
    ggplot2::geom_line(ggplot2::aes(y = fitted_lower), color = col.fitted, linetype = "22") +
    ggplot2::geom_line(ggplot2::aes(y = fitted_upper), color = col.fitted, linetype = "22") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = y_lower, ymax = y_upper),
                           color = col.obs, width = 0.5) +
    ggplot2::geom_point(ggplot2::aes(y = y_prop, color = "Observed")) +
    ggplot2::scale_colour_manual(values = c("Observed" = col.obs, "Fitted" = col.fitted)) +
    ggplot2::scale_x_continuous(breaks = times, labels = x.labels) +
    ggplot2::scale_y_continuous(breaks = y.breaks, expand = c(0, 0)) +
    ggplot2::labs(x = x.title, y = y.title, color = NULL ) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend.pos,
                   text = ggplot2::element_text(family = "sans"))
  return(g)
}
