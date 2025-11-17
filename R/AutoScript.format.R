#' Regression table auto-formatting
#'
#' Format regression results into manuscript-ready tables.
#' @param fit A fitted object of class `coxph`, `glm`, `glmerMod`, `gls`, `lm`,
#' or `lmerModLmerTest`.
#' @param name Optional string vector of coefficient names.
#' @param hetero.name Optional string vector of heteroscedasticity parameter
#' names.
#' @param digits.fixed Number of decimal places for summaries. Default = `2`.
#' @param digits.sig Number of significant figures for *p*-values. Default = `2`.
#' @param sig.thresh Threshold below which *p*-values are displayed as
#' `"< threshold"`. Default = `0.001`.
#' @return A character matrix representing a manuscript-ready table.
#' @details
#' *P*-values for `coxph` objects if `weights` are used are calculated using the
#' Huber–White sandwich estimator\ifelse{latex}{\out{$^{1,2}$}}{\ifelse{html}{\out{<sup>1,2</sup>}}{\out{^{1,2}}}} .
#' *P*-values for `lmerModLmerTest` objects are calculated using the Kenward–Roger
#' method\ifelse{latex}{\out{$^{3}$}}{\ifelse{html}{\out{<sup>3</sup>}}{^3}}.
#' @references
#' 1. Huber, P., 1967. The behavior of maximum likelihood estimates under
#' nonstandard conditions. In: *Proceedings of the fifth Berkeley symposium on
#' mathematical statistics and probability*, 1(1), pp. 221–233. Berkeley:
#' University of California Press.
#' 2. White, H., 1980. A heteroskedasticity-consistent covariance matrix
#' estimator and a direct test for heteroskedasticity. *Econometrica: Journal of
#' the Econometric Society*, pp. 817–838.
#' 3. Kenward, M.G. and Roger, J.H., 1997. Small sample inference for fixed effects
#' from restricted maximum likelihood. *Biometrics*, pp. 983–997.
#' @examples
#' # See GitHub README for further examples:
#' # https://github.com/hongconsulting/AutoScript
#' library(AutoScript)
#' library(survival)
#' data <- survival::veteran
#' fit <- coxph(Surv(data$time, data$status) ~ as.factor(data$trt))
#' print(AS.format(fit, name = "Treatment"))
#' @export
AS.format <- function(fit, name = NULL, hetero.name = NULL, digits.fixed = 2, digits.sig = 2, sig.thresh = 0.001) {
  # defaults
  b.title <- "\u03b2 (95%CI)"
  f <- function(x) x
  # main
  if (inherits(fit, "coxph")) {
    if (is.null(fit$weights)) {
      summ.col <- c(1, 3, 5)
    } else {
      summ.col <- c(1, 4, 6) # robust SEs
    }
    b.title <- "HR (95%CI)"
    f <- exp
  } else if (inherits(fit, "glm")) {
    if (stats::family(fit)$family == "binomial" & stats::family(fit)$link == "logit") {
      summ.col <- c(1, 2, 4) # logistic regression
      b.title <- "OR (95%CI)"
      f <- exp
    } else if (stats::family(fit)$family == "gaussian" & stats::family(fit)$link == "identity") {
      summ.col <- c(1, 2, 4) # linear regression
    } else if (inherits(fit, "negbin") & stats::family(fit)$link == "log") {
      summ.col <- c(1, 2, 4) # negative binomial regression
      b.title <- "IRR (95%CI)"
      f <- exp
    } else if (stats::family(fit)$family == "poisson" & stats::family(fit)$link == "log") {
      summ.col <- c(1, 2, 4) # Poisson regression
      b.title <- "IRR (95%CI)"
      f <- exp
    } else {
      stop(paste0("[AS.format] family ", stats::family(fit)$family, " with link ",
                  stats::family(fit)$link," not implemented"))
    }
  } else if (inherits(fit, "glmerMod")) {
    if (stats::family(fit)$family == "binomial" & stats::family(fit)$link == "logit") {
      summ.col <- c(1, 2, 4) # logistic regression
      b.title <- "OR (95%CI)"
      f <- exp
    } else {
      stop(paste0("[AS.format] family ", stats::family(fit)$family, " with link ",
                  stats::family(fit)$link," not implemented"))
    }
  } else if (inherits(fit, "gls")) {
    summ.col <- c(1, 2, 4)
  } else if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    summ.col <- c(1, 2, 4) # linear regression
  } else if (inherits(fit, "lmerModLmerTest")) {
    if (stats::family(fit)$family == "gaussian" & stats::family(fit)$link == "identity") {
      summ.col <- c(1, 2, 5) # linear regression
    } else {
      stop(paste0("[AS.format] family ", stats::family(fit)$family, " with link ",
                  stats::family(fit)$link," not implemented"))
    }
  } else {
    stop(paste0("[AS.format] class ", paste0(class(fit), collapse = " ")," not implemented"))
  }
  # summaries
  if (inherits(fit, "gls")) {
    summ <- summary(fit)$tTable
  } else if (inherits(fit, "lmerModLmerTest")) {
    # if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    #   stop("[AS.format] lmerModLmerTest requires package 'pbkrtest'")
    # }
    summ <- summary(fit, ddf = "Kenward-Roger")$coefficients
  } else {
    summ <- summary(fit)$coefficients
  }
  if (is.null(name)) name <- rownames(summ)
  beta <- summ[, summ.col[1]]
  SE <- summ[, summ.col[2]]
  p <- summ[, summ.col[3]]
  LL <- beta - stats::qnorm(0.975) * SE
  UL <- beta + stats::qnorm(0.975) * SE
  output <- matrix("", ncol = 3, nrow = length(beta) + 1)
  output[1, 2] <- b.title
  output[1, 3] <- "p"
  for (i in 1:length(beta)) {
    output[1 + i, 1] <- name[i]
    output[1 + i, 2] <- paste0(AS.fixdec(f(beta[i]), digits = digits.fixed),
                               " (", AS.fixdec(f(LL[i]), digits = digits.fixed),
                               " to ", AS.fixdec(f(UL[i]), digits = digits.fixed), ")")
    output[1 + i, 3] <- AS.signif(p[i], digits = digits.sig, threshold = sig.thresh)
  }
  # heteroscedasticity
  if (inherits(fit, "gls")) {
    hetero.summ <- nlme::intervals(fit)$varStruct
    if (is.null(hetero.name)) hetero.name <- rownames(hetero.summ)
    hetero.output <- matrix("", ncol = 3, nrow = nrow(hetero.summ) + 1)
    hetero.output[1, 1] <- "Heteroscedasticity:"
    hetero.output[1, 2] <- "\u03b8 (95%CI)"
    for (i in 1:nrow(hetero.summ)) {
      hetero.output[1 + i, 1] <- hetero.name[i]
      hetero.output[1 + i, 2] <- paste0(AS.fixdec(hetero.summ[i, 2], digits = digits.fixed),
                                 " (", AS.fixdec(hetero.summ[i, 1], digits = digits.fixed),
                                 " to ", AS.fixdec(hetero.summ[i, 3], digits = digits.fixed), ")")
    }
    output <- rbind(output, hetero.output)
  }
  return(output)
}
