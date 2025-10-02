#' Auto-format
#'
#' Format regression results into manuscript-ready tables.
#' @param fit A fitted object of class \code{coxph}, \code{glm}, \code{glmerMod},
#' \code{lm}, or \code{lmerModLmerTest}.
#' @param name Optional string vector of variable names.
#' @return A character matrix representing a manuscript-ready table.
#' @details
#' \emph{P}-values for \code{lmerModLmerTest} objects are calculated using the
#' Kenward\eqn{-}Roger method\eqn{^{1}}.
#' @references
#' 1. Kenward, M.G. and Roger, J.H., 1997. Small sample inference for fixed effects
#' from restricted maximum likelihood. \emph{Biometrics}, pp. 983\eqn{-}997.
#' @examples
#' library(AutoScript)
#' library(survival)
#' data <- survival::veteran
#' fit <- coxph(Surv(data$time, data$status) ~ as.factor(data$trt))
#' table2 <- AS.format(fit, name = "Treatment")
#' print(table2)
#' @export
AS.format <- function(fit, name = NULL) {
  # defaults
  b.title <- "\u03b2 (95%CI)"
  f <- function(x) x
  if (inherits(fit, "coxph")) {
    summ.col <- c(1, 3, 5)
    b.title <- "HR (95%CI)"
    f <- exp
  } else if (inherits(fit, "glm")) {
    if (stats::family(fit)$family == "binomial" & stats::family(fit)$link == "logit") {
      summ.col <- c(1, 2, 4) # logistic regression
      b.title <- "OR (95%CI)"
      f <- exp
    } else if (stats::family(fit)$family == "gaussian" & stats::family(fit)$link == "identity") {
      summ.col <- c(1, 2, 4) # linear regression
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
  if (inherits(fit, "lmerModLmerTest")) {
    summ <- summary(fit, ddf = "Kenward-Roger")$coefficients
  } else {
    summ <- summary(fit)$coefficients
  }
  if (is.null(name)) name <- rownames(summary(fit)$coefficients)
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
    output[1 + i, 2] <- paste0(AS.fixdec(f(beta[i])), " (", AS.fixdec(f(LL[i])),  " to ", AS.fixdec(f(UL[i])), ")")
    output[1 + i, 3] <- AS.signif(p[i])
  }
  return(output)
}
