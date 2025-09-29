#' Format regression results into manuscript-ready tables
#'
#' @param fit A fitted object of class \code{coxph} or \code{glm}.
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
    b.title <- "HR (95%CI)"
    f <- exp
  } else if (inherits(fit, "glm")) {
    if (stats::family(fit)$family == "binomial" & stats::family(fit)$link == "logit") {
      # logistic regression
      summ <- summary(fit)$coefficients
      if (is.null(name)) name <- rownames(summary(fit)$coefficients)
      beta <- summ[, 1]
      SE <- summ[, 2]
      p <- summ[, 4]
      b.title <- "OR (95%CI)"
      f <- exp
    } else if (stats::family(fit)$family == "gaussian" & stats::family(fit)$link == "identity") {
      # linear regression
      summ <- summary(fit)$coefficients
      if (is.null(name)) name <- rownames(summary(fit)$coefficients)
      beta <- summ[, 1]
      SE <- summ[, 2]
      p <- summ[, 4]
      b.title <- "b (95%CI)"
      f <- function(x) x
    } else {
      stop(paste0("[AS.format] family ", stats::family(fit)$family, " with link ",
                  stats::family(fit)$link," not implemented"))
    }
  } else {
    stop(paste0("[AS.format] class ", paste0(class(fit), collapse = " ")," not implemented"))
  }
  LL <- beta - stats::qnorm(0.975) * SE
  UL <- beta + stats::qnorm(0.975) * SE
  output <- matrix("", ncol = 3, nrow = length(beta) + 1)
  output[1, 2] <- b.title
  output[1, 3] <- "p"
  for (i in 1:length(beta)) {
    output[1 + i, 1] <- name[i]
    output[1 + i, 2] <- paste0(AS.fixdec(f(beta[i])), " (", AS.fixdec(f(LL[i])),  " to ", AS.fixdec(exp(UL[i])), ")")
    output[1 + i, 3] <- AS.signif(p[i])
  }
  return(output)
}
