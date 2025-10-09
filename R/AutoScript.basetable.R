#' Baseline characteristics tables: create
#'
#' See Details.
#' @param group Integer vector of group assignments, coded as 0, 1, 2, or 3.
#' @param name String vector of group names. Default = \code{c("Group 0", "Group 1", "Group 2", "Group 3")}.
#' @return A list with two elements:
#' \itemize{
#'   \item \code{group}: an integer vector of group assignments
#'   \item \code{table}: a character matrix containing column headers and
#'     sample sizes
#' }
#' @template AS.basetable_details
#' @export
AS.basetable.create <- function(group, name = c("Group 0", "Group 1", "Group 2", "Group 3")) {
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
  } else if (setequal(unique(group), 0:3)) {
    output$table <- matrix("", nrow = 2, ncol = 7)
    output$table[1, 1] <- "Name"
    output$table[1, 2] <- "Total"
    output$table[1, 3] <- name[1]
    output$table[1, 4] <- name[2]
    output$table[1, 5] <- name[3]
    output$table[1, 6] <- name[4]
    output$table[1, 7] <- "p (LR)"
    output$table[2, 2] <- paste0("n = ", length(group))
    output$table[2, 3] <- paste0("n = ", sum(group == 0))
    output$table[2, 4] <- paste0("n = ", sum(group == 1))
    output$table[2, 5] <- paste0("n = ", sum(group == 2))
    output$table[2, 6] <- paste0("n = ", sum(group == 3))
  } else {stop("[AS.basetable.create] must have 1, 2, 3, or 4 groups")}
  return(output)
}

################################################################################

#' Baseline characteristics tables: add binary variable
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param outcome Binary vector (0:1 or logical) of outcome values.
#' @template AS.basetable_param_0_digits.fixed
#' @template AS.basetable_return
#' @template AS.basetable_details
#' @export
AS.basetable.binary <- function(name, outcome, basetable, subset.mask = NULL, p.values = TRUE,
                                digits.fixed = 0, digits.sig = 2, sig.thresh = 0.001) {
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
      fit <- stats::glm(y ~ as.factor(X), family = stats::binomial)
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
      fit0 <- stats::glm(y ~ as.factor(X), family = stats::binomial)
      fit1 <- stats::glm(y ~ relevel(as.factor(X), ref = "1"), family = stats::binomial)
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[3, 4], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[3, 4], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- stats::glm(y ~ X != 0, family = stats::binomial)
      fit1x <- stats::glm(y ~ X != 1, family = stats::binomial)
      fit2x <- stats::glm(y ~ X != 2, family = stats::binomial)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[2, 4], digits.sig, sig.thresh)
      # LR
      fitnull <- stats::glm(y ~ 1, family = stats::binomial)
      output$table[r, 12] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 3) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.binary(y, digits.fixed)
    output$table[r, 3] <- AS.summary.binary(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.binary(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.binary(y[X == 2], digits.fixed)
    output$table[r, 6] <- AS.summary.binary(y[X == 3], digits.fixed)
    if (p.values) {
      fit0 <- stats::glm(y ~ as.factor(X), family = stats::binomial)
      fitnull <- stats::glm(y ~ 1, family = stats::binomial)
      output$table[r, 7] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.binary] must have 1, 2, 3, or 4 groups")}
  return(output)
}

#' Baseline characteristics tables: add blank row
#'
#' See Details.
#' @param text A string giving the custom text to display in the first column.
#' @param basetable A list created by \code{AS.basetable.create}, containing the group
#' assignments and current table structure.
#' @template AS.basetable_return
#' @template AS.basetable_details
#' @export
AS.basetable.blank <- function(text, basetable) {
  X <- basetable$group
  output <- basetable
  output$table <- rbind(output$table, rep("", ncol(output$table)))
  r <- nrow(output$table)
  output$table[r, 1] <- text
  return(output)
}

#' Baseline characteristics tables: add count variable
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param outcome Integer vector of outcome values.
#' @template AS.basetable_param
#' @template AS.basetable_return
#' @template AS.basetable_details
#' @export
AS.basetable.count <- function(name, outcome, basetable, subset.mask = NULL, p.values = TRUE,
                               digits.fixed = 2, digits.sig = 2, sig.thresh = 0.001) {
  X <- basetable$group
  y <- outcome
  if (!is.null(subset.mask)) {
    X <- X[subset.mask]
    y <- outcome[subset.mask]
  }
  if (length(y) != length(X)) {stop("[AS.basetable.count] inconsistent length")}
  if (any(is.na(y))) {stop("[AS.basetable.count] outcome contains NA, use subset.mask")}
  output <- basetable
  output$table <- rbind(output$table, rep("", ncol(output$table)))
  r <- nrow(output$table)
  if (max(basetable$group) == 0) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.count(y, digits.fixed)
  } else if (max(basetable$group) == 1) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.count(y, digits.fixed)
    output$table[r, 3] <- AS.summary.count(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.count(y[X == 1], digits.fixed)
    if (p.values) {
      fit <- glm.count(y ~ as.factor(X))
      output$table[r, 5] <- AS.signif(summary(fit)$coefficients[2, 4], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 2) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.count(y, digits.fixed)
    output$table[r, 3] <- AS.summary.count(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.count(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.count(y[X == 2], digits.fixed)
    if (p.values) {
      # 0 vs 1, 0 vs 2, 1 vs 2
      fit0 <- stats::glm(y ~ as.factor(X))
      fit1 <- stats::glm(y ~ relevel(as.factor(X), ref = "1"))
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[3, 4], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[3, 4], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- glm.count(y ~ X != 0)
      fit1x <- glm.count(y ~ X != 1)
      fit2x <- glm.count(y ~ X != 2)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[2, 4], digits.sig, sig.thresh)
      # LR
      fitnull <- glm.count(y ~ 1)
      output$table[r, 12] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 3) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.count(y, digits.fixed)
    output$table[r, 3] <- AS.summary.count(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.count(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.count(y[X == 2], digits.fixed)
    output$table[r, 6] <- AS.summary.count(y[X == 3], digits.fixed)
    if (p.values) {
      fit0 <- glm.count(y ~ as.factor(X))
      fitnull <- glm.count(y ~ 1)
      output$table[r, 7] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.count] must have 1, 2, 3, or 4 groups")}
  return(output)
}

#' Baseline characteristics tables: add "HH:MM" variable
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param outcome Numeric vector of continuous outcome values.
#' @template AS.basetable_param_no_digits.fixed
#' @template AS.basetable_return
#' @template AS.basetable_details
#' @export
AS.basetable.HHMM <- function(name, outcome, basetable, subset.mask = NULL, p.values = TRUE,
                              digits.sig = 2, sig.thresh = 0.001) {
  if (!requireNamespace("WHcircular", quietly = TRUE)) stop("[AS.basetable.HHMM] requires package 'WHcircular'")
  HHMM.lm.p <- function(y, X.formula) {
    X <- as.matrix(stats::model.matrix(X.formula)[, -1])
    fit <- WHcircular::WH_reg_circlinear(WHcircular::WH_HHMM_to_rad(y), X)
    return(fit$p)
  }
  X <- basetable$group
  y <- outcome
  if (!is.null(subset.mask)) {
    X <- X[subset.mask]
    y <- y[subset.mask]
  }
  if (length(y) != length(X)) {stop("[AS.basetable.HHMM] inconsistent length")}
  if (any(is.na(y))) {stop("[AS.basetable.HHMM] outcome contains NA, use subset.mask")}
  output <- basetable
  output$table <- rbind(output$table, rep("", ncol(output$table)))
  r <- nrow(output$table)
  if (max(basetable$group) == 0) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.HHMM(y)
  } else if (max(basetable$group) == 1) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.HHMM(y)
    output$table[r, 3] <- AS.summary.HHMM(y[X == 0])
    output$table[r, 4] <- AS.summary.HHMM(y[X == 1])
    if (p.values) {
      fit <- HHMM.lm.p(y, ~as.factor(X))
      output$table[r, 5] <- AS.signif(fit[1], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 2) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.HHMM(y)
    output$table[r, 3] <- AS.summary.HHMM(y[X == 0])
    output$table[r, 4] <- AS.summary.HHMM(y[X == 1])
    output$table[r, 5] <- AS.summary.HHMM(y[X == 2])
    if (p.values) {
      # 0 vs 1, 0 vs 2, 1 vs 2
      fit0 <- HHMM.lm.p(y, ~as.factor(X))
      fit1 <- HHMM.lm.p(y, ~relevel(as.factor(X), ref = "1"))
      output$table[r, 6] <- AS.signif(fit0[1], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(fit0[2], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(fit1[2], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- HHMM.lm.p(y, ~X != 0)
      fit1x <- HHMM.lm.p(y, ~X != 1)
      fit2x <- HHMM.lm.p(y, ~X != 2)
      output$table[r, 9] <- AS.signif(fit0x[1], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(fit1x[1], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(fit2x[1], digits.sig, sig.thresh)
      # LR
      output$table[r, 12] <- AS.signif(WHcircular::WH_CordeiroPaulaBotter(WHcircular::WH_HHMM_to_rad(y), X), digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 3) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.HHMM(y)
    output$table[r, 3] <- AS.summary.HHMM(y[X == 0])
    output$table[r, 4] <- AS.summary.HHMM(y[X == 1])
    output$table[r, 5] <- AS.summary.HHMM(y[X == 2])
    output$table[r, 6] <- AS.summary.HHMM(y[X == 3])
    if (p.values) {
      output$table[r, 7] <- AS.signif(WHcircular::WH_CordeiroPaulaBotter(WHcircular::WH_HHMM_to_rad(y), X), digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.HHMM] must have 1, 2, 3, or 4 groups")}
  return(output)
}

#' Baseline characteristics tables: add linear variable
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param outcome Numeric vector of continuous outcome values.
#' @template AS.basetable_param
#' @template AS.basetable_return
#' @template AS.basetable_details
#' @export
AS.basetable.linear <- function(name, outcome, basetable, subset.mask = NULL, p.values = TRUE,
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
      fit <- stats::glm(y ~ as.factor(X))
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
      fit0 <- stats::glm(y ~ as.factor(X))
      fit1 <- stats::glm(y ~ relevel(as.factor(X), ref = "1"))
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[3, 4], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[3, 4], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- stats::glm(y ~ X != 0)
      fit1x <- stats::glm(y ~ X != 1)
      fit2x <- stats::glm(y ~ X != 2)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[2, 4], digits.sig, sig.thresh)
      # LR
      fitnull <- stats::glm(y ~ 1)
      output$table[r, 12] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 3) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.linear(y, digits.fixed)
    output$table[r, 3] <- AS.summary.linear(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.linear(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.linear(y[X == 2], digits.fixed)
    output$table[r, 6] <- AS.summary.linear(y[X == 3], digits.fixed)
    if (p.values) {
      fit0 <- stats::glm(y ~ as.factor(X))
      fitnull <- stats::glm(y ~ 1)
      output$table[r, 7] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.linear] must have 1, 2, 3, or 4 groups")}
  return(output)
}

#' Baseline characteristics tables: add log-linear variable
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param outcome Numeric vector of continuous outcome values.
#' @template AS.basetable_param
#' @template AS.basetable_return
#' @template AS.basetable_details
#' @export
AS.basetable.loglinear <- function(name, outcome, basetable, subset.mask = NULL, p.values = TRUE,
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
    output$table[r, 2] <- AS.summary.loglinear(y, digits.fixed)
  } else if (max(basetable$group) == 1) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.loglinear(y, digits.fixed)
    output$table[r, 3] <- AS.summary.loglinear(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.loglinear(y[X == 1], digits.fixed)
    if (p.values) {
      fit <- stats::glm(log(y) ~ as.factor(X))
      output$table[r, 5] <- AS.signif(summary(fit)$coefficients[2, 4], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 2) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.loglinear(y, digits.fixed)
    output$table[r, 3] <- AS.summary.loglinear(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.loglinear(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.loglinear(y[X == 2], digits.fixed)
    if (p.values) {
      # 0 vs 1, 0 vs 2, 1 vs 2
      fit0 <- stats::glm(log(y) ~ as.factor(X))
      fit1 <- stats::glm(log(y) ~ relevel(as.factor(X), ref = "1"))
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[3, 4], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[3, 4], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- stats::glm(log(y) ~ X != 0)
      fit1x <- stats::glm(log(y) ~ X != 1)
      fit2x <- stats::glm(log(y) ~ X != 2)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[2, 4], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[2, 4], digits.sig, sig.thresh)
      # LR
      fitnull <- stats::glm(log(y) ~ 1)
      output$table[r, 12] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 3) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.loglinear(y, digits.fixed)
    output$table[r, 3] <- AS.summary.loglinear(y[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.loglinear(y[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.loglinear(y[X == 2], digits.fixed)
    output$table[r, 6] <- AS.summary.loglinear(y[X == 3], digits.fixed)
    if (p.values) {
      fit0 <- stats::glm(log(y) ~ as.factor(X))
      fitnull <- stats::glm(log(y) ~ 1)
      output$table[r, 7] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.loglinear] must have 1, 2, 3, or 4 groups")}
  return(output)
}

#' Baseline characteristics tables: add time-to-event
#'
#' See Details.
#' @param name A string giving the variable name to display in the first column.
#' @param time Numeric vector of follow-up times.
#' @param status Binary vector indicating event occurrence (1 = event, 0 = censored).
#' @template AS.basetable_param
#' @template AS.basetable_return
#' @template AS.basetable_details
#' @export
AS.basetable.TTE <- function(name, time, status, basetable, subset.mask = NULL, p.values = TRUE,
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
      fit <- survival::coxph(survival::Surv(time, status) ~ as.factor(X))
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
      fit0 <- survival::coxph(survival::Surv(time, status) ~ as.factor(X))
      fit1 <- survival::coxph(survival::Surv(time, status) ~ relevel(as.factor(X), ref = "1"))
      output$table[r, 6] <- AS.signif(summary(fit0)$coefficients[1, 5], digits.sig, sig.thresh)
      output$table[r, 7] <- AS.signif(summary(fit0)$coefficients[2, 5], digits.sig, sig.thresh)
      output$table[r, 8] <- AS.signif(summary(fit1)$coefficients[2, 5], digits.sig, sig.thresh)
      # 0 vs 12, 1 vs 02, 2 vs 01
      fit0x <- survival::coxph(survival::Surv(time, status) ~ X != 0)
      fit1x <- survival::coxph(survival::Surv(time, status) ~ X != 1)
      fit2x <- survival::coxph(survival::Surv(time, status) ~ X != 2)
      output$table[r, 9] <- AS.signif(summary(fit0x)$coefficients[5], digits.sig, sig.thresh)
      output$table[r, 10] <- AS.signif(summary(fit1x)$coefficients[5], digits.sig, sig.thresh)
      output$table[r, 11] <- AS.signif(summary(fit2x)$coefficients[5], digits.sig, sig.thresh)
      # LR
      fitnull <- survival::coxph(survival::Surv(time, status) ~ 1)
      output$table[r, 12] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 4], digits.sig, sig.thresh)
    }
  } else if (max(basetable$group) == 3) {
    output$table[r, 1] <- name
    output$table[r, 2] <- AS.summary.KM(time, status, digits.fixed)
    output$table[r, 3] <- AS.summary.KM(time[X == 0], status[X == 0], digits.fixed)
    output$table[r, 4] <- AS.summary.KM(time[X == 1], status[X == 1], digits.fixed)
    output$table[r, 5] <- AS.summary.KM(time[X == 2], status[X == 2], digits.fixed)
    output$table[r, 6] <- AS.summary.KM(time[X == 3], status[X == 3], digits.fixed)
    if (p.values) {
      fit0 <- survival::coxph(survival::Surv(time, status) ~ as.factor(X))
      fitnull <- survival::coxph(survival::Surv(time, status) ~ 1)
      output$table[r, 7] <- AS.signif(stats::anova(fitnull, fit0, test = "LRT")[2, 5], digits.sig, sig.thresh)
    }
  } else {stop("[AS.basetable.TTE] must have 1, 2, 3, or 4 groups")}
  return(output)
}

AS.basetable.test <- function(n.group = 2, n = 100) {
  group <- sample(0:(n.group - 1), n, T)
  outcome1 <- sample(0:1, n, T)
  outcome2 <- stats::rnorm(n)
  outcome3 <- WHcircular::WH_rad_to_HHMM(stats::runif(n, -pi, pi))
  test <- AS.basetable.create(group)
  test <- AS.basetable.binary("Outcome 1", outcome1, test)
  test <- AS.basetable.linear("Outcome 2", outcome2, test)
  test <- AS.basetable.HHMM("Outcome 3", outcome3, test)
  return(test)
}
