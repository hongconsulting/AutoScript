#' Baseline characteristics table functions (overview)
#'
#' @details
#' \itemize{
#'   \item \code{AS.basetable.create}: creates a template table structure
#'     with headers, group sample sizes, and p-value columns for 1, 2, or 3 groups.
#'   \item \code{AS.basetable.binary}: adds a row for a binary variable with
#'     counts and percentages. P-values are obtained from logistic regression.
#'   \item \code{AS.basetable.blank}: adds a blank row with a label, for separating sections.
#'   \item \code{AS.basetable.linear}: adds a row for a continuous variable
#'     with mean \eqn{\pm} SD. P-values are obtained from linear regression.
#'   \item \code{AS.basetable.TTE}: adds a row for a time-to-event variable,
#'     with Kaplan\eqn{-}Meier median and 95% confidence interval. P-values are obtained
#'     from Cox regression.
#' }
#' The resulting table includes total and group-specific summaries.
#' P-values are provided but are not always appropriate to report.
#' With 3 groups, seven possible comparisons are provided:
#' \itemize{
#'   \item Each pairwise comparison: group 0 versus 1, 0 versus 2, and 1 versus 2.
#'   \item Each group against the combination of the other two: group 0 versus 1 & 2,
#'   1 versus 0 & 2, 2 versus 0 & 1
#'   \item A global likelihood-ratio test.
#' }
#' @examples
#' library(AutoScript)
#' library(survival)
#' data <- survival::veteran
#' table1 <- AS.basetable.create(group = data$trt - 1, name = c("Control", "Experimental"))
#' table1 <- AS.basetable.linear("Age (years), mean \u00B1 SD", data$age, table1, digits.fixed = 1)
#' table1 <- AS.basetable.blank("Histology:", table1)
#' table1 <- AS.basetable.binary("- Non-small cell, n (%)", data$celltype != "smallcell", table1)
#' table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)", data$celltype == "adeno", table1, subset.mask = data$celltype != "smallcell")
#' table1 <- AS.basetable.binary("  - Squamous, n (%)", data$celltype == "squamous", table1, subset.mask = data$celltype != "smallcell")
#' table1 <- AS.basetable.binary("  - Large cell, n (%)", data$celltype == "large", table1, subset.mask = data$celltype != "smallcell")
#' table1 <- AS.basetable.binary("- Small cell, n (%)", data$celltype == "smallcell", table1, p.values = F)
#' print(table1$table)
