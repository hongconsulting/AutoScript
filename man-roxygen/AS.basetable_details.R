#' @details
#' \itemize{
#'   \item `AS.basetable.create`: creates a template table structure with
#'   headers, group sample sizes, and *p*-value columns for 1, 2, 3, or 4 groups.
#'   \item `AS.basetable.binary`: adds a row for a binary variable with counts
#'   and percentages. *P*-values are obtained from logistic regression.
#'   \item `AS.basetable.blank`: adds a blank row with a label, for separating
#'   sections.
#'   \item `AS.basetable.count`: adds a row for a count variable. *P*-values are
#'   obtained from Poisson regression or negative binomial regression, whichever
#'   model yields the lower Akaike information criterion¹.
#'   \item `AS.basetable.HHMM`: adds a row for a time-of-day variable in "HH:MM"
#'   string format with circular mean ± circular SD. *P*-values are obtained
#'   from circular-linear regression². Likelihood-ratio tests are performed
#'   using the Cordeiro–Paula–Botter method³.
#'   \item `AS.basetable.linear`: adds a row for a continuous variable with mean
#'   ± SD. *P*-values are obtained from linear regression.
#'   \item `AS.basetable.log1plinear`: adds a row for a log(x + 1)-transformed
#'   continuous variable with back-transformed mean ± back-transformed SD.
#'   *P*-values are obtained from linear regression on the transformed outcome.
#'   \item `AS.basetable.loglinear`: adds a row for a log-transformed continuous
#'   variable with geometric mean ± geometric SD. *P*-values are obtained from
#'   linear regression on the log-transformed outcome.
#'   \item `AS.basetable.TTE`: adds a row for a time-to-event variable, with
#'   Kaplan–Meier median and 95% confidence interval using the
#'   Brookmeyer–Crowley⁴ method with Greenwood's variance⁵ and a complementary
#'   log–log transformation⁶. *P*-values are obtained from Cox regression.
#' }
#' The resulting table includes total and group-specific summaries. *P*-values
#' are provided but are not always appropriate to report. With 3 groups, seven
#' possible comparisons are provided:
#' \itemize{
#'   \item Each pairwise comparison: group 0 versus 1, 0 versus 2, and 1 versus
#'   2.
#'   \item Each group against the combination of the other two: group 0 versus 1
#'   & 2, 1 versus 0 & 2, 2 versus 0 & 1.
#'   \item A global likelihood-ratio test.
#' }
#' With 4 groups, only the likelihood-ratio test is provided.
#' @references
#' 1. Akaike, H., 1974. A new look at the statistical model identification.
#' *IEEE Transactions on Automatic Control*, 19(6), pp. 716–723.
#' 2. Fisher, N.I. and Lee, A.J., 1992. Regression models for an angular
#' response. *Biometrics*, pp. 665–677.
#' 3. Cordeiro, G.M., Paula, G.A. and Botter, D.A., 1994. Improved likelihood
#' ratio tests for dispersion models. *International Statistical Review*, pp.
#' 257–274.
#' 4. Brookmeyer, R. and Crowley, J., 1982. A confidence interval for the median
#' survival time. *Biometrics*, pp. 29–41.
#' 5. Greenwood, M., 1926. A report on the natural duration of cancer. In:
#' *Reports on Public Health and Medical Subjects*, 33, pp. 1–26. London: Her
#' Majesty’s Stationery Office, Ministry of Health.
#' 6. Klein, J.P., Logan, B., Harhoff, M. and Andersen, P.K., 2007. Analyzing
#' survival curves at a fixed point in time. *Statistics in Medicine*, 26(24),
#' pp. 4505–4519.
#' @examples
#' # See GitHub README for further examples:
#' # https://github.com/hongconsulting/AutoScript
#' library(AutoScript)
#' library(survival)
#' data <- survival::veteran
#' table1 <- AS.basetable.create(group = data$trt - 1, name = c("Control", "Experimental"))
#' table1 <- AS.basetable.linear("Age (years), mean \u00b1 SD", data$age, table1, digits.fixed = 1)
#' table1 <- AS.basetable.loglinear("Time from diagnosis", data$diagtime, table1, digits.fixed = 1)
#' table1 <- AS.basetable.blank("(months), mean \u00b1 SD", table1)
#' table1 <- AS.basetable.blank("Histology:", table1)
#' table1 <- AS.basetable.binary("- Non-small cell, n (%)", data$celltype != "smallcell", table1)
#' table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)", data$celltype == "adeno", table1,
#'                               subset.mask = data$celltype != "smallcell")
#' table1 <- AS.basetable.binary("  - Squamous, n (%)", data$celltype == "squamous", table1,
#'                               subset.mask = data$celltype != "smallcell")
#' table1 <- AS.basetable.binary("  - Large cell, n (%)", data$celltype == "large", table1,
#'                               subset.mask = data$celltype != "smallcell")
#' table1 <- AS.basetable.binary("- Small cell, n (%)", data$celltype == "smallcell", table1,
#'                               p.values = FALSE)
#' options(width = 100)
#' print(table1$table)
