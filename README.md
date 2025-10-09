
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AutoScript

<!-- badges: start -->
<!-- badges: end -->

Automated clinical research manuscript tables.

## Installation

You can install the development version of AutoScript from
[GitHub](https://github.com/hongconsulting/AutoScript) with:

``` r
remotes::install_github("hongconsulting/AutoScript")
```

## 1. Baseline characteristics tables

Compared with [other packages offering similar
functionality](https://github.com/kaz-yos/tableone?tab=readme-ov-file#similar-or-complementary-projects),
**AutoScript** is built on the following principles:

- Fine-grained control through line-by-line construction and
  `subset.mask`.

- Significance testing via modern regression methods rather than
  classical (often eponymous) hypothesis tests (**note that significance
  testing of baseline characteristics is inappropriate for randomized
  groups and should be interpreted with caution for non-randomized
  groups**):

  - Classical tests are rarely indicated as regression methods generally
    perform the same function while providing additional capabilities.
    Using classical tests for baseline characteristics and regression
    methods for the main analyses results in a disjointed *Methods*
    section.

  - Non-parametric and “exact” methods are rarely indicated as they
    often needlessly sacrifice power; non-normality is usually better
    handled through mathematical transformation.

- Non-normality of continuous variables should be decided by the
  researcher rather than the data. Shapiro–Wilk and similar tests fail
  to detect non-normality in small samples while detecting clinically
  trivial non-normality in large samples; *statistical significance*
  depends on sample size and is distinct from *clinical significance*.

### Example: Veterans’ Administration lung cancer study

``` r
library(AutoScript)
library(survival)
data <- survival::veteran
table1 <- AS.basetable.create(group = data$trt - 1, name = c("Control", "Experimental"))
table1 <- AS.basetable.linear("Age (years), mean \u00b1 SD", data$age, table1, digits.fixed = 1)
table1 <- AS.basetable.loglinear("Time from diagnosis", data$diagtime, table1, digits.fixed = 1)
table1 <- AS.basetable.blank("(months), mean \u00b1 SD", table1)
table1 <- AS.basetable.blank("Histology:", table1)
table1 <- AS.basetable.binary("- Non-small cell, n (%)", data$celltype != "smallcell", table1)
table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)", data$celltype == "adeno", table1, 
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Squamous, n (%)", data$celltype == "squamous", table1, 
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Large cell, n (%)", data$celltype == "large", table1, 
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("- Small cell, n (%)", data$celltype == "smallcell", table1, 
                              p.values = FALSE)
options(width = 100)
print(table1$table)
#>       [,1]                        [,2]          [,3]          [,4]           [,5]   
#>  [1,] "Name"                      "Total"       "Control"     "Experimental" "p"    
#>  [2,] ""                          "n = 137"     "n = 69"      "n = 68"       ""     
#>  [3,] "Age (years), mean ± SD"    "58.3 ± 10.5" "57.5 ± 10.8" "59.1 ± 10.3"  "0.37" 
#>  [4,] "Time from diagnosis"       "5.8 ± 2.4"   "6.1 ± 2.3"   "5.5 ± 2.5"    "0.50" 
#>  [5,] "(months), mean ± SD"       ""            ""            ""             ""     
#>  [6,] "Histology:"                ""            ""            ""             ""     
#>  [7,] "- Non-small cell, n (%)"   "89 (65%)"    "39 (57%)"    "50 (74%)"     "0.038"
#>  [8,] "  - Adenocarcinoma, n (%)" "27 (30%)"    "9 (23%)"     "18 (36%)"     "0.19" 
#>  [9,] "  - Squamous, n (%)"       "35 (39%)"    "15 (38%)"    "20 (40%)"     "0.88" 
#> [10,] "  - Large cell, n (%)"     "27 (30%)"    "15 (38%)"    "12 (24%)"     "0.14" 
#> [11,] "- Small cell, n (%)"       "48 (35%)"    "30 (43%)"    "18 (26%)"     ""
```

### Example: eBird sightings

``` r
library(AutoScript)
suppressMessages(library(auk))
file <- system.file("extdata/ebd-sample.txt", package = "auk")
data <- utils::read.delim(file)[c("OBSERVATION.COUNT", "COUNTRY", "TIME.OBSERVATIONS.STARTED")]
data <- data[data$OBSERVATION.COUNT != "X",]
data$count <- as.numeric(data$OBSERVATION.COUNT)
data$time <- substr(data$TIME.OBSERVATIONS.STARTED, 1, 5)
table1 <- AS.basetable.create(group = data$COUNTRY == "United States", name = c("Non-US", "US"))
table1 <- AS.basetable.HHMM("Start time, mean \u00b1 SD", data$time, table1)
table1 <- AS.basetable.count("Count, n (mean)", data$count, table1)
print(table1$table)
#>      [,1]                    [,2]            [,3]            [,4]            [,5]     
#> [1,] "Name"                  "Total"         "Non-US"        "US"            "p"      
#> [2,] ""                      "n = 367"       "n = 152"       "n = 215"       ""       
#> [3,] "Start time, mean ± SD" "08:54 ± 01:29" "08:29 ± 01:26" "09:12 ± 01:27" "< 0.001"
#> [4,] "Count, n (mean)"       "1200 (3.27)"   "406 (2.67)"    "794 (3.69)"    "< 0.001"
```

## 2. Regression table auto-formatting

### Example: Cox regression

``` r
library(AutoScript)
library(survival)
data <- survival::veteran
fit <- survival::coxph(survival::Surv(time, status) ~ as.factor(trt), data = data)
print(AS.format(fit, name = "Treatment"))
#>      [,1]        [,2]                  [,3]  
#> [1,] ""          "HR (95%CI)"          "p"   
#> [2,] "Treatment" "1.02 (0.71 to 1.45)" "0.92"
```

### Example: heteroscedastic linear regression

``` r
library(AutoScript)
library(nlme)
set.seed(24601)

data0 <- data.frame("X" = 1:1000)
data0$y <- stats::rnorm(1000, mean = data0$X, sd = sqrt(data0$X))
fit0 <- nlme::gls(y ~ X, data = data0, weights = nlme::varPower(form = ~ X))
print(AS.format(fit0, hetero.name = "\u03b4"))
#>      [,1]                  [,2]                    [,3]     
#> [1,] ""                    "β (95%CI)"             "p"      
#> [2,] "(Intercept)"         "-0.58 (-1.31 to 0.15)" "0.12"   
#> [3,] "X"                   "1.00 (1.00 to 1.01)"   "< 0.001"
#> [4,] "Heteroscedasticity:" "θ (95%CI)"             ""       
#> [5,] "δ"                   "0.53 (0.49 to 0.58)"   ""

data1 <- data.frame("X" = rep(1:4, each = 250))
data1$y <- stats::rnorm(1000, mean = data1$X, sd = data1$X)
fit1 <- nlme::gls(y ~ as.factor(X), data = data1, weights = nlme::varIdent(form = ~ 1 | as.factor(X)))
print(AS.format(fit1, name = c("(Intercept)", paste0("Category ", 2:4)), hetero.name = paste0("Category ", 2:4)))
#>       [,1]                  [,2]                  [,3]     
#>  [1,] ""                    "β (95%CI)"           "p"      
#>  [2,] "(Intercept)"         "1.05 (0.93 to 1.18)" "< 0.001"
#>  [3,] "Category 2"          "1.07 (0.79 to 1.35)" "< 0.001"
#>  [4,] "Category 3"          "1.82 (1.45 to 2.19)" "< 0.001"
#>  [5,] "Category 4"          "2.89 (2.41 to 3.38)" "< 0.001"
#>  [6,] "Heteroscedasticity:" "θ (95%CI)"           ""       
#>  [7,] "Category 2"          "2.02 (1.79 to 2.29)" ""       
#>  [8,] "Category 3"          "2.78 (2.46 to 3.15)" ""       
#>  [9,] "Category 4"          "3.76 (3.32 to 4.25)" ""
```

### Example: linear mixed-effects regression

``` r
library(AutoScript)
library(lme4, quietly = TRUE, warn.conflicts = FALSE)
library(lmerTest, warn.conflicts = FALSE)
library(pbkrtest)
data <- lme4::sleepstudy
fit <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), data = data)
print(AS.format(fit, digits.fixed = 1))
#>      [,1]          [,2]                     [,3]     
#> [1,] ""            "β (95%CI)"              "p"      
#> [2,] "(Intercept)" "251.4 (238.0 to 264.8)" "< 0.001"
#> [3,] "Days"        "10.5 (7.4 to 13.5)"     "< 0.001"
```

### Example: logistic regression

``` r
library(AutoScript)
library(survival)
data <- survival::veteran
data$landmark <- NA
data$landmark[data$time <= 183 & data$status == 1] <- 0
data$landmark[data$time > 183] <- 1
fit <- stats::glm(landmark ~ as.factor(trt), family = stats::binomial, data = data)
print(AS.format(fit, name = c("(Intercept)", "Treatment")))
#>      [,1]          [,2]                  [,3]     
#> [1,] ""            "OR (95%CI)"          "p"      
#> [2,] "(Intercept)" "0.23 (0.12 to 0.43)" "< 0.001"
#> [3,] "Treatment"   "1.19 (0.50 to 2.82)" "0.69"
```

### Example: negative binomial regression

``` r
library(AutoScript)
library(MASS)
data <- MASS::epil
fit <- MASS::glm.nb(y ~ trt, data = data)
print(AS.format(fit, name = c("(Intercept)", "Treatment")))
#>      [,1]          [,2]                   [,3]     
#> [1,] ""            "IRR (95%CI)"          "p"      
#> [2,] "(Intercept)" "8.58 (6.99 to 10.53)" "< 0.001"
#> [3,] "Treatment"   "0.93 (0.70 to 1.23)"  "0.60"
print(AS.dispersion(fit))
#> [1] 1.140289
```

### Example: Poisson regression

``` r
library(AutoScript)
library(MASS)
data <- MASS::epil
fit <- stats::glm(y ~ trt, family = stats::poisson, data = data)
print(AS.format(fit, name = c("(Intercept)", "Treatment")))
#>      [,1]          [,2]                  [,3]     
#> [1,] ""            "IRR (95%CI)"         "p"      
#> [2,] "(Intercept)" "8.58 (8.05 to 9.14)" "< 0.001"
#> [3,] "Treatment"   "0.93 (0.85 to 1.01)" "0.098"
print(AS.dispersion(fit))
#> [1] 10.74825
```

## Further Reading

1.  Akaike, H., 1974. A new look at the statistical model
    identification. *IEEE Transactions on Automatic Control*, 19(6),
    pp. 716–723.
2.  Cordeiro, G.M., Paula, G.A. and Botter, D.A., 1994. Improved
    likelihood ratio tests for dispersion models. *International
    Statistical Review*, pp. 257–274.
3.  Fisher, N.I. and Lee, A.J., 1992. Regression models for an angular
    response. *Biometrics*, pp. 665–677.
4.  Kenward, M.G. and Roger, J.H., 1997. Small sample inference for
    fixed effects from restricted maximum likelihood. *Biometrics*,
    pp. 983–997.
