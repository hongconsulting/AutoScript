
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AutoScript

<!-- badges: start -->
<!-- badges: end -->

Automated clinical research manuscript tables.

## Installation

You can install the development version of AutoScript from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("hongconsulting/AutoScript")
```

## Example: baseline characteristics table

``` r
library(AutoScript)
library(survival)
data <- survival::veteran
table1 <- AS.basetable.create(group = data$trt - 1, name = c("Control", "Experimental"))
table1 <- AS.basetable.linear("Age (years), mean \u00B1 SD", data$age, table1, digits.fixed = 1)
table1 <- AS.basetable.loglinear("Time from diagnosis", data$diagtime, table1, digits.fixed = 1)
table1 <- AS.basetable.blank("(months), mean \u00B1 SD", table1)
table1 <- AS.basetable.blank("Histology:", table1)
table1 <- AS.basetable.binary("- Non-small cell, n (%)", data$celltype != "smallcell", table1)
table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)", data$celltype == "adeno", table1, subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Squamous, n (%)", data$celltype == "squamous", table1, subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Large cell, n (%)", data$celltype == "large", table1, subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("- Small cell, n (%)", data$celltype == "smallcell", table1, p.values = FALSE)
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

## Example: Cox regression table

``` r
library(AutoScript)
library(survival)
data <- survival::veteran
fit <- coxph(Surv(time, status) ~ as.factor(trt), data = data)
table2 <- AS.format(fit, name = "Treatment")
print(table2)
#>      [,1]        [,2]                  [,3]  
#> [1,] ""          "HR (95%CI)"          "p"   
#> [2,] "Treatment" "1.02 (0.71 to 1.45)" "0.92"
```

## Example: linear mixed-effects regression table

``` r
library(AutoScript)
library(lme4)
library(lmerTest)
data <- lme4::sleepstudy
fit <- lmer(Reaction ~ Days + (Days | Subject), data = data)
table3 <- AS.format(fit)
print(table3)
#>      [,1]          [,2]                        [,3]     
#> [1,] ""            "Beta (95%CI)"              "p"      
#> [2,] "(Intercept)" "251.41 (238.03 to 264.78)" "< 0.001"
#> [3,] "Days"        "10.47 (7.44 to 13.50)"     "< 0.001"
```

## Example: logistic regression table

``` r
library(AutoScript)
library(survival)
data <- survival::veteran
data$landmark <- NA
data$landmark[data$time <= 183 & data$status == 1] <- 0
data$landmark[data$time > 183] <- 1
fit <- glm(landmark ~ as.factor(trt), family = "binomial", data = data)
table4 <- AS.format(fit, name = c("(Intercept)", "Treatment"))
print(table4)
#>      [,1]          [,2]                  [,3]     
#> [1,] ""            "OR (95%CI)"          "p"      
#> [2,] "(Intercept)" "0.23 (0.12 to 0.43)" "< 0.001"
#> [3,] "Treatment"   "1.19 (0.50 to 2.82)" "0.69"
```
