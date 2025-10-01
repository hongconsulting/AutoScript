
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
table1 <- AS.basetable.loglinear("Time from diagnosis", data$diagtime, table1)
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
#>  [4,] "Time from diagnosis"       "5.77 ± 2.41" "6.07 ± 2.31" "5.48 ± 2.53"  "0.50" 
#>  [5,] "(months), mean ± SD"       ""            ""            ""             ""     
#>  [6,] "Histology:"                ""            ""            ""             ""     
#>  [7,] "- Non-small cell, n (%)"   "89 (65.0%)"  "39 (56.5%)"  "50 (73.5%)"   "0.038"
#>  [8,] "  - Adenocarcinoma, n (%)" "27 (30.3%)"  "9 (23.1%)"   "18 (36.0%)"   "0.19" 
#>  [9,] "  - Squamous, n (%)"       "35 (39.3%)"  "15 (38.5%)"  "20 (40.0%)"   "0.88" 
#> [10,] "  - Large cell, n (%)"     "27 (30.3%)"  "15 (38.5%)"  "12 (24.0%)"   "0.14" 
#> [11,] "- Small cell, n (%)"       "48 (35.0%)"  "30 (43.5%)"  "18 (26.5%)"   ""
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

## Example: Linear mixed-effects regression table

``` r
library(AutoScript)
library(lme4)
library(lmerTest)
data <- lme4::sleepstudy
fit <- lmer(Reaction ~ Days + (Days | Subject), data = data)
table3 <- AS.format(fit, name = c("(Intercept)", "Days"))
print(table3)
#>      [,1]          [,2]                        [,3]     
#> [1,] ""            "Beta (95%CI)"              "p"      
#> [2,] "(Intercept)" "251.41 (238.03 to 264.78)" "< 0.001"
#> [3,] "Days"        "10.47 (7.44 to 13.50)"     "< 0.001"
```

## Example: Logistic regression table

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
