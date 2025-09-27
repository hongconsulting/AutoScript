pkgname <- "AutoScript"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('AutoScript')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AS.basetable.TTE")
### * AS.basetable.TTE

flush(stderr()); flush(stdout())

### Name: AS.basetable.TTE
### Title: Baseline characteristics table functions
### Aliases: AS.basetable.TTE

### ** Examples

library(AutoScript)
library(survival)
data <- survival::veteran
table1 <- AS.basetable.create(group = data$trt - 1,
                              name = c("Control", "Experimental"))
table1 <- AS.basetable.linear("Age (years), mean \u00B1 SD", data$age,
                              table1, digits.fixed = 1)
table1 <- AS.basetable.blank("Histology:", table1)
table1 <- AS.basetable.binary("- Non-small cell, n (%)",
                              data$celltype != "smallcell", table1)
table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)",
                              data$celltype == "adeno", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Squamous, n (%)",
                              data$celltype == "squamous", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Large cell, n (%)",
                              data$celltype == "large", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("- Small cell, n (%)",
                              data$celltype == "smallcell", table1,
                              p.values = F)
print(table1$table)



cleanEx()
nameEx("AS.basetable.binary")
### * AS.basetable.binary

flush(stderr()); flush(stdout())

### Name: AS.basetable.binary
### Title: Baseline characteristics table functions
### Aliases: AS.basetable.binary

### ** Examples

library(AutoScript)
library(survival)
data <- survival::veteran
table1 <- AS.basetable.create(group = data$trt - 1,
                              name = c("Control", "Experimental"))
table1 <- AS.basetable.linear("Age (years), mean \u00B1 SD", data$age,
                              table1, digits.fixed = 1)
table1 <- AS.basetable.blank("Histology:", table1)
table1 <- AS.basetable.binary("- Non-small cell, n (%)",
                              data$celltype != "smallcell", table1)
table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)",
                              data$celltype == "adeno", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Squamous, n (%)",
                              data$celltype == "squamous", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Large cell, n (%)",
                              data$celltype == "large", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("- Small cell, n (%)",
                              data$celltype == "smallcell", table1,
                              p.values = F)
print(table1$table)



cleanEx()
nameEx("AS.basetable.blank")
### * AS.basetable.blank

flush(stderr()); flush(stdout())

### Name: AS.basetable.blank
### Title: Baseline characteristics table functions
### Aliases: AS.basetable.blank

### ** Examples

library(AutoScript)
library(survival)
data <- survival::veteran
table1 <- AS.basetable.create(group = data$trt - 1,
                              name = c("Control", "Experimental"))
table1 <- AS.basetable.linear("Age (years), mean \u00B1 SD", data$age,
                              table1, digits.fixed = 1)
table1 <- AS.basetable.blank("Histology:", table1)
table1 <- AS.basetable.binary("- Non-small cell, n (%)",
                              data$celltype != "smallcell", table1)
table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)",
                              data$celltype == "adeno", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Squamous, n (%)",
                              data$celltype == "squamous", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Large cell, n (%)",
                              data$celltype == "large", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("- Small cell, n (%)",
                              data$celltype == "smallcell", table1,
                              p.values = F)
print(table1$table)



cleanEx()
nameEx("AS.basetable.create")
### * AS.basetable.create

flush(stderr()); flush(stdout())

### Name: AS.basetable.create
### Title: Baseline characteristics table functions
### Aliases: AS.basetable.create

### ** Examples

library(AutoScript)
library(survival)
data <- survival::veteran
table1 <- AS.basetable.create(group = data$trt - 1,
                              name = c("Control", "Experimental"))
table1 <- AS.basetable.linear("Age (years), mean \u00B1 SD", data$age,
                              table1, digits.fixed = 1)
table1 <- AS.basetable.blank("Histology:", table1)
table1 <- AS.basetable.binary("- Non-small cell, n (%)",
                              data$celltype != "smallcell", table1)
table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)",
                              data$celltype == "adeno", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Squamous, n (%)",
                              data$celltype == "squamous", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Large cell, n (%)",
                              data$celltype == "large", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("- Small cell, n (%)",
                              data$celltype == "smallcell", table1,
                              p.values = F)
print(table1$table)



cleanEx()
nameEx("AS.basetable.linear")
### * AS.basetable.linear

flush(stderr()); flush(stdout())

### Name: AS.basetable.linear
### Title: Baseline characteristics table functions
### Aliases: AS.basetable.linear

### ** Examples

library(AutoScript)
library(survival)
data <- survival::veteran
table1 <- AS.basetable.create(group = data$trt - 1,
                              name = c("Control", "Experimental"))
table1 <- AS.basetable.linear("Age (years), mean \u00B1 SD", data$age,
                              table1, digits.fixed = 1)
table1 <- AS.basetable.blank("Histology:", table1)
table1 <- AS.basetable.binary("- Non-small cell, n (%)",
                              data$celltype != "smallcell", table1)
table1 <- AS.basetable.binary("  - Adenocarcinoma, n (%)",
                              data$celltype == "adeno", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Squamous, n (%)",
                              data$celltype == "squamous", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("  - Large cell, n (%)",
                              data$celltype == "large", table1,
                              subset.mask = data$celltype != "smallcell")
table1 <- AS.basetable.binary("- Small cell, n (%)",
                              data$celltype == "smallcell", table1,
                              p.values = F)
print(table1$table)



cleanEx()
nameEx("AS.format")
### * AS.format

flush(stderr()); flush(stdout())

### Name: AS.format
### Title: Format regression results into manuscript-ready tables
### Aliases: AS.format

### ** Examples

library(AutoScript)
library(survival)
data <- survival::veteran
fit <- coxph(Surv(data$time, data$status) ~ as.factor(data$trt))
table2 <- AS.format(fit, name = "Treatment")
print(table2)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
