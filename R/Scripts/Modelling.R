##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kFileVersion <- "2.2"
kFileName <- paste0(kDataDir, "gmax_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)
models <- vector(mode = "list")
formulas <- vector(mode = "list")
start.vals <- vector(mode = "list")

##########
## GAMs ##
##########
## Modelling preamble ##
library("mgcv")  ## Load required libraries.
## n <- 3  ## Number of models to be stored.
## models <- vector(mode = "list", length = n)  ## Create empty list of correct length in which to store the models.
## Evaluate and store models.
models <- list("gam_gha_sh100" = gam(data = bart.clean,
                                   formula = gha ~ s(h100, k = 5)),
               "gam_gha_sh100.EKL.I" = gam(data = bart.clean,
                                         formula = gha ~ s(h100.EKL.I, k = 5)),
               "gam_gha_sSI.h100" = gam(data = bart.clean,
                                        formula = gha ~ s(SI.h100, k = 26)))

############
## Sterba ##
############
## Modelling preamble.
library(package = "nls2")
## Setup for model "Sterba_dgGmax".
## formulas[["Sterba_dgGmax"]] <- as.formula(object = "dg ~ 1 / (a0 * (h100 ^ a1) * nha + b0 * (h100 ^ b1))")
## start.vals[["Sterba_dgGmax"]] <- expand.grid("a0" = c(-2, 10),
                                             ## "a1" = c(-2, 10),
                                             ## "b0" = c(-2, 10),
                                             ## "b1" = c(-2, 10))
## Setup for model "Sterba_Gmax".
formulas[["Sterba_Gmax"]] <- as.formula(object = "gha ~ pi / (16 * a0 * b0 * (h100 ^(a1 + b1)))")
start.vals[["Sterba_Gmax"]] <- expand.grid("a0" = c(4 * 10 -6, 1),  ## cp. Wördehoff (2016), tab. 3.6
                                           "a1" = c(0, 1),
                                           "b0" = c(0, 1),
                                           "b1" = c(-2, 2))
## Evaluate and store models.
for (cur.formula.name in names(x = formulas)) {
    models[[cur.formula.name]] <- nls2(formula = formulas[[cur.formula.name]],
                                       data = bart.clean,
                                       start = start.vals[[cur.formula.name]])
}
## Print model summaries.
print.cor <- FALSE
## print.cor <- TRUE
for (cur.model.name in names(x = models)) {
    print(x = paste0("Model: ", cur.model.name))
    print(x = summary(object = models[[cur.model.name]],
                      correlation = print.cor))
}
