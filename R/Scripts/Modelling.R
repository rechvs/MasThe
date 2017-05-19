##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
{sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kFileVersion <- "2.2"
kFileName <- paste0(kDataDir, "gmax_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)

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
