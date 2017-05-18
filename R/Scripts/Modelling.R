##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
{sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load base file.
kFileVersion <- "2.2"
kFileName <- paste0(kDataDir, "gmax_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)

###############################################
## Model "gha" using "SI.h100" as predictor. ##
###############################################
## Load required libraries.
library("mgcv")

## Evaluate models.
gam_gha_h100 <- gam(data = bart.clean,
                    formula = gha ~ s(h100, k = 5))
gam_gha_h100.EKL.I<- gam(data = bart.clean,
                         formula = gha ~ s(h100.EKL.I, k = 5))
gam_gha_SI.h100 <- gam(data = bart.clean,
                       formula = gha ~ s(SI.h100, k = 26))

## BEGIN TESTING ##
graphics.off()
dev.new()
plot.gam(x = gam_gha_h100,
         pages = 1,
         seWithMean = TRUE,
         all.terms = TRUE)
## END TESTING ##
