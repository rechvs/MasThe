## Preamble
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "R/Data/"
kgmaxObjects <- load(file = paste0(kDataDir, "gmax.RData"), verbose = TRUE)

##############################
## Create "gmax_1.0.RData". ##
##############################
## This version is an untamperd copy of the original version of "gmax.RData" (see email by Matthias Schmidt from 2017-04-27).
kFileVersion <- "1.0"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.1.RData". ##
##############################
## Based on version 1.0.
## In this version, "bart" contains an additional 21. column "sumksha" holding the sum of "ksha" for each combination of "edvid" and "auf".
kBaseFileVersion <- "1.0"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.1"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "sumksha" and store in data frame "ksha.sums".
ksha.sums <- aggregate(x = list(sumksha = bart$ksha),
                       by = list(edvid = bart$edvid,
                                 auf = bart$auf),
                       FUN = sum)
## Merge "ksha.sums" and "bart".
bart <- merge(x = bart,
              y = ksha.sums,
              by.x = c("edvid","auf"),
              by.y = c("edvid","auf"))
## Order "bart" by "edvid" and "auf".
bart <- bart[order(bart$edvid,bart$auf),]
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
