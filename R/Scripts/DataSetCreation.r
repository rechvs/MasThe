## Preamble
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"

##############################
## Create "gmax_1.0.RData". ##
##############################
## This version is an untamperd copy of the original version of "gmax.RData" (see email by Matthias Schmidt from 2017-04-27).
kFileVersion <- "1.0"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = paste0(kDataDir, "gmax.RData"), verbose = TRUE)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.1.RData". ##
##############################
## Based on version 1.0.
## In this version, "bart" contains an additional 21. column "sum.ksha" holding the sum of "ksha" for each combination of "edvid" and "auf".
kBaseFileVersion <- "1.0"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.1"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "sum.ksha" and store in data frame "ksha.sums".
ksha.sums <- aggregate(x = list(sum.ksha = bart$ksha),
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

##############################
## Create "gmax_1.2.RData". ##
##############################
## Based on version 1.1.
## In this version, "bart" contains an additional 22. column "rel.ksha" holding the relative portion of "ksha" of each combination of "edvid", "auf", and "art" based on "sum.ksha" for each combination of "edvid" and "auf".
kBaseFileVersion <- "1.1"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.2"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "rel.ksha" and store in column "rel.ksha".
bart$rel.ksha <- bart$ksha / bart$sum.ksha
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.3.RData". ##
##############################
## Based on version 1.2.
## In this version, "bart" contains an additional 23. column "rel.nhaa" = "nhaa" / "nha".
kBaseFileVersion <- "1.2"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.3"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "rel.nhaa".
bart$rel.nhaa <- bart$nhaa / bart$nha
