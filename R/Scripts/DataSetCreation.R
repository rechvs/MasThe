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
## In this version, "bart" contains an additional 21. column "ksha.sum.edvid.auf" holding the sum of "ksha" for each combination of "edvid" and "auf".
kBaseFileVersion <- "1.0"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.1"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ksha.sum.edvid.auf" and store in data frame "ksha.sums".
ksha.sums <- aggregate(x = list(ksha.sum.edvid.auf = bart$ksha),
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
## In this version, "bart" contains an additional 22. column "ksha.rel" holding the relative portion of "ksha" of each combination of "edvid", "auf", and "art" based on "ksha.sum.edvid.auf" for each combination of "edvid" and "auf".
kBaseFileVersion <- "1.1"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.2"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ksha.rel".
bart$ksha.rel <- bart$ksha / bart$ksha.sum.edvid.auf
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.3.RData". ##
##############################
## Based on version 1.2.
## In this version, "bart" contains an additional 23. column "nhaa.rel" = "nhaa" / "nha".
kBaseFileVersion <- "1.2"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.3"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "nhaa.rel".
bart$nhaa.rel <- bart$nhaa / bart$nha
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.4.RData". ##
##############################
## Based on version 1.3.
## In this version, the following columns of "bart" are transformed into factors:
## - edvid (1.)
## - art (3.)
kBaseFileVersion <- "1.3"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.4"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Transform columns.
bart$edvid <- as.factor(bart$edvid)
bart$art <- as.factor(bart$art)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.5.RData". ##
##############################
## Based on version 1.4.
## In this version, "bart" contains an additional 24. column "SI.h100" which holds the stand index calculated with the function by Nagel (see email by Matthias Schmidt from 2017-04-27 12:06).
kBaseFileVersion <- "1.4"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.5"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "SI_h100".
## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))  ## Original function (see email by Matthias Schmidt from 2017-04-27 12:06).
bart$SI.h100 <- (bart$h100 + 49.87200 - 7.33090 * log(x = bart$alt) - 0.77338 * ((log(x = bart$alt))^2.0))/(0.52684 + 0.10542 * log(x = bart$alt))
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.6.RData". ##
##############################
## Based on version 1.5.
## In this version, "bart" contains an additional 25. column "h100.EKL.I" which holds h100 for a given age if the stand were EKL I.
kBaseFileVersion <- "1.5"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.6"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "h100.EKL.I" based on the function by Nagel 1999 solved for "h100".
## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))  ## Original function (see email by Matthias Schmidt from 2017-04-27 12:06).
SI.h100.EKL.I <- 33.3  ##  This value should be h100 at age 100 (i.e., SI.h100) for EKL I., moderate thinning.
bart$h100.EKL.I <- SI.h100.EKL.I * (0.52684 + 0.10542 * log(x = bart$alt)) - 49.872 + 7.3309 * log(x = bart$alt) + 0.77338 * (log(x = bart$alt))^2
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.7.RData". ##
##############################
## Based on version 1.6.
## In this version, "bart" contains an additional 26. column "gha.diff" which holds the difference in "gha" between the current and the previous measurement, calculated separately for each combination of "edvid" and "art".
kBaseFileVersion <- "1.6"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.7"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "gha.diff".
for (parcel in levels(bart$edvid)) {
    for (species in levels(bart$art)) {
        gha.cur.par <- bart$gha[bart$edvid == parcel & bart$art == species]
        bart$gha.diff[bart$edvid == parcel & bart$art == species] <- c(diff(x = c(0, gha.cur.par)))  ## "gha" of year 0 is taken as 0
    }
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)

##############################
## Create "gmax_1.8.RData". ##
##############################
## Based on version 1.7.
## In this version, "bart" contains an additional 27. column "gha.rel.cha" which holds the relative change of "gha" between the previous and the current measurement relative to previous measurement, calculated separately for each combination of "edvid" and "art".
kBaseFileVersion <- "1.7"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.8"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "gha.rel.cha".
for (parcel in levels(bart$edvid)) {
    for (species in levels(bart$art)) {
        gha.cur.par <- bart$gha[bart$edvid == parcel & bart$art == species]
        gha.cur.par <- gha.cur.par[1:length(gha.cur.par)-1]  ## Remove last element since it is not necessary for the calculation.
        gha.diff <- bart$gha.diff[bart$edvid == parcel & bart$art == species]
        gha.diff <- gha.diff[2:length(gha.diff)]  ## Remove first element since it is not necessary for the calculation.
        bart$gha.rel.cha[bart$edvid == parcel & bart$art == species] <- c(NA, gha.diff / gha.cur.par)  ## First element of vector replaced by NA since its calculation would require dividing by 0.
    }
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
