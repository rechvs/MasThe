##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"

#############################
## Create "gmax_1.0.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## This version is an untamperd copy of the original version of "gmax.RData" (see email by Matthias Schmidt from 2017-04-27).
kFileVersion <- "1.0"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = paste0(kDataDir, "gmax.RData"), verbose = TRUE)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.1.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.2.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.3.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.4.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.5.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
bart$SI.h100 <- (bart$h100 + 49.87200 - 7.33090 * log(x = bart$alt) - 0.77338 * ((log(x = bart$alt))^2.0)) / (0.52684 + 0.10542 * log(x = bart$alt))
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.6.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
SI.h100.EKL.I <- 35.1  ##  This value is h_100 at age 100 (i.e., SI.h100) for EKL I of spruce, moderate thinning (source: Schober (1995)).
bart$h100.EKL.I <- SI.h100.EKL.I * (0.52684 + 0.10542 * log(x = bart$alt)) - 49.872 + 7.3309 * log(x = bart$alt) + 0.77338 * (log(x = bart$alt))^2
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.7.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.8.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
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
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_1.9.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 1.8.
## In version, an additional data frame "bart.clean.1.0" is created which is a subset of "bart", excluding certain data (see below for details).
kBaseFileVersion <- "1.8"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.9"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create untampered source version of "bart.clean.1.0".
bart.clean.1.0 <- bart
## Exclude all lines in which "bart.clean.1.0[["art"]] != 511".
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["art"]] == 511, ]
## Exclude all lines in which "bart.clean.1.0[["ksha.rel"]] < 0.7".
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["ksha.rel"]] >= 0.7, ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "4665111A"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "4665111A", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "4665112B"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "4665112B", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "4665113B"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "4665113B", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "4675111A"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "4675111A", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "4675112A"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "4675112A", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "4675113A"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "4675113A", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "4675114A"" [reason: treatment (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "4675114A", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "J6351111"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "J6351111", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "J6351121"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "J6351121", ]
## Exclude all lines in which "bart.clean.1.0[["edvid"]] == "J6351131"" [reason: low plant density (according to archive information)].
bart.clean.1.0 <- bart.clean.1.0[bart.clean.1.0[["edvid"]] != "J6351131", ]
## Exclude all consecutive measurements for a given "edvid" if "bart.clean.1.0[["gha.rel.cha"]] < 0".
names.vec <- NULL
for (parcel in levels(bart.clean.1.0[["edvid"]])) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean.1.0[bart.clean.1.0[["edvid"]] == parcel, ]
    auf.vec <- parcel.subset[["auf"]][parcel.subset[["gha.rel.cha"]] < 0]
    if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
        auf.mark <- min(auf.vec, na.rm = TRUE)
        parcel.subset <- parcel.subset[parcel.subset[["auf"]] < auf.mark, ]
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    }
}
## Create new data frame from objects created by "for" loop above.
bart.clean.1.0 <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean.1.0 <- rbind(bart.clean.1.0,
                          eval(expr = as.name(x = name.cur)))
}
## Drop unused levels.
bart.clean.1.0 <- droplevels(x = bart.clean.1.0)
## Add "bart.clean.1.0" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("bart.clean.1.0", kgmaxObjects)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.0.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 1.9.
## In this version, "bart.clean.1.0" contains an additional 28. column "h100.diff.EKL.I = h100.EKL.I - h100".
kBaseFileVersion <- "1.9"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.0"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "h100.diff.EKL.I".
bart.clean.1.0$h100.diff.EKL.I <- bart.clean.1.0$h100.EKL.I - bart.clean.1.0$h100
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.1.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.0.
## In this version, "bart.clean.1.0" contains an additional 29. column "ln.nha = log(x = nha, base = exp(x = 1))" and an additional 30. column "ln.dh = log(x = dg, base = exp(x = 1))".
kBaseFileVersion <- "2.0"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.1"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ln.nha".
bart.clean.1.0$ln.nha <- log(x = bart.clean.1.0$nha, base = exp(x = 1))
## Calculate "ln.dg".
bart.clean.1.0$ln.dg <- log(x = bart.clean.1.0$dg, base = exp(x = 1))
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.2.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.1.
## In this version, "bart.clean.1.0" contains an additional 31. column "log.nha = log10(x = nha)" and an additional 32. column "log.dh = log10(x = dg)".
kBaseFileVersion <- "2.1"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.2"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "log.nha".
bart.clean.1.0$log.nha <- log10(x = bart.clean.1.0$nha)
## Calculate "log.dg".
bart.clean.1.0$log.dg <- log10(x = bart.clean.1.0$dg)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.3.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.2.
## In this version, "bart.clean.1.0" contains an additional 33. column "ksha.diff" which holds the difference in "ksha" between the current and the previous measurement.
kBaseFileVersion <- "2.2"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.3"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ksha.diff".
for (parcel in levels(bart.clean.1.0$edvid)) {
    ksha.cur.par <- bart.clean.1.0$ksha[bart.clean.1.0$edvid == parcel]
    bart.clean.1.0$ksha.diff[bart.clean.1.0$edvid == parcel] <- c(diff(x = c(0, ksha.cur.par)))  ## "ksha" of year 0 is taken as 0
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.4.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.3.
## In this version, "bart.clean.1.0" contains an additional 34. column "ksha.rel.cha" which holds the relative change of "ksha" between the previous and the current measurement relative to previous measurement.
kBaseFileVersion <- "2.3"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.4"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ksha.rel.cha".
for (parcel in levels(bart.clean.1.0$edvid)) {
    ksha.cur.par <- bart.clean.1.0$ksha[bart.clean.1.0$edvid == parcel]
    ksha.cur.par <- ksha.cur.par[1:length(ksha.cur.par)-1]  ## Remove last element since it is not necessary for the calculation.
    ksha.diff <- bart.clean.1.0$ksha.diff[bart.clean.1.0$edvid == parcel]
    ksha.diff <- ksha.diff[2:length(ksha.diff)]  ## Remove first element since it is not necessary for the calculation.
    bart.clean.1.0$ksha.rel.cha[bart.clean.1.0$edvid == parcel] <- c(NA, ksha.diff / ksha.cur.par)  ## First element of vector replaced by NA since its calculation would require dividing by 0.
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.5.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.4.
## In this version, "bart.clean.1.0" contains an additional 36. column "jahr" which holds the value of "auf$jahr" for the given combination of "edvid" and "auf".
kBaseFileVersion <- "2.4"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.5"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create "jahr".
for (cur.row.index in 1:nrow(auf)) {
    cur.row <- auf[cur.row.index, ]
    cur.edvid <- cur.row$"edvid"
    cur.auf <- as.numeric(cur.row$"auf")
    cur.jahr <- as.numeric(cur.row$"jahr")
    index.bart.clean.1.0 <- which(x = bart.clean.1.0$"edvid" == cur.edvid & bart.clean.1.0$"auf" == cur.auf)
    bart.clean.1.0$"jahr"[index.bart.clean.1.0] <- cur.jahr
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.6.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.5.
## In this version, "bart.clean.1.0" contains an additional 37. column "ghaa.cum" which holds the cumulative sum of "ghaa" for the respective "edvid".
kBaseFileVersion <- "2.5"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.6"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ghaa.cum".
ghaa.cum <- NULL
for (cur.edvid in levels(bart.clean.1.0$"edvid")) {
    ghaa.subset <- bart.clean.1.0$"ghaa"[bart.clean.1.0$"edvid" == cur.edvid]
    ghaa.subset[is.na(x = ghaa.subset)] <- 0  ## Replace NA manually, to prevent "cumsum" from having to deal with them.
    ghaa.cum <- c(ghaa.cum,
                  cumsum(x = ghaa.subset))
}
bart.clean.1.0$"ghaa.cum" <- ghaa.cum
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.7.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.6.
## In this version, "bart.clean.1.0" contains an additional 38. column "age.class" which holds the age class (with a total of 7 age classes) of the given row.
kBaseFileVersion <- "2.6"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.7"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "age.class".
bart.clean.1.0[["age.class"]] <- cut(x = bart.clean.1.0[["alt"]], breaks = 7, include.lowest = TRUE)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.8.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.7.
## In this version, an additional data frame "edvid.archive.suppl.info" is created which contains all levels of "bart.clean.1.0[["edvid"]]" and the corresponding values of "vers[["forstamt"]]" and "vers[["abt"]]" and "parz[["BESONDERHEITEN"]]".
kBaseFileVersion <- "2.7"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.8"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Extract all levels of "bart.clean.1.0[["edvid"]]".
edvid.archive.suppl.info <- data.frame("edvid" = levels(x = bart.clean.1.0[["edvid"]]))
## Find out which elements of "vers[["vers"]]" match the elements of "edvid.archive.suppl.info[["edvid"]]".
vers.matches <- match(x = substr(x = edvid.archive.suppl.info[["edvid"]],
                                 start = 0,
                                 stop = 6),
                      table = vers[["vers"]])
## Find out which elements of "parz[["edvid"]]" match the elements of "edvid.archive.suppl.info[["edvid"]]".
parz.matches <- match(x = edvid.archive.suppl.info[["edvid"]],
                      table = parz[["edvid"]])
## Add columns "forstamt", "abt" and "BESONDERHEITEN" to "edvid.archive.suppl.info".
edvid.archive.suppl.info[["forstamt"]] <- vers[["forstamt"]][vers.matches]
edvid.archive.suppl.info[["abt"]] <- vers[["abt"]][vers.matches]
edvid.archive.suppl.info[["BESONDERHEITEN"]] <- parz[["BESONDERHEITEN"]][parz.matches]
## Add "edvid.archive.suppl.info" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("edvid.archive.suppl.info", kgmaxObjects)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_2.9.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.8.
## In this version, an additional data frame "bart.clean.1.1" is created which is a subset of "bart.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.clean.1.1[["ghaa"]] > 0.10 * bart.clean.1.1[["gha"]] ".
kBaseFileVersion <- "2.8"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.9"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create untampered source version of "bart.clean.1.1".
bart.clean.1.1 <- bart.clean.1.0
## Exclude all consecutive measurements for a given "edvid" if "bart.clean.1.1[["ghaa"]] > 0.10 * bart.clean.1.1[["gha"]] ".
names.vec <- NULL
for (parcel in levels(x = bart.clean.1.1[["edvid"]])) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean.1.1[bart.clean.1.1[["edvid"]] == parcel, ]
    auf.vec <- parcel.subset[["auf"]][parcel.subset[["ghaa"]] > 0.10 * parcel.subset[["gha"]]]
    if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
        auf.mark <- min(auf.vec, na.rm = TRUE)
        parcel.subset <- parcel.subset[parcel.subset[["auf"]] < auf.mark, ]
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    }
}
## Create new data frame from objects created by "for" loop above.
bart.clean.1.1 <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean.1.1 <- rbind(bart.clean.1.1,
                          eval(expr = as.name(x = name.cur)))
}
## Drop unused levels.
bart.clean.1.1 <- droplevels(x = bart.clean.1.1)
## Add "bart.clean.1.1" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("bart.clean.1.1", kgmaxObjects)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_3.0.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 2.9.
## In this version, an additional data frame "bart.clean.1.2" is created which is a subset of "bart.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.clean.1.2[["ghaa"]] > 0.20 * bart.clean.1.2[["gha"]] ".
kBaseFileVersion <- "2.9"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "3.0"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create untampered source version of "bart.clean.1.2".
bart.clean.1.2 <- bart.clean.1.0
## Exclude all consecutive measurements for a given "edvid" if "bart.clean.1.2[["ghaa"]] > 0.20 * bart.clean.1.2[["gha"]] ".
names.vec <- NULL
for (parcel in levels(x = bart.clean.1.2[["edvid"]])) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean.1.2[bart.clean.1.2[["edvid"]] == parcel, ]
    auf.vec <- parcel.subset[["auf"]][parcel.subset[["ghaa"]] > 0.20 * parcel.subset[["gha"]]]
    if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
        auf.mark <- min(auf.vec, na.rm = TRUE)
        parcel.subset <- parcel.subset[parcel.subset[["auf"]] < auf.mark, ]
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    }
}
## Create new data frame from objects created by "for" loop above.
bart.clean.1.2 <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean.1.2 <- rbind(bart.clean.1.2,
                          eval(expr = as.name(x = name.cur)))
}
## Drop unused levels.
bart.clean.1.2 <- droplevels(x = bart.clean.1.2)
## Add "bart.clean.1.2" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("bart.clean.1.2", kgmaxObjects)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_3.1.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 3.0.
## In this version, an additional data frame "bart.clean.1.3" is created which is a subset of "bart.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.clean.1.3[["ghaa"]] > 0.05 * bart.clean.1.3[["gha"]] ".
kBaseFileVersion <- "3.0"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "3.1"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create untampered source version of "bart.clean.1.3".
bart.clean.1.3 <- bart.clean.1.0
## Exclude all consecutive measurements for a given "edvid" if "bart.clean.1.3[["ghaa"]] > 0.05 * bart.clean.1.3[["gha"]] ".
names.vec <- NULL
for (parcel in levels(x = bart.clean.1.3[["edvid"]])) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean.1.3[bart.clean.1.3[["edvid"]] == parcel, ]
    auf.vec <- parcel.subset[["auf"]][parcel.subset[["ghaa"]] > 0.05 * parcel.subset[["gha"]]]
    if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
        auf.mark <- min(auf.vec, na.rm = TRUE)
        parcel.subset <- parcel.subset[parcel.subset[["auf"]] < auf.mark, ]
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    }
}
## Create new data frame from objects created by "for" loop above.
bart.clean.1.3 <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean.1.3 <- rbind(bart.clean.1.3,
                          eval(expr = as.name(x = name.cur)))
}
## Drop unused levels.
bart.clean.1.3 <- droplevels(x = bart.clean.1.3)
## Add "bart.clean.1.3" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("bart.clean.1.3", kgmaxObjects)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))

#############################
## Create "gmax_3.2.RData" ##
#############################
objects.before <- ls()  ## Required for clean up.
## Based on version 3.1.
## In this version, an additional data frame "bart.clean.1.4" is created which is a subset of "bart.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.clean.1.4[["ghaa"]] > 0.15 * bart.clean.1.4[["gha"]] ".
kBaseFileVersion <- "3.1"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "3.2"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create untampered source version of "bart.clean.1.4".
bart.clean.1.4 <- bart.clean.1.0
## Exclude all consecutive measurements for a given "edvid" if "bart.clean.1.4[["ghaa"]] > 0.15 * bart.clean.1.4[["gha"]] ".
names.vec <- NULL
for (parcel in levels(x = bart.clean.1.4[["edvid"]])) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean.1.4[bart.clean.1.4[["edvid"]] == parcel, ]
    auf.vec <- parcel.subset[["auf"]][parcel.subset[["ghaa"]] > 0.15 * parcel.subset[["gha"]]]
    if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
        auf.mark <- min(auf.vec, na.rm = TRUE)
        parcel.subset <- parcel.subset[parcel.subset[["auf"]] < auf.mark, ]
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    }
}
## Create new data frame from objects created by "for" loop above.
bart.clean.1.4 <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean.1.4 <- rbind(bart.clean.1.4,
                          eval(expr = as.name(x = name.cur)))
}
## Drop unused levels.
bart.clean.1.4 <- droplevels(x = bart.clean.1.4)
## Add "bart.clean.1.4" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("bart.clean.1.4", kgmaxObjects)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))
