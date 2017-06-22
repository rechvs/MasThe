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
## In this version, an additional data frame "bart.clean" is created which is a subset of "bart", excluding all invalid data (see below for details).
kBaseFileVersion <- "1.8"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "1.9"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create untampered source version of "bart.clean".
bart.clean <- bart
## Exclude all lines in which "bart[["art"]] != 511".
bart.clean <- bart.clean[bart.clean[["art"]] == 511, ]
## Exclude all lines in which "bart.clean[["ksha.rel"]] < 0.7".
bart.clean <- bart.clean[bart.clean[["ksha.rel"]] >= 0.7, ]
## Exclude all consecutive measurements for a given "edvid" if "bart.clean[["gha.rel.cha"]] < 0".
names.vec <- NULL
for (parcel in levels(bart.clean[["edvid"]])) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean[bart.clean[["edvid"]] == parcel, ]
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
bart.clean <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean <- rbind(bart.clean,
                          eval(expr = as.name(x = name.cur)))
}
## Drop unused levels.
bart.clean <- droplevels(x = bart.clean)
## Add "bart.clean" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("bart.clean", kgmaxObjects)
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
## In this version, "bart.clean" contains an additional 28. column "h100.diff.EKL.I = h100.EKL.I - h100".
kBaseFileVersion <- "1.9"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.0"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "h100.diff.EKL.I".
bart.clean$h100.diff.EKL.I <- bart.clean$h100.EKL.I - bart.clean$h100
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
## In this version, "bart.clean" contains an additional 29. column "ln.nha = log(x = nha, base = exp(x = 1))" and an additional 30. column "ln.dh = log(x = dg, base = exp(x = 1))".
kBaseFileVersion <- "2.0"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.1"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ln.nha".
bart.clean$ln.nha <- log(x = bart.clean$nha, base = exp(x = 1))
## Calculate "ln.dg".
bart.clean$ln.dg <- log(x = bart.clean$dg, base = exp(x = 1))
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
## In this version, "bart.clean" contains an additional 31. column "log.nha = log10(x = nha)" and an additional 32. column "log.dh = log10(x = dg)".
kBaseFileVersion <- "2.1"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.2"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "log.nha".
bart.clean$log.nha <- log10(x = bart.clean$nha)
## Calculate "log.dg".
bart.clean$log.dg <- log10(x = bart.clean$dg)
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
## In this version, "bart.clean" contains an additional 33. column "ksha.diff" which holds the difference in "ksha" between the current and the previous measurement.
kBaseFileVersion <- "2.2"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.3"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ksha.diff".
for (parcel in levels(bart.clean$edvid)) {
    ksha.cur.par <- bart.clean$ksha[bart.clean$edvid == parcel]
    bart.clean$ksha.diff[bart.clean$edvid == parcel] <- c(diff(x = c(0, ksha.cur.par)))  ## "ksha" of year 0 is taken as 0
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
## In this version, "bart.clean" contains an additional 34. column "ksha.rel.cha" which holds the relative change of "ksha" between the previous and the current measurement relative to previous measurement.
kBaseFileVersion <- "2.3"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.4"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ksha.rel.cha".
for (parcel in levels(bart.clean$edvid)) {
    ksha.cur.par <- bart.clean$ksha[bart.clean$edvid == parcel]
    ksha.cur.par <- ksha.cur.par[1:length(ksha.cur.par)-1]  ## Remove last element since it is not necessary for the calculation.
    ksha.diff <- bart.clean$ksha.diff[bart.clean$edvid == parcel]
    ksha.diff <- ksha.diff[2:length(ksha.diff)]  ## Remove first element since it is not necessary for the calculation.
    bart.clean$ksha.rel.cha[bart.clean$edvid == parcel] <- c(NA, ksha.diff / ksha.cur.par)  ## First element of vector replaced by NA since its calculation would require dividing by 0.
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
## In this version, "bart.clean" contains an additional 36. column "jahr" which holds the value of "auf$jahr" for the given combination of "edvid" and "auf".
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
    index.bart.clean <- which(x = bart.clean$"edvid" == cur.edvid & bart.clean$"auf" == cur.auf)
    bart.clean$"jahr"[index.bart.clean] <- cur.jahr
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
## In this version, "bart.clean" contains an additional 37. column "ghaa.cum" which holds the cumulative sum of "ghaa" for the respective "edvid".
kBaseFileVersion <- "2.5"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.6"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "ghaa.cum".
ghaa.cum <- NULL
for (cur.edvid in levels(bart.clean$"edvid")) {
    ghaa.subset <- bart.clean$"ghaa"[bart.clean$"edvid" == cur.edvid]
    ghaa.subset[is.na(x = ghaa.subset)] <- 0  ## Replace NA manually, to prevent "cumsum" from having to deal with them.
    ghaa.cum <- c(ghaa.cum,
                  cumsum(x = ghaa.subset))
}
bart.clean$"ghaa.cum" <- ghaa.cum
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
## In this version, "bart.clean" contains an additional 38. column "age.class" which holds the age class (with a total of 7 age classes) of the given row.
kBaseFileVersion <- "2.6"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kFileVersion <- "2.7"
kFileName <- paste0(kDataDir,"gmax_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Calculate "age.class".
bart.clean[["age.class"]] <- cut(x = bart.clean[["alt"]], breaks = 7, include.lowest = TRUE)
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.before))
