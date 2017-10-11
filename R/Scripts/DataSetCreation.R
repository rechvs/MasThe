##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
objects.at.start <- sort(x = c(ls(), "objects.at.start"))  ## Required for cleaning up workspace after each block.

####################################
## Create "gmax_merged_1.0.RData" ##
####################################
## This version serves as the base version consisting of untampered but renamed versions of the objects contained in "gmax.RData" (see email by Matthias Schmidt from 2017-04-27) and in "gmax_bu.RData". The resulting data set contains the following objects:
## - auf.beech
## - bart.beech
## - best.beech
## - parz.beech
## - vers.beech
## - auf.spruce
## - bart.spruce
## - best.spruce
## - parz.spruce
## - vers.spruce
kFileVersion <- "1.0"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file of spruce data.
kNamesOfLoadedSpruceObjects <- load(file = paste0(kDataDir, "Spruce/gmax.RData"), verbose = TRUE)
## Assing values of old objects to new objects with ".spruce" as a name suffix.
kNamesOfSpruceObjectsToSave <- vector(mode = "character")
for (old.object.name in kNamesOfLoadedSpruceObjects) {
    new.object.name <- paste0(old.object.name, ".spruce")
    kNamesOfSpruceObjectsToSave <- c(kNamesOfSpruceObjectsToSave, new.object.name)
    assign(x = new.object.name,
           value = get(x = old.object.name))
}
## Clear workspace of old spruce objects.
rm(list = kNamesOfLoadedSpruceObjects)
## Load base file of beech data.
kNamesOfLoadedBeechObjects <- load(file = paste0(kDataDir, "Beech/gmax_bu.RData"), verbose = TRUE)
## Assing values of old objects to new objects with ".beech" as a name suffix.
kNamesOfBeechObjectsToSave <- vector(mode = "character")
for (old.object.name in kNamesOfLoadedBeechObjects) {
    new.object.name <- paste0(old.object.name, ".beech")
    kNamesOfBeechObjectsToSave <- c(kNamesOfBeechObjectsToSave, new.object.name)
    assign(x = new.object.name,
           value = get(x = old.object.name))
}
## Clear workspace of old beech objects.
rm(list = kNamesOfLoadedBeechObjects)
## Save results (in an ordered fashion).
kObjectsToSave <- c(kNamesOfBeechObjectsToSave[order(kNamesOfBeechObjectsToSave)], kNamesOfSpruceObjectsToSave[order(kNamesOfSpruceObjectsToSave)])
save(list = kObjectsToSave,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.1.RData" ##
####################################
## Based on version 1.0.
## In this version, "bart.SPECIES" contains an additional column "ksha.sum.edvid.auf" holding the sum of "ksha" for each combination of "edvid" and "auf".
kBaseFileVersion <- "1.0"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.1"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "ksha.sum.edvid.auf" and store in data frame "ksha.sums".
    ksha.sums <- aggregate(x = list(ksha.sum.edvid.auf = cur.object$ksha),
                           by = list(edvid = cur.object$edvid,
                                     auf = cur.object$auf),
                           FUN = sum)
    ## Merge "ksha.sums" and "cur.object".
    cur.object <- merge(x = cur.object,
                        y = ksha.sums,
                        by.x = c("edvid","auf"),
                        by.y = c("edvid","auf"))
    ## Order "cur.object" by "edvid" and "auf".
    cur.object <- cur.object[order(cur.object$edvid,cur.object$auf),]
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.2.RData" ##
####################################
## Based on version 1.1.
## In this version, "bart.SPECIES" contains an additional column "ksha.rel" holding the relative portion of "ksha" of each combination of "edvid", "auf", and "art" based on "ksha.sum.edvid.auf" for each combination of "edvid" and "auf".
kBaseFileVersion <- "1.1"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.2"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "ksha.rel".
    cur.object[["ksha.rel"]] <- cur.object[["ksha"]] / cur.object[["ksha.sum.edvid.auf"]]
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.3.RData" ##
####################################
## Based on version 1.2.
## In this version, "bart.SPECIES" contains an additional column "nhaa.rel" = "nhaa" / "nha".
kBaseFileVersion <- "1.2"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.3"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "nhaa.rel".
    cur.object[["nhaa.rel"]] <- cur.object[["nhaa"]] / cur.object[["nha"]]
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.4.RData" ##
####################################
## Based on version 1.3.
## In this version, the following columns of "bart.SPECIES" are transformed into factors:
## - edvid
## - art
kBaseFileVersion <- "1.3"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.4"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Transform columns.
    cur.object[["edvid"]] <- as.factor(x = cur.object[["edvid"]])
    cur.object[["art"]] <- as.factor(x = cur.object[["art"]])
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.5.RData" ##
####################################
## Based on version 1.4.
## In this version, "bart.SPECIES" contains an additional column "SI.h100" which holds the stand index calculated with the function by Nagel (see email by Matthias Schmidt from 2017-04-27 12:06).
kBaseFileVersion <- "1.4"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.5"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "SI_h100".
    ## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))  ## Original function (see email by Matthias Schmidt from 2017-04-27 12:06).
    cur.object[["SI.h100"]] <- (cur.object[["h100"]] + 49.87200 - 7.33090 * log(x = cur.object[["alt"]]) - 0.77338 * ((log(x = cur.object[["alt"]])) ^ 2.0)) / (0.52684 + 0.10542 * log(x = cur.object[["alt"]]))
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.6.RData" ##
####################################
## Based on version 1.5.
## In this version, "bart.SPECIES" contains an additional column "h100.EKL.I" which holds h100 for a given age if the stand were EKL I.
kBaseFileVersion <- "1.5"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.6"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign species-specific value for "SI.h100.EKL.I".
    if (grepl(pattern = "beech", x = cur.object.name)) {
        SI.h100.EKL.I <- 32.4  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of beech, moderate thinning (source: Schober (1995)).
    }
    if (grepl(pattern = "spruce", x = cur.object.name)) {
        SI.h100.EKL.I <- 35.1  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of spruce, moderate thinning (source: Schober (1995)).
    }
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "h100.EKL.I" based on the function by Nagel 1999 solved for "h100".
    ## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))  ## Original function (see email by Matthias Schmidt from 2017-04-27 12:06).
    cur.object[["h100.EKL.I"]] <- SI.h100.EKL.I * (0.52684 + 0.10542 * log(x = cur.object[["alt"]])) - 49.872 + 7.3309 * log(x = cur.object[["alt"]]) + 0.77338 * (log(x = cur.object[["alt"]]))^2
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.7.RData" ##
####################################
## Based on version 1.6.
## In this version, "bart.SPECIES" contains an additional column "gha.diff" which holds the difference in "gha" between the current and the previous measurement, calculated separately for each combination of "edvid" and "art".
kBaseFileVersion <- "1.6"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.7"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "gha.diff".
    for (parcel in levels(cur.object[["edvid"]])) {
        for (species in levels(cur.object[["art"]])) {
            gha.cur.par <- cur.object[["gha"]][cur.object[["edvid"]] == parcel & cur.object[["art"]] == species]
            cur.object[["gha.diff"]][cur.object[["edvid"]] == parcel & cur.object[["art"]] == species] <- c(diff(x = c(0, gha.cur.par)))  ## "gha" of year 0 is taken as 0
        }
    }
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.8.RData" ##
####################################
## Based on version 1.7.
## In this version, "bart.SPECIES" contains an additional 27. column "gha.rel.cha" which holds the relative change of "gha" between the previous and the current measurement relative to previous measurement, calculated separately for each combination of "edvid" and "art".
kBaseFileVersion <- "1.7"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.8"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech", "bart.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "gha.rel.cha".
    for (parcel in levels(cur.object[["edvid"]])) {
        for (species in levels(cur.object[["art"]])) {
            gha.cur.par <- cur.object[["gha"]][cur.object[["edvid"]] == parcel & cur.object[["art"]] == species]
            gha.cur.par <- gha.cur.par[1:length(gha.cur.par) - 1]  ## Remove last element since it is not necessary for the calculation.
            gha.diff <- cur.object[["gha.diff"]][cur.object[["edvid"]] == parcel & cur.object[["art"]] == species]
            gha.diff <- gha.diff[2:length(gha.diff)]  ## Remove first element since it is not necessary for the calculation.
            cur.object[["gha.rel.cha"]][cur.object[["edvid"]] == parcel & cur.object[["art"]] == species] <- c(NA, gha.diff / gha.cur.par)  ## First element of vector replaced by NA since its calculation would require dividing by 0.
        }
    }
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_1.9.RData" ##
####################################
## Based on version 1.8.
## In this version, 2 additional data frames "bart.SPECIES.clean.1.0" are created, both of which are subsets of the respective "bart.SPECIES" data frame, excluding certain data (see below for details).
kBaseFileVersion <- "1.8"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "1.9"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create untampered source version "bart.species.clean.1.0".
    cur.bart.species.clean.1.0 <- get(x = paste0("bart.", cur.species.name))
    ## Exclude all lines in which "cur.bart.species.clean.1.0[["art"]] != cur.species.code".
    if (cur.species.name == "beech") {
        cur.species.code <- 211
    }
    if (cur.species.name == "spruce") {
        cur.species.code <- 511
    }
    cur.bart.species.clean.1.0 <- cur.bart.species.clean.1.0[cur.bart.species.clean.1.0[["art"]] == cur.species.code, ]
    ## Exclude all lines in which "cur.bart.species.clean.1.0[["ksha.rel"]] < 0.7".
    cur.bart.species.clean.1.0 <- cur.bart.species.clean.1.0[cur.bart.species.clean.1.0[["ksha.rel"]] >= 0.7, ]
    ## Exclude all consecutive measurements for a given "edvid" if "bart.spruce.clean.1.0[["gha.rel.cha"]] < 0".
    cur.names.vec <- NULL
    for (cur.edvid in levels(cur.bart.species.clean.1.0[["edvid"]])) {
        name.cur <- paste0("obj.", as.character(cur.edvid))
        cur.names.vec <- c(cur.names.vec, name.cur)
        cur.edvid.subset <- cur.bart.species.clean.1.0[cur.bart.species.clean.1.0[["edvid"]] == cur.edvid, ]
        auf.vec <- cur.edvid.subset[["auf"]][cur.edvid.subset[["gha.rel.cha"]] < 0]
        if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
            assign(x = make.names(names = name.cur),
                   value = cur.edvid.subset)
        } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
            auf.mark <- min(auf.vec, na.rm = TRUE)
            cur.edvid.subset <- cur.edvid.subset[cur.edvid.subset[["auf"]] < auf.mark, ]
            assign(x = make.names(names = name.cur),
                   value = cur.edvid.subset)
        }
    }
    ## Create new data frame from objects created by "for" loop above.
    cur.bart.species.clean.1.0 <- data.frame(NULL)
    for (name.cur in cur.names.vec) {
        cur.bart.species.clean.1.0 <- rbind(cur.bart.species.clean.1.0,
                                            eval(expr = as.name(x = name.cur)))
    }
    ## Drop unused levels.
    cur.bart.species.clean.1.0 <- droplevels(x = cur.bart.species.clean.1.0)
    ## Set the name for "bart.SPECIES.clean.1.0" based on "cur.species.name".
    cur.final.object.name <- paste0("bart.", cur.species.name, ".clean.1.0")
    ## Assign "cur.bart.species.clean.1.0" to "bart.SPECIES.clean.1.0".
    assign(x = cur.final.object.name,
           value = cur.bart.species.clean.1.0)
    ## Add "bart.SPECIES.clean.1.0" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(cur.final.object.name, kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.0.RData" ##
####################################
## Based on version 1.9.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "h100.diff.EKL.I = h100.EKL.I - h100".
kBaseFileVersion <- "1.9"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.0"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "h100.diff.EKL.I".
    cur.object[["h100.diff.EKL.I"]] <- cur.object[["h100"]] - cur.object[["h100.EKL.I"]]
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.1.RData" ##
####################################
## Based on version 2.0.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "ln.nha = log(x = nha, base = exp(x = 1))" and an additional column "ln.dh = log(x = dg, base = exp(x = 1))".
kBaseFileVersion <- "2.0"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.1"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "ln.nha".
    cur.object[["ln.nha"]] <- log(x = cur.object[["nha"]], base = exp(x = 1))
    ## Calculate "ln.dg".
    cur.object[["ln.dg"]] <- log(x = cur.object[["dg"]], base = exp(x = 1))
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.2.RData" ##
####################################
## Based on version 2.1.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "log.nha = log10(x = nha)" and an additional column "log.dh = log10(x = dg)".
kBaseFileVersion <- "2.1"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.2"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "log.nha".
    cur.object[["log.nha"]] <- log10(x = cur.object[["nha"]])
    ## Calculate "log.dg".
    cur.object[["log.dg"]] <- log10(x = cur.object[["dg"]])
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.3.RData" ##
####################################
## Based on version 2.2.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "ksha.diff" which holds the difference in "ksha" between the current and the previous measurement.
kBaseFileVersion <- "2.2"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.3"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "ksha.diff".
    for (parcel in levels(cur.object[["edvid"]])) {
        ksha.cur.par <- cur.object[["ksha"]][cur.object[["edvid"]] == parcel]
        cur.object[["ksha.diff"]][cur.object[["edvid"]] == parcel] <- c(diff(x = c(0, ksha.cur.par)))  ## "ksha" of year 0 is taken as 0
    }
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.4.RData" ##
####################################
## Based on version 2.3.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "ksha.rel.cha" which holds the relative change of "ksha" between the previous and the current measurement relative to previous measurement.
kBaseFileVersion <- "2.3"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.4"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "ksha.rel.cha".
    for (parcel in levels(cur.object[["edvid"]])) {
        ksha.cur.par <- cur.object[["ksha"]][cur.object[["edvid"]] == parcel]
        ksha.cur.par <- ksha.cur.par[1:length(ksha.cur.par)-1]  ## Remove last element since it is not necessary for the calculation.
        ksha.diff <- cur.object[["ksha.diff"]][cur.object[["edvid"]] == parcel]
        ksha.diff <- ksha.diff[2:length(ksha.diff)]  ## Remove first element since it is not necessary for the calculation.
        cur.object[["ksha.rel.cha"]][cur.object[["edvid"]] == parcel] <- c(NA, ksha.diff / ksha.cur.par)  ## First element of vector replaced by NA since its calculation would require dividing by 0.
    }
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.5.RData" ##
####################################
## Based on version 2.4.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "jahr" which holds the value of "auf$jahr" for the given combination of "edvid" and "auf".
kBaseFileVersion <- "2.4"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.5"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Assign data frame "auf.SPECIES".
    cur.auf.df.name <- paste0("auf.", strsplit(x = cur.object.name, split = ".", fixed = TRUE)[[1]][2])
    cur.auf.df <- get(x = cur.auf.df.name)
    ## Create "jahr".
    for (cur.row.index in 1:nrow(cur.auf.df)) {
        cur.row <- cur.auf.df[cur.row.index, ]
        cur.edvid <- cur.row[["edvid"]]
        cur.auf <- as.numeric(cur.row[["auf"]])
        cur.jahr <- as.numeric(cur.row[["jahr"]])
        index.cur.object <- which(x = cur.object[["edvid"]] == cur.edvid & cur.object[["auf"]] == cur.auf)
        cur.object[["jahr"]][index.cur.object] <- cur.jahr
    }
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.6.RData" ##
####################################
## Based on version 2.5.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "ghaa.cum" which holds the cumulative sum of "ghaa" for the respective "edvid".
kBaseFileVersion <- "2.5"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.6"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "ghaa.cum".
    ghaa.cum <- NULL
    for (cur.edvid in levels(cur.object[["edvid"]])) {
        ghaa.subset <- cur.object[["ghaa"]][cur.object[["edvid"]] == cur.edvid]
        ghaa.subset[is.na(x = ghaa.subset)] <- 0  ## Replace NA manually, to prevent "cumsum" from having to deal with them.
        ghaa.cum <- c(ghaa.cum,
                      cumsum(x = ghaa.subset))
    }
    cur.object[["ghaa.cum"]] <- ghaa.cum
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.7.RData" ##
####################################
## Based on version 2.6.
## In this version, "bart.SPECIES.clean.1.0" contains an additional column "age.class" which holds the age class (with a total of 7 age classes) of the given row.
kBaseFileVersion <- "2.6"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.7"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("bart.beech.clean.1.0", "bart.spruce.clean.1.0")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "age.class".
    cur.object[["age.class"]] <- cut(x = cur.object[["alt"]], breaks = 7, ordered_result = TRUE)
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.8.RData" ##
####################################
## Based on version 2.7.
## In this version, an additional data frame "edvid.archive.suppl.info" is created which contains all levels of "bart.spruce.clean.1.0[["edvid"]]" and the corresponding values of "vers[["forstamt"]]" and "vers[["abt"]]" and "parz[["BESONDERHEITEN"]]".
kBaseFileVersion <- "2.7"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.8"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Assign current "bart.SPECIES.clean.1.0".
    cur.bart.object <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Assign current "vers.SPECIES".
    cur.vers.object <- get(x = paste0("vers.", cur.species.name))
    ## Assign current "parz.SPECIES".
    cur.parz.object <- get(x = paste0("parz.", cur.species.name))
    ## Extract all levels of "cur.bart.object[["edvid"]]".
    edvid.archive.suppl.info <- data.frame("edvid" = levels(x = cur.bart.object[["edvid"]]))
    ## Find out which elements of "cur.vers.object[["vers"]]" match the elements of "edvid.archive.suppl.info[["edvid"]]".
    vers.matches <- match(x = substr(x = edvid.archive.suppl.info[["edvid"]],
                                     start = 0,
                                     stop = 6),
                          table = cur.vers.object[["vers"]])
    ## Find out which elements of "cur.parz.object[["edvid"]]" match the elements of "edvid.archive.suppl.info[["edvid"]]".
    parz.matches <- match(x = edvid.archive.suppl.info[["edvid"]],
                          table = cur.parz.object[["edvid"]])
    ## Add columns "forstamt", "abt" and "BESONDERHEITEN" to "edvid.archive.suppl.info".
    edvid.archive.suppl.info[["forstamt"]] <- cur.vers.object[["forstamt"]][vers.matches]
    edvid.archive.suppl.info[["abt"]] <- cur.vers.object[["abt"]][vers.matches]
    edvid.archive.suppl.info[["BESONDERHEITEN"]] <- cur.parz.object[["BESONDERHEITEN"]][parz.matches]
    ## Add "edvid.archive.suppl.info" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c("edvid.archive.suppl.info", kgmaxObjects)
    ## Assign "edvid.archive.suppl.info" to correctly named object.
    assign(x = paste0("edvid.archive.suppl.info.", cur.species.name),
           value = edvid.archive.suppl.info)
}
## Save results.
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_2.9.RData" ##
####################################
## Based on version 2.8.
## In this version, an additional data frame "bart.SPECIES.clean.1.1" is created which is a subset of "bart.SPECIES.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.0[["ghaa"]] > 0.20 * bart.SPECIES.clean.1.0[["gha"]]".
kBaseFileVersion <- "2.8"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "2.9"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create untampered source version of "bart.SPECIES.clean.1.1".
    cur.bart.clean.1.1 <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Exclude all consecutive measurements for a given "edvid" if "cur.bart.clean.1.1[["ghaa"]] > 0.20 * cur.bart.clean.1.1[["gha"]]".
    names.vec <- NULL
    for (parcel in levels(x = cur.bart.clean.1.1[["edvid"]])) {
        name.cur <- paste0("obj.", as.character(parcel))
        names.vec <- c(names.vec, name.cur)
        parcel.subset <- cur.bart.clean.1.1[cur.bart.clean.1.1[["edvid"]] == parcel, ]
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
    cur.bart.clean.1.1 <- data.frame(NULL)
    for (name.cur in names.vec) {
        cur.bart.clean.1.1 <- rbind(cur.bart.clean.1.1,
                                    eval(expr = as.name(x = name.cur)))
    }
    ## Drop unused levels.
    cur.bart.clean.1.1 <- droplevels(x = cur.bart.clean.1.1)
    ## Assign cur.bart.clean.1.1 to the respective object meant to be saved.
    assign(x = paste0("bart.", cur.species.name, ".clean.1.1"),
           value = cur.bart.clean.1.1)
    ## Add "bart.SPECIES.clean.1.1" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.1"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.0.RData" ##
####################################
## Based on version 2.9.
## In this version, an additional data frame "bart.SPECIES.clean.1.2" is created which is a subset of "bart.SPECIES.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.2[["ghaa"]] > 0.15 * bart.SPECIES.clean.1.2[["gha"]]".
kBaseFileVersion <- "2.9"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.0"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create untampered source version of "bart.SPECIES.clean.1.2".
    cur.bart.clean.1.2 <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Exclude all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.2[["ghaa"]] > 0.15 * bart.SPECIES.clean.1.2[["gha"]]".
    names.vec <- NULL
    for (parcel in levels(x = cur.bart.clean.1.2[["edvid"]])) {
        name.cur <- paste0("obj.", as.character(parcel))
        names.vec <- c(names.vec, name.cur)
        parcel.subset <- cur.bart.clean.1.2[cur.bart.clean.1.2[["edvid"]] == parcel, ]
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
    cur.bart.clean.1.2 <- data.frame(NULL)
    for (name.cur in names.vec) {
        cur.bart.clean.1.2 <- rbind(cur.bart.clean.1.2,
                                    eval(expr = as.name(x = name.cur)))
    }
    ## Drop unused levels.
    cur.bart.clean.1.2 <- droplevels(x = cur.bart.clean.1.2)
    ## Assign cur.bart.clean.1.2 to the respective object meant to be saved.
    assign(x = paste0("bart.", cur.species.name, ".clean.1.2"),
           value = cur.bart.clean.1.2)
    ## Add "bart.SPECIES.clean.1.2" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.2"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.1.RData" ##
####################################
## Based on version 3.0.
## In this version, an additional data frame "bart.SPECIES.clean.1.3" is created which is a subset of "bart.SPECIES.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.3[["ghaa"]] > 0.10 * bart.SPECIES.clean.1.3[["gha"]]".
kBaseFileVersion <- "3.0"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.1"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create untampered source version of "bart.SPECIES.clean.1.3".
    cur.bart.clean.1.3 <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Exclude all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.3[["ghaa"]] > 0.10 * bart.SPECIES.clean.1.3[["gha"]]".
    names.vec <- NULL
    for (parcel in levels(x = cur.bart.clean.1.3[["edvid"]])) {
        name.cur <- paste0("obj.", as.character(parcel))
        names.vec <- c(names.vec, name.cur)
        parcel.subset <- cur.bart.clean.1.3[cur.bart.clean.1.3[["edvid"]] == parcel, ]
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
    cur.bart.clean.1.3 <- data.frame(NULL)
    for (name.cur in names.vec) {
        cur.bart.clean.1.3 <- rbind(cur.bart.clean.1.3,
                                    eval(expr = as.name(x = name.cur)))
    }
    ## Drop unused levels.
    cur.bart.clean.1.3 <- droplevels(x = cur.bart.clean.1.3)
    ## Assign cur.bart.clean.1.3 to the respective object meant to be saved.
    assign(x = paste0("bart.", cur.species.name, ".clean.1.3"),
           value = cur.bart.clean.1.3)
    ## Add "bart.SPECIES.clean.1.3" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.3"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.2.RData" ##
####################################
## Based on version 3.1.
## In this version, an additional data frame "bart.SPECIES.clean.1.4" is created which is a subset of "bart.SPECIES.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.4[["ghaa"]] > 0.05 * bart.SPECIES.clean.1.4[["gha"]]".
kBaseFileVersion <- "3.1"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.2"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create untampered source version of "bart.SPECIES.clean.1.4".
    cur.bart.clean.1.4 <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Exclude all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.4[["ghaa"]] > 0.05 * bart.SPECIES.clean.1.4[["gha"]]".
    names.vec <- NULL
    for (parcel in levels(x = cur.bart.clean.1.4[["edvid"]])) {
        name.cur <- paste0("obj.", as.character(parcel))
        names.vec <- c(names.vec, name.cur)
        parcel.subset <- cur.bart.clean.1.4[cur.bart.clean.1.4[["edvid"]] == parcel, ]
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
    cur.bart.clean.1.4 <- data.frame(NULL)
    for (name.cur in names.vec) {
        cur.bart.clean.1.4 <- rbind(cur.bart.clean.1.4,
                                    eval(expr = as.name(x = name.cur)))
    }
    ## Drop unused levels.
    cur.bart.clean.1.4 <- droplevels(x = cur.bart.clean.1.4)
    ## Assign cur.bart.clean.1.4 to the respective object meant to be saved.
    assign(x = paste0("bart.", cur.species.name, ".clean.1.4"),
           value = cur.bart.clean.1.4)
    ## Add "bart.SPECIES.clean.1.4" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.4"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.3.RData" ##
####################################
## Based on version 3.2.
## In this version, an additional data frame "bart.SPECIES.clean.1.5" is created which is a subset of "bart.SPECIES.clean.1.0", excluding all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.5[["ghaa"]] > 0.00 * bart.SPECIES.clean.1.5[["gha"]]".
kBaseFileVersion <- "3.2"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.3"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create untampered source version of "bart.SPECIES.clean.1.5".
    cur.bart.clean.1.5 <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Exclude all consecutive measurements for a given "edvid" if "bart.SPECIES.clean.1.5[["ghaa"]] > 0.00 * bart.SPECIES.clean.1.5[["gha"]]".
    names.vec <- NULL
    for (parcel in levels(x = cur.bart.clean.1.5[["edvid"]])) {
        name.cur <- paste0("obj.", as.character(parcel))
        names.vec <- c(names.vec, name.cur)
        parcel.subset <- cur.bart.clean.1.5[cur.bart.clean.1.5[["edvid"]] == parcel, ]
        auf.vec <- parcel.subset[["auf"]][parcel.subset[["ghaa"]] > 0.00 * parcel.subset[["gha"]]]
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
    cur.bart.clean.1.5 <- data.frame(NULL)
    for (name.cur in names.vec) {
        cur.bart.clean.1.5 <- rbind(cur.bart.clean.1.5,
                                    eval(expr = as.name(x = name.cur)))
    }
    ## Drop unused levels.
    cur.bart.clean.1.5 <- droplevels(x = cur.bart.clean.1.5)
    ## Assign cur.bart.clean.1.5 to the respective object meant to be saved.
    assign(x = paste0("bart.", cur.species.name, ".clean.1.5"),
           value = cur.bart.clean.1.5)
    ## Add "bart.SPECIES.clean.1.5" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.5"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.4.RData" ##
####################################
## Based on version 3.3.
## In this version, an additional data frame "bart.SPECIES.clean.1.6" is created which is a subset of "bart.SPECIES.clean.1.0", excluding certain data (see below for details).
kBaseFileVersion <- "3.3"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.4"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create template for "bart.SPECIES.clean.1.6".
    assign(x = paste0("bart.", cur.species.name, ".clean.1.6"),
           value = data.frame(NULL))
    ## Define slope threshold values for the current species.
    if (cur.species.name == "beech") {
        cur.upper.threshold <- -1.00  ## arbitrary value
        cur.lower.threshold <- -2.00  ## arbitrary value
    }
    if (cur.species.name == "spruce") {
        cur.upper.threshold <- -1.00  ## arbitrary value
        cur.lower.threshold <- -2.00  ## arbitrary value
    }
    ## Create untampered source version of "bart.SPECIES.clean.1.6".
    cur.bart.clean.untampered <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Loop over all "edvid".
    for (cur.edvid in levels(x = cur.bart.clean.untampered[["edvid"]])) {
        ## Create subset of "cur.bart.clean.untampered" for the current "edvid".
        cur.subset <- subset(x = cur.bart.clean.untampered,
                             subset = cur.bart.clean.untampered[["edvid"]] == cur.edvid)
        ## Drop unused levels from "cur.subset".
        cur.subset <- droplevels(x = cur.subset)
        ## Create dummy version of "rows.kept.cur.subset" (the vector containing the row numbers to keep) to get the following "while" loop going.
        rows.kept.cur.subset <- rep(x = FALSE, times = nrow(x = cur.subset))
        ## Keep looping over the current subset, as long as not all rows are marked for keeping.
        while (!all(rows.kept.cur.subset)) {
            ## Create template for the vector containing the row numbers to keep.
            rows.kept.cur.subset <- rep(x = TRUE, times = nrow(x = cur.subset))
            ## Mark rows for keeping, depending on the log.nha-log.dg-slope between the current and adjacent rows.
            for (cur.row.number in seq_len(length.out = nrow(x = cur.subset))) {
                ## Check slope between current row and previous row and/or current row and next row (depending on which row we are at).
                if (cur.row.number != nrow(x = cur.subset) && cur.row.number != 1) {
                    ## Check current row only if previous row has been marked for keeping (otherwise we might delete more rows than necessary to obtain an acceptable slope).
                    if (rows.kept.cur.subset[cur.row.number - 1] == TRUE) {
                        ## Mark row for keeping if both the slope to the previous row is geq than the lower threshold AND the slope to the following row is leq than the upper threshold.
                        cur.slope.previous <- (cur.subset[["log.nha"]][cur.row.number] - cur.subset[["log.nha"]][cur.row.number - 1]) / (cur.subset[["log.dg"]][cur.row.number] - cur.subset[["log.dg"]][cur.row.number - 1])
                        cur.slope.following <- (cur.subset[["log.nha"]][cur.row.number + 1] - cur.subset[["log.nha"]][cur.row.number]) / (cur.subset[["log.dg"]][cur.row.number + 1] - cur.subset[["log.dg"]][cur.row.number])
                        rows.kept.cur.subset[cur.row.number] <- (cur.slope.previous >= cur.lower.threshold && cur.slope.following <= cur.upper.threshold)
                    }}
                if (cur.row.number == 1 && cur.row.number != nrow(x = cur.subset)) {
                    ## Mark row for keeping if the slope to the following row is leq than the upper threshold (the slope to the previous row cannot be calculated, since this is the first row).
                    cur.slope.following <- (cur.subset[["log.nha"]][cur.row.number + 1] - cur.subset[["log.nha"]][cur.row.number]) / (cur.subset[["log.dg"]][cur.row.number + 1] - cur.subset[["log.dg"]][cur.row.number])
                    rows.kept.cur.subset[cur.row.number] <- cur.slope.following <= cur.upper.threshold
                }
                if (cur.row.number != 1 && cur.row.number == nrow(x = cur.subset)) {
                    ## Mark row for keeping if the slope to the previous row is geq than the lower threshold (the slope to the following row cannot be calculated, since this is the last row).
                    cur.slope.previous <- (cur.subset[["log.nha"]][cur.row.number] - cur.subset[["log.nha"]][cur.row.number - 1]) / (cur.subset[["log.dg"]][cur.row.number] - cur.subset[["log.dg"]][cur.row.number - 1])
                    rows.kept.cur.subset[cur.row.number] <- cur.slope.previous >= cur.lower.threshold
                }
            }
            ## Create cleaned subset.
            cur.subset <- cur.subset[rows.kept.cur.subset, ]
        }
        ## If the cleaned subset contains more than 1 row, assign it to "bart.SPECIES.clean.1.6".
        if (nrow(x = cur.subset) > 1) {
            assign(x = paste0("bart.", cur.species.name, ".clean.1.6"),
                   value = rbind(get(x = paste0("bart.", cur.species.name, ".clean.1.6")),
                                 droplevels(x = cur.subset)))
        }}
    ## Recalculate columns "age.class", "h100.class", and "SI.h100.class".
    cur.species.bart.clean.1.6 <- get(x = paste0("bart.", cur.species.name, ".clean.1.6"))
    cur.species.bart.clean.1.6[["age.class"]] <- cut(x = cur.species.bart.clean.1.6[["alt"]],
                                                     breaks = 3,
                                                     ordered_result = TRUE)
    cur.species.bart.clean.1.6[["h100.class"]] <- cut(x = cur.species.bart.clean.1.6[["h100"]],
                                                      breaks = 3,
                                                      ordered_result = TRUE)
    cur.species.bart.clean.1.6[["SI.h100.class"]] <- cut(x = cur.species.bart.clean.1.6[["SI.h100"]],
                                                         breaks = 3,
                                                         ordered_result = TRUE)
    assign(x = paste0("bart.", cur.species.name, ".clean.1.6"),
           value = cur.species.bart.clean.1.6)
    ## Add "bart.SPECIES.clean.1.6" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.6"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.5.RData" ##
####################################
## Based on version 3.4.
## In this version, 2 columns are added to each "bart.SPECIES.clean.[0-9].[0-9]" data frame:
## - "h100.class", which contains the level of the respective measurement in terms of "h100" divided into 3 levels;
## - "SI.h100.class", which contains the level of the respective measurement in terms of "SI.h100" divided into 3 levels;
## - "trial", which contains the trial the respective "edvid" (defined by the first 3 digits of "edvid") belongs to as a factor level.
kBaseFileVersion <- "3.4"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.5"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Loop over all appropriate data source names.
    for (cur.data.source.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name, ".clean"), x = ls(), fixed = FALSE)]) {
        ## Assign current data source.
        cur.data.source <- get(x = cur.data.source.name)
        ## Calculate column "h100.class".
        cur.data.source[["h100.class"]] <- cut(x = cur.data.source[["h100"]], breaks = 3, ordered_result = TRUE)
        ## Calculate column "SI.h100.class".
        cur.data.source[["SI.h100.class"]] <- cut(x = cur.data.source[["SI.h100"]], breaks = 3, ordered_result = TRUE)
        ## Calculate column "trial".
        cur.data.source[["trial"]] <- as.factor(x = substr(x = cur.data.source[["edvid"]], start = 1, stop = 3))
        ## Assign new version of "bart.SPECIES.clean.[0-9].[0-9]".
        assign(x = cur.data.source.name,
               value = cur.data.source)
    }}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.6.RData" ##
####################################
## Based on version 3.5.
## In this version, the contents of "gmax_koords.RData" and "gmax_koords_bu.RData" are added to the resulting data set "gmax_merged_3.6.RData".
kBaseFileVersion <- "3.5"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.6"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Load "gmax_koords.RData".
kSpruceNewObjects <- load(file = paste0(kDataDir, "Spruce/", "gmax_koords.RData"), verbose = TRUE)
## Assign object "parz2" to "parz2.spruce".
parz2.spruce <- parz2
## Load "gmax_koords_bu.RData".
kSpruceNewObjects <- load(file = paste0(kDataDir, "Beech/", "gmax_koords_bu.RData"), verbose = TRUE)
## Assign object "parz2" to "parz2.beech".
parz2.beech <- parz2
## Add "parz2.beech" and "parz2.spruce" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("parz2.beech", "parz2.spruce", kgmaxObjects)
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.7.RData" ##
####################################
## Based on version 3.6.
## In this version, column "hnn_neu" from "parz2.SPECIES" is added to "bart.SPECIES", and "bart.SPECIES.clean.[0-9].[0-9]".
kBaseFileVersion <- "3.6"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.7"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over species.
for (cur.species.name in c("beech", "spruce")) {
    ## Assign "parz2.SPECIES2" to "cur.parz2".
    cur.parz2 <- get(x = paste0("parz2.", cur.species.name))
    ## Loop over all appropriate data frames
    for (cur.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name), x = ls(), fixed = FALSE)]) {
        ## Assign "bart.SPECIES..." to "cur.data.frame".
        cur.data.frame <- get(x = cur.data.frame.name)
        ## Merge "cur.data.frame" and "cur.parz2[, c("edvid", "hnn_neu")]" into "cur.data.frame.merged".
        cur.data.frame.merged <- merge(x = cur.data.frame,
                                       y = cur.parz2[, c("edvid", "hnn_neu")],
                                       by = "edvid",
                                       sort = FALSE)
        ## Change column name "hnn_neu" to "hnn.neu".
        names(x = cur.data.frame.merged)[names(x = cur.data.frame.merged) == "hnn_neu"] <- "hnn.neu"
        ## Assing value of "cur.data.frame.merged" to "cur.data.frame.name".
        assign(x = cur.data.frame.name,
               value = cur.data.frame.merged)
    }}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.8.RData" ##
####################################
## Based on version 3.7.
## In this version, an additional data frame "bart.SPECIES.clean.1.7" is created which is a subset of "bart.SPECIES.clean.1.0", excluding certain data (see below for details).
kBaseFileVersion <- "3.7"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.8"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create template for "bart.SPECIES.clean.1.7".
    assign(x = paste0("bart.", cur.species.name, ".clean.1.7"),
           value = data.frame(NULL))
    ## Define slope threshold values for the current species.
    if (cur.species.name == "beech") {
        cur.upper.threshold <- -1.70  ## based on literature
        cur.lower.threshold <- -1.94  ## based on literature
    }
    if (cur.species.name == "spruce") {
        cur.upper.threshold <- -1.30  ## based on literature
        cur.lower.threshold <- -1.88  ## based on literature
    }
    ## Create untampered source version of "bart.SPECIES.clean.1.7".
    cur.bart.clean.untampered <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Loop over all "edvid".
    for (cur.edvid in levels(x = cur.bart.clean.untampered[["edvid"]])) {
        ## Create subset of "cur.bart.clean.untampered" for the current "edvid".
        cur.subset <- subset(x = cur.bart.clean.untampered,
                             subset = cur.bart.clean.untampered[["edvid"]] == cur.edvid)
        ## Drop unused levels from "cur.subset".
        cur.subset <- droplevels(x = cur.subset)
        ## Create dummy version of "rows.kept.cur.subset" (the vector containing the row numbers to keep) to get the following "while" loop going.
        rows.kept.cur.subset <- rep(x = FALSE, times = nrow(x = cur.subset))
        ## Keep looping over the current subset, as long as not all rows are marked for keeping.
        while (!all(rows.kept.cur.subset)) {
            ## Create template for the vector containing the row numbers to keep.
            rows.kept.cur.subset <- rep(x = TRUE, times = nrow(x = cur.subset))
            ## Mark rows for keeping, depending on the log.nha-log.dg-slope between the current and adjacent rows.
            for (cur.row.number in seq_len(length.out = nrow(x = cur.subset))) {
                ## Check slope between current row and previous row and/or current row and next row (depending on which row we are at).
                if (cur.row.number != nrow(x = cur.subset) && cur.row.number != 1) {
                    ## Check current row only if previous row has been marked for keeping (otherwise we might delete more rows than necessary to obtain an acceptable slope).
                    if (rows.kept.cur.subset[cur.row.number - 1] == TRUE) {
                        ## Mark row for keeping if both the slope to the previous row is geq than the lower threshold AND the slope to the following row is leq than the upper threshold.
                        cur.slope.previous <- (cur.subset[["log.nha"]][cur.row.number] - cur.subset[["log.nha"]][cur.row.number - 1]) / (cur.subset[["log.dg"]][cur.row.number] - cur.subset[["log.dg"]][cur.row.number - 1])
                        cur.slope.following <- (cur.subset[["log.nha"]][cur.row.number + 1] - cur.subset[["log.nha"]][cur.row.number]) / (cur.subset[["log.dg"]][cur.row.number + 1] - cur.subset[["log.dg"]][cur.row.number])
                        rows.kept.cur.subset[cur.row.number] <- (cur.slope.previous >= cur.lower.threshold && cur.slope.following <= cur.upper.threshold)
                    }}
                if (cur.row.number == 1 && cur.row.number != nrow(x = cur.subset)) {
                    ## Mark row for keeping if the slope to the following row is leq than the upper threshold (the slope to the previous row cannot be calculated, since this is the first row).
                    cur.slope.following <- (cur.subset[["log.nha"]][cur.row.number + 1] - cur.subset[["log.nha"]][cur.row.number]) / (cur.subset[["log.dg"]][cur.row.number + 1] - cur.subset[["log.dg"]][cur.row.number])
                    rows.kept.cur.subset[cur.row.number] <- cur.slope.following <= cur.upper.threshold
                }
                if (cur.row.number != 1 && cur.row.number == nrow(x = cur.subset)) {
                    ## Mark row for keeping if the slope to the previous row is geq than the lower threshold (the slope to the following row cannot be calculated, since this is the last row).
                    cur.slope.previous <- (cur.subset[["log.nha"]][cur.row.number] - cur.subset[["log.nha"]][cur.row.number - 1]) / (cur.subset[["log.dg"]][cur.row.number] - cur.subset[["log.dg"]][cur.row.number - 1])
                    rows.kept.cur.subset[cur.row.number] <- cur.slope.previous >= cur.lower.threshold
                }
            }
            ## Create cleaned subset.
            cur.subset <- cur.subset[rows.kept.cur.subset, ]
        }
        ## If the cleaned subset contains more than 1 row, assign it to "bart.SPECIES.clean.1.7".
        if (nrow(x = cur.subset) > 1) {
            assign(x = paste0("bart.", cur.species.name, ".clean.1.7"),
                   value = rbind(get(x = paste0("bart.", cur.species.name, ".clean.1.7")),
                                 droplevels(x = cur.subset)))
        }}
    ## Recalculate columns "age.class", "h100.class", and "SI.h100.class".
    cur.species.bart.clean.1.7 <- get(x = paste0("bart.", cur.species.name, ".clean.1.7"))
    cur.species.bart.clean.1.7[["age.class"]] <- cut(x = cur.species.bart.clean.1.7[["alt"]],
                                                     breaks = 3,
                                                     ordered_result = TRUE)
    cur.species.bart.clean.1.7[["h100.class"]] <- cut(x = cur.species.bart.clean.1.7[["h100"]],
                                                      breaks = 3,
                                                      ordered_result = TRUE)
    cur.species.bart.clean.1.7[["SI.h100.class"]] <- cut(x = cur.species.bart.clean.1.7[["SI.h100"]],
                                                         breaks = 3,
                                                         ordered_result = TRUE)
    assign(x = paste0("bart.", cur.species.name, ".clean.1.7"),
           value = cur.species.bart.clean.1.7)
    ## Add "bart.SPECIES.clean.1.7" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.7"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_3.9.RData" ##
####################################
## Based on version 3.8.
## In this version, an additional data frame "bart.SPECIES.clean.1.8" is created which is a subset of "bart.SPECIES.clean.1.0", excluding certain data (see below for details).
kBaseFileVersion <- "3.8"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "3.9"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create template for "bart.SPECIES.clean.1.8".
    assign(x = paste0("bart.", cur.species.name, ".clean.1.8"),
           value = data.frame(NULL))
    ## Define slope threshold values for the current species.
    if (cur.species.name == "beech") {
        cur.upper.threshold <- -1.80 * 0.5  ## based on literature
        cur.lower.threshold <- -1.94 * 1.5  ## based on literature
    }
    if (cur.species.name == "spruce") {
        cur.upper.threshold <- -1.30 * 0.5  ## based on literature
        cur.lower.threshold <- -1.88 * 1.5  ## based on literature
    }
    ## Create untampered source version of "bart.SPECIES.clean.1.8".
    cur.bart.clean.untampered <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
    ## Loop over all "edvid".
    for (cur.edvid in levels(x = cur.bart.clean.untampered[["edvid"]])) {
        ## Create subset of "cur.bart.clean.untampered" for the current "edvid".
        cur.subset <- subset(x = cur.bart.clean.untampered,
                             subset = cur.bart.clean.untampered[["edvid"]] == cur.edvid)
        ## Drop unused levels from "cur.subset".
        cur.subset <- droplevels(x = cur.subset)
        ## Create dummy version of "rows.kept.cur.subset" (the vector containing the row numbers to keep) to get the following "while" loop going.
        rows.kept.cur.subset <- rep(x = FALSE, times = nrow(x = cur.subset))
        ## Keep looping over the current subset, as long as not all rows are marked for keeping.
        while (!all(rows.kept.cur.subset)) {
            ## Create template for the vector containing the row numbers to keep.
            rows.kept.cur.subset <- rep(x = TRUE, times = nrow(x = cur.subset))
            ## Mark rows for keeping, depending on the log.nha-log.dg-slope between the current and adjacent rows.
            for (cur.row.number in seq_len(length.out = nrow(x = cur.subset))) {
                ## Check slope between current row and previous row and/or current row and next row (depending on which row we are at).
                if (cur.row.number != nrow(x = cur.subset) && cur.row.number != 1) {
                    ## Check current row only if previous row has been marked for keeping (otherwise we might delete more rows than necessary to obtain an acceptable slope).
                    if (rows.kept.cur.subset[cur.row.number - 1] == TRUE) {
                        ## Mark row for keeping if both the slope to the previous row is geq than the lower threshold AND the slope to the following row is leq than the upper threshold.
                        cur.slope.previous <- (cur.subset[["log.nha"]][cur.row.number] - cur.subset[["log.nha"]][cur.row.number - 1]) / (cur.subset[["log.dg"]][cur.row.number] - cur.subset[["log.dg"]][cur.row.number - 1])
                        cur.slope.following <- (cur.subset[["log.nha"]][cur.row.number + 1] - cur.subset[["log.nha"]][cur.row.number]) / (cur.subset[["log.dg"]][cur.row.number + 1] - cur.subset[["log.dg"]][cur.row.number])
                        rows.kept.cur.subset[cur.row.number] <- (cur.slope.previous >= cur.lower.threshold && cur.slope.following <= cur.upper.threshold)
                    }}
                if (cur.row.number == 1 && cur.row.number != nrow(x = cur.subset)) {
                    ## Mark row for keeping if the slope to the following row is leq than the upper threshold (the slope to the previous row cannot be calculated, since this is the first row).
                    cur.slope.following <- (cur.subset[["log.nha"]][cur.row.number + 1] - cur.subset[["log.nha"]][cur.row.number]) / (cur.subset[["log.dg"]][cur.row.number + 1] - cur.subset[["log.dg"]][cur.row.number])
                    rows.kept.cur.subset[cur.row.number] <- cur.slope.following <= cur.upper.threshold
                }
                if (cur.row.number != 1 && cur.row.number == nrow(x = cur.subset)) {
                    ## Mark row for keeping if the slope to the previous row is geq than the lower threshold (the slope to the following row cannot be calculated, since this is the last row).
                    cur.slope.previous <- (cur.subset[["log.nha"]][cur.row.number] - cur.subset[["log.nha"]][cur.row.number - 1]) / (cur.subset[["log.dg"]][cur.row.number] - cur.subset[["log.dg"]][cur.row.number - 1])
                    rows.kept.cur.subset[cur.row.number] <- cur.slope.previous >= cur.lower.threshold
                }
            }
            ## Create cleaned subset.
            cur.subset <- cur.subset[rows.kept.cur.subset, ]
        }
        ## If the cleaned subset contains more than 1 row, assign it to "bart.SPECIES.clean.1.8".
        if (nrow(x = cur.subset) > 1) {
            assign(x = paste0("bart.", cur.species.name, ".clean.1.8"),
                   value = rbind(get(x = paste0("bart.", cur.species.name, ".clean.1.8")),
                                 droplevels(x = cur.subset)))
        }}
    ## Recalculate columns "age.class", "h100.class", and "SI.h100.class".
    cur.species.bart.clean.1.8 <- get(x = paste0("bart.", cur.species.name, ".clean.1.8"))
    cur.species.bart.clean.1.8[["age.class"]] <- cut(x = cur.species.bart.clean.1.8[["alt"]],
                                                     breaks = 3,
                                                     ordered_result = TRUE)
    cur.species.bart.clean.1.8[["h100.class"]] <- cut(x = cur.species.bart.clean.1.8[["h100"]],
                                                      breaks = 3,
                                                      ordered_result = TRUE)
    cur.species.bart.clean.1.8[["SI.h100.class"]] <- cut(x = cur.species.bart.clean.1.8[["SI.h100"]],
                                                         breaks = 3,
                                                         ordered_result = TRUE)
    assign(x = paste0("bart.", cur.species.name, ".clean.1.8"),
           value = cur.species.bart.clean.1.8)
    ## Add "bart.SPECIES.clean.1.8" to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(paste0("bart.", cur.species.name, ".clean.1.8"), kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.0.RData" ##
####################################
## Based on version 3.9.
## In this version, columns "WGS_EAST", and "WGS_NORTH" from "parz2.SPECIES" is added to "bart.SPECIES", and "bart.SPECIES.clean.[0-9].[0-9]".
kBaseFileVersion <- "3.9"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.0"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over species.
for (cur.species.name in c("beech", "spruce")) {
    ## Assign "parz2.SPECIES2" to "cur.parz2".
    cur.parz2 <- get(x = paste0("parz2.", cur.species.name))
    ## Loop over all appropriate data frames
    for (cur.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name), x = ls(), fixed = FALSE)]) {
        ## Assign "bart.SPECIES..." to "cur.data.frame".
        cur.data.frame <- get(x = cur.data.frame.name)
        ## Merge "cur.data.frame" and "cur.parz2[, c("edvid", "WGS_EAST", "WGS_NORTH")]" into "cur.data.frame.merged".
        cur.data.frame.merged <- merge(x = cur.data.frame,
                                       y = cur.parz2[, c("edvid", "WGS_EAST", "WGS_NORTH")],
                                       by = "edvid",
                                       sort = FALSE)
        ## Assing value of "cur.data.frame.merged" to "cur.data.frame.name".
        assign(x = cur.data.frame.name,
               value = cur.data.frame.merged)
    }}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.1.RData" ##
####################################
## Based on version 4.0.
## In this version, columns "WGS_EAST", and "WGS_NORTH" are renamed to "EAST.WGS" and "NORTH.WGS", respectively in all "bart.SPECIES" and "bart.SPECIES.clean.[0-9].[0-9]" data frames.
kBaseFileVersion <- "4.0"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.1"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over species.
for (cur.species.name in c("beech", "spruce")) {
    ## Loop over all appropriate data frames
    for (cur.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name), x = ls(), fixed = FALSE)]) {
        ## Assign "bart.SPECIES..." to "cur.data.frame".
        cur.data.frame <- get(x = cur.data.frame.name)
        ## Extract column names of "cur.data.frame".
        cur.col.names <- colnames(x = cur.data.frame)
        ## Replace "_" with "." in "WGS_EAST" and "WGS_NORTH".
        cur.col.names[cur.col.names == "WGS_EAST"] <- "EAST.WGS"
        cur.col.names[cur.col.names == "WGS_NORTH"] <- "NORTH.WGS"
        ## Assign corrected column names.
        colnames(x = cur.data.frame) <- cur.col.names
        ## Assign corrected version of "cur.data.frame" to the original data frame.
        assign(x = cur.data.frame.name,
               value = cur.data.frame)
    }}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.2.RData" ##
####################################
## Based on version 4.1.
## In this version, the contents of "gmax_koords_UTM.RData" and "gmax_koords_bu_UTM.RData" are added to the resulting data set "gmax_merged_4.2.RData".
kBaseFileVersion <- "4.1"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.2"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Load "gmax_koords_UTM.RData".
kSpruceNewObjects <- load(file = paste0(kDataDir, "Spruce/", "gmax_koords_UTM.RData"), verbose = TRUE)
## Assign object "parz2" to "parz2.UTM.spruce".
parz2.UTM.spruce <- parz2
## Load "gmax_koords_bu_UTM.RData".
kSpruceNewObjects <- load(file = paste0(kDataDir, "Beech/", "gmax_koords_bu_UTM.RData"), verbose = TRUE)
## Assign object "parz2" to "parz2.UTM.beech".
parz2.UTM.beech <- parz2
## Add "parz2.UTM.beech" and "parz2.UTM.spruce" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("parz2.UTM.beech", "parz2.UTM.spruce", kgmaxObjects)
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.3.RData" ##
####################################
## Based on version 4.2.
## In this version, columns "UTM_lon", and "UTM_lat" from "parz2.UTM.SPECIES" are added (as "EAST.UTM" and "NORTH.UTM", respectively) to "bart.SPECIES", and "bart.SPECIES.clean.[0-9].[0-9]".
kBaseFileVersion <- "4.2"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.3"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over species.
for (cur.species.name in c("beech", "spruce")) {
    ## Assign "parz2.UTM.SPECIES2" to "cur.parz2.UTM".
    cur.parz2.UTM <- get(x = paste0("parz2.UTM.", cur.species.name))
    ## Loop over all appropriate data frames
    for (cur.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name), x = ls(), fixed = FALSE)]) {
        ## Assign "bart.SPECIES..." to "cur.data.frame".
        cur.data.frame <- get(x = cur.data.frame.name)
        ## Merge "cur.data.frame" and "cur.parz2.UTM[, c("edvid", "hnn_neu")]" into "cur.data.frame.merged".
        cur.data.frame.merged <- merge(x = cur.data.frame,
                                       y = cur.parz2.UTM[, c("edvid", "UTM_lon", "UTM_lat")],
                                       by = "edvid",
                                       sort = FALSE)
        ## Change column name "UTM_lon" to "EAST.UTM".
        names(x = cur.data.frame.merged)[names(x = cur.data.frame.merged) == "UTM_lon"] <- "EAST.UTM"
        ## Change column name "UTM_lat" to "NORTH.UTM".
        names(x = cur.data.frame.merged)[names(x = cur.data.frame.merged) == "UTM_lat"] <- "NORTH.UTM"
        ## Assing value of "cur.data.frame.merged" to "cur.data.frame.name".
        assign(x = cur.data.frame.name,
               value = cur.data.frame.merged)
    }}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.4.RData" ##
####################################
## Based on version 4.3.
## In this version, "bart.SPECIES" and "bart.SPECIES.clean.[0-9].[0-9]" contain an additional column "SI.h100.diff.EKL.I = SI.h100 - SI.h100.EKL.I".
kBaseFileVersion <- "4.3"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.4"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over species.
for (cur.species.name in c("beech", "spruce")) {
    ## Assign species-specific value for "SI.h100.EKL.I".
    if (cur.species.name == "beech") {
        SI.h100.EKL.I <- 32.4  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of beech, moderate thinning (source: Schober (1995)).
    }
    if (cur.species.name == "spruce") {
        SI.h100.EKL.I <- 35.1  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of spruce, moderate thinning (source: Schober (1995)).
    }
    ## Loop over all appropriate data frames
    for (cur.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name), x = ls(), fixed = FALSE)]) {
        ## Assign current data frame.
        cur.data.frame <- get(x = cur.data.frame.name)
        ## Calculate "h100.diff.EKL.I".
        cur.data.frame[["SI.h100.diff.EKL.I"]] <- cur.data.frame[["SI.h100"]] - SI.h100.EKL.I
        ## Assign new version of current object.
        assign(x = cur.data.frame.name,
               value = cur.data.frame)
    }}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.5.RData" ##
####################################
## Based on version 4.4.
## In this version, objects "bld_utm", "brd_utm", "krs_utm", "nds_utm", and "rbz_utm" from file "vg250_ebenen_shp_UTM.RData" are added to the data set.
kBaseFileVersion <- "4.4"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.5"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Load file "vg250_ebenen_shp_UTM.RData".
load(file = paste0(kDataDir, "Supplementary_Information/vg250_ebenen_shp_UTM.RData"),
     verbose = TRUE)
## Add "bld_utm", "brd_utm", "krs_utm", "nds_utm", and "rbz_utm" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("bld_utm", "brd_utm", "krs_utm", "nds_utm", "rbz_utm", kgmaxObjects)
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.6.RData" ##
####################################
## Based on version 4.5.
## In this version, a new data frame "schober.beech" is added which contains:
## - a column "age",
## - a column "h100", and
## - a column "yield.class".
## The contained values are taken from Schober (1995) for beech at moderate thinning.
kBaseFileVersion <- "4.5"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.6"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create vector for age of I. yield class.
age.yield.class.I <- seq(from = 30,
                         to = 150,
                         by = 5)
## Create vector for h100 of I. yield class.
h100.yield.class.I <- c(10.5, 12.7, 15.0, 17.4, 19.5, 21.5, 23.1, 24.5, 25.8, 27.1, 28.3, 29.4, 30.5, 31.5, 32.4, 33.2, 34.0, 34.7, 35.4, 36.0, 36.5, 37.0, 37.6, 38.0, 38.4)
## Create vector for yield class of I. yield class.
yield.class.yield.class.I <- as.factor(x = rep(x = 1, times = length(x = h100.yield.class.I)))
## Create data frame for I. yield class.
df.yield.class.I <- data.frame("age" = age.yield.class.I,
                               "h100" = h100.yield.class.I,
                               "yield.class" = yield.class.yield.class.I)
## Create vector for age of II. yield class.
age.yield.class.II <- seq(from = 30,
                          to = 150,
                          by = 5)
## Create vector for h100 of II. yield class.
h100.yield.class.II <- c(8.1, 10.0, 12.1, 14.3, 16.3, 18.1, 19.7, 21.1, 22.4, 23.6, 24.7, 25.8, 26.8, 27.7, 28.6, 29.3, 30.1, 30.8, 31.4, 31.9, 32.5, 32.9, 33.3, 33.8, 34.2)
## Create vector for yield class of II. yield class.
yield.class.yield.class.II <- as.factor(x = rep(x = 2, times = length(x = h100.yield.class.II)))
## Create data frame for II. yield class.
df.yield.class.II <- data.frame("age" = age.yield.class.II,
                               "h100" = h100.yield.class.II,
                               "yield.class" = yield.class.yield.class.II)
## Create vector for age of III. yield class.
age.yield.class.III <- seq(from = 30,
                           to = 150,
                           by = 5)
## Create vector for h100 of III. yield class.
h100.yield.class.III <- c(5.8, 7.3, 9.3, 11.4, 13.2, 14.8, 16.3, 17.7, 19.0, 20.1, 21.2, 22.1, 23.0, 23.9, 24.7, 25.4, 26.2, 26.8, 27.3, 27.9, 28.3, 28.7, 29.1, 29.4, 29.8)
## Create vector for yield class of III. yield class.
yield.class.yield.class.III <- as.factor(x = rep(x = 3, times = length(x = h100.yield.class.III)))
## Create data frame for III. yield class.
df.yield.class.III <- data.frame("age" = age.yield.class.III,
                               "h100" = h100.yield.class.III,
                               "yield.class" = yield.class.yield.class.III)
## Create vector for age of IV. yield class.
age.yield.class.IV <- seq(from = 30,
                          to = 150,
                          by = 5)
## Create vector for h100 of IV. yield class.
h100.yield.class.IV <- c(NA, 3.9, 5.9, 8.2, 9.9, 11.4, 12.9, 14.3, 15.5, 16.6, 17.6, 18.4, 19.2, 20.0, 20.7, 21.4, 22.1, 22.7, 23.2, 23.6, 24.1, 24.4, 24.8, 25.1, 25.4)
## Create vector for yield class of IV. yield class.
yield.class.yield.class.IV <- as.factor(x = rep(x = 4, times = length(x = h100.yield.class.IV)))
## Create data frame for IV. yield class.
df.yield.class.IV <- data.frame("age" = age.yield.class.IV,
                               "h100" = h100.yield.class.IV,
                               "yield.class" = yield.class.yield.class.IV)
## Create final data frame containing all yield classes.
schober.beech <- rbind(df.yield.class.I,
                       df.yield.class.II,
                       df.yield.class.III,
                       df.yield.class.IV)
## Add "schober.beech" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("schober.beech", kgmaxObjects)
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.7.RData" ##
####################################
## Based on version 4.6.
## In this version, a new data frame "schober.spruce" is added which contains:
## - a column "age",
## - a column "h100", and
## - a column "yield.class".
## The contained values are taken from Schober (1995) for spruce at moderate thinning.
kBaseFileVersion <- "4.6"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.7"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Create vector for age of I. yield class.
age.yield.class.I <- seq(from = 20,
                         to = 120,
                         by = 5)
## Create vector for h100 of I. yield class.
h100.yield.class.I <- c(8.5, 11.1, 13.7, 16.4, 19.0, 21.4, 23.7, 25.5, 27.1, 28.4, 29.7, 30.8, 31.8, 32.7, 33.5, 34.4, 35.1, 35.8, 36.4, 36.9, 37.3)
## Create vector for yield class of I. yield class.
yield.class.yield.class.I <- as.factor(x = rep(x = 1, times = length(x = h100.yield.class.I)))
## Create data frame for I. yield class.
df.yield.class.I <- data.frame("age" = age.yield.class.I,
                               "h100" = h100.yield.class.I,
                               "yield.class" = yield.class.yield.class.I)
## Create vector for age of II. yield class.
age.yield.class.II <- seq(from = 20,
                         to = 120,
                         by = 5)
## Create vector for h100 of II. yield class.
h100.yield.class.II <- c(5.9, 8.0, 10.4, 12.8, 15.0, 17.3, 19.3, 21.3, 23.0, 24.5, 25.7, 26.9, 27.9, 28.8, 29.7, 30.5, 31.2, 31.9, 32.5, 33.1, 33.6)
## Create vector for yield class of II. yield class.
yield.class.yield.class.II <- as.factor(x = rep(x = 2, times = length(x = h100.yield.class.II)))
## Create data frame for II. yield class.
df.yield.class.II <- data.frame("age" = age.yield.class.II,
                               "h100" = h100.yield.class.II,
                               "yield.class" = yield.class.yield.class.II)
## Create vector for age of III. yield class.
age.yield.class.III <- seq(from = 20,
                         to = 120,
                         by = 5)
## Create vector for h100 of III. yield class.
h100.yield.class.III <- c(4.1, 5.9, 7.5, 9.3, 11.3, 13.6, 15.5, 17.2, 18.8, 20.2, 21.5, 22.7, 23.7, 24.7, 25.6, 26.4, 27.2, 28.1, 28.8, 29.5, 30.1)
## Create vector for yield class of III. yield class.
yield.class.yield.class.III <- as.factor(x = rep(x = 3, times = length(x = h100.yield.class.III)))
## Create data frame for III. yield class.
df.yield.class.III <- data.frame("age" = age.yield.class.III,
                               "h100" = h100.yield.class.III,
                               "yield.class" = yield.class.yield.class.III)
## Create vector for age of IV. yield class.
age.yield.class.IV <- seq(from = 30,
                         to = 120,
                         by = 5)
## Create vector for h100 of IV. yield class.
h100.yield.class.IV <- c(4.7, 6.6, 8.4, 10.1, 11.9, 13.6, 15.1, 16.5, 17.7, 18.9, 19.9, 20.9, 21.7, 22.6, 23.5, 24.2, 25.0, 25.6, 26.3)
## Create vector for yield class of IV. yield class.
yield.class.yield.class.IV <- as.factor(x = rep(x = 4, times = length(x = h100.yield.class.IV)))
## Create data frame for IV. yield class.
df.yield.class.IV <- data.frame("age" = age.yield.class.IV,
                               "h100" = h100.yield.class.IV,
                               "yield.class" = yield.class.yield.class.IV)
## Create vector for age of V. yield class.
age.yield.class.V <- seq(from = 40,
                         to = 100,
                         by = 5)
## Create vector for h100 of V. yield class.
h100.yield.class.V <- c(5.2, 6.7, 8.3, 9.8, 11.3, 12.7, 14.0, 15.2, 16.2, 17.3, 18.2, 19.0, 19.7)
## Create vector for yield class of V. yield class.
yield.class.yield.class.V <- as.factor(x = rep(x = 5, times = length(x = h100.yield.class.V)))
## Create data frame for V. yield class.
df.yield.class.V <- data.frame("age" = age.yield.class.V,
                               "h100" = h100.yield.class.V,
                               "yield.class" = yield.class.yield.class.V)
## Create final data frame containing all yield classes.
schober.spruce <- rbind(df.yield.class.I,
                        df.yield.class.II,
                        df.yield.class.III,
                        df.yield.class.IV,
                        df.yield.class.V)
## Add "schober.spruce" to the vector of names of objects meant to be saved.
kgmaxObjects <- c("schober.spruce", kgmaxObjects)
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.8.RData" ##
####################################
## Based on version 4.7.
## In this version, "schober.SPECIES" contains an additional column "h100.EKL.I" which holds h100 for a given age if the stand were EKL I.
kBaseFileVersion <- "4.7"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.8"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("schober.beech", "schober.spruce")) {
    ## Assign species-specific value for "SI.h100.EKL.I".
    if (grepl(pattern = "beech", x = cur.object.name)) {
        SI.h100.EKL.I <- 32.4  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of beech, moderate thinning (source: Schober (1995)).
    }
    if (grepl(pattern = "spruce", x = cur.object.name)) {
        SI.h100.EKL.I <- 35.1  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of spruce, moderate thinning (source: Schober (1995)).
    }
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "h100.EKL.I" based on the function by Nagel 1999 solved for "h100".
    ## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))  ## Original function (see email by Matthias Schmidt from 2017-04-27 12:06).
    cur.object[["h100.EKL.I"]] <- SI.h100.EKL.I * (0.52684 + 0.10542 * log(x = cur.object[["age"]])) - 49.872 + 7.3309 * log(x = cur.object[["age"]]) + 0.77338 * (log(x = cur.object[["age"]]))^2
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_4.9.RData" ##
####################################
## Based on version 4.8.
## In this version, "schober.SPECIES" contains an additional column "h100.diff.EKL.I = h100.EKL.I - h100".
kBaseFileVersion <- "4.8"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "4.9"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("schober.beech", "schober.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "h100.diff.EKL.I".
    cur.object[["h100.diff.EKL.I"]] <- cur.object[["h100"]] - cur.object[["h100.EKL.I"]]
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_5.0.RData" ##
####################################
## Based on version 4.9.
## In this version, "schober.SPECIES" contains an additional column "SI.h100" which holds the stand index calculated with the function by Nagel (see email by Matthias Schmidt from 2017-04-27 12:06).
kBaseFileVersion <- "4.9"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "5.0"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("schober.beech", "schober.spruce")) {
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "SI_h100".
    ## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))  ## Original function (see email by Matthias Schmidt from 2017-04-27 12:06).
    cur.object[["SI.h100"]] <- (cur.object[["h100"]] + 49.87200 - 7.33090 * log(x = cur.object[["age"]]) - 0.77338 * ((log(x = cur.object[["age"]])) ^ 2.0)) / (0.52684 + 0.10542 * log(x = cur.object[["age"]]))
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_5.1.RData" ##
####################################
## Based on version 5.0.
## In this version, "schober.SPECIES" contain an additional column "SI.h100.diff.EKL.I = SI.h100 - SI.h100.EKL.I".
kBaseFileVersion <- "5.0"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "5.1"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all relevant objects.
for (cur.object.name in c("schober.beech", "schober.spruce")) {
    ## Assign species-specific value for "SI.h100.EKL.I".
    if (grepl(pattern = "beech", x = cur.object.name)) {
        SI.h100.EKL.I <- 32.4  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of beech, moderate thinning (source: Schober (1995)).
    }
    if (grepl(pattern = "spruce", x = cur.object.name)) {
        SI.h100.EKL.I <- 35.1  ## This value is h_100 at age 100 (i.e., SI.h100) for EKL I of spruce, moderate thinning (source: Schober (1995)).
    }
    ## Assign current object.
    cur.object <- get(x = cur.object.name)
    ## Calculate "SI.h100.diff.EKL.I".
    cur.object[["SI.h100.diff.EKL.I"]] <- cur.object[["SI.h100"]] - SI.h100.EKL.I
    ## Assign new version of current object.
    assign(x = cur.object.name,
           value = cur.object)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_5.2.RData" ##
####################################
## Based on version 5.1.
## In this version, new data frames "nagel.SPECIES" are added which contain the following columns:
## - age
## - yield.class
## - h100
## - h100.EKL.I
## - SI.h100
## - SI.h100.diff.EKL.I
## Function by Nagel (1999) (see email by Matthias Schmidt from 2017-04-27 12:06):
## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))
kBaseFileVersion <- "5.1"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "5.2"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Create "SI.h100" values for yield classes -1 to 3. Values for yield classes 3, 2, and 1 are directly taken from Schober (1995) (moderate thinning). Values for yield classes 0 and -1 (name suffix ".1") are linearly interpolated from the values for classes 1 and 2.
    if (cur.species.name == "beech") {
        SI.h100.yield.class.3 <- 24.7
        SI.h100.yield.class.2 <- 28.6
        SI.h100.yield.class.1 <- 32.4
    }
    if (cur.species.name == "spruce") {
        SI.h100.yield.class.3 <- 27.2
        SI.h100.yield.class.2 <- 31.2
        SI.h100.yield.class.1 <- 35.1
    }
    SI.h100.yield.class.0 <- SI.h100.yield.class.1 + SI.h100.yield.class.1 - SI.h100.yield.class.2
    SI.h100.yield.class..1 <- SI.h100.yield.class.0 + SI.h100.yield.class.1 - SI.h100.yield.class.2
    ## Create a vector containing the suffixes to use for naming objects individually per yield class.
    name.suffixes <- c(".1", "0", "1", "2", "3")
    ## Create an "age" vector which will serve as the basis for various calculations for all yield classes.
    age <- seq(from = 10,
               to = 160,
               by = 1)
    ## Calculate "h100.yield.class.YC" for yield class "YC", based on "age" and "SI.h100.yield.class.YC", using the function by Nagel (1999).
    for (cur.name.suffix in name.suffixes) {
        SI.h100 <- get(x = paste0("SI.h100.yield.class.", cur.name.suffix))
        assign(x = paste0("h100.yield.class.", cur.name.suffix),
               value = SI.h100 * (0.52684 + 0.10542 * log(x = age)) - 49.872 + 7.3309 * log(x = age) + 0.77338 * (log(x = age) ^ 2))
    }
    ## Calculate "h100.EKL.I.yield.class.YC" for yield class "YC", based on "age" and "SI.h100.yield.class.1", using the function by Nagel (1999).
    for (cur.name.suffix in name.suffixes) {
        assign(x = paste0("h100.EKL.I.yield.class.", cur.name.suffix),
               value = SI.h100.yield.class.1 * (0.52684 + 0.10542 * log(x = age)) - 49.872 + 7.3309 * log(x = age) + 0.77338 * (log(x = age)) ^ 2)
    }
    ## Calculate "SI.h100.yield.class.YC.vec" for yield class "YC", based on "age" and "h100.yield.class.YC", using the function by Nagel (1999) [This procedure is a bit redundant, since it simply results in the corresponding "SI.h100.yield.class.YC" value defined above. I nevertheless opt for it, in order to make sure that the test data for sensitivity analysis is created in exactly the same way as the original data for model fitting was (cp. block "Create "gmax_merged_1.5.RData"").]
    for (cur.name.suffix in name.suffixes) {
        h100 <- get(x = paste0("h100.yield.class.", cur.name.suffix))
        assign(x = paste0("SI.h100.yield.class.", cur.name.suffix, ".vec"),
               value = (h100 + 49.872 - 7.3309 * log(x = age) - 0.77338 * ((log(x = age)) ^ 2)) / (0.52684 + 0.10542 * log(x = age)))
    }
    ## Calculate "SI.h100.diff.EKL.I.yield.class.YC = SI.h100.yield.class.YC.vec - SI.h100.yield.class.1.vec" for yield class "YC".
    for (cur.name.suffix in name.suffixes) {
        SI.h100.vec <- get(x = paste0("SI.h100.yield.class.", cur.name.suffix, ".vec"))
        assign(x = paste0("SI.h100.diff.EKL.I.yield.class.", cur.name.suffix),
               value = SI.h100.vec - SI.h100.yield.class.1.vec)
    }
    ## Create a data frame per yield class with columns "age", "yield.class", "h100", "h100.EKL.I", "SI.h100", and "SI.h100.diff.EKL.I".
    for (cur.name.suffix in name.suffixes) {
        assign(x = paste0("yield.class.", cur.name.suffix, ".df"),
               value = data.frame("age" = age,
                                  "yield.class" = as.factor(x = ifelse(test = cur.name.suffix == ".1", yes = -1, no = as.numeric(x = cur.name.suffix))),
                                  "h100" = get(x = paste0("h100.yield.class.", cur.name.suffix)),
                                  "h100.EKL.I" = get(x = paste0("h100.EKL.I.yield.class.", cur.name.suffix)),
                                  "SI.h100" = get(x = paste0("SI.h100.yield.class.", cur.name.suffix, ".vec")),
                                  "SI.h100.diff.EKL.I" = get(x = paste0("SI.h100.diff.EKL.I.yield.class.", cur.name.suffix))))
    }
    ## Concatenate the "yield.class.YC.df" data frames into "nagel.SPECIES".
    final.df.name <- paste0("nagel.", cur.species.name)
    assign(x = final.df.name,
           value = data.frame(NULL))
    for (cur.name.suffix in name.suffixes) {
        assign(x = final.df.name,
               value = rbind(get(x = final.df.name),
                             get(x = paste0("yield.class.", cur.name.suffix, ".df"))))
    }
    ## Add final data frame to the vector of names of objects meant to be saved.
    kgmaxObjects <- c(final.df.name, kgmaxObjects)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))

####################################
## Create "gmax_merged_5.3.RData" ##
####################################
## Based on version 5.2.
## In this version, "bart.SPECIES.clean.1.8" contains an additional column "SI.h100.class.new", which consists of the SI.h100 class as the result of comparing column "SI.h100" with values from Schober (1995) (moderate thinning).
## Function by Nagel (1999) (see email by Matthias Schmidt from 2017-04-27 12:06):
## fi1.2$SI_h100 <- (fi1.2$h100+49.87200-7.33090*log(fi1.2$alt)-0.77338*((log(fi1.2$alt))^2.0))/(0.52684+0.10542*log(fi1.2$alt))
kBaseFileVersion <- "5.2"
kBaseFileName <- paste0(kDataDir,"gmax_merged_", kBaseFileVersion, ".RData")
kFileVersion <- "5.3"
kFileName <- paste0(kDataDir,"gmax_merged_", kFileVersion, ".RData")
## Load base file.
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Loop over all species.
for (cur.species.name in c("beech", "spruce")) {
    ## Get name of data frame for current species.
    data.frame.name <- paste0("bart.", cur.species.name, ".clean.1.8")
    ## Initiate column "SI.h100.class.new".
    bart.species.clean.1.8 <- get(x = data.frame.name)
    bart.species.clean.1.8[["SI.h100.class.new"]] <- NA
    ## Set values for SI.h100. Values for yield classes 4, 3, 2, and 1 are taken from Schober (1995) (moderate thinning). Values for yield classes 0, -1, and -2 are linearly interpolated from values for yield classes 2 and 1.
    if (cur.species.name == "beech") {
        SI.h100.yield.class.4 <- 20.7
        SI.h100.yield.class.3 <- 24.7
        SI.h100.yield.class.2 <- 28.6
        SI.h100.yield.class.1 <- 32.4
    }
    if (cur.species.name == "spruce") {
        SI.h100.yield.class.4 <- 23.5
        SI.h100.yield.class.3 <- 27.2
        SI.h100.yield.class.2 <- 31.2
        SI.h100.yield.class.1 <- 35.1
    }
    SI.h100.yield.class.0 <- SI.h100.yield.class.1 + SI.h100.yield.class.1 - SI.h100.yield.class.2
    SI.h100.yield.class..1 <- SI.h100.yield.class.0 + SI.h100.yield.class.1 - SI.h100.yield.class.2
    SI.h100.yield.class..2 <- SI.h100.yield.class..1 + SI.h100.yield.class.1 - SI.h100.yield.class.2
    ## Calculate ranges for SI.h100 per yield class.
    SI.h100.range.yield.class.3 <- c(
        SI.h100.yield.class.3 + (SI.h100.yield.class.4 - SI.h100.yield.class.3) / 2,
        SI.h100.yield.class.3 - (SI.h100.yield.class.3 - SI.h100.yield.class.2) / 2)
    SI.h100.range.yield.class.2 <- c(
        SI.h100.yield.class.2 + (SI.h100.yield.class.3 - SI.h100.yield.class.2) / 2,
        SI.h100.yield.class.2 - (SI.h100.yield.class.2 - SI.h100.yield.class.1) / 2)
    SI.h100.range.yield.class.1 <- c(
        SI.h100.yield.class.1 + (SI.h100.yield.class.2 - SI.h100.yield.class.1) / 2,
        SI.h100.yield.class.1 - (SI.h100.yield.class.1 - SI.h100.yield.class.0) / 2)
    SI.h100.range.yield.class.0 <- c(
        SI.h100.yield.class.0 + (SI.h100.yield.class.1 - SI.h100.yield.class.0) / 2,
        SI.h100.yield.class.0 - (SI.h100.yield.class.0 - SI.h100.yield.class..1) / 2)
    SI.h100.range.yield.class..1 <- c(
        SI.h100.yield.class..1 + (SI.h100.yield.class.0 - SI.h100.yield.class..1) / 2,
        SI.h100.yield.class..1 - (SI.h100.yield.class..1 - SI.h100.yield.class..2) / 2)
    ## Compare column "SI.h100" with the ranges and set column "SI.h100.class.new" to the approriate value.
    for (cur.yield.class.name in c("3", "2", "1", "0", ".1")) {
        yield.class <- ifelse(test = cur.yield.class.name == ".1",
                              yes = -1,
                              no = as.numeric(cur.yield.class.name))
        yield.class.range <- get(x = paste0("SI.h100.range.yield.class.", cur.yield.class.name))
        bart.species.clean.1.8[bart.species.clean.1.8[["SI.h100"]] > yield.class.range[1] &
                               bart.species.clean.1.8[["SI.h100"]] <= yield.class.range[2],
                               "SI.h100.class.new"] <- yield.class
    }
    ## Turn column "SI.h100.class.new" into a factor.
    bart.species.clean.1.8[["SI.h100.class.new"]] <- as.factor(x = bart.species.clean.1.8[["SI.h100.class.new"]])
    ## Assign new version of data frame for current species.
    assign(x = data.frame.name,
           value = bart.species.clean.1.8)
}
## Save results.
kgmaxBeechObjects <- kgmaxObjects[grepl(pattern = ".beech", x = kgmaxObjects)]
kgmaxBeechObjects <- kgmaxBeechObjects[order(kgmaxBeechObjects)]
kgmaxSpruceObjects <- kgmaxObjects[grepl(pattern = ".spruce", x = kgmaxObjects)]
kgmaxSpruceObjects <- kgmaxSpruceObjects[order(kgmaxSpruceObjects)]
kgmaxUTMObjects <- kgmaxObjects[grepl(pattern = "_utm", x = kgmaxObjects)]
kgmaxUTMObjects <- kgmaxUTMObjects[order(kgmaxUTMObjects)]
kgmaxObjects <- c(kgmaxBeechObjects, kgmaxSpruceObjects, kgmaxUTMObjects)
save(list = kgmaxObjects,
     file = kFileName,
     precheck = TRUE)
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.start))
