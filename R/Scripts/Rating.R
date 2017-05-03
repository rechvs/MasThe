## Preamble
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"

############################
## Rate selected parcels. ##
############################
## Load base file.
kBaseFileVersion <- "1.4"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)

## "Manually" rate parcel "05451102" based on Schober (1975).
bart[bart$edvid == "05451102",c(1,2,3,4,8,18)]
## MODERATE THINNING, EKL I ##
## Calculate mean height at age 39.
kAge1 <- 35
kAge2 <- 40
kAgeDiff <- kAge2 - kAge1
kHeightAge1 <- 14.1
kHeightAge2 <- 16.6
kHeightDiff <- kHeightAge2 - kHeightAge1
age.in.question <- 39
height.in.question <- kHeightAge1 + (kHeightAge2 - kHeightAge1) / kAgeDiff * (age.in.question - kAge1)
height.in.question
## Calculate mean height at age 54.
kAge1 <- 50
kAge2 <- 55
kAgeDiff <- kAge2 - kAge1
kHeightAge1 <- 21.2
kHeightAge2 <- 23.1
kHeightDiff <- kHeightAge2 - kHeightAge1
age.in.question <- 54
height.in.question <- kHeightAge1 + (kHeightAge2 - kHeightAge1) / kAgeDiff * (age.in.question - kAge1)
height.in.question
## Copy mean height from all EKLs for moderate thinning of spruce from Schober (1975).
EKL.I = data.frame(age = seq(from = 20, to = 120, by = 5),
                   mean.height = c(7.1, 9.2, 11.5, 14.1, 16.6, 19.0, 21.2, 23.1, 24.7, 26.1, 27.4, 28.6, 29.7, 30.7, 31.6, 32.5, 33.3, 34.1, 24.8, 35.4, 35.9))
EKL.II = data.frame(age = seq(from = 20,                              to = 120,                              by = 5),
                    mean.height = c(5.1, 6.7, 8.6, 10.7, 12.8, 14.9, 16.9, 18.8, 20.5, 22.0, 23.3, 24.5, 25.6, 26.6, 27.6, 28.5, 29.3, 30.1, 30.8, 34.5, 32.1))
EKL.III = data.frame(age = seq(from = 20, to = 120, by = 5),
                     mean.height = c(3.9, 5.1, 6.2, 7.6, 9.3, 11.3, 13.1, 14.7, 16.2, 17.6, 18.9, 20.1, 21.2, 22.2, 23.2, 24.1, 25.0, 25.9, 26.7, 27.5, 28.2))
EKL.IV = data.frame(age = seq(from = 30, to = 120, by = 5),
                    mean.height = c(4.2, 5.5, 6.9, 8.3, 9.8, 11.3, 12.7, 14.0, 15.2, 16.3, 17.3, 18.3, 19.2, 20.1, 21.0, 21.8, 22.6, 23.3, 24.0))
EKL.V = data.frame(age = seq(from = 40, to = 100, by = 5),
                   mean.height = c(4.5, 5.6, 6.8, 8.0, 9.3, 10.5, 11.7, 12.8, 13.8, 14.8, 15.7, 16.5, 17.2))
yield.table.spruce.schober.1975 <- list(moderate.thinning = list(EKL.I = EKL.I,
                                                                 EKL.II = EKL.II,
                                                                 EKL.III = EKL.III,
                                                                 EKL.IV = EKL.IV,
                                                                 EKL.V = EKL.V))
