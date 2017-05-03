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
## bart[bart$edvid == "05451102",c(1,2,3,4,8,18)]
## Copy mean height from all EKLs for moderate thinning of spruce from Schober (1975).
EKL.I = data.frame(age = seq(from = 20, to = 120, by = 5),
                   mean.height = c(7.1, 9.2, 11.5, 14.1, 16.6, 19.0, 21.2, 23.1, 24.7, 26.1, 27.4, 28.6, 29.7, 30.7, 31.6, 32.5, 33.3, 34.1, 34.8, 35.4, 35.9))
EKL.II = data.frame(age = seq(from = 20,                              to = 120,                              by = 5),
                    mean.height = c(5.1, 6.7, 8.6, 10.7, 12.8, 14.9, 16.9, 18.8, 20.5, 22.0, 23.3, 24.5, 25.6, 26.6, 27.6, 28.5, 29.3, 30.1, 30.8, 31.5, 32.1))
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
## Calculate mean heights not listed in Schober (1975) for all EKLs for moderate thinning of spruce.
for (EKL.current.name in names(yield.table.spruce.schober.1975$moderate.thinning)) {
    EKL.current <- get(x = EKL.current.name)
    age <- EKL.current$age 
    mean.height <- EKL.current$mean.height
    diff.age <- diff(x = age)
    diff.mean.height <- diff(x = mean.height)
    diff.mean.height.per.year <- diff.mean.height / diff.age
    mean.height.complete <- list(NA)
    for (element in 1:length(diff.age)) {
        mean.height.complete[element] <- list(c(mean.height[element],
                                                rep(x = NA,
                                                    times = diff.age[element] - 1)))
    }
    mean.height.complete <- unlist(x = mean.height.complete)
    mean.height.na.elements <- which(x = is.na(x = mean.height.complete))
    element2 <- 0
    for (element in 1:length(mean.height.complete)) {
        if (element %% 5 == 1) {
            element2 <- element2 + 1
        }
        base.mean.height <- mean.height[element2]
        increment.yearly <- diff.mean.height.per.year[element2]
        if (element %% 5 != 0) {
            mult.fact <- ((element %% 5) - 1)
        } else {
            mult.fact <- 4
        }
        increment.mean.height <- mult.fact * increment.yearly
        mean.height.complete[element] <- base.mean.height + increment.mean.height
    }
    mean.height.complete[length(x = mean.height.complete) + 1] <- mean.height[length(x = mean.height)]
    EKL.current.complete <- data.frame(age = seq(from = age[1],
                                                 to = age[length(x = age)]),
                                       mean.height = mean.height.complete)
    yield.table.spruce.schober.1975$moderate.thinning[[paste0(EKL.current.name, ".complete")]] <-  EKL.current.complete
}
## Copy mean height from all EKLs for heavy thinning of spruce from Schober (1975).
EKL.I = data.frame(age = seq(from = 20, to = 120, by = 5),
                   mean.height = c(8.5, 10.5, 12.1, 14.8, 17.4, 19.8, 22.1, 24.0, 25.7, 27.1, 28.4, 29.6, 30.7, 31.7, 32.6, 33.5, 34.3, 35.0, 35.7, 36.4, 36.9))
EKL.II = data.frame(age = seq(from = 20, to = 120, by = 5),
                    mean.height = c(5.4, 7.3, 9.3, 11.4, 13.7, 15.8, 17.9, 19.8, 21.5, 23.0, 24.3, 25.5, 26.6, 27.6, 28.6, 29.5, 30.3, 31.1, 31.8, 32.5, 33.2))
EKL.III = data.frame(age = seq(from = 20, to = 120, by = 5),
                     mean.height = c(3.9, 5.6, 6.8, 8.4, 10.0, 12.1, 13.9, 15.5, 17.0, 18.4, 19.7, 20.9, 22.0, 23.0, 24.0, 24.9, 25.8, 26.7, 27.5, 28.3, 29.0))
yield.table.spruce.schober.1975 <- c(yield.table.spruce.schober.1975,
                                     list(heavy.thinning = list(EKL.I = EKL.I,
                                                                EKL.II = EKL.II,
                                                                EKL.III = EKL.III)))
## Calculate mean heights not listed in Schober (1975) for all EKLs for heavy thinning of spruce.
for (EKL.current.name in names(yield.table.spruce.schober.1975$heavy.thinning)) {
    EKL.current <- get(x = EKL.current.name)
    age <- EKL.current$age 
    mean.height <- EKL.current$mean.height
    diff.age <- diff(x = age)
    diff.mean.height <- diff(x = mean.height)
    diff.mean.height.per.year <- diff.mean.height / diff.age
    mean.height.complete <- list(NA)
    for (element in 1:length(diff.age)) {
        mean.height.complete[element] <- list(c(mean.height[element],
                                                rep(x = NA,
                                                    times = diff.age[element] - 1)))
    }
    mean.height.complete <- unlist(x = mean.height.complete)
    mean.height.na.elements <- which(x = is.na(x = mean.height.complete))
    element2 <- 0
    for (element in 1:length(mean.height.complete)) {
        if (element %% 5 == 1) {
            element2 <- element2 + 1
        }
        base.mean.height <- mean.height[element2]
        increment.yearly <- diff.mean.height.per.year[element2]
        if (element %% 5 != 0) {
            mult.fact <- ((element %% 5) - 1)
        } else {
            mult.fact <- 4
        }
        increment.mean.height <- mult.fact * increment.yearly
        mean.height.complete[element] <- base.mean.height + increment.mean.height
    }
    mean.height.complete[length(x = mean.height.complete) + 1] <- mean.height[length(x = mean.height)]
    EKL.current.complete <- data.frame(age = seq(from = age[1],
                                                 to = age[length(x = age)]),
                                       mean.height = mean.height.complete)
    yield.table.spruce.schober.1975$heavy.thinning[[paste0(EKL.current.name, ".complete")]] <-  EKL.current.complete
}
