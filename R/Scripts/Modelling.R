##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kFileVersion <- "2.7"
kFileName <- paste0(kDataDir, "gmax_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)
models <- vector(mode = "list")
models[["mgcv..gam"]] <- vector(mode = "list")
models[["stats..nls"]] <- vector(mode = "list")
models[["nls2..nls2"]] <- vector(mode = "list")
models[["minpack.lm..nlsLM"]] <- vector(mode = "list")
kFormulas <- vector(mode = "list")
kStartValsGrids <- vector(mode = "list")
kStartValsVecs <- vector(mode = "list")
kPrintSumries <- TRUE
kPrintSumries <- FALSE
kFunctionsToUse <- c("mgcv..gam")
kFunctionsToUse <- c("stats..nls")
kFunctionsToUse <- c("nls2..nls2")
## kFunctionsToUse <- c("minpack.lm..nlsLM")
## kFunctionsToUse <- c("stats..nls", "minpack.lm..nlsLM")
## kFunctionsToUse <- c("stats..nls", "nls2..nls2")
## kFunctionsToUse <- c("stats..nls", "nls2..nls2", "minpack.lm..nlsLM")

##########
## GAMs ##
##########
## Setup for model "GAM_gha_sh100".
kFormulas[["GAM_gha_sh100"]] <- as.formula(object = "gha ~ s(h100, k = 5)")
## Setup for model "GAM_gha_sh100.EKL.I".
kFormulas[["GAM_gha_sh100.EKL.I"]] <- as.formula(object = "gha ~ s(h100.EKL.I, k = 5)")
## Setup for model ""GAM_gha_sSI.h100"".
kFormulas[["GAM_gha_sSI.h100"]] <- as.formula(object = "gha ~ s(SI.h100, k = 26)")
## Evaluate and store models.
kFunction <- "mgcv..gam"
if (any(grepl(pattern = kFunction,
          x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        models[["mgcv..gam"]][[cur.formula.name]] <- mgcv::gam(formula = kFormulas[[cur.formula.name]],
                                                               data = bart.clean)
    }}

############
## Sterba ##
############
## Setup for model "Sterba_dgGmax". For possible start values cp. Sterba (1987), tab. 2.
kFormulas[["Sterba_dgGmax"]] <- as.formula(object = "dg ~ 1 / (a0 * (h100 ^ a1) * nha + b0 * (h100 ^ b1))")  ## cp. Wördehoff et al. (2014), (Gl. 1)
kStartValsGrids[["Sterba_dgGmax"]] <- expand.grid("a0" = c(-1, 1),
                                                  "a1" = c(0, 1),
                                                  "b0" = c(0, 1),
                                                  "b1" = c(-3, 0))
kStartValsVecs[["Sterba_dgGmax"]] <- c("a0" = 1 * 10 ^ -6,  ## "stats::nls" and "minpack.lm::nlsLM" converge with these starting values.
                                       "a1" = 0,
                                       "b0" = 2,
                                       "b1" = -2)
## ## Setup for model "Sterba_NGmax".
kFormulas[["Sterba_NGmax"]] <- as.formula(object = "nha ~ (b0 / a0) * (2 * b0 * dg) ^ (a1 / b1 -1)")  ## cp. Wördehoff et al. (2014), (Gl. 2)
kStartValsGrids[["Sterba_NGmax"]] <- expand.grid("a0" = c(-2:2),
                                                 "a1" = c(-2:2),
                                                 "b0" = c(-2:2),
                                                 "b1" = c(-2:2))
kStartValsVecs[["Sterba_NGmax"]] <- c("a0" = -1,
                                      "a1" = -2,
                                      "b0" = 0,
                                      "b1" = 1)
## Setup for model "Sterba_Gmax". For possible start values  cp. Wördehoff (2016), tab. 3.6.
kFormulas[["Sterba_Gmax"]] <- as.formula(object = "gha ~ pi / (16 * a0 * b0 * (h100 ^(a1 + b1)))")  ## cp. Wördehoff et al. (2014), (Gl. 3)
kStartValsGrids[["Sterba_Gmax"]] <- expand.grid("a0" = c(4 * 10 -6, 1),
                                                "a1" = c(0, 1),
                                                "b0" = c(0, 1),
                                                "b1" = c(-2, 2))
kStartValsVecs[["Sterba_Gmax"]] <- c("a0" = 4 * 10 ^ -6,
                                     "a1" = 0.1,
                                     "b0" = 0.1,
                                     "b1" = -1)
## Evaluate and store models fitted with "stats::nls".
kFunction <- "stats..nls"
if (any(grepl(pattern = kFunction,
          x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        if (grepl(pattern = "Sterba", x = cur.formula.name, fixed = TRUE)) {
            try(expr = 
                    models[["stats..nls"]][[cur.formula.name]] <- stats::nls(formula = kFormulas[[cur.formula.name]],
                                                                             data = bart.clean,
                                                                             start = kStartValsVecs[[cur.formula.name]])
                )
        }
    }}
## Evaluate and store models fitted with "nls2::nls2"
kFunction <- "nls2..nls2"
if (any(grepl(pattern = kFunction,
          x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        if (grepl(pattern = "Sterba", x = cur.formula.name, fixed = TRUE)) {
            try(expr = 
                    models[["nls2..nls2"]][[cur.formula.name]] <- nls2::nls2(formula = kFormulas[[cur.formula.name]],
                                                                             data = bart.clean,
                                                                             start = kStartValsGrids[[cur.formula.name]],
                                                                             algorithm = "random-search",
                                                                             control = (stats::nls.control(maxiter = 100,
                                                                                                           minFactor = 10 ^ -10)))
                )
        }
    }}
## Evaluate and store models fitted with "minpack.lm::nlsLM"
kFunction <- "minpack.lm..nlsLM"
if (any(grepl(pattern = kFunction,
          x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        if (grepl(pattern = "Sterba", x = cur.formula.name, fixed = TRUE)) {
            try(expr = 
                    models[["minpack.lm..nlsLM"]][[cur.formula.name]] <- minpack.lm::nlsLM(formula = kFormulas[[cur.formula.name]],
                                                                                           data = bart.clean,
                                                                                           start = kStartValsVecs[[cur.formula.name]])
                )
        }
    }}

#####################
## Print summaries ##
#####################
## Print model summaries.
if (kPrintSumries) {
    for (cur.model.name in names(x = models)) {
        print(x = paste0("Model: ", cur.model.name))
        print(x = summary(object = models[[cur.model.name]]))
    }
}
