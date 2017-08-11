##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kFileVersion <- "3.8"
kFileName <- paste0(kDataDir, "gmax_merged_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)
models <- vector(mode = "list")
models[["mgcv..gam"]] <- vector(mode = "list")
models[["gamlss..gamlss"]] <- vector(mode = "list")
models[["stats..nls"]] <- vector(mode = "list")
models[["nls2..nls2"]] <- vector(mode = "list")
models[["minpack.lm..nlsLM"]] <- vector(mode = "list")
kFormulas <- vector(mode = "list")
kStartValsGrids <- vector(mode = "list")
kStartValsVecs <- vector(mode = "list")
kDistFamilies <- vector(mode = "list")
kPrintSumries <- TRUE
kPrintSumries <- FALSE
kFunctionsToUse <- NULL
kFunctionsToUse <- c(kFunctionsToUse, "mgcv..gam")
kFunctionsToUse <- c(kFunctionsToUse, "gamlss..gamlss")
kFunctionsToUse <- c(kFunctionsToUse, "stats..nls")
kFunctionsToUse <- c(kFunctionsToUse, "nls2..nls2")
kFunctionsToUse <- c(kFunctionsToUse, "minpack.lm..nlsLM")
kFunctionsToUse <- c(kFunctionsToUse, "stats..lm")
kFormulasToUse <- NULL
## Create a vector containing the names of all appropriate input data sources.
names.input.data.sources <- ls()[grepl(pattern = "bart.((beech)|(spruce)).clean.1.[067]", x = ls(), fixed = FALSE)]
objects.at.start <- sort(x = c(ls(), "objects.at.start"))  ## Required for cleaning up workspace after each block.

#########
## GAM ##
#########
kFormulasToUse <- c(kFormulasToUse, "GAM_gha_sh100")
kFormulasToUse <- c(kFormulasToUse, "GAM_gha_sh100.EKL.I")
kFormulasToUse <- c(kFormulasToUse, "GAM_gha_sSI.h100")
kFormulasToUse <- c(kFormulasToUse, "GAM_log.nha_h100_sh100_by_log.dg")
## Setup for model "GAM_gha_sh100".
kFormulas[["GAM_gha_sh100"]] <- as.formula(object = "gha ~ s(h100)")
## Setup for model "GAM_gha_sh100.EKL.I".
kFormulas[["GAM_gha_sh100.EKL.I"]] <- as.formula(object = "gha ~ s(h100.EKL.I)")
## Setup for model ""GAM_gha_sSI.h100"".
kFormulas[["GAM_gha_sSI.h100"]] <- as.formula(object = "gha ~ s(SI.h100)")
## Setup for model "GAM_log.nha_h100_sh100_by_log.dg".
kFormulas[["GAM_log.nha_h100_sh100_by_log.dg"]] <- as.formula(object = "log.nha ~ 1 + h100 + s(h100, by = log.dg)")
## Setup for model "GAM_log.nha_h100".
kFormulas[["GAM_log.nha_h100"]] <- as.formula(object = "log.nha ~ 1 + h100")
## Initiate "for" loop (for looping over all names of input data sources).
for (cur.input.data.source.name in names.input.data.sources) {
    input.data <- eval(expr = parse(text = cur.input.data.source.name))
    ## Evaluate and store models.
    kFunction <- "mgcv..gam"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "GAM_", x = cur.formula.name, fixed = TRUE)) {
                    try(expr = 
                            models[["mgcv..gam"]][[cur.input.data.source.name]][[cur.formula.name]] <- mgcv::gam(formula = kFormulas[[cur.formula.name]],
                                                                                                                 data = input.data))
                }
            }
        }
    }
}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

############
## GAMLSS ##
############
## Preamble.
kSigmaFormulas <- vector(mode = "list")
kNuFormulas <- vector(mode = "list")
kTauFormulas <- vector(mode = "list")
kColumnsToSelect <- vector(mode = "list")  ## Required for "gamlss::gamlss(...)" to avoid omission of more rows than necessary.
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCG_gha_h100")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCG_gha_psh100")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCG_gha_SI.h100")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCG_gha_SI.h100_hnn.neu")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCG_gha_h100.diff.EKL.I")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_gha_h100")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_gha_psh100")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_gha_SI.h100")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_gha_SI.h100_hnn.neu")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_gha_h100.diff.EKL.I")

## Setup for model "GAMLSS_BCCG_gha_h100".
kFormulas[["GAMLSS_BCCG_gha_h100"]] <- as.formula(object = "gha ~ h100")
kSigmaFormulas[["GAMLSS_BCCG_gha_h100"]] <- as.formula(object = "gha ~ h100")
kNuFormulas[["GAMLSS_BCCG_gha_h100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCG_gha_h100"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCG_gha_h100"]] <- "gamlss.dist::BCCG()"
kColumnsToSelect[["GAMLSS_BCCG_gha_h100"]] <- c("gha", "h100")

## Setup for model "GAMLSS_BCCG_gha_psh100".
kFormulas[["GAMLSS_BCCG_gha_psh100"]] <- as.formula(object = "gha ~ gamlss::ps(x = h100, df = 2)")
kSigmaFormulas[["GAMLSS_BCCG_gha_psh100"]] <- as.formula(object = "gha ~ gamlss::ps(x = h100, df = 2)")
kNuFormulas[["GAMLSS_BCCG_gha_psh100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCG_gha_psh100"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCG_gha_psh100"]] <- "gamlss.dist::BCCG()"
kColumnsToSelect[["GAMLSS_BCCG_gha_psh100"]] <- c("gha", "h100")

## Setup for model "GAMLSS_BCCG_gha_SI.h100".
kFormulas[["GAMLSS_BCCG_gha_SI.h100"]] <- as.formula(object = "gha ~ SI.h100")
kSigmaFormulas[["GAMLSS_BCCG_gha_SI.h100"]] <- as.formula(object = "gha ~ SI.h100")
kNuFormulas[["GAMLSS_BCCG_gha_SI.h100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCG_gha_SI.h100"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCG_gha_SI.h100"]] <- "gamlss.dist::BCCG()"
kColumnsToSelect[["GAMLSS_BCCG_gha_SI.h100"]] <- c("gha", "SI.h100")

## Setup for model "GAMLSS_BCCG_gha_SI.h100_hnn.neu".
kFormulas[["GAMLSS_BCCG_gha_SI.h100_hnn.neu"]] <- as.formula(object = "gha ~ SI.h100 * hnn.neu")
kSigmaFormulas[["GAMLSS_BCCG_gha_SI.h100_hnn.neu"]] <- as.formula(object = "gha ~ SI.h100 * hnn.neu")
kNuFormulas[["GAMLSS_BCCG_gha_SI.h100_hnn.neu"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCG_gha_SI.h100_hnn.neu"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCG_gha_SI.h100_hnn.neu"]] <- "gamlss.dist::BCCG()"
kColumnsToSelect[["GAMLSS_BCCG_gha_SI.h100_hnn.neu"]] <- c("gha", "SI.h100", "hnn.neu")

## Setup for model "GAMLSS_BCCG_gha_h100.diff.EKL.I".
kFormulas[["GAMLSS_BCCG_gha_h100.diff.EKL.I"]] <- as.formula(object = "gha ~ h100.diff.EKL.I")
kSigmaFormulas[["GAMLSS_BCCG_gha_h100.diff.EKL.I"]] <- as.formula(object = "gha ~ h100.diff.EKL.I")
kNuFormulas[["GAMLSS_BCCG_gha_h100.diff.EKL.I"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCG_gha_h100.diff.EKL.I"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCG_gha_h100.diff.EKL.I"]] <- "gamlss.dist::BCCG()"
kColumnsToSelect[["GAMLSS_BCCG_gha_h100.diff.EKL.I"]] <- c("gha", "h100.diff.EKL.I")

## Setup for model "GAMLSS_BCCGo_gha_h100".
kFormulas[["GAMLSS_BCCGo_gha_h100"]] <- as.formula(object = "gha ~ h100")
kSigmaFormulas[["GAMLSS_BCCGo_gha_h100"]] <- as.formula(object = "gha ~ h100")
kNuFormulas[["GAMLSS_BCCGo_gha_h100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCGo_gha_h100"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCGo_gha_h100"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_gha_h100"]] <- c("gha", "h100")

## Setup for model "GAMLSS_BCCGo_gha_psh100".
kFormulas[["GAMLSS_BCCGo_gha_psh100"]] <- as.formula(object = "gha ~ gamlss::ps(x = h100, df = 2)")
kSigmaFormulas[["GAMLSS_BCCGo_gha_psh100"]] <- as.formula(object = "gha ~ gamlss::ps(x = h100, df = 2)")
kNuFormulas[["GAMLSS_BCCGo_gha_psh100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCGo_gha_psh100"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCGo_gha_psh100"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_gha_psh100"]] <- c("gha", "h100")

## Setup for model "GAMLSS_BCCGo_gha_SI.h100".
kFormulas[["GAMLSS_BCCGo_gha_SI.h100"]] <- as.formula(object = "gha ~ SI.h100")
kSigmaFormulas[["GAMLSS_BCCGo_gha_SI.h100"]] <- as.formula(object = "gha ~ SI.h100")
kNuFormulas[["GAMLSS_BCCGo_gha_SI.h100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCGo_gha_SI.h100"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCGo_gha_SI.h100"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_gha_SI.h100"]] <- c("gha", "SI.h100")

## Setup for model "GAMLSS_BCCGo_gha_SI.h100_hnn.neu".
kFormulas[["GAMLSS_BCCGo_gha_SI.h100_hnn.neu"]] <- as.formula(object = "gha ~ SI.h100 * hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_gha_SI.h100_hnn.neu"]] <- as.formula(object = "gha ~ SI.h100 * hnn.neu")
kNuFormulas[["GAMLSS_BCCGo_gha_SI.h100_hnn.neu"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCGo_gha_SI.h100_hnn.neu"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCGo_gha_SI.h100_hnn.neu"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_gha_SI.h100_hnn.neu"]] <- c("gha", "SI.h100", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_gha_h100.diff.EKL.I".
kFormulas[["GAMLSS_BCCGo_gha_h100.diff.EKL.I"]] <- as.formula(object = "gha ~ h100.diff.EKL.I")
kSigmaFormulas[["GAMLSS_BCCGo_gha_h100.diff.EKL.I"]] <- as.formula(object = "gha ~ h100.diff.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_gha_h100.diff.EKL.I"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_BCCGo_gha_h100.diff.EKL.I"]] <- as.formula(object = "~1")
kDistFamilies[["GAMLSS_BCCGo_gha_h100.diff.EKL.I"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_gha_h100.diff.EKL.I"]] <- c("gha", "h100.diff.EKL.I")

## Initiate "for" loop (for looping over all names of input data sources).
for (cur.input.data.source.name in names.input.data.sources) {
    input.data <- eval(expr = parse(text = cur.input.data.source.name))
    ## Evaluate and store models.
    kFunction <- "gamlss..gamlss"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "GAMLSS_", x = cur.formula.name, fixed = TRUE)) {
                    try(expr =
                            models[["gamlss..gamlss"]][[cur.input.data.source.name]][[cur.formula.name]] <- gamlss::gamlss(formula = kFormulas[[cur.formula.name]],
                                                                                                                           sigma.formula = kSigmaFormulas[[cur.formula.name]],
                                                                                                                           nu.formula = kNuFormulas[[cur.formula.name]],
                                                                                                                           tau.formula = kTauFormulas[[cur.formula.name]],
                                                                                                                           family = eval(expr = parse(text = kDistFamilies[[cur.formula.name]])),
                                                                                                                           data = na.omit(object = subset(x = input.data, select = kColumnsToSelect[[cur.formula.name]])),
                                                                                                                           method = RS(1000)))
                }
            }
        }
    }
}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

############
## Sterba ##
############
## Preamble.
## kFormulasToUse <- c(kFormulasToUse, "Sterba_dgGmax")
## kFormulasToUse <- c(kFormulasToUse, "Sterba_NGmax")
## kFormulasToUse <- c(kFormulasToUse, "Sterba_Gmax")
## Setup for model "Sterba_dgGmax".
## Source of model formula: Wördehoff et al. (2014), (Gl. 1) or Sterba (1975), eq. (12).
## Source of possible start values: Sterba (1987), tab. 2.
kFormulas[["Sterba_dgGmax"]] <- as.formula(object = "dg ~ 1 / (a0 * (h100 ^ a1) * nha + b0 * (h100 ^ b1))")
kStartValsGrids[["Sterba_dgGmax"]] <- expand.grid("a0" = seq(from = 1.260e-6, to =1.261e-6, by = 1e-10),
                                                  "a1" = seq(from = 0.840, to = 0.841, by = 1e-4),
                                                  "b0" = seq(from = 2.0135, to = 2.0145, by = 1e-4),
                                                  "b1" = seq(from = -1.3615, to = -1.3625, by = -1e-4))
kStartValsVecs[["Sterba_dgGmax"]] <- c("a0" = 1 * 10 ^ -6,
                                       "a1" = 0,
                                       "b0" = 2,
                                       "b1" = -2)
## ## Setup for model "Sterba_NGmax".
## Source of model formula: Wördehoff et al. (2014), (Gl. 2) or Sterba (1981), eq. (11).
kFormulas[["Sterba_NGmax"]] <- as.formula(object = "nha ~ (b0 / a0) * (2 * b0 * dg) ^ (a1 / b1 -1)")
kStartValsGrids[["Sterba_NGmax"]] <- expand.grid("a0" = c(-2:2),
                                                 "a1" = c(-2:2),
                                                 "b0" = c(-2:2),
                                                 "b1" = c(-2:2))
kStartValsVecs[["Sterba_NGmax"]] <- c("a0" = -1,
                                      "a1" = -2,
                                      "b0" = 0,
                                      "b1" = 1)
## Setup for model "Sterba_Gmax".
## Source of model formula: Wördehoff et al. (2014), (Gl. 3) or Sterba (1975), eq. (10).
## Source of possible start values: Wördehoff (2016), tab. 3.6.
kFormulas[["Sterba_Gmax"]] <- as.formula(object = "gha / 10000 ~ pi / (16 * a0 * b0 * (h100 ^(a1 + b1)))")
kStartValsGrids[["Sterba_Gmax"]] <- expand.grid("a0" = c(4 * 10 -6, 1),
                                                "a1" = c(0, 1),
                                                "b0" = c(0, 1),
                                                "b1" = c(-2, 2))
kStartValsVecs[["Sterba_Gmax"]] <- c("a0" = 4 * 10 ^ -6,
                                     "a1" = 0.1,
                                     "b0" = 0.1,
                                     "b1" = -1)
## Initiate "for" loop (for looping over all names of input data sources).
for (cur.input.data.source.name in names.input.data.sources) {
    input.data <- eval(expr = parse(text = cur.input.data.source.name))
    ## Evaluate and store models fitted with "stats::nls".
    kFunction <- "stats..nls"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "Sterba_",
                          x = cur.formula.name,
                          fixed = TRUE)) {
                    try(expr =
                            models[["stats..nls"]][[cur.input.data.source.name]][[cur.formula.name]] <- stats::nls(formula = kFormulas[[cur.formula.name]],
                                                                                                                   data = input.data,
                                                                                                                   start = kStartValsVecs[[cur.formula.name]])
                        )
                }
            }
        }
    }
    ## Evaluate and store models fitted with "nls2::nls2"
    kFunction <- "nls2..nls2"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "Sterba_", x = cur.formula.name, fixed = TRUE)) {
                    try(expr =
                            models[["nls2..nls2"]][[cur.input.data.source.name]][[cur.formula.name]] <- nls2::nls2(formula = kFormulas[[cur.formula.name]],
                                                                                                                   data = input.data,
                                                                                                                   start = kStartValsGrids[[cur.formula.name]],
                                                                                                                   algorithm = "random-search",
                                                                                                                   control = (stats::nls.control(maxiter = 100,
                                                                                                                                                 minFactor = 10 ^ -10)))
                        )
                }
            }
        }
    }
    ## Evaluate and store models fitted with "minpack.lm::nlsLM"
    kFunction <- "minpack.lm..nlsLM"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "Sterba_", x = cur.formula.name, fixed = TRUE)) {
                    try(expr =
                            models[["minpack.lm..nlsLM"]][[cur.input.data.source.name]][[cur.formula.name]] <- minpack.lm::nlsLM(formula = kFormulas[[cur.formula.name]],
                                                                                                                                 data = input.data,
                                                                                                                                 start = kStartValsVecs[[cur.formula.name]])
                        )
                }
            }
        }
    }
}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

########
## LM ##
########
## Select formulas to use.
kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg")
kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg_fixed_slope")
kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg_h100")
kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg_h100_ni")
## Setup for model "LM_log.nha_log.dg".
kFormulas[["LM_log.nha_log.dg"]] <- as.formula(object = "log.nha ~ log.dg")
## Setup for model "LM_log.nha_log.dg_fixed_slope".
kFormulas[["LM_log.nha_log.dg_fixed_slope"]] <- as.formula(object = "log.nha - -1.605 * log.dg ~ 1")
## Setup for model "LM_log.nha_log.dg_h100".
kFormulas[["LM_log.nha_log.dg_h100"]] <- as.formula(object = "log.nha ~ log.dg * h100")
## Setup for model "LM_log.nha_log.dg_h100_ni".
kFormulas[["LM_log.nha_log.dg_h100_ni"]] <- as.formula(object = "log.nha ~ log.dg + h100")
## Initiate "for" loop (for looping over all names of input data sources).
for (cur.input.data.source.name in names.input.data.sources) {
    input.data <- eval(expr = parse(text = cur.input.data.source.name))
    ## Evaluate and store models.
    kFunction <- "stats..lm"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "LM_", x = cur.formula.name, fixed = TRUE)) {
                    try(expr =
                            models[["stats..lm"]][[cur.input.data.source.name]][[cur.formula.name]] <- stats::lm(formula = kFormulas[[cur.formula.name]],
                                                                                                                 data = input.data,
                                                                                                                 na.action = na.omit))
                }
            }
        }
    }
}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

#########
## GLM ##
#########
## Select formulas to use.
kFormulasToUse <- c(kFormulasToUse, "GLM_log.nha_1_log.dg")
## Setup for model "GLM_log.nha_1_log.dg".
kFormulas[["GLM_log.nha_1_log.dg"]] <- as.formula(object = "log.nha ~ 1 + log.dg")
kDistFamilies[["GLM_log.nha_1_log.dg"]] <- "Gamma(link = log)"
## Initiate "for" loop (for looping over all names of input data sources).
for (cur.input.data.source.name in names.input.data.sources) {
    input.data <- eval(expr = parse(text = cur.input.data.source.name))
    ## Evaluate and store models.
    kFunction <- "stats..lm"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "GLM_", x = cur.formula.name, fixed = TRUE)) {
                    try(expr =
                            models[["stats..glm"]][[cur.input.data.source.name]][[cur.formula.name]] <- stats::glm(formula = kFormulas[[cur.formula.name]],
                                                                                                                  family = eval(expr = parse(text = kDistFamilies[[cur.formula.name]])),
                                                                                                                  data = input.data,
                                                                                                                  na.action = na.omit))
                }
            }
        }
    }
}

#####################
## Print summaries ##
#####################
## Print model summaries.
if (kPrintSumries) {
    for (cur.function.name in names(x = models)) {
        for (cur.data.frame.name in names(x = models[[cur.function.name]])) {
            for (cur.model.name in names(x = models[[cur.function.name]][[cur.data.frame.name]])) {
                print(x = paste0("Function: ", cur.function.name))
                print(x = paste0("Data frame: ", cur.data.frame.name))
                print(x = paste0("Model: ", cur.model.name))
                print(x = summary(object = models[[cur.function.name]][[cur.data.frame.name]][[cur.model.name]]))
            }}}}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

####################
## Output to file ##
####################
## Store file path to main output directory in "kOutputDirPath".
kOutputDirPath <- "R/Output/"
## Loop over all modelling functions.
for (cur.function.name in names(x = models)) {
    ## Loop over all species.
    for (cur.species.name in c("beech", "spruce")) {
        ## Create template data frame in which to store the relevant benchmarks of function...
        if (cur.function.name == "stats..lm") {  ## ..."stats::lm".
            cur.function.species.benchmark.df <- data.frame("formula" = vector(mode = "character"),
                                                            "data.frame" = vector(mode = "character"),
                                                            "r.squared" = vector(mode = "numeric"),
                                                            "adj.r.squared" = vector(mode = "numeric"))
        }
        if (cur.function.name == "mgcv..gam") {  ## ..."mgcv::gam".
            cur.function.species.benchmark.df <- data.frame("formula" = vector(mode = "character"),
                                                            "data.frame" = vector(mode = "character"),
                                                            "GCV" = vector(mode = "numeric"))
        }
        if (cur.function.name == "gamlss..gamlss" || cur.function.name == "stats..glm") {  ## ..."gamlss::gamlss" or "stats::glm".
            cur.function.species.benchmark.df <- data.frame("formula" = vector(mode = "character"),
                                                            "distribution" = vector(mode = "character"),
                                                            "data.frame" = vector(mode = "character"),
                                                            "AIC" = vector(mode = "numeric"))
        }
        ## Generate a vector of appropriate data frame names based on the current species.
        cur.possible.data.frame.names <- names(x = models[[cur.function.name]])[grepl(pattern = cur.species.name,
                                                                                      x = names(x = models[[cur.function.name]]))]
        ## Loop over all appropriate data frames.
        for (cur.data.frame.name in cur.possible.data.frame.names) {
            ## Loop over all models.
            for (cur.model.name in names(x = models[[cur.function.name]][[cur.data.frame.name]])) {
                ## Store current model in "cur.model".
                cur.model <- models[[cur.function.name]][[cur.data.frame.name]][[cur.model.name]]
                ## Store "kFormulas[[cur.model.name]]" as a string in "cur.formula".
                cur.formula <- format(x = kFormulas[[cur.model.name]])
                ## Create the first 2 columns (containing the model formula and the data frame name) of the benchmark data frame.
                cur.formula.data.frame.name.df <- data.frame("formula" = cur.formula,
                                                             "data.frame" = cur.data.frame.name)
                ## Store benchmarks of function...
                ## ..."stats::lm".
                if (cur.function.name == "stats..lm") {
                    ## Store current model summary in "cur.summary".
                    cur.summary <- summary(object = cur.model)
                    ## Store "cur.summary[["r.squared"]]" in "cur.r.squared".
                    cur.r.squared <- cur.summary[["r.squared"]]
                    ## Store "cur.summary[["adj.r.squared"]]" in "cur.adj.r.squared".
                    cur.adj.r.squared <- cur.summary[["adj.r.squared"]]
                    ## Store "cur.formula.data.frame.name.df", "cur.r.squared", and "cur.adj.r.squared" in a 1 row data frame "cur.model.benchmark.df". 
                    cur.model.benchmark.df <- data.frame(cur.formula.data.frame.name.df,
                                                         "r.squared" = cur.r.squared,
                                                         "adj.r.squared" = cur.adj.r.squared)
                    ## Append "cur.model.benchmark.df" to "cur.function.species.benchmark.df".
                    cur.function.species.benchmark.df <- rbind(cur.function.species.benchmark.df,
                                                               cur.model.benchmark.df)
                    ## Order "cur.function.species.benchmark.df".
                    cur.function.species.benchmark.df <- cur.function.species.benchmark.df[order(cur.function.species.benchmark.df[["r.squared"]],
                                                                                                 cur.function.species.benchmark.df[["adj.r.squared"]],
                                                                                                 decreasing = TRUE), ]
                    ## Create the name of the file for outputting "cur.function.species.benchmark.df".
                    cur.output.file.name <- paste0(kOutputDirPath,
                                                   cur.species.name,
                                                   "_LM_R-squared.txt")
                }
                ## ..."mgcv::gam".
                if (cur.function.name == "mgcv..gam") {
                    ## Store "cur.model[["gcv.ubre"]]" in "cur.gcv" (without name).
                    cur.gcv <- unname(obj = cur.model[["gcv.ubre"]])
                    ## Store "cur.formula.data.frame.name.df", and "cur.gcv" in a 1 row data frame "cur.model.benchmark.df". 
                    cur.model.benchmark.df <- data.frame(cur.formula.data.frame.name.df,
                                                         "GCV" = cur.gcv)
                    ## Append "cur.model.benchmark.df" to "cur.function.species.benchmark.df".
                    cur.function.species.benchmark.df <- rbind(cur.function.species.benchmark.df,
                                                               cur.model.benchmark.df)
                    ## Order "cur.function.species.benchmark.df" based on column "GCV".
                    cur.function.species.benchmark.df <- cur.function.species.benchmark.df[order(cur.function.species.benchmark.df[["GCV"]]), ]
                    ## Create the name of the file for outputting "cur.function.species.benchmark.df".
                    cur.output.file.name <- paste0(kOutputDirPath,
                                                   cur.species.name,
                                                   "_GAM_GCV.txt")
                }
                ## ..."gamlss::gamlss".
                if (cur.function.name == "gamlss..gamlss" || cur.function.name == "stats..glm") {
                    ## Store "cur.model[["aic"]]" in "cur.aic".
                    cur.aic <- cur.model[["aic"]]
                    ## Store "kDistFamilies[[cur.model.name]]" in "cur.dist.family".
                    cur.dist.family <- kDistFamilies[[cur.model.name]]
                    ## Store "cur.formula.data.frame.name.df", and "cur.aic" in a 1 row data frame "cur.model.benchmark.df".
                    cur.model.benchmark.df <- data.frame("formula" = cur.formula.data.frame.name.df[, 1],
                                                         "distribution" = cur.dist.family,
                                                         "data.frame" = cur.formula.data.frame.name.df[, 2],
                                                         "AIC" = cur.aic)
                    ## Append "cur.model.benchmark.df" to "cur.function.species.benchmark.df".
                    cur.function.species.benchmark.df <- rbind(cur.function.species.benchmark.df,
                                                               cur.model.benchmark.df)
                    ## Order "cur.function.species.benchmark.df" based on column "AIC".
                    cur.function.species.benchmark.df <- cur.function.species.benchmark.df[order(cur.function.species.benchmark.df[["AIC"]]), ]
                    ## Create the name of the file for outputting "cur.function.species.benchmark.df" (different one for "gamlss..gamlss" and for "stats..glm").
                    if (cur.function.name == "gamlss..gamlss") {
                        cur.file.suffix <- "_GAMLSS_AIC.txt"
                    }
                    if (cur.function.name == "stats..glm") {
                        cur.file.suffix <- "_GLM_AIC.txt"
                    }
                    cur.output.file.name <- paste0(kOutputDirPath,
                                                   cur.species.name,
                                                   cur.file.suffix)
                }
                ## Store and write output only if "cur.function.species.benchmark.df" (which should be deleted at the end of every "cur.function.name" loop) exists
                if (exists(x = "cur.function.species.benchmark.df")) {
                    ## Store printing of "cur.function.species.benchmark.df" in "cur.output", while left justifying output and suppressing row numbers and row names.
                    cur.output <- capture.output(print(x = format(x = cur.function.species.benchmark.df,
                                                                  justify="left",
                                                                  scientific = FALSE),
                                                       row.names = FALSE))
                    ## Write "cur.output" to "cur.output.file.name".
                    cat(cur.output,
                        file = cur.output.file.name,
                        sep = "\n",
                        fill = FALSE)
                }}}}
}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))
