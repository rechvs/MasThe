##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kFileVersion <- "3.7"
kFileName <- paste0(kDataDir, "gmax_merged_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)
models <- vector(mode = "list")
models[["mgcv..gam"]] <- vector(mode = "list")
models[["gamlss..gamlss"]] <- vector(mode = "list")
models[["stats..nls"]] <- vector(mode = "list")
models[["nls2..nls2"]] <- vector(mode = "list")
models[["minpack.lm..nlsLM"]] <- vector(mode = "list")
kFormulas <- vector(mode = "list")
kSigmaFormulas <- vector(mode = "list")
kNuFormulas <- vector(mode = "list")
kTauFormulas <- vector(mode = "list")
kStartValsGrids <- vector(mode = "list")
kStartValsVecs <- vector(mode = "list")
kColumnsToSelect <- vector(mode = "list")  ## Required for "gamlss::gamlss(...)" to avoid having to use "data = na.omit(DATA)".
kDistFamilyToUse <- vector(mode = "list")
kPrintSumries <- TRUE
## kPrintSumries <- FALSE
kFunctionsToUse <- NULL
kFunctionsToUse <- c(kFunctionsToUse, "mgcv..gam")
kFunctionsToUse <- c(kFunctionsToUse, "gamlss..gamlss")
kFunctionsToUse <- c(kFunctionsToUse, "stats..nls")
kFunctionsToUse <- c(kFunctionsToUse, "nls2..nls2")
kFunctionsToUse <- c(kFunctionsToUse, "minpack.lm..nlsLM")
kFunctionsToUse <- c(kFunctionsToUse, "stats..lm")
kFormulasToUse <- NULL
## kFormulasToUse <- c(kFormulasToUse, "GAM_gha_sh100")
## kFormulasToUse <- c(kFormulasToUse, "GAM_gha_sh100.EKL.I")
## kFormulasToUse <- c(kFormulasToUse, "GAM_gha_sSI.h100")
## kFormulasToUse <- c(kFormulasToUse, "Sterba_dgGmax")
## kFormulasToUse <- c(kFormulasToUse, "Sterba_NGmax")
## kFormulasToUse <- c(kFormulasToUse, "Sterba_Gmax")
## kFormulasToUse <- c(kFormulasToUse, "Reineke_improved_quadratic")
## Create a vector containing the names of all appropriate input data sources.
objects.present <- ls()
names.input.data.sources <- objects.present[grepl(pattern = "bart.((beech)|(spruce)).clean.1.[06]", x = objects.present, fixed = FALSE)]

#########
## GAM ##
#########
## Setup for model "GAM_gha_sh100".
kFormulas[["GAM_gha_sh100"]] <- as.formula(object = "gha ~ s(h100, k = 5)")
## Setup for model "GAM_gha_sh100.EKL.I".
kFormulas[["GAM_gha_sh100.EKL.I"]] <- as.formula(object = "gha ~ s(h100.EKL.I, k = 5)")
## Setup for model ""GAM_gha_sSI.h100"".
kFormulas[["GAM_gha_sSI.h100"]] <- as.formula(object = "gha ~ s(SI.h100, k = 26)")
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
                    models[["mgcv..gam"]][[cur.input.data.source.name]][[cur.formula.name]] <- mgcv::gam(formula = kFormulas[[cur.formula.name]],
                                                                                                         data = input.data)
                }
            }
        }
    }
}

############
## GAMLSS ##
############
## kFormulasToUse <- c(kFormulasToUse, "GAMLSS_gha_h100")
## kFormulasToUse <- c(kFormulasToUse, "GAMLSS_gha_psh100")
## kFormulasToUse <- c(kFormulasToUse, "GAMLSS_gha_SI.h100")
## kFormulasToUse <- c(kFormulasToUse, "GAMLSS_gha_SI.h100_hnn.neu")
## kFormulasToUse <- c(kFormulasToUse, "GAMLSS_gha_h100.diff.EKL.I")

## Setup for model "GAMLSS_gha_h100".
kFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "gha ~ h100")
kSigmaFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "gha ~ h100")
kNuFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "~1")
kDistFamilyToUse[["GAMLSS_gha_h100"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_gha_h100"]] <- c("gha", "h100")

## Setup for model "GAMLSS_gha_psh100".
kFormulas[["GAMLSS_gha_psh100"]] <- as.formula(object = "gha ~ gamlss::ps(x = h100, df = 2)")
kSigmaFormulas[["GAMLSS_gha_psh100"]] <- as.formula(object = "gha ~ gamlss::ps(x = h100, df = 2)")
kNuFormulas[["GAMLSS_gha_psh100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_gha_psh100"]] <- as.formula(object = "~1")
kDistFamilyToUse[["GAMLSS_gha_psh100"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_gha_psh100"]] <- c("gha", "h100")

## Setup for model "GAMLSS_gha_SI.h100".
kFormulas[["GAMLSS_gha_SI.h100"]] <- as.formula(object = "gha ~ SI.h100")
kSigmaFormulas[["GAMLSS_gha_SI.h100"]] <- as.formula(object = "gha ~ SI.h100")
kNuFormulas[["GAMLSS_gha_SI.h100"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_gha_SI.h100"]] <- as.formula(object = "~1")
kDistFamilyToUse[["GAMLSS_gha_SI.h100"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_gha_SI.h100"]] <- c("gha", "SI.h100")

## Setup for model "GAMLSS_gha_SI.h100_hnn.neu".
kFormulas[["GAMLSS_gha_SI.h100_hnn.neu"]] <- as.formula(object = "gha ~ SI.h100 * hnn.neu")
kSigmaFormulas[["GAMLSS_gha_SI.h100_hnn.neu"]] <- as.formula(object = "gha ~ SI.h100 * hnn.neu")
kNuFormulas[["GAMLSS_gha_SI.h100_hnn.neu"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_gha_SI.h100_hnn.neu"]] <- as.formula(object = "~1")
kDistFamilyToUse[["GAMLSS_gha_SI.h100_hnn.neu"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_gha_SI.h100_hnn.neu"]] <- c("gha", "SI.h100", "hnn.neu")

## Setup for model "GAMLSS_gha_h100.diff.EKL.I".
kFormulas[["GAMLSS_gha_h100.diff.EKL.I"]] <- as.formula(object = "gha ~ h100.diff.EKL.I")
kSigmaFormulas[["GAMLSS_gha_h100.diff.EKL.I"]] <- as.formula(object = "gha ~ h100.diff.EKL.I")
kNuFormulas[["GAMLSS_gha_h100.diff.EKL.I"]] <- as.formula(object = "~1")
kTauFormulas[["GAMLSS_gha_h100.diff.EKL.I"]] <- as.formula(object = "~1")
kDistFamilyToUse[["GAMLSS_gha_h100.diff.EKL.I"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_gha_h100.diff.EKL.I"]] <- c("gha", "h100.diff.EKL.I")

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
                                                                                                                           family = eval(expr = parse(text = kDistFamilyToUse[[cur.formula.name]])),
                                                                                                                           data = na.omit(object = subset(x = input.data, select = kColumnsToSelect[[cur.formula.name]])),
                                                                                                                           method = RS(1000)))
                }
            }
        }
    }
}

############
## Sterba ##
############
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

######################
## Reineke improved ##
######################
## Setup for model "Reineke_improved_quadratic".
## Source of model formula: Schütz (2008), eq. (1); Zeide (1995), eq. (2)
## Source of possible start values:
kFormulas[["Reineke_improved_quadratic"]] <- as.formula(object = "ln.nha ~ a + b * ln.dg + c * (ln.dg)^2")
kStartValsVecs[["Reineke_improved_quadratic"]] <- c("a" = 1,
                                                    "b" = 1,
                                                    "c" = 1)
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
                if (grepl(pattern = "Reineke_improved_",
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
}

########
## LM ##
########
## Select formulas to use.
## kFormulasToUse <- c(kFormulasToUse, "LM_ln.nha_ln.dg")
## kFormulasToUse <- c(kFormulasToUse, "LM_ln.nha_ln.dg_fixed_slope")
kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg")
## kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg_fixed_slope")
kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg_h100")
kFormulasToUse <- c(kFormulasToUse, "LM_log.nha_log.dg_h100_ni")
## Setup for model "LM_ln.nha_ln.dg".
kFormulas[["LM_ln.nha_ln.dg"]] <- as.formula(object = "ln.nha ~ ln.dg")
## Setup for model "LM_ln.nha_ln.dg_fixed_slope".
kFormulas[["LM_ln.nha_ln.dg_fixed_slope"]] <- as.formula(object = "ln.nha - -1.605 * ln.dg ~ 1")
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
