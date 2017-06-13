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
kPrintSumries <- TRUE
kPrintSumries <- FALSE
kFunctionsToUse <- c("mgcv..gam")
## kFunctionsToUse <- c("stats..nls")
## kFunctionsToUse <- c("nls2..nls2")
## kFunctionsToUse <- c("gamlss..gamlss")
kFunctionsToUse <- c("mgcv..gam", "nls2..nls2", "gamlss..gamlss")
## kFunctionsToUse <- c("minpack.lm..nlsLM")
## kFunctionsToUse <- c("stats..nls", "minpack.lm..nlsLM")
## kFunctionsToUse <- c("stats..nls", "nls2..nls2")
## kFunctionsToUse <- c("stats..nls", "nls2..nls2", "minpack.lm..nlsLM")
## kFormulasToUse <- c("Sterba_dgGmax")
## kFormulasToUse <- c("Sterba_NGmax")
kFormulasToUse <- c("Sterba_Gmax")
## kFormulasToUse <- c("Sterba_dgGmax", "Sterba_NGmax", "Sterba_Gmax")
## kFormulasToUse <- c("GAM_gha_sh100", "Sterba_dgGmax")
kFormulasToUse <- c("GAM_gha_sh100")
kFormulasToUse <- c("GAMLSS_gha_SI.h100_ghaa.cum_h100.EKL.I", "GAMLSS_ksha_SI.h100_ghaa.cum_h100.EKL.I", "GAMLSS_gha_h100")

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
        if (any(grepl(pattern = cur.formula.name,
                      x = kFormulasToUse))) {
            models[["mgcv..gam"]][[cur.formula.name]] <- mgcv::gam(formula = kFormulas[[cur.formula.name]],
                                                                   data = bart.clean)
        }}}

#############
## GAMLSSs ##
#############
## Setup for model "GAMLSS_gha_SI.h100_ghaa.cum_h100.EKL.I".
kFormulas[["GAMLSS_gha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "gha ~ gamlss::cs(SI.h100) + gamlss::cs(ghaa.cum) + gamlss::cs(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_gha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "gha ~ gamlss::cs(SI.h100) + gamlss::cs(ghaa.cum) + gamlss::cs(h100.EKL.I)")
kNuFormulas[["GAMLSS_gha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "gha ~ gamlss::cs(SI.h100) + gamlss::cs(ghaa.cum) + gamlss::cs(h100.EKL.I)")
kTauFormulas[["GAMLSS_gha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "~1")
## Setup for model "GAMLSS_ksha_SI.h100_ghaa.cum_h100.EKL.I".
kFormulas[["GAMLSS_ksha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "ksha ~ gamlss::cs(SI.h100) + gamlss::cs(ghaa.cum) + gamlss::cs(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_ksha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "ksha ~ gamlss::cs(SI.h100) + gamlss::cs(ghaa.cum) + gamlss::cs(h100.EKL.I)")
kNuFormulas[["GAMLSS_ksha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "ksha ~ gamlss::cs(SI.h100) + gamlss::cs(ghaa.cum) + gamlss::cs(h100.EKL.I)")
kTauFormulas[["GAMLSS_ksha_SI.h100_ghaa.cum_h100.EKL.I"]] <- as.formula(object = "~1")
## Setup for model "GAMLSS_gha_h100".
kFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "gha ~ gamlss::cs(h100)")
kSigmaFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "gha ~ gamlss::cs(h100)")
kNuFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "gha ~ gamlss::cs(h100)")
kTauFormulas[["GAMLSS_gha_h100"]] <- as.formula(object = "~1")
## Evaluate and store models.
kFunction <- "gamlss..gamlss"
if (any(grepl(pattern = kFunction,
              x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        if (any(grepl(pattern = cur.formula.name,
                      x = kFormulasToUse))) {
            models[["gamlss..gamlss"]][[cur.formula.name]] <- gamlss::gamlss(formula = kFormulas[[cur.formula.name]],
                                                                             sigma.formula = kSigmaFormulas[[cur.formula.name]],
                                                                             nu.formula = kNuFormulas[[cur.formula.name]],
                                                                             tau.formula = kTauFormulas[[cur.formula.name]],
                                                                             family = gamlss.dist::BCCG(),
                                                                             data = na.omit(bart.clean))
        }}}

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
## Evaluate and store models fitted with "stats::nls".
kFunction <- "stats..nls"
if (any(grepl(pattern = kFunction,
              x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        if (any(grepl(pattern = cur.formula.name,
                      x = kFormulasToUse))) {
            if (grepl(pattern = "Sterba",
                      x = cur.formula.name,
                      fixed = TRUE)) {
                try(expr = 
                        models[["stats..nls"]][[cur.formula.name]] <- stats::nls(formula = kFormulas[[cur.formula.name]],
                                                                                 data = bart.clean,
                                                                                 start = kStartValsVecs[[cur.formula.name]])
                    )
            }
        }}}
## Evaluate and store models fitted with "nls2::nls2"
kFunction <- "nls2..nls2"
if (any(grepl(pattern = kFunction,
              x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        if (any(grepl(pattern = cur.formula.name,
                      x = kFormulasToUse))) {
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
        }}}
## Evaluate and store models fitted with "minpack.lm::nlsLM"
kFunction <- "minpack.lm..nlsLM"
if (any(grepl(pattern = kFunction,
              x = kFunctionsToUse))) {
    for (cur.formula.name in names(x = kFormulas)) {
        if (any(grepl(pattern = cur.formula.name,
                      x = kFormulasToUse))) {
            if (grepl(pattern = "Sterba", x = cur.formula.name, fixed = TRUE)) {
                try(expr = 
                        models[["minpack.lm..nlsLM"]][[cur.formula.name]] <- minpack.lm::nlsLM(formula = kFormulas[[cur.formula.name]],
                                                                                               data = bart.clean,
                                                                                               start = kStartValsVecs[[cur.formula.name]])
                    )
            }
        }}}

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
