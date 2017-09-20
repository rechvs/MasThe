##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kFileVersion <- "4.5"
kFileName <- paste0(kDataDir, "gmax_merged_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)
models <- vector(mode = "list")
models[["mgcv..gam"]] <- vector(mode = "list")
models[["gamlss..gamlss"]] <- vector(mode = "list")
models[["stats..nls"]] <- vector(mode = "list")
models[["stats..glm"]] <- vector(mode = "list")
models[["nls2..nls2"]] <- vector(mode = "list")
models[["minpack.lm..nlsLM"]] <- vector(mode = "list")
models[["scam..scam"]] <- vector(mode = "list")
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
kFunctionsToUse <- c(kFunctionsToUse, "stats..glm")
kFunctionsToUse <- c(kFunctionsToUse, "scam..scam")
kFormulasToUse <- NULL
kColumnsToSelect <- vector(mode = "list")  ## Required for "gamlss::gamlss(...)" to avoid omission of more rows than necessary.
## Create a vector containing the names of all appropriate input data sources.
names.input.data.sources <- ls()[grepl(pattern = "bart.((beech)|(spruce)).clean.1.8", x = ls(), fixed = FALSE)]
## Plotting preamble.
kPdfWidth <- 30
kPdfHeight <- kPdfWidth * 0.625
kPdfPointSize <- 19
kPdfFamily <- "Times"
kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
## Define segmented functions.
rhs <- function (x, c) {
    return(ifelse(test = x > c,
                  yes = x-c,
                  no = 0))
}
lhs <- function (x, c) {
    return(ifelse(test = x <= c,
                  yes = c-x,
                  no = 0))
}
objects.at.start <- sort(x = c(ls(), "objects.at.start"))  ## Required for cleaning up workspace after each block.

##########
## SCAM ##
##########
## Preamble.
## kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_h100")  ## Model does not work due to "subscript out of bounds" error.
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_mpih100")
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_mpiSI.h100.diff.EKL.I_mpih100.EKL.I")  ## Beech: better fit than unrestrained "SI.h100.diff.EKL.I", since it does not have a nonsensical effect. Spruce: no improvement compared to unrestrained "SI.h100.diff.EKL.I".
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_s1SI.h100.diff.EKL.I_mpih100.EKL.I")  ## Beech: best model fit with respect to different values of "k" (but still includes a nonsensical effect of "SI.h100.diff.EKL.I").
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_s1SI.h100.diff.EKL.I_s1h100.EKL.I")  ## Beech: nonsensical effect of "h100.EKL.I" which is not improved when chaning "k". Spruce: best model fit with respect to different values of "k".
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_s4SI.h100.diff.EKL.I_mpih100.EKL.I")  ## Spruce: best model fit with respect to different values of "k".
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_sefuminus3SI.h100.diff.EKL.I_mpih100.EKL.I")
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_sefuminus7SI.h100.diff.EKL.I_mpih100.EKL.I")
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_sh100")
kFormulasToUse <- c(kFormulasToUse, "SCAM_gha_SI.h100.diff.EKL.I_mpih100.EKL.I")

## Setup for model "SCAM_gha_h100".
kFormulas[["SCAM_gha_h100"]] <- as.formula(object = "gha ~ h100")

## Setup for model "SCAM_gha_mpih100".
kFormulas[["SCAM_gha_mpih100"]] <- as.formula(object = "gha ~ s(h100, k = 15, bs = \"mpi\")")

## Setup for model "SCAM_gha_mpiSI.h100.diff.EKL.I_mpih100.EKL.I".
kFormulas[["SCAM_gha_mpiSI.h100.diff.EKL.I_mpih100.EKL.I"]] <- as.formula(object = "gha ~ s(SI.h100.diff.EKL.I, bs = \"mpi\") + s(h100.EKL.I, bs = \"mpi\")")

## Setup for model "SCAM_gha_s1SI.h100.diff.EKL.I_mpih100.EKL.I".
kFormulas[["SCAM_gha_s1SI.h100.diff.EKL.I_mpih100.EKL.I"]] <- as.formula(object = "gha ~ s(SI.h100.diff.EKL.I, k = 1) + s(h100.EKL.I, bs = \"mpi\")")

## Setup for model "SCAM_gha_s1SI.h100.diff.EKL.I_s1h100.EKL.I".
kFormulas[["SCAM_gha_s1SI.h100.diff.EKL.I_s1h100.EKL.I"]] <- as.formula(object = "gha ~ s(SI.h100.diff.EKL.I, k = 1) + s(h100.EKL.I, k = 1)")

## Setup for model "SCAM_gha_s4SI.h100.diff.EKL.I_mpih100.EKL.I".
kFormulas[["SCAM_gha_s4SI.h100.diff.EKL.I_mpih100.EKL.I"]] <- as.formula(object = "gha ~ s(SI.h100.diff.EKL.I, k = 4) + s(h100.EKL.I, bs = \"mpi\")")

## Setup for model "SCAM_gha_sefuminus3SI.h100.diff.EKL.I_mpih100.EKL.I".
kFormulas[["SCAM_gha_sefuminus3SI.h100.diff.EKL.I_mpih100.EKL.I"]] <- as.formula(object = "gha ~ lhs(x = SI.h100.diff.EKL.I, c = -3) + rhs(x = SI.h100.diff.EKL.I, c = -3) + s(h100.EKL.I, bs = \"mpi\")")

## Setup for model "SCAM_gha_sefuminus7SI.h100.diff.EKL.I_mpih100.EKL.I".
kFormulas[["SCAM_gha_sefuminus7SI.h100.diff.EKL.I_mpih100.EKL.I"]] <- as.formula(object = "gha ~ lhs(x = SI.h100.diff.EKL.I, c = -7) + rhs(x = SI.h100.diff.EKL.I, c = -7) + s(h100.EKL.I, bs = \"mpi\")")

## Setup for model "SCAM_gha_sh100".
kFormulas[["SCAM_gha_sh100"]] <- as.formula(object = "gha ~ s(h100, k = 15)")

## Setup for model "SCAM_gha_SI.h100.diff.EKL.I_mpih100.EKL.I".
kFormulas[["SCAM_gha_SI.h100.diff.EKL.I_mpih100.EKL.I"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + s(h100.EKL.I, bs = \"mpi\")")

## Initiate "for" loop (for looping over all names of input data sources).
for (cur.input.data.source.name in names.input.data.sources) {
    input.data <- eval(expr = parse(text = cur.input.data.source.name))
    ## Evaluate, store and plot models.
    kFunction <- "scam..scam"
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        for (cur.formula.name in names(x = kFormulas)) {
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                if (grepl(pattern = "SCAM_", x = cur.formula.name, fixed = TRUE)) {
                    ## If a distribution family was specified, use that for model fitting. Otherwise, use "Gamma(link = "log")".
                    if (cur.formula.name %in% names(x = kDistFamilies)) {
                        cur.dist <- eval(expr = parse(text = kDistFamilies[[cur.formula.name]]))
                    } else {
                        cur.dist <- Gamma(link = "log")
                    }
                    ## Try to fit model.
                    cur.model <- try(expr = scam::scam(formula = kFormulas[[cur.formula.name]],
                                                       data = input.data,
                                                       family = cur.dist))
                    ## Continue only if model fit was successful.
                    if (!inherits(x = cur.model, what = "try-error")) {
                        ## Store model in object "models".
                        models[["scam..scam"]][[cur.input.data.source.name]][[cur.formula.name]] <- cur.model
                        ## Plot model ##
                        ################
                        ## Extract current model formula.
                        cur.formula <- cur.model[["formula"]]
                        ## Store current model formula as a string and remove any whitespace from it.
                        cur.formula.string <- gsub(pattern = " ",
                                                   replacement = "",
                                                   x = Reduce(f = paste,
                                                              x = deparse(expr = cur.formula)))
                        ## Turn off graphics device.
                        graphics.off()
                        ## If nonexistent, create subdirectory in which to store graphics.
                        graphics.subdir <- paste0("Graphics/Models/SCAM/", cur.input.data.source.name, "/")
                        system2(command = "mkdir",
                                args = paste0("-p ", graphics.subdir))
                        ## Create file name.
                        file.name <-paste0(graphics.subdir,
                                           cur.formula.name,
                                           ".pdf")
                        ## Start graphics device driver for producing PDF graphics.
                        pdf(file = file.name,
                            width = kPdfWidth,
                            height = kPdfHeight,
                            pointsize = kPdfPointSize,
                            family = kPdfFamily)
                        ## Set plot layout, depending on number of independent variables.
                        nr.independent.vars <- length(x = all.vars(expr = cur.formula)[-1])
                        if (nr.independent.vars == 1) {
                            mfrow <- c(1, 1)
                        }
                        if (nr.independent.vars == 2) {
                            mfrow <- c(2, 1)
                        }
                        if (nr.independent.vars == 3 || nr.independent.vars == 4) {
                            mfrow <- c(2, 2)
                        }
                        par(mfrow = mfrow)
                        ## Set plot margins.
                        par(mar = kPlotMargins)
                        ## Plot model term effects.
                        scam::plot.scam(x = cur.model,
                                        all.terms = TRUE,
                                        main = paste0(cur.formula.string,
                                                      ", ", cur.input.data.source.name))
                        ## Plot model diagnostics (which currently requires to define and use a custom version of "scam::scam.check" in order to be able to set argument "pch" to a user defined value).
                        my.scam.check.string <- deparse(expr = scam::scam.check)
                        my.scam.check.string[1] <- paste0("my.scam.check <- ", my.scam.check.string[1])
                        my.scam.check.string <- paste0(my.scam.check.string, collapse = "\n")
                        my.scam.check.string <- gsub(pattern = ", pch = \".\"", fixed = TRUE, replacement = "", x = my.scam.check.string)
                        eval(expr = parse(text = my.scam.check.string))
                        my.scam.check(b = cur.model,
                                      pch = 19)
                        ## Turn off graphics device.
                        graphics.off()
                    }}}}}}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

#########
## GAM ##
#########
## Preamble.
kFormulasToUse <- c(kFormulasToUse, "GAM_gha_h100")
kFormulasToUse <- c(kFormulasToUse, "GAM_gha_s1SI.h100.diff.EKL.I_s1h100.EKL.I")
kFormulasToUse <- c(kFormulasToUse, "GAM_gha_s1h100")

## Setup for model "GAM_gha_h100".
kFormulas[["GAM_gha_h100"]] <- as.formula(object = "gha ~ h100")

## Setup for model "GAM_gha_s1SI.h100.diff.EKL.I_s1h100.EKL.I".
kFormulas[["GAM_gha_s1SI.h100.diff.EKL.I_s1h100.EKL.I"]] <- as.formula(object = "gha ~ s(SI.h100.diff.EKL.I, k = 1) + s(h100.EKL.I, k = 1)")

## Setup for model "GAM_gha_s1h100".
kFormulas[["GAM_gha_s1h100"]] <- as.formula(object = "gha ~ s(h100, k = 1)")

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
                    ## If a distribution family was specified, used that for model fitting. Otherwise, use "Gamma(link = "log")".
                    if (cur.formula.name %in% names(x = kDistFamilies)) {
                        cur.dist <- eval(expr = parse(text = kDistFamilies[[cur.formula.name]]))
                    } else {
                        cur.dist <- Gamma(link = "log")
                    }
                    ## Try to fit model.
                    cur.model <- try(expr = mgcv::gam(formula = kFormulas[[cur.formula.name]],
                                                      data = input.data,
                                                      family = cur.dist,
                                                      na.action = na.omit))
                    ## Continue only if model fit was successful.
                    if (!inherits(x = cur.model, what = "try-error")) {
                        ## Store model in object "models".
                        models[["mgcv..gam"]][[cur.input.data.source.name]][[cur.formula.name]] <- cur.model
                        ## Plot model ##
                        ################
                        ## Extract current model formula.
                        cur.formula <- cur.model[["formula"]]
                        ## Store current model formula as a string and remove any whitespace from it.
                        cur.formula.string <- gsub(pattern = " ",
                                                   replacement = "",
                                                   x = Reduce(f = paste,
                                                              x = deparse(expr = cur.formula)))
                        ## Turn off graphics device.
                        graphics.off()
                        ## If nonexistent, create subdirectory in which to store graphics.
                        graphics.subdir <- paste0("Graphics/Models/GAM/", cur.input.data.source.name, "/")
                        system2(command = "mkdir",
                                args = paste0("-p ", graphics.subdir))
                        ## Create file name.
                        file.name <-paste0(graphics.subdir,
                                           cur.formula.name,
                                           ".pdf")
                        ## Start graphics device driver for producing PDF graphics.
                        pdf(file = file.name,
                            width = kPdfWidth,
                            height = kPdfHeight,
                            pointsize = kPdfPointSize,
                            family = kPdfFamily)
                        ## Set plot layout, depending on number of independent variables.
                        nr.independent.vars <- length(x = all.vars(expr = cur.formula)[-1])
                        if (nr.independent.vars == 1) {
                            mfrow <- c(1, 1)
                        }
                        if (nr.independent.vars == 2) {
                            mfrow <- c(2, 1)
                        }
                        if (nr.independent.vars == 3 || nr.independent.vars == 4) {
                            mfrow <- c(2, 2)
                        }
                        par(mfrow = mfrow)
                        ## Set plot margins.
                        par(mar = kPlotMargins)
                        ## Plot model term effects.
                        mgcv::plot.gam(x = cur.model,
                                       all.terms = TRUE,
                                       main = paste0(cur.formula.string,
                                                     ", ",
                                                     cur.input.data.source.name))
                        ## Plot model diagnostics.
                        mgcv::gam.check(b = cur.model,
                                        type = "response",
                                        pch = 19)
                        ## Turn off graphics device.
                        graphics.off()
                    }}}}}}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

############
## GAMLSS ##
############
## Preamble.
library("gamlss")
kUseStepGAIC <- vector(mode = "list")
kSigmaFormulas <- vector(mode = "list")
kNuFormulas <- vector(mode = "list")
kTauFormulas <- vector(mode = "list")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I" and "hnn.neu" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" and "hnn.neu" on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni")  ## nonsensical effect of “NORTH.UTM” on mu for beech; nonsensical effect of “NORTH.UTM” on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "NORTH.UTM" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" and "hnn.neu" and “NORTH.UTM” on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "hnn.neu" on mu, nonensical effect of "h100.EKL.I" on sigma for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu, nonensical effect of "h100.EKL.I" on sigma for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" and "hnn.neu" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" and "hnn.neu" on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" and "NORTH.UTM" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" and "hnn.neu" and "NORTH.UTM" on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" and "NORTH.UTM" on mu, nonsensical effect of "h100.EKL.I" for beech; nonsensical effect of "SI.h100.diff.EKL.I" and "hnn.neu" and "NORTH.UTM" on mu, nonsensical effect of "h100.EKL.I" on sigma for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" and "hnn.neu" on mu, nonsensical effect of "h100.EKL.I" on sigma for beech; nonsensical effect of "SI.h100.diff.EKL.I" and "hnn.neu" on mu, nonsensical effect of "h100.EKL.I" on sigma for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni")  ## nonsensical effect of "hnn.neu" on mu for beech; nonsensical effect of "hnn.neu" on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni")  ## nonsensical effect of "hnn.neu" on mu for beech; nonsensical effect of "hnn.neu" on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "hnn.neu" and "NORTH.UTM" on mu, nonsensical effect of "h100.EKL.I" on sigma for beech; nonsensical effect of "hnn.neu" and "NORTH.UTM" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "hnn.neu" on mu, nonsensical effect "h100.EKL.I" on sigma for beech; nonsensical effect of "hnn.neu" on mu, nonsensical effect "h100.EKL.I" on sigma for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni")  ## nonsensical effect of "h100.EKL.I" and "hnn.neu" on mu for beech; nonsensical effect of "hnn.neu" on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni")  ## nonsensical effect of "h100.EKL.I" and "hnn.neu" and "NORTH.UTM" on mu for spruce; nonsensical effect of "hnn.neu" and "NORTH.UTM" on mu for spruce; high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "h100.EKL.I" and "hnn.neu" on mu, nonsensical effect of "h100.EKL.I" on sigma for beech; nonsensical effect of "h100.EKL.I" and "hnn.neu" and "NORTH.UTM" on mu, nonsensical effect of "h100.EKL.I" on sigma for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni")  ## nonsensical effect of "h100.EKL.I" and "hnn.neu" on mu for beech; nonsensical effect of "h100.EKL.I" and "hnn.neu" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni")  ## high AIC
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; high AIC, small effect of "hnn.neu" on mu for beech; small effect of "SI.h100.diff.EKL.I" and "hnn.neu" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni")  ## nonsensical effect of "h100.EKL.I" and "hnn.neu" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I", small effect of "hnn.neu" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I", small effect of "hnn.neu" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I", small effect of "hnn.neu" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni")  ## nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu, high AIC for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I", small effect of "hnn.neu" on mu, high AIC for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; small effect of "hnn.neu" on mu, high AIC for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species, high AIC; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I", small effect of "hnn.neu" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" and "hnn.neu" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni")  ## nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu, high AIC for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu, high AIC for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps1SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech; small effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## high AIC, contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## contradictory effect of "h100.EKL.I" on sigma between species;  nonsensical effect of "SI.h100.diff.EKL.I" and "h100.EKL.I" on mu for beech; nonsensical effect of "SI.h100.diff.EKL.I" on mu for spruce
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni")  ## further increase in df has no effect, nonsensical effect of "SI.h100.diff.EKL.I" on mu, contradictory effect of "h100.EKL.I" on sigma between species; nonsensical effect of "h100.EKL.I" on mu for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for beech
kFormulasToUse <- c(kFormulasToUse, "GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni")  ## high AIC for spruce
rhs <- function (x, c) {
    return(ifelse(test = x > c,
                  yes = x-c,
                  no = 0))
}
lhs <- function (x, c) {
    return(ifelse(test = x <= c,
                  yes = c-x,
                  no = 0))
}

## Setup for model "GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ lhs(x = SI.h100.diff.EKL.I, c = -3) + rhs(x = SI.h100.diff.EKL.I, c = -3) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_sefuminus3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ lhs(x = SI.h100.diff.EKL.I, c = -7) + rhs(x = SI.h100.diff.EKL.I, c = -7) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_sefuminus7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 1) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm1SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 2) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm2SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 3) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm3SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 4) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm4SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 5) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm5SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 6) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm6SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 7) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm7SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 8) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm8SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 9) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm9SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 10) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm10SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 11) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm11SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 12) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm12SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 13) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm13SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 14) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm14SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 15) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm15SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 16) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm16SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 17) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm17SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 18) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm18SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, df = 19) + pbm(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_pbm19SI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 2) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps2SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 3) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps3SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 4) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps4SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 5) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps5SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")


## Setup for model "GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 10) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps10SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 11) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps11SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 12) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps12SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 13) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps13SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 14) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps14SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 15) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps15SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 16) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps16SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 17) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps17SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 18) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps18SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 19) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps19SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I, df = 20) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_mu_ps20SI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + pbm(h100.EKL.I, mono = \"up\") + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + ps(h100.EKL.I) + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + pbm(h100.EKL.I, mono = \"up\")")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + pbm(h100.EKL.I, mono = \"up\") + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + ps(h100.EKL.I) + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + pbm(h100.EKL.I, mono = \"up\")")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ pbm(SI.h100.diff.EKL.I, mono = \"up\") + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_pbmSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\") + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I) + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\")")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\") + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I) + hnn.neu")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_hnn.neu_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\")")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ h100.EKL.I")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_sigma_h100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu) + ps(NORTH.UTM)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu) + ps(NORTH.UTM)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I) + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I) + ps(hnn.neu) + ps(NORTH.UTM)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I) + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(SI.h100.diff.EKL.I) + ps(h100.EKL.I) + ps(hnn.neu) + ps(NORTH.UTM)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_psSI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_psNORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu) + NORTH.UTM")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + pbm(h100.EKL.I, mono = \"up\") + ps(hnn.neu) + NORTH.UTM")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_pbmh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + ps(h100.EKL.I) + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + ps(h100.EKL.I) + ps(hnn.neu) + NORTH.UTM")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + ps(h100.EKL.I) + ps(hnn.neu)")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu")

## Setup for model "GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni".
kFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ SI.h100.diff.EKL.I + ps(h100.EKL.I) + ps(hnn.neu) + NORTH.UTM")
kSigmaFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ ps(h100.EKL.I)")
kNuFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kTauFormulas[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- as.formula(object = "gha ~ 1")
kDistFamilies[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- "gamlss.dist::BCCGo()"
kColumnsToSelect[["GAMLSS_BCCGo_mu_gha_SI.h100.diff.EKL.I_psh100.EKL.I_pshnn.neu_NORTH.UTM_sigma_gha_psh100.EKL.I_ni"]] <- c("gha", "SI.h100.diff.EKL.I", "h100.EKL.I", "hnn.neu", "NORTH.UTM")

## Check whether the function needed for this block is selected for execution.
kFunction <- "gamlss..gamlss"
if (any(grepl(pattern = kFunction,
              x = kFunctionsToUse))) {
    ## Loop over all appropriate input data names.
    for (cur.input.data.name in names.input.data.sources) {
        ## Extract current species name.
        cur.species.name <- strsplit(x = cur.input.data.name, split = ".", fixed = TRUE)[[1]][2]
        ## Get untampered version of input data.
        cur.input.data <- get(x = cur.input.data.name)
        ## Loop over all formulas.
        for (cur.formula.name in names(x = kFormulas)) {
            ## Check whether the current formula is selected for evaluation.
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                ## Check whether the current formula name contains the string "GAMLSS_", continue only if it does.
                if (grepl(pattern = "GAMLSS_", x = cur.formula.name, fixed = TRUE)) {
                    ## Check whether the current formula is suited for the current species.
                    if (grepl(pattern = cur.species.name, x = cur.formula.name, fixed = TRUE) || (!grepl(pattern = "(spruce)|(beech)", x = cur.formula.name))) {
                        ## Subset "cur.input.data" to the column names in "kColumnsToSelect[[cur.formula.name]]".
                        cur.input.data.col.subset <- cur.input.data[, kColumnsToSelect[[cur.formula.name]]]
                        ## Remove missing values from "cur.input.data.col.subset".
                        cur.input.data.col.subset.na.omitted <- na.omit(object = cur.input.data.col.subset)
                        ## Determine the distribution parameters for the current distribution.
                        cur.dist <- eval(expr = parse(text = kDistFamilies[[cur.formula.name]]))
                        cur.dist.parameters.names <- names(x = cur.dist[["parameters"]])
                        ## Start sinking output.
                        sink(file = "/dev/null")
                        ## If "kUseStepGAIC" contains a value for the current formula, use that to determine whether or not to use "gamlss::stepGAIC". Otherwise, do not use "gamlss::stepGAIC".
                        if (cur.formula.name %in% names(x = kUseStepGAIC)) {
                            use.step.gaic <- kUseStepGAIC[[cur.formula.name]]
                        } else {
                            use.step.gaic <- FALSE
                        }
                        ## Fit model, either directly or stepwise, depending on "use.step.gaic".
                        if (!use.step.gaic) {
                            ## Evaluate model.
                            try(expr =
                                    models[["gamlss..gamlss"]][[cur.input.data.name]][[cur.formula.name]] <- gamlss::gamlss(formula = kFormulas[[cur.formula.name]],
                                                                                                                            sigma.formula = kSigmaFormulas[[cur.formula.name]],
                                                                                                                            nu.formula = kNuFormulas[[cur.formula.name]],
                                                                                                                            tau.formula = kTauFormulas[[cur.formula.name]],
                                                                                                                            family = cur.dist,
                                                                                                                            data = cur.input.data.col.subset.na.omitted,
                                                                                                                            method = RS(1000)))
                        } else {
                            ## Loop over all distribution parameters.
                            for (cur.dist.parameter.name in cur.dist.parameters.names) {
                                ## Determine name of dependent variable.
                                cur.dependent.variable.name <- as.character(x = kFormulas[[cur.formula.name]])[2]
                                ## Create default maximal model formulas for all distribution parameters.
                                cur.mu.max.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                cur.sigma.max.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                cur.nu.max.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                cur.tau.max.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                ## If currently modelling parameter sigma, nu, or tau (i.e., after modelling mu), create a maximal model formula for parameter mu from previously selected model.
                                if (grepl(pattern = "sigma|nu|tau", x = cur.dist.parameter.name)) {
                                    prev.selected.model <- models[["gamlss..gamlss"]][[cur.input.data.name]][[cur.formula.name]]
                                    cur.mu.max.formula <- formula(x = prev.selected.model,
                                                                  what = "mu")
                                }
                                ## If currently modelling parameter nu or tau (i.e., after modelling sigma), create a maximal model formula for parameter sigma from previously selected model.
                                if (grepl(pattern = "nu|tau", x = cur.dist.parameter.name)) {
                                    prev.selected.model <- models[["gamlss..gamlss"]][[cur.input.data.name]][[cur.formula.name]]
                                    cur.sigma.max.formula <- formula(x = prev.selected.model,
                                                                     what = "sigma")
                                }
                                ## If currently modelling parameter tau (i.e., after modelling nu), create a maximal model formula for parameter nu from previously selected model.
                                if (grepl(pattern = "nu|tau", x = cur.dist.parameter.name)) {
                                    prev.selected.model <- models[["gamlss..gamlss"]][[cur.input.data.name]][[cur.formula.name]]
                                    cur.nu.max.formula <- formula(x = prev.selected.model,
                                                                  what = "nu")
                                }
                                ## Create maximal model formula for current distribution parameter.
                                assign(x = paste0("cur.", cur.dist.parameter.name, ".max.formula"),
                                       value = kFormulas[[cur.formula.name]])
                                ## Create model from maximal model formulas.
                                cur.max.model <- gamlss::gamlss(formula = cur.mu.max.formula,
                                                                sigma.formula = cur.sigma.max.formula,
                                                                nu.formula = cur.nu.max.formula,
                                                                tau.formula = cur.tau.max.formula,
                                                                family = eval(expr = parse(text = kDistFamilies[[cur.formula.name]])),
                                                                data = cur.input.data.col.subset.na.omitted,
                                                                method = RS(1000))
                                ## Create default minimal model formulas for all distribution parameters.
                                cur.mu.min.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                cur.sigma.min.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                cur.nu.min.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                cur.tau.min.formula <- as.formula(object = paste0(cur.dependent.variable.name, " ~ 1"))
                                ## Create minimal model formula for current distribution parameter.
                                assign(x = paste0("cur.", cur.dist.parameter.name, ".min.formula"),
                                       value = as.formula(object = paste0(cur.dependent.variable.name, "~ 1")))
                                ## Create model from minimal model formulas.
                                cur.min.model <- gamlss::gamlss(formula = cur.mu.min.formula,
                                                                sigma.formula = cur.sigma.min.formula,
                                                                nu.formula = cur.nu.min.formula,
                                                                tau.formula = cur.tau.min.formula,
                                                                family = eval(expr = parse(text = kDistFamilies[[cur.formula.name]])),
                                                                data = cur.input.data.col.subset.na.omitted,
                                                                method = RS(1000))
                                ## Store model selected via "stepGAIC" in "cur.selected.model".
                                cur.selected.model <- gamlss::stepGAIC(object = cur.max.model,
                                                                       scope = list(upper = cur.max.model,
                                                                                    lower = cur.min.model),
                                                                       direction = "both",
                                                                       trace = TRUE,
                                                                       keep = NULL,
                                                                       steps = 1000,
                                                                       ## k = 2,  ## Base model selection on AIC.
                                                                       k = log(x = nrow(x = cur.input.data.col.subset.na.omitted), base = 10),  ## Base model selection on BIC.
                                                                       what = cur.dist.parameter.name,
                                                                       parallel = "no")
                                ## Store "cur.selected.model" in "models".
                                models[["gamlss..gamlss"]][[cur.input.data.name]][[cur.formula.name]] <- cur.selected.model
                                ## Remove all objects from workspace that could erroneously be reused in following cycle.
                                rm(list = c("cur.max.model", "cur.min.model", "cur.selected.model"))
                            }}
                        ## Stop sinking output.
                        sink(file = NULL)
                    }}}}}}
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
kColumnsToSelect[["Sterba_dgGmax"]] <- c("dg", "h100", "nha")
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
kColumnsToSelect[["Sterba_NGmax"]] <- c("nha", "dg")
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
kColumnsToSelect[["Sterba_Gmax"]] <- c("gha", "h100")
## Loop over all input data source names.
for (cur.input.data.source.name in names.input.data.sources) {
    ## Get original input data.
    input.data <- get(x = cur.input.data.source.name)
    ## Set modelling function needed for this section.
    kFunction <- "stats..nls"
    ## Proceed only if the needed modelling function is meant to be executed.
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        ## Loop over all model formulas.
        for (cur.formula.name in names(x = kFormulas)) {
            ## Proceed only if the current model formula is meant to be evaluated.
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                ## Proceed only if the current model formula belongs to this block.
                if (grepl(pattern = "Sterba_",
                          x = cur.formula.name,
                          fixed = TRUE)) {
                    ## Restrict input data to the columns needed for the current model formula.
                    input.data <- input.data[, kColumnsToSelect[[cur.formula.name]]]
                    ## Evaluate and store models fitted with "stats::nls".
                    try(expr =
                            models[["stats..nls"]][[cur.input.data.source.name]][[cur.formula.name]] <- stats::nls(formula = kFormulas[[cur.formula.name]],
                                                                                                                   data = input.data,
                                                                                                                   start = kStartValsVecs[[cur.formula.name]],
                                                                                                                   na.action = na.omit,
                                                                                                                   control = stats::nls.control(maxiter = 1000)))
                }}}}
    ## Set modelling function needed for this section.
    kFunction <- "nls2..nls2"
    ## Proceed only if the needed modelling function is meant to be executed.
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        ## Loop over all model formulas.
        for (cur.formula.name in names(x = kFormulas)) {
            ## Proceed only if the current model formula is meant to be evaluated.
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                ## Proceed only if the current model formula belongs to this block.
                if (grepl(pattern = "Sterba_",
                          x = cur.formula.name,
                          fixed = TRUE)) {
                    ## Restrict input data to the columns needed for the current model formula.
                    input.data <- input.data[, kColumnsToSelect[[cur.formula.name]]]
                    ## Evaluate and store models fitted with "nls2::nls2".
                    try(expr =
                            models[["nls2..nls2"]][[cur.input.data.source.name]][[cur.formula.name]] <- nls2::nls2(formula = kFormulas[[cur.formula.name]],
                                                                                                                   data = input.data,
                                                                                                                   start = kStartValsGrids[[cur.formula.name]],
                                                                                                                   algorithm = "random-search",
                                                                                                                   control = (stats::nls.control(maxiter = 100,
                                                                                                                                                 minFactor = 10 ^ -10)),
                                                                                                                   na.action = na.omit))
                }}}}
    ## Set modelling function needed for this section.
    kFunction <- "minpack.lm..nlsLM"
    ## Proceed only if the needed modelling function is meant to be executed.
    if (any(grepl(pattern = kFunction,
                  x = kFunctionsToUse))) {
        ## Loop over all model formulas.
        for (cur.formula.name in names(x = kFormulas)) {
            ## Proceed only if the current model formula is meant to be evaluated.
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                ## Proceed only if the current model formula belongs to this block.
                if (grepl(pattern = "Sterba_",
                          x = cur.formula.name,
                          fixed = TRUE)) {
                    ## Restrict input data to the columns needed for the current model formula.
                    input.data <- input.data[, kColumnsToSelect[[cur.formula.name]]]
                    ## Evaluate and store models fitted with "minpack.lm::nlsLM"
                    try(expr =
                            models[["minpack.lm..nlsLM"]][[cur.input.data.source.name]][[cur.formula.name]] <- minpack.lm::nlsLM(formula = kFormulas[[cur.formula.name]],
                                                                                                                                 data = input.data,
                                                                                                                                 start = kStartValsVecs[[cur.formula.name]],
                                                                                                                                 na.action = na.omit))
                }}}}}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

#########
## GLM ##
#########
## Preamble.
## kFormulasToUse <- c(kFormulasToUse, "GLM_Gamma_log.nha_log.dg") ## Model has higher AIC than its gaussian counterpart for both data sets.
## kFormulasToUse <- c(kFormulasToUse, "GLM_gaussian_log.nha_ALL_ni")
## kFormulasToUse <- c(kFormulasToUse, "GLM_gaussian_log.nha_log.dg")
## kFormulasToUse <- c(kFormulasToUse, "GLM_gaussian_log.nha_log.dg_h100")
## kFormulasToUse <- c(kFormulasToUse, "GLM_gaussian_log.nha_log.dg_h100_ni")
## kFormulasToUse <- c(kFormulasToUse, "GLM_gaussian_log.nha_log.dg_hnn.neu")
## kFormulasToUse <- c(kFormulasToUse, "GLM_gaussian_log.nha_minus1.605_log.dg")

## Setup for model "GLM_Gamma_log.nha_log.dg".
kFormulas[["GLM_Gamma_log.nha_log.dg"]] <- as.formula(object = "log.nha ~ log.dg")
kDistFamilies[["GLM_Gamma_log.nha_log.dg"]] <- "Gamma(link = \"log\")"

## Setup for model "GLM_gaussian_log.nha_ALL_ni".
kFormulas[["GLM_gaussian_log.nha_ALL_ni"]] <- as.formula(object = "log.nha ~ h100 + h100.class + h100.diff.EKL.I + h100.EKL.I + hnn.neu + log.dg + SI.h100 + SI.h100.class + WGS_EAST + WGS_NORTH")
kDistFamilies[["GLM_gaussian_log.nha_ALL_ni"]] <- "gaussian"

## Setup for model "GLM_gaussian_log.nha_log.dg".
kFormulas[["GLM_gaussian_log.nha_log.dg"]] <- as.formula(object = "log.nha ~ log.dg")
kDistFamilies[["GLM_gaussian_log.nha_log.dg"]] <- "gaussian"

## Setup for model "GLM_gaussian_log.nha_log.dg_h100".
kFormulas[["GLM_gaussian_log.nha_log.dg_h100"]] <- as.formula(object = "log.nha ~ log.dg * h100")
kDistFamilies[["GLM_gaussian_log.nha_log.dg_h100"]] <- "gaussian"

## Setup for model "GLM_gaussian_log.nha_log.dg_h100_ni".
kFormulas[["GLM_gaussian_log.nha_log.dg_h100_ni"]] <- as.formula(object = "log.nha ~ log.dg + h100")
kDistFamilies[["GLM_gaussian_log.nha_log.dg_h100_ni"]] <- "gaussian"

## Setup for model "GLM_gaussian_log.nha_log.dg_hnn.neu".
kFormulas[["GLM_gaussian_log.nha_log.dg_hnn.neu"]] <- as.formula(object = "log.nha ~ log.dg * hnn.neu")
kDistFamilies[["GLM_gaussian_log.nha_log.dg_hnn.neu"]] <- "gaussian"

## Setup for model "GLM_gaussian_log.nha_minus1.605_log.dg".
kFormulas[["GLM_gaussian_log.nha_minus1.605_log.dg"]] <- as.formula(object = "log.nha - -1.605 * log.dg ~ 1")
kDistFamilies[["GLM_gaussian_log.nha_minus1.605_log.dg"]] <- "gaussian"

## Check whether the function needed for this block is selected for execution.
kFunction <- "stats..glm"
if (any(grepl(pattern = kFunction,
              x = kFunctionsToUse))) {
    ## Loop over all appropriate input data names.
    for (cur.input.data.name in names.input.data.sources) {
        ## Get untampered version of input data.
        cur.input.data <- get(x = cur.input.data.name)
        ## Loop over all formulas.
        for (cur.formula.name in names(x = kFormulas)) {
            ## Check whether the current formula is selected for evaluation.
            if (any(grepl(pattern = paste0("^", cur.formula.name, "$"),
                          x = kFormulasToUse))) {
                ## Check whether the current formula name contains the string "GLM_", continue only if it does.
                if (grepl(pattern = "GLM_", x = cur.formula.name, fixed = TRUE)) {
                    ## Extract names of columns needed for current formula.
                    cur.formula <- kFormulas[[cur.formula.name]]
                    cur.formula.col.names <- strsplit(x = paste0(as.character(x = cur.formula)[2],
                                                                 as.character(x = cur.formula)[1],
                                                                 as.character(x = cur.formula)[3],
                                                                 collapse=""),
                                                      split = "[*+~-]")[[1]]
                    cur.formula.col.names <- cur.formula.col.names[grepl(pattern = "[a-zA-Z]", x = cur.formula.col.names)]
                    cur.formula.col.names <- gsub(pattern = " ", replacement = "", x = cur.formula.col.names, fixed = TRUE)
                    ## Subset "cur.input.data" to the column names in "cur.formula.col.names".
                    cur.input.data.col.subset <- subset(x = cur.input.data,
                                                        select = cur.formula.col.names)
                    ## Remove missing values from "cur.input.data.col.subset".
                    cur.input.data.col.subset.na.omitted <- na.omit(object = cur.input.data.col.subset)
                    ## Extract formula for maximal model.
                    cur.max.formula <- kFormulas[[cur.formula.name]]
                    ## Create formual for minimal model, based on formula for maximal model.
                    cur.min.formula <- as.formula(object = paste0(as.character(x = cur.max.formula)[2], " ~ 1"))
                    ## Create models.
                    cur.max.model <- glm(formula = cur.max.formula,
                                         data = cur.input.data.col.subset.na.omitted,
                                         family = eval(expr = parse(text = kDistFamilies[[cur.formula.name]])),
                                         na.action = na.omit)
                    cur.min.model <- glm(formula = cur.min.formula,
                                         data = cur.input.data.col.subset.na.omitted,
                                         family = eval(expr = parse(text = kDistFamilies[[cur.formula.name]])),
                                         na.action = na.omit)
                    ## Evaluate models (while sinking output).
                    sink(file = "/dev/null")
                    try(expr =
                            models[["stats..glm"]][[cur.input.data.name]][[cur.formula.name]] <- MASS::stepAIC(object = cur.max.model,
                                                                                                               scope = list(upper = cur.max.model,
                                                                                                                            lower = cur.min.model,
                                                                                                                            direction = "both")))
                    sink(file = NULL)
                }}}}}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))

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
        if (cur.function.name == "mgcv..gam" || cur.function.name == "scam..scam") {  ## ..."mgcv::gam" or "scam::scam".
            cur.function.species.benchmark.df <- data.frame(
                ## "formula" = vector(mode = "character"),
                "model.name" = vector(mode = "character"),
                ## "data.frame" = vector(mode = "character"),
                "GCV" = vector(mode = "numeric"))
        }
        if (cur.function.name == "gamlss..gamlss" || cur.function.name == "stats..glm") {  ## ..."gamlss::gamlss" or "stats::glm".
            cur.function.species.benchmark.df <- data.frame(
                ## "formula" = vector(mode = "character"),
                "model.name" = vector(mode = "character"),
                ## "distribution" = vector(mode = "character"),
                ## "data.frame" = vector(mode = "character"),
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
                ## Store formula of "cur.model" as a string in "cur.formula.string".
                cur.formula.string <- Reduce(f = paste,
                                             x = deparse(expr = cur.model[["formula"]]))
                ## Remove all whitespace in "cur.formula.string".
                cur.formula.string <- gsub(pattern = " ", replacement = "", x = cur.formula.string)
                ## Truncate "cur.formula.string" if it is longer than 110 characters.
                if (nchar(x = cur.formula.string) > 110) {
                    cur.formula.string <- paste0(substr(x = cur.formula.string,
                                                        start = 1,
                                                        stop = 110),
                                                 "...")
                }
                ## Truncate "cur.model.name" if it is longer than 110 characters.
                if (nchar(x = cur.model.name) > 110) {
                    cur.model.name <- paste0(substr(x = cur.model.name,
                                                        start = 1,
                                                        stop = 110),
                                                 "...")
                }
                ## Create the first 2 columns (containing the model formula and the data frame name) of the benchmark data frame.
                cur.formula.data.frame.name.df <- data.frame(
                    "model.name" = cur.model.name
                    ## "formula" = cur.formula.string
                   ## ,"data.frame" = cur.data.frame.name
                )
                ## Prepare storing benchmarks of function...
                ## ..."mgcv::gam" or "scam::scam".
                if (cur.function.name == "mgcv..gam" || cur.function.name == "scam..scam") {
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
                    ## Reset row numbers of "cur.function.species.benchmark.df".
                    rownames(x = cur.function.species.benchmark.df) <- NULL
                    ## Create the name of the file for outputting "cur.function.species.benchmark.df" (different one for "mgcv..gam" and for "scam..scam").
                    if (cur.function.name == "mgcv..gam") {
                        cur.file.suffix <- "_GAM_GCV.txt"
                    }
                    if (cur.function.name == "scam..scam") {
                        cur.file.suffix <- "_SCAM_GCV.txt"
                    }
                    cur.output.file.name <- paste0(kOutputDirPath,
                                                   cur.species.name,
                                                   cur.file.suffix)
                }
                ## ..."gamlss::gamlss" or "stats..glm"..
                if (cur.function.name == "gamlss..gamlss" || cur.function.name == "stats..glm") {
                    ## Store "cur.model[["aic"]]" in "cur.aic".
                    cur.aic <- cur.model[["aic"]]
                    ## Store "kDistFamilies[[cur.model.name]]" in "cur.dist.family".
                    cur.dist.family <- kDistFamilies[[cur.model.name]]
                    ## Store "cur.formula.data.frame.name.df", and "cur.aic" in a 1 row data frame "cur.model.benchmark.df".
                    cur.model.benchmark.df <- data.frame(
                        ## "formula" = cur.formula.data.frame.name.df[, "formula"],
                        "model.name" = cur.model.name,
                        ## "distribution" = cur.dist.family,
                        ## "data.frame" = cur.formula.data.frame.name.df[, "data.frame"],
                        "AIC" = cur.aic)
                    ## Append "cur.model.benchmark.df" to "cur.function.species.benchmark.df".
                    cur.function.species.benchmark.df <- rbind(cur.function.species.benchmark.df,
                                                               cur.model.benchmark.df)
                    ## Order "cur.function.species.benchmark.df" based on column "AIC".
                    cur.function.species.benchmark.df <- cur.function.species.benchmark.df[order(cur.function.species.benchmark.df[["AIC"]]), ]
                    ## Reset row numbers of "cur.function.species.benchmark.df".
                    rownames(x = cur.function.species.benchmark.df) <- NULL
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
                ## Store and write output only if "cur.function.species.benchmark.df" (which should be deleted at the end of every "cur.function.name" loop) exists.
                if (exists(x = "cur.function.species.benchmark.df")) {
                    ## Set options for printing.
                    op.saved <- options("width" = 10^3)
                    ## Store printing of "cur.function.species.benchmark.df" in "cur.output", while left justifying output.
                    cur.output <- capture.output(print(x = format(x = cur.function.species.benchmark.df,
                                                                  justify="left",
                                                                  scientific = FALSE),
                                                       row.names = TRUE))
                    ## Write "cur.output" to "cur.output.file.name".
                    cat(cur.output,
                        file = cur.output.file.name,
                        sep = "\n",
                        fill = FALSE)
                    ## Reset options.
                    options(op.saved)
                }}}}}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.start))
