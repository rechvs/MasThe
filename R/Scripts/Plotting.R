##############
## Preamble ##
##############
objects.to.keep <- c("models")
rm(list = setdiff(x = ls(), y = objects.to.keep))
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets while suppressing output.
## {sink(file = "/dev/null"); source(file = "R/Scripts/Modelling.R"); sink()}  ## Evaluate models. The models should end up in list "models" (see "~/laptop02_MasAr/R/Scripts/Modelling.R").
## Load data set.
kBaseFileVersion <- "5.3"
kBaseFileName <- paste0(kDataDir, "gmax_merged_", kBaseFileVersion, ".RData")
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Tree species according to Wördehoff (2016).
## 110 = Eiche
## 211 = Buche
## 511 = Fichte
## 611 = Douglasie
## 711 = Kiefer
kBlocksToExecute <- vector(mode = "character")
kBlocksToExecute <- c(kBlocksToExecute, "term.plots.comparison")
## kBlocksToExecute <- c(kBlocksToExecute, "predictions.comparison")
## kBlocksToExecute <- c(kBlocksToExecute, "relations")
## kBlocksToExecute <- c(kBlocksToExecute, "locations")
## kBlocksToExecute <- c(kBlocksToExecute, "GAM")
## kBlocksToExecute <- c(kBlocksToExecute, "SCAM")
## kBlocksToExecute <- c(kBlocksToExecute, "GAMLSS")
## kBlocksToExecute <- c(kBlocksToExecute, "measurements.predictions")
## Store names of objects currently present in workspace.
objects.at.script.start <- c(ls(), "objects.at.script.start")

##############################
## Compare model term plots ##
##############################
## Proceed only if the current block is meant to be executed.
if ("term.plots.comparison" %in% kBlocksToExecute) {
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
    ## Plotting preamble.
    kPdfWidth <- 30
    kPdfHeight <- kPdfWidth * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
    ## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
    ## Create a list of 2-elements lists, according to the following format:
    ## - name of each element = "package..function" pair for the respective function
    ## - elements: set of models meant for comparison.
    kComparisonLists <- list(list("mgcv..gam" = "GAM_gha_sh100.EKL.I_sSI.h100.diff.EKL.I",
                                  "gamlss..gamlss" = "GAMLSS_gha_psh100.EKL.I_psSI.h100.diff.EKL.I"),
                             list("mgcv..gam" = "GAM_gha_sh100.EKL.I_SI.h100.diff.EKL.I",
                                  "gamlss..gamlss" = "GAMLSS_gha_psh100.EKL.I_SI.h100.diff.EKL.I"),
                             list("scam..scam" = "SCAM_gha_mpih100.EKL.I_SI.h100.diff.EKL.I",
                                  "gamlss..gamlss" = "GAMLSS_gha_pbmh100.EKL.I_SI.h100.diff.EKL.I"),
                             list("mgcv..gam" = "GAM_gha_lhs22h100.EKL.I_SI_rhs22h100.EKL.I_SI.h100.diff.EKL.I",
                                  "gamlss..gamlss" = "GAMLSS_gha_lhs22h100.EKL.I_rhs22h100.EKL.I_SI.h100.diff.EKL.I"),
                             list("mgcv..gam" = "GAM_gha_lhs27h100.EKL.I_SI.h100.diff.EKL.I",
                                  "gamlss..gamlss" = "GAMLSS_gha_lhs27h100.EKL.I_SI.h100.diff.EKL.I"))
    ## Store names of objects currently present in workspace.
    objects.at.block.start <- c(ls(), "objects.at.block.start")
    ## Loop over all input data frame names.
    for (cur.input.data.frame.name in c("bart.beech.clean.1.8", "bart.spruce.clean.1.8")) {
        ## Loop over all comparison lists
        for (cur.compar.list in kComparisonLists) {
            ## Turn off graphics device.
            graphics.off()
            ## If nonexistent, create subdirectory in which to store graphics.
            graphics.subdir <- paste0("Graphics/Term_Plot_Comparisons/", cur.input.data.frame.name, "/")
            system2(command = "mkdir",
                    args = paste0("-p ", graphics.subdir))
            ## Create file name.
            file.name <-paste0(graphics.subdir,
                               paste0(cur.compar.list, collapse = "_"),
                               ".pdf")
            ## Start graphics device driver for producing PDF graphics.
            pdf(file = file.name,
                width = kPdfWidth,
                height = kPdfHeight,
                pointsize = kPdfPointSize,
                family = kPdfFamily)
            ## Set plot margins.
            par(mar = kPlotMargins)
            ## Initiate "layout.mat".
            layout.mat <- matrix()
            ## Initiate "nr.of.regression.terms"
            nr.of.regression.terms <- vector(mode = "numeric")
            ## Loop over all model indices in "cur.compar.list."
            for (cur.model.index in seq_len(length.out = length(x = cur.compar.list))) {
                ## Extract current model name as character vector.
                cur.model.name <- as.character(x = cur.compar.list[cur.model.index])
                ## Extract "package..function" pair for current model.
                cur.pckg..fnctn <- names(x = cur.compar.list[cur.model.index])
                ## Extract current model from "models".
                cur.model <- models[[cur.pckg..fnctn]][[cur.input.data.frame.name]][[cur.model.name]]
                ## Determine number of regressor terms in current model.
                nr.of.regression.terms <- c(nr.of.regression.terms,
                                           length(x = attr(x = terms.formula(x = formula(x = cur.model)), which = "term.labels")))
            }
            ## Initiate matrix to be used for "layout(...)", based on contents of "nr.of.regression.terms".
            layout.mat <- matrix(data = NA,
                                 nrow = max(nr.of.regression.terms),
                                 ncol = length(x = nr.of.regression.terms))
            ## Populate "layout.mat".
            for (cur.regression.terms.nr.index in seq_len(length.out = length(x = nr.of.regression.terms))) {
                ## Extract current number of regressor terms.
                cur.nr.of.regression.terms <- nr.of.regression.terms[cur.regression.terms.nr.index]
                ## Create a sequence from "cur.nr.of.regression.terms".
                cur.plot.nrs <- seq_len(length.out = cur.nr.of.regression.terms)
                ## Check whether we are at the first column of "layout.mat" and, depending on the result, set the number to be added to the values in the current column of "layout.mat".
                if (cur.regression.terms.nr.index == 1) {
                    plot.nr.addition <- 0
                } else {
                    plot.nr.addition <- max(layout.mat[, cur.regression.terms.nr.index - 1], na.rm = TRUE)
                }
                layout.mat[, cur.regression.terms.nr.index] <- c(cur.plot.nrs + plot.nr.addition,
                                                                rep(x = 0,
                                                                    times = nrow(x = layout.mat) - length(x = cur.plot.nrs)))
            }
            ## Set plot layout via "layout(...)".
            layout(mat = layout.mat)
            ## Loop over all model names in "cur.compar.list".
            for (cur.model.name in cur.compar.list) {
                ## Extract "package..function" pair for current model.
                cur.pckg..fnctn <- names(x = cur.compar.list[which(x = cur.compar.list == cur.model.name)])
                ## Extract current model from "models".
                cur.model <- models[[cur.pckg..fnctn]][[cur.input.data.frame.name]][[cur.model.name]]
                ## Extract current model formula using the appropriate function call to "formula(...)" for the current model type.
                if (cur.pckg..fnctn == "gamlss..gamlss") {
                    cur.formula <- formula(x = cur.model, what = "mu")
                } else {
                    cur.formula <- formula(x = cur.model)
                }
                ## Store current model formula as a whitespace-free string.
                cur.formula.string <- gsub(pattern = " ",
                                           replacement = "",
                                           x = Reduce(f = paste,
                                                      x = deparse(expr = cur.formula)))
                ## Plot model term effects using the appropriate function for the current model type.
                ## Note (2017-10-09): "mgcv::plot.gam" and "scam::plot.scam" ignore the "main = ..." argument if the current model term is a simple linear term and use the term's variable as the plot's main title. Not sure how to fix this.
                if (cur.pckg..fnctn == "mgcv..gam") {
                    input.data <- get(x = cur.input.data.frame.name)
                    mgcv::plot.gam(x = cur.model,
                                   all.terms = TRUE,
                                   main = paste0(cur.pckg..fnctn,
                                                 ", ",
                                                 cur.formula.string,
                                                 ", ",
                                                 cur.input.data.frame.name))
                }
                if (cur.pckg..fnctn == "scam..scam") {
                    scam::plot.scam(x = cur.model,
                                    all.terms = TRUE,
                                    ## data = get(x = cur.input.data.frame.name),
                                    main = paste0(cur.pckg..fnctn,
                                                  ", ",
                                                  cur.formula.string,
                                                  ", ", cur.input.data.frame.name))
                }
                if (cur.pckg..fnctn == "gamlss..gamlss") {
                    ## Store current input data in "cur.input.data".
                    cur.input.data <- get(x = cur.input.data.frame.name)
                    ## Subset "cur.input.data" to the independent variables of the current model.
                    indep.vars <- all.vars(expr = formula(x = cur.model, what = "mu"))
                    cur.input.data.col.subset <- cur.input.data[, indep.vars]
                    ## Remove missing values from "cur.input.data.col.subset".
                    cur.input.data.col.subset.na.omitted <- na.omit(object = cur.input.data.col.subset)
                    gamlss::term.plot(object = cur.model,
                                      se = TRUE,
                                      partial.resid = FALSE,
                                      what = "mu",
                                      pages = 0,
                                      ask = FALSE,
                                      data = cur.input.data.col.subset.na.omitted,
                                      rug = TRUE,
                                      main = paste0(cur.pckg..fnctn,
                                                    ", ",
                                                    "Mu: ",
                                                    cur.formula.string,
                                                    ", ",
                                                    cur.input.data.frame.name))
                }
            }
            ## Turn off graphics device.
            graphics.off()
        }
        ## Clean up workspace.
        rm(list = setdiff(x = ls(),
                          y = objects.at.block.start))
    }}
## Clean up workspace.
rm(list = setdiff(x = ls(),
                  y = objects.at.script.start))
    
###############################
## Compare model predictions ##
###############################
## Proceed only if the current block is meant to be executed.
if ("predictions.comparison" %in% kBlocksToExecute) {
    ## Plotting preamble.
    kPdfWidth <- 30
    kPdfHeight <- kPdfWidth * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
    ## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
    plot.ltys <- c("solid", "dashed")  ## Vector must contain one line type per model type (GAM, SCAM, GAMLSS) contained in one plot.
    plot.cols <- c("green", "cyan", "blue", "magenta", "brown")  ## Vector must contain one color per yield class contained in one plot.
    grid.col <- "gray"
    grid.lty <- "dashed"
    legend.legend <- vector(mode = "character")
    legend.col <- vector(mode = "character")
    legend.lty <- vector(mode = "character")
    ## Define a list of vectors which contain the names of the models for which predictions should be included in the same plot.
    kModelNamesVectors <- list("set1" = c("SCAM_gha_mpih100.EKL.I_SI.h100.diff.EKL.I",
                                          "GAMLSS_gha_pbmh100.EKL.I_SI.h100.diff.EKL.I"),
                               "set2" = c("GAM_gha_sefu12h100.EKL.I_SI.h100.diff.EKL.I",
                                          "GAMLSS_gha_sefu12h100.EKL.I_SI.h100.diff.EKL.I"),
                               "set3" = c("GAM_gha_sefu13h100.EKL.I_SI.h100.diff.EKL.I",
                                          "GAMLSS_gha_sefu13h100.EKL.I_SI.h100.diff.EKL.I"),
                               "set4" = c("GAM_gha_sefu27h100.EKL.I_SI.h100.diff.EKL.I",
                                          "GAMLSS_gha_sefu27h100.EKL.I_SI.h100.diff.EKL.I"),
                               "set5" = c("GAM_gha_sefu28h100.EKL.I_SI.h100.diff.EKL.I",
                                          "GAMLSS_gha_sefu30h100.EKL.I_SI.h100.diff.EKL.I"))
    ## Loop over all model name vectors.
    for (cur.model.names.vector in kModelNamesVectors) {
        ## Loop over all species names.
        for (cur.species.name in c("beech", "spruce")) {
            ## Get "nagel.SPECIES" and assign it to "pred.df".
            pred.df <- get(x = paste0("nagel.", cur.species.name))
            ## Create the name of the data frame used for model fitting.
            fit.df.name <- paste0("bart.", cur.species.name, ".clean.1.8")
            ## Loop over all model names in current model name vector.
            for (cur.model.name in cur.model.names.vector) {
                model.type <- strsplit(x = cur.model.name,
                                       split = "_",
                                       fixed = TRUE)[[1]][1]
                if (model.type == "GAM") {
                    model <- models[["mgcv..gam"]][[fit.df.name]][[cur.model.name]]
                    pred.df[[cur.model.name]] <- mgcv::predict.gam(object = model,
                                                                   newdata = pred.df,
                                                                   type = "response")
                }
                if (model.type == "SCAM") {
                    model <- models[["scam..scam"]][[fit.df.name]][[cur.model.name]]
                    pred.df[[cur.model.name]] <- scam::predict.scam(object = model,
                                                                    newdata = pred.df,
                                                                    type = "response")
                }
                if (model.type == "GAMLSS") {
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
                    ## Store the data frame used for model fitting in object "cur.input.data.col.subset.na.omitted". [This is necessary for calculating GAMLSS predictions.]
                    cur.input.data.col.subset.na.omitted <- get(x = paste0("bart.", cur.species.name, ".clean.1.8"))
                    model <- models[["gamlss..gamlss"]][[fit.df.name]][[cur.model.name]]
                    pred.df[[cur.model.name]] <- predict(object = model,
                                                         newdata = subset(x = pred.df,
                                                                          select = all.vars(expr = formula(x = model,
                                                                                                           what = "mu"))[-1]),
                                                         what = "mu",
                                                         type = "response")
                }}
            ## Turn off graphics device.
            graphics.off()
            ## If nonexistent, create subdirectory in which to store graphics.
            graphics.subdir <- paste0("Graphics/Predictions_Comparisons/", fit.df.name, "/")
            system2(command = "mkdir",
                    args = paste0("-p ", graphics.subdir))
            ## Create file name.
            file.name <-paste0(graphics.subdir,
                               paste0(cur.model.names.vector, collapse = "_"),
                               ".pdf")
            ## Start graphics device driver for producing PDF graphics.
            pdf(file = file.name,
                width = kPdfWidth,
                height = kPdfHeight,
                pointsize = kPdfPointSize,
                family = kPdfFamily)
            ## Loop over columns "age" and "h100" as the sources for the plot's x-values.
            for (cur.x.values.column.name in c("age", "h100")) {
                ## Create empty plot.
                xmin <- min(pred.df[[cur.x.values.column.name]], na.rm = TRUE)
                xmax <- max(pred.df[[cur.x.values.column.name]], na.rm = TRUE)
                ymin <- min(pred.df[, cur.model.names.vector], na.rm = TRUE)
                ymax <- max(pred.df[, cur.model.names.vector], na.rm = TRUE)
                plot(x = NULL,
                     xlim = c(xmin,
                              xmax + abs(x = (xmax - xmin)) * 0.15),  ## adds additional space to place legend in
                     ylim = c(ymin,
                              ymax),
                     xlab = cur.x.values.column.name,
                     ylab = "gha",
                     main = paste0("Comparison of predictions of ",
                                   paste0(c(paste0(cur.model.names.vector,
                                                   collapse = " and "),
                                            paste0("nagel.",
                                                   cur.species.name)),
                                          collapse = " for ")),
                     panel.first = abline(v = seq(from = 0, to = round(x = xmax + 50, digits = -2), by = 5),  ## Adds a grid to the plot.
                                          h = seq(from = 0, to = round(x = ymax + 50, digits = -2), by = 5),
                                          col = grid.col,
                                          lty = grid.lty))
                ## Loop over all model name indexes.
                for (cur.model.name.index in seq_len(length.out = length(x = cur.model.names.vector))) {
                    model.name <- cur.model.names.vector[cur.model.name.index]
                    ## Loop over all yield class indexes.
                    for (cur.yield.class.index in seq_len(length.out = length(x = levels(x = pred.df[["yield.class"]])))) {
                        ## Extract name of current yield class.
                        yield.class.name <- levels(x = pred.df[["yield.class"]])[cur.yield.class.index]
                        ## Set point color.
                        point.col <- plot.cols[cur.yield.class.index]
                        ## Set line type.
                        line.ty <- plot.ltys[cur.model.name.index]
                        ## Add points to plot.
                        points(x = pred.df[pred.df[["yield.class"]] == yield.class.name, cur.x.values.column.name],
                               y = pred.df[pred.df[["yield.class"]] == yield.class.name, model.name],
                               col = point.col,
                               type = "l",
                               lty = line.ty,
                               lwd = 2)
                        ## Append current combination of model name and yield class to "legend.legend".
                        legend.legend <- c(legend.legend,
                                           paste0(strsplit(x = model.name,
                                                           split = "_",
                                                           fixed = TRUE)[[1]][1],
                                                  " yield class ",
                                                  yield.class.name))
                        ## Append current point/line color to "legend.col".
                        legend.col <- c(legend.col,
                                        point.col)
                        ## Append current line type to "legend.lty".
                        legend.lty <- c(legend.lty,
                                        line.ty)
                    }}
                ## Add legend.
                legend(x = "topright",
                       legend = legend.legend,
                       lty = legend.lty,
                       lwd = 2,
                       col = legend.col,
                       bg = "slategray1")
                ## Reset legend components.
                legend.legend <- vector(mode = "character")
                legend.col <- vector(mode = "character")
                legend.lty <- vector(mode = "character")
            }
            ## Turn off graphics device.
            graphics.off()
        }}}

####################
## Plot relations ##
####################
## Proceed only if the current block is meant to be executed.
if ("relations" %in% kBlocksToExecute) {
    ## Plotting preamble.
    kPdfWidth <- 30
    kPdfHeight <- kPdfWidth * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
    ## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
    kPointsType <- "b"
    kPlotXAxs <- "r"
    kPlotYAxs <- "r"
    kGridCol <- "black"
    kGridLwd <- 2
    kLegendX <- "topright"
    kLegendBg <- "slategray1"
    kColPchSchemes <- c("col.trial.pch.edvid",
                        "col.h100.class",
                        "col.SI.h100.class")
    kLtyVec <- 1
    kLwdVec <- 2
    kColVecAll <- c("#92de6b", "#543090", "#d2c440", "#b75fc3", "#4ca23a", "#d14b8f", "#64e99e", "#ce465a", "#6ee9d9", "#c95730", "#4fb9d2", "#e19a3a", "#7175d2", "#cae176", "#402c63", "#86993b", "#80325d", "#5fb574", "#d090c4", "#365a1e", "#638dc8", "#ae8039", "#55bea0", "#7a3126", "#b0d89d", "#d88674", "#42845a", "#d9c481", "#716026")  ## Generated at "http://tools.medialab.sciences-po.fr/iwanthue/" with "H 0 360", "C 25 75", and "L 0 100".
    kPchVecAll <- c(21:25, 10)
    points.lines.settings <- vector(mode = "list")
    for (cur.colpch.scheme.name in kColPchSchemes) {
        for (cur.species.name in c("beech", "spruce")) {
            ## Create "points.lines.settings[["col.trial.pch.edvid"]][[SPECIES]]".
            if (cur.colpch.scheme.name == "col.trial.pch.edvid") {
                ## Assign current "bart.SPECIES.clean.1.0" to "cur.bart.species.clean.1.0". We’re using "bart.SPECIES.clean.1.0" here, because we want the points and lines setting to be the same for all data frames for color and point scheme "col.trial.pch.edvid".
                cur.bart.species.clean.1.0 <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
                ## Count the number each trial appears in "cur.bart.species.clean.1.0".
                trials.counts <- table(cur.bart.species.clean.1.0[["trial"]])
                ## Create a template for the data frame meant to contain all information necessary for plotting.
                nrows <- nrow(x = cur.bart.species.clean.1.0)
                cur.points.lines.settings.data.frame <- data.frame("trial" = rep(x = NA, times = nrows),
                                                                   "edvid" = rep(x = NA, times = nrows),
                                                                   "auf" = rep(x = NA, times = nrows),
                                                                   "col" = rep(x = NA, times = nrows),
                                                                   "pch" = rep(x = NA, times = nrows),
                                                                   "lty" = rep(x = NA, times = nrows),
                                                                   "lwd" = rep(x = NA, times = nrows),
                                                                   stringsAsFactors = FALSE)
                ## Create a vector containing the colors (one color per "trial").
                cur.col.vec <- vector(mode = "character")
                for (cur.trial.nr in seq_len(length.out = length(x = trials.counts))) {
                    cur.col.vec <- c(cur.col.vec,
                                     rep(x = kColVecAll[cur.trial.nr],
                                         times = trials.counts[cur.trial.nr]))
                }
                ## Create a vector containing the point characters (one point character per "edvid" per "trial").
                cur.pch.vec <- vector(mode = "numeric")
                for (cur.trial in names(x = trials.counts)) {
                    cur.trial.subset <- droplevels(x = cur.bart.species.clean.1.0[cur.bart.species.clean.1.0[["trial"]] == cur.trial, ])
                    cur.edvids <- cur.trial.subset[["edvid"]]
                    cur.edvids.counts <- table(cur.edvids)
                    for (cur.edvid in levels(x = cur.edvids)) {
                        cur.edvid.nr <- which(x = levels(x  = cur.edvids) == cur.edvid)
                        cur.edvid.pch.vec <- rep(x = kPchVecAll[cur.edvid.nr],
                                                 times = cur.edvids.counts[[cur.edvid]])
                        cur.pch.vec <- c(cur.pch.vec,
                                         cur.edvid.pch.vec)
                    }}
                ## Assign the appropriate subset of "cur.points.lines.settings.data.frame" to "points.lines.settings[[cur.colpch.scheme.name]][[cur.species.name]][[cur.measurements.data.frame.name]]".
                for (cur.measurements.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name, ".clean"), x = ls(), fixed = FALSE)]) {
                    cur.measurements.data.frame <- get(x = cur.measurements.data.frame.name)
                    cur.points.lines.settings.data.frame <- data.frame("trial" = cur.bart.species.clean.1.0[["trial"]],
                                                                       "edvid" = cur.bart.species.clean.1.0[["edvid"]],
                                                                       "auf" = cur.bart.species.clean.1.0[["auf"]],
                                                                       "col" = cur.col.vec,
                                                                       "pch" = cur.pch.vec,
                                                                       "lty" = kLtyVec,
                                                                       "lwd" = kLwdVec,
                                                                       stringsAsFactors = FALSE)
                    ## Extract only those points and lines settings relevant for the measurements at hand.
                    points.lines.settings[[cur.colpch.scheme.name]][[cur.species.name]][[cur.measurements.data.frame.name]] <-
                        merge(x = cur.measurements.data.frame,
                              y = cur.points.lines.settings.data.frame,
                              by = c("trial", "edvid", "auf"),
                              sort = FALSE)[, c("trial", "edvid", "auf", "col", "pch", "lty", "lwd")]
                }}}
        if (cur.colpch.scheme.name == "col.h100.class" || cur.colpch.scheme.name == "col.SI.h100.class" ) {
            ## Loop over all species.
            for (cur.species.name in c("beech", "spruce")) {
                ## Extract the name of the column on which to base color selection.
                color.selection.column.name <- substr(x = cur.colpch.scheme.name, start = 5, stop = nchar(x = cur.colpch.scheme.name))
                ## Loop over all appropriate data frames containing measurements.
                for (cur.measurements.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name, ".clean"), x = ls(), fixed = FALSE)]) {
                    ## Assign current measurements data frame.
                    cur.measurements.data.frame <- get(x = cur.measurements.data.frame.name)
                    ## Count the number each class in colum "color.selection.colum.name" occurs in "cur.measurements.data.frame".
                    class.counts <- table(cur.measurements.data.frame[[color.selection.column.name]])
                    ## Create a vector containing the colors (one color per class).
                    cur.col.vec <- rep(x = NA, times = nrow(x = cur.measurements.data.frame))
                    for (cur.class.nr in seq_len(length.out = length(x = class.counts))) {
                        cur.class <- names(x = class.counts)[cur.class.nr]
                        ## Use different colors for different factors.
                        if (cur.colpch.scheme.name == "col.h100.class") {
                            cur.col.vec[cur.measurements.data.frame[[color.selection.column.name]] == cur.class] <- kColVecAll[cur.class.nr]
                        }
                        if (cur.colpch.scheme.name == "col.SI.h100.class") {
                            cur.col.vec[cur.measurements.data.frame[[color.selection.column.name]] == cur.class] <- kColVecAll[cur.class.nr + 3]
                        }
                    }
                    ## Create a vector containing the point characters.
                    cur.pch.vec <- rep(x = 21,
                                       times = nrow(x = cur.measurements.data.frame))
                    ## Assign the appropriate subset of "cur.points.lines.settings.data.frame" to "points.lines.settings[[cur.colpch.scheme.name]][[cur.species.name]][[cur.measurements.data.frame.name]]".
                    cur.points.lines.settings.data.frame <- data.frame(color.selection.column.name = cur.measurements.data.frame[[color.selection.column.name]],
                                                                       "edvid" = cur.measurements.data.frame[["edvid"]],
                                                                       "auf" = cur.measurements.data.frame[["auf"]],
                                                                       "col" = cur.col.vec,
                                                                       "pch" = cur.pch.vec,
                                                                       "lty" = rep(x = 0,
                                                                                   times = nrow(x = cur.measurements.data.frame)),
                                                                       "lwd" = rep(x = 0,
                                                                                   times = nrow(x = cur.measurements.data.frame)),
                                                                       stringsAsFactors = FALSE)
                    names(x = cur.points.lines.settings.data.frame) <- c(color.selection.column.name, "edvid", "auf", "col", "pch", "lty", "lwd")
                    ## Extract only those points and lines settings relevant for the measurements at hand.
                    points.lines.settings[[cur.colpch.scheme.name]][[cur.species.name]][[cur.measurements.data.frame.name]] <-
                        merge(x = cur.measurements.data.frame,
                              y = cur.points.lines.settings.data.frame,
                              by = c(color.selection.column.name, "edvid", "auf"),
                              sort = FALSE)[, c(color.selection.column.name, "edvid", "auf", "col", "pch", "lty", "lwd")]
                }}}}
    ## Create list containing the information necessary to create the respective plot, namely (order may be arbitrary):
    ## - list name: name of the column containing the x values and name of the column containing the y values in this format: XCOL_YCOL
    ## - "kPlotXLab": x axis label
    ## - "kPlotYLab": y axis label
    kPlottingInformation <- list(
        ## "alt_ekl" = list("kPlotXLab" = "alt [a]",
        ## "kPlotYLab" = "ekl"),
        "alt_gha" = list("kPlotXLab" = "alt [a]",
                         "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        "alt_ghaa" = list("kPlotXLab" = "alt [a]",
                          "kPlotYLab" = expression("ghaa [m"^2*" ha"^-1*"]")),
        ## "dg_gha" = list("kPlotXLab" = "dg [cm]",
        ## "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        ## "alt_ghaa.cum" = list("kPlotXLab" = "alt [a]",
        ## "kPlotYLab" = expression("ghaa.cum [m"^2*" ha"^-1*"]")),
        "alt_h100" = list("kPlotXLab" = "alt [a]",
                          "kPlotYLab" = "h100 [m]"),
        "alt_SI.h100" = list("kPlotXLab" = "alt [a]",
                             "kPlotYLab" = "SI.h100 [m]"),
        "h100_gha" = list("kPlotXLab" = "h100 [m]",
                          "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        "gha_h100" = list("kPlotXLab" = expression("gha [m"^2*" ha"^-1*"]"),
                          "kPlotYLab" = "h100 [m]"),
        "gha_SI.h100" = list("kPlotXLab" = expression("gha [m"^2*" ha"^-1*"]"),
                             "kPlotYLab" = "SI.h100 [m]"),
        "SI.h100_gha" = list("kPlotXLab" = "SI.h100 [m]",
                             "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        "log.dg_log.nha" = list("kPlotXLab" = "log.dg",
                                "kPlotYLab" = "log.nha"),
        "h100.EKL.I_h100.diff.EKL.I" = list("kPlotXLab" = "h100.EKL.I [m]",
                                            "kPlotYLab" = "h100.diff.EKL.I [m]"),
        "h100_h100.diff.EKL.I" = list("kPlotXLab" = "h100 [m]",
                                      "kPlotYLab" = "h100.diff.EKL.I [m]"),
        "h100_h100.EKL.I" = list("kPlotXLab" = "h100 [m]",
                                 "kPlotYLab" = "h100.EKL.I [m]"),
        "h100_WGS.EAST" = list("kPlotXLab" = "h100 [m]",
                               "kPlotYLab" = "WGS.EAST"),
        "h100_WGS.NORTH" = list("kPlotXLab" = "h100 [m]",
                                "kPlotYLab" = "WGS.NORTH"))
    ## Set flag to determine whether the newly created .pdf file should be opened.
    kOpenPdf <- FALSE
    ## kOpenPdf <- TRUE
    ## Initiate "for" loops.
    for (cur.species.name in c("beech", "spruce")) {
        for (cur.data.source.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name, "[.]clean[.]1[.][0678]"), x = ls(), fixed = FALSE)]) {
            for (cur.colpch.scheme.name in names(x = points.lines.settings)) {
                for (cur.plotinfo.list.name in names(x = kPlottingInformation)) {
                    ## Turn off graphics device.
                    graphics.off()
                    ## Extract the necessary information for the current plot from "kPlottingInformation".
                    for (cur.plotinfo.object.name in names(x = kPlottingInformation[[cur.plotinfo.list.name]])) {  ## Need to use "for" loop here, because the "*apply" functions seem to drop the name of "X".
                        cur.plotinfo.el <- kPlottingInformation[[cur.plotinfo.list.name]][cur.plotinfo.object.name]
                        assign(x = names(x = cur.plotinfo.el),
                               value = unlist(x = unname(obj = cur.plotinfo.el)))  ## Need to "unname" the object, because plot seemingly cannot handle named expressions. Need to "unlist" the object, because "plot(log = …)" cannot handle lists.
                    }
                    ## Create data source.
                    cur.data.source <- get(x = cur.data.source.name)
                    ## Create vectors containing the actual x and y values.
                    x.column.name <- strsplit(x = cur.plotinfo.list.name, split = "_", fixed = TRUE)[[1]][1]
                    y.column.name <- strsplit(x = cur.plotinfo.list.name, split = "_", fixed = TRUE)[[1]][2]
                    x.values <- cur.data.source[[x.column.name]]
                    y.values <- cur.data.source[[y.column.name]]
                    ## Calculate numerical values necessary for creating the plot.
                    x.lim.low <- range(x.values, na.rm = TRUE)[1]
                    x.lim.high <- range(x.values, na.rm = TRUE)[2] + diff(x = range(x.values, na.rm = TRUE)) * 0.15  ## accounts for extra space for placing the legend.
                    x.lim <- c(x.lim.low, x.lim.high)
                    y.lim.low <- range(y.values, na.rm = TRUE)[1]
                    y.lim.high <- range(y.values, na.rm = TRUE)[2]
                    y.lim <- c(y.lim.low, y.lim.high)
                    ## Create file name.
                    graphics.subdir <- paste0("Graphics/Measurements/", cur.colpch.scheme.name, "/", cur.data.source.name, "/")
                    file.name <-paste0(graphics.subdir,
                                       cur.plotinfo.list.name,
                                       ".pdf")
                    ## If nonexistent, create "graphics.subdir".
                    system2(command = "mkdir",
                            args = paste0("-p ", graphics.subdir))
                    ## Start graphics device driver for producing PDF graphics.
                    pdf(file = file.name,
                        width = kPdfWidth,
                        height = kPdfHeight,
                        pointsize = kPdfPointSize,
                        family = kPdfFamily)
                    ## Set plot margins.
                    par(mar = kPlotMargins)
                    ## Create empty plot.
                    plot(x = NA,
                         y = NA,
                         xlab = kPlotXLab,
                         ylab = kPlotYLab,
                         xlim = x.lim,
                         ylim = y.lim,
                         xaxs = kPlotXAxs,
                         yaxs = kPlotYAxs,
                         main = cur.data.source.name)
                    grid(col = kGridCol,
                         lwd = kGridLwd)
                    ## Extract plotting information from "points.lines.settings.SPECIES" relevant to the current data source.
                    cur.points.lines.settings <- points.lines.settings[[cur.colpch.scheme.name]][[cur.species.name]][[cur.data.source.name]]
                    ## Add points to empty plot (the method depends on the color and point character scheme).
                    if (cur.colpch.scheme.name == "col.trial.pch.edvid") {
                        for (cur.edvid in levels(x = cur.data.source[["edvid"]])) {
                            points(x = x.values[cur.data.source[["edvid"]] == cur.edvid],
                                   y = y.values[cur.data.source[["edvid"]] == cur.edvid],
                                   type = kPointsType,
                                   col = cur.points.lines.settings[["col"]][cur.points.lines.settings[["edvid"]] == cur.edvid],
                                   bg = cur.points.lines.settings[["col"]][cur.points.lines.settings[["edvid"]] == cur.edvid],
                                   pch = cur.points.lines.settings[["pch"]][cur.points.lines.settings[["edvid"]] == cur.edvid],
                                   lty = cur.points.lines.settings[["lty"]][cur.points.lines.settings[["edvid"]] == cur.edvid],
                                   lwd = cur.points.lines.settings[["lwd"]][cur.points.lines.settings[["edvid"]] == cur.edvid])
                        }}
                    if (cur.colpch.scheme.name == "col.h100.class" ||cur.colpch.scheme.name == "col.SI.h100.class") {
                        ## Extract the name of the column on which to base color selection.
                        color.selection.column.name <- substr(x = cur.colpch.scheme.name, start = 5, stop = nchar(x = cur.colpch.scheme.name))
                        for (cur.class in levels(x = cur.data.source[[color.selection.column.name]])) {
                            points(x = x.values[cur.data.source[[color.selection.column.name]] == cur.class],
                                   y = y.values[cur.data.source[[color.selection.column.name]] == cur.class],
                                   type = kPointsType,
                                   col = cur.points.lines.settings[["col"]][cur.points.lines.settings[[color.selection.column.name]] == cur.class],
                                   bg = cur.points.lines.settings[["col"]][cur.points.lines.settings[[color.selection.column.name]] == cur.class],
                                   pch = cur.points.lines.settings[["pch"]][cur.points.lines.settings[[color.selection.column.name]] == cur.class],
                                   lty = cur.points.lines.settings[["lty"]][cur.points.lines.settings[[color.selection.column.name]] == cur.class],
                                   lwd = cur.points.lines.settings[["lwd"]][cur.points.lines.settings[[color.selection.column.name]] == cur.class])
                        }}
                    ## Add legend.
                    if (cur.colpch.scheme.name == "col.trial.pch.edvid") {
                        cur.points.lines.settings.legend <- unique(x = cur.points.lines.settings[, c("trial", "edvid", "col", "pch", "lty", "lwd")])
                        legend(x = kLegendX,
                               legend = paste0("edvid: ", cur.points.lines.settings.legend[["edvid"]]),
                               bg = kLegendBg,
                               col = cur.points.lines.settings.legend[["col"]],
                               pt.bg = cur.points.lines.settings.legend[["col"]],
                               pch = cur.points.lines.settings.legend[["pch"]],
                               lty = cur.points.lines.settings.legend[["lty"]],
                               lwd = cur.points.lines.settings.legend[["lwd"]])
                    }
                    if (cur.colpch.scheme.name == "col.h100.class" ||cur.colpch.scheme.name == "col.SI.h100.class") {
                        ## Extract the name of the column on which to base color selection.
                        color.selection.column.name <- substr(x = cur.colpch.scheme.name, start = 5, stop = nchar(x = cur.colpch.scheme.name))
                        cur.points.lines.settings.legend <- unique(x = cur.points.lines.settings[, c(color.selection.column.name, "col", "pch", "lty", "lwd")])
                        legend(x = kLegendX,
                               legend = paste0(color.selection.column.name, ": ", cur.points.lines.settings.legend[[color.selection.column.name]]),
                               bg = kLegendBg,
                               col = cur.points.lines.settings.legend[["col"]],
                               pt.bg = cur.points.lines.settings.legend[["col"]],
                               pch = cur.points.lines.settings.legend[["pch"]],
                               lty = cur.points.lines.settings.legend[["lty"]],
                               lwd = cur.points.lines.settings.legend[["lwd"]])
                    }
                    ## Turn off graphics device.
                    graphics.off()
                    ## If desired, open .pdf file via mupdf.
                    if (kOpenPdf) {
                        system2(command = "mupdf",
                                args = paste0("-r 64 ",
                                              file.name),
                                wait = FALSE)
                    }}}}}}

####################
## Plot locations ##
####################
## Proceed only if the current block is meant to be executed.
if ("locations" %in% kBlocksToExecute) {
    ## Plotting preamble.
    kLocationsGraphicsDir <- "Graphics/Locations/"
    ## kColVecAll <- c("#92de6b", "#543090", "#d2c440", "#b75fc3", "#4ca23a", "#d14b8f", "#64e99e", "#ce465a", "#6ee9d9", "#c95730", "#4fb9d2", "#e19a3a", "#7175d2", "#cae176", "#402c63", "#86993b", "#80325d", "#5fb574", "#d090c4", "#365a1e", "#638dc8", "#ae8039", "#55bea0", "#7a3126", "#b0d89d", "#d88674", "#42845a", "#d9c481", "#716026")  ## Generated at "http://tools.medialab.sciences-po.fr/iwanthue/" with "H 0 360", "C 25 75", and "L 0 100".
    kColVecAll <- "black"
    kPchVecAll <- c(21:25, 10)
    kPchVecAll <- 20
    kPdfHeight <- 20
    kPdfWidth <- kPdfHeight * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    ## kPlotMargins <- c(0, 0, 1.5, 0)  ## As small as possible using fractions of lines.
    kPlotMargins <- c(0, 0, 0, 0)
    ## Loop over all species.
    for (cur.species.name in c("beech", "spruce")) {
        ## Loop over all data frames.
        for (cur.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name), x = ls(), fixed = FALSE)]) {
            ## Assign "bart.SPECIES.clean.1.8".
            cur.data.frame <- get(x = cur.data.frame.name)
            ## Condense "cur.data.frame" to the unique values of columns "edvid", "EAST.UTM", and "NORTH.UTM".
            cur.data.frame <- unique(x = cur.data.frame[, c("edvid", "EAST.UTM", "NORTH.UTM")])
            ## Extract easting and northing.
            easting <- cur.data.frame[["EAST.UTM"]]
            northing <- cur.data.frame[["NORTH.UTM"]]
            ## Calculate plot axis limits.
            x.lim.low <- range(easting, na.rm = TRUE)[1]
            x.lim.high <- range(easting, na.rm = TRUE)[2]
            y.lim.low <- range(northing, na.rm = TRUE)[1]
            y.lim.high <- range(northing, na.rm = TRUE)[2]
            x.lim <- c(x.lim.low, x.lim.high)
            y.lim <- c(y.lim.low, y.lim.high)
            ## Create file name.
            file.name <-paste0(kLocationsGraphicsDir, "Locations_", cur.data.frame.name, ".pdf")
            ## If nonexistent, create "kLocationsGraphicsDir".
            system2(command = "mkdir",
                    args = paste0("-p ", kLocationsGraphicsDir))
            ## Turn off graphics device.
            graphics.off()
            ## Start graphics device driver for producing PDF graphics.
            pdf(file = file.name,
                width = kPdfWidth,
                height = kPdfHeight,
                pointsize = kPdfPointSize,
                family = kPdfFamily)
            ## Set plot margins.
            par(mar = kPlotMargins)
            ## Plot national and federal state boundaries.
            library("sp")
            plot(x = bld_utm,
                 add = FALSE,
                 ## xlim = x.lim,
                 ## ylim = y.lim
                 )
            ## Add plot title (we’re using "title(...)" because the normal placement via "plot(main = ...)" wastes too much space).
            title(main = cur.data.frame.name,
                  outer = FALSE,
                  line = -4)
            ## Add locations.
            points(x = easting,
                   y = northing,
                   xlab = "Easting",
                   ylab = "Northing",
                   bg = kColVecAll,
                   col = kColVecAll,
                   pch = kPchVecAll)
            ## Turn off graphics device.
            graphics.off()
        }}}

###############
## Plot SCAM ##
###############
## Proceed only if the current block is meant to be executed.
if ("SCAM" %in% kBlocksToExecute) {
    ## Plotting preamble.
    kPdfWidth <- 30
    kPdfHeight <- kPdfWidth * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
    ## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
    ## Set flag to determine whether the newly created .pdf file should be opened.
    kOpenPdf <- FALSE
    ## kOpenPdf <- TRUE
    ## Loop over all modelling function names.
    for (cur.function.name in names(x = models)) {
        ## Proceed only if "cur.function.name == "scam..scam"".
        if (cur.function.name == "scam..scam") {
            ## Loop over all input data source names.
            for (cur.input.data.source.name in names(x = models[[cur.function.name]])) {
                ## Loop over all model names.
                for (cur.model.name in names(x = models[[cur.function.name]][[cur.input.data.source.name]])) {
                    if (grepl(pattern = "SCAM_", x = cur.model.name, fixe = TRUE)) {  ## If this is true, it means the current model is a SCAM and we can continue with this block.
                        ## Extract current model.
                        cur.model <- models[[cur.function.name]][[cur.input.data.source.name]][[cur.model.name]]
                        ## Extract current model formula.
                        cur.formula <- cur.model[["formula"]]
                        ## Store current model formula as a string.
                        cur.formula.string <- Reduce(f = paste,
                                                     x = deparse(expr = cur.formula))
                        ## Remove whitespace from "cur.formula.string".
                        cur.formula.string <- gsub(pattern = " ", replacement = "", x = cur.formula.string)
                        ## Turn off graphics device.
                        graphics.off()
                        ## Create file name.
                        file.name <- gsub(pattern = "[$]",
                                          replacement = ".",
                                          x = paste0("Graphics/",
                                                     cur.model.name,
                                                     ".pdf"))
                        ## Create file name.
                        graphics.subdir <- paste0("Graphics/Models/SCAM/", cur.input.data.source.name, "/")
                        file.name <-paste0(graphics.subdir,
                                           cur.model.name,
                                           ".pdf")
                        ## If nonexistent, create "graphics.subdir".
                        system2(command = "mkdir",
                                args = paste0("-p ", graphics.subdir))
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
                        if (nr.independent.vars >= 5) {
                            mfrow <- c(3, 2)
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
                        ## If desired, open .pdf file via mupdf.
                        if (kOpenPdf) {
                            system2(command = "mupdf",
                                    args = paste0("-r 64 ",
                                                  file.name),
                                    wait = FALSE)
                        }}}}}}}

##############
## Plot GAM ##
##############
## Proceed only if the current block is meant to be executed.
if ("GAM" %in% kBlocksToExecute) {
    ## Plotting preamble.
    kPdfWidth <- 30
    kPdfHeight <- kPdfWidth * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
    ## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
    ## Set flag to determine whether the newly created .pdf file should be opened.
    kOpenPdf <- FALSE
    ## kOpenPdf <- TRUE
    ## Loop over all modelling function names.
    for (cur.function.name in names(x = models)) {
        ## Proceed only if "cur.function.name == "mgcv..gam"".
        if (cur.function.name == "mgcv..gam") {
            ## Loop over all input data source names.
            for (cur.input.data.source.name in names(x = models[[cur.function.name]])) {
                ## Loop over all model names.
                for (cur.model.name in names(x = models[[cur.function.name]][[cur.input.data.source.name]])) {
                    if (grepl(pattern = "GAM_", x = cur.model.name, fixe = TRUE)) {  ## If this is true, it means the current model is a GAM and we can continue with this block.
                        ## Extract current model.
                        cur.model <- models[[cur.function.name]][[cur.input.data.source.name]][[cur.model.name]]
                        ## Turn off graphics device.
                        graphics.off()
                        ## Create file name.
                        file.name <- gsub(pattern = "[$]",
                                          replacement = ".",
                                          x = paste0("Graphics/",
                                                     cur.model.name,
                                                     ".pdf"))
                        ## Create file name.
                        graphics.subdir <- paste0("Graphics/Models/GAM/", cur.input.data.source.name, "/")
                        file.name <-paste0(graphics.subdir,
                                           cur.model.name,
                                           ".pdf")
                        ## If nonexistent, create "graphics.subdir".
                        system2(command = "mkdir",
                                args = paste0("-p ", graphics.subdir))
                        ## Start graphics device driver for producing PDF graphics.
                        pdf(file = file.name,
                            width = kPdfWidth,
                            height = kPdfHeight,
                            pointsize = kPdfPointSize,
                            family = kPdfFamily)
                        ## Set plot layout, depending on number of independent variables.
                        nr.independent.vars <- length(x = all.vars(expr = formula(x = cur.model))[-1])
                        if (nr.independent.vars == 1) {
                            mfrow <- c(1, 1)
                        }
                        if (nr.independent.vars == 2) {
                            mfrow <- c(2, 1)
                        }
                        if (nr.independent.vars == 3 || nr.independent.vars == 4) {
                            mfrow <- c(2, 2)
                        }
                        if (nr.independent.vars >= 5) {
                            mfrow <- c(3, 2)
                        }
                        par(mfrow = mfrow)
                        ## Set plot margins.
                        par(mar = kPlotMargins)
                        ## Plot model term effects.
                        mgcv::plot.gam(x = cur.model,
                                       all.terms = TRUE,
                                       main = paste0(as.character(x = as.expression(x = formula(x = cur.model))),
                                                     ", ", cur.input.data.source.name))
                        ## Plot model diagnostics.
                        mgcv::gam.check(b = cur.model,
                                        type = "response",
                                        pch = 19)
                        ## Turn off graphics device.
                        graphics.off()
                        ## If desired, open .pdf file via mupdf.
                        if (kOpenPdf) {
                            system2(command = "mupdf",
                                    args = paste0("-r 64 ",
                                                  file.name),
                                    wait = FALSE)
                        }}}}}}}

#################
## Plot GAMLSS ##
#################
## Proceed only if the current block is meant to be executed.
if ("GAMLSS" %in% kBlocksToExecute) {
    ## Plotting preamble.
    kPdfWidth <- 30
    kPdfHeight <- kPdfWidth * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
    ## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
    ## Set flag to determine whether the newly created .pdf file should be opened.
    kOpenPdf <- FALSE
    ## kOpenPdf <- TRUE
    ## Loop over all modelling function names.
    for (cur.function.name in names(x = models)) {
        ## Proceed only if "cur.function.name == "gamlss..gamlss"".
        if (cur.function.name == "gamlss..gamlss") {
            ## Loop over all input data source names.
            for (cur.input.data.source.name in names(x = models[[cur.function.name]])) {
                ## Store path to graphics subdirectory for current input data source in "cur.graphics.subdir".
                cur.graphics.subdir <- paste0("Graphics/Models/GAMLSS/", cur.input.data.source.name, "/")
                ## If nonexistent, create "cur.graphics.subdir".
                system2(command = "mkdir",
                        args = paste0("-p ", cur.graphics.subdir))
                ## Loop over all models.
                for (cur.model.name in names(x =  models[[cur.function.name]][[cur.input.data.source.name]])) {
                    if (grepl(pattern = "GAMLSS_", x = cur.model.name, fixe = TRUE)) {  ## If this is true, it means the current model is a GAMLSS and we can continue with this block.
                        ## Extract current model.
                        cur.model <- models[[cur.function.name]][[cur.input.data.source.name]][[cur.model.name]]
                        ## Extract distribution parameter names.
                        dist.params.names <- cur.model[["parameters"]]
                        ## Turn off graphics device.
                        graphics.off()
                        ## Create filename.
                        file.name <- paste0(cur.graphics.subdir,
                                            cur.model.name,
                                            ".pdf")
                        ## Start graphics device driver for producing PDF graphics.
                        pdf(file = file.name,
                            width = kPdfWidth,
                            height = kPdfHeight,
                            pointsize = kPdfPointSize,
                            family = kPdfFamily)
                        ## Plot regression terms. ##
                        ## Loop over all distribution parameters.
                        for (cur.dist.parameter.name in dist.params.names) {
                            ## Set default model formulas for all distribution parameters.
                            mu.formula <- vector(mode = "character")
                            sigma.formula <- vector(mode = "character")
                            nu.formula <- vector(mode = "character")
                            tau.formula <- vector(mode = "character")
                            ## Extract actual model formula for current distribution parameter and store it in "cur.formula".
                            assign(x = "cur.formula",
                                   value = formula(x = cur.model, what = cur.dist.parameter.name))
                            ## If "cur.formula != as.formula("~ 1"), continue, else, skip over to the next parameter.
                            if (cur.formula != as.formula(object = "gha ~ 1")) {
                                ## Set plot margins.
                                par("mar" = kPlotMargins)
                                ## Store current input data in "cur.input.data".
                                cur.input.data <- get(x = cur.input.data.source.name)
                                ## Subset "cur.input.data" to the column names in "kColumnsToSelect[[cur.model.name]]".
                                cur.input.data.col.subset <- cur.input.data[, kColumnsToSelect[[cur.model.name]]]
                                ## Remove missing values from "cur.input.data.col.subset".
                                cur.input.data.col.subset.na.omitted <- na.omit(object = cur.input.data.col.subset)
                                ## Set plot layout, depending on number of independent variables.
                                nr.independent.vars <- length(x = all.vars(expr = formula(x = cur.model,
                                                                                          what = cur.dist.parameter.name))[-1])
                                if (nr.independent.vars == 1) {
                                    mfrow <- c(1, 1)
                                }
                                if (nr.independent.vars == 2) {
                                    mfrow <- c(2, 1)
                                }
                                if (nr.independent.vars == 3 || nr.independent.vars == 4) {
                                    mfrow <- c(2, 2)
                                }
                                if (nr.independent.vars >= 5) {
                                    mfrow <- c(3, 2)
                                }
                                par(mfrow = mfrow)
                                ## Plot regression terms for current distribution parameter.
                                gamlss::term.plot(object = cur.model,
                                                  se = TRUE,
                                                  partial.resid = FALSE,
                                                  what = cur.dist.parameter.name,
                                                  pages = 0,
                                                  ask = FALSE,
                                                  data = cur.input.data.col.subset.na.omitted,
                                                  rug = TRUE)
                                ## Add plot title (we use "title(...)" because the placing via "plot(main = ...)" does not suit our needs).
                                title(main = paste0(toupper(x = substr(x = cur.dist.parameter.name,
                                                                       start = 1,
                                                                       stop = 1)),
                                                    substr(x = cur.dist.parameter.name,
                                                           start = 2,
                                                           stop = nchar(x = cur.dist.parameter.name)),
                                                    ": ",
                                                    as.character(x = as.expression(x = formula(x = cur.model,
                                                                                               what = cur.dist.parameter.name))),
                                                    ", ",
                                                    cur.input.data.source.name),
                                      line = 0.25)
                            }}
                        ## Plot model overview. ##
                        ## Set plot margins.
                        par("mar" = kPlotMargins)
                        ## Create plots.
                        plot(x = cur.model,
                             parameters = par("mfrow" = c(2, 2),  ## Settings inspired by Stasinopoulos et al. (2008), p. 122.
                                              "mar" = par("mar") + c(0, 1, 0, 0),
                                              "col.axis" = "black",
                                              "col" = "black",
                                              "col.main" = "black",
                                              "col.lab" = "black",
                                              "pch" = 20,
                                              "cex" = 1.00,
                                              "cex.lab" = 1.00,
                                              "cex.axis" = 1,
                                              "cex.main" = 1.5))
                        ## Turn off graphics device.
                        graphics.off()
                        ## If desired, open .pdf file via mupdf.
                        if (kOpenPdf) {
                            system2(command = "mupdf",
                                    args = paste0("-r 64 ",
                                                  file.name),
                                    wait = FALSE)
                        }}}}}}}

##############
## QQ-Plots ##
##############
if (FALSE) { ## WORK IN PROGRESS (2017-06-15) ##
    X <- bart.clean$gha
    X <- bart.clean$ksha
    p <- seq(from = 0.01, to = 0.99, by = 0.01)
    q <- quantile(x = Z,
                  probs = p)
    Y <- qnorm(p = p)
    Y <- qBCCG(p = p,
               mu = mean(x = X),
               sigma = 0.35)
    qqplot(x = X,
           y = Y,
           xlab = "Sample Quantiles",
           ylab = "Theoretical Quantiles")
    abline(a = 0,
           b = 1)
}

#####################################################
## Plot relations and respective model predictions ##
#####################################################
## Proceed only if the current block is meant to be executed.
if ("measurements.predictions" %in% kBlocksToExecute) {
    ## Plotting preamble.
    kPdfWidth <- 30
    kPdfHeight <- kPdfWidth * 0.625
    kPdfPointSize <- 19
    kPdfFamily <- "Times"
    kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
    ## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
    kPlotType <- "p"
    kPlotXAxs <- "r"
    kPlotYAxs <- "r"
    kLegendBg <- "slategray1"
    kPlottingInformation <- list(
        ## "h100_gha" = list("kPlotMain" = "Measurements and model predictions for gha vs. h100",
        ## "kPlotXLabel" = "h100 [m]",
        ## "kPlotYLabel" = expression("gha [m"^2*" ha"^-1*"]"),
        ## "kNPlots" = 3,
        ## "kAppropriateNamesSource" = "names(x = models[[\"nls2..nls2\"]])",
        ## "kXSource1" = "data.source[[\"h100\"]]",
        ## "kYSource1" = "data.source[[\"gha\"]]",
        ## "kCoeffsSource2" = "coef(object = models[[\"nls2..nls2\"]][[data.source.name]][[\"Sterba_Gmax\"]])",
        ## "kCurveExpr2" = "pi/(16 * eval(parse(text = kCoeffsSource2))[[\"a0\"]] * eval(parse(text = kCoeffsSource2))[[\"b0\"]] * (x ^ (eval(parse(text = kCoeffsSource2))[[\"a1\"]] + eval(parse(text = kCoeffsSource2))[[\"b1\"]]))) * 10000",
        ## "kCurveExpr3" = "pi/(16 * 4.913256e-06 * 0.3716977 * (x ^ (0.4394706 + -0.9097641))) / 10000",
        ## "kLegendLegend" = "c(\"Measurements\", as.expression(x = bquote(expr = \"G\"[max]*\"(h\"[100]*\") predicted using estimated coefficients\")), as.expression(x = bquote(expr = \"G\"[max]*\"(h\"[100]*\") predicted using coefficients from Wördehoff (2016)\")))",
        ## "kLegendX" = "topright",
        ## "kPch" = c(1, NA, NA),
        ## "kLty" = c(NA, 1, 1),
        ## "kCol" = c("black", "red", "blue")),
        ## "dg_nha" = list("kPlotMain" = "Measurements and model predictions for nha vs. dg",
        ## "kPlotXLabel" = "dg [cm]",
        ## "kPlotYLabel" = expression("nha [ha"^-1*"]"),
        ## "kNPlots" = 3,
        ## "kAppropriateNamesSource" = "names(x = models[[\"nls2..nls2\"]])",
        ## "kXSource1" = "data.source[[\"dg\"]]",
        ## "kYSource1" = "data.source[[\"nha\"]]",
        ## "kCoeffsSource2" = "coef(object = models[[\"nls2..nls2\"]][[data.source.name]][[\"Sterba_NGmax\"]])",
        ## "kCurveExpr2" = "eval(expr = parse(text = kCoeffsSource2))[[\"b0\"]] / eval(expr = parse(text = kCoeffsSource2))[[\"a0\"]] * (2 * eval(expr = parse(text = kCoeffsSource2))[[\"b0\"]] * x) ^ (eval(expr = parse(text = kCoeffsSource2))[[\"a1\"]] / eval(expr = parse(text = kCoeffsSource2))[[\"b1\"]] - 1)",
        ## "kCurveExpr3" = "1/x * 27000",
        ## "kLegendLegend" = "c(\"Measurements\", as.expression(x = bquote(expr = \"N\"[G[max]]*\"(dg\"[G[max]]*\") predicted using estimated coefficients\")), as.expression(x = bquote(expr = frac(1, x)%.%\"27000\")))",
        ## "kLegendX" = "topright",
        ## "kPch" = c(1, NA, NA),
        ## "kLty" = c(NA, 1, 1),
        ## "kCol" = c("black", "red", "blue")),
        ## "ln.dg_ln.nha" = list("kPlotMain" = "Measurements and model predictions for ln(nha) vs. ln(dg)",
        ## "kPlotXLabel" = "ln(dg)",
        ## "kPlotYLabel" = "ln(nha)",
        ## "kNPlots" = 4,
        ## "kAppropriateNamesSource" = "names(x = models[[\"stats..lm\"]])",
        ## "kXSource1" = "data.source[[\"ln.dg\"]]",
        ## "kYSource1" = "data.source[[\"ln.nha\"]]",
        ## "kCoeffsSource2" = "coef(object = models[[\"stats..lm\"]][[data.source.name]][[\"LM_ln.nha_ln.dg\"]])",
        ## "kCoeffsSource4" = "coef(object = models[[\"stats..lm\"]][[data.source.name]][[\"LM_ln.nha_ln.dg_fixed_slope\"]])",
        ## "kCurveExpr2" = "eval(expr = parse(text = kCoeffsSource2))[[\"(Intercept)\"]] + eval(expr = parse(text = kCoeffsSource2))[[\"ln.dg\"]] * x",
        ## "kCurveExpr3" = "eval(expr = parse(text = kCoeffsSource2))[[\"(Intercept)\"]] + -1.605 * x",
        ## "kCurveExpr4" = "eval(expr = parse(text = kCoeffsSource4))[[\"(Intercept)\"]] + -1.605 * x",
        ## "kLegendLegend" = "c(\"Measurements\",
        ## paste0(\"Curve 1 (estimated slope, estimated intercept): y = \",
        ## eval(parse( text = round(x = eval(parse(text = kCoeffsSource2))[[\"ln.dg\"]], digits = 3))),
        ## \" x + \",
        ## eval(parse( text = round(x = eval(parse(text = kCoeffsSource2))[[\"(Intercept)\"]], digits = 3)))),
        ## paste0(\"Curve 2 (Reinek slope, intercept from curve 1): y = -1.605 x + \",
        ## eval(parse(text = round(x = eval(parse(text = kCoeffsSource2))[[\"(Intercept)\"]], digits = 3)))),
        ## paste0(\"Curve 3 (Reineke slope, estimated intercept): y = -1.605 x + \",
        ## eval(parse(text = round(x = eval(parse(text = kCoeffsSource4))[[\"(Intercept)\"]], digits = 3)))))",
        ## "kLegendX" = "topright",
        ## "kPch" = c(1, NA, NA, NA),
        ## "kLty" = c(NA, 1, 1, 1),
        ## "kCol" = c("black", "red", "blue", "magenta")),
        "log.dg_log.nha" = list("kPlotMain" = "Measurements and model predictions for log(nha) vs. log(dg)",
                                "kPlotXLabel" = "log(dg)",
                                "kPlotYLabel" = "log(nha)",
                                "kNPlots" = 4,
                                "kAppropriateNamesSource" = "names(x = models[[\"stats..lm\"]])",
                                "kXSource1" = "data.source[[\"log.dg\"]]",
                                "kYSource1" = "data.source[[\"log.nha\"]]",
                                "kCoeffsSource2" = "coef(object = models[[\"stats..lm\"]][[data.source.name]][[\"LM_log.nha_log.dg\"]])",
                                "kCoeffsSource4" = "coef(object = models[[\"stats..lm\"]][[data.source.name]][[\"LM_log.nha_log.dg_fixed_slope\"]])",
                                "kCurveExpr2" = "eval(expr = parse(text = kCoeffsSource2))[[\"(Intercept)\"]] + eval(expr = parse(text = kCoeffsSource2))[[\"log.dg\"]] * x",
                                "kCurveExpr3" = "eval(expr = parse(text = kCoeffsSource2))[[\"(Intercept)\"]] + -1.605 * x",
                                "kCurveExpr4" = "eval(expr = parse(text = kCoeffsSource4))[[\"(Intercept)\"]] + -1.605 * x",
                                "kLegendLegend" = "c(\"Measurements\",
                                                     paste0(\"Curve 1 (estimated slope, estimated intercept): y = \",
                                                            eval(parse( text = round(x = eval(parse(text = kCoeffsSource2))[[\"log.dg\"]], digits = 3))),
                                                            \" x + \",
                                                            eval(parse( text = round(x = eval(parse(text = kCoeffsSource2))[[\"(Intercept)\"]], digits = 3)))),
                                                     paste0(\"Curve 2 (Reinek slope, intercept from curve 1): y = -1.605 x + \",
                                                            eval(parse(text = round(x = eval(parse(text = kCoeffsSource2))[[\"(Intercept)\"]], digits = 3)))),
                                                     paste0(\"Curve 3 (Reineke slope, estimated intercept): y = -1.605 x + \",
                                                            eval(parse(text = round(x = eval(parse(text = kCoeffsSource4))[[\"(Intercept)\"]], digits = 3)))))",
                                "kLegendX" = "topright",
                                "kPch" = c(1, NA, NA, NA),
                                "kLty" = c(NA, 1, 1, 1),
                                "kCol" = c("black", "red", "blue", "magenta")))
    ## Set flag to determine whether the newly created .pdf file should be opened.
    kOpenPdf <- FALSE
    ## kOpenPdf <- TRUE
    ## Initiate "for" loops.
    for (cur.list.name in names(x = kPlottingInformation)) {
        for (data.source.name in eval(expr = parse(text = kPlottingInformation[[cur.list.name]][["kAppropriateNamesSource"]]))) {
            ## Create data source.
            data.source <- eval(expr = parse(text = data.source.name))
            ## Turn off graphics device.
            graphics.off()
            ## Extract the necessary information for the current plot from "kPlottingInformation".
            for (cur.el.name in names(x = kPlottingInformation[[cur.list.name]])) {  ## Need to use "for" loop here, because the "*apply" functions seem to drop the name of "X".
                cur.el <- kPlottingInformation[[cur.list.name]][cur.el.name]
                assign(x = names(x = cur.el),
                       value = unlist(x = unname(obj = cur.el)))  ## Need to "unname" the object, because plot seemingly cannot handle named expressions. Need to "unlist" the object, because "plot(log = …)" cannot handle lists.
            }
            ## Create file name.
            graphics.subdir <- paste0("Graphics/Measurements_Predictions/", data.source.name, "/")
            file.name <-paste0(graphics.subdir,
                               cur.list.name,
                               ".pdf")
            ## If nonexistent, create "graphics.subdir".
            system2(command = "mkdir",
                    args = paste0("-p ", graphics.subdir))
            ## Start graphics device driver for producing PDF graphics.
            pdf(file = file.name,
                width = kPdfWidth,
                height = kPdfHeight,
                pointsize = kPdfPointSize,
                family = kPdfFamily)
            for (plot.nr in seq_len(length.out = kNPlots)) {
                ## Set plot margins.
                par(mar = kPlotMargins)
                if (plot.nr == 1) {  ## If this is true, it means we are plotting the base relation and need to determine all settings required for and call "plot(…)".
                    ## Create vectors containing the actual x and y values.
                    x.values <- eval(expr = parse(text = eval(expr = parse(text = paste0("kXSource", plot.nr)))))
                    y.values <- eval(expr = parse(text = eval(expr = parse(text = paste0("kYSource", plot.nr)))))
                    ## Calculate numerical values necessary for creating the plot.
                    x.lim.low <- range(x.values, na.rm = TRUE)[1]
                    x.lim.high <- range(x.values, na.rm = TRUE)[2]
                    x.lim <- c(x.lim.low, x.lim.high)
                    y.lim.low <- range(y.values, na.rm = TRUE)[1]
                    y.lim.high <- range(y.values, na.rm = TRUE)[2] # + diff(x = range(y.values, na.rm = TRUE)) * 0.10  ## accounts for extra space for placing the legend.
                    y.lim <- c(y.lim.low, y.lim.high)
                    ## Create plot.
                    plot(x = x.values,
                         y = y.values,
                         xlab = kPlotXLabel,
                         ylab = kPlotYLabel,
                         xlim = x.lim,
                         ylim = y.lim,
                         xaxs = kPlotXAxs,
                         yaxs = kPlotYAxs,
                         main = paste0(kPlotMain, " (", data.source.name, ")"),
                         pch = kPch[plot.nr],
                         col = kCol[plot.nr],
                         type = kPlotType)
                } else {  ## If this is true, it means we are plotting model predictions and need to use "curve(…)".
                    ## Add curve.
                    func <- function(x) {}
                    body(func) <- parse(text = eval(expr = parse(text = paste0("kCurveExpr", plot.nr))))
                    curve(expr = func,
                          add = TRUE,
                          col = kCol[plot.nr],
                          lty = kLty[plot.nr])
                }
            }
            ## Add legend.
            legend(x = kLegendX,
                   legend = eval(expr = parse(text = kLegendLegend)),
                   bg = kLegendBg,
                   col = kCol,
                   pch = kPch,
                   lty = kLty)
            ## Turn off graphics device.
            graphics.off()
            ## If desired, open .pdf file via mupdf.
            if (kOpenPdf) {
                system2(command = "mupdf",
                        args = paste0("-r 64 ",
                                      file.name),
                        wait = FALSE)
            }}}}
