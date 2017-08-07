##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## {sink(file = "/dev/null"); source(file = "R/Scripts/Modelling.R"); sink()}  ## Evaluate models. The models should end up in list "models" (see "~/laptop02_MasAr/R/Scripts/Modelling.R").
## Load data set.
kBaseFileVersion <- "3.6"
kBaseFileName <- paste0(kDataDir, "gmax_merged_", kBaseFileVersion, ".RData")
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## Tree species according to Wördehoff (2016).
## 110 = Eiche
## 211 = Buche
## 511 = Fichte
## 611 = Douglasie
## 711 = Kiefer

####################
## Plot relations ##
####################
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
                                  "kPlotYLab" = "h100.diff.EKL.I [m]"))
## Set flag to determine whether the newly created .pdf file should be opened.
kOpenPdf <- FALSE
## kOpenPdf <- TRUE
## Initiate "for" loops.
for (cur.species.name in c("beech", "spruce")) {
    for (cur.data.source.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name, ".clean"), x = ls(), fixed = FALSE)]) {
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
                }
            }
        }
    }
}

##############
## Plot GAM ##
##############
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
## Initiate "for" loops.
for (cur.function.name in names(x = models)) {
    for (cur.input.data.source.name in names(x = models[[cur.function.name]])) {
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
                graphics.subdir <- paste0("Graphics/models/GAM/", cur.input.data.source.name, "/")
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
                ## Set plot margins.
                par(mar = kPlotMargins)
                ## Plot model.
                mgcv::plot.gam(x = cur.model,
                               main = as.character(as.expression(x = formula(x = cur.model))))
                ## Turn off graphics device.
                graphics.off()
                ## If desired, open .pdf file via mupdf.
                if (kOpenPdf) {
                    system2(command = "mupdf",
                            args = paste0("-r 64 ",
                                          file.name),
                            wait = FALSE)
                }
            }
        }
    }
}

#################
## Plot GAMLSS ##
#################
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
## Initiate "for" loops.
for (cur.function.name in names(x = models)) {
    for (cur.input.data.source.name in names(x = models[[cur.function.name]])) {
        for (cur.model.name in names(x = models[[cur.function.name]][[cur.input.data.source.name]])) {
            if (grepl(pattern = "GAMLSS_", x = cur.model.name, fixe = TRUE)) {  ## If this is true, it means the current model is a GAMLSS and we can continue with this block.
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
                graphics.subdir <- paste0("Graphics/Models/GAMLSS/", cur.input.data.source.name, "/")
                file.name <-paste0(graphics.subdir,
                                   cur.model.name,
                                   ".pdf")
                ## If nonexistent, create "graphics.subdir".
                system2(command = "mkdir",
                        args = paste0("-p ", graphics.subdir))
                ## Create title page.
                pdf(file = paste0(substr(x = file.name, start = 1, stop = nchar(file.name) - 3), "Titlepage", ".pdf"),
                    width = kPdfWidth,
                    height = kPdfHeight,
                    pointsize = kPdfPointSize,
                    family = kPdfFamily)
                plot(x = 1:10,
                     type = "n",
                     axes = FALSE,
                     xlab = NA,
                     ylab = NA)
                text(x = 5.5,
                     y = 5.5,
                     pos = 1,
                     labels = cur.model.name,
                     cex = 4)
                graphics.off()
                ## Start graphics device driver for producing PDF graphics.
                pdf(file = file.name,
                    width = kPdfWidth,
                    height = kPdfHeight,
                    pointsize = kPdfPointSize,
                    family = kPdfFamily)
                ## Set plot margins.
                par("mar" = kPlotMargins)
                ## Plot model.
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
                                      "cex.main" = 1.5)
                 )
                ## Turn off graphics device.
                graphics.off()
                ## If desired, open .pdf file via mupdf.
                if (kOpenPdf) {
                    system2(command = "mupdf",
                            args = paste0("-r 64 ",
                                          file.name),
                            wait = FALSE)
                }
            }
        }
    }
}

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
    "h100_gha" = list("kPlotMain" = "Measurements and model predictions for gha vs. h100",
                      "kPlotXLabel" = "h100 [m]",
                      "kPlotYLabel" = expression("gha [m"^2*" ha"^-1*"]"),
                      "kNPlots" = 3,
                      "kAppropriateNamesSource" = "names(x = models[[\"nls2..nls2\"]])",
                      "kXSource1" = "data.source[[\"h100\"]]",
                      "kYSource1" = "data.source[[\"gha\"]]",
                      "kCoeffsSource2" = "coef(object = models[[\"nls2..nls2\"]][[data.source.name]][[\"Sterba_Gmax\"]])",
                      "kCurveExpr2" = "pi/(16 * eval(parse(text = kCoeffsSource2))[[\"a0\"]] * eval(parse(text = kCoeffsSource2))[[\"b0\"]] * (x ^ (eval(parse(text = kCoeffsSource2))[[\"a1\"]] + eval(parse(text = kCoeffsSource2))[[\"b1\"]]))) * 10000",
                      "kCurveExpr3" = "pi/(16 * 4.913256e-06 * 0.3716977 * (x ^ (0.4394706 + -0.9097641))) / 10000",
                      "kLegendLegend" = "c(\"Measurements\", as.expression(x = bquote(expr = \"G\"[max]*\"(h\"[100]*\") predicted using estimated coefficients\")), as.expression(x = bquote(expr = \"G\"[max]*\"(h\"[100]*\") predicted using coefficients from Wördehoff (2016)\")))",
                      "kLegendX" = "topleft",
                      "kPch" = c(1, NA, NA),
                      "kLty" = c(NA, 1, 1),
                      "kCol" = c("black", "red", "blue")),
    "dg_nha" = list("kPlotMain" = "Measurements and model predictions for nha vs. dg",
                    "kPlotXLabel" = "dg [cm]",
                    "kPlotYLabel" = expression("nha [ha"^-1*"]"),
                    "kNPlots" = 3,
                    "kAppropriateNamesSource" = "names(x = models[[\"nls2..nls2\"]])",
                    "kXSource1" = "data.source[[\"dg\"]]",
                    "kYSource1" = "data.source[[\"nha\"]]",
                    "kCoeffsSource2" = "coef(object = models[[\"nls2..nls2\"]][[data.source.name]][[\"Sterba_NGmax\"]])",
                    "kCurveExpr2" = "eval(expr = parse(text = kCoeffsSource2))[[\"b0\"]] / eval(expr = parse(text = kCoeffsSource2))[[\"a0\"]] * (2 * eval(expr = parse(text = kCoeffsSource2))[[\"b0\"]] * x) ^ (eval(expr = parse(text = kCoeffsSource2))[[\"a1\"]] / eval(expr = parse(text = kCoeffsSource2))[[\"b1\"]] - 1)",
                    "kCurveExpr3" = "1/x * 27000",
                    "kLegendLegend" = "c(\"Measurements\", as.expression(x = bquote(expr = \"N\"[G[max]]*\"(dg\"[G[max]]*\") predicted using estimated coefficients\")), as.expression(x = bquote(expr = frac(1, x)%.%\"27000\")))",
                    "kLegendX" = "topleft",
                    "kPch" = c(1, NA, NA),
                    "kLty" = c(NA, 1, 1),
                    "kCol" = c("black", "red", "blue")),
    "ln.dg_ln.nha" = list("kPlotMain" = "Measurements and model predictions for ln(nha) vs. ln(dg)",
                          "kPlotXLabel" = "ln(dg)",
                          "kPlotYLabel" = "ln(nha)",
                          "kNPlots" = 4,
                          "kAppropriateNamesSource" = "names(x = models[[\"stats..lm\"]])",
                          "kXSource1" = "data.source[[\"ln.dg\"]]",
                          "kYSource1" = "data.source[[\"ln.nha\"]]",
                          "kCoeffsSource2" = "coef(object = models[[\"stats..lm\"]][[data.source.name]][[\"LM_ln.nha_ln.dg\"]])",
                          "kCoeffsSource4" = "coef(object = models[[\"stats..lm\"]][[data.source.name]][[\"LM_ln.nha_ln.dg_fixed_slope\"]])",
                          "kCurveExpr2" = "eval(expr = parse(text = kCoeffsSource2))[[\"(Intercept)\"]] + eval(expr = parse(text = kCoeffsSource2))[[\"ln.dg\"]] * x",
                          "kCurveExpr3" = "eval(expr = parse(text = kCoeffsSource2))[[\"(Intercept)\"]] + -1.605 * x",
                          "kCurveExpr4" = "eval(expr = parse(text = kCoeffsSource4))[[\"(Intercept)\"]] + -1.605 * x",
                          "kLegendLegend" = "c(\"Measurements\",
                                               paste0(\"y = \",
                                                      eval(parse( text = round(x = eval(parse(text = kCoeffsSource2))[[\"ln.dg\"]], digits = 3))),
                                                      \" x + \",
                                                      eval(parse( text = round(x = eval(parse(text = kCoeffsSource2))[[\"(Intercept)\"]], digits = 3)))),
                                               paste0(\"y = -1.605 x + \",
                                                      eval(parse(text = round(x = eval(parse(text = kCoeffsSource2))[[\"(Intercept)\"]], digits = 3)))),
                                               paste0(\"y = -1.605 x + \",
                                                      eval(parse(text = round(x = eval(parse(text = kCoeffsSource4))[[\"(Intercept)\"]], digits = 3)))))",
                          "kLegendX" = "topleft",
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
        graphics.subdir <- paste0("Graphics/measurements_predictions/", data.source.name, "/")
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
        for (plot.nr in 1:kNPlots) {
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
        }
    }
}

## Continue here ##
"kCoeffsSource5" = "coef(object = models[[\"stats..lm\"]][[data.source.name]][[\"Reineke_improved_quadratic\"]])",
"kCurveExpr5" = "eval(expr = parse(text = kCoeffsSource5))[[\"a\"]] + parse(text = kCoeffsSource5))[[\"b\"]] * x + parse(text = kCoeffsSource5))[[\"c \"]] * x^2",
