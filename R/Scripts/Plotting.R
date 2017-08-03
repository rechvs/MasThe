##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## {sink(file = "/dev/null"); source(file = "R/Scripts/Modelling.R"); sink()}  ## Evaluate models. The models should end up in list "models" (see "~/laptop02_MasAr/R/Scripts/Modelling.R").
## Load data set.
kBaseFileVersion <- "3.5"
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
kColPchSchemes <- c("col_trial_pch_edvid",
                    "col_h100.class",
                    "col_SI.h100.class")
kLtyVec <- 1
kLwdVec <- 2
kColVecAll <- c("#92de6b", "#543090", "#d2c440", "#b75fc3", "#4ca23a", "#d14b8f", "#64e99e", "#ce465a", "#6ee9d9", "#c95730", "#4fb9d2", "#e19a3a", "#7175d2", "#cae176", "#402c63", "#86993b", "#80325d", "#5fb574", "#d090c4", "#365a1e", "#638dc8", "#ae8039", "#55bea0", "#7a3126", "#b0d89d", "#d88674", "#42845a", "#d9c481", "#716026")  ## Generated at "http://tools.medialab.sciences-po.fr/iwanthue/" with "H 0 360", "C 25 75", and "L 0 100".
kPchVecAll <- c(21:25, 10)
points.lines.settings <- vector(mode = "list")
for (cur.colpch.scheme in kColPchSchemes) {
    for (cur.species.name in c("beech", "spruce")) {
        ## Create "points.lines.settings[["col_trial_pch_edvid"]][[SPECIES]]".
        if (cur.colpch.scheme == "col_trial_pch_edvid") {
            ## Assign current "bart.SPECIES.clean.1.0" to "cur.bart.species.clean.1.0". We’re using "bart.SPECIES.clean.1.0" here, because we want the points and lines setting to be the same for all data frames for color and point scheme "col_trial_pch_edvid".
            cur.bart.species.clean.1.0 <- get(x = paste0("bart.", cur.species.name, ".clean.1.0"))
            ## Count the number each trial appears in "cur.bart.species.clean.1.0".
            trials.counts <- table(cur.bart.species.clean.1.0[["trial"]])
            cur.pch.vec.selection <- NULL
            cur.col.vec.selection <- NULL
            ## Create a template for the data frame meant to contain all information necessary for plotting.
            nrows <- nrow(x = cur.bart.species.clean.1.0)
            cur.points.lines.settings.df <- data.frame("trial" = rep(x = NA, times = nrows),
                                                       "edvid" = rep(x = NA, times = nrows),
                                                       "auf" = rep(x = NA, times = nrows),
                                                       "col" = rep(x = NA, times = nrows),
                                                       "pch" = rep(x = NA, times = nrows),
                                                       "lty" = rep(x = NA, times = nrows),
                                                       "lwd" = rep(x = NA, times = nrows),
                                                       stringsAsFactors = FALSE)
            ## Assign "trial", "edvid", and "auf" to data frame.
            cur.points.lines.settings.df[["trial"]] <- cur.bart.species.clean.1.0[["trial"]]
            cur.points.lines.settings.df[["edvid"]] <- cur.bart.species.clean.1.0[["edvid"]]
            cur.points.lines.settings.df[["auf"]] <- cur.bart.species.clean.1.0[["auf"]]
            ## Create a vector containing the colors (one color per "trial").
            col.vec <- vector(mode = "character")
            for (cur.trial.nr in seq_len(length.out = length(x = trials.counts))) {
                col.vec <- c(col.vec,
                             rep(x = kColVecAll[cur.trial.nr],
                                 times = trials.counts[cur.trial.nr]))
            }
            ## Assign colors vector to data frame.
            cur.points.lines.settings.df[["col"]] <- col.vec
            ## Create a vector containing the point characters (one point character per "edvid" per "trial").
            pch.vec <- vector(mode = "numeric")
            for (cur.trial in names(x = trials.counts)) {
                cur.trial.subset <- droplevels(x = cur.bart.species.clean.1.0[cur.bart.species.clean.1.0[["trial"]] == cur.trial, ])
                cur.edvids <- cur.trial.subset[["edvid"]]
                cur.edvids.counts <- table(cur.edvids)
                for (cur.edvid in levels(x = cur.edvids)) {
                    cur.edvid.nr <- which(x = levels(x  = cur.edvids) == cur.edvid)
                    cur.pch.vec <- rep(x = kPchVecAll[cur.edvid.nr],
                                       times = cur.edvids.counts[[cur.edvid]])
                    pch.vec <- c(pch.vec,
                                 cur.pch.vec)
                }}
            cur.points.lines.settings.df[["pch"]] <- pch.vec
        }}}  ## TESTING
            for (cur.element in seq_len(length.out = length(x = trials.counts))) {
                cur.counts <- trials.counts[cur.element]
                cur.pch.vec.selection <- c(cur.pch.vec.selection,
                                           kPchVecAll[1:cur.counts])
                cur.col.vec.selection <- c(cur.col.vec.selection,
                                           rep(x = kColVecAll[cur.element],
                                               times = cur.counts))
            }
            ## Assign the same values for each "bart.SPECIES.clean.[0-9].[0-9]" data frame to "points.lines.settings[[cur.colpch.scheme]][[cur.species.name]]" (in order to ease accessing the information later).
            for (cur.data.frame.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name, ".clean"), x = ls(), fixed = FALSE)]) {
                points.lines.settings[[cur.colpch.scheme]][[cur.species.name]][[cur.data.frame.name]] <- data.frame(
                    "trial" = cur.bart.species.clean.1.0[["trial"]],
                    "edvid" = cur.bart.species.clean.1.0[["edvid"]],
                    "auf" = cur.bart.species.clean.1.0[["auf"]],
                    "col" = cur.col.vec.selection,
                    "pch" = cur.pch.vec.selection,
                    "lty" = kLtyVec,
                    "lwd" = kLwdVec,
                    stringsAsFactors = FALSE)
            }}}
    if (cur.colpch.scheme == "col_h100.class" || cur.colpch.scheme == "col_SI.h100.class" ) {
        
        }}
        
## Create list containing the information necessary to create the respective plot, namely (order may be arbitrary):
## - list name: name of the column containing the x values and name of the column containing the y values in this format: XCOL_YCOL
## - "kPlotXLab": x axis label
## - "kPlotYLab": y axis label
kPlottingInformation <- list(
    "alt_ekl" = list("kPlotXLab" = "alt [a]",
                     "kPlotYLab" = "ekl"),
    "alt_gha" = list("kPlotXLab" = "alt [a]",
                     "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
    "dg_gha" = list("kPlotXLab" = "dg [cm]",
                    "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
    "alt_ghaa.cum" = list("kPlotXLab" = "alt [a]",
                          "kPlotYLab" = expression("ghaa.cum [m"^2*" ha"^-1*"]")),
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
    "gha_SI.h100" = list("kPlotXLab" = expression("gha [m"^2*" ha"^-1*"]"),
                         "kPlotYLab" = "SI.h100 [m]"),
    "ln.dg_ln.nha" = list("kPlotXLab" = "ln.dg",
                          "kPlotYLab" = "ln.nha"),
    "log.dg_log.nha" = list("kPlotXLab" = "log.dg",
                            "kPlotYLab" = "log.nha"),
    "h100_h100.diff.EKL.I" = list("kPlotXLab" = "h100 [m]",
                                  "kPlotYLab" = "h100.diff.EKL.I [m]"),
    "ghaa.cum_gha" = list("kPlotXLab" = expression("ghaa.cum [m"^2*" ha"^-1*"]"),
                          "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")))
## Set flag to determine whether the newly created .pdf file should be opened.
kOpenPdf <- FALSE
## kOpenPdf <- TRUE
## Initiate "for" loops.
for (cur.colpch.scheme in 
for (cur.species.name in c("beech", "spruce")) {
    for (cur.data.source.name in ls()[grepl(pattern = paste0("^bart.", cur.species.name, ".clean"), x = ls(), fixed = FALSE)]) {
        for (cur.list.name in names(x = kPlottingInformation)) {
            ## Turn off graphics device.
            graphics.off()
            ## Extract the necessary information for the current plot from "kPlottingInformation".
            for (cur.object.name in names(x = kPlottingInformation[[cur.list.name]])) {  ## Need to use "for" loop here, because the "*apply" functions seem to drop the name of "X".
                cur.el <- kPlottingInformation[[cur.list.name]][cur.object.name]
                assign(x = names(x = cur.el),
                       value = unlist(x = unname(obj = cur.el)))  ## Need to "unname" the object, because plot seemingly cannot handle named expressions. Need to "unlist" the object, because "plot(log = …)" cannot handle lists.
            }
            ## Create data source.
            cur.data.source <- get(x = cur.data.source.name)
            ## Create vectors containing the actual x and y values.
            x.column.name <- strsplit(x = cur.list.name, split = "_", fixed = TRUE)[[1]][1]
            y.column.name <- strsplit(x = cur.list.name, split = "_", fixed = TRUE)[[1]][2]
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
            graphics.subdir <- paste0("Graphics/measurements/", cur.data.source.name, "/")
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
            cur.points.lines.settings <- get(x = paste0("points.lines.settings.", cur.species.name))
            cur.points.lines.settings <- cur.points.lines.settings[cur.points.lines.settings[["edvid"]] %in% levels(x = cur.data.source[["edvid"]]), ]
            ## Add points to empty plot.
            kCntr <- 1
            for (ts in levels(cur.data.source$edvid)) {
                points(x = x.values[cur.data.source$edvid == ts],
                       y = y.values[cur.data.source$edvid == ts],
                       type = kPointsType,
                       col = cur.points.lines.settings$col[kCntr],
                       bg = cur.points.lines.settings$col[kCntr],
                       pch = cur.points.lines.settings$pch[kCntr],
                       lty = cur.points.lines.settings$lty[kCntr],
                       lwd = cur.points.lines.settings$lwd[kCntr])
                kCntr <- kCntr+1
            }
            ## Add legend.
            legend(x = kLegendX,
                   legend = paste("edvid: ", levels(cur.data.source$edvid)),
                   bg = kLegendBg,
                   col = cur.points.lines.settings$col,
                   pt.bg = cur.points.lines.settings$col,
                   pch = cur.points.lines.settings$pch,
                   lty = cur.points.lines.settings$lty,
                   lwd = cur.points.lines.settings$lwd)
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
                graphics.subdir <- paste0("Graphics/models/GAMLSS/", cur.input.data.source.name, "/")
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
                plot(x = cur.model
                 ## ,xvar = bart.clean$h100  ## To be turned on and off as desired.
                    ,parameters = par("mfrow" = c(2, 2),
                                  "mar" = par("mar") + c(0, 1, 0, 0),
                                  "col.axis" = "black",
                                  "col" = "black",
                                  "col.main" = "black",
                                  "col.lab" = "black",
                                  "pch" = 20,
                                  "cex" = 1.00,
                                  "cex.lab" = 1.00,
                                  "cex.axis" = 1,
                                  "cex.main" = 1.5)  ## Settings inspired by Stasinopoulos et al. (2008), p. 122.
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
