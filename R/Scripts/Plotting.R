##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## {sink(file = "/dev/null"); source(file = "R/Scripts/Modelling.R"); sink()}  ## Evaluate models. The models should end up in list "models" (see "~/laptop02_MasAr/R/Scripts/Modelling.R").
## Load data set.
kBaseFileVersion <- "3.0"
kBaseFileName <- paste0(kDataDir, "gmax_", kBaseFileVersion, ".RData")
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
## Create "kPointsLinesSettings".
kLtyVec <- 1
kLwdVec <- 2
## kColVecAll <- c("black", "green", "red", "purple", "cyan", "darkorange", "burlywood", "dimgray", "yellow4", "magenta", "azure", "darkkhaki", "darkslategray", "darkblue", "darksalmon", "greenyellow", "forestgreen", "maroon", "orchid", "peachpuff", "pink", "powderblue", "peru", "plum" , "wheat", "royalblue", "springgreen", "tan")
kColVecAll <- c("#630053", "#50cb56", "#8e62e3", "#008322", "#e167e0", "#84ffbc", "#e32b4a", "#37558f", "#0263d9", "#e39305", "#01418a", "#f18022", "#029ba6", "#810012", "#ecffa2", "#ffaaff", "#6b7400", "#c5c4ff", "#945a00", "#ffadc3", "#795e55", "#ff8d94", "#442900", "#ffd584", "#6d1f00", "#c1be8f", "#685f35", "#ffaf77")  ## Generated at "http://tools.medialab.sciences-po.fr/iwanthue/" with "H 0 360", "C 25 75", and "L 0 100".
## Create list of subsetting information based on the following guidelines:
## - names of top hierarchical elements will be used as directory names and must therefore end with a "/"
## - "subsetting.vector" is a logical vector used for the "subset(subset = ...)" call
## - "kPlotMainAppendix" is a character string which will be appended to the main plot title and should therefore start with an appropriate character
kSubsettingInformation <- list(
    "NoSubsetting/" = list("subsetting.vector" = TRUE,
                           "kPlotMainAppendix" = ", subset: no subsetting."),
    "ksha.clean.mean.edvid.ratio_geq_1.00/" = list("subsetting.vector" = bart.clean[["ksha.clean.mean.edvid.ratio"]] >= 1.00,
                                                   "kPlotMainAppendix" = ", subset: ksha.clean.mean.edvid.ratio >= 1.00."))
## Loop over all subsetting information.
for (cur.subsetting.info.name in names(x = kSubsettingInformation)) {
    ## Create data source.
    data.src <- droplevels(x = subset(x = bart.clean,
                                      subset = kSubsettingInformation[[cur.subsetting.info.name]][["subsetting.vector"]]))
    ## Set flag to determine whether parcels belonging to the same trial (trials being distinguishable by the first 3 digits of "edvid") should share the same color (TRUE) or whether a distinct combination of color and point character should be given to each "edvid" (FALSE).
    kColPerTrial <- TRUE
    ## kColPerTrial <- FALSE
    if (kColPerTrial) {
        kPchVecAll <- c(21:25, 10)
        kEdvidSubstr <- substr(x = levels(data.src$edvid), start = 1, stop = 3)
        kEdvidSubstrCounts <- table(kEdvidSubstr)
        ## n.colors <- length(x = unique(x = kEdvidSubstr))  ## Determine required number of colors (not required for script execution).
        ## n.pchs <- max(kEdvidSubstrCounts)  ## Determine maximum number of point characters required (not required for script execution).
        kPchVecSelection <- NULL
        kColVecSelection <- NULL
        for (cur.element in 1:length(kEdvidSubstrCounts)) {
            cur.counts <- kEdvidSubstrCounts[cur.element]
            kPchVecSelection <- c(kPchVecSelection,
                                  kPchVecAll[1:cur.counts])
            kColVecSelection <- c(kColVecSelection,
                                  rep(x = kColVecAll[cur.element],
                                      times = cur.counts))
        }
        kPointsLinesSettings <- data.frame("col" = kColVecSelection,
                                           "pch" = kPchVecSelection,
                                           "lty" = kLtyVec,
                                           "lwd" = kLwdVec,
                                           stringsAsFactors = FALSE)
    } else {
        kPchVec <- c(21:25)
        kColVecSelection <- c(vapply(X = kColVecAll[1:10],
                                     FUN.VALUE = vector(mode = "character", length = length(x = kPchVec)),
                                     FUN = function(col) { rep(x = col, times = length(kPchVec)) }))
        kPointsLinesSettings <- data.frame("col" = kColVecSelection,
                                           "pch" = kPchVec,
                                           "lty" = kLtyVec,
                                           "lwd" = kLwdVec,
                                           stringsAsFactors = FALSE)
    }
    ## Create list containing the information necessary to create the respective plot, namely (order may be arbitrary):
    ## - "kXSource": source of the x values
    ## - "kYSource": source of the y values
    ## - "kPlotMain": main plot title
    ## - "kPlotXLab": x axis label
    ## - "kPlotYLab": y axis label
    kPlottingInformation <- list(
        "h100_gha" = list("kXSource" = "data.src$h100",
                               "kYSource" = "data.src$gha",
                               "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                               "kPlotXLab" = "h100 [m]",
                               "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        "alt_ekl" = list("kXSource" = "data.src$alt",
                              "kYSource" = "data.src$ekl",
                              "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                              "kPlotXLab" = "alt [a]",
                              "kPlotYLab" = "ekl"),
        "alt_gha" = list("kXSource" = "data.src$alt",
                              "kYSource" = "data.src$gha",
                              "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                              "kPlotXLab" = "alt [a]",
                              "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        "alt_SI.h100" = list("kXSource" = "data.src$alt",
                                  "kYSource" = "data.src$SI.h100",
                                  "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                  "kPlotXLab" = "alt [a]",
                                  "kPlotYLab" = "SI.h100 [m]"),
        "ln.dg_ln.nha" = list("kXSource" = "data.src$ln.dg",
                                   "kYSource" = "data.src$ln.nha",
                                   "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                   "kPlotXLab" = "ln.dg",
                                   "kPlotYLab" = "ln.nha"),
        "log.dg_log.nha" = list("kXSource" = "data.src$log.dg",
                                     "kYSource" = "data.src$log.nha",
                                     "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                     "kPlotXLab" = "log.dg",
                                     "kPlotYLab" = "log.nha"),
        "alt_ksha" = list("kXSource" = "data.src$alt",
                               "kYSource" = "data.src$ksha",
                               "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                               "kPlotXLab" = "alt [a]",
                               "kPlotYLab" = expression("ksha [m"^2*" ha"^-1*"]")),
        "h100_ksha" = list("kXSource" = "data.src$h100",
                                "kYSource" = "data.src$ksha",
                                "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                "kPlotXLab" = "h100 [m]",
                                "kPlotYLab" = expression("ksha [m"^2*" ha"^-1*"]")),
        "h100_h100.diff.EKL.I" = list("kXSource" = "data.src$h100",
                                           "kYSource" = "data.src$h100.diff.EKL.I",
                                           "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                           "kPlotXLab" = "h100 [m]",
                                           "kPlotYLab" = "h100.diff.EKL.I [m]"),
        "gha_ksha" = list("kXSource" = "data.src$gha",
                               "kYSource" = "data.src$ksha",
                               "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                               "kPlotXLab" = expression("gha [m"^2*" ha"^-1*"]"),
                               "kPlotYLab" = expression("ksha [m"^2*" ha"^-1*"]")),
        "gha_ksha.clean" = list("kXSource" = "data.src$gha",
                                     "kYSource" = "data.src$ksha.clean",
                                     "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                     "kPlotXLab" = expression("gha [m"^2*" ha"^-1*"]"),
                                     "kPlotYLab" = expression("ksha.clean [m"^2*" ha"^-1*"]")),
        "ghaa.cum_gha" = list("kXSource" = "data.src$ghaa.cum",
                                   "kYSource" = "data.src$gha",
                                   "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, ghaa.cum.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                   "kPlotXLab" = expression("ghaa.cum [m"^2*" ha"^-1*"]"),
                                   "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        "ksha.clean_gha" = list("kXSource" = "data.src$ksha.clean",
                                     "kYSource" = "data.src$gha",
                                     "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, ksha.clean.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                     "kPlotXLab" = expression("ksha.clean [m"^2*" ha"^-1*"]"),
                                     "kPlotYLab" = expression("gha [m"^2*" ha"^-1*"]")),
        "alt_ksha.clean" = list("kXSource" = "data.src$alt",
                                     "kYSource" = "data.src$ksha.clean",
                                     "kPlotMain" = paste0("data = data.src (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)", kSubsettingInformation[[cur.subsetting.info.name]][["kPlotMainAppendix"]]),
                                     "kPlotXLab" = "alt [a]",
                                     "kPlotYLab" = expression("ksha.clean [m"^2*" ha"^-1*"]")))
    ## Set flag to determine whether the newly created .pdf file should be opened.
    kOpenPdf <- FALSE
    ## kOpenPdf <- TRUE
    ## Initiate "for" loop.
    for (cur.list in names(x = kPlottingInformation)) {
        ## Turn off graphics device.
        graphics.off()
        ## Extract the necessary information for the current plot from "kPlottingInformation".
        for (cur.name in names(x = kPlottingInformation[[cur.list]])) {  ## Need to use "for" loop here, because the "*apply" functions seem to drop the name of "X".
            cur.el <- kPlottingInformation[[cur.list]][cur.name]
            assign(x = names(x = cur.el),
                   value = unlist(x = unname(obj = cur.el)))  ## Need to "unname" the object, because plot seemingly cannot handle named expressions. Need to "unlist" the object, because "plot(log = …)" cannot handle lists.
        }
        ## Create vectors containing the actual x and y values.
        x.values <- eval(expr = parse(text = kXSource))
        y.values <- eval(expr = parse(text = kYSource))
        ## Calculate numerical values necessary for creating the plot.
        x.lim.low <- range(x.values, na.rm = TRUE)[1]
        x.lim.high <- range(x.values, na.rm = TRUE)[2] + diff(x = range(x.values, na.rm = TRUE)) * 0.15  ## accounts for extra space for placing the legend.
        x.lim <- c(x.lim.low, x.lim.high)
        y.lim.low <- range(y.values, na.rm = TRUE)[1]
        y.lim.high <- range(y.values, na.rm = TRUE)[2]
        y.lim <- c(y.lim.low, y.lim.high)
        ## If nonexistent, create destination directory.
        kGraphicsDestDir <- paste0("Graphics/meas/",
                                   cur.subsetting.info.name)
        system2(command = "mkdir",
                args = paste0("-p ", kGraphicsDestDir))
        ## Create file name.
        file.name <- gsub(pattern = "[$]",
                          replacement = ".",
                          x = paste0(kGraphicsDestDir,
                                     cur.list,
                                     ".pdf"))
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
             main = kPlotMain)
        grid(col = kGridCol,
             lwd = kGridLwd)
        ## Add points to empty plot.
        kCntr <- 1
        for (ts in levels(data.src$edvid)) {
            points(x = x.values[data.src$edvid == ts],
                   y = y.values[data.src$edvid == ts],
                   type = kPointsType,
                   col = kPointsLinesSettings$col[kCntr],
                   bg = kPointsLinesSettings$col[kCntr],
                   pch = kPointsLinesSettings$pch[kCntr],
                   lty = kPointsLinesSettings$lty[kCntr],
                   lwd = kPointsLinesSettings$lwd[kCntr])
            kCntr <- kCntr+1
        }
        ## Add legend.
        legend(x = kLegendX,
               legend = paste("edvid: ", levels(data.src$edvid)),
               bg = kLegendBg,
               col = kPointsLinesSettings$col,
               pt.bg = kPointsLinesSettings$col,
               pch = kPointsLinesSettings$pch,
               lty = kPointsLinesSettings$lty,
               lwd = kPointsLinesSettings$lwd)
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

#################
## Plot models ##
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
    for (cur.model.name in names(x = models[[cur.function.name]])) {
        ## Extract current model.
        cur.model <- models[[cur.function.name]][[cur.model.name]]
        ## Turn off graphics device.
        graphics.off()
        ## Create file name.
        file.name <- gsub(pattern = "[$]",
                          replacement = ".",
                          x = paste0("Graphics/",
                                     cur.model.name,
                                     ".pdf"))
        if (grepl(pattern = "GAM_", x = cur.model.name, fixe = TRUE)) {  ## If this is true, it means the current model is a GAM and we need to use “mgcv::plot.gam(...)”.
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
        }
        if (grepl(pattern = "GAMLSS_", x = cur.model.name, fixe = TRUE)) {  ## If this is true, it means the current model is a GAMLSS and we need to use “gamlss::plot.gamlss(...)”.
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
    "h100_gha" = list("kPlotMain" = "Measurements and model predictions for gha vs. h100 (data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05))",
                      "kPlotXLabel" = "h100 [m]",
                      "kPlotYLabel" = expression("gha [m"^2*" ha"^-1*"]"),
                      "kNPlots" = 3,
                      "kXSource1" = "bart.clean$h100",
                      "kYSource1" = "bart.clean$gha",
                      "kCoeffsSource2" = "coef(object = models$\"nls2..nls2\"$\"Sterba_Gmax\")",
                      "kCurveExpr2" = "pi/(16 * eval(parse(text = kCoeffsSource2))[[\"a0\"]] * eval(parse(text = kCoeffsSource2))[[\"b0\"]] * (x ^ (eval(parse(text = kCoeffsSource2))[[\"a1\"]] + eval(parse(text = kCoeffsSource2))[[\"b1\"]]))) * 10000",
                      "kCurveExpr3" = "pi/(16 * 4.913256e-06 * 0.3716977 * (x ^ (0.4394706 + -0.9097641))) / 10000",
                      "kLegendLegend" = "c(\"Measurements\", as.expression(x = bquote(expr = \"G\"[max]*\"(h\"[100]*\") predicted using estimated coefficients\")), as.expression(x = bquote(expr = \"G\"[max]*\"(h\"[100]*\") predicted using coefficients from Wördehoff (2016)\")))",
                      "kLegendX" = "topleft",
                      "kPch" = c(1, NA, NA),
                      "kLty" = c(NA, 1, 1),
                      "kCol" = c("black", "red", "blue")),
    "dg_nha" = list("kPlotMain" = "Measurements and model predictions for nha vs. dg (data = bart.clean (art == 511, ksha.rel >= 0.7, nha.rel.cha >= -0.05))",
                    "kPlotXLabel" = "dg [cm]",
                    "kPlotYLabel" = expression("nha [ha"^-1*"]"),
                    "kNPlots" = 3,
                    "kXSource1" = "bart.clean$dg",
                    "kYSource1" = "bart.clean$nha",
                    "kCoeffsSource2" = "coef(object = models$\"nls2..nls2\"$\"Sterba_NGmax\")",
                    "kCurveExpr2" = "eval(expr = parse(text = kCoeffsSource2))[[\"b0\"]] / eval(expr = parse(text = kCoeffsSource2))[[\"a0\"]] * (2 * eval(expr = parse(text = kCoeffsSource2))[[\"b0\"]] * x) ^ (eval(expr = parse(text = kCoeffsSource2))[[\"a1\"]] / eval(expr = parse(text = kCoeffsSource2))[[\"b1\"]] - 1)",
                    "kCurveExpr3" = "1/x * 27000",
                    "kLegendLegend" = "c(\"Measurements\", as.expression(x = bquote(expr = \"N\"[G[max]]*\"(dg\"[G[max]]*\") predicted using estimated coefficients\")), as.expression(x = bquote(expr = frac(1, x)%.%\"27000\")))",
                    "kLegendX" = "topleft",
                    "kPch" = c(1, NA, NA),
                    "kLty" = c(NA, 1, 1),
                    "kCol" = c("black", "red", "blue")),
    "ln.dg_ln.nha" = list("kPlotMain" = "Measurements and model predictions for ln(nha) vs. ln(dg) (data = bart.clean (art == 511, ksha.rel >= 0.7, nha.rel.cha >= -0.05))",
                          "kPlotXLabel" = "ln(dg)",
                          "kPlotYLabel" = "ln(nha)",
                          "kNPlots" = 4,
                          "kXSource1" = "bart.clean$ln.dg",
                          "kYSource1" = "bart.clean$ln.nha",
                          "kCoeffsSource2" = "coef(object = models$\"stats..lm\"$\"LM_ln.nha_ln.dg\")",
                          "kCoeffsSource4" = "coef(object = models$\"stats..lm\"$\"LM_ln.nha_ln.dg_fixed_slope\")",
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
## Initiate "for" loop.
for (cur.list.name in names(x = kPlottingInformation)) {
    ## Turn off graphics device.
    graphics.off()
    ## Extract the necessary information for the current plot from "kPlottingInformation".
    for (cur.el.name in names(x = kPlottingInformation[[cur.list.name]])) {  ## Need to use "for" loop here, because the "*apply" functions seem to drop the name of "X".
        cur.el <- kPlottingInformation[[cur.list.name]][cur.el.name]
        assign(x = names(x = cur.el),
               value = unlist(x = unname(obj = cur.el)))  ## Need to "unname" the object, because plot seemingly cannot handle named expressions. Need to "unlist" the object, because "plot(log = …)" cannot handle lists.
    }
    ## Create file name.
    file.name <- gsub(pattern = "[$]",
                      replacement = ".",
                      x = paste0("Graphics/",
                                 "measmod_",
                                 kXSource1,
                                 "_",
                                 kYSource1,
                                 ".pdf"))
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
                 main = kPlotMain,
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
