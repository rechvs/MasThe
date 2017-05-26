##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kBaseFileVersion <- "2.5"
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
kXAxs <- "r"
kYAxs <- "r"
kGridCol <- "black"
kGridLwd <- 2
kPchVec <- c(21:25)
kColVec <- c(vapply(X = c("black", "green", "red", "purple", "cyan", "darkorange", "burlywood", "dimgray", "yellow4", "magenta"),
                    FUN.VALUE = vector(mode = "character", length = length(x = kPchVec)),
                    FUN = function(col) { rep(x = col, times = length(kPchVec)) }))
kLtyVec <- 1
kLwdVec <- 2
kPointsLinesSettings <- data.frame("col" = kColVec,
                                   "pch" = kPchVec,
                                   "lty" = kLtyVec,
                                   "lwd" = kLwdVec,
                                   stringsAsFactors = FALSE)
kLegendX <- "topright"
kLegendBg <- "slategray1"
## Create list containing the information necessary to create the respective plot, namely (order may be arbitrary):
## - "x.source": source of the x values
## - "y.source": source of the y values
## - "main.": main plot title
## - "x.label": x axis label
## - "y.label": y axis label
kPlottingInformation <- list("h100_gha" = list("x.source" = "bart.clean$h100",
                                               "y.source" = "bart.clean$gha",
                                               "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                               "x.label" = "h100 [m]",
                                               "y.label" = expression("gha [m"^2*" ha"^-1*"]")),
                             "alt_ekl" = list("x.source" = "bart.clean$alt",
                                              "y.source" = "bart.clean$ekl",
                                              "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                              "x.label" = "alt [a]",
                                              "y.label" = "ekl"),
                             "alt_gha" = list("x.source" = "bart.clean$alt",
                                              "y.source" = "bart.clean$gha",
                                              "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                              "x.label" = "alt [a]",
                                              "y.label" = expression("gha [m"^2*" ha"^-1*"]")),
                             "alt_SI.h100" = list("x.source" = "bart.clean$alt",
                                                  "y.source" = "bart.clean$SI.h100",
                                                  "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                                  "x.label" = "alt [a]",
                                                  "y.label" = "SI.h100 [m]"),
                             "ln.dg_ln.nha" = list("x.source" = "bart.clean$ln.dg",
                                                   "y.source" = "bart.clean$ln.nha",
                                                   "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                                   "x.label" = "ln.dg",
                                                   "y.label" = "ln.nha"),
                             "log.dg_log.nha" = list("x.source" = "bart.clean$log.dg",
                                                     "y.source" = "bart.clean$log.nha",
                                                     "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                                     "x.label" = "log.dg",
                                                     "y.label" = "log.nha"),
                             "alt_ksha" = list("x.source" = "bart.clean$alt",
                                               "y.source" = "bart.clean$ksha",
                                               "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                               "x.label" = "alt [a]",
                                               "y.label" = expression("ksha [m"^2*" ha"^-1*"]")),
                             "h100_ksha" = list("x.source" = "bart.clean$h100",
                                                "y.source" = "bart.clean$ksha",
                                                "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                                "x.label" = "h100 [m]",
                                                "y.label" = expression("ksha [m"^2*" ha"^-1*"]")),
                             "h100_h100.diff.EKL.I" = list("x.source" = "bart.clean$h100",
                                                           "y.source" = "bart.clean$h100.diff.EKL.I",
                                                           "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                                           "x.label" = "h100 [m]",
                                                           "y.label" = "h100.diff.EKL.I [m]"),
                             "gha_ksha" = list("x.source" = "bart.clean$gha",
                                               "y.source" = "bart.clean$ksha",
                                               "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                               "x.label" = expression("gha [m"^2*" ha"^-1*"]"),
                                               "y.label" = expression("ksha [m"^2*" ha"^-1*"]")),
                             "gha_ksha.clean" = list("x.source" = "bart.clean$gha",
                                                     "y.source" = "bart.clean$ksha.clean",
                                                     "main." = "data = bart.clean (art == 511, ksha.rel >= 0.7, gha.rel.cha >= -0.05)",
                                                     "x.label" = expression("gha [m"^2*" ha"^-1*"]"),
                                                     "y.label" = expression("ksha.clean [m"^2*" ha"^-1*"]")))
## Set flag to determine whether the newly created .pdf file should be opened.
open.pdf <- FALSE
## open.pdf <- TRUE
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
    x.values <- eval(expr = parse(text = x.source))
    y.values <- eval(expr = parse(text = y.source))
    ## Calculate numerical values necessary for creating the plot.
    x.lim.low <- range(x.values, na.rm = TRUE)[1]
    x.lim.high <- range(x.values, na.rm = TRUE)[2] + diff(x = range(x.values, na.rm = TRUE)) * 0.15  ## accounts for extra space for placing the legend.
    x.lim <- c(x.lim.low, x.lim.high)
    y.lim.low <- range(y.values, na.rm = TRUE)[1]
    y.lim.high <- range(y.values, na.rm = TRUE)[2]
    y.lim <- c(y.lim.low, y.lim.high)
    ## Create file name.
    file.name <- gsub(pattern = "[$]",
                      replacement = ".",
                      x = paste0("Graphics/",
                                 x.source,
                                 "_",
                                 y.source,
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
         xlab = x.label,
         ylab = y.label,
         xlim = x.lim,
         ylim = y.lim,
         xaxs = kXAxs,
         yaxs = kYAxs,
         main = main.)
    grid(col = kGridCol,
         lwd = kGridLwd)
    ## Add points to empty plot.
    kCntr <- 1
    for (ts in levels(bart.clean$edvid)) {
        points(x = x.values[bart.clean$edvid == ts],
               y = y.values[bart.clean$edvid == ts],
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
           legend = paste("edvid: ", levels(bart.clean$edvid)),
           bg = kLegendBg,
           col = kPointsLinesSettings$col,
           pt.bg = kPointsLinesSettings$col,
           pch = kPointsLinesSettings$pch,
           lty = kPointsLinesSettings$lty,
           lwd = kPointsLinesSettings$lwd)
    ## Turn off graphics device.
    graphics.off()
    ## If desired, open .pdf file via mupdf.
    if (open.pdf) {
        system2(command = "mupdf",
                args = paste0("-r 64 ",
                              file.name),
                wait = FALSE)
    }
}

#################
## Plot models ##
#################
## Plotting preamble.
{sink(file = "/dev/null"); source(file = "R/Scripts/Modelling.R"); sink()}  ## Evaluate models. The models should end up in list "models" (see "~/laptop02_MasAr/R/Scripts/Modelling.R").
kPdfWidth <- 30
kPdfHeight <- kPdfWidth * 0.625
kPdfPointSize <- 19
kPdfFamily <- "Times"
kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
kPointsType <- "b"
kXAxs <- "r"
kYAxs <- "r"
kGridCol <- "black"
kGridLwd <- 2
## Set flag to determine whether the newly created .pdf file should be opened.
open.pdf <- FALSE
## open.pdf <- TRUE
## Initiate "for" loop.
for (cur.model.name in names(x = models)) {
    ## Turn off graphics device.
    graphics.off()
    ## Create file name.
    file.name <- gsub(pattern = "[$]",
                      replacement = ".",
                      x = paste0("Graphics/",
                                 cur.model.name,
                                 ".pdf"))
    ## Start graphics device driver for producing PDF graphics.
    pdf(file = file.name,
        width = kPdfWidth,
        height = kPdfHeight,
        pointsize = kPdfPointSize,
        family = kPdfFamily)
    ## Set plot margins.
    par(mar = kPlotMargins)
    ## Plot model.
    cur.model <- models[[cur.model.name]]
    plot.gam(x = cur.model)
    ## Turn off graphics device.
    graphics.off()
    ## If desired, open .pdf file via mupdf.
    if (open.pdf) {
        system2(command = "mupdf",
                args = paste0("-r 64 ",
                              file.name),
                wait = FALSE)
    }
}
