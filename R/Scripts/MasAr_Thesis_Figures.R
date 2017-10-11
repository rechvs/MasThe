##############
## Preamble ##
##############
## TEX file: ~/laptop02_MasAr/LaTeX/Thesis/MasAr_Thesis.tex
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## Load data set.
kBaseFileVersion <- "5.3"
kBaseFileName <- paste0(kDataDir, "gmax_merged_", kBaseFileVersion, ".RData")
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
## If nonexistent, create subdirectory in which to store graphics.
kGraphicsSubdir <- "Graphics/Thesis/"
system2(command = "mkdir",
        args = paste0("-p ", kGraphicsSubdir))
objects.at.script.start <- c(ls(), "objects.at.script.start")  ## Required for cleaning up workspace.

#####################################################
## log(N)-log(D)-plots before/after data selection ##
#####################################################
## TEX file: ~/laptop02_MasAr/LaTeX/Thesis/Data_Selection.tex
## This block creates 2 files, one per species. Each file contains 2 plots (in a 1 row, 2 column layout): the left plot for observations before application of the data selection mechanism, the right plot for observations after application of the data selection mechanism.
## Plotting preamble.
kPdfWidth <- (210 - 30 - 20) * 0.3937
## kPdfHeight <- kPdfWidth * 0.55
kPdfHeight <- kPdfWidth
kPdfPointSize <- 90
kPdfFamily <- "Times"
kPlotMargins <- c(3.5, 3.5, 1, 0.5)  ## As small as possible using fractions of lines.
kPlotMgp <- c(2, 1, 0)
kLineWidth <- 6
kLineType <- "solid"
kPointCharacter <- 20
kXLab <- expression(paste("log(", italic("D"), ")"))
kYLab <- expression(paste("log(", italic("N"), ")"))
kXMin <- list("beech" = 0.8,
              "spruce" = 0.8)
kXMax <- list("beech" = 1.8,
              "spruce" = 1.8)
kYMin <- list("beech" = 1.6,
              "spruce" = 2.4 * 0.9)
kYMax <- list("beech" = 3.6,
              "spruce" = 3.6)
kGridLineType <- "dashed"
kGridLineCol <- "gray"
kGridLineWidth <- 2
kSpeciesNames <- c("beech", "spruce")
kPointLineColors <- list("beech" = "#ee7f00",  ## Colors taken from Niedersächsische Landesforsten (2011), fig. 3.
                         "spruce" = "#4066aa")
kPlotLayout <- 1:4
kUpperThresholdLineType <- "dashed"
kLowerThresholdLineType <- "solid"
kThresholdLinesFrom <- 0
kThresholdLinesTo <- 10
kThresholdLinesCol <- "black"
## Loop over all species names.
## for (cur.species.name in kSpeciesNames) {
## Turn off graphics device.
graphics.off()
## Create file name.
file.name <-paste0(kGraphicsSubdir,
                   ## cur.species.name,
                   ## "_logN_logD_Plot_before_after_data_selection.pdf")
                   "logN_logD_Plots_before_after_data_selection.pdf")
## Start graphics device driver for producing PDF graphics.
pdf(file = file.name,
    width = kPdfWidth,
    height = kPdfHeight,
    pointsize = kPdfPointSize,
    family = kPdfFamily)
## Set plot margins.
par("mar" = kPlotMargins)
## Set plot layout to 1 row and 2 columns (1 graphics device per species, 1 plot per data selection state).
layout(mat = matrix(data = kPlotLayout,
                    ## nrow = 1,
                    nrow = 2,
                    ncol = 2,
                    ## byrow = FALSE))
                    byrow = TRUE))
## Loop over all plot numbers.
for (cur.plot.nr in kPlotLayout) {
    ## Set current species name ("beech" for plots number 1 and 2, "spruce" for plots number 3 and 4).
    cur.species.name <- ifelse(test = cur.plot.nr <= 2,
                               yes = "beech",
                               no = "spruce")
    ## Extract x- and y-limits for current species.
    cur.xmin <- as.numeric(x = kXMin[names(x = kXMin) == cur.species.name])
    cur.xmax <- as.numeric(x = kXMax[names(x = kXMax) == cur.species.name])
    cur.ymin <- as.numeric(x = kYMin[names(x = kYMin) == cur.species.name])
    cur.ymax <- as.numeric(x = kYMax[names(x = kYMax) == cur.species.name])
    ## Set point and line color for current species.
    cur.point.line.col <- as.character(x = kPointLineColors[names(x = kPointLineColors) == cur.species.name])
    ## Set data frame version for current plot number ("1.0" for plots number 1 and 3, "1.8" for plots number 2 and 4).
    cur.data.frame.version <- ifelse(test = cur.plot.nr %% 2 != 0,
                                     yes = "1.0",
                                     no = "1.8")
    ## Get data frame for current species and plot number.
    cur.data.frame <- get(x = paste0("bart.", cur.species.name, ".clean.", cur.data.frame.version))
    ## Select column specifier for current plot ("A" for plot number 1, "B" for plot number 2, "" for plots number 3 and 4).
    if (cur.plot.nr <=2) {
        cur.plot.main <- ifelse(test = cur.plot.nr %% 2 != 0,
                                yes = "A",
                                no = "B")
    } else {
        cur.plot.main <- ""
    }
    ## Set x-axis label for current plot ("" for plots number 1 and 2, kXLab for plots number 3 and 4).
    cur.xlab <- ifelse(test = cur.plot.nr <= 2,
                       yes = "",
                       no = kXLab)
    ## Set y-axis label for current plot (kYLab for plots number 1 and 3, "" for plots number 2 and 4).
    cur.ylab <- ifelse(test = cur.plot.nr %% 2 != 0,
                       yes = kYLab,
                       no = "")
    ## Initiate legend components.
    legend.legend <- vector(mode = "character")
    legend.pch <- vector(mode = "numeric")
    legend.lty <- vector(mode = "character")
    legend.lwd <- vector(mode = "numeric")
    legend.col <- vector(mode = "character")
    ## Create empty plot.
    plot(x = 0,
         type = "n",
         xlim = c(cur.xmin, cur.xmax),
         ylim = c(cur.ymin, cur.ymax),
         ## xlab = kXLab,
         xlab = cur.xlab,
         ## ylab = kYLab,
         ylab = cur.ylab,
         xaxs = "i",
         yaxs = "i",
         mgp = kPlotMgp,
         main = cur.plot.main,
         panel.first = abline(v = seq(from = 0,
                                      to = 2,
                                      by = 0.2),
                              h = seq(from = 0,
                                      to = 4,
                                      by = 0.2),
                              lty = kGridLineType,
                              lwd = kGridLineWidth,
                              col = kGridLineCol))
    ## Define function for calculating the lines representing the slope thresholds.
    slope.threshold.func <- function(s, x, k) {
        res <- s * x + k
        return(res)
    }
    ## Add line for upper slope threshold to plot.
    cur.species.upper.slope.threshold <- ifelse(test = cur.species.name == "beech",
                                                yes = -0.9,
                                                no = -0.65)
    cur.species.threshold.func.upper.intercept <- cur.ymax - cur.species.upper.slope.threshold * cur.xmin
    curve(expr = slope.threshold.func(s = cur.species.upper.slope.threshold,
                                      x,
                                      k = cur.species.threshold.func.upper.intercept),
          from = kThresholdLinesFrom,
          to = kThresholdLinesTo,
          lty = kUpperThresholdLineType,
          lwd = kLineWidth,
          col = kThresholdLinesCol,
          add = TRUE)
    ## Update legend components.
    legend.legend <- c(legend.legend, paste0("upper slope threshold ", cur.species.name))
    legend.pch <- c(legend.pch, NA)
    legend.lty <- c(legend.lty, kUpperThresholdLineType)
    legend.lwd <- c(legend.lwd, kLineWidth)
    legend.col <- c(legend.col, kThresholdLinesCol)
    ## Add line for lower slope threshold to plot.
    cur.species.lower.slope.threshold <- ifelse(test = cur.species.name == "beech",
                                                yes = -2.91,
                                                no = -2.82)
    cur.species.threshold.func.lower.intercept <- cur.ymax - cur.species.lower.slope.threshold * cur.xmin
    curve(expr = slope.threshold.func(s = cur.species.lower.slope.threshold,
                                      x,
                                      k = cur.species.threshold.func.lower.intercept),
          from = kThresholdLinesFrom,
          to = kThresholdLinesTo,
          lty = kLowerThresholdLineType,
          lwd = kLineWidth,
          col = kThresholdLinesCol,
          add = TRUE)
    ## Update legend components.
    legend.legend <- c(legend.legend,  paste0("lower slope threshold ", cur.species.name))
    legend.pch <- c(legend.pch, NA)
    legend.lty <- c(legend.lty, kLowerThresholdLineType)
    legend.lwd <- c(legend.lwd, kLineWidth)
    legend.col <- c(legend.col, kThresholdLinesCol)
    ## Loop over all "edvid"s.
    for (cur.edvid.name in levels(x = cur.data.frame[["edvid"]])) {
        ## Extract x-values for current "edvid".
        cur.x.values <- cur.data.frame[["log.dg"]][cur.data.frame[["edvid"]] == cur.edvid.name]
        ## Extract y-values for current "edvid".
        cur.y.values <- cur.data.frame[["log.nha"]][cur.data.frame[["edvid"]] == cur.edvid.name]
        ## Add points to plot per "edvid", with distinct colors per species.
        points(x = cur.x.values,
               y = cur.y.values,
               type = "p",
               pch = kPointCharacter,
               lty = kLineType,
               col = cur.point.line.col)
        ## Add lines to plot per "edvid", with distinct colors per species.
        lines(x = cur.x.values,
              y = cur.y.values,
              lty = kLineType,
              lwd = kLineWidth,
              col = cur.point.line.col)
    }
    ## Update legend components.
    legend.legend <- c(legend.legend, paste0("observation ", cur.species.name))
    legend.pch <- c(legend.pch, kPointCharacter)
    legend.lty <- c(legend.lty, kLineType)
    legend.lwd <- c(legend.lwd, kLineWidth)
    legend.col <- c(legend.col, cur.point.line.col)
    ## Add legend.
    legend(x = "bottomleft",
           legend = legend.legend,
           col = legend.col,
           lty = legend.lty,
           pch = legend.pch,
           lwd = legend.lwd,
           bg = "gray")
    ## Reset legend components.
    legend.legend <- vector(mode = "character")
    legend.pch <- vector(mode = "numeric")
    legend.lty <- vector(mode = "character")
    legend.lwd <- vector(mode = "numeric")
    legend.col <- vector(mode = "character")
}
## Turn off graphics device.
graphics.off()
## }
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.script.start))
