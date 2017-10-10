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
kPdfHeight <- kPdfWidth * 0.55
kPdfPointSize <- 90
kPdfFamily <- "Times"
kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
kLineWidth <- 4
kLineType <- "solid"
kPointCharacter <- 20
kXLab <- "log(D)"
kYLab <- "log(N)"
kXMin <- 0.5
kXMin <- list("beech" = 0.8,
              "spruce" = 0.75)
kXMax <- 2
kXMax <- list("beech" = 1.8,
              "spruce" = 1.75)
kYMin <- 1
kYMin <- list("beech" = 1.6,
              "spruce" = 2.4)
kYMax <- 4
kYMax <- list("beech" = 3.6,
              "spruce" = 3.6)
kGridLineType <- "dashed"
kGridLineCol <- "gray"
kGridLineWidth <- 2
kSpeciesNames <- c("beech", "spruce")
kPointLineColors <- list("beech" = "#ee7f00",
                         "spruce" = "#4066aa")
kPlotLayout <- 1:2
## Loop over all species names.
for (cur.species.name in kSpeciesNames) {
    ## Turn off graphics device.
    graphics.off()
    ## Create file name.
    file.name <-paste0(kGraphicsSubdir,
                       cur.species.name,
                       "_logN_logD_Plot_before_after_data_selection.pdf")
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
                        nrow = 1,
                        ncol = 2,
                        byrow = FALSE))
    ## Loop over all plot numbers.
    for (cur.plot.nr in kPlotLayout) {
        ## Extract x- and y-limits for current species.
        cur.xmin <- as.numeric(x = kXMin[names(x = kXMin) == cur.species.name])
        cur.xmax <- as.numeric(x = kXMax[names(x = kXMax) == cur.species.name])
        cur.ymin <- as.numeric(x = kYMin[names(x = kYMin) == cur.species.name])
        cur.ymax <- as.numeric(x = kYMax[names(x = kYMax) == cur.species.name])
        ## Set point and line color for current species.
        cur.point.line.col <- as.character(x = kPointLineColors[names(x = kPointLineColors) == cur.species.name])
        ## Set data frame version for current plot number ("1.0" for plot number 1, "1.8" for plot number 2).
        cur.data.frame.version <- ifelse(test = cur.plot.nr %% 2 != 0,
                                         yes = "1.0",
                                         no = "1.8")
        ## Get data frame for current species and plot number.
        cur.data.frame <- get(x = paste0("bart.", cur.species.name, ".clean.", cur.data.frame.version))
        ## Select title for current plot ("A" for plot number 1, "B" for plot number 2).
        cur.plot.main <- ifelse(test = cur.plot.nr %% 2 != 0,
                                yes = "A",
                                no = "B")
        ## Create empty plot.
        plot(x = 1,
             type = "n",
             xlim = c(cur.xmin, cur.xmax),
             ylim = c(cur.ymin, cur.ymax),
             xlab = kXLab,
             ylab = kYLab,
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
                   lwd = kLineWidth,
                   col = cur.point.line.col)
            ## Add lines to plot per "edvid", with distinct colors per species.
            lines(x = cur.x.values,
                  y = cur.y.values,
                  lty = kLineType,
                  lwd = kLineWidth,
                  col = cur.point.line.col)
            ## Add legend.
            legend(x = "topright",
                   legend = cur.species.name,
                   col = cur.point.line.col,
                   lty = kLineType,
                   pch = kPointCharacter,
                   lwd = kLineWidth,
                   bg = "gray")
        }}
    ## Turn off graphics device.
    graphics.off()
}
## Clean up workspace.
rm(list = setdiff(x = ls(), y = objects.at.script.start))
