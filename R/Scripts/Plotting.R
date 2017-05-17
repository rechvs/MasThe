###############
## Preamble. ##
###############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## Load base file.
kBaseFileVersion <- "1.9"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)

## Tree species according to Wördehoff (2016).
## 110 = Eiche
## 211 = Buche
## 511 = Fichte
## 611 = Douglasie
## 711 = Kiefer

############################
## Plot various relations ##
############################
## Plotting preamble.
kPdfWidth <- 30
kPdfHeight <- kPdfWidth * 0.625
kPdfPointSize <- 19
kPdfFamily <- "Times"
kPlotMargins <- c(4.1, 4.2, 1.5, 0.1)  ## As small as possible using fractions of lines.
kPointsType <- "b"
kXAxs <- "r"
kYAxs <- "r"
kGridCol <- "black"
kGridLwd <- 2
kPchVec <- c(21:25)
kColVec <- c(vapply(X = c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta"),
                    FUN.VALUE = vector(mode = "character", length = length(x = kPchVec)),
                    FUN = function(col) { rep(x = col, times = length(kPchVec)) }))
kLtyVec <- 1
kLwdVec <- 2
kPointsLinesSettings <- data.frame("col" = kColVec,
                                   "pch" = kPchVec,
                                   "lty" = kLtyVec,
                                   "lwd" = kLwdVec,
                                   stringsAsFactors = FALSE)
## kPlotMargins <- c(5, 5, 2, 1)  ## As small as possible using whole lines.
## Create list containing the information necessary to create the respective plot, namely:
## - source of the x values
## - source of the y values
## - x axis label
## - y axis label
## - main plot title
kPlottingInformation <- list("plot1" = list("bart.clean$h100", "bart.clean$gha", "h100 [m]", expression("gha [m"^2*"ha"^-1*"]"), "data = bart.clean"),
                             "plot2" = list("bart.clean$alt", "bart.clean$ekl", "alt [a]", "ekl", "data = bart.clean"),
                             "plot3" = list("bart.clean$alt", "bart.clean$gha", "alt [a]", expression("gha [m"^2*"ha"^-1*"]"), "data = bart.clean"),
                             "plot4" = list("bart.clean$alt", "bart.clean$SI.h100", "alt [a]", "SI.h100 [m]", "data = bart.clean"))
## Set flag to determine whether the newly created .pdf file should be opened.
open.pdf <- FALSE
open.pdf <- TRUE
## Initiate "for" loop.
for (cur.list in names(x = kPlottingInformation)) {
    graphics.off()
    ## Extract the necessary information for the current plot from "kPlottingInformation".
    x.source <- unlist(kPlottingInformation[[cur.list]][1])
    y.source <- unlist(kPlottingInformation[[cur.list]][2])
    x.label <- unlist(kPlottingInformation[[cur.list]][3])
    y.label <- unlist(kPlottingInformation[[cur.list]][4])
    main. <- kPlottingInformation[[cur.list]][5]
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
    legend(x = "topright",
           legend = paste("edvid: ", levels(bart.clean$edvid)),
           bg = "slategray1",
           col = kPointsLinesSettings$col,
           pt.bg = kPointsLinesSettings$col,
           pch = kPointsLinesSettings$pch,
           lty = kPointsLinesSettings$lty,
           lwd = kPointsLinesSettings$lwd)
    graphics.off()
    ## Open .pdf file via mupdf.
    if (open.pdf) {
        system2(command = "mupdf",
                args = paste0("-r 64 ",
                              file.name),
                wait = FALSE)
    }
}
