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

########################################################################################
## Plot bart.clean$gha against bart.clean$h100, separately for each bart.clean$edvid. ##
########################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/gha_h100.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 19,
    family = "Times")
## Set plot margins.
par(mar = c(4.1, 4.2, 1.5, 0.1))  ## As small as possible using fractions of lines.
## par(mar = c(5, 5, 2, 1))  ## As small as possible using whole lines.
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "h100 [m]",
     ylab = expression("gha [m"^2*" ha"^-1*"]"),
     xlim = c(range(bart.clean$h100, na.rm = TRUE)[1],range(bart.clean$h100, na.rm = TRUE)[2]+5),
     ylim = range(bart.clean$gha, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     main = expression(bold("art == 511, ksha.rel >= 0.7, gha.rel.cha >= 0")))
grid(col = "black",
     lwd = 2)
## Create data frame containing combinations of col and pch.
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta")
kPchVec <- c(21:25)
kLtyVec <- 1
kLwdVec <- 2
kPlotSettingsDataFrame <- data.frame()
for (col in kColVec) {
    kPlotSettingsDataFrame <- rbind(kPlotSettingsDataFrame,
                              cbind(col = col,
                                    pch = kPchVec,
                                    lty = kLtyVec,
                                    lwd = kLwdVec),
                              stringsAsFactors = FALSE)
}
kPlotSettingsDataFrame$pch <- as.numeric(kPlotSettingsDataFrame$pch)
kPlotSettingsDataFrame$lty <- as.numeric(kPlotSettingsDataFrame$lty)
kPlotSettingsDataFrame$lwd <- as.numeric(kPlotSettingsDataFrame$lwd)
kCntr <- 1
## Add points and lines to empty plot.
for (ts in levels(bart.clean$edvid)) {
    points(x = bart.clean$h100[bart.clean$edvid == ts],
           y = bart.clean$gha[bart.clean$edvid == ts],
           type = "b",
           col = kPlotSettingsDataFrame$col[kCntr],
           bg = kPlotSettingsDataFrame$col[kCntr],
           pch = kPlotSettingsDataFrame$pch[kCntr],
           lty = kPlotSettingsDataFrame$lty[kCntr],
           lwd = kPlotSettingsDataFrame$lwd[kCntr])
    kCntr <- kCntr+1
}
## Add legend.
legend(x = "topright",
       legend = paste("edvid: ", levels(bart.clean$edvid)),
       bg = "slategray1",
       col = kPlotSettingsDataFrame$col,
       pt.bg = kPlotSettingsDataFrame$col,
       pch = kPlotSettingsDataFrame$pch,
       lty = kPlotSettingsDataFrame$lty,
       lwd = kPlotSettingsDataFrame$lwd)
graphics.off()
## Open .pdf file via mupdf.
system2(command = "mupdf",
        args = "-r 64 Graphics/gha_h100.pdf",
        wait = FALSE)

#######################################################################################
## Plot bart.clean$ekl against bart.clean$alt, separately for each bart.clean$edvid. ##
#######################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/ekl_alt.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 19,
    family = "Times")
## Set plot margins.
par(mar = c(4.1, 4.2, 1.5, 0.1))  ## As small as possible using fractions of lines.
## par(mar = c(5, 5, 2, 1))  ## As small as possible using whole lines.
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = "ekl",
     xlim = c(range(bart.clean$alt, na.rm = TRUE)[1],range(bart.clean$alt, na.rm = TRUE)[2]+20),  ## accounts for extra space for placing the legend.
     ylim = range(bart.clean$ekl, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     ## main = "spruce only")
     main = expression(bold("art == 511, ksha.rel >= 0.7, gha.rel.cha >= 0")))
grid(col = "black",
     lwd = 2)
## Create data frame containing combinations of col and pch.
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta")
kPchVec <- c(21:25)
kLtyVec <- 1
kLwdVec <- 2
kPlotSettingsDataFrame <- data.frame()
for (col in kColVec) {
    kPlotSettingsDataFrame <- rbind(kPlotSettingsDataFrame,
                              cbind(col = col,
                                    pch = kPchVec,
                                    lty = kLtyVec,
                                    lwd = kLwdVec),
                              stringsAsFactors = FALSE)
}
kPlotSettingsDataFrame$pch <- as.numeric(kPlotSettingsDataFrame$pch)
kPlotSettingsDataFrame$lty <- as.numeric(kPlotSettingsDataFrame$lty)
kPlotSettingsDataFrame$lwd <- as.numeric(kPlotSettingsDataFrame$lwd)
kCntr <- 1
## Add points to empty plot.
for (ts in levels(bart.clean$edvid)) {
    points(x = bart.clean$alt[bart.clean$edvid == ts],
           y = bart.clean$ekl[bart.clean$edvid == ts],
           type = "b",
           col = kPlotSettingsDataFrame$col[kCntr],
           bg = kPlotSettingsDataFrame$col[kCntr],
           pch = kPlotSettingsDataFrame$pch[kCntr],
           lty = kPlotSettingsDataFrame$lty[kCntr],
           lwd = kPlotSettingsDataFrame$lwd[kCntr])
    kCntr <- kCntr+1
}
## Add legend.
legend(x = "topright",
       legend = paste("edvid: ", levels(bart.clean$edvid)),
       bg = "slategray1",
       col = kPlotSettingsDataFrame$col,
       pt.bg = kPlotSettingsDataFrame$col,
       pch = kPlotSettingsDataFrame$pch,
       lty = kPlotSettingsDataFrame$lty,
       lwd = kPlotSettingsDataFrame$lwd)
graphics.off()
## Open .pdf file via mupdf.
system2(command = "mupdf",
        args = "-r 64 Graphics/ekl_alt.pdf",
        wait = FALSE)

#######################################################################################
## Plot bart.clean$gha against bart.clean$alt, separately for each bart.clean$edvid. ##
#######################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/gha_alt.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 19,
    family = "Times")
## Set plot margins.
par(mar = c(4.1, 4.2, 1.5, 0.1))  ## As small as possible using fractions of lines.
## par(mar = c(5, 5, 2, 1))  ## As small as possible using whole lines.
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = expression("gha [m"^2*"]"),
     xlim = c(range(bart.clean$alt, na.rm = TRUE)[1],range(bart.clean$alt, na.rm = TRUE)[2]+20),  ## accounts for extra space for placing the legend.
     ylim = range(bart.clean$gha, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     main = expression(bold("art == 511, ksha.rel >= 0.7, gha.rel.cha >= 0")))
grid(col = "black",
     lwd = 2)
## Create data frame containing combinations of col, pch, lty, and lwd.
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta")
kPchVec <- c(21:25)
kLtyVec <- 1
kLwdVec <- 2
kPlotSettingsDataFrame <- data.frame()
for (col in kColVec) {
    kPlotSettingsDataFrame <- rbind(kPlotSettingsDataFrame,
                                    cbind(col = col,
                                          pch = kPchVec,
                                          lty = kLtyVec,
                                          lwd = kLwdVec),
                                    stringsAsFactors = FALSE)
}
kPlotSettingsDataFrame$pch <- as.numeric(kPlotSettingsDataFrame$pch)
kPlotSettingsDataFrame$lty <- as.numeric(kPlotSettingsDataFrame$lty)
kPlotSettingsDataFrame$lwd <- as.numeric(kPlotSettingsDataFrame$lwd)
kCntr <- 1
## Add points and lines to empty plot.
for (ts in levels(bart.clean$edvid)) {
    points(x = bart.clean$alt[bart.clean$edvid == ts],
           y = bart.clean$gha[bart.clean$edvid == ts],
           type = "b",
           col = kPlotSettingsDataFrame$col[kCntr],
           bg = kPlotSettingsDataFrame$col[kCntr],
           pch = kPlotSettingsDataFrame$pch[kCntr],
           lty = kPlotSettingsDataFrame$lty[kCntr],
           lwd = kPlotSettingsDataFrame$lwd[kCntr])
    kCntr <- kCntr+1
}
## Add legend.
legend(x = "topright",
       legend = paste("edvid: ", levels(bart.clean$edvid)),
       bg = "slategray1",
       col = kPlotSettingsDataFrame$col,
       pt.bg = kPlotSettingsDataFrame$col,
       pch = kPlotSettingsDataFrame$pch,
       lty = kPlotSettingsDataFrame$lty,
       lwd = kPlotSettingsDataFrame$lwd)
graphics.off()
## Open .pdf file via mupdf.
system2(command = "mupdf",
        args = "-r 64 Graphics/gha_alt.pdf",
        wait = FALSE)

###################################################################################################################
## Plot max(bart$gha) against bart$alt, separately for each bart$edvid while excluding invalid data (see below). ##
###################################################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/gmax_alt.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 19,
    family = "Times")
## Set plot margins.
par(mar = c(4.1, 4.2, 1.5, 0.1))  ## As small as possible using fractions of lines.
## par(mar = c(5, 5, 2, 1))  ## As small as possible using whole lines.
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = expression("gha"[max]*"[m"^2*" ha"^-1*"]"),
     xlim = c(range(bart.clean$alt, na.rm = TRUE)[1],range(bart.clean$alt, na.rm = TRUE)[2]+20),  ## accounts for extra space for placing the legend.
     ylim = range(bart.clean$gha, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     main = expression(bold("art == 511, ksha.rel >= 0.7, gha.rel.cha >= 0")))
grid(col = "black",
     lwd = 2)
## Create data frame containing combinations of col, pch, lty, and lwd.
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta")
kPchVec <- c(21:25)
kLtyVec <- 1
kLwdVec <- 2
kPlotSettingsDataFrame <- data.frame()
for (col in kColVec) {
    kPlotSettingsDataFrame <- rbind(kPlotSettingsDataFrame,
                                    cbind(col = col,
                                          pch = kPchVec,
                                          lty = kLtyVec,
                                          lwd = kLwdVec),
                                    stringsAsFactors = FALSE)
}
kPlotSettingsDataFrame$pch <- as.numeric(kPlotSettingsDataFrame$pch)
kPlotSettingsDataFrame$lty <- as.numeric(kPlotSettingsDataFrame$lty)
kPlotSettingsDataFrame$lwd <- as.numeric(kPlotSettingsDataFrame$lwd)
kCntr <- 1
## Add points and lines to empty plot.
for (ts in levels(bart.clean$edvid)) {
    y.vec <- max(bart.clean$gha[bart.clean$edvid == ts], na.rm = TRUE)
    x.vec <- bart.clean$alt[bart.clean$edvid == ts & bart.clean$gha == y.vec]
    points(x = x.vec,
           y = y.vec,
           type = "b",
           col = kPlotSettingsDataFrame$col[kCntr],
           bg = kPlotSettingsDataFrame$col[kCntr],
           pch = kPlotSettingsDataFrame$pch[kCntr],
           lty = kPlotSettingsDataFrame$lty[kCntr],
           lwd = kPlotSettingsDataFrame$lwd[kCntr])
    kCntr <- kCntr+1
}
## Add legend.
legend(x = "topright",
       legend = paste("edvid: ", levels(bart.clean$edvid)),
       bg = "slategray1",
       col = kPlotSettingsDataFrame$col,
       pt.bg = kPlotSettingsDataFrame$col,
       pch = kPlotSettingsDataFrame$pch,
       lty = kPlotSettingsDataFrame$lty,
       lwd = kPlotSettingsDataFrame$lwd)
graphics.off()
## Open .pdf file via mupdf.
system2(command = "mupdf",
        args = "-r 64 Graphics/gmax_alt.pdf",
        wait = FALSE)

#######################################################################################
## Plot bart.clean$SI.h100 against bart.clean$alt, separately for each bart.clean$edvid. ##
#######################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/SI.h100_alt.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 19,
    family = "Times")
## Set plot margins.
par(mar = c(4.1, 4.2, 1.5, 0.1))  ## As small as possible using fractions of lines.
## par(mar = c(5, 5, 2, 1))  ## As small as possible using whole lines.
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = "SI.h100 [m]",
     xlim = c(range(bart.clean$alt, na.rm = TRUE)[1],range(bart.clean$alt, na.rm = TRUE)[2]+20),  ## accounts for extra space for placing the legend.
     ylim = range(bart.clean$SI.h100, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     ## main = "spruce only")
     main = expression(bold("art == 511, ksha.rel >= 0.7, gha.rel.cha >= 0")))
grid(col = "black",
     lwd = 2)
## Create data frame containing combinations of col and pch.
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta")
kPchVec <- c(21:25)
kLtyVec <- 1
kLwdVec <- 2
kPlotSettingsDataFrame <- data.frame()
for (col in kColVec) {
    kPlotSettingsDataFrame <- rbind(kPlotSettingsDataFrame,
                              cbind(col = col,
                                    pch = kPchVec,
                                    lty = kLtyVec,
                                    lwd = kLwdVec),
                              stringsAsFactors = FALSE)
}
kPlotSettingsDataFrame$pch <- as.numeric(kPlotSettingsDataFrame$pch)
kPlotSettingsDataFrame$lty <- as.numeric(kPlotSettingsDataFrame$lty)
kPlotSettingsDataFrame$lwd <- as.numeric(kPlotSettingsDataFrame$lwd)
kCntr <- 1
## Add points to empty plot.
for (ts in levels(bart.clean$edvid)) {
    points(x = bart.clean$alt[bart.clean$edvid == ts],
           y = bart.clean$SI.h100[bart.clean$edvid == ts],
           type = "b",
           col = kPlotSettingsDataFrame$col[kCntr],
           bg = kPlotSettingsDataFrame$col[kCntr],
           pch = kPlotSettingsDataFrame$pch[kCntr],
           lty = kPlotSettingsDataFrame$lty[kCntr],
           lwd = kPlotSettingsDataFrame$lwd[kCntr])
    kCntr <- kCntr+1
}
## Add legend.
legend(x = "topright",
       legend = paste("edvid: ", levels(bart.clean$edvid)),
       bg = "slategray1",
       col = kPlotSettingsDataFrame$col,
       pt.bg = kPlotSettingsDataFrame$col,
       pch = kPlotSettingsDataFrame$pch,
       lty = kPlotSettingsDataFrame$lty,
       lwd = kPlotSettingsDataFrame$lwd)
graphics.off()
## Open .pdf file via mupdf.
system2(command = "mupdf",
        args = "-r 64 Graphics/SI.h100_alt.pdf",
        wait = FALSE)
