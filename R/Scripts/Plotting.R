###############
## Preamble. ##
###############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## Load base file.
kBaseFileVersion <- "1.8"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)

## Tree species according to Wördehoff (2016).
## 110 = Eiche
## 211 = Buche
## 511 = Fichte
## 611 = Douglasie
## 711 = Kiefer

##########################################################################################
## Plot bart$gha against bart$h100 for bart$art == 511, separately for each bart$edvid. ##
##########################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/gha_h100.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 18,
    family = "Times")
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "h100 [m]",
     ylab = "gha [m²]",
     xlim = range(bart$h100, na.rm = TRUE),
     ylim = range(bart$gha, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     main = "spruce only")
grid(col = "black",
     lwd = 2)
## Create data frame containing combinations of col and pch.
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta")
kPchVec <- c(21:25)
kPchColDataFrame <- data.frame()
for (col in kColVec) {
    kPchColDataFrame <- rbind(kPchColDataFrame,
                              cbind(col = col,pch = kPchVec),
                              stringsAsFactors = FALSE)
}
kPchColDataFrame$pch <- as.numeric(kPchColDataFrame$pch)
kCntr <- 1
## Add points to empty plot.
for (ts in levels(bart$edvid)) {
    points(x = bart$h100[bart$edvid == ts & bart$art == 511],
           y = bart$gha[bart$edvid == ts & bart$art == 511],
           col = kPchColDataFrame$col[kCntr],
           bg = kPchColDataFrame$col[kCntr],
           pch = kPchColDataFrame$pch[kCntr])
    kCntr <- kCntr+1
}
## Add legend.
legend(x = "topleft",
       legend = paste("edvid: ", levels(bart$edvid)),
       bg = "slategray1",
       col = kPchColDataFrame$col,
       pt.bg = kPchColDataFrame$col,
       pch = kPchColDataFrame$pch,)
graphics.off()
## Open .pdf file via mupdf.
system2(command = "mupdf",
        args = "-r 64 Graphics/gha_h100.pdf",
        wait = FALSE)

#########################################################################################
## Plot bart$ekl against bart$alt for bart$art == 511, separately for each bart$edvid. ##
#########################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/ekl_alt.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 18,
    family = "Times")
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = "ekl",
     xlim = c(range(bart$alt, na.rm = TRUE)[1],range(bart$alt, na.rm = TRUE)[2]+20),  ## accounts for extra space for placing the legend.
     ylim = range(bart$ekl, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     main = "spruce only")
grid(col = "black",
     lwd = 2)
## Create data frame containing combinations of col and pch.
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow4","magenta")
kPchVec <- c(21:25)
kPchColDataFrame <- data.frame()
for (col in kColVec) {
    kPchColDataFrame <- rbind(kPchColDataFrame,
                              cbind(col = col,pch = kPchVec),
                              stringsAsFactors = FALSE)
}
kPchColDataFrame$pch <- as.numeric(kPchColDataFrame$pch)
kCntr <- 1
## Add points to empty plot.
for (ts in levels(bart$edvid)) {
    points(x = bart$alt[bart$edvid == ts & bart$art == 511],
           y = bart$ekl[bart$edvid == ts & bart$art == 511],
           col = kPchColDataFrame$col[kCntr],
           bg = kPchColDataFrame$col[kCntr],
           pch = kPchColDataFrame$pch[kCntr])
    kCntr <- kCntr+1
}
## Add legend.
legend(x = "topright",
       legend = paste("edvid: ", levels(bart$edvid)),
       bg = "slategray1",
       col = kPchColDataFrame$col,
       pt.bg = kPchColDataFrame$col,
       pch = kPchColDataFrame$pch,)
graphics.off()
## Open .pdf file via mupdf.
system2(command = "mupdf",
        args = "-r 64 Graphics/ekl_alt.pdf",
        wait = FALSE)

#########################################################################################
## Plot bart$gha against bart$alt for bart$art == 511, separately for each bart$edvid. ##
#########################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/gha_alt.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 18,
    family = "Times")
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = "gha [m²]",
     xlim = c(range(bart$alt, na.rm = TRUE)[1],range(bart$alt, na.rm = TRUE)[2]+20),  ## accounts for extra space for placing the legend.
     ylim = range(bart$gha, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     main = "spruce only")
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
for (ts in levels(bart$edvid)) {
    points(x = bart$alt[bart$edvid == ts & bart$art == 511],
           y = bart$gha[bart$edvid == ts & bart$art == 511],
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
       legend = paste("edvid: ", levels(bart$edvid)),
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

##############################################################################################################
## Plot bart$gha against bart$alt, separately for each bart$edvid while excluding invalid data (see below). ##
##############################################################################################################
## Create new data frame "bart.clean" from "bart" by excluding invalid data. ##
###############################################################################
## Exclude all lines in which "bart$art != 511".
bart.clean <- droplevels(x = bart[bart$art == 511, ])
## Exclude all lines in which "bart.clean$ksha.rel < 0.7".
bart.clean <- bart.clean[bart.clean$ksha.rel >= 0.7, ]
## Exclude all consecutive measurements for a given "edvid" if "bart.clean$gha.rel.cha <= 0".
names.vec <- NULL
for (parcel in levels(bart.clean$edvid)) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean[bart.clean$edvid == parcel, ]
    auf.vec <- parcel.subset$auf[parcel.subset$gha.rel.cha < 0]
    if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
        auf.mark <- min(auf.vec, na.rm = TRUE)
        parcel.subset <- parcel.subset[parcel.subset$auf < auf.mark, ]
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    }
}
## Create new data frame from objects created by "for" loop above.
bart.clean <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean <- rbind(bart.clean,
                          eval(expr = as.name(x = name.cur)))
}
bart.clean <- droplevels(x = bart.clean)
rm(list = names.vec)  ## Perform a bit of clean up.
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/gha_alt_invalid_excluded.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 18,
    family = "Times")
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = "gha [m²]",
     xlim = c(range(bart.clean$alt, na.rm = TRUE)[1],range(bart.clean$alt, na.rm = TRUE)[2]+20),  ## accounts for extra space for placing the legend.
     ylim = range(bart.clean$gha, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r",
     main = "art == 511, ksha.rel >= 0.7, gha.rel.cha >= 0")
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
        args = "-r 64 Graphics/gha_alt_invalid_excluded.pdf",
        wait = FALSE)

###################################################################################################################
## Plot max(bart$gha) against bart$alt, separately for each bart$edvid while excluding invalid data (see below). ##
###################################################################################################################
## Create new data frame "bart.clean" from "bart" by excluding invalid data. ##
###############################################################################
## Exclude all lines in which "bart$art != 511".
bart.clean <- droplevels(x = bart[bart$art == 511, ])
## Exclude all lines in which "bart.clean$ksha.rel < 0.7".
bart.clean <- bart.clean[bart.clean$ksha.rel >= 0.7, ]
## Exclude all consecutive measurements for a given "edvid" if "bart.clean$gha.rel.cha <= 0".
names.vec <- NULL
for (parcel in levels(bart.clean$edvid)) {
    name.cur <- paste0("obj.", as.character(parcel))
    names.vec <- c(names.vec, name.cur)
    parcel.subset <- bart.clean[bart.clean$edvid == parcel, ]
    auf.vec <- parcel.subset$auf[parcel.subset$gha.rel.cha < 0]
    if (all(is.na(x = auf.vec))) {  ## If this is true it means that the current subset contains no occasion of "gha.rel.cha < 0", i.e., no exclusions are necessary.
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    } else {  ## If this is true it means that the current subset contains occasions of "gha.rel.cha < 0", i.e., exclusions are necessary.
        auf.mark <- min(auf.vec, na.rm = TRUE)
        parcel.subset <- parcel.subset[parcel.subset$auf < auf.mark, ]
        assign(x = make.names(names = name.cur),
               value = parcel.subset)
    }
}
## Create new data frame from objects created by "for" loop above.
bart.clean <- data.frame(NULL)
for (name.cur in names.vec) {
    bart.clean <- rbind(bart.clean,
                          eval(expr = as.name(x = name.cur)))
}
bart.clean <- droplevels(x = bart.clean)
rm(list = names.vec)  ## Perform a bit of clean up.
graphics.off()
## Start graphics device driver for producing PDF graphics.
kPdfWidth <- 30
pdf(file = "Graphics/gmax_alt_invalid_excluded.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 19,
    family = "Times")
## Create empty plot.
plot(x = NA,
     y = NA,
     xlab = "alt [a]",
     ylab = expression("gha"[max]*"[m"^2*"]"),
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
        args = "-r 64 Graphics/gmax_alt_invalid_excluded.pdf",
        wait = FALSE)
