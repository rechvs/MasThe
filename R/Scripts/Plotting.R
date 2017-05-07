###############
## Preamble. ##
###############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
## Load base file.
kBaseFileVersion <- "1.5"
kBaseFileName <- paste0(kDataDir,"gmax_", kBaseFileVersion, ".RData")
kgmaxObjects <- load(file = kBaseFileName, verbose = TRUE)
load(file = "R/Data/gmax.RData")

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
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow","magenta")
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
kColVec <- c("black","green","red","brown","cyan","darkorange","burlywood","dimgray","yellow","magenta")
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
