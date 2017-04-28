## Preamble
rm(list = ls())
setwd("~/laptop02_MasAr")
load("R/Data/gmax.RData")

## Transform columns to factor if appropriate
bart$edvid <- as.factor(bart$edvid)

## Tree species according to Wördehoff (2016)
## 110 = Eiche
## 211 = Buche
## 511 = Fichte
## 611 = Douglasie
## 711 = Kiefer

#########################################################################################
## Plot bart$gha against bart$h100 for bart$art == 511, separately for each bart$edvid ##
#########################################################################################
graphics.off()
## Start graphics device driver for producing PDF graphics
kPdfWidth <- 30
pdf(file = "Graphics/gha_h100.pdf",
    width = kPdfWidth,
    height = kPdfWidth*0.625,
    pointsize = 18,
    family = "Times")
## Create empty plot
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
## Create data frame containing combinations of col and pch
kColVec <- c("black","green","red","brown","cyan","darkorange","darkblue","dimgray")
kPchVec <- c(19,21:25)
kPchColDataFrame <- data.frame()
for (col in kColVec) {
    kPchColDataFrame <- rbind(kPchColDataFrame,
                              cbind(col = col,pch = kPchVec),
                              stringsAsFactors = FALSE)
}
kPchColDataFrame$pch <- as.numeric(kPchColDataFrame$pch)
kCntr <- 1
## Add points to empty plot
for (ts in levels(bart$edvid)) {
    points(x = bart$h100[bart$edvid == ts & bart$art == 511],
           y = bart$gha[bart$edvid == ts & bart$art == 511],
           col = kPchColDataFrame$col[kCntr],
           bg = kPchColDataFrame$col[kCntr],
           pch = kPchColDataFrame$pch[kCntr])
    kCntr <- kCntr+1
}
## Add legend
legend(x = "topleft",
       legend = paste("edvid: ", levels(bart$edvid)),
       bg = "slategray1",
       col = kPchColDataFrame$col,
       pt.bg = kPchColDataFrame$col,
       pch = kPchColDataFrame$pch,)
graphics.off()
## Open .pdf file via mupdf
system2(command = "mupdf",
        args = "-r 64 Graphics/gha_h100.pdf",
        wait = FALSE)
