## Preamble
rm(list = ls())
setwd("~/laptop02_MasAr/R")
load("Data/gmax.RData")

## Transform columns to factor if appropriate
bart$edvid <- as.factor(bart$edvid)

## Tree species according to Wördehoff (2016)
## 110 = Eiche
## 211 = Buche
## 511 = Fichte
## 611 = Douglasie
## 711 = Kiefer

## Plot basal area (gha) against Oberhöhe (h100), separately for each edvid
graphics.off()
plot(x = NA,
     y = NA,
     xlab = "h100 [m]",
     ylab = "gha [m²]",
     xlim = range(bart$h100, na.rm = TRUE),
     ylim = range(bart$gha, na.rm = TRUE),
     xaxs = "r",
     yaxs = "r")
grid()
n <- nlevels(bart$edvid)
colnr <- 1
colvec <- sample(x = colors(),
                 size = n,
                 replace = FALSE)
for (ts in levels(bart$edvid)) {
    points(x = bart$h100[bart$edvid == ts],
           y = bart$gha[bart$edvid == ts],
           col = colvec[colnr],
           pch = 19)
    colnr <- colnr+1
}
