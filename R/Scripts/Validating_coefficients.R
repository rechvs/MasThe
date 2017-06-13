##############
## Preamble ##
##############
rm(list = ls())
setwd(dir = "~/laptop02_MasAr")
kDataDir <- "Data/"
## {sink(file = "/dev/null"); source(file = "R/Scripts/DataSetCreation.R"); sink()}  ## Create up-to-date data sets  while suppressing output.
## Load data set.
kFileVersion <- "2.7"
kFileName <- paste0(kDataDir, "gmax_", kFileVersion, ".RData")
kgmaxObjects <- load(file = kFileName, verbose = TRUE)

####################################
## Check validity of coefficients ##
####################################
objects.before <- ls()  ## Required for clean up.
a0.Sterba <- 1.2604e-6 ## Source of coefficients: Sterba (1987), tab. 2.
a1.Sterba <- 0.8408
b0.Sterba <- 2.0140
b1.Sterba <- - 1.3618
a0.Wördehoff <- 4.913256e-06 ## Source of coefficients: Wördehof (2016), tab. 3.6.
a1.Wördehoff <- 0.4394706
b0.Wördehoff <- 0.3716977
b1.Wördehoff <- -0.9097641
nha <- bart.clean$nha
h100 <- bart.clean$h100
dg <- bart.clean$dg
dg_Gmax.Sterba <- 1 / (a0.Sterba * h100 ^ a1.Sterba * nha + b0.Sterba * h100 ^ b1.Sterba)  ## Source of equation: Sterba (1975), eq. (12).
dg_Gmax.Wördehoff <- 1 / (a0.Wördehoff * h100 ^ a1.Wördehoff * nha + b0.Wördehoff * h100 ^ b1.Wördehoff)
NGmax.Sterba <- b0.Sterba / a0.Sterba * (2 * b0.Sterba * dg) ^ (a1.Sterba / b1.Sterba - 1)  ## Source of equation: Sterba (1981), eq. (11).
NGmax.Wördehoff <- b0.Wördehoff / a0.Wördehoff * (2 * b0.Wördehoff * dg) ^ (a1.Wördehoff / b1.Wördehoff - 1)
Gmax.Sterba <- pi / (16 * a0.Sterba * b0.Sterba * h100 ^ (a1.Sterba + b1.Sterba))  ## Source of equation: Sterba (1975), eq. (10).
Gmax.Wördehoff <- pi / (16 * a0.Wördehoff * b0.Wördehoff * (h100 ^ (a1.Wördehoff + b1.Wördehoff)))
dg_Gmax.Sterba / bart.clean$dg  ## Results seem to be correct.
dg_Gmax.Wördehoff / bart.clean$dg  ## Results seem to be correct.
NGmax.Sterba / bart.clean$nha  # Results seem to be correct.
NGmax.Wördehoff / bart.clean$nha  ## Results seem to be correct.
Gmax.Sterba / bart.clean$gha  ## Results seem off by a factor of about 10000.
Gmax.Wördehoff / bart.clean$gha  ## Results seem off by a factor of about 10000.
rm(list = setdiff(x = ls(), y = objects.before))  ## Clean up.
