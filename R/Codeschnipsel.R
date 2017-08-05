## Gmax Wördehoff2016
library(gamlss)
mod1 <- gamlss(gha_bestand ~ ps(h100_bestand, df=2), sigma.formula= ~ps(h100_bestand, df=1), data=bu1.2, family="BCCGo", trace=TRUE)
summary(mod1)
par(mfrow=c(2,2))
term.plot(mod1, se = TRUE, partial.resid = FALSE, what = c("mu"), main = "mu")
term.plot(mod1, se = TRUE, partial.resid = FALSE, what = c("sigma"), main = "sigma")
mod1 <- update(mod1, sigma.formula= ~h100_bestand)
summary(mod1)
term.plot(mod1, se = TRUE, partial.resid = FALSE, what = c("mu"), main = "mu")
term.plot(mod1, se = TRUE, partial.resid = FALSE, what = c("sigma"), main = "sigma")
mod1 <- gamlss(gha_bestand ~ ps(h100_bestand, df=2), data=bu1.2, family="BCCGo", trace=TRUE)
summary(mod1)
term.plot(mod1, se = TRUE, partial.resid = FALSE, what = c("mu"), main = "mu")
par(mfrow=c(1,1))

mod1dat.response <- as.data.frame(predictAll(mod1, type="response"))
mod1dat.response$h100_bestand <- bu1.2$h100_bestand
head(mod1dat.response)

mod1dat.terms <- as.data.frame(predictAll(mod1, type="terms"))

(const.mu <- attributes(predict(mod1, what="mu", type="terms"))$constant)
(const.sigma <- attributes(predict(mod1, what="sigma", type="terms"))$constant)
(const.nu <- attributes(predict(mod1, what="nu", type="terms"))$constant)

mod1dat.terms$sigma <- const.sigma
mod1dat.terms$nu <- const.nu
mod1dat.terms$h100_bestand <- bu1.2$h100_bestand
head(mod1dat.terms)
colnames(mod1dat.terms) <- c("y", "mu", "sigma", "nu", "h100_bestand")

f.mu <- approxfun(mod1dat.terms$h100_bestand, mod1dat.terms$mu, rule=2:2)
f.sigma <- approxfun(mod1dat.terms$h100_bestand, mod1dat.terms$sigma, rule=2:2)
f.nu <- approxfun(mod1dat.terms$h100_bestand, mod1dat.terms$nu, rule=2:2)

(min.h100 <- min(bu1.2$h100_bestand))
(max.h100 <- max(bu1.2$h100_bestand))

mod1dat.terms.approx <- data.frame(h100_bestand=seq(min.h100, max.h100, 0.1))
mod1dat.terms.approx$mu <- f.mu(mod1dat.terms.approx$h100_bestand)
mod1dat.terms.approx$sigma <- f.sigma(mod1dat.terms.approx$h100_bestand)
mod1dat.terms.approx$nu <- f.nu(mod1dat.terms.approx$h100_bestand)

Parameterdaten.mod1 <- data.frame(h100_bestand=seq(min.h100, max.h100, 0.1))
Parameterdaten.mod1$mu <- exp(const.mu + mod1dat.terms.approx$mu)
Parameterdaten.mod1$sigma <- exp(const.sigma)
Parameterdaten.mod1$nu <- const.nu

plot(mod1dat.response$h100_bestand, mod1dat.response$mu)
points(Parameterdaten.mod1$h100_bestand, Parameterdaten.mod1$mu, col=2, cex=0.3)
plot(mod1dat.response$h100_bestand, mod1dat.response$sigma)
points(Parameterdaten.mod1$h100_bestand, Parameterdaten.mod1$sigma,col=2,pch=19,cex=.3)
plot(bu1.2$h100_bestand, mod1dat.response$nu)
points(Parameterdaten.mod1$h100_bestand, Parameterdaten.mod1$nu,col=2,pch=19,cex=.3)

## Gmax Döbbeler2004
dgGmax <- function(a1, h100_bestand, a2, nha_bestand, b1, b2) 1/((a1*h100_bestand^a2*nha_bestand)+(b1*h100_bestand^b2))
nGmax <- function(a1, b1, dgGmax, a2, b2) (b1/a1)*((2*b1*dgGmax)^(a2/b2-1))
Gmax <- function(h100_bestand, a1, b1, a2, b2) (pi/(16*a1*b1*h100_bestand^(a2+b2)))/10000

## Koeffizienten für Buche
a1 <- 1.0829*10^-7
a2 <- 1.5374
b1 <- 8.3652
b2 <- -1.7365

