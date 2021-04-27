###################################
### Regression Analysis         ###
### PC Session 4: Salinity data ###
###################################

# Libraries required: robustbase and MASS

rm(list = ls())

# Load salinity data
library(robustbase)
data(salinity)
head(salinity)
summary(salinity)
attach(salinity)

# Scatter plot
pairs(salinity)
pairs(salinity, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

# Linear regression model
LS <- lm(Y ~ X1 + X2 + X3, data = salinity)
summary(LS)

# Standardized residuals
library(MASS)
LS.stdres <- stdres(LS)
plot(LS.stdres, ylim = c(-4,4), ylab = "Standardized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Studentized residuals
LS.studres <- studres(LS)
plot(LS.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagonal elements of hat matrix
LS.influence <- influence(LS)
plot(LS.influence$hat, ylab = "Diagonal elements of hat matrix")
n <- dim(salinity)[1]
p <- dim(salinity)[2]
abline(h = 2*p/n, col = "red")

# DFFITS
LS.dffits <- dffits(LS)
plot(LS.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")

# Cook's distance
LS.Cd <- cooks.distance(LS)
plot(LS.Cd, ylab = "Cook's distance")
abline(h = 1, col = "red")

# DFBETAS
LS.dfbetas <- dfbetas(LS)
LS.dfbetas
2/sqrt(n)
# observation 16 is always detected as an outlier
# observation 9 is detected as influential for DFFITS and DFBETAS

# RLTS (50% breakdown value)
RLTS <- ltsReg(Y ~ X1 + X2 + X3, data = salinity)
summary(RLTS)

# Detection of outliers
plot(RLTS, which = "rindex")
plot(RLTS, which = "rdiag")

# Standardized residuals
RLTS.stdres <- RLTS$residuals/RLTS$scale
plot(RLTS.stdres, ylim = c(-12,12), ylab = "Standardized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagnostic plot
plot(RLTS$RD, RLTS.stdres, ylim = c(-12,12), xlab = "Robust distances", ylab = "Standardized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")
# outliers 5, 23 and 24 are masked in the classical analysis

# RLTS (10% breakdown value)
RLTS2 <- ltsReg(Y ~ X1 + X2 + X3, data = salinity, alpha = 0.9)
summary(RLTS2)

# Detection of outliers
plot(RLTS2, which = "rindex")
plot(RLTS2, which = "rdiag")

detach(salinity)
