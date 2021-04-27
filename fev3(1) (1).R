############################
### Regression Analysis  ###
### Homework 2: FEV data ###
############################

# Libraries required: MASS

rm(list = ls())

# Load fev data
# sex is coded 0 for females and 1 for males
# smoke is code 0 for non-smokers and 1 for smokers
fev <- read.table("fev.txt", header = TRUE)
head(fev)
attach(fev)

# FEV vs age + height + height^2 + sex + smoke
fit1 <- lm(FEV ~ age + height + I(height^2) + sex + smoke)
summary(fit1)

# Detection of heteroscedasticity
library(MASS)
fit1.stdres <- stdres(fit1)
fit1.fittedvalues <- fitted.values(fit1)
par(mfrow = c(1,3))
plot(fit1.fittedvalues, fit1.stdres, xlab = "Fitted value", ylab = "Standardized residual")
plot(fit1.fittedvalues, abs(fit1.stdres), xlab = "Fitted value", ylab = "|Standardized residual|")
plot(fit1.fittedvalues, fit1.stdres^2, xlab = "Fitted value", ylab = "Squared standardized residual")
par(mfrow = c(1,2))
plot(age, fit1.stdres, ylab = "Standardized residual")
plot(height, fit1.stdres, ylab = "Standardized residual")
# all plots detect heteroscedasticity

# Box-Cox transformation
par(mfrow = c(1,1))
out <- boxcox(FEV ~ age + height + I(height^2) + sex + smoke, lambda = seq(-0.5,0.5,0.001), plotit = TRUE)
lambda <- out$x[which(out$y == max(out$y))]
fit2 <- lm(((FEV)^lambda - 1)/lambda ~ age + height + I(height^2) + sex + smoke)
fit2 <- lm(log(FEV) ~ age + height + I(height^2) + sex + smoke)
summary(fit2)

# Detection of heteroscedasticity
fit2.stdres <- stdres(fit2)
fit2.fittedvalues <- fitted.values(fit2)
par(mfrow = c(1,3))
plot(fit2.fittedvalues, fit2.stdres, xlab = "Fitted value", ylab = "Standardized residual")
plot(fit2.fittedvalues, abs(fit2.stdres), xlab = "Fitted value", ylab = "|Standardized residual|")
plot(fit2.fittedvalues, fit2.stdres^2, xlab = "Fitted value", ylab = "Squared standardized residual")
par(mfrow = c(1,2))
plot(age, fit2.stdres, ylab = "Standardized residual")
plot(height, fit2.stdres, ylab = "Standardized residual")
# error variance has been stabilized

# Weighted least squares
w <- 1/lm(abs(fit1.stdres) ~ age + height + I(height^2) + sex + smoke)$fitted.values^2
fit3 <- lm(FEV ~ age + height + I(height^2) + sex + smoke, weight = w)
summary(fit1)
summary(fit3)
# coefficient estimates are not much different, iteration not needed
# some standard errors are decreased

# Detection of heteroscedasticity
fit3.res <- fit3$residuals
fit3.fittedvalues <- fit3$fitted.values
par(mfrow = c(1,1))
plot(fit3.fittedvalues, fit3.res*sqrt(w), xlab = "Fitted value", ylab = "Weighted residual")
par(mfrow = c(2,2))
plot(age, fit3.res, xlab = "age", ylab = "Residual")
plot(height, fit3.res, xlab = "height", ylab = "Residual")
plot(age, fit3.res*sqrt(w), xlab = "age", ylab = "Weighted residual")
plot(height, fit3.res*sqrt(w), xlab = "height", ylab = "Weigthed residual")
# error variance has been stabilized

detach(fev)
