###############################################
### Regression Analysis                     ###
### Homework 3: Ultrasonic calibration data ###
###############################################

# Libraries required:

rm(list = ls())

# Load ultrasonic calibration data
# MD = metal distance, UR = ultrasonic response
calibration <- read.table("ultrasonic_calibration.txt", header = TRUE)
head(calibration)
summary(calibration)
attach(calibration)

# Plot MD vs UR
plot(MD, UR)

# Linear regression model
fit1 <- lm(UR ~ MD, data = calibration)
fit1
fit1.sum <- summary(fit1)
fit1.sum
abline(fit1, col = "red")

# Log-transform
fit2 <- lm(log(UR) ~ MD, data = calibration)
fit2
fit2.sum <- summary(fit2)
fit2.sum
x <- seq(0, 7, 0.01)
lines(x, exp(fit2$coef[1] + fit2$coef[2] * x), col = "green")

# Non-linear regression
fit3 <- nls(UR ~ exp(- A * MD) / (B + C * MD), data = calibration, start = list(A = - fit2$coef[2], B = 0.1, C = 0.1), trace = TRUE)
fit3
fit3.sum <- summary(fit3)
fit3.sum
lines(x, exp(- fit3.sum$coef[1] * x) / (fit3.sum$coef[2] + fit3.sum$coef[3] * x), col = "blue")

# Add legend
legend(3.5, 80, c("linear model", "linear model (log transformed)", "non-linear model"), lty = 1, col = c("red", "green", "blue"))

# Residual sum
fit3.res <- residuals(fit3)
fit3.fittedvalues <- fitted.values(fit3)
fit3.stdres <- fit3.res/fit3.sum$sigma
sum(fit3.res)
# residuals do not sum to zero

# Check model assumptions
par(mfrow = c(2,2))
qqnorm(fit3.stdres, main = "")
qqline(fit3.stdres)
plot(fit3.res, xlab = "Index", ylab = "Residual")
plot(fit3.fittedvalues, fit3.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit3.res ~ fit3.fittedvalues), col = "red")
plot(fit3.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-4,4))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: deviations from normal distributed residuals at the tails
# UR: pattern
# BL: heteroscedasticity
# BR: several outliers
par(mfrow = c(1,1))
plot(MD, fit3.res, ylab = "Residual")
lines(lowess(fit3.res ~ MD), col = "red")
# heteroscedastic errors

detach(calibration)
