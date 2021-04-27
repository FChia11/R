#####################################
### Regression Analysis           ###
### PC Session 4: Motorcycle data ###
#####################################

# Libraries required: MASS

rm(list = ls())

# Load motorcycle data
library(MASS)
data(mcycle)
head(mcycle)
summary(mcycle)
attach(mcycle)

# Plot time vs acceleration
plot(times, accel, main = "Polynomial regression")

# Linear model
fit1 <- lm(accel ~ times, data = mcycle)
abline(fit1, col = "red")

# Quadratic model
fit2 <- lm(accel ~ times + I(times^2), data = mcycle)
fit2.coef <- fit2$coefficients
curve(fit2.coef[1] + fit2.coef[2]*x + fit2.coef[3]*x^2, 0, 60, add = TRUE, col = "green")

# Cubic model
fit3 <- lm(accel ~ times + I(times^2) + I(times^3), data = mcycle)
fit3.coef <- fit3$coefficients
curve(fit3.coef[1] + fit3.coef[2]*x + fit3.coef[3]*x^2 + fit3.coef[4]*x^3, 0, 60, add = TRUE, col = "blue")

# Add legend
legend(40, -60, c("linear", "quadratic", "cubic"), lty = 1, col = c("red", "green", "blue"))

# Local linear regression
plot(times, accel, main = "Local linear regression")
s <- c(2/3, 1/3, 1/10)
colors <- c("red", "green", "blue")
for (i in 1:length(s)) lines(times, predict(loess(accel ~ times, span = s[i], degree = 1), data = mcycle), col = colors[i])
legend(40, -60, c("span = 2/3", "span = 1/3", "span = 1/10"), lty = 1, col = colors)

# Local quadratic regression
plot(times, accel, main = "Local quadratic regression")
for (i in 1:length(s)) lines(times, predict(loess(accel ~ times, span = s[i], degree = 2), data = mcycle), col = colors[i])
legend(40, -60, c("span = 2/3", "span = 1/3", "span = 1/10"), lty = 1, col = colors)
# local quadratic fit with span = 1/3 seems good

# Check model assumptions
fit.loess <- loess(accel ~ times, span = 1/3, degree = 2)
qqnorm(residuals(fit.loess))
qqline(residuals(fit.loess))
scatter.smooth(residuals(fit.loess), span = 1, degree = 1)
scatter.smooth(fitted(fit.loess), sqrt(abs(residuals(fit.loess))), span = 1, degree = 1)
# constant variance is questionable (region with times = 1:14 has small variance, region with times 15:57 has large variance)

# Prediction
t <- c(seq(10, 50, by = 10))
t.pred <- predict(fit.loess, t, se = TRUE)
t.upper <- t.pred$fit + qnorm(0.975) * t.pred$se.fit
t.lower <- t.pred$fit - qnorm(0.975) * t.pred$se.fit
data.frame("pred" = t.pred$fit, "lower" = t.lower, "upper" = t.upper)

# Test for nonlinearity
traceS <- fit.loess$trace.hat
SSE0 <- sum(residuals(fit1)^2)
SSE1 <- sum(residuals(fit.loess)^2)
n <- dim(mcycle)[1]
Fvalue <- ((SSE0 - SSE1) / (traceS - 2)) / (SSE1 / (n - traceS))
Fvalue
Fcrit <- qf(0.95, traceS - 2, n - traceS)
Fcrit
1 - pf(Fvalue, traceS - 2, n - traceS)
# linearity is rejected since P-value is zero

detach(mcycle)
