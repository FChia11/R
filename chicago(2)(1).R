################################
### Regression Analysis      ###
### Homework 2: Chicago data ###
################################

# Libraries required: MASS

rm(list = ls())

# Load chicago data
chicago <- read.table("chicago.txt", header = TRUE)
head(chicago)
attach(chicago)

# Exploratory analysis
summary(chicago)
hist(involact)
boxplot(involact)
boxplot(theft)
cor(chicago[,-7])
# distribution of involact seems to be highly skewed

# Scatter plot
pairs(chicago)
pairs(chicago, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = 2)})

# Involact ~ race
fit1 <- lm(involact ~ race)
summary(fit1)
# race has an effect on involact

# Involact ~ race + fire + theft + age + income + side
fit2 <- lm(involact ~ race + fire + theft + age + income + side)
summary(fit2)

# Check model assumptions
par(mfrow = c(2,2))
plot(fit2)

# Check model assumptions (2)
library(MASS)
fit2.res <- residuals(fit2)
fit2.stdres <- stdres(fit2)
fit2.fittedvalues <- fitted.values(fit2)
par(mfrow = c(2,2))
qqnorm(fit2.stdres, main="")
qqline(fit2.stdres)
plot(fit2.res, xlab = "Index", ylab = "Residual")
plot(fit2.fittedvalues, fit2.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit2.res ~ fit2.fittedvalues), col = "red")
plot(fit2.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: normal distributed residuals except at the tails
# UR: no pattern
# BL: funnel behavior indicates heteroscedastic errors
# BR: two small outliers
par(mfrow = c(1,5))
plot(race, fit2.res, ylab = "Residual")
lines(lowess(fit2.res ~ race), col = "red")
plot(fire, fit2.res, ylab = "Residual")
lines(lowess(fit2.res ~ fire), col = "red")
plot(theft, fit2.res, ylab = "Residual")
lines(lowess(fit2.res ~ theft), col = "red")
plot(age, fit2.res, ylab = "Residual")
lines(lowess(fit2.res ~ age), col = "red")
plot(income, fit2.res, ylab = "Residual")
lines(lowess(fit2.res ~ income), col = "red")
# again small funnel behavior, detected little curvature

plot(race, involact)
plot(fire, involact)
plot(theft, involact)
plot(age, involact)
plot(income, involact)


# Transformations
# 1) log(income)
plot(log(income), involact)
plot(income, involact)
fit3 <- lm(involact ~ race + fire + theft + age + log(income) + side)
summary(fit3)
summary(fit2)

# 2) log(involact + 1)
plot(income, log(involact + 1))
fit4 <- lm(log(involact + 1) ~ race + fire + theft + age + income + side)
summary(fit4)

# 3) sqrt(involact)
plot(age, sqrt(involact))
plot(age, involact)
fit5 <- lm(sqrt(involact) ~ race + fire + theft + age + income + side)
summary(fit5)

# 4) involact^2
plot(fire, involact^2)
plot(fire, involact)
fit6 <- lm(involact^2 ~ race + fire + theft + age + income + side)
summary(fit6)

# 5) Box-Cox transformation (response should be strictly positive)
par(mfrow = c(1,1))
out <- boxcox(involact + 1 ~ race + fire + theft + age + income + side, plotit = TRUE)
lambda <- out$x[which(out$y == max(out$y))]
fit7 <- lm(((involact + 1)^lambda - 1)/lambda ~ race + fire + theft + age + income +side)
summary(fit7)

# QQ-plots
fit.stdres <- lapply(list(fit2, fit3, fit4, fit5, fit6, fit7), stdres)
par(mfrow = c(2,3))
for (i in 1:6) {
  qqnorm(fit.stdres[[i]], main = "")
  qqline(fit.stdres[[i]])
}
# fit2 and fit3 show small differences
# fit4, (fit5), fit6, fit7 (transformed involact) lead to less normal distributed errors

# Residuals vs fitted values
fit.fittedvalues <- lapply(list(fit2, fit3, fit4, fit5, fit6, fit7), fitted.values)
fit.res <- lapply(list(fit2, fit3, fit4, fit5, fit6, fit7), residuals)
par(mfrow = c(2,3))
for (i in 1:6) {
  plot(fit.fittedvalues[[i]], fit.res[[i]], xlab = "Fitted value", ylab = "Residual")
  lines(lowess(fit.res[[i]] ~ fit.fittedvalues[[i]]), col = "red")
}
# for fit4, fit5 and fit7 a linear model is more or less appropriate


# Without side (highly insignificant)
# Transformations
# 1) log(income)
plot(log(income), involact)
plot(income, involact)
fit3 <- lm(involact ~ race + fire + theft + age + log(income))
summary(fit3)
summary(fit2)

# 2) log(involact + 1)
plot(income, log(involact + 1))
fit4 <- lm(log(involact + 1) ~ race + fire + theft + age + income)
summary(fit4)

# 3) sqrt(involact)
plot(age, sqrt(involact))
plot(age, involact)
fit5 <- lm(sqrt(involact) ~ race + fire + theft + age + income)
summary(fit5)

# 4) involact^2
plot(fire, involact^2)
plot(fire, involact)
fit6 <- lm(involact^2 ~ race + fire + theft + age + income)
summary(fit6)

# 5) Box-Cox transformation (response should be strictly positive)
par(mfrow = c(1,1))
out <- boxcox(involact + 1 ~ race + fire + theft + age + income , plotit = TRUE)
lambda <- out$x[which(out$y == max(out$y))]
fit7 <- lm(((involact + 1)^lambda - 1)/lambda ~ race + fire + theft + age + income)
summary(fit7)

# QQ-plots
fit.stdres <- lapply(list(fit2, fit3, fit4, fit5, fit6, fit7), stdres)
par(mfrow = c(2,3))
for (i in 1:6) {
     qqnorm(fit.stdres[[i]], main = "")
     qqline(fit.stdres[[i]])
}
# fit2 and fit3 show small differences
# fit4, fit5 and fit6 (transformed involact) lead to less normal distributed errors
# fit7 improves normality at the tails of the error distribution

# Residuals vs fitted values
fit.fittedvalues <- lapply(list(fit2, fit3, fit4, fit5, fit6, fit7), fitted.values)
fit.res <- lapply(list(fit2, fit3, fit4, fit5, fit6, fit7), residuals)
par(mfrow = c(2,3))
for (i in 1:6) {
     plot(fit.fittedvalues[[i]], fit.res[[i]], xlab = "Fitted value", ylab = "Residual")
     lines(lowess(fit.res[[i]] ~ fit.fittedvalues[[i]]), col = "red")
}
# for fit4 and fit7 a linear model is more or less appropriate

# Summary:
# fit7 (Box-Cox) has approximately normal errors and the relation is quite linear
# but interpretation of the response is now very difficult
# therefore fit2 (linear model) should not be forgotten
# remark: the many zero response values make interpretations of the plots difficult

detach(chicago)
