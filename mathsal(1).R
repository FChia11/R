##################################
### Regression Analysis        ###
### PC session 1: Mathsal data ###
##################################

# Libraries: MASS and ellipse

rm(list = ls())

# Load mathsal data
mathsal <- read.table("mathsal.txt", header = TRUE)
mathsal
n <- dim(mathsal)[1]
p <- dim(mathsal)[2]
attach(mathsal)

# Scatter plot
pairs(mathsal)
pairs(mathsal, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

# Linear regression model
fit1 <- lm(salary ~ workqual + experience + pubsucc, data = mathsal)
fit1
fit1.sum <- summary(fit1)
fit1.sum
# intercept: average salary of a mathematician with workqual = 0, experience = 0 and pubsucc = 0 is 17.847
# slope workqual: if workqual is increased one unit (keeping experience and pubsucc constant),
#                 the average salary of a mathematician increases 1.103 units

# ANOVA
anova(fit1)
fit1.SST <- sum((salary - mean(salary))^2)
fit1.SSR <- sum((fit1$fitted.values - mean(salary))^2)
fit1.SSE <- sum((fit1$residuals)^2)
fit1.MSR <- fit1.SSR/(p - 1)
fit1.MSE <- fit1.SSE/(n - p)
fit1.ANOVA <- data.frame("SS"  = c(fit1.SSR, fit1.SSE, fit1.SST),
                         "Df"  = c(p - 1, n - p, n - 1),
                         "MSS" = c(fit1.MSR, fit1.MSE, NA))
row.names(fit1.ANOVA) <- c("Regression", "Error", "Total")
fit1.ANOVA

# R^2
fit1.sum$r.squared
fit1.SSR/fit1.SST
# 91% of the total variation is explained by the linear model

# Check model assumptions
par(mfrow = c(2,2))
plot(fit1)

# Check model assumptions (2)
library(MASS)
fit1.stdres <- stdres(fit1)
par(mfrow = c(2,2))
qqnorm(fit1.stdres, main="")
qqline(fit1.stdres)
plot(fit1$residuals, xlab = "Index", ylab = "Residual")
plot(fit1$fitted.values, fit1$residuals, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit1$residuals ~ fit1$fitted.values), col = "red")
plot(fit1.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: small deviations from normal distributed residuals
# UR: parabolic trend suggests adding first-order or second-order terms
#     (however, avoid overinterpretation in small sample settings)
# BL: suggestion of unequal variance
#     no clear patterns, curved band suggests linearity assumption is not satisfied
#     (but again, avoid overinterpretation in small sample settings, 
#     especially of the non-parametric fit)
# BR: no outliers
par(mfrow = c(1,3))
plot(workqual, fit1.stdres, ylab = "Residual")
lines(lowess(fit1$residuals ~ workqual), col = "red")
plot(experience, fit1$residuals, ylab = "Residual")
lines(lowess(fit1$residuals ~ experience), col = "red")
plot(pubsucc, fit1$residuals, ylab = "Residual")
lines(lowess(fit1$residuals ~ pubsucc), col = "red")
par(mfrow = c(1,1))
# no clear patterns, although the lowess lines suggest non-linearity

# normality ok overall
# linear function most probably not sufficient
# hints towards non-constant variance (but difficult to assess with small sample)
# no outliers

# Individual confidence intervals
alpha <- 0.05
confint(fit1, parm = 2:3, level = 1 - alpha)
# manually
fit1.slopes <- coefficients(fit1)[2:3]
fit1.slopes.sd <- fit1.sum$coef[2:3,2]
halflength <- fit1.slopes.sd * qt(1 - alpha/2, n - p)
lower <- fit1.slopes - halflength
upper <- fit1.slopes + halflength
confinterval <- cbind(lower,upper)
confinterval

# Simultaneous confidence intervals with Bonferroni correction
alpha <- 0.05
confint(fit1, parm = 2:3, level = 1 - alpha / 2)
# manually
fit1.slopes <- coefficients(fit1)[2:3]
fit1.slopes.sd <- fit1.sum$coef[2:3,2]
halflength <- fit1.slopes.sd*qt(1 - alpha/(2 * 2), n - p)
lower <- fit1.slopes - halflength
upper <- fit1.slopes + halflength
confinterval <- cbind(lower,upper)
confinterval

# Joint confidence region
library(ellipse)
alpha <- 0.05
plot(ellipse(fit1, which = c("workqual", "experience"), level = 1 - alpha), type = "l")
points(fit1$coefficients["workqual"], fit1$coefficients["experience"])
abline(v = confinterval[1,1], lty = 2)
abline(v = confinterval[1,2], lty = 2)
abline(h = confinterval[2,1], lty = 2)
abline(h = confinterval[2,2], lty = 2)
# check results
x <- ellipse(fit1, which = c("workqual", "experience"), level = 1 - alpha)
qf(0.95, 2, fit1$df.residual)
diff <- x[1,] - fit1.slopes[1:2]
t(diff) %*% solve(fit1.sum$cov.unscaled[2:3,2:3]) %*% diff/(2 * fit1.sum$sigma^2)

# Prediction
preddata <- data.frame("workqual" = 5.4, "experience" = 17, "pubsucc" = 6.0)
predict(fit1, newdata = preddata, interval = "prediction")
# manually
preddata <- as.numeric(c(1,unname(preddata)))
y0 <- sum(coefficients(fit1) * preddata)
halflength <- as.numeric(qt(1 - alpha/2, n - p)*sqrt(fit1.MSE*(preddata %*% fit1.sum$cov.unscaled %*% preddata + 1)))
y0 - halflength
y0 + halflength

# ANOVA
fit2 <- lm(salary ~ experience + pubsucc + workqual, data = mathsal)
anova(fit2)
# workqual can not be dropped from the model since P-value = 0.003 < 0.05
fit3 <- lm(salary ~ experience, data = mathsal)
anova(fit3, fit2)
# manually
alpha <- 0.05
Fvalue <- ((85.33 + 34.42)/2)/(61.44/20)
Fcrit <- qf(1 - alpha, 2, 20)
Pvalue <- 1 - pf(Fvalue, 2, 20)
# workqual and pubsucc can not be dropped from the model since F = 19.49 > 3.49 or P-value < 0.05

# ANOVA (2)
fit5 <- lm(salary ~ pubsucc + workqual + experience, data = mathsal)
anova(fit5)
# ordering the predictors differently creates different ANOVA tables

detach(mathsal)
