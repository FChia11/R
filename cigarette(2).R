####################################
### Regression Analysis          ###
### PC session 1: Cigarette data ###
####################################

# Libraries required: rgl and MASS

rm(list = ls())

# Load cigarette data
cigarette <- read.table("cigarette.txt", header = TRUE)
cigarette <- cigarette[,-1]
cigarette
n <- dim(cigarette)[1]
p <- dim(cigarette)[2]
attach(cigarette)

# Descriptive statistics
summary(cigarette)

# Histograms
par(mfrow = c(2,2))
hist(Tar)
hist(Nicotine)
hist(Weight)
hist(CO)

# Boxplots
par(mfrow = c(2,2))
boxplot(Tar, main = "Boxplot of Tar")
boxplot(Nicotine, main = "Boxplot of Nicotine")
boxplot(Weight, main = "Boxplot of Weight")
boxplot(CO, main = "Boxplot of CO")
# observation 3 is deviating from the main trend
par(mfrow = c(1,1))

# Scatter plot
pairs(cigarette)
pairs(cigarette, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

# CO ~ Tar
fit1 <- lm(CO ~ Tar, data = cigarette)
fit1
plot(Tar, CO)
abline(fit1, col = "red")
# intercept: average CO content for cigarettes with 0 Tar is 2.743
# slope: if Tar is increased one unit, the average CO content increases 0.801 units

# CO ~ Nicotine
fit2 <- lm(CO ~ Nicotine, data = cigarette)
fit2
plot(Nicotine, CO)
abline(fit2, col = "red")
# intercept: average CO content for cigarettes with 0 Nicotine is 1.665
# slope: if Nicotine is increased one unit, the average CO content increases 12.395 units

# Co ~ Tar + Nicotine
fit3 <- lm(CO ~ Tar + Nicotine, data = cigarette)
fit3
fit3.sum <- summary(fit3)
fit3.sum
# regression slope of Nicotine changed sign
# regression slope of Nicotine is non-significant since P-value = 0.492 > 0.05

# 3D plot regression surface
library(rgl)
plot3d(Tar, Nicotine, CO, type = "s", col = "red", size = 1)
fit3.coef <- coefficients(fit3)
a <- fit3.coef["Tar"]
b <- fit3.coef["Nicotine"]
c <- - 1
d <- fit3.coef["(Intercept)"]
planes3d(a, b, c, d, alpha = 0.3)

# Outlier diagnostics
library(MASS)
fit3.stdres <- stdres(fit3)
plot(fit3.stdres, ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# observation 3 is an outlier

# Co ~ Tar + Nicotine (without observation 3)
fit4 <- lm(CO ~ Tar + Nicotine, data = cigarette[-3,])
fit4
fit4.sum <- summary(fit4)
fit4.sum
# regression slope of Nicotine is again positive
# regression slope of Nicotine is still non-significant since P-value = 0.846 > 0.05

# 3D plot regression surface
library(rgl)
plot3d(Tar[-3], Nicotine[-3], CO[-3], type = "s", col = "red", size = 1, xlab = "Tar", ylab = "Nicotine", zlab = "CO")
fit4.coef <- coefficients(fit4)
a <- fit4.coef["Tar"]
b <- fit4.coef["Nicotine"]
c <- - 1
d <- fit4.coef["(Intercept)"]
planes3d(a, b, c, d, alpha = 0.3)

# Correlation between Tar and Nicotine
plot(Tar, Nicotine)
cor(Tar, Nicotine)
# this correlation could be the cause of a non-significant Nicotine

detach(cigarette)
