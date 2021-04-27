######################################################
###          Regression Analysis                   ###
###          Exam Project: Invertebrate dataset    ###
######################################################

# Libraries: MASS

rm(list = ls())

# Load invertebrate Data #Training and Validation Set
# Variables: SWI, SWF, temperature, size, management, duration

data.full = read.table("invertebrate.txt",header=T)
set.seed(r0773487)
d.test <- sample(1:dim(data.full)[1], 200 )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]

summary(data.full)

attach(data.training)

# Exploratory Data Analysis
data.training["duration"] <- NULL
summary(data.training)

# Histograms
par(mfrow = c(2,6))
hist(SWI)
hist(SWF)
hist(temperature)
hist(size)
hist(management)
hist(duration)

# Boxplots
par(mfrow = c(1,1))
data.training["duration"] <- NULL 
boxplot(data.training, main = "Boxplot of data.training")
boxplot(SWI, main = "Boxplot of SWI")
boxplot(SWF, main = "Boxplot of SWF")
boxplot(temperature, main = "Boxplot of temperature")
boxplot(size, main = "Boxplot of size")
boxplot(management, main = "Boxplot of management")
boxplot(duration, main = "Boxplot of duration")

par(mfrow = c(1,1))

# Scatter plot
data.training["duration"] <- NULL
pairs(data.training)
pairs(data.training, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})


library(psych)
data.training["duration"] <- NULL
pairs.panels(data.training[,-5], method = "pearson", hist.col = "green", density = TRUE,
             ellipses = TRUE)

#Fit a Linear Regression Model
fit.full <- lm(SWI ~ SWF + temperature + size + management, data = data.training)
fit.full
summary(fit.full)
fit.full.sum <- summary(fit.full)
fit.full.sum

#ANOVA
summary.aov(fit.full)

#Correlation
summary(fit.full, correlation = TRUE)

# Check model assumptions
par(mfrow = c(2,2))
plot(fit.full)

fit.full<-wagesmicrodata[,3:12]
library(GGally)
ggpairs(fit.full)

cooks.distance(fit.full)
shapiro.test(SWI)
shapiro.test(SWF)
shapiro.test(temperature)
shapiro.test(size)
shapiro.test(management)

#Breusch-Pagan Test
bptest(SWI ~ SWF + temperature + size + management)

# Check model assumptions (2)
library(MASS)
fit.full.res <- residuals(fit.full)
fit.full.stdres <- stdres(fit.full)
fit.full.fittedvalues <- fitted.values(fit.full)
par(mfrow = c(2,2))
qqnorm(fit.full.stdres, main="")
qqline(fit.full.stdres)
plot(fit.full.res, xlab = "Index", ylab = "Residual")
plot(fit.full.fittedvalues, fit.full.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit.full.res ~ fit.full.fittedvalues), col = "red")
plot(fit.full.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-5,5))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# UL: deviations from normal distributed residuals at the tails
# UR: pattern indicates heteroscedastic errors
# BL: curved band suggests a different pattern than a linear one
#     variance of residuals increase with the level of the response variable
# BR: several outliers
par(mfrow = c(1,2))
plot(SWI, fit.full.res, ylab = "Residual")
lines(lowess(fit.full.res ~ SWI), col = "red")
plot(SWF, fit.full.res, ylab = "Residual")
lines(lowess(fit.full.res ~ SWF), col = "red")
plot(temperature, fit.full.res, ylab = "Residual")
lines(lowess(fit.full.res ~ temperature), col = "red")
plot(size, fit.full.res, ylab = "Residual")
lines(lowess(fit.full.res ~ size), col = "red")
plot(management, fit.full.res, ylab = "Residual")
lines(lowess(fit.full.res ~ management), col = "red")
# plots indicate the linear model is defective (add quadratic terms) and the errors are heteroscedastic

# Partial residual plots
fit.full.coef <- coefficients(fit.full)
fit.full.pres.SWI <- fit.full.res + fit.full.coef[2] * SWI
fit.full.pres.SWF <- fit.full.res + fit.full.coef[2] * SWF
fit.full.pres.temperature <- fit.full.res + fit.full.coef[3] * temperature
fit.full.pres.size <- fit.full.res + fit.full.coef[2] * size
fit.full.pres.management <- fit.full.res + fit.full.coef[3] * management

par(mfrow = c(1,2))
plot(SWI, fit.full.pres.SWI, ylab = "Partial residual (SWI)")
abline(lm(unname(fit.full.pres.SWI) ~ SWI))
lines(lowess(SWI, fit.full.pres.SWI), col = "red")
plot(SWF,fit.full.pres.SWF, ylab = "Partial residual (SWF)")
abline(lm(fit.full.pres.SWF ~ SWF))
lines(lowess(SWF, fit.full.pres.SWF), col = "red")
plot(temperature,fit.full.pres.temperature, ylab = "Partial residual (temperature)")
abline(lm(fit.full.pres.temperature ~ temperature))
lines(lowess(temperature, fit.full.pres.temperature), col = "red")
plot(size,fit.full.pres.size, ylab = "Partial residual (size)")
abline(lm(fit.full.pres.size ~ size))
lines(lowess(size, fit.full.pres.size), col = "red")
plot(management,fit.full.pres.management, ylab = "Partial residual (management)")
abline(lm(fit.full.pres.management ~ management))
lines(lowess(management, fit.full.pres.management), col = "red")

# Assessing Outliers
outlierTest(fit.full) # Bonferonni p-value for most extreme obs
qqPlot(fit.full, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit.full) # leverage plots

# Influential Observations
# added variable plots
avPlots(fit.full)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit.full$coefficients)-2))
plot(fit.full, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit.full, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit.full, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit.full)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit.full)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit.full)

# Evaluate Collinearity
vif(SWF) # variance inflation factors
sqrt(vif(SWF)) > 2 # problem?

# Correlation
fit.full.cor <- cor(fit.full)
fit.full.cor
corx <- fit.full.cor[-1,-1]
# large correlation between X1 and X2

# VIF
fit.full.VIF <- diag(solve(corx))
fit.full.VIF
# VIF values for X1 and X2 are large

# Eigenvalues
corx.eig <- eigen(corx)$values
corx.eig
sqrt(max(corx.eig)/corx.eig)
# third eigenvalue is very small

#----------------------------#
# Multicollinearity remedies #
#----------------------------#

# Correlation transformation
n <- dim(cosmetics)[1]
cosmetics.stand <- scale(cosmetics, center = T, scale = T) / sqrt(n - 1)
cosmetics.stand <- as.data.frame(cosmetics.stand)
attach(cosmetics.stand)
LS.stand <- lm(S ~ X1 + X2 + X3 - 1, data = cosmetics.stand)
LS.stand
LS.stand.sum <- summary(LS.stand)
LS.stand.sum

# PCR (standardized data)
PCA <- princomp(cosmetics.stand[,-1])
summary(PCA)
plot(PCA)
k <- 2
PCA.scores <- PCA$scores[,1:k]
PCR.stand <- lm(S ~ PCA.scores - 1, data = cosmetics.stand)
summary(PCR.stand)
PCR.stand.coef <- PCA$loadings[,1:k]%*% coefficients(PCR.stand)

# equivalently
cosmetics.stand.cor <- cor(cosmetics.stand)
corx <- cosmetics.stand.cor[-1,-1]
corx.specdec <- eigen(corx)
crossprod <- corx.specdec$vectors[,1:k] %*% diag(1 / corx.specdec$values[1:k]) %*% t(corx.specdec$vectors[,1:k])
PCR.stand.coef <- crossprod %*% t(cosmetics.stand[,-1]) %*% as.matrix(cosmetics.stand$S)
PCR.stand.coef

# VIF of PCR
PCR.VIF<-diag(crossprod)
PCR.VIF
# PCR (original data)
PCR.coef <- rep(0,4)
PCR.coef[2] <- sd(cosmetics[,1])/sd(cosmetics[,2])*PCR.stand.coef[1]
PCR.coef[3] <- sd(cosmetics[,1])/sd(cosmetics[,3])*PCR.stand.coef[2]
PCR.coef[4] <- sd(cosmetics[,1])/sd(cosmetics[,4])*PCR.stand.coef[3]
PCR.coef[1] <- mean(cosmetics[,1])- PCR.coef[2]*mean(cosmetics[,2])- PCR.coef[3]*mean(cosmetics[,3])- PCR.coef[4]*mean(cosmetics[,4])
PCR.coef

# Ridge regression (standardized data)

# Note that if you downloaded Chapter 8_handouts,
# there was previously an error on slides 43, 49, 50, and 51.
# the penalty term in equations (6), (7), (8), and (9) 
# should not be "c", but "n*c" (with c as specified in (5)).

library(MASS)
c <- seq(0.01,0.1,0.01)
penalty<-n*c
lmr1<-lm.ridge(S ~ X1 + X2 + X3 - 1, data=cosmetics.stand, lambda = penalty)
plot(lmr1$coef[1,]~penalty,ylim=c(-0,0.2))
points(lmr1$coef[2,]~penalty,col="red")
points(lmr1$coef[3,]~penalty,col="blue")

c <- 0.06
penalty<-n*c
lmr1<-lm.ridge(S ~ X1 + X2 + X3 - 1, data=cosmetics.stand, lambda = penalty)

#equivalently
cosmetics.stand.cor <- cor(cosmetics.stand)
corx <- cosmetics.stand.cor[-1,-1]
corxy <- cosmetics.stand.cor[2:4,1]
RR.stand.coef <- solve((corx + c*diag(c(1,1,1))),corxy)
RR.stand.coef

# VIF values
crossprod <- solve(corx + c*diag(rep(1,3)))
RR.VIF <- diag(crossprod %*% corx %*% crossprod)
RR.VIF
PCR.VIF

# Ridge regression (original data)
lm.ridge(S ~ X1 + X2 + X3, data=cosmetics, lambda = n*c)
#equivalently
RR.coef <- rep(0,4)
RR.coef[2] <- sd(cosmetics[,1])/sd(cosmetics[,2])*RR.stand.coef[1]
RR.coef[3] <- sd(cosmetics[,1])/sd(cosmetics[,3])*RR.stand.coef[2]
RR.coef[4] <- sd(cosmetics[,1])/sd(cosmetics[,4])*RR.stand.coef[3]
RR.coef[1] <- mean(cosmetics[,1])- RR.coef[2]*mean(cosmetics[,2])- RR.coef[3]*mean(cosmetics[,3])- RR.coef[4]*mean(cosmetics[,4])
RR.coef

# Comparison coefficients
data.frame("LS" = LS$coefficients, "PCR" = PCR.coef, "RR" = RR.coef)
data.frame("LS" = LS.stand$coefficients, "PCR" = PCR.stand.coef, "RR" = RR.stand.coef)

detach(cosmetics.stand)


# Evaluate Nonlinearity
# component + residual plot
crPlots(fit.full)
# Ceres plots
ceresPlots(fit.full)

# Test for Autocorrelated Errors
durbinWatsonTest(fit.full)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit.full)
summary(gvmodel)

######################################################################################################

###### Fit a New Model ######

fit.full1 <- lm(SWI ~ SWF + temperature + management)
summary(fit.full1)
plot(fit.full1)
anova(fit.full1)
residuals(fit.full1)

fit.full2 <- lm(log1p(SWI) ~ SWF + temperature + size + management)
summary(fit.full2)
plot(fit.full2)
anova(fit.full2)
residuals(fit.full2)

fit.full3 <- lm(log1p(SWI) ~ SWF + temperature + management)
summary(fit.full3)
plot(fit.full3)
anova(fit.full3)
residuals(fit.full3)

fit.full4 <- lm(log1p(SWI) ~ log1p(SWF) + log1p(temperature) + log1p(size) + log1p(management), data = data.training)
summary(fit.full4)
plot(fit.full4)
anova(fit.full4)
residuals(fit.full4)

fit.full5 <- lm(log1p(SWI) ~ log1p(SWF) + log1p(temperature) + log1p(management), data = data.training)
summary(fit.full5)
plot(fit.full5)
anova(fit.full5)
residuals(fit.full5)

# Non-Linearity Remedies #

fit.full6 <- lm(SWI ~ SWF + I(SWF^2) + temperature + I(temperature^2) + size + management, data = data.training)
summary(fit.full6)
plot(fit.full6)
anova(fit.full6)
residuals(fit.full6)

fit.full7 <- lm(SWI ~ SWF + temperature + size + I(size^2)+ management, data = data.training)
summary(fit.full7)
plot(fit.full7)
anova(fit.full7)
residuals(fit.full7)

fit.full8 <- lm(SWI ~ SWF + temperature + size + I(size^2)+ management, data = data.training)
######################################################################################################

# 6 - #Paramatric VS Quadratic Model
#Question #Local Regression

data.training = as.numeric(data.training)
model.l = loess(duration ~ temperature, data = data.training, span = 0.25, degree = 1, family = "gaussian")
summary(model.l)
residuals(model.l)
plot(temperature, duration)
lines(loess.smooth(temperature, duration, span = 0.25, degree = 1))
library(rcompanion)
plotPredy(data = data.training, x = temperature, y = duration, model = model.l, xlab = "temperature", ylab = "duration")

data.training = as.numeric(data.training)
model.l = loess(duration ~ temperature, data = data.training, span = 0.25, degree = 2, family = "gaussian")
summary(model.l)
residuals(model.l)
plot(temperature, duration)
lines(loess.smooth(temperature, duration, span = 0.25, degree = 2))
library(rcompanion)
plotPredy(data = data.training, x = temperature, y = duration, model = model.l, xlab = "temperature", ylab = "duration")

data.training = as.numeric(data.training)
model.l = loess(duration ~ temperature, data = data.training, span = 0.5, degree = 1, family = "gaussian")
summary(model.l)
residuals(model.l)
plot(temperature, duration)
lines(loess.smooth(temperature, duration, span = 0.5, degree = 1))
library(rcompanion)
plotPredy(data = data.training, x = temperature, y = duration, model = model.l, xlab = "temperature", ylab = "duration")

data.training = as.numeric(data.training)
model.l = loess(duration ~ temperature, data = data.training, span = 0.5, degree = 2, family = "gaussian")
summary(model.l)
residuals(model.l)
plot(temperature, duration)
lines(loess.smooth(temperature, duration, span = 0.5, degree = 2))
library(rcompanion)
plotPredy(data = data.training, x = temperature, y = duration, model = model.l, xlab = "temperature", ylab = "duration")

data.training = as.numeric(data.training)
model.l = loess(duration ~ temperature, data = data.training, span = 0.75, degree = 1, family = "gaussian")
summary(model.l)
residuals(model.l)
plot(temperature, duration)
lines(loess.smooth(temperature, duration, span = 0.75, degree = 1))
library(rcompanion)
plotPredy(data = data.training, x = temperature, y = duration, model = model.l, xlab = "temperature", ylab = "duration")

data.training = as.numeric(data.training)
model.l = loess(duration ~ temperature, data = data.training, span = 0.75, degree = 2, family = "gaussian")
summary(model.l)
residuals(model.l)
plot(temperature, duration)
lines(loess.smooth(temperature, duration, span = 0.75, degree = 2))
library(rcompanion)
plotPredy(data = data.training, x = temperature, y = duration, model = model.l, xlab = "temperature", ylab = "duration")

scatter.smooth(temperature, duration, span = 0.25, degree = 1)
scatter.smooth(temperature, residuals(model.l), span = 0.25, degree = 1)
scatter.smooth(temperature, duration, span = 0.25, degree = 2)
scatter.smooth(temperature, residuals(model.l), span = 0.25, degree = 2)

scatter.smooth(temperature, duration, span = 0.5, degree = 1)
scatter.smooth(temperature, residuals(model.l), span = 0.5, degree = 1)
scatter.smooth(temperature, duration, span = 0.5, degree = 2)
scatter.smooth(temperature, residuals(model.l), span = 0.5, degree = 2)

scatter.smooth(temperature, duration, span = 0.75, degree = 1)
scatter.smooth(temperature, residuals(model.l), span = 0.75, degree = 1)
scatter.smooth(temperature, duration, span = 0.75, degree = 2)
scatter.smooth(temperature, residuals(model.l), span = 0.75, degree = 2)

#D Quadratic Model

temperature2 <- temperature^2
quadratic.model <- lm(duration ~ temperature + I(temperature^2))
summary(quadratic.model)
residuals(quadratic.model)
plot(quadratic.model)
predicted.plot <- predict(quadratic.model, list(Temperature = temperature, Temperature2 = temperature2))
plot(temperature, duration, pch=16, xlab = "Temperature", ylab = "Duration", cex.lab = 1.3, col = "blue")
abline()
lines(temperature, predicted.plot, col = "darkgreen", lwd = 3)

plot(temperature, duration, main="Quadratic model", 
     xlab="temperature", ylab="duration", pch=19)
par(new = TRUE)
lines(duration,quadratic, col="blue")

library(ggplot2)
temperature <- rnorm(100)
duration <- rnorm(100) - temperature^2
qplot(x=temperature, y=duration, geom=c("point", "smooth"), method="lm", formula = duration ~
        poly(temperature, 2))

ggplot(quadratic.model, aes(temperature, duration) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))


library(ggplot2)
ggplot(data=data.training, aes(temperature, duration)) +
  geom_point() + 
  geom_smooth(method="lm", formula=duration~temperature+temperature2)
