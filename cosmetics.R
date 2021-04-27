##################################
### Regression Analysis        ###
### Homework 3: Cosmetics data ###
##################################

# Libraries required: ridge

rm(list = ls())

# Load cosmetics data
cosmetics <- read.table("cosmetics.txt", header = TRUE)
head(cosmetics)
summary(cosmetics)
attach(cosmetics)

# Scatter plot
pairs(cosmetics)
pairs(cosmetics, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

# S ~ X1 + X2 + X3
LS <- lm(S ~ X1 + X2 + X3, data = cosmetics)
LS
LS.sum <- summary(LS)
LS.sum
# no significant variables

#-------------------------------#
# Multicollinearity diagnostics #
#-------------------------------#

# Correlation
cosmetics.cor <- cor(cosmetics)
cosmetics.cor
corx <- cosmetics.cor[-1,-1]
# large correlation between X1 and X2

# VIF
cosmetics.VIF <- diag(solve(corx))
cosmetics.VIF
# VIF values for X1 and X2 are large

# Eigenvalues
corx.eig <- eigen(corx)$values
corx.eig
sqrt(max(corx.eig)/corx.eig)
# third eigenvalue is very small

detach(cosmetics)

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
