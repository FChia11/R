rm(list=ls())

########################
# U.K. 3-Month T-Bill Rate #
########################

# Set working directory in RStudio:
# Session -> Set Working Directory -> To Source File Location (or Chose a Directory...)
setwd("...") 

mydata <- read.table(file="ukdata.txt",header=TRUE,dec=".")
attach(mydata)
head(mydata)

#UNIVARIATE ANALYSIS#

#1# TB3

TB3_ts <- ts(TB3,frequency=4,start=c(1977,1))
ts.plot (TB3_ts)

logTB3_ts <- log(TB3_ts)
ts.plot(logTB3_ts)

library(fGarch)

fit_garchb<-garchFit(~arma(1,1)+garch(0,1), include.mean = F ,data=TB3)
summary(fit_garchb)
plot(fit_garchb)

library(forecast)

plot(fit_garchb)
lines(log(TB3_ts), col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("log(TB3_ts)","GARCHPred"))

accuracy(fit_garchb)

av.res(fit_garchb)

