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

library(forecast)

arimaMod <- auto.arima(log(TB3_ts), stepwise = FALSE, approximation = FALSE)
arimaMod.Fr <- forecast(arimaMod, h=20)

plot(arimaMod.Fr)
lines(log(TB3_ts), col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("log(TB3_ts)","ARIMAPred"))

accuracy(arimaMod.Fr)


library(fGarch)

fit_garch<-garchFit(~arma(1,1)+garch(1,1), include.mean = F ,data=log(TB3))
summary(fit_garch)
plot(fit_garch)
fit_garch.Fr <- predict(fit_garch, 20, plot=T)

plot(fit_garch.Fr)
lines(log(TB3_ts), col="pink")
legend("topleft", lty = 1, bty = "n", col = c("green", "black"), c(log(TB3_ts), "GARCHPred"))

fit_garch <- garchFit(arma(1,1)~ garch(1,1), data = log(TB3_ts), trace = FALSE)
fit_garch_predict <- predict(fit_garch, n.ahead = 20)
plot(fit_garch_predict$meanForecast,main="log(TB3_ts) - 
