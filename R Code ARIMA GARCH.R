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

#1#

fit_arima <- arima(logTB3_ts, order = c(1,1,0), seasonal = list(order = c(0,1,0)))
fit_arima

library(forecast)

fit_arima1 <- auto.arima(log(TB3_ts), stepwise = FALSE, approximation = FALSE)
fit_arima1.Fr <- forecast(fit_arima1, h=20)

plot(fit_arima1.Fr)
lines(log(TB3_ts), col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("log(TB3_ts)","ARIMAPred"))

accuracy(fit_arima1.Fr)

ts.plot(fit_arima1$residuals)
acf(fit_arima1$residuals)
Box.test(fit_arima1$residuals, lag = 4, type = "Ljung-Box")

AIC(fit_arima1)
AIC(fit_arima1, k=log(124))

#2#

fit_arima2 <- arima(logTB3_ts, order = c(2,1,0), seasonal = list(order = c(0,1,0)))
fit_arima2

library(forecast)

plot(fit_arima2.Fr)
lines(log(TB3_ts), col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("log(TB3_ts)","ARIMAPred"))

accuracy(fit_arima2.Fr)

ts.plot(fit_arima2$residuals)
acf(fit_arima2$residuals)
Box.test(fit_arima2$residuals, lag = 4, type = "Ljung-Box")

AIC(fit_arima2)
AIC(fit_arima2, k=log(124))
