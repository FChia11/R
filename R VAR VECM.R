
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

#1# TB3

TB3_ts <- ts(TB3, frequency = 4, start = C(1977, 1))
ts.plot (TB3_ts)

logTB3_ts <- log(TB3_ts)
ts.plot(logTB3_ts)

dlogTB3_ts <- diff(log(TB3_ts))
ts.plot(dlogTB3_ts)

#2# M1

M1_ts <- ts(M1,frequency=4,start=c(1977,1))
ts.plot (M1_ts)

logM1_ts <- log(M1_ts)
ts.plot(logM1_ts)

dlogM1_ts <- diff(log(M1_ts))
ts.plot(dlogM1_ts)

#3# IPI 

IPI_ts <- ts(IPI, frequency = 4, start=c(1977,1))
ts.plot (IPI_ts)

logIPI_ts <- log(IPI_ts)
ts.plot(logIPI_ts)

dlogIPI_ts <- diff(log(IPI_ts))
ts.plot(dlogIPI_ts)

library(vars)
dlogdata<-data.frame(diff(log(TB3)), diff(log(M1)),diff(log(IPI)))
names(dlogdata)<-c("dlogTB3","dlogM1","dlogIPI")
attach(dlogdata)
fit_var1<-VAR(dlogdata,type="const",p=1)
summary(fit_var1)
var1_residuals<-resid(fit_var1)
par(mfrow=c(2,2))
acf(var1_residuals[,1])
acf(var1_residuals[,2])
ccf(var1_residuals[,1],var1_residuals[,2])
par(mfrow=c(1,1))

VARselect(dlogdata,lag.max=10,type="const")
irf_var<-irf(fit_var1,ortho=F,boot=T)
plot(irf_var)

serial.test(fit_var1, lags.pt=20)

regressors <- tail(dlogdata[4:124], 20)
fct <- predict(fit_var1, n.ahead = 20)

