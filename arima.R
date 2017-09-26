
getwd()
setwd("/export/home/users/n/a/nahid/desktop")
GNP=read.csv("GNP_Quarterly1.csv")
library(forecast)
myts<-ts(GNP)

plot(myts)
# Autocorrelation and autocovariance plots
acf(myts)
acf(myts, type = "covariance")
# covariance
lagmyts<-(myts[1]-myts[1])
for (i in 1:(length(myts)-12)){
  lagmyts[i+12]<-myts[i+12]-myts[i]
  lagmyts[is.na(lagmyts)]<-0
}
cov(myts, lagmyts)

# correlation
cor(myts, lagmyts)

# autocorrelation values:
acf(myts, plot = F)
acf(myts, plot = F)$acf


## partial autocorrelation function
pacf(myts)


str(myts)
str(GNP)
head(GNP)



mytsdiff<-diff(myts, differences=1)
plot.ts(mytsdiff)

mytsdiff2<-diff(mytsdiff,diffrences=2)
plot.ts(mytsdiff2)


acf(myts, lag.max=10)           
acf(myts, lag.max=10, plot=FALSE)

library(forecast)

install.packages("tseries")
library(tseries)
adf.test(myts)

#building Arima model 
auto.arima(GNP[,1])


fit2 <- Arima(GNP[,1], order=c(2,0,1))
fit2

fit4<-fit2 <- Arima(GNP[,1], order=c(1,0,1))
fit4

predict(fit2,4)


 # forcasting the time series
GNPforecast<-arima(myts,order=c(2,0,2))
