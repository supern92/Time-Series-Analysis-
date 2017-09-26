
getwd()
setwd("/export/home/users/n/a/nahid/desktop")
ride<-read.csv("Ridership.csv")


summary(ride)

library(forecast)
#Exponential Smoothing
myts <- ts(ride[,1])
plot.ts(myts)

ses(ride[,1], alpha =.3, h=8)
ses(ride[,1], alpha =.9, h=8)
accuracy(myts)

#Holt-Winters exponential smoothing
Rideforecast<-ts(Rideforecast)
Rideforecast<-HoltWinters(myts, beta=FALSE, gamma=FALSE)
Rideforecast
# Model Accuracy 
accuracy(Rideforecast)

#Random walk with a drift 
Wride<-ts(Wride)
Wride<-rwf(myts, drift = T, h=8)
# Model Accuracy 
accuracy(Wride)

#conditional mean
condmean<-aggregate(ride, by=list(ride$Metro), mean)
plot(condmean[,1])
points(condmean[,2], type = "l", col = "blue")

# compare conditional mean with the timeseries
plot(ride$Metro)
points(condmean$LON, type = "l")
driftmyts<-diff(myts)

# compare with the lagged timeseries 
plot(driftmyts)
points(lagmyts, col = "blue", type = "l")

# error of conditional mean model
plot(c(1:length(ride[,1])), ride[,1] - condmean, col = "purple", type = "l")


