#Alaina Rose Plaza | 2 BS Applied Mathematics
#AMAT 132 | Forecasting Final Requirement

###################################################################

#BEGIN

#check working directory
getwd()

#import .csv file to global environment
phtemp <- read.csv("TemperatureinCentury.csv", header = TRUE)

#check content of imported file
head (phtemp)
tail(phtemp)

###################################################################

#FEATURE CONSTRUCTION

library(fpp2)

#create time series
month.ts <-ts(phtemp[,2], start=c(1900, 1), frequency = 12)
print(month.ts)


#create time plot
autoplot(month.ts) +
  ggtitle("Temperature in the Philippines in a Century") +
  xlab("Date") +
  ylab("Temperature")

#check for seasonality
ggseasonplot(month.ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Temperature") +
  ggtitle("Seasonal plot:Temperature in the Philippines in a Century")

#FINDINGS: Data has seasonality. Peak in temperature every month of May.
#FINDINGS: Graphically seen that data is not stationary, data transformation must be done.

###################################################################

#FEATURE SELECTION AND TRANSFORMATION


library(urca)

#unit root test: to determine if differencing is needed 
month.ts%>% ur.kpss()%>% summary()

#FINDINGS: test statistic: 7.0046 > 0.739 
#FINDINGS: hence data is not stationary.


#unit root test: for differenced data
month.ts %>% diff() %>% ur.kpss() %>% summary()

#FINDINGS: test statistic: 0.0047 < 0.739. 
#FINDINGS: hence data will be stationary after differencing.

#determine appropriate number of differencing
ndiffs(month.ts)

#FINDINGS:[1] 1. Thus we need to do first order differencing.


#DIFFERENCING

#first order differencing
first.dif <- diff(month.ts)

#time plot of transformed data / first difference
autoplot(first.dif) +
  ggtitle("First Difference Temperature in the Philippines in a Century") +
  xlab("Date") +
  ylab("Temperature")

#seasonal plot appears trend
ggseasonplot(first.dif) +
  ggtitle("Seasonality Plot of First Difference Temperatures") +
  xlab("Date") +
  ylab("Temperature")

#subseries plot
ggsubseriesplot(first.dif)+
  ggtitle("Subseries Plot of First Difference Temperatures") +
  xlab("Date") +
  ylab("Temperature")

#FINDINGS: transformed data still has seasonality but is already stationary.

###################################################################

#MODEL TRAINING

#first forecasting method: Seasonal Naive Method
fit.snaive <- snaive(first.dif)
print(summary(fit))
checkresiduals(fit)

#FINDINGS: residual sd: 0.4585

#second forecasting method: Exponential Smoothing
fit.ets <-ets(month.ts)
print(summary(fit.ets))
checkresiduals(fit.ets)

#FINDINGS: residual sd: 0.3031


#third forecasting method: ARIMA ACF
fit.arima <- auto.arima(month.ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit.arima))
checkresiduals(fit.arima)

#Best model: ARIMA(0,1,3)(2,1,0)[12]
#FINDIGS: residual sd = 0.3354102

###################################################################

#FORECASTING WITH ARIMA

#forecasting 5 years from 2013
forecast.arima1 <- forecast(fit.arima, h=60)
autoplot(forecast.arima)

#forecasting 5 years from 2013, include last 120 months
forecast.arima2 <- forecast(fit.arima, h=60)
autoplot(forecast.arima2, include=120)
print(summary(forecast.arima1))

#END
