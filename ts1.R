#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html

#basics
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries = ts(kings)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))

plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)

logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

#decomposition
## non-seasonal
#install.packages("TTR")
library(TTR)
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)
## seasonal
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal #estimated
plot(birthstimeseriescomponents)
## seasonally adjusting
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
par(mfrow=c(2,1))
plot(birthstimeseriescomponents$x)
plot(birthstimeseriesseasonallyadjusted)
par(mfrow=(c(1,1)))

#Forecasts using Exponential Smoothing
## Simple Exponential Smooting
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
head(rainseriesforecasts$fitted)
#plot(rainseriesforecasts$fitted)
rainseriesforecasts$SSE
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)
#install.packages("forecast")
library(forecast)
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
rainseriesforecasts2
plot.forecast(rainseriesforecasts2)
acf(rainseriesforecasts2$residuals, lag.max=20)



