#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html

#install.packages("TTR")
library(TTR)
#install.packages("forecast")
library(forecast)

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
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(rainseriesforecasts2$residuals)
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(rainseriesforecasts2$residuals)
##Holt’s Exponential Smoothing
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE
plot(skirtsseriesforecasts)
HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)
skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=19)
plot.forecast(skirtsseriesforecasts2)
acf(skirtsseriesforecasts2$residuals, lag.max=20)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(skirtsseriesforecasts2$residuals)            # make a time plot
plotForecastErrors(skirtsseriesforecasts2$residuals) # make a histogram
## Holt-Winters Expoential Smoothing
logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2 <- forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
plot.forecast(souvenirtimeseriesforecasts2)
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(souvenirtimeseriesforecasts2$residuals)            # make a time plot
plotForecastErrors(souvenirtimeseriesforecasts2$residuals) # make a histogram

#ARIMA Models
##Differencing a Time Series
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)
skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)
kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)
##Selecting a Candidate ARIMA Model
acf(kingtimeseriesdiff1, lag.max=20)             # plot a correlogram
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the autocorrelation values
pacf(kingtimeseriesdiff1, lag.max=20)             # plot a partial correlogram
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the partial autocorrelation values
#The auto.arima() function can be used to find the appropriate ARIMA model, eg., type “library(forecast)”, then “auto.arima(kings)”. The output says an appropriate model is ARIMA(0,1,1).
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)
acf(volcanodustseries, lag.max=20)             # plot a correlogram
acf(volcanodustseries, lag.max=20, plot=FALSE) # get the values of the autocorrelations
pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)
#Forecasting Using an ARIMA Model
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesarima
kingstimeseriesforecasts <- forecast.Arima(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot.forecast(kingstimeseriesforecasts)
acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(kingstimeseriesforecasts$residuals)            # make time plot of forecast errors
plotForecastErrors(kingstimeseriesforecasts$residuals) # make a histogram
volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima
volcanodustseriesforecasts <- forecast.Arima(volcanodustseriesarima, h=31)
volcanodustseriesforecasts
plot.forecast(volcanodustseriesforecasts)
acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(volcanodustseriesforecasts$residuals)            # make time plot of forecast errors
plotForecastErrors(volcanodustseriesforecasts$residuals) # make a histogram


















