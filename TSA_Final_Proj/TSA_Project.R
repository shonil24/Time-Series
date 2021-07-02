options(warn=-1)

library(fpp2)
library(TSA)
library(tseries)
library(forecast)
library(lmtest)
library(readr)
library(FitAR)

## Model Fitting 1.1
#################################################################### 
cafe.ts = ts(auscafe, start=c(2009,4), end=c(2017,9), frequency = 12)
plot(cafe.ts,type='o',ylab='Monthly expenditure(billions$)',
     main='Fig 1. Monthly expenditure on eating out in Australia from April:2009 to September:2017')
points(y=cafe.ts,x=time(cafe.ts), pch=as.vector(season(cafe.ts)))
# looking at the graph, there is evidence of seasonality. 
# linear upward trend (upward / downward line of mean)
# no intervention (changing point)
# no changing variance

## Correlation
#################################################################### 
# Scatter plot
plot(y=cafe.ts,x=zlag(cafe.ts),ylab='Monthly Expenditure', xlab='Previous Year Monthly Expenditure',
     col='Red',lwd=2,main='Fig 2. Correlation of Scatter Plot')

# Correlation 
y = cafe.ts
x = zlag(cafe.ts) 
index = 2:length(x) 
cor(y[index],x[index]) 

# Linear upward trend and high correlation of 0.96 is observed

# ACF and PACF plot
####################################################################

par(mfrow=c(1,2),main="Fig 3. ACF and PACF plot of cafe time series")
acf(cafe.ts,lag.max = 120,main="cafe time series")
pacf(cafe.ts,lag.max = 120,main="cafe time series")

# ACF has a decaying pattern and in sinusoidal form which means there is an existence of trend (decrease gradually as lags increase) and the ACF is periodic. The bottom is at lag 6, which means the cycle would complete at 12. 
# This implies that there are 12 lags which is same as a year.
# Most of the expenditure are not within the confidence interval which indicates significant auto-correlation in the expenditure.

# 2 Spikes at lag 1 which are significant followed by correlations that are insignificant indicates Auto regressive behavior.
# Since trends and seasonality exists, we conclude that the series is non-stationary and not white noise.

# From this we infer the non-seasonal AR(1 or 2) model (correlation at low lags).

# Differencing
####################################################################

# As we can see, the data is non-stationary and evidence of seasonality is present we will try SARIMA models.
# we do seasonal differencing to get rid of seasonal trend and fitting a plain model until time series 
# and ACF/PACF plots of residuals show no sign of seasonality.

# Taking the first seasonal difference, and seeing if the seasonal trend still exists or not
m1.cafe = arima(log(cafe.ts),order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12))
res.m1 = residuals(m1.cafe);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m1, lag.max = 100, main = "The sample ACF of the residuals")
pacf(res.m1, lag.max = 100, main = "The sample PACF of the residuals")
# There exists a trend as evident by the ts plot.
# Trend and seasonality is still seen, taking ordinary difference

###############################################################
#As there exists a sinusoidal pattern still we need to take ordinary difference
m2.cafe = arima(log(cafe.ts),order=c(0,1,0),seasonal=list(order=c(0,1,0), period=12))
res.m2 = residuals(m2.cafe);  
par(mfrow=c(1,1))
plot(res.m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m2, lag.max = 50, main = "The sample ACF of the residuals")
pacf(res.m2, lag.max = 50, main = "The sample PACF of the residuals")
# we see seasonal lags (1.2) in both ACF and PACF plot indicating SARMA(1,1). 

###############################################################

m3.cafe = arima(log(cafe.ts),order=c(0,1,0),seasonal=list(order=c(1,1,1), period=12))
res.m3 = residuals(m3.cafe);  
par(mfrow=c(1,1))
plot(res.m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m3, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m3, lag.max = 36, main = "The sample PACF of the residuals")
# Correlation is still present on seasonal lag (in ACF plot). We use high number of Q (MA) until filtering out seasonal lags.

###############################################################

m4.cafe = arima(log(cafe.ts),order=c(0,1,0),seasonal=list(order=c(1,1,2), period=12))
res.m4 = residuals(m4.cafe);  
par(mfrow=c(1,1))
plot(res.m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m4, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m4, lag.max = 36, main = "The sample PACF of the residuals")
# We do not see any seasonal lags but few significant lags before seasonal lag. Although trend is not observed, we still transform the data.
# From the plots, we consider, SARMA(1,2) for seasonal terms and for no-seasonal terms, we consider, AR(2) and MA(3) model. 

# To verify the stationarity adf test was used and we found the series stationary.. 
adf.test(res.m4)

## EACF
eacf(res.m4, ar.max = 3, ma.max = 3)
# We get, ARMA(0,1), ARMA(1,2), ARMA(2,1) and ARMA(3,2)

# BIC
BIC.cafe.ts = armasubsets(y=res.m4,nar=4,nma=4,y.name='test',ar.method='ols')
plot(BIC.cafe.ts,main="BIC table of differencing")

# The possible models are:-
# SARMA(2,1,0)(1,1,2) by ACF/PACF
# SARMA(0,1,1)(1,1,2) by EACF
# SARMA(1,1,2)(1,1,2) by EACF
# SARMA(2,1,1)(1,1,2) by EACF
# SARMA(3,1,2)(1,1,2) by EACF
# SARMA(2,1,3)(1,1,2) by ACF/PACF

#### Model Fitting
## Parameter estimations

model1.cafe = arima(log(cafe.ts),order=c(2,1,0),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model1.cafe)

model2.cafe = arima(log(cafe.ts),order=c(0,1,1),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model2.cafe)

model3.cafe = arima(log(cafe.ts),order=c(1,1,2),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model3.cafe)

model4.cafe = arima(log(cafe.ts),order=c(2,1,1),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model4.cafe)

model5.cafe = arima(log(cafe.ts),order=c(3,1,2),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model5.cafe)

model6.cafe = arima(log(cafe.ts),order=c(2,1,3),seasonal=list(order=c(1,1,2), period=12),method = "ML")
coeftest(model6.cafe)

## AIC | BIC
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

sc.AIC=AIC(model1.cafe,model2.cafe,model3.cafe,model4.cafe,model5.cafe,model6.cafe)
sc.BIC=AIC(model1.cafe,model2.cafe,model3.cafe,model4.cafe,model5.cafe,model6.cafe,k=log(length(cafe.ts)))

sort.score(sc.AIC, score = "aic")
sort.score(sc.BIC, score = "aic")

sarima.for(log(cafe.ts),120,1,1,2,1,1,2,12)

