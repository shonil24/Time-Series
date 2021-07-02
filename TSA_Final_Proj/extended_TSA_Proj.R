options(warn=-1)

library(fpp2)
library(TSA)
library(tseries)
library(forecast)
library(lmtest)
library(readr)
library(FitAR)
###################################################################

sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}
residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH", "fGARCH")[1]){
  library(TSA)
  library(FitAR)
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  LBQPlot(res.model, lag.max = 30, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}

## Model Fitting 1.1
#################################################################### 
cafe.ts = ts(auscafe, start=c(2009,4), end=c(2017,9), frequency = 12)
plot(cafe.ts,type='o',ylab='Monthly expenditure(billions$)',
     main='Fig 1. Monthly expenditure on eating out in Australia from April:2009 to September:2017')
points(y=cafe.ts,x=time(cafe.ts), pch=as.vector(season(cafe.ts)))
## looking at the graph, there is evidence of seasonality. 
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

# Normality and stationarity 1.2 [Not needed]. We have seasonality by default its non-stationary so differencing is needed.
####################################################################

# Fig 4.1 and 4.2
#adf.test(cafe.ts)
#As p greater than 0.05, null hypothesis (non-stationary evidence) is not rejected.
#shapiro.test(cafe.ts)
#As p less than 0.05, null hypothesis (normal distribution) is rejected.

# The differences of p less in normality test and greater in ADF test, it is clear that the data has non-stationarity (mean not constant) and isn't normally distributed.

#[Not Needed: Save for reference] - Only when trend exists and no seasonality.
# Transformation 1.3
####################################################################

# We will transform the data to stabilize the changing variance and mean of the series (non-stationarity to stationarity).
# Note: transform only when there is a white noise, here it isn't

#par(mfrow=c(1,1),main="Fig 5.1.a Box Cox Transformation")
#cafets.trans = BoxCox.ar(cafe.ts)
#cafets.trans$ci

# To apply log and power transformation the lambda should not lie within -2 and 2.
# However, the confidence interval lies within -1.1 and -0.3 which suggests that log and power transformation are not needed.

# To further confirm about the normality, we will use shapiro test and QQ plot.

#lambda = -1
#cafets.trans = (cafe.ts^lambda-1)/lambda
#plot(cafets.trans,type='o',ylab='Yearly changes in the expenditure of cafes, restaurants and takeway food services',xlab ='Year',col = 'Red', lwd = 2, main = 'Fig 5.1.b Time Series of Box Cox transformation')

# We still observe a linear upward trend and seasonality (non-stationarity).

#qqnorm(cafets.trans, main = "Fig 5.2. Normal Q-Q Plot ")
#qqline(cafets.trans, col = 'Red', lwd = 1)
#shapiro.test(cafets.trans)

# In Fig 5.2, we can see that tail end departs from straight line and in shapiro test, p-value is less than 0.05 which means the series does not have normal distribution.

# Differencing 1.4
####################################################################

# [Not needed]
# We will use non-seasonal differencing to detrend the model and seasonal differencing to make the series into stationary (to lose seasonality)
# diff.cafets.trans1 = diff(cafets.trans)
#adf.test(diff.cafets.trans1) # p = 0.01
#shapiro.test(diff.cafets.trans1) # p = 0.11

#par(mfrow=c(1,1))
#plot(diff.cafets.trans1,type='o',ylab='Expenditure', xlab='Year',col = 'Red', lwd = 2,main='Fig 6.1. Time series plot after first order differencing')

# There are no components defining non-stationarity in the plot which confirms the stationarity.

### ACF and PACF plot
#par(mfrow=c(1,2),main='Fig 6.2. ACF and PACF plot after first order differencing')
#acf(diff.cafets.trans1,main='ACF of differenced series', lag.max = 120)
#pacf(diff.cafets.trans1,main='PACF of differenced series', lag.max = 120)

# The ACF plot still has a trend (slowly decreasing values (only positive)) and seasonality (repeated values within boundary). Whereas, in PACF plot, we see 1 significant lag and lags with decreasing values.

#qqnorm(diff.cafets.trans1, main = "Fig 6.3. Normal Q-Q Plot ")
#qqline(diff.cafets.trans1, col = 'Red', lwd = 1)

# In QQ plot, All the points lie at one end except for 2 and the some of the points lie beyond straight line.
# So we see that normalization is improved.
#-------------------------------------------------------

# Since we found both seasonality and trend in the plot, we will use seasonal differencing.

diff.cafets12 = diff(cafe.ts,  differences = 12)
adf.test(diff.cafets12) # p = 0.01

# From  adf test, we confirm that data is stationary.

### ACF and PACF plot
par(mfrow=c(1,2),main='Fig 4. ACF and PACF plot seasonal differencing')
acf(diff.cafets12,main='ACF of differenced series', lag.max = 120)
pacf(diff.cafets12,main='PACF of differenced series', lag.max = 120)

plot(diff.cafets12) # no component exists in this plot

#-- seasonality is observed. checking for doubled seasonal difference
double_diff = diff(diff.cafets12,  differences = 1)
adf.test(double_diff) # p = 0.01

### ACF and PACF plot
par(mfrow=c(1,2),main='Fig 4. ACF and PACF plot seasonal differencing')
acf(double_diff,main='ACF of differenced series', lag.max = 120)
pacf(double_diff,main='PACF of differenced series', lag.max = 120)

plot(diff.cafets12) # no component exists in this plot

# Adf test shows evidence of stationarity.
# Although, seasonality exists, the amount of remaining auto-correlation (in ACF plot) is small i.e. tendency to return to its mean which suggests series as stationary with no long-term trend.
# Note: the value of negative correlation should not be more than -0.5(2-3 values may be considered but only if after that there is sudden drop).   
# Note: we see >-0.5 correlation

############### GOT  STUCk. STOP HERE. SEE TSA_Project

### EACF
par(mfrow=c(1,1))
eacf(diff.cafets.trans1, ar.max = 3, ma.max = 3)


####################################################################
# As we can see, the data is non-stationary and evidence of seasonality is present we will
#try SARIMA models.

##Taking the first seasonal difference, and seeing if the seasonal trend still exists or not
#we do seasonal differencing to get rid of seasonal trend and fitting a plain model until time series 
#and ACF/PACF plots of residuals show no sign of seasonality.
m1.cafe = arima(log(cafe.ts),order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12))
res.m1 = residuals(m1.cafe);  
par(mfrow=c(1,1))
plot(res.m1,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m1, lag.max = 100, main = "The sample ACF of the residuals")
pacf(res.m1, lag.max = 100, main = "The sample PACF of the residuals")
# There exists a trend as evident by the ts plot.

###############################################################
#As there exists a sinusoidal pattern still we need to take ordinary difference
m2.cafe = arima(log(cafe.ts),order=c(0,1,0),seasonal=list(order=c(0,1,0), period=12))
res.m2 = residuals(m2.cafe);  
par(mfrow=c(1,1))
plot(res.m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m2, lag.max = 36, main = "The sample ACF of the residuals")
pacf(res.m2, lag.max = 36, main = "The sample PACF of the residuals")
# There seems to have no seasonality and trend. 

###############################################################


## 
m3.cafe = arima(log(cafe.ts),order=c(1,1,1),seasonal=list(order=c(0,1,0), period=12))
res.m3 = residuals(m3.cafe);  
par(mfrow=c(1,1))
plot(res.m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m3, lag.max = 48, main = "The sample ACF of the residuals")
pacf(res.m3, lag.max = 48, main = "The sample PACF of the residuals")


###############################################################

## There is significant lag at in acf and at 2 in pacf. so, we will take
## 
m4.cafe = arima(log(cafe.ts),order=c(1,1,1),seasonal=list(order=c(1,1,1), period=12))
res.m4 = residuals(m4.cafe);  
par(mfrow=c(1,1))
plot(res.m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals")
par(mfrow=c(1,2))
acf(res.m4, lag.max = 48, main = "The sample ACF of the residuals")
pacf(res.m4, lag.max = 48, main = "The sample PACF of the residuals")
adf.test(res.m4)
eacf(res.m4)
#SARIMA(1,1,2)(1,1,1)
#SARIMA(2,1,1)(1,1,1)
#SARIMA(2,1,2)(1,1,1)


model2.cafe = arima(cafe.ts,order=c(1,1,1),seasonal=list(order=c(1,1,1), period=12),method = "ML")
model3.cafe = arima(cafe.ts,order=c(2,1,1),seasonal=list(order=c(1,1,1), period=12),method = "ML")
model4.cafe = arima(cafe.ts,order=c(1,1,2),seasonal=list(order=c(1,1,1), period=12),method = "ML")
model5.cafe = arima(cafe.ts,order=c(2,1,2),seasonal=list(order=c(1,1,1), period=12),method = "ML")
coeftest(model2.cafe)
coeftest(model3.cafe)
coeftest(model4.cafe)
coeftest(model5.cafe)

sc.AIC=AIC(model2.cafe,model3.cafe,model4.cafe, model5.cafe)
sc.BIC=AIC(model2.cafe,model3.cafe,model4.cafe, model5.cafe)
sort.score(sc.AIC, score = "aic")

sort.score(sc.BIC, score = "aic")

residual.analysis(model=model3.cafe)
coeftest(model3.cafe)



########################################################################

m_cafe = Arima(log(cafe.ts),order=c(2,1,1),seasonal=list(order=c(1,1,1), period=12), 
                        lambda = 0, method = "ML")
m_cafe = Arima(log(cafe.ts),order=c(2,1,1),seasonal=list(order=c(1,1,1), period=12))
preds1 = forecast(m_cafe, h = 10)
plot(preds1)

