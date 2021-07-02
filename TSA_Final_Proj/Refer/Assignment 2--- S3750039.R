rm(list=ls())

library(TSA)
library(fUnitRoots)
library(lmtest)
library(tseries)
library(FitAR)
library(forecast)
setwd('C:/Users/Wajahath/Desktop/Analytics/Sem 3/Time Series/Assignment 2')

source('C:/Users/Wajahath/Desktop/Analytics/Sem 3/Time Series/Tasks/Module 6/sort.score.R')
source('C:/Users/Wajahath/Desktop/Analytics/Sem 3/Time Series/Tasks/Module 6/residual.analysis.R')

#--------------------------IMPORT DATA-------------------------------#
eggs <- read.csv('eggs.csv',header=FALSE, skip = 1)$V2 #READ SECOND COL
class(eggs)
eggs = ts(as.vector(eggs), start = 1981, end = 1996) #TIME SERIES
class(eggs)
eggs
plot(eggs, type='o', ylab='Egg depositions', col = 'Blue', lwd = 2,
     xlab = 'Years', main = 'Time series -- Egg deposition')
# Follows AR behaviour
# No Intervention
# Increasing trend
# Changing variance not really
# No Seasonality

#-----------------------------CORRELATION-----------------------------#
#Scatter plot
plot(y=eggs,x=zlag(eggs),ylab='Deposition', xlab='Previous Year Deposition',
     col = 'Blue', lwd = 2,main= 'Scatter plot for Correlation')
#Correlation check
y = eggs 
x = zlag(eggs) 
index = 2:length(x) 
cor(y[index],x[index]) 
# HIGH CORR -- 0.7445

#-----------------------------AUTOCORRELATION------------------------#
par(mfrow=c(1,2))
acf(eggs, main='ACF TS')
pacf(eggs, main='PACF TS')

shapiro.test(eggs)
#ADF test for stationarity
adf.test(eggs)
#SERIES IS NON-STATIONARY. Therefore, transformation required

#-----------------------------TRANSFORMATION--------------------------------#
par(mfrow=c(1,1))
eggs.trans = BoxCox.ar(eggs)
eggs.trans = BoxCox.ar(eggs, method = "yule-walker")
eggs.trans$ci
lambda = 0.5
eggs.trans = (eggs^lambda-1)/lambda
plot(eggs.trans,type='o',ylab='Yearly changes in the Deposition of Eggs',
     xlab ='Year',col = 'Blue', lwd = 2, main = 'BoxCox transformed Time Series')


qqnorm(eggs.trans)
qqline(eggs.trans, col = 'Blue')
shapiro.test(eggs.trans)
#----------------------------DIFFERENCING----------------------------------#
diff.eggs.trans = diff(eggs.trans)
par(mfrow=c(1,1))
plot(diff.eggs.trans,type='o',ylab='Egg Depositions', xlab='Year',
     col = 'Blue', lwd = 2,main='Time series plot after differencing')

adf.test(diff.eggs.trans)

# diff.eggs.trans1 = diff(eggs.trans, difference = 2)
# adf.test(diff.eggs.trans1)
# 
# diff.eggs.trans2 = diff(eggs.trans, difference = 3)
# adf.test(diff.eggs.trans2)

par(mfrow=c(1,2))
acf(diff.eggs.trans, main='ACF of differenced series')
pacf(diff.eggs.trans,main='PACF of differenced series')

#EACF
par(mfrow=c(1,1))
eacf(diff.eggs.trans, ar.max = 3, ma.max = 3)
# Possible models:
# ARIMA(0,1,1),ARIMA(1,1,0),ARIMA(1,1,1)
# ARIMA(2,1,0),ARIMA(2,1,1),ARIMA(2,1,2)
# ARIMA(3,1,0),ARIMA(3,1,1),ARIMA(3,1,2)

res = armasubsets(y=diff.eggs.trans,nar=3,nma=3,y.name='test',ar.method='ols')
plot(res)
# ARIMA(1,1,0),ARIMA(2,1,0),ARIMA(3,1,0)

#-------------------------------MODEL FITTING-------------------------------#
#ARIMA(0,1,1)
mod_011_css = arima(eggs.trans, order = c(0,1,1), method = 'CSS')
coeftest(mod_011_css)

mod_011_ml = arima(eggs.trans, order = c(0,1,1), method = 'ML')
coeftest(mod_011_ml)

#ARIMA(1,1,0)
mod_110_css = arima(eggs.trans, order = c(1,1,0), method = 'CSS')
coeftest(mod_110_css)

mod_110_ml = arima(eggs.trans, order = c(1,1,0), method = 'ML')
coeftest(mod_110_ml)

#ARIMA(1,1,1)
mod_111_css = arima(eggs.trans, order = c(1,1,1), method = 'CSS')
coeftest(mod_111_css)

mod_111_ml = arima(eggs.trans, order = c(1,1,1), method = 'ML')
coeftest(mod_111_ml)

#ARIMA(2,1,0)
mod_210_css = arima(eggs.trans, order = c(2,1,0), method = 'CSS')
coeftest(mod_210_css)

mod_210_ml = arima(eggs.trans, order = c(2,1,0), method = 'ML')
coeftest(mod_210_ml)

#ARIMA(2,1,1)
mod_211_css = arima(eggs.trans, order = c(2,1,1), method = 'CSS')
coeftest(mod_211_css)

mod_211_ml = arima(eggs.trans, order = c(2,1,1), method = 'ML')
coeftest(mod_211_ml)

#ARIMA(2,1,2)
mod_212_css = arima(eggs.trans, order = c(2,1,2), method = 'CSS')
coeftest(mod_212_css)

mod_212_ml = arima(eggs.trans, order = c(2,1,2), method = 'ML')
coeftest(mod_212_ml)

#ARIMA(3,1,0)
mod_310_css = arima(eggs.trans, order = c(3,1,0), method = 'CSS')
coeftest(mod_310_css)

mod_310_ml = arima(eggs.trans, order = c(3,1,0), method = 'ML')
coeftest(mod_310_ml)

#ARIMA(3,1,1)
mod_311_css = arima(eggs.trans, order = c(3,1,1), method = 'CSS')
coeftest(mod_311_css)

mod_311_ml = arima(eggs.trans, order = c(3,1,1), method = 'ML')
coeftest(mod_311_ml)

#ARIMA(3,1,2)
mod_312_css = arima(eggs.trans, order = c(3,1,2), method = 'CSS')
coeftest(mod_312_css)

mod_312_ml = arima(eggs.trans, order = c(3,1,2), method = 'ML')
coeftest(mod_312_ml)

sort.score(AIC(mod_011_ml,mod_110_ml,mod_111_ml,
               mod_210_ml,mod_211_ml,mod_212_ml,
               mod_310_ml,mod_311_ml,mod_312_ml), 
           score = 'aic')

sort.score(BIC(mod_011_ml,mod_110_ml,mod_111_ml,
               mod_210_ml,mod_211_ml,mod_212_ml,
               mod_310_ml,mod_311_ml,mod_312_ml), 
           score = 'bic')

#------------------------Overfitting--------------------------#

# ARIMA(3,1,2)
mod_312_css = arima(eggs.trans,order=c(3,1,2),method='CSS')
coeftest(mod_312_css)


# ARIMA(2,1,3)
mod_213_css = arima(eggs.trans,order=c(2,1,3),method='CSS')
coeftest(mod_213_css)

#---------------------------Residual Analysis--------------------#
residual.analysis(mod_212_css)
residual.analysis(mod_213_css)

#---------------------------Forecasting--------------------------#
install.packages('forecast')
library(forecast)
par(mfrow=c(1,1))
fit = Arima(eggs, model = mod_212_css, lambda=0.5)
plot(forecast(fit,h=5), ylab = 'Egg deposition in millions', ylim = c(-2,4),
     xlim = c(1982,2001), type = 'l', main = 'Egg deposition forecast using ARIMA(2,1,2)')



#References
#https://stackoverflow.com/questions/42649126/how-to-specify-line-thickness-in-points-in-an-r-plot