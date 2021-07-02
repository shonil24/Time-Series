rm(list=ls())

# loading the libraries
library(TSA) 
library(readr)

# Reading the dataset contents
ozone <- read.csv("C:/Users/winuser/Downloads/Projects/Time_Series/data1.csv", header = FALSE)
head(ozone)

# Setting time series object to data frame and checking the class
ozone <- ts(as.vector(ozone), start = 1927, end = 2016, frequency = 1) 
class(ozone)

# Plotting time series data
plot(ozone, type = 'o', ylab = 'Thickness of ozone layer (Dobson units)', xlab = 'Year', main = 'Time series plot for thickness of Ozone layer', col = 'blue')

# Plotting scatter plot with respect to previous year
plot(y = ozone, x = zlag(ozone), ylab = "Thickness of Ozone layer (Dobson Units)", xlab="Previous year Ozone Thickness", main = "Scatter plot of neighboring thickness", col="blue")

# Correlation 
y = ozone # Read the data in to y
x = zlag(ozone) #Generate first lag of the series
index = 2:length(x) # ignore the NA
cor(y[index],x[index]) #Calculate the correlation 

## -----------------------------------------------------------------------------------------

# Linear trend model
ozone.model1 = lm(ozone~time(ozone)) #label the model as ozone.model1 
summary(ozone.model1)

# Plotting linear model
plot(ozone, type = 'o', ylab = 'Thickness of Ozone layer (Dobson Units)', xlab = 'Year', main = "Fitted linear model to the Ozone Thickness", col = "blue")
abline(ozone.model1, col = "red") # add the fitted least squares line from model1

# Residual analysis
# Standardized Residuals
plot(y = rstudent(ozone.model1), x = as.vector(time(ozone)), type='o', ylab = 'Standardized Residuals', xlab = 'Year', main = "Time Series plot of residuals", col="blue", ylim = c(-3,3))

# Fitted Residuals
plot(y = rstudent(ozone.model1), x = as.vector(fitted(ozone.model1)), type = 'n', ylab = 'Standardized Residuals', xlab = 'Fitted Trend Values', main = "Time series plot of standardised residuals 
     versus fitted trend values.", col = "blue")
points(y = rstudent(ozone.model1), x = as.vector(fitted(ozone.model1)))

# Histogram
hist(rstudent(ozone.model1), xlab = 'Standardized Residuals', main = "Histogram of the standardized residuals from 
     the Linear Trend model", col = "orange", xlim = c(-3,3))

# Normal QQ plot
qqnorm(y = rstudent(ozone.model1), main = "Normal Q-Q plot of the standardized residuals from 
     the Linear Trend model", col = "blue")
qqline(y = rstudent(ozone.model1), col = "red", lwd = 1)

# Shapiro-Wilk Normality test
shapiro.test(rstudent(ozone.model1))

# Sample Auto-correlation Function
acf(rstudent(ozone.model1), main = "ACF of standardized residuals")

## ------------------------------------------------------------------------------------------

# Quadratic model
t = time(ozone)
t2 = t^2
ozone.model2 = lm(ozone~t+t2) # label the model as ozone.model2
summary(ozone.model2)

# Plotting fitted quadratic curve
plot(ts(fitted(ozone.model2)), ylim = c(min(c(fitted(ozone.model2), as.vector(ozone))), 
                                        max(c(fitted(ozone.model2), as.vector(ozone)))), 
                                        ylab='Thickness of Ozone layer (Dobson Units)' , xlab='Year',
                                        main = "Fitted quadratic curve to the Ozone layer thickness data", col = "red")
lines(as.vector(ozone), type="o", col = "blue")
legend ("topright", lty = 1, pch = 1, col = c("blue","red"), text.width = 25,
        c("Time Series Plot","Quadratic Trend Line"))

# Residual analysis
# Standardized Residuals
plot(y = rstudent(ozone.model2), x = as.vector(time(ozone)), type='o', ylab = 'Standardized Residuals', xlab = 'Year', main = "Time Series plot of residuals", col="blue", ylim = c(-3,3))

# Fitted Residuals
plot(y = rstudent(ozone.model2), x = as.vector(fitted(ozone.model2)), type = 'n', ylab = 'Standardized Residuals', xlab = 'Fitted Trend Values', main = "Time series plot of standardised residuals 
     versus fitted trend values.", col = "blue")
points(y = rstudent(ozone.model2), x = as.vector(fitted(ozone.model2)))

# Histogram
hist(rstudent(ozone.model2), xlab = 'Standardized Residuals', main = "Histogram of the standardized residuals from 
     the Quadratic Trend model", col = "orange", xlim = c(-3,3))

# Normal QQ plot
qqnorm(y = rstudent(ozone.model2), main = "Normal Q-Q plot of the standardized residuals from 
     the Quadratic Trend model", col = "blue")
qqline(y = rstudent(ozone.model2), col = "red", lwd = 1)

# Shapiro-Wilk Normality test
shapiro.test(rstudent(ozone.model2))

# Sample Auto-correlation Function
acf(rstudent(ozone.model2), main = "ACF of standardized residuals")

## ------------------------------------------------------------------------------------------

# Harmonic model
har.=harmonic(ozone, 0.45) # calculate cos(2*pi*t) and sin(2*pi*t)
ozone.model3 = lm(ozone~har.)
summary(ozone.model3)

# Plotting fitted cosine curve
plot(ts(fitted(ozone.model3)), ylim = c(min(c(fitted(ozone.model3),as.vector(ozone))), 
                                        max(c(fitted(ozone.model3),as.vector(ozone)))), 
     ylab="Thickness of Ozone layer (Dobson Units)", xlab="Year", 
     main = "Fitted harmonic curve to Ozone layer thickness data", type = "l", lty = 1, col = "red")
lines(as.vector(ozone), type="o", col = "blue")
legend ("topright", lty = 1, pch = 1, col = c("blue","red"), text.width = 25,
        c("Time Series Plot","Harmonic Trend Line"))

## ----------------------------------------------------------------------------

# Forcasting
t = c(2017, 2018, 2019, 2020, 2021)
t2 = t^2
pred = data.frame(t,t2)
forecast = predict(ozone.model2, pred, interval = "prediction")
print(forecast)

# Plotting the forcast data
plot(ozone, xlim = c(1926, 2022), ylim = c(-15, 5), type="o", ylab="Thickness of Ozone Layer (Dobson Units)" , main = " Forcasting using quadratic trend model", col="blue")
lines(ts(as.vector(forecast [,1]), start = c(2017,1), frequency = 1), col="red", type="l", lwd=2)
lines(ts(as.vector(forecast [,2]), start = c(2017,1), frequency = 1), col="darkgreen", type="l", lwd=2)
lines(ts(as.vector(forecast [,3]), start = c(2017,1), frequency = 1), col="darkgreen", type="l", lwd=2)