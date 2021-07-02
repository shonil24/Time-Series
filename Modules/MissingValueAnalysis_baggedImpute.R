set.seed("1232")

# library(devtools); install_github("demirhanhaydar/baggedImpute")

library(baggedImpute)
library(imputeTS)

data = ts(rnorm(100, 0, 1))

plot(data, type ="o")
data[ceiling(runif(10, 1,100))] <- NA
print(data)

x <- baggedImpute(data = data, bootrep = 1000, blocklen = 2, 
                  BBM = "NBB", misstr = "ams", 
                  interpolation = "Stineman" )  
lines(x, col = "red")


# Second example
value <- read.csv("~/Desktop/MATH1318/Week11/purchaseValue.csv", header = FALSE)
value = ts(value,start=1976) #
par(mfrow=c(1,2))
plot(value,type='o',main="Time series plot of total purchase value")

NAs = c(5,10,15,33)
value.NA = value
value.NA[NAs] = NA
plot(value.NA,type='o',main="Time series plot of total purchase value")


diff.value = diff(value, differences = 2)

diff.value.NA = diff(value.NA, differences = 2)
par(mfrow=c(1,1))
plot(diff.value,type='o',ylab='Time series plot of the second difference of total purchase value')

plot(diff.value.NA,type='o',ylab='Time series plot of the second difference of total purchase value with missings')

valueImp <- baggedImpute(data = diff.value.NA, bootrep = 100, blocklen = 4, 
                  BBM = "NBB", misstr = "ams", 
                  interpolation = "Kalman" )  
par(mfrow=c(1,1))
plot(diff.value ,type='o',main="Time series plot of total purchase value")
lines(valueImp, type='b', col = "red")


valueImp <- baggedImpute(data = diff.value.NA, bootrep = 100, blocklen = 4, 
                         BBM = "NBB", misstr = "ams", 
                         interpolation = "Stineman" )  
par(mfrow=c(1,1))
plot(diff.value ,type='o',main="Time series plot of total purchase value")
lines(valueImp, type='b', col = "red")




valueImp <- na.interpolation(diff.value.NA, option = "linear")
par(mfrow=c(1,1))
plot(diff.value,type='o',main="Time series plot of total purchase value - Linear interpolation")
lines(valueImp, type='b', col = "red")

valueImp <- na.interpolation(diff.value.NA, option = "stine")
par(mfrow=c(1,1))
plot(diff.value,type='o',main="Time series plot of total purchase value - Stineman interpolation")
lines(valueImp, type='b', col = "red")

valueImp <- na.kalman(diff.value.NA, model = "StructTS", smooth = TRUE)
par(mfrow=c(1,1))
plot(diff.value,type='o',main="Time series plot of total purchase value")
lines(valueImp, type='b', col = "red")



# diff.valueImp.back = diffinv(valueImp, differences = 2, xi = matrix(value[c(1,2)], 2,1))
# value.Imputed = value.NA
# value.Imputed[NAs] = diff.valueImp.back[NAs]