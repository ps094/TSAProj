library(tidyr)
library(dplyr)
library(forecast)
library(fracdiff)
library(tseries)
df<-data.frame(read.csv("Data.csv"))

# Create a sample time series object
df <- ts(df$Electricity, frequency = 12)
plot(df,main="Electricity Production Time Series", ylab="Electricity")
kpss.test(df)
kpss.test(diff(df))

acf(diff(df))
pacf(diff(df))

df<-log(df)

train <- df[1:300]

auto.arima(train)
arima(train,order=c(2,1,2))
arima(train,order=c(1,1,2))
arima(train,order=c(3,1,2))
arima(train,order=c(2,1,3))
arima_model<-arima(train,order=c(2,1,3))
acf(arima_model$residuals)
pacf(arima_model$residuals)
kpss.test(arima_model$residuals)
plot(arima_model$residuals)

arima_model<-arima(train, order = c(2, 1, 3), seasonal = list(order = c(2, 1, 0), period = 12))
acf(arima_model$residuals)
pacf(arima_model$residuals)
kpss.test(arima_model$residuals)
plot(arima_model$residuals)

arima_forecast<-c()
#direct_recursive
for (i in 300:396){
  arima_model<-arima(df[1:i], order = c(2, 1, 3), seasonal = list(order = c(2, 1, 0), period = 12))
  temp<-forecast(arima_model,1)
  arima_forecast<-c(arima_forecast,temp$mean)  
}
test<-df[301:397]

plot(type='l',test,xlab="Time",ylab="Electricity")
lines(arima_forecast,col="green")
legend("topright", legend = c("Actual", "Forecast"), col = c("black", "green"), lty = 1)

mape <- function(actual, predicted) {
  n <- length(actual)
  mape_value <- sum(abs((actual - predicted) / actual)) * (100 / n)
  return(mape_value)
}
test<-exp(test)
arima_forecast<-exp(arima_forecast)

mape_value <- mape(test, arima_forecast)
print(mape_value)

OOS_residuals<-test-arima_forecast
acf(OOS_residuals)
pacf(OOS_residuals)
kpss.test(OOS_residuals)
plot(OOS_residuals,type="l")

summary(arima_model)
