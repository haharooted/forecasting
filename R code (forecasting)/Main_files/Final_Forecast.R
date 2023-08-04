######## >>>>> Final Model Comparison & Forecast <<<<< ########

rm(list=ls())
library(forecast)
library(dplyr)
library(tidyverse)
library(zoo)
library(lmtest)
library(ggplot2)
data <- read.table("/Users/jkatz/desktop/Forecasting/employed_in_real_estate.csv", sep = ";", header = TRUE)
data$Month <- as.yearmon(data$Month, "%YM%m")
ts_data <- ts(data$Employed, start = c(2008, 1), frequency = 12)
logged_ts_data <- log(ts_data)
logged_diffed_ts_data <- diff(logged_ts_data, lag=12)
train_data <- window(ts_data, end=c(2020, 12))
test_data <- window(ts_data, start=c(2021, 1))
forecast_future <- 24



# =============== 1) ARMA w/ seasonality (dummies) + trend (quadratic) =============================
y_train <- log(train_data)
y_train_diffed <- diff(y_train, lag=12)

h_train <- length(train_data)

y_test <- log(test_data)
y_test_diffed <- diff(y_train, lag=12)
h_test <- length(test_data)



# 1.1) Define model:
T <- length(train_data)
t <- (1:T)
t2 <- t^2
M <- seasonaldummy(y_train)
ModelQuadraticWDummies <- model.matrix(~ t+t2+M)
model.ar2 <- Arima(y_train,order = c(2, 0, 0),include.mean = FALSE,xreg=ModelQuadraticWDummies)
model.ar2_test <- model.ar2
# 1.2) Compute the forecast for the test_data period
T <- length(train_data)
h <- length(test_data) #period of test_data set we want to forecast based on train data model
t <- ((T+1):(T+h))
t2 <- t^2
M <- I(seasonaldummy(y_train,h))
ForecastModelWQAndDummies <- model.matrix(~ t+t2+M)
FIT <- y_train-model.ar2$residuals
myforecast <- forecast(model.ar2, h=h, xreg=ForecastModelWQAndDummies)
myforecast$mean<-exp(myforecast$mean) #UN-log the forecast back to the levels via exp( )
myforecast$upper<-exp(myforecast$upper)
myforecast$lower<-exp(myforecast$lower)
myforecast$x<-exp(myforecast$x)
myforecast

FIT <- exp(FIT) #Un-log the regression fit
plot(myforecast,ylab="Employed in Real Estate DK",main="AR(2) with quadratic trend and seasonal dummies")
myforecast_test <- myforecast #so we can use later
lines(FIT,col="green")
lines(test_data, col="orange")

# 1.3) Calculate accuracy of the forecast trained on train data based on real test data:
accuracy(myforecast, test_data)

# 1.4) Refit model and perform forecast for next 2 years
ts_data_logged <- log(ts_data) #log ts data!
T <- length(ts_data_logged)
t <- (1:T)
t2 <- t^2
M <- seasonaldummy(ts_data_logged)
ModelQuadraticWDummies <- model.matrix(~ t+t2+M)
model.ar2 <- Arima(ts_data_logged,order = c(2, 0, 0),include.mean = FALSE,xreg=ModelQuadraticWDummies)


h <- forecast_future #forecast 2 years into future
t <- ((T+1):(T+h))
t2 <- t^2
M <- I(seasonaldummy(ts_data_logged,h))
ForecastModelWQAndDummies <- model.matrix(~ t+t2+M)
FIT <- ts_data_logged-model.ar2$residuals
myforecast <- forecast(model.ar2, h=h, xreg=ForecastModelWQAndDummies)
myforecast$mean<-exp(myforecast$mean) #UN-log the forecast back to the levels via exp( )
myforecast$upper<-exp(myforecast$upper)
myforecast$lower<-exp(myforecast$lower)
myforecast$x<-exp(myforecast$x)
myforecast

FIT <- exp(FIT) #Un-log the regression fit
plot(myforecast,ylab="Employed in Real Estate DK",main="AR(2) with quadratic trend and seasonal dummies")
lines(FIT,col="green")










# ====================== ARIMA w/ stochastic trend (d=1) & seasonality dummies ======================
# Test for stationarity: 
test_stationarity_sarima <- ur.df(y_train,type=c("drift"),lags=10,selectlags="AIC")
summary(test_stationarity_sarima)
# Data is not stationary

# 1.1) Forecast ARIMA based on train_data on test_data (y_train = log(train_data))
train_seasonal_dummies <- seasonaldummy(y_train)
test_seasonal_dummies <- seasonaldummy(y_train, h = length(test_data))
model.arima <- auto.arima(y_train, d=1, seasonal = FALSE, ic = "aic", 
                          stepwise = FALSE, approximation = FALSE, 
                          trace = TRUE, allowdrift=TRUE, xreg=train_seasonal_dummies)
#no trend or seasonality
model.arima_no_seasonality_or_trend <- auto.arima(y_train, d=0, seasonal = FALSE, ic = "aic", 
                          stepwise = FALSE, approximation = FALSE, 
                          trace = TRUE, allowdrift=TRUE)

model.arima_no_seasonality_dummies <- auto.arima(y_train, d=1, seasonal = FALSE, ic = "aic", 
                          stepwise = FALSE, approximation = FALSE, 
                          trace = TRUE, allowdrift=TRUE)
model.arima
#model.arima #ARIMA(2, 1, 2) with drift
model.arima_test <- model.arima
#model.arima_no_seasonality_dummies <- Arima(y_train, order=c(2,1,2), seasonal=c(0,0,0))

arima_residuals <- y_train - model.arima$residuals
forecast_result_no_seasonality_dummies_or_trend <- forecast(model.arima_no_seasonality_or_trend, h = length(test_data))
checkresiduals(forecast_result_no_seasonality_dummies_or_trend)
forecast_result_no_seasonality_dummies <- forecast(model.arima_no_seasonality_dummies, h = length(test_data))
checkresiduals(forecast_result_no_seasonality_dummies)

forecast_result <- forecast(model.arima, xreg = test_seasonal_dummies, h = length(test_data))
checkresiduals(forecast_result)
forecast_result$mean<-exp(forecast_result$mean) #UN-log the forecast back to the levels via exp( )
forecast_result$upper<-exp(forecast_result$upper)
forecast_result$lower<-exp(forecast_result$lower)
forecast_result$x<-exp(forecast_result$x)

forecast_result_test <- forecast_result
plot(forecast_result, ylab="Employed in Real Estate DK",main="ARIMA")
lines(test_data, col="orange")
FIT <- exp(arima_residuals) #Un-log the regression fit
lines(FIT, col="green")


legend("topleft", 
       legend=c("ARIMA forecast", "Test Data", "ARIMA fitted"),
       col=c("blue", "orange", "green"), 
       lty=1,
       cex=0.8)


# 1.2) Plot accuracy of the arima model on test data
accuracy(forecast_result, test_data)




# 1.3) Refit model on full dataset
real_seasonal_dummies <- seasonaldummy(logged_ts_data)
future_seasonal_dummies <- seasonaldummy(logged_ts_data, h=forecast_future)

model.arima <- Arima(logged_ts_data, order=c(2,1,2), seasonal=c(0,0,0), include.drift=TRUE, xreg=real_seasonal_dummies)

model.arima #ARIMA(2, 1, 2) with drift
arima_residuals <- logged_ts_data - model.arima$residuals

forecast_result <- forecast(model.arima, xreg = future_seasonal_dummies, h = forecast_future)
forecast_result$mean<-exp(forecast_result$mean) #UN-log the forecast back to the levels via exp( )
forecast_result$upper<-exp(forecast_result$upper)
forecast_result$lower<-exp(forecast_result$lower)
forecast_result$x<-exp(forecast_result$x)

plot(forecast_result, ylab="Employed in Real Estate DK",main="ARIMA(2,1,2) with drift & deterministic seasonality", ylim=c(30000, 48000), xlim=c(2015, 2026))
FIT <- exp(arima_residuals) #Un-log the regression fit
lines(FIT, col="green")
forecast_result
legend("topleft", 
       legend=c("ARIMA forecast", "ARIMA fitted"),
       col=c("blue", "green"), 
       lty=1,
       cex=0.8)






# =============== SARIMA w/ stochastic trend (d=1) & stochastic seasonality (D=1) =====================
#1.1) check stationarity with df
test_stationarity_sarima <- ur.df(y_train,type=c("drift"),lags=10,selectlags="AIC")
acf(y_train, lag.max = 3*12)
pacf(y_train, lag.max = 3*12)
summary(test_stationarity_sarima)
test_stationarity_sarima_diffed1 <- ur.df(y_train_diffed,type=c("drift"),lags=10,selectlags="AIC")
summary(test_stationarity_sarima_diffed1)
acf(y_train_diffed, lag.max = 3*12)
pacf(y_train_diffed, lag.max = 3*12)

#1.2) Build ARIMA() model    d=1 removes stochastic trend,    D=1 removes stochastic seasonality
#model.sarima <- auto.arima(y_train, d=1, D=1, seasonal = TRUE, ic = "aic", 
#                          stepwise = FALSE, approximation = FALSE, 
#                          trace = TRUE)
#model.sarima # ARIMA(0,1,0)(0,1,1)[12]  
model.sarima <- Arima(y_train, order=c(0,1,0), seasonal=c(0,1,1))
model.sarima_test <- model.sarima
sarima_residuals <- y_train - model.sarima$residuals

forecast_result_sarima <- forecast(model.sarima, h = length(test_data))
forecast_result_sarima$mean<-exp(forecast_result_sarima$mean) #UN-log the forecast back to the levels via exp( )
forecast_result_sarima$upper<-exp(forecast_result_sarima$upper)
forecast_result_sarima$lower<-exp(forecast_result_sarima$lower)
forecast_result_sarima$x<-exp(forecast_result_sarima$x)
forecast_result_sarima_test <- forecast_result_sarima
plot(forecast_result_sarima_test, ylab="Employed in Real Estate DK",main="ARIMA(0,1,0)(0,1,1)[12] test_data")
lines(test_data, col="orange")
FIT <- exp(sarima_residuals) #Un-log the regression fit
lines(FIT, col="green")
legend("topleft", 
       legend=c("SARIMA forecast", "Test Data", "SARIMA fitted"),
       col=c("blue", "orange", "green"), 
       lty=1,
       cex=0.8)

accuracy(forecast_result_sarima, test_data)

#1.3) Refit ARIMA() on full data: 
model.sarima <- Arima(logged_ts_data, order=c(0,1,0), seasonal=c(0,1,1))
sarima_residuals <- logged_ts_data - model.sarima$residuals
forecast_result_sarima <- forecast(model.sarima, h = forecast_future)
forecast_result_sarima$mean<-exp(forecast_result_sarima$mean) #UN-log the forecast back to the levels via exp( )
forecast_result_sarima$upper<-exp(forecast_result_sarima$upper)
forecast_result_sarima$lower<-exp(forecast_result_sarima$lower)
forecast_result_sarima$x<-exp(forecast_result_sarima$x)
plot(forecast_result_sarima, ylab="Employed in Real Estate DK",main="SARIMA(0,1,0)(0,1,1)[12] forecast 2y")
FIT <- exp(sarima_residuals) #Un-log the regression fit
lines(FIT, col="green")
legend("topleft", 
       legend=c("SARIMA forecast", "SARIMA fitted"),
       col=c("blue", "green"), 
       lty=1,
       cex=0.8)

forecast_result_sarima






# ============= Holt-Winters Exponential Smoothing w/ alpha, beta, gamma =================

# 1.1) Plot different HW
# Holt-Winters w/ gamma and beta
hw1 <- HoltWinters(train_data)
hwforecast1 <- forecast(hw1, h=length(test_data), level=95)

# Holt-Winters without Trend
hw2 <- HoltWinters(train_data, beta=FALSE)
hwforecast2 <- forecast(hw2, h=length(test_data), level=95)

# Holt-Winters without seasonality
hw3 <- HoltWinters(train_data, gamma=FALSE)
hwforecast3 <- forecast(hw3, h=length(test_data), level=95)


plot(hwforecast1, ylab="Employed in Real Estate DK", main="Comparing Holt-Winters Models on test data", xlim=c(2018, 2024))
lines(hwforecast1$mean, col="green")
lines(hwforecast2$mean, col="blue")
lines(hwforecast3$mean, col="red")
lines(test_data, col="orange")

hwforecast1_test <- hwforecast1

legend("topleft", 
       legend=c("Basic Holt-Winters", "HW Without Trend", "HW Without seasonality", "Test Data"),
       col=c("green", "blue", "red", "orange"), 
       lty=1,
       cex=0.8)

# 1.2) Check accuracy of HW on test set: 
accuracy(hwforecast1, test_data)
accuracy(hwforecast2, test_data)
accuracy(hwforecast3, test_data)

# 1.3) Perform 2 year forecast with the best HW refitted on the full dataset
hw1 <- HoltWinters(ts_data)
hwforecast1 <- forecast(hw1, h=forecast_future, level=95)

# Holt-Winters without Trend
hw2 <- HoltWinters(ts_data, beta=FALSE)
hwforecast2 <- forecast(hw2, h=forecast_future, level=95)

# Holt-Winters without seasonality
hw3 <- HoltWinters(ts_data, gamma=FALSE)
hwforecast3 <- forecast(hw3, h=forecast_future, level=95)

plot(hwforecast1, ylab="Employed in Real Estate DK", main="HW on Future 2y based on full dataset. HW w/ gamma-beta selected", xlim=c(2018, 2026))
lines(hwforecast1$mean, col="green")
lines(hwforecast2$mean, col="blue")
lines(hwforecast3$mean, col="red")
FIT <- ts_data-hwforecast1$residuals
lines(FIT, col="orange")
legend("topleft", 
       legend=c("Holt-Winters w/ gamma/beta", "HW Without Trend", "HW Without seasonality", "Holt-Winters w/ gamma/beta FIT"),
       col=c("green", "blue", "red", "orange"), 
       lty=1,
       cex=0.8)








######### CONCLUSION #######


### ACCURACY ON TEST DATA: ###
accuracy(myforecast_test, test_data) #ARMA
accuracy(forecast_result_test, test_data) #ARIMA
accuracy(forecast_result_sarima_test, test_data) #SARIMA
accuracy(hwforecast1_test, test_data) #HW

########TEST DATA#########
end_year <- 2023 + (3-1) / 12

plot(myforecast_test, lwd=3, ylab="Employed in Real Estate DK", main="[TEST SET] Forecasting employees in the Real Estate industry in Denmark", xlim=c(2017, end_year))
lines(forecast_result_test$mean, col="green", lty=2, lwd=2) #ARIMA
lines(forecast_result_sarima_test$mean, col="purple", lty=2, lwd=2) #SARIMA
lines(hwforecast1_test$mean, col="red", lty=2, lwd=2) # HW dotted, fatter line
lines(test_data, col="orange", lty=2, lwd=3) # HW dotted, fatter line

legend("topleft", 
       legend=c("ARMA(2,0,0) w/ deterministic seasonality & trend", "ARIMA(2,1,2) w/ deterministic seasonality", "SARIMA(0,1,0)(0,1,1)[12]" ,"Holt-Winters w/ gamma/beta", "Test data"),
       col=c("blue", "green", "purple", "red", "orange"), 
       lty=1,
       cex=0.8)

##########REAL DATA############
# Define the start and end dates for your x-axis
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2026-05-01")

# Create a sequence of dates from start to end, by month
dates <- seq(start_date, end_date, by="month")
end_year <- 2025 + (3- 1) / 12
plot(myforecast, lwd=3, ylab="Employed in Real Estate DK", main="Forecasting employees in the Real Estate industry in Denmark", xlim=c(2020, end_year), ylim=c(39000, 49000))
lines(forecast_result$mean, col="green", lty=2, lwd=2) #ARIMA
lines(forecast_result_sarima$mean, col="purple", lty=2, lwd=2) #SARIMA
lines(hwforecast1$mean, col="red", lty=2, lwd=2) # HW dotted, fatter line
axis.Date(1, at=dates, format="%Y-%m", las=2, cex.axis=0.3)

legend("topleft", 
       legend=c("ARMA(2,0,0) w/ deterministic seasonality & trend", "ARIMA(2,1,2) w/ deterministic seasonality", "SARIMA(0,1,0)(0,1,1)[12]" ,"Holt-Winters w/ gamma/beta"),
       col=c("blue", "green", "purple", "red"), 
       lty=1,
       cex=1)


## TEST DATA
cat("ARMA Model: AIC =", AIC(model.ar2_test), "BIC =", BIC(model.ar2_test), "\n")
cat("ARIMA Model: AIC =", AIC(model.arima_test), "BIC =", BIC(model.arima_test), "\n")
cat("SARIMA Model: AIC =", AIC(model.sarima_test), "BIC =", BIC(model.sarima_test), "\n")


## REAL DATA
cat("ARMA Model: AIC =", AIC(model.ar2), "BIC =", BIC(model.ar2), "\n")
cat("ARIMA Model: AIC =", AIC(model.arima), "BIC =", BIC(model.arima), "\n")
cat("SARIMA Model: AIC =", AIC(model.sarima), "BIC =", BIC(model.sarima), "\n")
#cat("Holt-Winters Model: AIC =", AIC(hw1), "BIC =", BIC(hw1), "\n")

hw_model <- ets(logged_ts_data)
cat("ETS (Holt-Winters) Model: AIC =", hw_model$aic, "\n")
plot(hw_model)
