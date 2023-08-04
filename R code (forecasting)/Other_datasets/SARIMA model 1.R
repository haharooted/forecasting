library(forecast)
library(dplyr)
library(tidyverse)
library(zoo)
library(lmtest)
library(ggplot2)
library(tseries)

rm()


### LOAD DATA AND SPLIT TEST/TRAINING SET ### 
# Load the data
#data <- read.table("/Users/jkatz/desktop/Forecasting/total_employed_in_information_industry_denmark.csv", sep = ";", header = TRUE)
#data <- read.table("/Users/jkatz/desktop/Forecasting/total_employed_bygge_industry_denmark.csv", sep = ";", header = TRUE)
data <- read.table("/Users/jkatz/desktop/Forecasting/total_employed_fosvar_politi_denmark.csv", sep = ";", header = TRUE)

# Convert the "Month" column to a time series
data$Month <- as.yearmon(data$Month, "%YM%m")
ts_data <- ts(data$Employed, start = c(2008, 1), frequency = 12)
#ts_data <- window(ts_data, start=c(2009, 1))

plot(ts_data)

# Split the data into training and test sets
train_data <- window(ts_data, end=c(2021, 12))
test_data <- window(ts_data, start=c(2022, 1))



ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("cnt employed") +
  ggtitle("Season plot of dk employment")

ggseasonplot(ts_data, polar=TRUE) +
  ylab("cnt employed") +
  ggtitle("Season plot of dk employment")


## 1. IDENTIFY THE ORDER OF DIFFERENCING (d) NEEDED ### 

# Plot original series
autoplot(train_data) +
  ggtitle("Original Series")

# Plot differenced series
autoplot(diff_data) +
  ggtitle("First-Differenced Series")




# Conduct ADF test on the original series to identify the order of regular differencing (d)
adf.test(train_data, alternative = "stationary")

# Suppose the ADF test suggests that first difference is needed to remove the stochastic trend
diff_data <- diff(train_data, differences = 1)

# Conduct ADF test on the differenced series to confirm that it is stationary
adf.test(diff_data, alternative = "stationary")

# For the seasonal differencing, we use nsdiffs() function in the forecast package in R. It applies the OCSB test and returns the recommended order of seasonal differencing (D).
D <- nsdiffs(train_data)

# Apply the seasonal differencing
seasonally_diff_data <- diff(diff_data, lag = frequency(train_data), differences = D)

# Conduct ADF test on the seasonally differenced series to confirm that it is stationary
adf.test(seasonally_diff_data, alternative = "stationary")




#### THE RESULTS INDICATE THAT FIRST DIFF IS STATIONARY P 0.05>



# Generate ACF and PACF plots of the seasonally differenced series
par(mfrow=c(2,1))
acf(seasonally_diff_data, main="ACF of Seasonally Differenced Series")
pacf(seasonally_diff_data, main="PACF of Seasonally Differenced Series")

#However, a large "fall" or negative spike at lag 1 in both the ACF and PACF plots suggests negative autocorrelation at lag 1. This could indicate the need for one AR or MA term.




# Fit a tentative SARIMA model
tentative_model <- Arima(train_data, order = c(0,1,1), seasonal = list(order = c(0,D,0)), method = "ML")
summary(tentative_model)



# From these measures, it seems like the tentative SARIMA(0,1,1)(0,1,0)[12] model fits the training data reasonably well.
checkresiduals(tentative_model)



# Both of these findings suggest that the model could be improved by adding a seasonal MA term at lag 12, i.e., we may need to revise our model to ARIMA(0,1,1)(0,1,1)[12]. The (0,1,1) at lag 12 represents the seasonal MA part of the model.



# Update the model with the seasonal MA term
updated_model <- Arima(train_data, order = c(0,1,1), seasonal = list(order = c(0,D,1)), method = "ML")
summary(updated_model)

# Check residuals of the updated model
checkresiduals(updated_model)


Box.test(updated_model$residuals, lag = 24, fitdf = 2, type = "Ljung-Box")


# p = 0.5419 i.e. we fail to reject the null hypothesis that the residuals are independently distributed



# Generate forecasts
forecasts <- forecast(updated_model, h = length(test_data))

# Plot the forecasts
autoplot(forecasts) +
  autolayer(test_data, series = "Test Data", PI = FALSE) +
  ggtitle("Forecasts from ARIMA(0,1,1)(0,1,1)[12] model") +
  xlab("Time") + ylab("Employment")

# Calculate accuracy measures
accuracy(forecasts, test_data)
