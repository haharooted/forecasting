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
train_data <- window(ts_data, end=c(2020, 12))
test_data <- window(ts_data, start=c(2021, 1))



ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("cnt employed") +
  ggtitle("Season plot of dk employment")

ggseasonplot(ts_data, polar=TRUE) +
  ylab("cnt employed") +
  ggtitle("Season plot of dk employment")


## 1. IDENTIFY THE ORDER OF DIFFERENCING (d) NEEDED ### 


### IDENTIFY THE ORDER OF DIFFERENCING (d) ###
# Conduct Augmented Dickey-Fuller (ADF) test
adf.test(train_data, alternative = "stationary")



# Apply first difference and re-run ADF test
diff_data <- diff(train_data, differences = 1)
adf.test(diff_data, alternative = "stationary")

# Plot original series
autoplot(train_data) +
  ggtitle("Original Series")

# Plot differenced series
autoplot(diff_data) +
  ggtitle("First-Differenced Series")



### IDENTIFY THE ORDER OF THE AR (p) AND MA (q) PARTS ###
# Generate ACF and PACF plots of the differenced series
par(mfrow=c(2,1))
acf(diff_data, main="ACF of Differenced Series")
pacf(diff_data, main="PACF of Differenced Series")



# The plots shows:
#ACF shows the largest spike over blue line is at 0.0 and other big spike over blue line is 0.5, and another large spike over blue line is at 1.0 and a smaller spike over blue line at 1.5
#The PCAF shows one large spike at 0.5


# This indicates a potential seasonal pattern in the ACF, as the significant lags are at intervals of 6, which can be interpreted as semi-annual seasonality. However, since we want to model deterministic seasonality, we should account for this pattern using seasonal dummy variables rather than a SARIMA model.
# Regarding the order of the AR (p) and MA (q) parts, the ACF cuts off after the first lag, which suggests that we should include one MA term. The PACF also has a significant spike at the first lag, suggesting that we should include one AR term.


# So, from the ACF and PACF, we can estimate an ARIMA(1,1,1) model, where p=1, d=1, and q=1.


# Split the data into training and test sets
train_data <- window(ts_data, end=c(2020, 12))
test_data <- window(ts_data, start=c(2021, 1))

# Create 11 dummy variables (one less than the number of seasons)
train_month_dummies <- seasonaldummy(train_data)
test_month_dummies <- seasonaldummy(test_data)

# Combine the dummy variables with the training and test datasets
train_data_ext <- cbind(train_data, train_month_dummies)
test_data_ext <- cbind(test_data, test_month_dummies)

# Fit an ARIMA(1,1,1) model with dummy variables
fit <- Arima(train_data_ext[,1], order=c(1,1,1), xreg=train_data_ext[,-1])
summary(fit)

# Use the fitted model to make forecasts
forecasts <- forecast(fit, h=length(test_data_ext[,1]), xreg=test_data_ext[,-1])

# Plot the forecasts
autoplot(forecasts) + autolayer(test_data_ext[,1], series="Actual")

# Calculate the RMSE
rmse <- sqrt(mean((forecasts$mean - test_data_ext[,1])^2))
cat("RMSE: ", rmse, "\n")

# Check the residuals
checkresiduals(fit)










