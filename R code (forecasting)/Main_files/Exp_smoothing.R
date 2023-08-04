rm()
library(forecast)
library(dplyr)
library(tidyverse)
library(zoo)
library(lmtest)
library(ggplot2)



### LOAD DATA AND SPLIT TEST/TRAINING SET ### 
# Load the data
#data <- read.table("/Users/jkatz/desktop/Forecasting/total_employed_in_information_industry_denmark.csv", sep = ";", header = TRUE)
#data <- read.table("/Users/jkatz/desktop/Forecasting/total_employed_bygge_industry_denmark.csv", sep = ";", header = TRUE)
#data <- read.table("/Users/jkatz/desktop/Forecasting/total_employed_forsvar_politi_denmark.csv", sep = ";", header = TRUE)
data <- read.table("/Users/jkatz/desktop/Forecasting/employed_in_real_estate.csv", sep = ";", header = TRUE)


# Convert the "Month" column to a time series
data$Month <- as.yearmon(data$Month, "%YM%m")
ts_data <- ts(data$Employed, start = c(2008, 1), frequency = 12)
#ts_data <- window(ts_data, start=c(2008, 1))


# Split the data into training and test sets
train_data <- window(ts_data, end=c(2020, 12))
test_data <- window(ts_data, start=c(2021, 1))


### APPLY HOLT-WINTERS EXPONENTIAL SMOOTHING ### 
# Fit the Holt-Winters Exponential Smoothing model to the training data
holt_winters_model_multiplicative <- HoltWinters(train_data, seasonal = "multiplicative")
holt_winters_model_additive <- HoltWinters(train_data, seasonal = "additive")

# Make predictions for the test set
forecast_values_additive <- forecast(holt_winters_model_additive, h = length(test_data))
forecast_values_multiplicative <- forecast(holt_winters_model_multiplicative, h = length(test_data))

# Extract the point forecasts for the test set
predicted_values_additive <- forecast_values_additive$mean
predicted_values_multiplicative <- forecast_values_multiplicative$mean

# Print the predicted values (optional)
print(predicted_values_additive)
print(predicted_values_multiplicative)

### VISUALIZE FORECASTS IN A GRAPH ###

# Combine the forecasts with the test data
forecast_df <- data.frame(
  Date = time(test_data),
  Actual = as.numeric(test_data),
  Additive_Forecast = as.numeric(predicted_values_additive),
  Multiplicative_Forecast = as.numeric(predicted_values_multiplicative)
)

# Create the plot
ggplot(forecast_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Additive_Forecast, color = "Additive Forecast"), size = 1, linetype = "dashed") +
  geom_line(aes(y = Multiplicative_Forecast, color = "Multiplicative Forecast"), size = 1, linetype = "dashed") +
  labs(title = "Holt-Winters Exponential Smoothing Forecast",
       x = "Date",
       y = "Employed") +
  scale_color_manual(name = "Forecast Type",
                     labels = c("Actual", "Additive", "Multiplicative"),
                     values = c("Actual" = "black", "Additive Forecast" = "blue", "Multiplicative Forecast" = "red")) +
  theme_minimal()




# Combine the forecasts with the entire time series data
full_ts_data <- data.frame(
  Date = time(ts_data),
  Employed = as.numeric(ts_data),
  Additive_Forecast = c(rep(NA, length(ts_data) - length(test_data)), as.numeric(predicted_values_additive)),
  Multiplicative_Forecast = c(rep(NA, length(ts_data) - length(test_data)), as.numeric(predicted_values_multiplicative))
)

# Create the plot
ggplot(full_ts_data, aes(x = Date)) +
  geom_line(aes(y = Employed, color = "Actual"), size = 1) +
  geom_line(aes(y = Additive_Forecast, color = "Additive Forecast"), size = 1, linetype = "dashed") +
  geom_line(aes(y = Multiplicative_Forecast, color = "Multiplicative Forecast"), size = 1, linetype = "dashed") +
  labs(title = "Holt-Winters Exponential Smoothing Forecast",
       x = "Date",
       y = "Employed") +
  scale_color_manual(name = "Forecast Type",
                     labels = c("Actual", "Additive", "Multiplicative"),
                     values = c("Actual" = "black", "Additive Forecast" = "blue", "Multiplicative Forecast" = "red")) +
  theme_minimal()











####### ETS??? #######
### APPLY ETS (EXPONENTIAL SMOOTHING STATE SPACE MODEL) ### 
# Fit the ETS model to the training data
ets_model <- ets(train_data)

# Make predictions (forecasts with prediction intervals) for the test set
forecast_values <- forecast(ets_model, h = length(test_data))

# Extract the point forecasts and prediction intervals for the test set
predicted_values <- forecast_values$mean
lower_interval <- as.numeric(forecast_values$lower[length(forecast_values$mean)])
upper_interval <- as.numeric(forecast_values$upper[length(forecast_values$mean)])

# Print the predicted values (optional)
print(predicted_values)

### VISUALIZE FORECASTS WITH PREDICTION INTERVALS ###

# Combine the forecasts with the test data
forecast_df <- data.frame(
  Date = time(test_data),
  Actual = as.numeric(test_data),
  Forecast = as.numeric(predicted_values),
  Lower_Interval = lower_interval,
  Upper_Interval = upper_interval
)

# Create the plot
ggplot(forecast_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Forecast, color = "Forecast"), size = 1) +
  geom_ribbon(aes(ymin = Lower_Interval, ymax = Upper_Interval, fill = "Prediction Interval"), alpha = 0.2) +
  labs(title = "Exponential Smoothing Forecast with Prediction Intervals (ETS Model)",
       x = "Date",
       y = "Employed") +
  scale_color_manual(name = "Forecast Type",
                     labels = c("Actual", "Forecast"),
                     values = c("Actual" = "black", "Forecast" = "blue")) +
  scale_fill_manual(name = "Prediction Interval",
                    values = "lightblue") +
  theme_minimal()