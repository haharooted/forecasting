rm()

library(forecast)
library(dplyr)
library(tidyverse)
library(zoo)
library(lmtest)




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
#ts_data <- log(ts_data)

# Split the data into training and test sets
train_data <- window(ts_data, end=c(2020, 12))
test_data <- window(ts_data, start=c(2021, 1))


####### SHOW SEASONALITY? ####

ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("cnt employed") +
  ggtitle("Season plot of employment")

ggseasonplot(ts_data, polar=TRUE) +
  ylab("cnt employed") +
  ggtitle("Season plot of employment")

#### DECOMPOSE? #####

###################   LINEAR REGRESSION     #############################

############# DETERMINISTIC TREND #############

# Fit a linear trend model
fit_linear <- lm(train_data ~ time(train_data))

# Fit a quadratic trend model
fit_quadratic <- lm(train_data ~ I(time(train_data)^2) + time(train_data))


# Fit a cubic trend model

# Fit a logarithmic trend model
#train_data_log <- log(train_data)
#fit_log <- lm(train_data_log ~ time(train_data_log))

# Extract AIC and BIC values
aic_values <- c(Linear = AIC(fit_linear), Quadratic = AIC(fit_quadratic))
bic_values <- c(Linear = BIC(fit_linear), Quadratic = BIC(fit_quadratic))

# Print values
print(aic_values)
print(bic_values)

r2_values <- c(
  Linear = summary(fit_linear)$r.squared,
  Quadratic = summary(fit_quadratic)$r.squared
)

# Print R-squared values
print(r2_values)


# Create a new time series for each set of fitted values
fitted_linear_ts <- ts(fitted(fit_linear), start = start(train_data), frequency = frequency(train_data))
fitted_quadratic_ts <- ts(fitted(fit_quadratic), start = start(train_data), frequency = frequency(train_data))

# Plot the original data
plot(train_data, main = "Fit deterministic trends to DK Employment", ylab = "Employed (log)", xlab = "Time")

# Add fitted trends to the plot
lines(fitted_linear_ts, col = "blue", lwd = 2)
lines(fitted_quadratic_ts, col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("Linear", "Quadratic"), 
       col = c("blue", "red"), lty = 1, lwd = 2, bty = "n")






























######## ARIMA #########

# Fit the model
model <- auto.arima(train_data)

# Check the model
summary(model)

# Make predictions
forecasts <- forecast(model, h = length(test_data))  # h is the forecast horizon

# Plot the forecasts
plot(forecasts)

# Compare the forecasts with the test data
accuracy(forecasts, test_data)


# Plot the forecasts along with the actual data
autoplot(forecasts) +
  autolayer(test_data, series = "Actual", PI = FALSE) +
  xlab("Time") + ylab("Employed") +
  ggtitle("ARIMA Forecast of Total Employed in Information Industry in Denmark") +
  guides(colour = guide_legend(title = "Series"))




########## ARIMA + SEASONALITY ###########



# Creating a dataframe with seasonal dummies
seasonal_dummies <- model.matrix(~as.factor(cycle(ts_data)))[,-1]

# Make sure the dummy variables have the same length as the train data
seasonal_dummies_train <- window(ts(seasonal_dummies, start = start(train_data), frequency = frequency(train_data)), end = end(train_data))

# Fit the model
model <- auto.arima(train_data, xreg = seasonal_dummies_train)

# Make predictions
# We need to provide the future values of the dummies for forecasting
seasonal_dummies_test <- window(ts(seasonal_dummies, start = start(test_data), frequency = frequency(test_data)), start = start(test_data))

# Number of periods to forecast
h_periods <- length(test_data)

# Make forecasts
forecasts <- forecast(model, xreg = seasonal_dummies_test[1:h_periods, ], h = h_periods)

# Plot the forecasts along with the actual data
autoplot(forecasts) +
  autolayer(test_data, series = "Actual", PI = FALSE) +
  xlab("Time") + ylab("Employed") +
  ggtitle("ARIMA Forecast of Total Employed in Information Industry in Denmark with deterministic seasonality") +
  guides(colour = guide_legend(title = "Series"))







########## SARIMA ##########


# Fit the model
model_sarima <- auto.arima(train_data, D = 1, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Check the model
summary(model_sarima)

# Make predictions
forecasts_sarima <- forecast(model_sarima, h = length(test_data))

# Plot the forecasts along with the actual data
autoplot(forecasts_sarima) +
  autolayer(test_data, series = "Actual", PI = FALSE) +
  xlab("Time") + ylab("Employed") +
  ggtitle("SARIMA Forecast of Total Employed in industry in Denmark") +
  guides(colour = guide_legend(title = "Series"))

# Compare the forecasts with the test data
accuracy(forecasts_sarima, test_data)


