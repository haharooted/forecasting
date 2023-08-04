library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)

# URL of the data

data <- read.table("/Users/jkatz/desktop/Forecasting/total_employed_in_information_industry_denmark.csv", sep = ";", header = TRUE)

# Convert the "Month" column to a time series
data$Month <- as.yearmon(data$Month, "%YM%m")
ts_data <- ts(data$Employed, start = c(2008, 1), frequency = 12)


trend_data = ma(ts_data, order = 12, centre = T)
plot(as.ts(ts_data))
lines(trend_data)
plot(as.ts(trend_data))




detrend_data = ts_data / trend_data
plot(as.ts(detrend_data))



decomposed_data <- decompose(ts_data)
plot(decomposed_data)