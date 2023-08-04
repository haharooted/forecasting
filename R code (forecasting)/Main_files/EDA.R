rm()

library(forecast)
library(dplyr)
library(tidyverse)
library(zoo)
library(lmtest)
library(ggplot2)
library(tseries)

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
data <- read.table("/Users/jkatz/desktop/Forecasting/employed_in_real_estate.csv", sep = ";", header = TRUE)

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
