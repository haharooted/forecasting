### CVR DATA ###


#Industry Employment brancheid = 59, computerprogrammering hovedbranche
#aar	maaned	total_aarsvaerk	total_ansatte
#2015	1	19781	22087
#2015	2	19948	22225
#2015	3	20087	22421
#2015	4	20022	22406
#2015	5	20168	22466
#2015	6	20224	22511
#2015	7	19764	22153
#2015	8	20097	22542
#2015	9	20551	22904
#2015	10	20675	23036
#2015	11	20738	23116





# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)


# Read the data from the csv file
data <- read_csv("/Users/jkatz/desktop/Forecasting/CVR/IndustryEmployment_202307302214.csv")

# Convert the 'aar' and 'maaned' columns to a date
data$date <- ymd(paste(data$aar, data$maaned, "01", sep="-"))

# Convert to a time series
total_aarsvaerk_ts <- ts(data$total_aarsvaerk, start=c(min(data$aar),min(data$maaned)), frequency = 12)
total_ansatte_ts <- ts(data$total_ansatte, start=c(min(data$aar),min(data$maaned)), frequency = 12)

# Plot the series
plot(total_aarsvaerk_ts, type="l", col="blue", ylab="Values", xlab="Time", main="Time series of total_aarsvaerk and total_ansatte")
lines(total_ansatte_ts, col="red")
legend("topright", legend=c("total_aarsvaerk", "total_ansatte"), col=c("blue", "red"), lty=1)



# Summary statistics
summary(total_aarsvaerk_ts)
summary(total_ansatte_ts)

# Autocorrelation plots
acf(total_aarsvaerk_ts)
acf(total_ansatte_ts)

# Partial autocorrelation plots
pacf(total_aarsvaerk_ts)
pacf(total_ansatte_ts)

# Decompose the time series to show trend, seasonality, and residuals
decomposed_aarsvaerk <- decompose(total_aarsvaerk_ts)
plot(decomposed_aarsvaerk)

decomposed_ansatte <- decompose(total_ansatte_ts)
plot(decomposed_ansatte)

# Augmented Dickey-Fuller test for stationarity
library(tseries)
adf.test(total_aarsvaerk_ts)
adf.test(total_ansatte_ts)

# Boxplots for each month to check for seasonal patterns and outliers
#boxplot(total_aarsvaerk_ts ~ cycle(total_aarsvaerk_ts))
#boxplot(total_ansatte_ts ~ cycle(total_ansatte_ts))





########################## V2 #########################

# Load necessary library
library(forecast)

# Create a season plot
ggseasonplot(total_aarsvaerk_ts, year.labels=TRUE, year.labels.left=TRUE, 
             main="Season plot: total_aarsvaerk", col=rainbow(12))
ggseasonplot(total_ansatte_ts, year.labels=TRUE, year.labels.left=TRUE, 
             main="Season plot: total_ansatte", col=rainbow(12))

# Create a seasonal subseries plot
ggsubseriesplot(total_aarsvaerk_ts, main="Seasonal subseries plot: total_aarsvaerk", col=rainbow(12))
ggsubseriesplot(total_ansatte_ts, main="Seasonal subseries plot: total_ansatte", col=rainbow(12))

# Create box plots
boxplot(total_aarsvaerk_ts ~ cycle(total_aarsvaerk_ts), xlab="Month", ylab="total_aarsvaerk", main="Box plot: total_aarsvaerk")
boxplot(total_ansatte_ts ~ cycle(total_ansatte_ts), xlab="Month", ylab="total_ansatte", main="Box plot: total_ansatte")







