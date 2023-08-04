# forecasting
Forecasting industry specific employee numbers using ARMA(), ARIMA(), SARIMA() &amp; exponential smoothing with (almost) real time data from the CVR Database


## structure

The project contains two parts: 

* R forecasting:
  -  This contains the code for fetching and cleaning data from an SQL query in R and transforming it into .csv
  -  The .csv is then used in projects to forecast employee count in the Real Estate industry over a period of 2 years

 The main part of the forecasting code is in R Code (forecasting)/Main_files/Final_Forecast.R



* Python fetcher (this is a forked version of cvrdata/cvrparser) for querying the CVR database with a researcher login and importing it into an SQL database, which is later queried by R to get the employee numbers.


### How to run:
  - Setup MySQL database locally (my local db and pw are removed from the code)
  - Run python3 -m cvr_parser and wait for data to be inserted

  - Extract data from SQL to R and make .csv
  - Use data to perform forecasts etc.
