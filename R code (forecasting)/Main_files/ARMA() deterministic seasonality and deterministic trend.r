########### Deterministic trend, deterministic seasonality, cyclic component ############
rm(list=ls())
require(lmtest); require(forecast); require(texreg);
#Load the data
data <- read.table("/Users/jkatz/desktop/Forecasting/employed_in_real_estate.csv", sep = ";", header = TRUE)
data$Month <- as.yearmon(data$Month, "%YM%m")
ts_data <- ts(data$Employed, start = c(2008, 1), frequency = 12)
ts_data_asvector <- as.vector(ts_data)
# Split the data into training and test sets
train_data <- window(ts_data, end=c(2020, 12))
test_data <- window(ts_data, start=c(2021, 01))

#Data plot shows that the liquor sales follow a strong trend 
#and that they have a seasonal pattern with one major and several minor 
#peaks during the year. The plot suggests that the liquor sales are 
#described by a trend and increasing seasonal variation. The logarithmic 
#transformation is used to equalize the seasonal increasing variation:
#Let's also check how the transformed data look 


s <- 12 #set up the data frequency
startyear <- 2008
st <- c(startyear,1) #start period
#how many years your data will cover
T <- length(train_data) #set up the length of your data
#years <- T/s #NOTE: works only if each year has a full set of seasonal observations
#------------------------------------------------
train_data <- ts(train_data,start=st,frequency=s)
plot(train_data)

y <- log(train_data)

#On the top of seasonal effects, liquor sales dynamics features prominent trend 
#and cyclical effects. Liquor sales trend upward and the trend appears nonlinear.
#To handle the nonlinear trend, we adopt a quadratic trend model.
t <- (1:T)
t2 <- t^2
#Run a model of quadratic trend:
modelT<-lm(y~t+t2) 
#To find out the details, use the summary function:
summary(modelT)
screenreg(modelT); #stargazer(modelT)
#check the model fit on the graph
trend<-y-modelT$residuals
plot(y,main="Employment plot comparing residuals on trend",ylab="Add title to y axis here")
lines(trend,col="green")

residuals<-y-trend
par(mar=c(5,5,5,5))
plot(y,ylim=c(0.75*min(y),max(y)),main="Missed seasonality in residuals...")
lines(trend,col="blue")
#tell R to draw over the current plot with a new one
par(new=T)
plot(residuals,ylim=c(min(residuals),3*max(residuals)),ylab='',axes=F)
axis(4, pretty(c(min(residuals),max(residuals))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)

#The ACF of the residuals confirms the neglected seasonality:
acf(residuals)

#Let's add seasonal dummies to our model of trend. 
#To exclude last season
M <- seasonaldummy(y)
modelTS<- lm(y ~ t+t2 + M)
summary(modelTS)
screenreg(list(modelT, modelTS)); #stargazer(modelT, modelTS, single.row=TRUE)

#The quadratic trend and seasonal dummies appears to be highly significant.
#Now, that we accounted for seasonal components, Durbin-Watson statistic has greater 
#ability to detect serial correlation.
dwtest(modelTS)

#Let's construct the plot that shows how the model fits the data and how the residuals
#look
TS <- y-modelTS$residuals
residuals <- y-TS

par(mar=c(5,5,5,5))
plot(y,ylim=c(0.75*min(y),max(y)),main="Residuals")
lines(TS,col="blue")
#tell R to draw over the current plot with a new one
par(new=T)
plot(residuals,ylim=c(min(residuals),3*max(residuals)),ylab='',axes=F)
axis(4, pretty(c(min(residuals),max(residuals))))
abline(h=0,col='grey')
mtext("R",side=4,line=2.5,at=0)
#The graph shows that the residuals are highly persistent, therefore, predictable.

#Further, ther autocorrelation function shows that the sample autocorrelations decay 
#slowly, and the residual partial autocorrelation function cuts off at 4 lags.
acf(residuals)
pacf(residuals)

# If we fit AR( ) model to the residuals, the ACF suggest the order 4 that AR(4) 
#would provide a good approximation to the disturbance term. Note that Dielbold's
#application in your textbook "Elements of Forecasting" uses AR(3). For many
#reasons it makes sense to be conservative and go with the lower number of lags.

#Let's check what combination of ARMA( ) is fits the best 
model.arma <- auto.arima(residuals, d=0, seasonal = FALSE, ic = "aic", 
                         stepwise = FALSE, approximation = FALSE, 
                         trace = TRUE)
#the best ARMA model is ARMA(3,2).

#Let's replicate Diebold's results with AR(3):
TrSeasQ <- model.matrix(~ t+t2+M)
TrSeasL <- model.matrix(~ t+M)
#model.ar3 <- arima(y,order = c(3, 0, 0),include.mean = FALSE,xreg=TrSeasQ);stargazer(modelT, modelTS, model.ar3, single.row=TRUE)
model.ar3 <- Arima(y,order = c(2, 0, 0),include.mean = FALSE,xreg=TrSeasQ)
model.ar3; screenreg(list(modelT, modelTS,model.ar3)); 
#FYI: notice that the coefficient on t2 is too small, it is rounded to zero. In the 
#report it will not be sufficient, because it is hard to see if the coefficient is
#significant or not. If you would like to see more precise values, type:
model.ar3$coef
sqrt(diag(model.ar3$var.coef))
#it will give you coefficients and their standard errors in a more precise way.
#the coefficient on t2 is -1.185125e-05, with s.e. 5.460489e-07, which gives 
#t-statistic = 1.185125e-05/5.460489e-07 = 21.7, which is much greater than 2,
#therefore the coefficient by t2 is significant and precisely estimated.

#The autocorrelation function shows that there are no patterns in the 
#residuals:
acf(model.ar3$residuals)
pacf(model.ar3$residuals)

#Let's construct the plot that shows how the model fits the data and how the residuals
#look
TSC <- y-model.ar3$residuals
residuals <- y-TSC

par(mar=c(5,5,5,5))
plot(y,ylim=c(0.75*min(y),max(y)),main="Add title here")
lines(TSC,col="blue")
#tell R to draw over the current plot with a new one
par(new=T)
plot(residuals,ylim=c(min(residuals),3*max(residuals)),ylab='',axes=F)
axis(4, pretty(c(min(residuals),max(residuals))))
abline(h=0,col='grey')
mtext("Residuals",side=4,line=2.5,at=0)



#We construct a histogram and hope that it looks like a bell-shaped normal 
#distribution:
hist(residuals, breaks = 30)
#The residuals appear to be fairly well approximated by a normal distribution.

# Compute the forecast 
h <- length(train_data)
t <- ((T+1):(T+h))
t2 <- t^2
M <- I(seasonaldummy(y,h))
ForecastTrSeas <- model.matrix(~ t+t2+M)
FIT <- y-model.ar3$residuals
myforecast <- forecast(model.ar3, h=h, xreg=ForecastTrSeas)
#UN-log the forecast back to the levels via exp( )
myforecast$mean<-exp(myforecast$mean)
myforecast$upper<-exp(myforecast$upper)
myforecast$lower<-exp(myforecast$lower)
myforecast$x<-exp(myforecast$x)
myforecast

FIT <- exp(FIT) #Un-log the regression fit
plot(myforecast,ylab="",main="AR(3) with quadratic trend and seasonality")
lines(FIT,col="green")

#-----------------------------------------------------------
#Further diagnostics:

#Let's check what combination of ARMA( ) is fits the best 
model.arma <- auto.arima(y, d=0, seasonal = FALSE, ic = "aic",xreg=TrSeasQ, 
                         stepwise = FALSE, approximation = FALSE, 
                         trace = TRUE)
#the best ARMA model is ARMA(3,2) now with quadratic trend and seasonality, but
#the coefficient on t2 does not have a standard error (numerical problem)
#Second best is AR(4), but with the same numerical problem on t2
#Third best is ARMA(3,1), but with the same numerical problem on t2
#Fourth best is AR(3), and fifth best is ARMA(1,4)
model.arma14 <- Arima(y,order = c(1, 0, 4),include.mean = FALSE,xreg=TrSeasQ)
model.arma14
model.arma14$coef
sqrt(diag(model.arma14$var.coef))

#Let's check if we need this quadratic component in trend, I remove it now:
model.arma <- auto.arima(y, d=0, seasonal = FALSE, ic = "aic",xreg=TrSeasL, 
                         stepwise = FALSE, approximation = FALSE, 
                         trace = TRUE)
model.ar2 <- model.arma

#In-sample forecast
#I take 24 data points out of sample, which I will forecast later.
#h <- s*2
#y <- subset(data, end=length(data)-h); z <- subset(data, start=length(data)-h+1)
y <- train_data
z <- test_data
T <- length(y) #set up the length of your data
t <- (1:T)
t2 <- t^2
M <- seasonaldummy(y)
TrSeasQ <- model.matrix(~ t+t2+M)
Tr.model.ar3 <- Arima(y,order = c(3, 0, 0),include.mean = FALSE,xreg=TrSeasQ)
Tr.model.ar3

Tr.model.arma14 <- Arima(y,order = c(1, 0, 4),include.mean = FALSE,xreg=TrSeasQ)
Tr.model.arma14

TrSeasL <- model.matrix(~ t+M)
Tr.model.ar2L <- Arima(y,order = c(2,0,0),include.mean = FALSE,xreg=TrSeasL)
Tr.model.ar2L

TrSeasLL <- model.matrix(~ t+M)
Tr.model.ar2LL <- Arima(y,order = c(2,0,0),include.mean = FALSE,xreg=TrSeasQ)
Tr.model.ar2LL

# Compute the insample forecast accuracy measures 
t <- ((T+1):(T+h))
t2 <- t^2
M <- seasonaldummy(train_data)

TrSeasQ <- model.matrix(~ t+t2+M)
forecast.ar3<-forecast(Tr.model.ar3, h=h, xreg=TrSeasQ)
forecast.arma14<-forecast(Tr.model.arma14, h=h, xreg=TrSeasQ)
plot(forecast.ar3,xlim=c(2008,2024),ylab="",main="AR(3) model")
lines(data)
plot(forecast.arma14,xlim=c(2008,2024),ylab="",main="ARMA(1,4) with quadtaric trend")
lines(data)

TrSeasL <- model.matrix(~ t+M)
forecast.ar2L<-forecast(Tr.model.ar2L, h=h, xreg=TrSeasL)
plot(forecast.ar2L,xlim=c(2008,2024),ylab="",main="ARMA(2,0,0) with linear trend")
lines(data)

TrSeasL <- model.matrix(~ t+M)
forecast.ar2LL<-forecast(Tr.model.ar2LL, h=h, xreg=TrSeasQ)
plot(forecast.ar2LL,xlim=c(2008,2024),ylab="",main="ARMA(2,0,0) with quadtradic trend")
lines(data)


accuracy(forecast.ar3,z)
accuracy(forecast.arma14,z)
accuracy(forecast.ar2L,z)
accuracy(forecast.ar2LL,z)

#ME, RMSE, MAEs point to the ARMA(1,4) with quadratic trend as the best of these three methods for this data set.

