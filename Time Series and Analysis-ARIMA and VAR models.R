#Time-Series Analysis and Forecasting of Bitcoin Prices

#Load required and relevant packages 

library(pacman)
pacman::p_load(data.table, ggplot2, DBI, broom, sandwich, lmtest, estimatr, lubridate, dplyr, tseries, scales, forecast, TSA, vars, lattice)

#1. Daily Bitcoin Prices-1st Jan, 2014 to 8th Oct, 2021 [Source:https://www.investing.com/crypto/bitcoin/historical-data]
bitcoin_prices <- as.data.table(read.table("Bitcoin Data.csv", header=TRUE, sep=","))
bitcoin_prices$DATE <- bitcoin_prices$?..Date
bitcoin_prices$?..Date <- NULL
bitcoin_prices$DATE <-mdy(bitcoin_prices$DATE)

#2. Loading the Time Series Data sets 

#S&P 500
s_p500 <- as.data.table(read.table("S&P 500.csv", header=TRUE, sep=","))
s_p500$DATE <- as.Date(s_p500$DATE)

#London Bullion Market Price for Gold
gold <- as.data.table(read.table("GOLDAMGBD228NLBM.csv", header=TRUE, sep=","))
gold$DATE <- as.Date(gold$DATE)

#US/Euro Exchange Rate
us_euro_exch <- as.data.table(read.table("DEXUSEU.csv", header=TRUE, sep=","))
us_euro_exch$DATE <- as.Date(us_euro_exch$DATE)

#West Texas Intermediate Spot Price of Oil
oil <- as.data.table(read.table("DCOILWTICO.csv", header=TRUE, sep=","))
oil$DATE <- as.Date(oil$DATE)

#3. Merging the Data sets
merge_ts <- merge(bitcoin_prices, s_p500)
merge_ts <- merge(merge_ts, gold)
merge_ts <- merge(merge_ts, us_euro_exch)
merge_ts <- merge(merge_ts, oil)

#Renaming the Variables and Storing as Numeric Data
merge_ts <- merge_ts%>%rename(bitcoin_price=Price, gold_price=GOLDAMGBD228NLBM, us_eur_exch=DEXUSEU, oil_price=DCOILWTICO)

merge_ts$bitcoin_price <- merge_ts$bitcoin_price%>%gsub(",","",.)%>%as.numeric()
merge_ts$SP500 <- merge_ts$SP500%>%gsub(",","",.)%>%as.numeric()
merge_ts$gold_price <- merge_ts$gold_price%>%gsub(",","",.)%>%as.numeric()
merge_ts$us_eur_exch <- merge_ts$us_eur_exch%>%gsub(",","",.)%>%as.numeric()
merge_ts$oil_price <- merge_ts$oil_price%>%gsub(",","",.)%>%as.numeric()

#All the missing values for SP500, gold_price, us_eur_exch and oil_price coerced to NA's

#4. Plotting the Series 

#Bitcoin Price Series
ggplot(merge_ts, aes(x=DATE, y=bitcoin_price))+geom_line(na.rm = TRUE)+
  scale_x_date(labels=date_format ("%Y-%b"))+
  labs(title="Bitcoin Price Series", subtitle="Jan 2014-Oct 2021", x="Date", y="Bitcoin Price ($)")

#S&P 500 Index Series
ggplot(merge_ts, aes(x=DATE, y=SP500))+geom_line(na.rm = TRUE)+
  scale_x_date(labels=date_format ("%Y-%b"))+
  labs(title="S&P 500 Index Series", subtitle="Jan 2014-Oct 2021", x="Date", y="S&P 500 Index")

#Gold Prices Series
ggplot(merge_ts, aes(x=DATE, y=gold_price))+geom_line(na.rm = TRUE)+
  scale_x_date(labels=date_format ("%Y-%b"))+
  labs(title="Gold Price Series", subtitle="Jan 2014-Oct 2021", x="Date", y="Gold Prices ($)")

#US Euro Exchange Rate Series
ggplot(merge_ts, aes(x=DATE, y=us_eur_exch))+geom_line(na.rm = TRUE)+
  scale_x_date(labels=date_format ("%Y-%b"))+
  labs(title="US Euro Exchange Rate Series", subtitle="Jan 2014-Oct 2021", x="Date", y="US/Euro Exchange Rate")

#West Texas Intermediate Spot Price Series
ggplot(merge_ts, aes(x=DATE, y=oil_price))+geom_line(na.rm = TRUE)+
  scale_x_date(labels=date_format ("%Y-%b"))+
  labs(title="West Texas Oil Price Series", subtitle="Jan 2014-Oct 2021", x="Date", y="Intermediate Spot Price")

#5. Naive Linear Model of Bitcoin Price on Other Series in the Data set
n_bitcoin_model <-lm(bitcoin_price~SP500+gold_price+us_eur_exch+oil_price+DATE, merge_ts) 
summary(n_bitcoin_model)

#6. KPSS Test-Find Number of Differences 
rep.kpss(merge_ts$bitcoin_price)
rep.kpss(merge_ts$SP500)
rep.kpss(merge_ts$gold_price)
rep.kpss(merge_ts$us_eur_exch)
rep.kpss(merge_ts$oil_price)

#All of the above series will become level and trend stationary after taking the first difference

#7. Regression Bitcoin Price on Other Series in the Data set-After Differencing 

#merge_ts$date <- as.numeric(merge_ts$DATE)

diff_bitcoin_model <-lm(diff(bitcoin_price)~diff(SP500)+diff(gold_price)+diff(us_eur_exch)+diff(oil_price)+DATE[2:nrow(merge_ts)], merge_ts) 
summary(diff_bitcoin_model)

#8. Removing Data before Jan 2017 and Plotting the Series
merge_ts_2017 <- merge_ts[DATE>="2017-01-01"]

#Plot the New Bitcoin Price Series-2017 and Onwards
ggplot(merge_ts_2017, aes(x=DATE, y=bitcoin_price))+geom_line(na.rm = TRUE)+
  scale_x_date(labels=date_format ("%Y-%b"))+
  labs(title="Bitcoin Price Series", subtitle="Jan 2017-Oct 2021", x="Date", y="Bitcoin Price ($)")

#9. ACF and PACF Plots of the Bitcoin Price Series (Differenced Series)

#Plotting ACF and PACF of Stationary Bitcoin Price Series
acf(diff(merge_ts_2017$bitcoin_price))
pacf(diff(merge_ts_2017$bitcoin_price))


#10. Fitting Various ARIMA Models and Selecting the Best using AIC

#We do not observe any typical exponential decaying or sinusoidal pattern decaying trend in ACF and there is no significant spike at lag p (after which there is no significant spike) in PACF. We cannot get to know about AR(p) process from these plots
#We also do not observe any typical exponential decaying or sinusoidal pattern decaying trend in PACF and there is no significant spike at lag p (after which there is no significant spike) in ACF. We cannot get to know about MR(q) process from these plots. 
#We now establish base ARIMA models ARIMA(0,1,0), ARIMA(1,1,0), ARIMA(0,1,1), ARIMA(1,1,1) and try few models before using AIC and BIC criterion to find the best prediction model.

#Model A ARIMA(0,1,0)
modela <- Arima(merge_ts_2017$bitcoin_price, c(0,1,0), include.drift=TRUE)
modela

#Model B ARIMA(1,1,0)
modelb <- Arima(merge_ts_2017$bitcoin_price, c(1,1,0), include.drift=TRUE)
modelb

#Model C ARIMA(0,1,1)
modelc <- Arima(merge_ts_2017$bitcoin_price, c(0,1,1), include.drift=TRUE)
modelc

#Model D ARIMA(0,1,1)
modeld <- Arima(merge_ts_2017$bitcoin_price, c(1,1,1), include.drift=TRUE)
modeld

#Model E ARIMA(2,1,2)
modele <- Arima(merge_ts_2017$bitcoin_price, c(2,1,2), include.drift=TRUE)
modele

#Model E ARIMA(3,1,3)
modele <- Arima(merge_ts_2017$bitcoin_price, c(3,1,3), include.drift=TRUE)
modele

#Model F ARIMA(4,1,4)
modelf <- Arima(merge_ts_2017$bitcoin_price, c(4,1,4), include.drift=TRUE)
modelf

#Model G ARIMA(5,1,5)
modelg <- Arima(merge_ts_2017$bitcoin_price, c(5,1,5), include.drift=TRUE)
modelg

#Model H ARIMA(6,1,6)
modelh <- Arima(merge_ts_2017$bitcoin_price, c(6,1,6), include.drift=TRUE)
modelh

#Model I ARIMA(6,1,0)
modeli <- Arima(merge_ts_2017$bitcoin_price, c(6,1,0), include.drift=TRUE)
modeli

#Model J ARIMA(0,1,6)
modelj <- Arima(merge_ts_2017$bitcoin_price, c(0,1,6), include.drift=TRUE)
modelj

#Model K ARIMA(7,1,7)
modelk <- Arima(merge_ts_2017$bitcoin_price, c(7,1,7), include.drift=TRUE)
modelk

#Model L ARIMA(7,1,0)
modell <- Arima(merge_ts_2017$bitcoin_price, c(7,1,0), include.drift=TRUE)
modell

#Model M ARIMA(7,1,0)
modelm <- Arima(merge_ts_2017$bitcoin_price, c(0,1,7), include.drift=TRUE)
modelm

#Model O ARIMA(17,1,17)
modeln <- Arima(merge_ts_2017$bitcoin_price, c(17,1,17), include.drift=TRUE)
modeln


#Model Selection using AIC
AIC(modela)          #20706.89
AIC(modelb)          #20708.89
AIC(modelc)          #20708.47
AIC(modeld)          #20710.46
AIC(modele)          #20686.35       
AIC(modelf)          #20689
AIC(modelg)          #20676.17
AIC(modelh)          #20679.52 
AIC(modeli)          #20696.94
AIC(modelj)          #20696.42
AIC(modelk)          #20669.45
AIC(modell)          #20682.46
AIC(modelm)          #20688.23
AIC(modeln)          #20619.5

min(c(AIC(modela),AIC(modelb), AIC(modelc), AIC(modeld), AIC(modele), AIC(modelf), AIC(modelg), AIC(modelh), AIC(modeli), AIC(modelj), AIC(modelk)), AIC(modell), AIC(modelm), AIC(modeln))

#The model n has the lowest AIC value of 20619.5 so we select this ARIMA(17,1,17) model for our predictions. 

#11. Forecast for the Next 30 Days and Forecast Plot
forecast_arima <- forecast(modeln,h=30)
autoplot(forecast_arima)+labs(title="Forecast of Bitcoin Prices for Next 30 Days", y="Bitcoin Price ($)")

#The forecast shows a slightly upward trend in the bitcoin price for the next 30 days.

#12. Periodogram of the Difference of Bitcoin Series 
TSA::periodogram(diff(merge_ts_2017$bitcoin_price))

#Inspection for Seasonality 

#The periodogram peaks at two frequencies i.e., 0.1 and 0.28 (approximate values). 
1/.1            #Period=10 days
1/.28           #Period=3.57~4 days (maximum/highest peak in the periodogram at this frequency)

#Then seasonal trend in the data repeats after 4 days. 

#13. Regression of Stationarity-Transformed Price on the Dummy Variables for Different Days of the Week

#Extracting the Data of the Week Variable
merge_ts_2017$weekday<- lubridate::wday(merge_ts_2017$DATE)

#Constructing Dummy Variables [The data does not include bitcoin prices on weekends]
merge_ts_2017$mon<- ifelse(merge_ts_2017$weekday==2,1,0)
merge_ts_2017$tues<- ifelse(merge_ts_2017$weekday==3,1,0)
merge_ts_2017$wed<- ifelse(merge_ts_2017$weekday==4,1,0)
merge_ts_2017$thurs<- ifelse(merge_ts_2017$weekday==5,1,0)
merge_ts_2017$fri <- ifelse(merge_ts_2017$weekday==6,1,0)

#Calculating the Differenced Bitcoin Price Series
diff_bitcoin <- diff(merge_ts_2017$bitcoin_price)

#Sub-setting the merge_ts_2017 Data set
subset_merge_ts_2017 <- merge_ts_2017[2:1245]

#Binding the Difference Bitcoin Price Series to Subsetted Data
subset_merge_ts_2017$diff_bitcoin <- diff_bitcoin

#Regression of Stationarity-Transformed Price on the Dummy Variables of Days
bitcoin_price_day <- lm(diff_bitcoin~mon+tues+wed+thurs, subset_merge_ts_2017)
summary(bitcoin_price_day)

#Residuals of the Model
subset_merge_ts_2017$res_season <- residuals(bitcoin_price_day)

#Periodogram of the Residuals
TSA::periodogram(subset_merge_ts_2017$res_season)

#The periodogram has not changed significantly and the model has not helped us capture seasonality in the data.

#14. Vector AutoRegressive Model (p) for the Bitcoin Time Series

#Taking First Difference to Make Series' Stationary
diff_sp_500 <- diff(merge_ts_2017$SP500)
diff_gold <- diff(merge_ts_2017$gold_price)
diff_exch <- diff(merge_ts_2017$us_eur_exch)
diff_oil <- diff(merge_ts_2017$oil_price)

#Column Binding the 5 Differenced Series
merged_ts <- cbind(diff_bitcoin, diff_sp_500, diff_gold, diff_exch, diff_oil)
merged_ts <- na.omit(merged_ts)         #Removing Rows Containing Missing Values because VAR function does not accept missing values

#Selecting the Best VAR (p) using AIC
AIC(vars::VAR(merged_ts,1,type="both"))   #AIC=33978.19
AIC(vars::VAR(merged_ts,2,type="both"))   #AIC=33916.79
AIC(vars::VAR(merged_ts,3,type="both"))   #AIC=33869.68
AIC(vars::VAR(merged_ts,4,type="both"))   #AIC=33861.67

#The VAR(4) model best captures the relationship between our 5 variables based on AIC.

#Granger's Causality Relationships
modelz <- vars::VAR(merged_ts,4,type="both")
summary(vars::VAR(merged_ts,4,type="both"))

#Changes in gold price and US/Euro exchange rate Granger-cause change in the bitcoin price
#Changes in gold price, oil price and US/Euro exchange rate Granger-cause change in the S&P 500 index
#Changes in US/Euro exchange rate Granger-cause change in the gold price
#Changes in gold price Granger-cause change in the US/Euro exchange rate
#Changes in S&P 500 index and gold prices Granger-cause change in the oil price

#15. Forecast for Next 30 days based on VAR (4) Model and Comparison with ARIMA Model 

#Store Forecasts in the forecast_ar variable
forecast_ar <- predict(modelz,n.ahead=30, ci=.95)

#Plotting the VAR Forecast
par(mar=c(3,3,3,3))
plot(forecast_ar)

#Plotting the ARIMA Forecast
autoplot(forecast_arima)+labs(title="Forecast of Bitcoin Prices for Next 30 Days", y="Bitcoin Price ($)")

#It is difficult to compare the forecasts particularly because the forecasts for AR model are in terms of the first differenced series values and the ARIMA predictions are in level scale.
#The predictions of the differenced series show that the differences in bitcoin prices is relatively smaller, indicating that the prices are going to remain at a stagnant level. However, they are expected to follow a slight positve trend if any. 
#The prediction interval for the differenced series prediction based on AR model is comparatively smaller to the predictions from ARIMA model. 



