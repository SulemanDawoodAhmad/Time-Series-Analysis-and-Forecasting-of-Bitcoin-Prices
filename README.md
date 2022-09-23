# Time Series Analysis and Forecasting of Bitcoin Prices
 Time Series Analysis-ARIMA, VAR Models

Time series analysis and forecasting of bitcoin prices based on Auto Regressive Integrated Moving Averages (ARIMA) and Vector Autoregressive (VAR) models. Gold prices, oil prices, stock index and US/EURO exchange rates were considered as other series in the analysis. The analysis was performed on 6 years of data from 2014-2020. 

I started with KPSS test of level and trend stationairty and applied appropriate differencing to make all series stationary. In first half of the project, ARIMA model was employed to understand the bitcoin prices. Different levels of lags and moving averages were used to create multiple models and AIC was used to identify the best model. The identified model was then used to predict the bitcoin prices for the next 'n' days. 

After the ARIMA model, VAR models were considered to established granger causality between the series and different levels of lags were used. AIC was again used to pick the best possible model out the constructed ones and then it was used to predict bitcoin prices for the next 'n' days. 

The data importing, merging, data wrangling and anlysis was done in R using data.table, dplyr,ggplot2, TSA, var, tseries, scales and forecast packages. 

The code has comments that can help you understand each step, however, please share your feedback and feel free to ask any questions. Thanks!
