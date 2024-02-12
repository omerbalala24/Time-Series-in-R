rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(collapse)
library(lubridate)
library(ggplot2)
library(readxl)
library(openxlsx)
library(forecast)
library(zoo)
library(tseries)

exre<-read_xlsx("/Users/omarbalala/Desktop/R practice/midterm assignement data/RBIB_Table_No._32___Foreign_Exchange_Reserves.xlsx", skip=2)

#output: 13 variables, 1155 observations

df_exre<-as.data.frame(exre[-c(1,2), -c(1,7, 8, 11)])

colnames(df_exre)[1]<-"Weekly_dates_starting_sixthapril2001"
colnames(df_exre)[2]<-"Foreign_Currency_Assets_INR_Crore"
colnames(df_exre)[3]<-"Foreign_Currency_Assets_USD_Million"
colnames(df_exre)[4]<-"Gold_INR_Crore"
colnames(df_exre)[5]<-"Gold_USD_Million"
colnames(df_exre)[6]<-"SDRs_INR_Crore"
colnames(df_exre)[7]<-"SDRs_USD_Million"
colnames(df_exre)[8]<-"Total_INR_Crore"
colnames(df_exre)[9]<-"Total_USD_Million"

df_exre<-na.omit(df_exre) #remove na values

#Comprehending beginning date of the time series and the last date
openxlsx::convertToDate(44862) #=2022-10-28
openxlsx::convertToDate(36987) #=2001-04-06

#creating date column for exre dataframe
Date <- as.Date(as.numeric(df_exre$Weekly_dates_starting_sixthapril2001), origin = "1899-12-30")
df_exre$date <- as.Date(as.numeric(df_exre$Weekly_dates_starting_sixthapril2001), origin = "1899-12-30")

df_exre= subset(df_exre, select = -c(Weekly_dates_starting_sixthapril2001) )#and removing the character dates indexed by excel
df_exre[, c(9,1:8)] #reordering columns in the dataframe of exre

#importing spot prices of crude oil
oilprice<-read.csv("/Users/omarbalala/Desktop/R practice/midterm assignement data/brent-week.csv")
df_oilprice<-as.data.frame(oilprice)#creating a dataframe of oilprice

class(df_oilprice$Date)#is character so creating creating a new column of it as row 

df_oilprice$date<-as.Date(df_oilprice$Date)


#subsetting data between the relevant dates
df_oilprice_1<-df_oilprice[df_oilprice$date >= "2001-04-06" &    # Extract data frame subset
                                        df_oilprice$date <= "2022-10-28", ]

df_oilprice_clean<-df_oilprice_1[, -c(1)]

df_oilprice_clean[, c(2,1)] #reordering columns in the dataframe of exre

#importing exchange rate dataset 
exra<-read_csv("/Users/omarbalala/Desktop/R practice/midterm assignement data/FRB_H10-2.csv", skip=5)
df_exra<-as.data.frame(exra)#making dataframe out of it

#changing colnames of dataframe to harmonise date id and make other column interpretable
colnames(df_exra)[1]<-"date"
colnames(df_exra)[2]<-"USD_to_INR"

#subsetting rows to only keep every 5th observation as this datasets excludes saturday and sunday observations
df_exra_clean<-df_exra %>%
  slice(which(row_number() %% 5 == 1))

#All datasets are cleaned and converted to dataframe.
#merging the dataframe into a new dataframe by id=date
df1<-merge(df_oilprice_clean, df_exra_clean, by = "date")

cleandataframe<-merge(df1, df_exre, by="date")

#clear ND in USD_to_INR to create timeseries
#USD_to_INR has some non numeric data saved as "ND" so changing it to NA
cleandataframe$USD_to_INR <- na_if(cleandataframe$USD_to_INR, 'ND') 

#now filling all na observations in USD_to_INR with last non missing value
cleandataframe$USD_to_INR<-na.locf(cleandataframe$USD_to_INR, FromLast = FALSE)

cleandataframe$USD_to_INR<-as.numeric(cleandataframe$USD_to_INR)
cleandataframe$Foreign_Currency_Assets_USD_Million<-as.numeric(cleandataframe$Foreign_Currency_Assets_USD_Million)

class(cleandataframe$USD_to_INR)
class(cleandataframe$Foreign_Currency_Assets_USD_Million)
class(cleandataframe$Price)

#All 3 variables of interest are in numeric form, with no missing values.

#summary statistics
##Time Series for Crude Oil Prices
crp <- ts(cleandataframe$Price, start = decimal_date(ymd("2001-04-06")),
          frequency = 365.25 / 7)

plot(crp, xlab ="Year",
     ylab ="Brent Crude Oil Price (USD per Barrel)",
     main ="Crude Oil Prices",
     col.main ="darkgreen")

#Time series for Exchange Reserves in India
fca <- ts(cleandataframe$Foreign_Currency_Assets_USD_Million, start = decimal_date(ymd("2001-04-06")),
          frequency = 365.25 / 7)

plot(fca, xlab ="Year",
     ylab ="Exchange Reserces in Millions (USD)",
     main ="RBI Foreign Exchange Reserves",
     col.main ="darkgreen")

fora<- ts(cleandataframe$USD_to_INR, start=decimal_date(ymd("2001-04-06")),
          frequency = 365.25/7)
plot(fora, xlab="Year",
     ylab="USD/INR",
     main="USD to INR Exchange Rates",
     col.main="dark green")

#residuals of crude oil price
resprice <- residuals(naive(cleandataframe$Price))
autoplot(resprice)+xlab("Weeks") + ylab("") + 
  ggtitle("Residuals of Crude oil Prices using Naive Method")

resexrate <- residuals(naive(cleandataframe$USD_to_INR))
autoplot(resexrate)+xlab("Weeks") + ylab("") + 
  ggtitle("Residuals of USD to INR Exchange Rates Naive Method")

resforassests <- residuals(naive(cleandataframe$Foreign_Currency_Assets_USD_Million))
autoplot(resforassests)+xlab("Weeks") + ylab("") + 
  ggtitle("Residuals of Foreign Currency Assets Using Naive Method")

#Autocrrelation test and then autocorrelation function

#ACF exchange rate series
acf(cleanandlaggeddf$USD_to_INR, main="ACF of USD to INR Exchange Rate")
acf_pctexra<-acf(cleanandlaggeddf$pctUSD_to_INR)
plot(acf_pctexra, main="USD to INR %Change Exchange Rate ACF", xlab="Week Lag")
#ADF test for checking echange rate stationarity
adf.test(cleanandlaggeddf$USD_to_INR)

#ACF of Oil price
acf(cleanandlaggeddf$Price, main="ACF Crude Oil Price")
acf_pctprice<-acf(cleanandlaggeddf$pctprice)
plot(acf_pctprice, main="Oil Price %Change ACF", xlab="Week Lag")

#ADF test for checking stationarity
adf.test(cleanandlaggeddf$Price)
adf.test(cleanandlaggeddf$Foreign_Currency_Assets_USD_Million)
adf.test(cleanandlaggeddf$USD_to_INR)

#ACF of currency reserves
acf(cleanandlaggeddf$Foreign_Currency_Assets_USD_Million, main="Forex Reserves ACF")
acf_pctextre<-acf(cleanandlaggeddf$pctexre)
plot(acf_pctextre,  main="Foreign Currency Assets %Change ACF", xlab="Week Lag")
#ADF test for checking currency reserves stationarity
adf.test(cleanandlaggeddf$Foreign_Currency_Assets_USD_Million)

#creating new dataframe with log columns
logdf<-log(cleanandlaggeddf[,c(2,3,5)])

adf.test(logdf$Price)
adf.test(logdf$USD_to_INR)
adf.test(logdf$Foreign_Currency_Assets_USD_Million)

#series are still non-stationary. So differencing the series to remove sesonal trends

#first differencing oil price serires makes is stationary
diff_price<-diff(cleanandlaggeddf$Price, differences=1)
adf.test(diff_price)
acf(diff_price, main="First Differenced Oil Prices Time Series ACF")

#first differencing exchange rates makes it stationary
diff_exra<-diff(cleanandlaggeddf$USD_to_INR, differences = 1)
adf.test(diff_exra)
acf(diff_exra, main="First differenced Exchange Rate ACF")

#first differencing exchange reserves makes it stationary
diff_exre<-diff(cleanandlaggeddf$Foreign_Currency_Assets_USD_Million, differences = 1)
adf.test(diff_exre)
acf(diff_exre, main="First Differenced Forex Reserve ACF")


#detrended time series
#plotting first differenced oilprice series
diffcrp <- ts(diff_price, start = decimal_date(ymd("2001-04-06")),
              frequency = 365.25 / 7)

plot(diffcrp, xlab ="Years",
     ylab ="First Diff Crude Oil Price (USD per Barrel)",
     main ="Crude Oil Prices",
     col.main ="darkgreen")

#plotting first differenced exchangereserves series
difffca <- ts(diff_exre, start = decimal_date(ymd("2001-04-06")),
              frequency = 365.25 / 7)

plot(difffca, xlab ="Years",
     ylab ="First Diff Exchange Reserves USDMillion",
     main ="RBI Foreign Exchange Reserves",
     col.main ="dark green")

#plotting first differenced exchangerates series
difffora<- ts(diff_exra, start=decimal_date(ymd("2001-04-06")),
              frequency = 365.25/7)
plot(difffora, xlab="Years",
     ylab="First Differenced USD/INR",
     main="USD to INR Exchange Rates",
     col.main="dark green")


#bivariate regression of the series
summary(lm(diff_exre~diff_price))
summary(lm(diff_exre~diff_exra))
#Multivariate regression of the series
summary(lm(diff_exre~diff_price+diff_exra))

#in order to create regression, we need percentage change values of the variables of interest
#This can be done by first creating lagged variable column then prctchange=((colname-laggedcol)/laggedcol)*100

#create lags
cleanandlaggeddf<-(cleandataframe$Lag_<-transform(cleandataframe,lag_Price=c(Price[-1],NA),lag_exra=c(USD_to_INR[-1],NA),lag_exre=c(Foreign_Currency_Assets_USD_Million[-1],NA)))

#Create precentage change columns
cleanandlaggeddf$pctprice<-((cleanandlaggeddf$lag_Price-cleanandlaggeddf$Price)/cleanandlaggeddf$lag_Price)*100
cleanandlaggeddf$pctUSD_to_INR<-((cleanandlaggeddf$lag_exra-cleanandlaggeddf$USD_to_INR)/cleanandlaggeddf$lag_exra)*100
cleanandlaggeddf$pctexre<-((cleanandlaggeddf$lag_exre-cleanandlaggeddf$Foreign_Currency_Assets_USD_Million)/cleanandlaggeddf$lag_exre)*100

#need to move all values of this column down by one row
cleanandlaggeddf$pctprice <- data.table::shift(cleanandlaggeddf$pctprice, fill = cleanandlaggeddf$pctprice[1])
cleanandlaggeddf$pctUSD_to_INR<-data.table::shift(cleanandlaggeddf$pctUSD_to_INR, fill = cleanandlaggeddf$pctUSD_to_INR[1])
cleanandlaggeddf$pctexre<-data.table::shift(cleanandlaggeddf$pctexre, fill = cleanandlaggeddf$pctexre[1])

#ADF of Percentage change series
adf.test(cleanandlaggeddf$pctprice)
adf.test(cleanandlaggeddf$pctUSD_to_INR)
adf.test(cleanandlaggeddf$pctexre)

summary(lm(cleanandlaggeddf$pctexre~cleanandlaggeddf$pctprice+cleanandlaggeddf$pctUSD_to_INR))

plot(diff_exre~diff_exra+diff_price)

plot(cleanandlaggeddf$Foreign_Currency_Assets_USD_Million~cleanandlaggeddf$USD_to_INR+cleanandlaggeddf$Price)

###PLOTTING!!
#plotting the crude oil price on exchange rate 
ggplot(cleandataframe, aes(y = Price, x=USD_to_INR)) + geom_smooth()
ggplot(cleandataframe, aes(y = Foreign_Currency_Assets_USD_Million, x=USD_to_INR)) + geom_smooth()
ggplot(cleandataframe, aes(y = Foreign_Currency_Assets_USD_Million, x=Price)) + geom_smooth()
#can clearly see seasonality which could be because of time trend. So need to detrend the series.
#must also perform unit root test to check if the series contains unit root.


#ggplot using percentage change of variables 

ggplot(cleanandlaggeddf, mapping=aes(x=pctprice, y=pctUSD_to_INR))+
  geom_point()+
  geom_smooth()+
  labs(x="Percentage change in Oil Price", y="Percentage change in USD to INR exchange rate", title="exchange rate change and oil price change")

ggplot(cleanandlaggeddf, aes(y=pctexre, x=pctprice))+geom_point()+geom_smooth()+
  labs(x="Percentage change in Oil Price", y="Percentage change Forex Reserves", title="Exchange reserve and oil price")

ggplot(cleanandlaggeddf, aes(y=pctexre, x=pctUSD_to_INR))+geom_point()+geom_smooth()+
  labs(x="Percentage change in Forex Reserve", y="Percentage Change Exchange Rate", title="Exchange reserve and Exchange Rate")


#Plotting residuals of orginal series
plot(lm(resforassests~resexrate))
plot(lm(resforassests~resprice))
plot(lm(resexrate~resprice))



