rm(list=ls())

library(wbstats)
library(xts)
library(tseries)
library(ggplot2)
library(tidyverse)
library(urca)
library(forecast)
library(vars)
library(pco)
library(cointReg)

Ind_GDP_Growth<-as.data.frame(wb_data("NY.GDP.MKTP.KD.ZG", start_date=1993, end_date=2022, country="India"))
Ind_GDP_Growth<-Ind_GDP_Growth[,-c(1,2,3,6,7,8,9)]

Pak_GDP_Growth<-as.data.frame(wb_data("NY.GDP.MKTP.KD.ZG", start_date=1993, end_date=2022, country="Pakistan"))
Pak_GDP_Growth<-Pak_GDP_Growth[,-c(1,2,3,6,7,8,9)]

Bgl_GDP_Growth<-as.data.frame(wb_data("NY.GDP.MKTP.KD.ZG", start_date=1993, end_date=2022, country="Bangladesh"))
Bgl_GDP_Growth<-Bgl_GDP_Growth[,-c(1,2,3,6,7,8,9)]

Nep_GDP_Growth<-as.data.frame(wb_data("NY.GDP.MKTP.KD.ZG", start_date=1993, end_date=2022, country="Nepal"))
Nep_GDP_Growth<-Nep_GDP_Growth[,-c(1,2,3,6,7,8,9)]

Chi_GDP_Growth<-as.data.frame(wb_data("NY.GDP.MKTP.KD.ZG", start_date=1993, end_date=2022, country="China"))
Chi_GDP_Growth<-Chi_GDP_Growth[,-c(1,2,3,6,7,8,9)]

df_gdp<-cbind(Ind_GDP_Growth, Pak_GDP_Growth, Bgl_GDP_Growth, Nep_GDP_Growth, Chi_GDP_Growth)
df_gdp<-df_gdp[,-c(3,5,7,9)]

colnames(df_gdp)[2]<-"Ind_GDP_Growth"
colnames(df_gdp)[3]<-"Pak_GDP_Growth"
colnames(df_gdp)[4]<-"Bgl_GDP_Growth"
colnames(df_gdp)[5]<-"Nep_GDP_Growth"
colnames(df_gdp)[6]<-"Chi_GDP_Growth"


adf.test(df_gdp$Ind_GDP_Growth)
adf.test(df_gdp$Pak_GDP_Growth)
adf.test(df_gdp$Bgl_GDP_Growth)
adf.test(df_gdp$Nep_GDP_Growth)
adf.test(df_gdp$Chi_GDP_Growth)

#Doing same for Remittance as a percentage of GDP
Ind_Remit<-as.data.frame(wb_data("BX.TRF.PWKR.DT.GD.ZS", start_date=1993, end_date=2022, country="India"))
Ind_Remit<-Ind_Remit[,-c(1,2,3,6,7,8,9)]

Pak_Remit<-as.data.frame(wb_data("BX.TRF.PWKR.DT.GD.ZS", start_date=1993, end_date=2022, country="Pakistan"))
Pak_Remit<-Pak_Remit[,-c(1,2,3,6,7,8,9)]

Bgl_Remit<-as.data.frame(wb_data("BX.TRF.PWKR.DT.GD.ZS", start_date=1993, end_date=2022, country="Bangladesh"))
Bgl_Remit<-Bgl_Remit[,-c(1,2,3,6,7,8,9)]

Nep_Remit<-as.data.frame(wb_data("BX.TRF.PWKR.DT.GD.ZS", start_date=1993, end_date=2022, country="Nepal"))
Nep_Remit<-Nep_Remit[,-c(1,2,3,6,7,8,9)]

Chi_Remit<-as.data.frame(wb_data("BX.TRF.PWKR.DT.GD.ZS", start_date=1993, end_date=2022, country="China"))
Chi_Remit<-Chi_Remit[,-c(1,2,3,6,7,8,9)]

df_remit<-cbind(Ind_Remit, Pak_Remit, Bgl_Remit, Nep_Remit, Chi_Remit)
df_remit<-df_remit[,-c(3,5,7,9)]

colnames(df_remit)[2]<-"Ind_Remittance_Received"
colnames(df_remit)[3]<-"Pak_Remittance_Received"
colnames(df_remit)[4]<-"Bgl_Remittance_Received"
colnames(df_remit)[5]<-"Nep_Remittance_Received"
colnames(df_remit)[6]<-"Chi_Remittance_Received"

adf.test(df_remit$Ind_Remittance_Received)
adf.test(df_remit$Pak_Remittance_Received)
adf.test(df_remit$Bgl_Remittance_Received)
adf.test(df_remit$Nep_Remittance_Received)
adf.test(df_remit$Chi_Remittance_Received)

#Time Series Plot for GDP Growth

ts_gdp<-round(data.frame(year=1993:2021,
                         ts1=df_gdp$Ind_GDP_Growth,
                         ts2=df_gdp$Pak_GDP_Growth,
                         ts3=df_gdp$Bgl_GDP_Growth,
                         ts4=df_gdp$Nep_GDP_Growth,
                         ts5=df_gdp$Chi_GDP_Growth))

plot(ts_gdp$year,
     ts_gdp$ts1,
     xlab="Year",
     ylim=c(-13, 15),
     ylab="Percentage Change",
     type="l",
     col=2,
     main="GDP Growth",
     col.main="steelblue")
lines(ts_gdp$year,
      ts_gdp$ts2,
      type="l",
      col=3)
lines(ts_gdp$year,
      ts_gdp$ts3,
      type="l",
      col=4)
lines(ts_gdp$year,
      ts_gdp$ts4,
      type="l",
      col=5)
lines(ts_gdp$year,
      ts_gdp$ts5,
      type="l",
      col=6)
legend("bottomleft",
       c("India", "Pakistan", "Bangladesh", "Nepal", "China"),
       lty=1,
       col=2:6)

#lag selection
lagselect<-VARselect(ts_gdp, lag.max=7, type="const")
lagselect$selection
#since AIC is 4, we use 4-1=3 lag order in this case 


#repeating for Remittance growth
#Time Series Plot for GDP Growth

ts_remit<-round(data.frame(year=1993:2021,
                           ts_1=df_remit$Ind_Remittance_Received,
                           ts_2=df_remit$Pak_Remittance_Received,
                           ts_3=df_remit$Bgl_Remittance_Received,
                           ts_4=df_remit$Nep_Remittance_Received,
                           ts_5=df_remit$Chi_Remittance_Received))

plot(ts_remit$year,
     ts_remit$ts_1,
     xlab="Year",
     ylim=c(0, 27),
     ylab="Percentage of GDP",
     type="l",
     col=2,
     main="Personal Remittance Received",
     col.main="steelblue")
lines(ts_remit$year,
      ts_remit$ts_2,
      type="l",
      col=3)
lines(ts_remit$year,
      ts_remit$ts_3,
      type="l",
      col=4)
lines(ts_remit$year,
      ts_remit$ts_4,
      type="l",
      col=5)
lines(ts_remit$year,
      ts_remit$ts_5,
      type="l",
      col=6)
legend("topleft",
       c("India", "Pakistan", "Bangladesh", "Nepal", "China"),
       lty=1,
       col=2:6)

#lag selection
lagselectremit<-VARselect(ts_remit, lag.max=7, type="const")
lagselect$selection
#since AIC is 4, we use 4-1=3 lag order in this case 


#do ADF and plot ACFs

#making AutoCorrelation Function Plots
#ACF of GDP growth 
acf(Ind_GDP_Growth$NY.GDP.MKTP.KD.ZG, main="ACF of India's GDP Growth Series")
acf(Pak_GDP_Growth$NY.GDP.MKTP.KD.ZG, main="ACF of Pakistan's GDP Growth Series")
acf(Bgl_GDP_Growth$NY.GDP.MKTP.KD.ZG, main="ACF of Bangladehs's GDP Growth Series")
acf(Nep_GDP_Growth$NY.GDP.MKTP.KD.ZG, main="ACF of Nepal's GDP Growth Series")
acf(Chi_GDP_Growth$NY.GDP.MKTP.KD.ZG, main="ACF of China's GDP Growth Series")


#ACF of Remittance as percentage of GDP  
acf(Ind_Remit$BX.TRF.PWKR.DT.GD.ZS, main="ACF of India's Remittance as % of GDP")
acf(Pak_Remit$BX.TRF.PWKR.DT.GD.ZS, main="ACF of Pakistan's Remittance as % of GDP ")
acf(Bgl_Remit$BX.TRF.PWKR.DT.GD.ZS, main="ACF of Bangladesh's Remittance as % of GDP")
acf(Nep_Remit$BX.TRF.PWKR.DT.GD.ZS, main="ACF of Nepal's Remittance as % of GDP")
acf(Chi_Remit$BX.TRF.PWKR.DT.GD.ZS, main="ACF of China's Remittance as % of GDP")


#testing for cointegration (IS THE LINEAR COMBINATION OF NON STATIONARY SERIES STATIONARY?)

#Cointegration test of India's remittance growth and gdp growth 

jotest_Ind=ca.jo(data.frame(Ind_GDP_Growth$NY.GDP.MKTP.KD.ZG, Ind_Remit$BX.TRF.PWKR.DT.GD.ZS), type="trace", K=3, ecdet="none", spec="longrun")
summary(jotest_Ind)

jotest_Chi=ca.jo(data.frame(Chi_GDP_Growth$NY.GDP.MKTP.KD.ZG, Chi_Remit$BX.TRF.PWKR.DT.GD.ZS),type="trace", K=3, ecdet="none", spec="longrun")
summary(jotest_Chi)

jotest_Pak=ca.jo(data.frame(Pak_GDP_Growth$NY.GDP.MKTP.KD.ZG, Pak_Remit$BX.TRF.PWKR.DT.GD.ZS),type="trace", K=3, ecdet="none", spec="longrun")
summary(jotest_Pak)

jotest_Nep=ca.jo(data.frame(Nep_GDP_Growth$NY.GDP.MKTP.KD.ZG, Nep_Remit$BX.TRF.PWKR.DT.GD.ZS),type="trace", K=3, ecdet="none", spec="longrun")
summary(jotest_Nep)

jotest_Bgl=ca.jo(data.frame(Bgl_GDP_Growth$NY.GDP.MKTP.KD.ZG, Bgl_Remit$BX.TRF.PWKR.DT.GD.ZS),type="trace", K=3, ecdet="none", spec="longrun")
summary(jotest_Bgl)

res_ind <- cor.test(Ind_GDP_Growth$NY.GDP.MKTP.KD.ZG, Ind_Remit$BX.TRF.PWKR.DT.GD.ZS, 
                method = "pearson")
res_ind

res_chi <- cor.test(Chi_GDP_Growth$NY.GDP.MKTP.KD.ZG, Chi_Remit$BX.TRF.PWKR.DT.GD.ZS, 
                    method = "pearson")
res_chi

res_pak <- cor.test(Pak_GDP_Growth$NY.GDP.MKTP.KD.ZG, Pak_Remit$BX.TRF.PWKR.DT.GD.ZS, 
                    method = "pearson")
res_pak

res_bgl <- cor.test(Bgl_GDP_Growth$NY.GDP.MKTP.KD.ZG, Bgl_Remit$BX.TRF.PWKR.DT.GD.ZS, 
                    method = "pearson")
res_bgl

res_nep <-cor.test(Nep_GDP_Growth$NY.GDP.MKTP.KD.ZG, Nep_Remit$BX.TRF.PWKR.DT.GD.ZS, 
                               method = "pearson")
res_nep

cointRegD(Nep_GDP_Growth$NY.GDP.MKTP.KD.ZG, Nep_Remit$BX.TRF.PWKR.DT.GD.ZS, kernel = c("ba", "pa", "qs", "tr"),
          bandwidth = c("and", "nw"), n.lead = NULL, n.lag = NULL,
          kmax = c("k4", "k12"), info.crit = c("AIC", "BIC"), demeaning = FALSE,
          check = TRUE,)


cointRegD(Bgl_GDP_Growth$NY.GDP.MKTP.KD.ZG, Bgl_Remit$BX.TRF.PWKR.DT.GD.ZS, kernel = c("ba", "pa", "qs", "tr"),
          bandwidth = c("and", "nw"), n.lead = NULL, n.lag = NULL,
          kmax = c("k4", "k12"), info.crit = c("AIC", "BIC"), demeaning = FALSE,
          check = TRUE,)

####DETRENDING THE SERIES TO MAKE IT STATIONAR########
#first Remittance serires makes is stationary
Ind_Remit_detrended<-diff(Ind_Remit$BX.TRF.PWKR.DT.GD.ZS, differences=1)
adf.test(Ind_Remit_detrended)

Chi_Remit_detrended<-diff(Chi_Remit$BX.TRF.PWKR.DT.GD.ZS, differences=1)
adf.test(Chi_Remit_detrended)

Pak_Remit_detrended<-diff(Pak_Remit$BX.TRF.PWKR.DT.GD.ZS, differences=1)
adf.test(Pak_Remit_detrended)

Bgl_Remit_detrended<-diff(Bgl_Remit$BX.TRF.PWKR.DT.GD.ZS, differences=1)
adf.test(Bgl_Remit_detrended)

Nep_Remit_detrended<-diff(Nep_Remit$BX.TRF.PWKR.DT.GD.ZS, differences=1)
adf.test(Nep_Remit_detrended)
###-------------First Differencing GDP growth series-------------------
Ind_GDP_Growth_detrended<-diff(Ind_GDP_Growth$NY.GDP.MKTP.KD.ZG, differences=1)
adf.test(Ind_GDP_Growth_detrended)

Chi_GDP_Growth_detrended<-diff(Chi_GDP_Growth$NY.GDP.MKTP.KD.ZG, differences=1)
adf.test(Chi_Remit_detrended)

Pak_GDP_Growth_detrended<-diff(Pak_GDP_Growth$NY.GDP.MKTP.KD.ZG, differences=1)
adf.test(Pak_Remit_detrended)

Bgl_GDP_Growth_detrended<-diff(Bgl_GDP_Growth$NY.GDP.MKTP.KD.ZG, differences=1)
adf.test(Bgl_Remit_detrended)

Nep_GDP_Growth_detrended<-diff(Nep_GDP_Growth$NY.GDP.MKTP.KD.ZG, differences=1)
adf.test(Nep_GDP_Growth_detrended)

#Regressing 
lm(Ind_GDP_Growth_detrended~Ind_Remit_detrended)
lm(Chi_GDP_Growth_detrended~Chi_Remit_detrended)
lm(Bgl_GDP_Growth_detrended~Bgl_Remit_detrended)
lm(Nep_GDP_Growth_detrended~Nep_Remit_detrended)
lm(Pak_GDP_Growth_detrended~Pak_Remit_detrended)

#---------------------------------------------------TASK2----------------------------------------------------
# Read the data set
data <- read.csv("/Users/omarbalala/Desktop/ECON0128/amazon/Video_Games.csv")

#Loading the necessary packages
library(data.table)
library(tidyverse)
library(tidytext)
library(dplyr)
library(nnet)
library(boot)
library(tm)

#converting to dataframe and then to datatable
data<-as.data.frame(data)
setDT(data)

# Removing missing values
data <- data[!(is.na(data$reviewText)|is.na(data$summary)),]

#Counting the number of words in summary and text reviews
n_textwords<-sort(table(unlist(strsplit(data$reviewText, " "))),      
                  decreasing = TRUE)

View(n_textwords)

n_summary_words <- sort(table(unlist(strsplit(data$summary, " "))),
                        decreasing = TRUE)
View(n_summary_words) 

# Construct independent variables

#Construcing indenpendet variable for text
data$Number <- lengths(gregexpr("\\W+", data$reviewText))

#Counting number of words specific words associated with rating

data$good <- str_count(data$reviewText, "good")

data$great <- str_count(data$reviewText, "great")

data$excellent <- str_count(data$reviewText, "excellent")

data$love <- str_count(data$reviewText, "love")

data$happy <- str_count(data$reviewText, "happy")

data$amazing <- str_count(data$reviewText, "amazing")

data$bad <- str_count(data$reviewText, "bad")

data$terrible <- str_count(data$reviewText, "terrible")

data$hate <- str_count(data$reviewText, "hate")

data$trash <- str_count(data$reviewText, "trash")

data$dislike <- str_count(data$reviewText, "dislike")

data$disappointed <- str_count(data$reviewText, "disappointed")

data$awful <- str_count(data$reviewText, "awful")

# Removing the NA's in vote and replacing with 0
data$vote[is.na(data$vote)] <- 0
data$vote <- as.numeric(data$vote)

# Constructing independent variable for review summary
data$Number_sum <- lengths(gregexpr("\\W+", data$summary))

data$Zelda <- str_count(data$summary, "Zelda")

data$Amazing <- str_count(data$summary, "amazing")

data$Great <- str_count(data$summary, "great")

data$thrilling <- str_count(data$summary, "thrilling")

data$amazed <- str_count(data$summary, "amazed")

data$Five <- str_count(data$summary, "Five")

data$Stars <- str_count(data$summary, "Stars")

data$Four <- str_count(data$summary, "Four")

data$Three <- str_count(data$summary, "Three")

data$Two <- str_count(data$summary, "Two")

data$Perfect <- str_count(data$summary, "Perfect")

data$best <- str_count(data$summary, "Best")

data$Wow <- str_count(data$summary, "Wow")

# Transforming the dataset and making a new data set by deleting irrelevant columns 
data2 <- data[,-c(3:9)]



# Running a Poison Regression 
mod_a <- glm(overall ~ ., "poisson", data = data2)

#Finding execution time
t1 <- system.time(mod_a)
summary(mod2)

t1

# Performing Cross validation 
set.seed(123)

# Refitting linear regression
o <- glm(overall ~ ., "poisson", data=data2) 
sqrt(cv.glm(data2,mod2, K=10)$delta)


# Running Multinomial model
t2 <- system.time(mod_a <- multinom(overall ~ ., data = data2))
summary(mod1)

t2


# Cross validation
K <- 10
cv_index <- cut(
  x=1:nrow(data2),
  breaks=K,
  labels=FALSE)

# Setting reproducible shuffle
set.seed(123)
cv_index <- sample(cv_index, size=length(cv_index))

mse_cv <- c() 
for (i in 1:K){
  test_index <- which(cv_index == i)
  train <- data2[-test_index,]
  test <- data2[test_index,]
  fit <- multinom(
    formula=overall ~ .,
    data=train)
  mse_cv[i] <- mean((test$overall - predict(fit, test))^2)
}


mean(sqrt(mse_cv))


