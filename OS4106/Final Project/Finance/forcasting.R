

###
### PACKAGES 
###
#install.packages('forecast')
#install.packages('tidyverse')
#install.packages('caret')
#install.packages('lubridate')
#install.packages('fpp2')
library(tidyverse)
library(dplyr)
library(caret)
library(forecast)
library(lubridate)
library(ggplot2)
library(fpp2)

###
### SETUP WORKING AREA
getwd()
setwd("C:\\Users\\bmccs\\OneDrive\\Finance")
df <- read.csv('TQQQ.csv', header=TRUE)

###
### TRANSFORM DATA
###
head(df)
df <- df %>% mutate(Date=ymd(Date), Close = as.double(Close)) 
tail(df)
str(df)


###
### BASIc PLOT
###
closingPrices <- ts(df$Close, frequency = 12)
autoplot(closingPrices) + xlab("Day") + ylab("Closing Price ($)") +
  ggtitle("Daily Closing Price: TQQQ", subtitle = "Apr 20 2018 - Apr 20 2019")


###
### 30DAY-MOVING AVERAGE (30-MA)
###
autoplot(closingPrices, series="Data") +
  autolayer(ma(closingPrices,30), series="30-MA") +
  xlab("Day") + ylab("Closing Price ($)") +
  ggtitle("Daily Closing Price: TQQQ", subtitle = "Apr 20 2018 - Apr 20 2019") +
  scale_colour_manual(values=c("Data"="grey50","30-MA"="red"),
                      breaks=c("Data","30-MA"))

###
### CLASSICAL DECOMPOSITION
### Sometimes examining the trend can help you find windows of opportunity that avoid high peaks or low valleys
###
df$Date %>%  head
ts_df_close<-ts(df$Close,
                frequency =365.25)
?ts
ts.plot(ts_df_close)
fit<- decompose(ts_df_close,type="additive")
plot(fit)
plot(fit$trend-fit$seasonal-fit$random)
plot(fit$seasonal[1200:length(fit$seasonal)], type="line")
fit$seasonal %>% length

###
### STL FORECAST
###
ts_df_close
fit <- stl(ts_df_close,t.window=31, s.window="periodic",
           robust=TRUE)
fit %>% autoplot()
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("Closing ($)") + xlab("Day Number")
  ggtitle("TQQQ Prediction Interval" ,subtitle="Naive forecasts of seasonally adjusted data")

fit %>% seasadj() %>%  head(10)
preds <- fit %>% seasadj() %>% naive()
preds

