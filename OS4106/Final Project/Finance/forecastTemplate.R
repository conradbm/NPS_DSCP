library(tidyverse)
library(dplyr)
library(caret)
library(forecast)
library(lubridate)
library(ggplot2)
library(fpp2)

getwd()
setwd("C:\\Users\\bmccs\\OneDrive\\Finance")
df <- read.csv('TQQQ.csv', header=TRUE)
df <- df %>% mutate(Date=ymd(Date), Close = as.double(Close)) 
head(df)

df$PercentChangeClose <- (df$Close - lag(df$Close,1))/lag(df$Close,1)
df$CumulativePercentChangeClose <- c(NA, cumsum(df$PercentChangeClose[2:nrow(df)])* 100)

fit <- auto.arima(df$Close)
fc<-fit %>% forecast %>% data.frame
fc$Date<-seq(as.Date('2019-04-22'),as.Date('2019-05-01'),by = 1)
fc

ggplot(data=df) + 
  theme_bw()+
  geom_line(aes(x=Date, y=Close),color='black') + 
  scale_x_date(date_breaks = "months" , date_labels = "%y-%b-%d") +
  scale_y_continuous(breaks=seq(30,80,by=5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("TQQQ Closing Price", subtitle="April 2018- April 2019") +
  geom_hline(yintercept=df$Close[1],color='black')+
  geom_hline(yintercept=df$Close[nrow(df)], color='green') +
  geom_smooth(aes(x=Date, y=Point.Forecast, ymax=Hi.80, ymin=Lo.80), 
              colour='blue', data=fc, stat='identity', alpha=0.7)+
  geom_smooth(aes(x=Date, y=Point.Forecast, ymax=Hi.95, ymin=Lo.95), 
              colour='blue', data=fc, stat='identity', alpha=0.5)+
  geom_hline(yintercept=fc$Hi.80[length(fc$Date)-2], color='grey',alpha=0.9)+
  geom_hline(yintercept=fc$Lo.80[length(fc$Date)-2], color='grey',alpha=0.9)+
  ylab("Close ($)")

ggplot(data=df) + 
  theme_bw()+
  geom_line(aes(x=Date, y=CumulativePercentChangeClose),color='black') + 
  scale_x_date(date_breaks = "months" , date_labels = "%y-%b-%d") +
  scale_y_continuous(breaks=seq(-30,50,by=10))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("TQQQ Total Percent Increase", subtitle="April 2018- April 2019") +
  geom_hline(yintercept=df$CumulativePercentChangeClose[2],color='black')+
  geom_hline(yintercept=df$CumulativePercentChangeClose[nrow(df)], color='green') +
  ylab("Total Percent Increase (%)")


