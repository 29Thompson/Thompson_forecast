library(forecast)
library(fpp2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fUnitRoots)

setwd("~/Documents/Capstone")

#Reading in and checking the data   
data <- read.csv("dt_kwh_1.csv")
head(data)

data <- data %>% 
  select(dt,kwh)

head(data)

data <- data %>% 
  mutate(dt = ymd_hms(dt))

head(data)
str(data)
tail(data)

#Getting hourly data
data_hr <- data %>%
  mutate(date = date(dt),
         hour = hour(dt)) %>%
  group_by(date,hour) %>%
  summarize(kwh = mean(kwh)) %>% 
  ungroup

head(data_hr)

data_hr <- data_hr %>% 
  mutate( time = as.character(hour),
          dt = ymd_h(paste(date,time)))


head(data_hr)

data_hr <- data_hr %>% 
  select(dt,kwh)

head(data_hr)
tail(data_hr)
str(data_hr)

#Plots data nicely 
#data <- data_hr#tail(data_hr, 24*7*13.04*4)
for_plot <- tail(data, 96*7*140)
ggplot(for_plot, aes(dt, kwh)) + geom_line()  + xlab("Year") + ylab("kw") + ggtitle("University of Montana Energy Use")


#Get some descriptives
max(data_hr$kwh)
min(data_hr$kwh)
max(data_hr$dt)
min(data_hr$dt)

#Making a subset of the data len 731 days, hrly data called elec
elec <- tail(data_hr, 24*731)
head(elec)
str(elec)

#Getting some descriptives
mean(elec$kwh)
max(elec$kwh)
min(elec$kwh)

#Make time series object of elect with daily, weekly, quarterly, and annually seasonality 
msts_kwhr<-elec$kwh %>% msts(seasonal.periods = c(24, 24*7, 24*7*13.04, 24*7*13.04*4))
msts_kwhr   %>% mstl() %>% autoplot()

#Create test and training set for 1 week forecast
msts_train <- head(msts_kwhr, length(msts_kwhr) - 24*7)
msts_test <- tail(msts_kwhr,  24*7)

#Tbats forecasting model 
tbats_mod <- msts_train %>%
  log() %>% 
  tbats(use.box.cox = FALSE, 
        use.trend = FALSE, 
        use.damped.trend = FALSE)
tbats_model <-  forecast(tbats_mod,h=24*7) 

#STLM Model 
stlm_model <- msts_train %>%
  stlm(lambda = 0) %>% 
  forecast(h = 24*7) 

#Getting accuracy of models
result<-rbind(accuracy(as.vector(stlm_model$mean) , msts_test), 
              accuracy(as.vector(exp(tbats_model$mean)) , msts_test)) 
rownames(result) <- c("stlm_model","tbats_model")
result

#Plotting accuracy of models
accuracyData <- data.frame(datetime= elec$dt %>% tail(24*7),
                           actual = as.vector(msts_test) ,
                           stlmForecast = as.vector(stlm_model$mean) ,
                           tbatsForecast = as.vector(exp(tbats_model$mean))
)


accuracyData %>% 
  ggplot() +
  geom_line(aes(x = (elec$dt %>% tail(24*7)), y = (elec$kwh %>% tail(24*7)), colour = "Actual"))+
  geom_line(aes(x = (elec$dt %>% tail(24*7)), y = stlm_model$mean, colour = "STLM"))+
  geom_line(aes(x = (elec$dt %>% tail(24*7)), y = exp(tbats_model$mean),   colour = "TBATS"))+ 
  #scale_y_continuous(labels = comma)+
  labs(
    title = "A Comparison of Forecasting Techniques",
    x = "Date",
    y = "kwh",
    colour = ""
  )

#Look at predictions 
exp(tbats_model$mean)
stlm_model$mean
max(elec$kwh %>% tail(24*7))

