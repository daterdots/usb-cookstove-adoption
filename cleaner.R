# libraries and boring stuff ---------------------------------
rm(list = ls())
suppressMessages(library(dplyr))
library(ggplot2)
library(tidyr)
library(data.table)
library(reshape2)
library(zoo)
library(RColorBrewer)

#set the working directory
setwd('~/GitHub/usb-cookstove-adoption')

#load up the data
load("data.Rda")

data$stove = as.factor(data$stove)

#add deploy periods
deploy_periods = read.csv("village start and stop times.csv", stringsAsFactors=FALSE)
parsetime2 = function(date_time){
  as.POSIXct(strptime(date_time, "%m/%d/%y %H:%M",tz=""))
}
deploy_periods$start_datetime = parsetime2(deploy_periods$start)
deploy_periods$stop_datetime = parsetime2(deploy_periods$stop)
deploys = select(deploy_periods, village, start_datetime, stop_datetime)
deploys$village = as.factor(deploys$village)
deploys$deploy_period = difftime(deploys$stop_datetime, deploys$start_datetime, units = "days")

df = merge(data, deploys, by = 'village')

#get rid of data for the training (call it the first 6 hours after training starts)
training_hours = 6
df[,'training'] = ifelse(df$datetime < df$start_datetime + 1 * 60 * 60 * training_hours, TRUE, FALSE)
df[,'fan_state'] = ifelse(df$fan_v > 3, 'off', ifelse(df$fan_v < 0.5, 'high', 'low'))
df[,'stove_type'] = ifelse(substr(df$stove, 1, 1) == 'A', 'USB', 'no USB')
df[,'pot_state'] = ifelse(df$switch_v < 2, 'on', 'off')
df[,'usb_state'] = ifelse(df$usb_v < 4, 'off', 'on')
df[,'charge_state'] = ifelse(df$usb_v < 5 & df$stove_type == 'USB' & df$usb_state == 'on', 'charging', 'not charging')

#create a simple definition of "using" by saying "using" is when the fan is on and it's not the traning session
df[,'using'] = ifelse(df$fan_state != 'off' & df$training == FALSE, TRUE, FALSE)

#refine the definition of "using" into "events"
max_break_points = 10 #max number of datapoints to makeup a real break
min_event_points = 30 #minmum number of datapoints in a real event
rl_obj = rle(df$using)

#remove short breaks between cooking (< ~90s)
rl_obj$values[rl_obj$lengths < max_break_points & rl_obj$values == F] = T
df$using_smoothed1=inverse.rle(rl_obj)

#remove short cooking events (< ~ 5m)
rl_obj2 = rle(df$using_smoothed1)
rl_obj2$values[rl_obj2$lengths < min_event_points & rl_obj2$values == T] = F
df$using_smoothed2=inverse.rle(rl_obj2)

#number cooking events sequentially
rl_obj3=rle(df$using_smoothed2)
rl_obj3$values=ifelse(rl_obj3$values == T, cumsum(rl_obj3$values), NA)
event_nums=inverse.rle(rl_obj3)
df$event_num = event_nums

#make a new time series dataframe that only includes points that are part of events
ts_data = df[!is.na(df$event_num),]

save(ts_data,file="ts_data.Rda")
save(deploys,file="deploys.Rda")

