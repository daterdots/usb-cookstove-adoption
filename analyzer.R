# libraries and boring stuff ---------------------------------
rm(list = ls())
suppressMessages(library(dplyr))
library(ggplot2)
library(tidyr)
library(data.table)
library(reshape2)
library(zoo)
library(RColorBrewer)
library(gridExtra)
library(grid)

#set the working directory
setwd('~/GitHub/usb-cookstove-adoption')

#load up the data
load("ts_data.Rda")
load("deploys.Rda")

ts_data = ts_data %>%
  group_by(event_num) %>%
  mutate(duration = as.numeric(difftime(max(datetime), min(datetime), units = "secs")/60),
         current_duration = as.numeric(difftime(datetime, min(datetime), units = "secs")/60),
         prop_complete = current_duration/duration,
         obs = n(),
         sampling_interval = duration/obs * 60)

ts_data$is_charging = ts_data$charge_state=="charging"
ts_data$is_charging_pot_off = ts_data$charge_state=="charging" & ts_data$pot_state == "off" 
ts_data$is_usb_available = ts_data$usb_state=="on"

#define a categorical use state
ts_data$use_state=NA
ts_data$use_state[ts_data$pot_state == "off" & ts_data$usb_state=="off"]="pot-off & USB not ready"
ts_data$use_state[ts_data$pot_state == "off" & ts_data$usb_state=="on"]="pot-off & USB ready"
ts_data$use_state[ts_data$pot_state == "off" & ts_data$charge_state=="charging"]="pot-off & charging"
ts_data$use_state[ts_data$pot_state == "on" & ts_data$usb_state=="off"]="pot-on & USB not ready"
ts_data$use_state[ts_data$pot_state == "on" & ts_data$usb_state=="on"]="pot-on & USB ready"
ts_data$use_state[ts_data$pot_state == "on" & ts_data$charge_state=="charging"]="pot-on & charging"

x_cut=seq(from=min(ts_data$prop_complete),to=max(ts_data$prop_complete),length=101)
g_round=function(x,s){
  round(x*s,0)/s
}

ts_data$pc_binned=g_round(ts_data$prop_complete,20)

#### look at marginal cooking vs. cumulative cooking
ts_data = ts_data %>% group_by(village, stove) %>% 
  mutate(cum_cooking = cumsum(as.numeric(sampling_interval)/3600))
ts_data = ts_data %>% group_by(village, stove) %>% 
  mutate(days_in = ceiling(as.numeric(difftime(datetime, start_datetime, unit = "days"))))

ts_day_data = ts_data %>% group_by(village,stove,stove_type,days_in) %>% 
  summarise(cum_cooking2 = max(cum_cooking), daily_cooking = max(cum_cooking)-min(cum_cooking))

##percent complete data
pc_data = ts_data %>%
  group_by(village, stove, stove_type, event_num, pc_binned) %>%
  summarise(
    prop_ready= sum(is_usb_available)/n(),
    prop_charging = sum(is_charging)/n(), #what proportion of the PC you spend charging
    prop_charging_pot_off = sum(is_charging_pot_off)/n(), #what proportion of the PC you spend charging with pot off
    prop_charging_pot_off_ratio = prop_charging_pot_off / prop_charging, #ratio of pot off charging to total charging time
    prop_charging_usb_on_ratio = sum(is_charging) / sum(is_usb_available) #propotion of USB available time spent charging
  )

pc_state_data = ts_data %>%
  group_by(village, stove, stove_type, event_num, pc_binned, use_state) %>%
  summarise(
    n_in_state = n()
    )

pc_state_data = pc_state_data %>%
  group_by(village, stove, stove_type, event_num, pc_binned) %>%
  mutate(
    prop_in_state = n_in_state/sum(n_in_state)
  )

#instead of time series, this is "proportion complete" data (e.g. 10% completed)
pc_events = pc_data %>%
  group_by(stove_type, pc_binned) %>%
  summarise(
    prop_ready = mean(prop_ready, na.rm=T),
    prop_charging_event = mean(prop_charging, na.rm = T),
    prop_charging_pot_off_event = mean(prop_charging_pot_off, na.rm = T),
    prop_charging_pot_off_ratio_event = mean(prop_charging_pot_off_ratio, na.rm = T),
    prop_charging_usb_on_ratio_event = mean(prop_charging_usb_on_ratio, na.rm = T)
  )

#make data to plot charging as a function of percentage of event completed
pc_events_plot = filter(melt(pc_events, id.vars = c("stove_type","pc_binned")), stove_type == 'USB')

#instead of time series, this is "proportion complete" data (e.g. 10% completed)
pc_state_events = pc_state_data %>%
  group_by(stove_type, pc_binned,use_state) %>%
  summarise(
    prop_in_state = mean(prop_in_state, na.rm = T),
    n_in_state = sum(n_in_state, na.rm = T)
  )

pc_state_events = pc_state_events %>%
  group_by(stove_type, pc_binned) %>%
  mutate(
    prop_in_state2 = n_in_state/sum(n_in_state)
  )

pc_state_events$use_state2 = ifelse(pc_state_events$stove_type == "USB", pc_state_events$use_state, 
                                     ifelse(startsWith(pc_state_events$use_state,"pot-off"), "pot-off", "pot-on"))

pc_state_events2 = pc_state_events %>%
  group_by(stove_type, pc_binned, use_state2) %>%
  summarise(
    n_in_state = sum(n_in_state)
  )

pc_state_events2 = pc_state_events2 %>%
  group_by(stove_type, pc_binned) %>%
  mutate(
    prop_in_state2 = n_in_state/sum(n_in_state)
  )

#reorder factors
pc_state_events2$use_state2 =  factor(pc_state_events2$use_state2, levels = c("pot-off",
                                                                              "pot-on",
                                                                              "pot-off & USB not ready",
                                                                              "pot-off & USB ready",
                                                                              "pot-off & charging",
                                                                              "pot-on & charging", 
                                                                              "pot-on & USB ready",
                                                                              "pot-on & USB not ready"))

f_area = ggplot(pc_state_events2[order(pc_state_events2$use_state2),],
                aes(x=pc_binned*100,y=prop_in_state2*100,fill=use_state2))+
  geom_area(alpha = 0.5)+facet_wrap(~stove_type)+
  theme_bw()+scale_y_continuous(expand=c(0,0))+scale_x_continuous(expand=c(0,0))+
  scale_fill_manual(values=c("#cc0000","#336699","#cc0000","#ff3333","#ff9999","#b3cce6","#6699cc","#336699")) +
  theme(panel.margin = unit(1, "lines")) +
  labs(x = "Amount of Event Completed (%)", y = "Probability of Use Mode (%)", fill = "Use Mode", 
       title = "Use Mode vs. Event Completion") +
  guides(fill=guide_legend(ncol=1, reverse = T))
f_area
ggsave("figures/use-mode-area.pdf", width = 10, height = 4, units = "in")
unlink("use-mode-area.pdf")

events = ts_data %>%
  select(village, stove, stove_type, datetime, event_num, temperature, fan_state, pot_state, usb_state, charge_state) %>%
  group_by(village, stove, stove_type, event_num) %>%
  summarise(duration = difftime(max(datetime), min(datetime), units = "mins"),
            start_event_datetime = min(datetime),
            obs = n(),
            sampling_interval = duration/obs * 60,
            charge_time = length(which(charge_state == 'charging')) * sampling_interval / 60,
            charge_no_pot_time = length(which(charge_state == 'charging' & pot_state == 'off')) * sampling_interval / 60,
            pot_off_time = length(which(pot_state == 'off')) * sampling_interval / 60,
            charge_obs = length(which(charge_state == 'charging')),
            charge_no_pot_obs = length(which(charge_state == 'charging' & pot_state == 'off')),
            charge_no_pot_prop = charge_no_pot_obs/obs,
            charge_prop = charge_obs/obs,
            pot_off_obs = length(which(pot_state == 'off')),
            pot_off_prop = pot_off_obs/obs * 100,
            usb_available_obs = length(which(usb_state == 'on')),
            usb_unavailable_prop = (obs-usb_available_obs)/obs,
            charging_obs = length(which(charge_state == 'charging')),
            charging_prop_of_usb_available = charging_obs/usb_available_obs)
events = merge(events, deploys, by = 'village')
events$start_event_days_in = difftime(events$start_event_datetime, events$start_datetime, units = 'days')

users = events %>%
  group_by(village, stove, stove_type)%>%
  summarise(
    min_per_day = sum(duration)/as.numeric(median(deploy_period, na.rm = T)),
    charging_per_day = sum(charge_time)/as.numeric(median(deploy_period, na.rm = T)),
    charging_pot_off_per_day = sum(charge_no_pot_time)/as.numeric(median(deploy_period, na.rm = T)),
    charge_time_avg = mean(charge_time, na.rm = T),
    charge_no_pot_time_avg = mean(charge_no_pot_time, na.rm = T),
    pot_off_time_avg = mean(pot_off_time, na.rm = T),
    pot_off_prop_avg = sum(pot_off_time)/as.numeric(sum(duration)),
    duration_avg = mean(duration, na.rm = T),
    duration_total = sum(duration),
    event_count = n(),
    obs_days = median(deploy_period)
  )

types_from_events = events %>%
  group_by(stove_type) %>%
  summarise(
    duration_avg = mean(duration, na.rm = T),
    duration_sd = sd(duration, na.rm = T),
    charge_time_avg = mean(charge_time, na.rm = T),
    charge_time_sd = sd(charge_time, na.rm = T),
    charge_no_pot_time_avg = mean(charge_no_pot_time, na.rm = T),
    charge_no_pot_time_sd = sd(charge_no_pot_time, na.rm = T),
    pot_off_charging_prop_of_charging = as.numeric(charge_no_pot_time_avg) / as.numeric(charge_time_avg),
    pot_off_time_avg = mean(pot_off_time, na.rm = T),
    pot_off_time_sd = sd(pot_off_time, na.rm = T),
    pot_off_prop_avg = mean(pot_off_prop, na.rm = T),
    pot_off_prop_sd = sd(pot_off_prop, na.rm = T),
    pot_off_charging_prop_avg = mean(charge_no_pot_prop, na.rm = T),
    pot_off_charging_prop_sd = sd(charge_no_pot_prop, na.rm = T),
    use_time_hr = sum(duration) / 60, #hrs
    event_count = n()
  )

types_from_users = users %>%
  group_by(stove_type) %>%
  summarise(
    users = n(),
    obs_days = sum(obs_days),
    use_per_day_avg = mean(min_per_day, na.rm = T),
    use_per_day_sd = sd(min_per_day, na.rm = T),
    charging_per_day_avg = mean(charging_per_day, na.rm = T),
    charging_per_day_sd = sd(charging_per_day, na.rm = T),
    charging_pot_off_per_day_avg = mean(charging_pot_off_per_day, na.rm = T),
    charging_pot_off_per_day_sd = sd(charging_pot_off_per_day, na.rm = T)
  )

types = merge(types_from_events, types_from_users, by = "stove_type")

marginal_pot_off_prop = diff(types$pot_off_prop_avg)/100
marginal_pot_off_use = marginal_pot_off_prop * select(filter(types, stove_type == "USB"), use_per_day_avg)
expected_pot_off_charging = select(filter(types, stove_type == "USB"), charging_per_day_avg) * 
  select(filter(types, stove_type == "no USB"), pot_off_prop_avg)/100

extra_pot_off_charging = c(as.numeric(select(filter(types, stove_type == "USB"), 
                                             charging_pot_off_per_day_avg) - expected_pot_off_charging), 0)
extra_pot_off_not_charging = c(as.numeric(marginal_pot_off_use - extra_pot_off_charging[1]), 0)
cooking_and_charging = c(as.numeric(select(filter(types, stove_type == "USB"), 
                                           charging_per_day_avg) - extra_pot_off_charging[1]), 0)
cooking_alone = c(as.numeric(select(filter(types, stove_type == "USB"), use_per_day_avg)), 
                  as.numeric(select(filter(types, stove_type == "no USB"), use_per_day_avg))) -
  cooking_and_charging - extra_pot_off_not_charging - extra_pot_off_charging
stove_type = c("USB", "no USB")

daily_use = data.frame(extra_pot_off_charging, extra_pot_off_not_charging, cooking_and_charging, cooking_alone, stove_type)
daily_use_long = melt(daily_use, id.vars = "stove_type")
daily_use_long$variable = factor(daily_use_long$variable, labels=c("extra pot off time", 
                                                                   "extra pot off & charging", 
                                                                   "cooking & charging", 
                                                                   "cooking"))

daily_use_test = t.test(as.numeric(filter(users, stove_type == "USB")$min_per_day), 
                        as.numeric(filter(users, stove_type == "no USB")$min_per_day))

pot_off_test = t.test(filter(events, stove_type == "USB")$pot_off_prop , 
                      filter(events, stove_type == "no USB")$pot_off_prop)

duration_test = t.test(as.numeric(filter(events, stove_type == "USB")$duration), 
                       as.numeric(filter(events, stove_type == "no USB")$duration))

### plots and such
bar_data = arrange(daily_use_long, -row_number())
bar_data$value
bar_data = bar_data %>%
  group_by(stove_type) %>%
  mutate(pos = cumsum(value) - (0.5 * value))

fbar = ggplot(bar_data, aes(x = stove_type, y = value, fill = variable, 
                            label = sprintf("%.1f", round(value, digits = 1)))) + 
  geom_bar(stat = "identity") + theme_bw() + geom_text(aes(y = pos), color = "white", size = 3) + 
  theme(legend.key = element_rect(colour = NA)) + scale_fill_brewer(palette = "Spectral") + 
  labs(x = "Stove Type", y = "Duration (min/day)", title = "Use Modes", fill = "Mode")
fbar
ggsave("figures/use-bar.pdf", width = 4, height = 3.5, units = "in")
unlink("use-bar.pdf")

events_long = melt(events)
users_long = melt(users)
densities = filter(rbind(events_long, users_long), 
                   variable == "pot_off_prop" | variable == "duration" & value < 300 | variable == "min_per_day"  )
densities$variable2 = factor(densities$variable, labels = 
                               c("Event Duration (min)", "% of Event w/ Pot Off", "Daily Cooking (min)") )
ggplot(densities) + geom_density(aes(x = value, fill = stove_type), color = NA, alpha = 0.5, adjust = 0.5) + 
  theme_bw() + 
  theme(legend.position="none") +
  theme(legend.key = element_rect(colour = NA)) + scale_fill_discrete(guide = guide_legend(title = "stove type")) + 
  facet_grid(stove_type~variable2, scales = "free_x") +
  labs(x="", y = "Density", title = "Density Plots of Key Usage Parameters")
ggsave("figures/density-grid.pdf", width = 6, height = 3, units = "in")
unlink("density-grid.pdf")

f5 = ggplot(ts_day_data) + geom_smooth(aes(x = cum_cooking2, y = daily_cooking, color = stove_type), alpha = 0.2) + 
  xlim(0,10) + theme_bw() +
  geom_vline(xintercept = c(2.92, 6.01), linetype = "longdash") +
  annotate("text", x = 4.5, y = 2, label = "sample wood \n depleted") +
  labs(x = "Cumulative Use (hr)", y = "Daily Use (hr/day)", title = "Daily Use vs. Cumulative Use", color = "Stove Type")
f5
ggsave("figures/cum-use.pdf", width = 5.5, height = 3, units = "in")
unlink("cum-use.pdf")

f6 = ggplot(ts_day_data) + geom_smooth(aes(x = cum_cooking2, y = daily_cooking, color = stove_type), alpha = 0.2) + 
  xlim(0,10) + theme_bw() +
  geom_vline(xintercept = c(2.92, 6.01), linetype = "longdash") +
  annotate("text", x = 4.5, y = 2, label = "sample wood \n depleted") +
  labs(x = "Cumulative Use (hr)", y = "Daily Use (hr/day)", title = "Daily Use vs. Cumulative Use", color = "Stove Type")
f6
ggsave("figures/cum-use.pdf", width = 5.5, height = 3, units = "in")
unlink("cum-use.pdf")

### surveys
survey_data = read.csv("survey_reported_use.csv")

use = merge(survey_data, users, by = c("stove","village"))
use = rename(use, min_per_day_survey = min_per_day.x, min_per_day_sensor = min_per_day.y)

f7 = ggplot(use, aes(x=min_per_day_sensor, y=min_per_day_survey, color=stove_type))+geom_point(alpha=0.5)+xlim(0,250)+ylim(0,250)+
  geom_abline(slope=1, intercept=0, size=.2)+ 
  theme_bw() + labs(title = "Sensor vs. Survey-Measured Use", x="sensor data (minutes/day)", y="survey data (minutes/day)", color = "Stove Type")
f7
ggsave("figures/survey-vs-sensor.pdf", width = 3.7, height = 2.8, units = "in")
unlink("survey-vs-sensor.pdf")