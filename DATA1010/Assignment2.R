#----------------------------------------------
# DATA 101 - HOMEWORK 2
# 
#----------------------------------------------

# Please save this script as "YOUR LAST NAME_HW2.R" and upload the script to Canvas. 
# You should also upload a word document containing your write up and graphs.
# Please type your code into the sections outlined below. 

#----------------------------------------------
# Question 1

#general setup

library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
?flights
View(flights)

#doing an analysis of flight data without not_cancelled
flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  filter(carrier == "UA")

#creating not_cancelled
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

#doing the same analysis of flight data but with not_cancelled
not_cancelled %>%
  filter(carrier == "UA")

#The code starting on line 30 is a line shorter than the code starting
#on line 21, thanks to not_cancelled.


#----------------------------------------------
# Question 2

#average delay per day and proportion of flights delayed per day
flights %>%
  group_by(month, day) %>%
  mutate(cancelled = is.na(dep_time)) %>%
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    pct_cancelled = 100*mean(cancelled)
  ) %>%
  ggplot(mapping = aes(x = pct_cancelled, y = avg_delay)) +
    geom_point() + 
    geom_smooth()

#finding which days are the two outliers on pct_cancelled 
flights %>%
  group_by(month, day) %>%
  mutate(cancelled = is.na(dep_time)) %>%
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    pct_cancelled = 100*mean(cancelled)
  ) %>%
  arrange(-pct_cancelled)

#looks like Feb 8th and Feb 9th, so fair enough, could be big snow days in NYC

#now lets remove those days and redo the plot
flights %>%
  group_by(month, day) %>%
  mutate(cancelled = is.na(dep_time)) %>%
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    pct_cancelled = 100*mean(cancelled)
  ) %>%
  filter(!(month == 2 & day %in% c(8,9))) %>%
  ggplot(mapping = aes(x = pct_cancelled, y = avg_delay)) +
  geom_point() + 
  geom_smooth()


#----------------------------------------------
# Question 3

#creating not_cancelled because cancelled flights fall outside of the universe
#of "delayed" flights - delayed flights, by definition, actually happened
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

#checking out how the dep_time variable handles hours
glimpse(flights)
summary(flights$dep_time)

#within not_cancelled, creating departure hour and checking mean delay time by hour
not_cancelled %>% 
  mutate(dep_hour = dep_time %/% 100) %>%
  group_by(dep_hour) %>%
  summarize(
    delay = mean(dep_delay, na.rm = TRUE)
  ) %>%
  print(n = 30)

#this is odd, the dep_hour variable shouldn't have values for both 0 and 24
#because they represent the same time of day (midnight)
#let's see how many 2400 dep_times there are
not_cancelled %>% 
  filter(dep_time == 2400) %>%
  print(n = 30)
#OK, 29 flights in the data took off at "2400"

#let's see how many dep_times there are below "100", i.e. between midnight and 1am
not_cancelled %>%
  filter(dep_time < 100)
#OK, so ~900 flights afte midnight before 1am

#let's classify the "2400" flights so that they're in the same category as the midnight flights
not_cancelled %>% 
  mutate(dep_hour = ifelse(dep_time == 2400, 0, dep_time %/% 100)) %>%
  group_by(dep_hour) %>%
  summarize(
    avg_delay = mean(dep_delay),
    count = n()
  ) %>%
  arrange(avg_delay) %>%
  print(n = 30)

#do all of this again but get rid of the hours that have less than 1 flight per day on average
not_cancelled %>% 
  mutate(dep_hour = ifelse(dep_time == 2400, 0, dep_time %/% 100)) %>%
  group_by(dep_hour) %>%
  summarize(
    avg_delay = mean(dep_delay),
    count = n()
  ) %>%
  filter(count>365) %>%
  arrange(avg_delay) %>%
  print(n = 30)

#now computing the percentage of flights that leave on time or early in each hour
not_cancelled %>% 
  mutate(
    dep_hour = ifelse(dep_time == 2400, 0, dep_time %/% 100),
    on_time_or_early = ifelse(dep_delay<=0, TRUE, FALSE)
  ) %>%
  group_by(dep_hour) %>%
  summarize(
    avg_delay = mean(dep_delay),
    on_time_early = mean(on_time_or_early),
    count = n()
  ) %>%
  arrange(-on_time_early) %>%
  print(n = 30)
  
  
#----------------------------------------------
# Question 4

not_cancelled %>%
  mutate(
    delay_30_plus = ifelse(dep_delay >= 30, TRUE, FALSE)
  ) %>%
  group_by(carrier) %>%
  summarize(
    pct_delay_30_plus = mean(delay_30_plus),
    count = n()
  ) %>%
  arrange(-pct_delay_30_plus) %>%
  print(n = 30)





#----------------------------------------------
# Question 5


not_cancelled %>%
  group_by(dest) %>%
  summarize(
    arr_delay = mean(arr_delay),
    count = n()
  ) %>%
  arrange(arr_delay)

#filter out destinations with less than 1 flight per day
not_cancelled %>%
  group_by(dest) %>%
  summarize(
    arr_delay = mean(arr_delay),
    count = n()
  ) %>%
  filter(count>365) %>%
  arrange(arr_delay)






