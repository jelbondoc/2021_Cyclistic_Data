# 2021_Cyclistic_Data
An analysis on 2021 data of Cyclistic, a Chicago-based bike-sharing app.

## Run the packages needed for the data analysis
library(tidyverse)
library(janitor)
library(lubridate)
library(janitor)
library(readxl)

## Import files
d1 <- read_excel("C:/Users/jelb/DivvyData/Jan2021.xlsx")
d2 <- read_excel("C:/Users/jelb/DivvyData/Feb2021.xlsx")
d3 <- read_excel("C:/Users/jelb/DivvyData/Mar2021.xlsx")
d4 <- read_excel("C:/Users/jelb/DivvyData/Apr2021.xlsx")
d5 <- read_excel("C:/Users/jelb/DivvyData/May2021.xlsx")
d6 <- read_excel("C:/Users/jelb/DivvyData/Jun2021.xlsx")
d7 <- read_excel("C:/Users/jelb/DivvyData/Jul2021.xlsx")
d8 <- read_excel("C:/Users/jelb/DivvyData/Aug2021.xlsx")
d9 <- read_excel("C:/Users/jelb/DivvyData/Sep2021.xlsx")
d10 <- read_excel("C:/Users/jelb/DivvyData/Oct2021.xlsx")
d11 <- read_excel("C:/Users/jelb/DivvyData/Nov2021.xlsx")
d12 <- read_excel("C:/Users/jelb/DivvyData/Dec2021.xlsx")

## Merge 12 files into 1 data frame and clean empty cell
y2021 <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)
y2021 <- janitor::remove_empty(y2021, which = c("cols"))
y2021 <- janitor::remove_empty(y2021, which = c("rows"))

## Add ride_length, month, days_of_week columns to the y2021 data frame
y2021 <- mutate(y2021, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
y2021 <- mutate(y2021,"ride_length" = ended_at - started_at, ride_length = seconds_to_period(ride_length))
y2021 <- mutate(y2021, "month" = month(started_at))
y2021 <- mutate(y2021, "days_of_week" = weekdays(started_at))

## Change the ride_length column format from minutes and seconds to seconds
y2021$ride_length <- difftime(y2021$ended_at,y2021$started_at)

## Remove bad data from ride_length (bad data = ride_length <0)
y2021 <- y2021[!y2021$ride_length<0,]

## Create data frames needed for descriptive analysis
average_table <- data.frame(aggregate(y2021$ride_length ~ y2021$member_casual, FUN = mean))
median_table <- data.frame(aggregate(y2021$ride_length ~ y2021$member_casual, FUN = median))
max_table <- data.frame(aggregate(y2021$ride_length ~ y2021$member_casual, FUN = max))
min_table <- data.frame(aggregate(y2021$ride_length ~ y2021$member_casual, FUN = min))
y2021$days_of_week <- ordered(y2021$days_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
avg_by_day <- data.frame(aggregate(y2021$ride_length ~ y2021$member_casual + y2021$days_of_week, FUN = mean))

## Create summarise data frame for months, days_of_week, and start_station to end_station of both casual and member rides
Months2021 <- y2021 %>% group_by(member_casual,month) %>% summarise(number_of_ride = n(),average_duration = mean(ride_length))
Days2021 <- y2021 %>% group_by(member_casual,days_of_week) %>% summarise(number_of_ride = n(),average_duration = mean(ride_length))
Locations2021 <- y2021 %>% group_by(member_casual,start_station_name,end_station_name) %>% summarise(number_of_ride = n(),average_duration = mean(ride_length))

## Save the data frames as CSV and edit in Excel for further analysis
write.csv(average_table,"C://Users/jelb/DivvyDataClean/average_table.csv", row.names = FALSE)
write.csv(median_table,"C://Users/jelb/DivvyDataClean/median_table.csv", row.names = FALSE)
write.csv(max_table,"C://Users/jelb/DivvyDataClean/max_table.csv", row.names = FALSE)
write.csv(min_table,"C://Users/jelb/DivvyDataClean/min_table.csv", row.names = FALSE)
write.csv(avg_by_day,"C://Users/jelb/DivvyDataClean/avg_by_day.csv", row.names = FALSE)
write.csv(Months2021,"C://Users/jelb/DivvyDataClean/Months2021.csv", row.names = FALSE)
write.csv(Days2021,"C://Users/jelb/DivvyDataClean/Days2021.csv", row.names = FALSE)
write.csv(Locations2021,"C://Users/jelb/DivvyDataClean/Locations2021.csv", row.names = FALSE)

## Proceed to MS Excel for further analysis and data visualization
