#install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

#load packages
library(tidyverse)
library(lubridate)
library(ggplot2)

#import data
Sep2021 <- read_csv("googleCaseStudy1/TripData/2021_09.csv")
Oct2021 <- read_csv("googleCaseStudy1/TripData/2021_10.csv")
Nov2021 <- read_csv("googleCaseStudy1/TripData/2021_11.csv")
Dec2021 <- read_csv("googleCaseStudy1/TripData/2021_12.csv")
Jan2022 <- read_csv("googleCaseStudy1/TripData/2022_01.csv")
Feb2022 <- read_csv("googleCaseStudy1/TripData/2022_02.csv")
Mar2022 <- read_csv("googleCaseStudy1/TripData/2022_03.csv")
Apr2022 <- read_csv("googleCaseStudy1/TripData/2022_04.csv")
May2022 <- read_csv("googleCaseStudy1/TripData/2022_05.csv")
Jun2022 <- read_csv("googleCaseStudy1/TripData/2022_06.csv")
Jul2022 <- read_csv("googleCaseStudy1/TripData/2022_07.csv")
Aug2022 <- read_csv("googleCaseStudy1/TripData/2022_08.csv")

#compare column names for consistency
colnames(Sep2021)
colnames(Oct2021)
colnames(Nov2021)
colnames(Dec2021)
colnames(Jan2022)
colnames(Feb2022)
colnames(Mar2022)
colnames(Apr2022)
colnames(May2022)
colnames(Jun2022)
colnames(Jul2022)
colnames(Aug2022)

#compare data types for consistency
str(Sep2021)
str(Oct2021)
str(Nov2021)
str(Dec2021)
str(Jan2022)
str(Feb2022)
str(Mar2022)
str(Apr2022)
str(May2022)
str(Jun2022)
str(Jul2022)
str(Aug2022)

#stack data into one bid data frame
all_trips <- bind_rows(Sep2021, Oct2021, Nov2021, Dec2021,
                       Jan2022, Feb2022, Mar2022, Apr2022,
                       May2022, Jun2022, Jul2022, Aug2022)

#remove lat and long from new data frame
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#inspect new data frame
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# Check to make sure the proper number of observations were assigned
table(all_trips$member_casual)

#add columns for date, month, day, year, and day of week
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" to numeric so we can run calculations
is.numeric(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

#create new data frame with removed ride length values of 0
all_trips_v2 <- all_trips[!(all_trips$ride_length<=0),]

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# fix order or days of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", 
                                             "Wednesday", "Thursday", "Friday", 
                                             "Saturday"))

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

#visualize the number of rides by rider type
options(scipen = 999)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#visualize the average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#create csv file
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/googleCaseStudy1/avg_ride_length.csv')

