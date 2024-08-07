# load libraries 
library(tidyverse) 
library(lubridate) 
library(hms) 
library(data.table)

# load original csv files
jan_df <- read_csv("202301-divvy-tripdata.csv") 
feb_df <- read_csv("202302-divvy-tripdata.csv")
mar_df <- read_csv("202303-divvy-tripdata.csv")
apr_df <- read_csv("202304-divvy-tripdata.csv")
may_df <- read_csv("202305-divvy-tripdata.csv") 
jun_df <- read_csv("202306-divvy-tripdata.csv")
jul_df <- read_csv("202307-divvy-tripdata.csv") 
aug_df <- read_csv("202308-divvy-tripdata.csv") 
sep_df <- read_csv("202309-divvy-tripdata.csv")
oct_df <- read_csv("202310-divvy-tripdata.csv")
nov_df <- read_csv("202311-divvy-tripdata.csv") 
dec_df <- read_csv("202312-divvy-tripdata.csv")

# merge all the data
cyclistic_df <- rbind(jan_df, feb_df, mar_df, apr_df, may_df, jun_df, jul_df,
                      aug_df, sep_df,oct_df, nov_df, dec_df)

head(cyclistic_df)

# remove individual month data frames
remove(jan_df, feb_df, mar_df, apr_df, may_df, jun_df, jul_df,
        aug_df, sep_df,oct_df, nov_df, dec_df)

# remove null values
cyclistic_df <- na.omit(cyclistic_df)

# remove duplicates
cyclistic_df <- distinct(cyclistic_df)

# calculate ride length and convert it to minutes
cyclistic_df$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at,
                                       units = "mins")

# remove where ride length <=0
cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length <=0),]

# date
cyclistic_df$date <- as.Date(cyclistic_df$started_at)

# day of the week
cyclistic_df$day_of_week <- wday(cyclistic_df$started_at)

# column for day of the week
cyclistic_df$day_of_week <- format(as.Date(cyclistic_df$date), "%A")

# column for day
cyclistic_df$day <- format(as.Date(cyclistic_df$date), "%d")

# column for month
cyclistic_df$month <- format(as.Date(cyclistic_df$date), "%m")

# column for year
cyclistic_df$year <- format(as.Date(cyclistic_df$date), "%Y")

# format time as HH:MM:SS
cyclistic_df$time <- format(as.Date(cyclistic_df$date), "%H:%M:%S") 

# column for time
cyclistic_df$time <- as_hms((cyclistic_df$started_at))

# column for hour
cyclistic_df$hour <- hour(cyclistic_df$time)

# remove columns not needed
cyclistic_df <- cyclistic_df %>%  
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng))

# column for different time_of_day
cyclistic_df <-cyclistic_df %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)

# column for different seasons
cyclistic_df <-cyclistic_df %>% mutate(season = 
                                             case_when(month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter",
                                                       month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06" ~ "Summer",
                                                       month == "07" ~ "Summer",
                                                       month == "08" ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter")
)

# average ride_length
cyclistic_avgRide <- mean(cyclistic_df$ride_length)
cyclistic_avgRide

# average ride_length by member type and bike type
cyclistic_df %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# MEMBER============================================

# type
member_df <- cyclistic_df %>%
    group_by(member_casual) %>% 
    count(member_casual)

# average ride_length
cyclistic_df %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))


# BIKE==================================================

# type
cyclistic_df %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

# total rides 
rideable_type_df <- cyclistic_df %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

# average ride_length
cyclistic_df %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# HOUR======================================================

# total rides by member type
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(hour)

# total rides
hour_df <- cyclistic_df %>%
  count(hour)

# average ride_length by member type
cyclistic_df %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) 

# average ride_length
cyclistic_df %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#TIME OF DAY====================================

# rides by member type 
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(time_of_day)

# total rides
time_of_day_df <- cyclistic_df %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

# average ride length by member type
cyclistic_df %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# average ride length
cyclistic_df %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# DAY OF THE WEEK========================================

# rides by member type
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(day_of_week)

# total rides 
day_of_week_df <- cyclistic_df %>%
  count(day_of_week)

# average ride_length by member type
cyclistic_df %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# average ride_length 
cyclistic_df %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))  

# DAY OF THE MONTH=====================================

# rides by member type
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(day)

# total rides
day_df <- cyclistic_df %>%
  count(day)

# average ride_length by member type
cyclistic_df %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) 

# average ride_length
cyclistic_df %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# MONTH=============================================

# rides by member type 
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(month)

# total rides
month_df <- cyclistic_df %>%
  count(month)

# average ride_length by member type
cyclistic_df %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) 

# average ride_length
cyclistic_df %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# SEASON======================================

# rides by member type
cyclistic_df %>%
  group_by(season, member_casual) %>% 
  count(season)

# total rides
season_df <- cyclistic_df %>%
  group_by(season) %>% 
  count(season)

# average ride length by member type
cyclistic_df %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

# average ride length 
cyclistic_df %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))















