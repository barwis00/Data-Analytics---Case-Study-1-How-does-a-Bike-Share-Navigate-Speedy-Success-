#PREPARE
# df <- list.files(path='D:/Bootcamp, Webinar, etc/Digitalent Kominfo FGA - Google - Data Analytics [05 Sept - 07 Nov 22]/Capstone/Dataset Capstone/Case Study 1 - How does a bike-share navigate speedy success/2021 Cyclistic Dataset') %>% 
#   lapply(read_csv) %>% 
#   bind_rows
# 
# View(df)
# 
# fwrite(df, "cyclistic_2021_all.csv")

#PROCESS
bikeshare_df <- read_csv("cyclistic_2021_all.csv")

View(bikeshare_df)

summary(bikeshare_df)

colnames(bikeshare_df)

# bisa dibuat pie chart untuk proporsi tipe member
table(bikeshare_df['member_casual'])

#create day of the week column
bikeshare_df$day_of_week <- wday(bikeshare_df$started_at)

View(bikeshare_df)

#create ride_length column
bikeshare_df$ride_length_mins <- round(difftime(bikeshare_df$ended_at, bikeshare_df$started_at, units = "mins"), 2)

#create date, month column
bikeshare_df$date <- as.Date(bikeshare_df$started_at)
bikeshare_df$month <- format(as.Date(bikeshare_df$date), "%b")

View(bikeshare_df)

#change format to numeric ride length
bikeshare_df$ride_length_mins<- as.numeric(bikeshare_df$ride_length_mins)

bikeshare_df <- mutate(bikeshare_df, distance_km = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))*0.001)

bikeshare_df$distance_km<- as.numeric(bikeshare_df$distance_km)

sapply(bikeshare_df, function(x) sum(is.na(x)))

#ANALYZE
#remove ride_length negative values
bikeshare_df_2 <- bikeshare_df[!(bikeshare_df$ride_length_mins<0),]
colnames(bikeshare_df)
# bikeshare_df_2$ride_length <- NULL

any(bikeshare_df_2$ride_length_mins < 0) # checking for negative values
View(bikeshare_df_2)

#export clean data 
# fwrite(bikeshare_df_2, "cyclistic_2021_all_clean.csv")

# # selection of desired columns to keep for export
# myvars <- c("ride_id", "rideable_type", "member_casual", "date", "month",
#             "day_of_week", "ride_length_mins", "start_station_name",
#             "end_station_name", "start_lat", "start_lng", "end_lat", "end_lng", "distance_km")
# 
# # store selected columns in a data frame
# bikeshare_subset <- bikeshare_df_2[myvars]
# 
# View(bikeshare_subset)
# # write subset data frame to CSV file
# fwrite(bikeshare_subset, "cyclistic_2021_all_clean_subset.csv")

#analyze summary ride length in minutes
summary(bikeshare_df_2$ride_length_mins)

summary(bikeshare_df_2$distance_km)

#compare ride_length casual vs member (avg, median, min, max)
bikeshare_df_2 %>% 
  group_by(member_casual) %>% 
  summarise(min_ride_length = min(ride_length_mins), max_ride_length = max(ride_length_mins), 
          mean_ride_length = mean(ride_length_mins), med_ride_length = median(ride_length_mins))

#compare distance_km casual vs member (avg, median, min, max)
bikeshare_df_2 %>% 
  group_by(member_casual) %>% 
  summarise(min_distance = min(distance_km, na.rm = TRUE), max_distance = max(distance_km, na.rm = TRUE), 
            mean_distance = mean(distance_km, na.rm = TRUE), med_distance = median(distance_km, na.rm = TRUE))

# convert day of week from numeric to char
bikeshare_df_2$day_of_week <- format(as.Date(bikeshare_df_2$date), "%a")

#order day_of_week by weekday
bikeshare_df_2$day_of_week <- ordered(bikeshare_df_2$day_of_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri",
                                                                           "Sat", "Sun"))
#order month 
bikeshare_df_2$month <- ordered(bikeshare_df_2$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                               "Aug", "Sep", "Oct", "Nov", "Dec"))

#Compare ride_length group by day_of_week and  member_casual
bikeshare_df_2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(day_avg_ride_length = mean(ride_length_mins))

#compare member_casual to find mode number_of_rides by weekday
bikeshare_df_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_mins)) %>% # calculates the average duration
  arrange(desc(number_of_rides))

#VISUALIZE PLOTTING
#create plot number_of_rides by usertype and weekday
bikeshare_df_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_mins)) %>% # calculates the average duration
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  labs(title = "Number of Riders by Weekday and User Type 2021", x = "Weekday", y = "Number of Rides") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  geom_col(position = "dodge")


#create plot avg duration by usertype and weekday
bikeshare_df_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_mins)) %>% # calculates the average duration
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  labs(title = "Average Duration by Weekday and User Type 2021", x = "Weekday", y = "Average Duration (in minutes)") +
  geom_col(position = "dodge")

#create plot number_of_rides by usertype and month
bikeshare_df_2 %>% 
  group_by(member_casual, month) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_mins)) %>% # calculates the average duration
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title = "Number of Riders by Month and User Type 2021", x = "Month", y = "Number of Rides") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  geom_col(position = "dodge")

#create plot avg duration by usertype and month
bikeshare_df_2 %>% 
  group_by(member_casual, month) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_mins)) %>% # calculates the average duration
  arrange(member_casual, month) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  labs(title = "Average Duration by Month and User Type 2021", x = "Month", y = "Average Duration (in minutes)") +
  geom_col(position = "dodge")

sapply(bikeshare_df_2, function(x) sum(is.na(x)))
distinct(bikeshare_df, month)

#continue code here...



#export clean data for tableau
# fwrite(bikeshare_df_2, "cyclistic_2021_all_clean.csv")

# # selection of desired columns to keep for export
# myvars <- c("ride_id", "rideable_type", "member_casual", "date", "month",
#             "day_of_week", "ride_length_mins", "start_station_name",
#             "end_station_name", "start_lat", "start_lng", "end_lat", "end_lng", "distance_km")
#
# # store selected columns in a data frame
# bikeshare_subset <- bikeshare_df_2[myvars]
#
# View(bikeshare_subset)
# # write subset data frame to CSV file
# fwrite(bikeshare_subset, "cyclistic_2021_all_clean_subset.csv")












