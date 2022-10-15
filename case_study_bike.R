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
bikeshare_df$month <- format(as.Date(bikeshare_df$date), "%m")

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

# selection of desired columns to keep for export
myvars <- c("ride_id", "rideable_type", "member_casual", "date", "month", 
            "day_of_week", "ride_length_mins", "start_station_name", 
            "end_station_name", "start_lat", "start_lng", "end_lat", "end_lng", "distance_km")

# store selected columns in a data frame
bikeshare_subset <- bikeshare_df_2[myvars]

View(bikeshare_subset)
# write subset data frame to CSV file
fwrite(bikeshare_subset, "cyclistic_2021_all_clean_subset.csv")

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












