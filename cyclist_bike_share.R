
### --------- Installing and loading necessary packages   ----------------------
#-------------------------------------------------------------------------------

install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages(tidyverse)
install.packages(ggplot2)
install.packages(lubridate)
install.packages(dplyr)
install.packages(readr)
install.packages(janitor)
install.packages(data.table)
install.packages(tidyr)
install.packages("VIM")              # Install VIM package forchecking missing values


library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)
library(scales)
library(purrr)
library(DataExplorer)
library("VIM")                       # Load VIM


##------------   Load 12 months of bike_sharing data in 2021     ---------------
#-------------------------------------------------------------------------------

Jan <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202101-divvy-tripdata.csv")
Feb <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202102-divvy-tripdata.csv")
Mar <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202103-divvy-tripdata.csv")
Apr <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202104-divvy-tripdata.csv")
May <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202105-divvy-tripdata.csv")
Jun <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202106-divvy-tripdata.csv")
Jul <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202107-divvy-tripdata.csv")
Aug <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202108-divvy-tripdata.csv")
Sep <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202109-divvy-tripdata.csv")
Oct <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202110-divvy-tripdata.csv")
Nov <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202111-divvy-tripdata.csv")
Dec <- read.csv("C:\\Users\\NEONARD\\Documents\\R scripts\\Capstone project\\data\\202112-divvy-tripdata.csv")

## --------------  Merging data into a data frame -----------------------------
triprawdata <- rbind(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
totalcells <-  prod(dim(triprawdata))

# create_report(triprawdata)

#---------------------------------------------------------------------------
#-------------------       Process  ----------------------------------------
#---------------------------------------------------------------------------
# Viewing 
nrow(triprawdata) #number of rows
ncol(triprawdata) #number of columns
dim(triprawdata)   # number rows and columns
head(triprawdata) #see the first 6 rows of the data frame
tail(triprawdata) #see the last 6 rows of the data frame
str(triprawdata)  #see list of columns and data types
glimpse(triprawdata)
summary(triprawdata) #statistical summary of data
colnames(triprawdata) #list of column names

#-------------------------------------------------------------------------------
#-----------        Data transformation and cleaning      ----------------------
#-------------------------------------------------------------------------------

#---------   Checking and Removing NA's ----------------------

# calculating the number of cells with na
# which(is.na(triprawdata$start_station_name))
aggr(triprawdata)                       # Create aggregation plot for missing values

colSums(is.na(triprawdata))             # Count missing values by column
colSums(is.na(triprawdata)) / nrow(triprawdata)    # Percentage of missing values by column
missingcells <- sum(is.na(triprawdata))
print("Missing value cells")
print(missingcells)

# calculating percentage of missing values
percentage <- (missingcells * 100 )/(totalcells)
print("Percentage of missing values' cells")
print (percentage)

triprawdata<- na.omit(triprawdata)
triprawdata <- janitor::remove_empty(triprawdata, which = c("cols"))
triprawdata <- janitor::remove_empty(triprawdata, which = c("rows"))

# Remove columns not required or beyond the scope of project
triprawdata <- triprawdata %>%
  select(-c(start_lat:end_lng))

# Making data consistent
# The date format is inconsistent, some data are in the YYYY-MM-DD format or MM/DD/YYYY. Check the attached code for details. All dates are converted into YYYY-MM-DD format.
#Hold on! started_at & ended_at should be in datetime datatype instead of char. Convert all from char to datetime.
triprawdata[['started_at']] <- ymd_hms(triprawdata[['started_at']])
triprawdata[['ended_at']] <- ymd_hms(triprawdata[['ended_at']])


# ----------------------------------------------------------------------------
#      Create columns: date, month, day,year, weekday, start_hour, end_hour
# Adding a column for Ride length
#-----------------------------------------------------------------------------
triprawdata$ride_length <- difftime(triprawdata$ended_at, 
                                    triprawdata$started_at, units = "mins")
# Rename columns for better readability
triprawdata <- triprawdata %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)

#column for day of the week the trip started
triprawdata$day_of_the_week <- format(as.Date(triprawdata$start_time),'%a')

# start of rides
triprawdata$start_date <- as.Date(triprawdata$start_time)

#column for month when the trip started
#triprawdata$month <- format(as.Date(triprawdata$start_time),'%b_%y')
triprawdata$month <- format(as.Date(triprawdata$start_time),'%b')

#column for time of the day when the trip started only the hours-minutes-seconds are.
triprawdata$start_hour = format(as.POSIXct(triprawdata$start_time), "%H:%M")
triprawdata$end_hour = format(as.POSIXct(triprawdata$end_time), "%H:%M")

'triprawdata$start_hour <- as.numeric(format(as.POSIXct(triprawdata$start_time), "%H%M"))
triprawdata$start_hour <- format(as.POSIXct(sprintf("%04d", triprawdata$start_hour), format="%H%M"), "%H:%M")'

str(triprawdata)

triprawdata$season <- ifelse (triprawdata$month %in% c('Jun','Jul','Aug'), "Summer",
                            ifelse (triprawdata$month %in% c('Sep','Oct','Nov'), "Fall",
                                    ifelse (triprawdata$month %in% c('Dec','Jan','Feb'), "Winter",
                                            ifelse (triprawdata$month %in% c('Mar','Apr','May'), "Spring", NA))))


# checking for trip lengths less than 0
nrow(subset(triprawdata,ride_length < 0))  # 146 ride_length < 0

#checking for testrides that were made by company for quality checks
nrow(subset(triprawdata, start_station_name %like% "TEST"))
nrow(subset(triprawdata, start_station_name %like% "test"))
nrow(subset(triprawdata, start_station_name %like% "Test")) # zero test station

# remove negative trip durations 
triprawdata <- triprawdata[!(triprawdata$ride_length < 0),]

# It is important to make sure that customer_type column has only two distinct values. Let's confirm the same.
# checking count of distinct values
table(triprawdata$customer_type)

# ?setNames or help(setNames)

#aggregating total trip duration by customer type
setNames(aggregate(ride_length ~ customer_type, triprawdata, sum), c("customer_type", "total_trip_duration(mins)"))

# Summary statistics for ride_length
summary(triprawdata$ride_length)

#statistical summary of trip_duration by customer_type
triprawdata %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(ride_length),max_trip_duration = max(ride_length),
            median_trip_duration = median(ride_length), mean_trip_duration = mean(ride_length))



# Total number of trips by customer type and day of the week
'fix the order for the day_of_the_week and month variable so that they show up 
in the same sequence in output tables and visualizations'
triprawdata$day_of_the_week <- ordered(triprawdata$day_of_the_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
triprawdata$month <- ordered(triprawdata$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec"))

# daily average rides of customers
triprawdata %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(ride_length)) %>% 
  arrange(customer_type, desc(number_of_rides))

#-------------------------------------------------------------------------------
#                             Visualization:
#-------------------------------------------------------------------------------


#================== Ride Analysis of customer types ===========================

# count of customers
triprawdata%>% group_by(customer_type)%>% summarise(n=n())%>%
  mutate(percent = n*100/sum(n))
ggplot(data = triprawdata,mapping= aes(x= customer_type, fill =customer_type))+ 
  scale_fill_manual(values = c("Purple", "pink")) +geom_bar() +	ylab('Ride Count')+
  labs(title="Member Vs Casual") + labs(subtitle = "January 2021 - December 2021")

#Average Ride behavior of members and casual riders
bar<-triprawdata%>% 
  group_by(customer_type)%>% 
  summarise(avg_ride_length=mean(ride_length))
ggplot(bar, aes(x = customer_type, y = avg_ride_length, fill =customer_type))+ylab('avg_Ride_Count')+
  geom_col()+ scale_fill_manual(values = c("Purple", "pink"))+
  labs(title="Member Vs Casual") + labs(subtitle = "January 2021 - December 2021")



#======================    Comparing Bikes =================================
# Ride Type Trends
# Count of ride type by user type
triprawdata %>% count(ride_type, customer_type)

# average rides_type
triprawdata%>% 
  group_by(ride_type) %>% 
  summarise(n=n())%>% 
  mutate(percent = n*100/sum(n))
ggplot(data = triprawdata,mapping= aes(x= ride_type,fill=ride_type)) + 
  geom_bar() + labs(title="Bike Types used") + 
  scale_fill_manual(values = c("#3f93a2", "#99cad5" , "#72CADC"))


#Choice of Bikes by Riders
member_type<-triprawdata%>% 
  group_by(customer_type,ride_type) %>% 
  summarise(n=n())%>%  
  mutate(percent = n*100/sum(n))
ggplot(data = as.data.frame(member_type),mapping= aes(x= customer_type, y=n, fill =ride_type)) +
  geom_bar(stat = 'identity') + labs(title="Choice of Bike by Riders")


# Count of ride type by user type
ggplot(triprawdata, aes(x = ride_type, fill = customer_type)) + 
  geom_bar(position = "dodge") +
  ggtitle('Ride Type by User Type', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Ride Type') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))


# average trip by customer type vs day of the week
triprawdata %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week", subtitle="Membership comparision") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"))

'week_days <- triprawdata %>% count(day_of_the_week, customer_type, ride_type,ride_length) 
print(week_days)'

# Average number of trips by customer type and month
unique(triprawdata$month)

triprawdata %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>% 
  arrange(customer_type,desc(number_of_rides))

# monthly ride by customers
triprawdata %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("#3f93a2", "#99cad5"))




# Visualizaton of ride type Vs. number of trips by customer type

triprawdata %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips") + 
  scale_fill_manual(values = c("#99cad5", "#3f93a2"))




# ==================Visualizaton of weekly rides by customers==================
#==============================================================================
# average trip duration by customer type on each day of the week
triprawdata %>%
  group_by(customer_type, day_of_the_week) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(customer_type, day_of_the_week) %>%
  ggplot(aes(x = day_of_the_week, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle('Average Ride Duration by User Type and Weekday', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('Weekday') + ylab('Ride Duration (sec)') + 
  labs(fill='User Type') +
  labs(caption = "NOTE: 1000 sec = 16.6667 min") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Ridership count\sum by weekday and user type
ggplot(triprawdata, aes(x = day_of_the_week, fill = customer_type)) +
  geom_bar(position = "dodge") +
  ggtitle('Daily Rides by User Type', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Weekday') + ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))


# ============================= Monthly Trend =====================================
----------------------------------------------------------------------------------
  
# Monthly trends by user type
monthly_usercount <- triprawdata %>% count(month, customer_type)
monthly_usercount 

ggplot(triprawdata, aes(x = month, fill = customer_type)) +
  geom_bar(position = "dodge") +
  ggtitle('Monthly Trends by User Type', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('Month') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  labs(caption = "NOTE: Months represented in MM format") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# average trip duration by customer type Vs. month

# Average ride duration by user type and month
avg_duration <- triprawdata %>%
  group_by(customer_type, month) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(customer_type, month) 

print(avg_duration, n=24)

ggplot(avg_duration, aes(x = month, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge") + ggtitle('Average Ride Duration by User Type and Month', subtitle = "January 2021 - December 2021") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + xlab('Month') + ylab('Ride Duration (sec)') + labs(fill='User Type') + labs(caption = "NOTE: 1000 sec = 16.6667 min | Months represented in MM format") + scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))


#==========================  Hourly Trends =====================================
#-------------------------------------------------------------------------------
# Popular start hours by user type
str(triprawdata)
triprawdata$start_hour <- format(triprawdata$start_hour, format = "%H:%M")
triprawdata$start_hour <- as.POSIXct(triprawdata$start_hour, format = "%H:%M")

triprawdata$end_hour <- format(triprawdata$start_hour, format = "%H:%M")
triprawdata$end_hour <- as.POSIXct(triprawdata$start_hour, format = "%H:%M")

pop_start_hour <- triprawdata %>% count(start_hour, customer_type, sort = TRUE)
casual_start_hour <- filter(pop_start_hour, customer_type == 'casual')
casual_start_hour <- casual_start_hour %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(casual_start_hour)

member_start_hour <- filter(pop_start_hour, customer_type == 'member')
member_start_hour <- member_start_hour %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(member_start_hour)

# Start hour trends by user type
ggplot(triprawdata, aes(x = start_hour, fill = customer_type)) +
  geom_bar(position = "dodge") + 
  ggtitle('Start Hour Trends by User Type', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('Start Hour (Military Time)') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  labs(caption = 'NOTE: 0000 / 2400 = 12 a.m.') +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Popular start hours - Casuals
ggplot(casual_start_hour, aes(x = start_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 Start Hours - Casuals', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Start Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma) 

# Popular start hours - Members
ggplot(data = member_start_hour, aes(x = start_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 Start Hours - Members', subtitle = "April 2020 - March 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Start Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma) 


# Popular end hours by user type
pop_end_hour <- triprawdata %>% count(end_hour, customer_type, sort = TRUE) 
member_end_hour <- filter(pop_end_hour, customer_type == 'member', sort = TRUE) 
member_end_hour <- member_end_hour %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(member_end_hour)

casual_end_hour <- filter(pop_end_hour, customer_type == 'casual', sort = TRUE) 
casual_end_hour <- casual_end_hour %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(casual_end_hour)

# End hour trends by user type
ggplot(triprawdata, aes(x = end_hour, fill = customer_type)) +
  geom_bar(position = "dodge") + 
  ggtitle('End Hour Trends by User Type', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('End Hour (Military Time)') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  labs(caption = 'NOTE: 0000 / 2400 = 12 a.m.') +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Popular end hours - Casuals
ggplot(casual_end_hour, aes(x = end_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 End Hours - Casuals', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('End Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma) 

# Popular end hours - Members
ggplot(data = member_end_hour, aes(x = end_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 End Hours - Members', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('End Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma) 


# Visualizaton of bike demand over 24 hr period (a day)
triprawdata %>%  
  group_by(customer_type, start_hour) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = start_hour, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")


#--------------------------- Seasonal Trends -----------------------------------
# Seasonal trends by user type
triprawdata %>% count(season, customer_type)
ggplot(triprawdata, aes(x = season, fill = customer_type)) +
  geom_bar(position = "dodge") +
  ggtitle('Seasonal Trends by User Type', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Season') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Average ride duration by user type and season
seasonal_avg_duration <- triprawdata %>%
  group_by(customer_type, season) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(customer_type, season) 

print(seasonal_avg_duration)

ggplot(seasonal_avg_duration, aes(x = season, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge") +
  ggtitle('Average Ride Duration by User Type and Season', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + 
  xlab('Season') + ylab('Ride Duration (sec)') + 
  labs(fill='User Type') +
  labs(caption = "NOTE: 1000 sec = 16.6667 min") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))



#------------------------------ Station Trends --------------------------------------
# Popular start stations by user type
popular_stations <- triprawdata %>% count(start_station_name, customer_type)
print(popular_stations)

# Popular end stations by user type
end_stations <- triprawdata %>% count(end_station_name, customer_type)
print(end_stations)

# Top 10 start stations - Casuals
pop_stations_casual<- filter(popular_stations, customer_type == 'casual')
pop_stations_casual <- pop_stations_casual %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(pop_stations_casual)

ggplot(data = pop_stations_casual, aes(x = start_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 Start Stations - Casuals', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )

# Top 10 start stations - Members
pop_stations_member<- filter(popular_stations, customer_type == 'member')
pop_stations_member <- pop_stations_member %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(pop_stations_member)

ggplot(data = pop_stations_member, aes(x = start_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 Start Stations - Members', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )

# Top 10 end stations - Casuals 
end_stations_casual<- filter(end_stations, customer_type == 'casual') 
end_stations_casual <- end_stations_casual %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(end_stations_casual)

ggplot(data = end_stations_casual, aes(x = end_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 End Stations - Casuals', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )

# Top 10 end stations - Members 
end_stations_member <- filter(end_stations, customer_type == 'member')
end_stations_member <- end_stations_member %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)
print(end_stations_member)

ggplot(data = end_stations_member, aes(x = end_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 End Stations - Members', subtitle = "January 2021 - December 2021") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )


# Creating a csv file of the clean data for futher analysis or visualizations in other tools like SQL, Tableau, Power BI, etc.

'clean_data <- aggregate(triprawdata$ride_length ~ triprawdata$customer_type + triprawdata$day_of_the_week, FUN = mean)
write.csv(clean_data, "Clean Data.csv", row.names = F)
'
# write.csv(week_days, "C:\\Users\\NEONARD\\Documents\\R scripts\\week_days.csv", row.names = F)
