library(lubridate)
library(dplyr)
library(ggplot2)
# Classwork
##################
# Loading Data
weather <- read.csv("/cloud/project/activity04/campus_weather.csv", na.strings = "#N/A")

# Parsing Date
weather$dateF <- mdy_hm(weather$Date)

#Testing Function
interval <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval

# Set up time intervals in a vector of dates
#Creating Funcion
timeInterval <- function(x) {
  x[-length(x)] %--% x[-1]
}

timeInterval(weather$dateF)

# For loop

for (i in 1:6) {
  print(paste("example",i))
}

seq <- c(1,4,6)

for (i in seq) {
  print(paste("example",i))
}

chEx <- character()

for (i in 1:6) {
  chEx[i] <- paste("example",i)
}

num_ex <- numeric()

for (i in 1:6) {
  num_ex[i] <- 6*i
}


# Prompt 1

rolling_temp <- numeric()

weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

rolling <- weather %>% filter(doy == 2 & year == 2022)

sequences <- 120/15

mean(rolling$AirTemp[1:8])

for (i in 8:nrow(rolling)) {
  rolling_temp[i] <- mean(rolling$AirTemp[(i-7):i])
}

rolling$rolling_temp <- rolling_temp

# Prompt 2

ggplot(weather, aes(x = dateF,y = SolRad)) + 
  geom_point() + 
  labs(x = "Date", y = "Solar Radiation", title = "Plot of Solar Radiation ")

# Prompt 3
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}

timeCheck900(weather$dateF)

#Homework

########## 
#{Homework}

# Question 1 
# Goal: exclude precipitation  that occurs when air temperature is below zero 
# also ensure no observations have X or Y levels greater than two degrees

adj_weather <- weather %>% filter(AirTemp >= 0, XLevel  <=  2.0, YLevel <= 2)

# Number of missing observations - 14,094 observations missing

missing <- nrow(weather) - nrow(adj_weather)

# Question 2 - creating flag for when voltage is less than 8.5 volts 

#Examine the metadata
meta <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv")
# Data is in mV, so 8.5 volts corresponds to 8,500 volts

adj_weather$Battery_Flag <- if_else(adj_weather$BatVolt < 8500, 1, 0)

# Question 3 - Function checking for unrealistic data changes in temperature and solar radiation
######
# Volatility Method

# Examine distribution of temperature

ggplot(adj_weather, aes(x = AirTemp)) +
  geom_boxplot()

# Setting standards for air temp
avg_AirTemp <- mean(adj_weather$AirTemp)
sd_AirTemp <- sd(adj_weather$AirTemp)

rolling_temp_vol <- numeric()

for (i in 10:nrow(adj_weather)) {

  rolling_temp_vol[i] <- sd(adj_weather$AirTemp[(i-9):i])
  
}

adj_weather$rolling_temp_vol <- rolling_temp_vol

abnormal_multiple <- 1.5
temp_vol_threshold <- abnormal_multiple * sd_AirTemp
# Setting standards for radiation

ggplot(adj_weather, aes(x = SolRad)) +
  geom_boxplot()

sd_SolRad <- sd(adj_weather$SolRad)

rolling_rad_vol <- numeric()

for (i in 10:nrow(adj_weather)) {
  
  rolling_rad_vol[i] <- sd(adj_weather$SolRad[(i-9):i])
  
}

adj_weather$rolling_rad_vol <- rolling_rad_vol
rad_vol_threshold <- abnormal_multiple * sd_SolRad

#Test using flag

adj_weather$test_flag <- ifelse(rolling_temp_vol >=  temp_vol_threshold & rolling_rad_vol >= rad_vol_threshold,
                                1, 0)

abnormality <- function(x,y) {
  
  ifelse(x >= temp_vol_threshold & y >= rad_vol_threshold, 1, 0)
  
}
abnormality(adj_weather$rolling_temp_vol, adj_weather$rolling_rad_vol)

#######
# "Range" Method (check out manual and ranges of data that don't make sense for NY)
avg_SolRad <- mean(adj_weather$SolRad)

quantile_temp <- quantile(adj_weather$AirTemp)
quantile_rad <-  quantile(adj_weather$SolRad)

temp_threshold <- quantile_temp[4] + 4 %>% as.numeric()
rad_threshold <-  quantile_rad[4] + 400 %>% as.numeric()

abnormality_range <- function(x,y) {
  
  if_else(x > temp_threshold & y > rad_threshold, 1, 0)
}

adj_weather$abnormality_range <- abnormality_range(adj_weather$AirTemp, adj_weather$SolRad)


# Question 4 - Graph of winter air temperatures from Jan-Mar 2021

air_graph_data <- adj_weather %>% filter(year == 2021 & doy <= 90 )

ggplot(air_graph_data, aes(x = dateF, y = AirTemp)) + 
  geom_line() +
  labs(y = "Air Temperature in Celsius", title = "Graph of Air Temperature from January to March 2021", x = "Date")


# Question 5 - Calculating total precipitation between March and April 2021

precipitation_data <- adj_weather %>% filter(year == 2021 & doy <= 120 & doy >= 60)


# 35F in Celsius is ~1.67 & changing to NAs


# There are 96 15-minute intervals in one day - use minimum daily temperature

daily <- 
  precipitation_data %>% 
  group_by(doy) %>% 
  summarise(daily_precipitation = sum(Precip, na.rm = TRUE),
            daily_airtemp = min(AirTemp, na.rm = TRUE))

Precip_store_new <- numeric()

for (i in 2:nrow(daily)) {
  
   Precip_store_new[i] <- if_else(daily$daily_airtemp[i] <= 1.67 | daily$daily_airtemp[i-1] <= 1.67,
                              NA,
                              daily$daily_precipitation[i])
  
}

daily$Precip_store_new <- Precip_store_new

answer <- sum(!is.na(daily$Precip_store_new))
answer
