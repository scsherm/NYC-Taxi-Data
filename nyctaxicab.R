setwd("/Users/scsherm/Documents/DataIncubator")
library(dplyr)
library(MASS)
library(data.table)
library(geosphere)
library(reshape2)
unzip("trip_data_3.csv.zip")
unzip("trip_fare_3.csv.zip")
trip_data <- read.csv("trip_data_3.csv")
trip_fare <- read.csv("trip_fare_3.csv")

# Examing data
head(trip_data)
head(trip_fare)
levels(trip_fare$payment_type)
length(trip_fare$total_amount)
length(trip_data$trip_distance) #same same length

# PAYMENTS UNDER $5 USING A CREDIT CARD
#Assuming that "CRD" represents a credit card and each "total_payment" represents 1 payment.
under_five <- trip_fare[trip_fare$total_amount < 5, ]
crd_under_five <- under_five[under_five$payment_type == "CRD", ]
payUnder5 <- length(under_five$total_amount)
crdUnder5 <- length(crd_under_five$total_amount)
percentCrdUnder5 <- crdUnder5/payUnder5
as.fractions(percentCrdUnder5) # GCD is 1, 8.9% are card payments < 5

# PAYMENTS OVER $50 USING A CREDIT CARD
#Assuming that "CRD" represents a credit card and each "total_payment" represents 1 payment.
over_fifty <- trip_fare[trip_fare$total_amount > 50, ]
crd_over_fifty <- over_fifty[over_fifty$payment_type == "CRD", ]
payOver50 <- length(over_fifty$total_amount)
crdOver50 <- length(crd_over_fifty$total_amount)
percentCrdOver50 <- crdOver50/payOver50
as.fractions(percentCrdOver50) #GCD is 4, 68.2% are card payments > 50

# MEAN FARE PER MINUTE
taxiData1 <- trip_data %>% 
        select(medallion, hack_license, pickup_datetime, trip_time_in_secs)
taxiData2 <- trip_fare %>% 
        select(medallion, hack_license, pickup_datetime, fare_amount)

#Covert to data.table for faster merge with key based on 3 cols
taxiData1 <- data.table(taxiData1, key = c("medallion", "hack_license", "pickup_datetime"))
taxiData2 <- data.table(taxiData2, key = c("medallion", "hack_license", "pickup_datetime"))
taxiData_fare_time <- merge(taxiData1, taxiData2) #Merge
taxiData_fare_time <- transform(taxiData_fare_time, time_min = trip_time_in_secs/60)
fare_per_min <- taxiData_fare_time$fare_amount/taxiData_fare_time$time_min
mean(fare_per_min, trim = 0.1) # 1.0

# MEDIAN FARE PER MILE 
#Assuming trip distance is in miles
taxiData1 <- trip_data %>% 
        select(medallion, hack_license, pickup_datetime, trip_distance)
taxiData2 <- trip_fare %>% 
        select(medallion, hack_license, pickup_datetime, fare_amount)

#Covert to data.table for faster merge
taxiData1 <- data.table(taxiData1, key = c("medallion", "hack_license", "pickup_datetime"))
taxiData2 <- data.table(taxiData2, key = c("medallion", "hack_license", "pickup_datetime"))
taxiData_fare_mile <- merge(taxiData1, taxiData2) #Merge
fare_per_mile <- taxiData_fare_mile$fare_amount/taxiData_fare_mile$trip_distance
median(fare_per_mile) #5.0

# 95 PERCENTILE FOR MPH 
#Assuming trip distance is in miles
taxiData <- select(trip_data, trip_time_in_secs, trip_distance)
taxiData_hr <- transform(taxiData, time_hr = trip_time_in_secs/60/60)
mile_per_hr <- taxiData_hr$trip_distance/taxiData_hr$time_hr
quantile(mile_per_hr, .95, na.rm = TRUE) #26.68 mph

# MEAN RATIO OF "STRAIGHT" DISTANCE TO DISTANCE DRIVEN
trip_data2 <- trip_data
#Function to compute distance
gcd.slc <- function(long1, lat1, long2, lat2) {
        R <- 6371 # Earth mean radius [km]
        d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
        return(d) # Distance in km
}

#Degrees to radians
deg2rad <- function(deg) return(deg*pi/180) 
trip_data2$pickup_longitude <- deg2rad(trip_data2$pickup_longitude)
trip_data2$pickup_latitude <- deg2rad(trip_data2$pickup_latitude)
trip_data2$dropoff_longitude <- deg2rad(trip_data2$dropoff_longitude)
trip_data2$dropoff_latitude <- deg2rad(trip_data2$dropoff_latitude)

#Distance in miles
trip_data2$straight_dist <- (gcd.slc(trip_data2$pickup_longitude, trip_data2$pickup_latitude, 
                                     trip_data2$dropoff_longitude, trip_data2$dropoff_latitude))*0.621371
distRatio <- trip_data2$straight_dist/trip_data2$trip_distance #Ratio
distRatio <- na.omit(distRatio) #Remove Na's
mean(distRatio, trim = 0.1) #0.8 average ratio

# AVERAGE TIP FOR RIDES FROM JFK
#Using google maps for range of coordinates around airport
#40.64 - 40.67, -73.74 - 73.82 
trip_data3 <- trip_data
trip_fare3 <- trip_fare
trip_data3 <- data.table(trip_data3, key = c("medallion", "hack_license", "pickup_datetime"))
trip_fare3 <- data.table(trip_fare3, key = c("medallion", "hack_license", "pickup_datetime"))
jfkData <- merge(trip_data3, trip_fare3) #Merge
jfkData <- subset(jfkData, pickup_longitude >= -73.82 & pickup_longitude <= -73.74)
jfkData <- subset(jfkData, pickup_latitude >= 40.64 & pickup_latitude <= 40.67)
mean(jfkData$tip_amount) #4.5

# MEDIAN MARCH REVENUE FOR TAXI DRIVER
trip_fare3$date <- strptime(trip_fare3$pickup_datetime, 
                            format = "%Y-%m-%d %H:%M:%S")

