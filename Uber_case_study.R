##--------------   Uber Supply Demand Case Study   -------------------##

##-------------------------------------------------------------##
## Installing and loading required packages for the case study
##-------------------------------------------------------------##

install.packages("tidyr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)


##  Loading uber request data to R studio

uber_data <- read.csv("Uber Request Data.csv")
View(uber_data)


## Converting all timestamp column data (Request.timestamp and Drop.timestamp) to uniform format

uber_data$Request.timestamp <- parse_date_time(uber_data$Request.timestamp, orders = c("d-m-y H:M", "d/m/y H:M:S"))

uber_data$Drop.timestamp <- parse_date_time(uber_data$Drop.timestamp, orders = c("d-m-y H:M", "d/m/y H:M:S"))


# Basic plot to observ count of each status, i.e, completed trip, cancelled trip and no car available in the uber_data df.

uber_data_plot <- ggplot(uber_data, aes(x=factor(uber_data$Status)))+geom_bar()

uber_data_plot + labs(title = "Trip status count", x="Trip status", y="No of trips")


## Hourly demand and supply at city and airport

hourly_uber_request <- ggplot(uber_data, aes(x=factor(hour(Request.timestamp)), fill = Pickup.point))+ geom_bar(position = "dodge")

hourly_uber_request  + labs(title = "Hourly trip requests from Airtport and City", x="hours", y="No of trips")


# From the above plots we can observe that there is high trip request in morning and evening hours at city and airport respectively  
# lets see what are the status of trip requests in these hours

uber_data$hrs <- hour(uber_data$Request.timestamp)

hourly_uber_status <- ggplot(uber_data, aes(x=factor(hrs), fill = uber_data$Status)) + geom_bar(position = "stack")

hourly_uber_status + labs(title = "Hourly trip requests from Airtport and City", x="hours", y="No of trips")

trip_complete_city <- filter(uber_data, Status == "Trip Completed" & Pickup.point == "City")

trip_incomplete_city <- filter(uber_data, Status != "Trip Completed" & Pickup.point == "City")

trip_complete_airport <- filter(uber_data, Status == "Trip Completed" & Pickup.point == "Airport")

trip_incomplete_airport <- filter(uber_data, Status != "Trip Completed" & Pickup.point == "Airport")


## Most problematic type of requests, as we can see in above plot the trip request is high from city in morning and from airport in evening
## Let's create 6 different time slot for the trip request, and obserb the plot

## Converting 24hrs format into different time slot, assuming below timeslot 
## i.e 12-4 - late_night, 4-7 - early_morning, 7-11 - morning, 11-5-day, 5-9-evening, 9-12-night

typeof(uber_data$Request.timestamp)

time_slot_fun <- function(x){
  ifelse(x <= "04:00:00", "late_night",
         ifelse((x > "04:00:00" & x <= "07:00:00"), "early_morning",
                ifelse((x > "07:00:00" & x <= "11:00:00"), "morning",
                       ifelse((x > "11:00:00" & x <= "17:00:00"), "day_time",
                              ifelse((x > "17:00:00" & x <= "21:00:00"), "evening",
                                     ifelse((x > "21:00:00" & x <= "23:59:59"), "night", NA))))))
}


uber_data$req_time_slot <- sapply((format(ymd_hms(uber_data$Request.timestamp), "%H:%M:%S")), time_slot_fun)

#trip frequency with different status stacked based on 6 different derived slot
slot_wise_plot <- ggplot(uber_data, aes(x=factor(uber_data$req_time_slot), fill = uber_data$Status)) + geom_bar(position = "stack")
slot_wise_plot + labs(title = "Request at airport at different slots", x="slots", y="No of trips")

## From the above two plots, the maximum revenue loss observed in morning and evening time slot for Uber, as they can't complete most of the trips
## Also the maximum no of request is seen from City in morning hours, 
## whereas maximum no of request is seen from Airport is in evening hours

## Filtering the uber data from City in morning hours

uber_data_city_morning <- filter(uber_data, (req_time_slot == "early_morning" | req_time_slot == "morning") & Pickup.point=="City"  )

## From the below plot we can see maximum number of request is getting cancelled in morning time, 

plot_uber_city_morning <- ggplot(uber_data_city_morning, aes(x= Pickup.point,fill = Status)) + geom_bar(position = "fill")
plot_uber_city_morning + labs(title = "Request in city in morning",  y="%")

## Finding the percentage loss for uber from city in morning hours

total_trip_city_morning <- nrow(uber_data_city_morning)
incompleted_trip_city_morning <- nrow(filter(uber_data_city_morning, Status != "Trip Completed"))

trip_loss_from_city_morning = (incompleted_trip_city_morning / total_trip_city_morning)*100


## Filtering the uber data from Airport in evening hours

uber_data_airport_evening <- filter(uber_data, (req_time_slot == "evening") & Pickup.point=="Airport"  )

## Unlike city data for morning time, here in the below plot we can see maximum number of request is incomplete due to unavailabilty of cars (no cars available) at the airport in night hours
plot_uber_airport_evening <- ggplot(uber_data_airport_evening, aes(x= Pickup.point,fill = Status)) + geom_bar(position = "fill")
plot_uber_airport_evening+ labs(title = "Request at airport in evening",  y="%")

## Finding the percentage loss for uber from Airport in evening hours
total_trip_airport_evening <- nrow(uber_data_airport_evening)
incompleted_trip_airport_evening <- nrow(filter(uber_data_airport_evening, Status != "Trip Completed"))

trip_loss_from_airport_evening = (incompleted_trip_airport_evening / total_trip_airport_evening)*100

