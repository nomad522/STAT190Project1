rm(list = ls())
#load packages
install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(ggplot2)

#read in work order data
wo <- read.csv("work order scrubbed.csv")

#please don't attach your data
str(wo)
View(wo)
table(wo$cause_code)

#convert start and finish columns to date objects (currently, they're character)
wo$wo_start_date <- ymd_hms(wo$wo_start_date)
#see if it worked:
summary(wo)
wo$wo_finish_date <- ymd_hms(wo$wo_finish_date)
summary(wo)

ggplot(data = wo) +
  geom_bar(aes(wo_start_date))

unique(wo$location_id)

#Read in Sensor Files ----

#list the files available in the fault code folder
sensor_files <- list.files("class_data")

sensor_files[1]
read.csv(paste0("class_data/", sensor_files[2]), header = FALSE)

sensor <- read.csv(paste0("class_data/", sensor_files[2]), header = FALSE)

for (i in 3:length(sensor_files)){
  new <- read.csv(paste0("class_data/", sensor_files[i]), header = FALSE)
  sensor <- rbind(sensor, new)
}
str(sensor)

#Sensor data preparation ----
#Convert date time from char to date time 
sensor$V2 <- ymd_hms(sensor$V2)
summary(sensor)

#Rename columns
names(sensor) <- c("location_id", "DateTime", "Date", "FaultCode", "StatusCode", "Description", "Type")
str(sensor)
wo

#Fault code Sensor data exploration----
#Identify top 5 most common reasons for failure
top5 <- sort(table(sensor$Description),decreasing=TRUE)[1:5]
top5

#Create bar chart to display 5 most common reasons for failure
ggplot(data = as.data.frame(top5), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  labs(y= "Frequency", x = "Description")

#Rank most common Turbines with fault code data
t <- sort(table(sensor$location_id),decreasing=TRUE)[1:11]
t

#Create bar chart to display Turbine and incident count
library(scales)
ggplot(data = as.data.frame(t), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  labs(y= "Frequency", x = "Turbine") +
  scale_y_continuous(labels = label_comma())

#Rank most common Turbines with work orders
t2 <- sort(table(wo$location_id),decreasing=TRUE)[1:20]
t2

#Figure out what Turbine has highest ratio of work orders / fault codes
percents <- merge(x=as.data.frame(t2), y=as.data.frame(t), by="Var1")
percents$need_work <- percents$Freq.x / percents$Freq.y
percents

#Create bar chart for ratio of work orders / fault codes
ggplot(data = percents, aes(x=Var1, y=need_work)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  labs(y= "% Needing Work", x = "Turbine")

#Read in Gearbox Bearing Data ----

#list the files available in the fault code folder
bearing_files <- list.files("Gearbox Bearing Temperature")

bearing_files[1]
read.csv(paste0("Gearbox Bearing Temperature/", bearing_files[2]), header = FALSE)

bearing <- read.csv(paste0("Gearbox Bearing Temperature/", bearing_files[2]), header = FALSE)

for (i in 3:length(bearing_files)){
  temp <- read.csv(paste0("Gearbox Bearing Temperature/", bearing_files[i]), header = FALSE)
  bearing <- rbind(bearing, temp)
}
str(bearing)

noah <- wo[grep("Gear", wo$Order_info), ]

#start with work order, merge preceding hour of data from sensor data

Hello
