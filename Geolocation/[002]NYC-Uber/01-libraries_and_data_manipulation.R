library(ggplot2)
library(tidyr)


# Read the data
data <- read.csv("uber-raw-data-apr14.csv", header=T)

# Create two columns for "Date" and "Time"
data <- data %>% separate(Date.Time, c("Date", "Time"), sep=" ")

# Create dataset with hours, minutes and seconds separated
final_data <- data
final_data <- final_data %>% separate(Time, c("Hour", "Minutes", "Seconds"), sep=":")
final_data$Hour <- as.numeric(final_data$Hour)

# Change sys language  
Sys.setlocale("LC_TIME", "C")
final_data$Day <- weekdays(as.Date(final_data$Date))

# Reorder dataset
final_data <- cbind(final_data[,c(1,8,2,3,4,5,6,7)])
