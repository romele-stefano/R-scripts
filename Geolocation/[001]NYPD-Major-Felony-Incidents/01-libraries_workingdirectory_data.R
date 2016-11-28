###################
#    LIBRARIES    #
###################

# Used for extracting Latitude and Longitude from single column
library(tidyr)
# Used for plot
library(ggplot2)
# Used for maps
library(ggmap)



###############################
#    SET WORKING DIRECTORY    #
###############################

setwd("C:/Users/ste/Documents/R/data/nyc-felony")



##############
#    DATA    #
##############

# Read Data
original_data <- read.csv("Major_Felony_Incidents.csv", header=TRUE)

# Split Location Column into Latitude and Longitude
data <- original_data %>% extract(Location.1, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")

# Select data for year 2006 and later: 1,117,273 obs. 
final_data <- subset(data, Occurrence.Year > 2005)

# Data we will not use: 5948 obs.
deleted_data <- subset(data, Occurrence.Year < 2006)

