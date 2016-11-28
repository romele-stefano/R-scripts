###############################
#    MAP BY BOROUGH, MURDER   #
###############################

### MANHATTAN ###

coordinates_manhattan <- subset(final_data, Borough == "MANHATTAN")
coordinates_manhattan_murder <- subset(coordinates_manhattan, Offense == "MURDER & NON-NEGL. MANSLAUGHTE")
coordinates_manhattan_murder <- subset(coordinates_manhattan_murder, Occurrence.Year == 2006)
coordinates_manhattan_murder <- data.frame(lat=coordinates_manhattan_murder$Latitude, lon=coordinates_manhattan_murder$Longitude)


# Transform factors into numeric
options(digits=9)
coordinates_manhattan_murder <- data.frame(lapply(coordinates_manhattan_murder, function(x) {
    if(!is.factor(x)) x 
    else as.numeric(gsub(",","",levels(x),fixed=TRUE))[x] 
}))

# Create the map
# Bottom left coordinate: 40.680458, -74.050258
# Top right coordinate: 40.870734, -73.891825
lat = c(40.680458, 40.870734)
lon = c(-74.050258, -73.891825)
map <- get_map(location = c(lon=mean(lon), lat=mean(lat)), zoom=12)
manhattan_map <- ggmap(map)

# Add coordinates to map
manhattan_map_points <- ggmap(map) + ggtitle("Manhattan: Murder, 2006") + geom_count(data=coordinates_manhattan_murder, color="blue", show.legend=T)

### BROOKLYN ###

coordinates_brooklyn <- subset(final_data, Borough == "BROOKLYN")
coordinates_brooklyn_murder <- subset(coordinates_brooklyn, Offense == "MURDER & NON-NEGL. MANSLAUGHTE")
coordinates_brooklyn_murder <- subset(coordinates_brooklyn_murder, Occurrence.Year == 2006)
coordinates_brooklyn_murder <- data.frame(lat=coordinates_brooklyn_murder$Latitude, lon=coordinates_brooklyn_murder$Longitude)


# Transform factors into numeric
options(digits=9)
coordinates_brooklyn_murder <- data.frame(lapply(coordinates_brooklyn_murder, function(x) {
    if(!is.factor(x)) x 
    else as.numeric(gsub(",","",levels(x),fixed=TRUE))[x] 
}))

# Create the map
# Bottom left coordinate: 40.562791, -74.068603
# Top right coordinate: 40.759680, -73.849563
lat = c(40.562791, 40.759680)
lon = c(-74.068603, -73.849563)
map <- get_map(location = c(lon=mean(lon), lat=mean(lat)), zoom=12)
brooklyn_map <- ggmap(map)

# Add coordinates to map
brooklyn_map_points <- ggmap(map) + ggtitle("Brooklyn: Murder, 2006") + geom_count(data=coordinates_brooklyn_murder, color="blue", show.legend=T)

