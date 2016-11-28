##############################
#    MAP BY BOROUGH, RAPE    #
##############################

### BRONX ###

coordinates_bronx <- subset(final_data, Borough == "BRONX")
coordinates_bronx_rape <- subset(coordinates_bronx, Offense == "RAPE")
coordinates_bronx_rape <- subset(coordinates_bronx_rape, Occurrence.Year == 2015)
coordinates_bronx_rape <- data.frame(lat=coordinates_bronx_rape$Latitude, lon=coordinates_bronx_rape$Longitude)

# Transform factors into numeric
options(digits=9)
coordinates_bronx_rape <- data.frame(lapply(coordinates_bronx_rape, function(x) {
    if(!is.factor(x)) x 
    else as.numeric(gsub(",","",levels(x),fixed=TRUE))[x] 
}))

# Create the map
# Bottom left coordinate: 40.796821, -73.985819
# Top right coordinate: 40.894215, -73.730043
lat = c(40.796821, 40.894215)
lon = c(-73.985813, -73.730043)
map <- get_map(location = c(lon=mean(lon), lat=mean(lat)), zoom=12)
bronx_map <- ggmap(map)

# Add coordinates to map
bronx_map_points <- ggmap(map) + ggtitle("Bronx: Rape, 2015") + geom_count(data=coordinates_bronx_rape, color="red")

### MANHATTAN ###

coordinates_manhattan <- subset(final_data, Borough == "MANHATTAN")
coordinates_manhattan_rape <- subset(coordinates_manhattan, Offense == "RAPE")
coordinates_manhattan_rape <- subset(coordinates_manhattan_rape, Occurrence.Year == 2015)
coordinates_manhattan_rape <- data.frame(lat=coordinates_manhattan_rape$Latitude, lon=coordinates_manhattan_rape$Longitude)

# Transform factors into numeric
options(digits=9)
coordinates_manhattan_rape <- data.frame(lapply(coordinates_manhattan_rape, function(x) {
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
manhattan_map_points <- ggmap(map) + ggtitle("Manhattan: Rape, 2015")+ geom_count(data=coordinates_manhattan_rape, color="red")

