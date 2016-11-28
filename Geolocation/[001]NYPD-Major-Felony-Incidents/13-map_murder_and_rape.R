############################
#    MAP, MURDER & RAPE    #
############################

# Create data set with data on murders in 2006
coordinates_nyc_murder <- subset(final_data, Offense == "MURDER & NON-NEGL. MANSLAUGHTE")
coordinates_nyc_murder <- subset(coordinates_nyc_murder, Occurrence.Year == 2006)
coordinates_nyc_murder <- data.frame(lat=coordinates_nyc_murder$Latitude, lon=coordinates_nyc_murder$Longitude)

# Create data set with data on rapes in 2006
coordinates_nyc_rape <- subset(final_data, Offense == "RAPE")
coordinates_nyc_rape <- subset(coordinates_nyc_rape, Occurrence.Year == 2006)
coordinates_nyc_rape <- data.frame(lat=coordinates_nyc_rape$Latitude, lon=coordinates_nyc_rape$Longitude)


# Transform factors into numeric
options(digits=9)
coordinates_nyc_murder <- data.frame(lapply(coordinates_nyc_murder, function(x) {
    if(!is.factor(x)) x 
    else as.numeric(gsub(",","",levels(x),fixed=TRUE))[x] 
}))

# Transform factors into numeric
options(digits=9)
coordinates_nyc_rape <- data.frame(lapply(coordinates_nyc_rape, function(x) {
    if(!is.factor(x)) x 
    else as.numeric(gsub(",","",levels(x),fixed=TRUE))[x] 
}))

# Create map of NYC
map <- get_map("New York City", zoom=11)
nyc_map <- ggmap(map)

# Add coordinates to map
nyc_map_points <- ggmap(map) + ggtitle("NYC, Murder(blue) & Rape(red), 2006") + geom_count(data=coordinates_nyc_rape, show.legend=F, color="red") +
geom_count(data=coordinates_nyc_murder, show.legend=F, color="blue") 


