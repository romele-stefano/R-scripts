###################################################
#    EXPLORE FELONY BY BOROUGH AND SQUARE MILE    #
###################################################

# Create Data Frame with data of square mile
borough <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
# Square mile, data from wikipedia
sm <- c(42, 71, 23, 109, 58)
sm_by_borough <- data.frame(borough=borough, square_mile=sm)

# Find felonies by square mile
felony_per_sm <- felony_by_borough/sm

# Create barplot (mar for setting margins)
par(mar=c(5,5,10,5))
barplot(felony_per_sm, main="Number of Felonies per square mile", yaxp = c(0, 15000, 15), cex.axis=.8, cex.names=.5, las=2)


