##################################################
#    EXPLORE FELONY BY BOROUGH AND POPULATION    #
##################################################

# Create Data Frame with data of population
borough <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
# Data from wikipedia, estimated population in 2014
population <- c(1438159, 2621793, 1636268, 2321580, 473279)
population_by_borough <- data.frame(borough=borough, population=population)

# Find felonies per 1000 people
felony_per_population <- felony_by_borough/(population/1000)

# Create barplot (mar for setting margins)
par(mar=c(5,5,5,5))
barplot(felony_per_population, main="Number of Felonies per 1000 people", yaxp = c(0, 200, 20), cex.axis=.8, cex.names=.5, las=2)

