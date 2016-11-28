################################
#    EXPLORE FELONY BY YEAR    #
################################

# Create temporary dataframe with rows ordered by year)
temp <- final_data[order(final_data$Occurrence.Year), ]

# Create table with number of felonies by day
felony_by_year <- table(temp$Occurrence.Year)

# Consider only year 2006 and later
felony_by_year <- subset(felony_by_year, names(felony_by_year) > 2005)

# Create barplot (mar for setting margins)
par(mar=c(5,5,10,5))
barplot(felony_by_year, main="Number of Felonies by Year", yaxp = c(0, 135000, 10), cex.axis=.8, cex.names=.8, las=2)

# Find percentage of felony type over total
felony_by_year_percentage <- felony_by_year/nrow(final_data)

# Create pie chart with percentage distributions
cols=rainbow(10)
pie(felony_by_year_percentage, main="Felony by Year, percentage", cex=.7, radius=.7, col=cols, labels=round(felony_by_year_percentage, digits=2))
legend("topright", names(felony_by_year_percentage), cex=.7, fill=cols)

