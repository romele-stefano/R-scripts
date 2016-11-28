###################################
#    EXPLORE FELONY BY BOROUGH    #
###################################

# Create table with number of felonies by borough
felony_by_borough <- table(final_data$Borough)

# Remove NA
felony_by_borough <- felony_by_borough[-1]
felony_by_borough <- felony_by_borough[-1]

# Create barplot (mar for setting margins)
par(mar=c(8,5,5,5))
barplot(felony_by_borough, main="Number of Felonies by Borough", yaxp = c(0, 350000, 20), cex.axis=.8, cex.names=.6, las=2)

# Find percentage of felony by borough
felony_by_borough_percentage <- felony_by_borough/nrow(final_data)

# Create pie chart with percentage distributions
cols=rainbow(5)
pie(felony_by_borough_percentage, main="Felony by Borough, percentage", cex=.7, col=cols, labels=round(felony_by_borough_percentage, digits=2))
legend("topright", names(felony_by_borough_percentage), cex=.7, fill=cols)
