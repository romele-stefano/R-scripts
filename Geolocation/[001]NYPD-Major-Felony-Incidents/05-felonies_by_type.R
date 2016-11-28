################################
#    EXPLORE FELONY BY TYPE    #
################################

# Create table with number of felonies by type
felony_by_type <- table(final_data$Offense)

# Create barplot (mar for setting margins)
par(mar=c(12,5,5,5))
barplot(felony_by_type, main="Number of Felonies by Type", yaxp = c(0, 450000, 20), cex.axis=.8, cex.names=.6, las=2)

# Find percentage of felony type over total
felony_by_type_percentage <- felony_by_type/nrow(final_data)

# Create pie chart with percentage distributions
cols=rainbow(7)
pie(felony_by_type_percentage, main="Felony by Type, Percentage", cex=.7, radius=.6, col=cols, labels=round(felony_by_type_percentage, digits=2))
legend("topright", names(felony_by_type_percentage), cex=.5, fill=cols)

