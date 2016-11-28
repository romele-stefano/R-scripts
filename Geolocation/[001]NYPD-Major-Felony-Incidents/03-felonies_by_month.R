#################################
#    EXPLORE FELONY BY MONTH    #
#################################

# Find numbers of felony by day of the week
felony_by_month <- table(final_data$Occurrence.Month)

# Eliminate first column (NA values)
felony_by_month <- felony_by_month[-1]

# Order the variable
ordered_felony_by_month <- c(felony_by_month[5], felony_by_month[4], felony_by_month[8],
felony_by_month[1], felony_by_month[9], felony_by_month[7], felony_by_month[6],
felony_by_month[2], felony_by_month[12], felony_by_month[11], felony_by_month[10],
felony_by_month[3])

# Create barplot (mar for setting margins)
par(mar=c(5,5,10,5))
barplot(ordered_felony_by_month, main="Number of Felonies by Month", yaxp = c(0, 110000, 10), cex.axis=.8, cex.names=.8, las=2)

# Find percentage of felony by month
ordered_felony_by_month_percentage <- ordered_felony_by_month/nrow(final_data)

# Create pie chart with percentage distributions
cols=rainbow(12)
pie(ordered_felony_by_month_percentage, main="Felony by Month, percentage", cex=.7, radius=.7, col=cols, labels=round(ordered_felony_by_month_percentage, digits=2))
legend("topright", names(ordered_felony_by_month_percentage), cex=.7, fill=cols)
 