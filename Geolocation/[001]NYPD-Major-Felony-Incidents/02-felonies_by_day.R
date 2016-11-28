#######################################
#    EXPLORE FELONY BY DAY OF WEEK    #
#######################################

# Find numbers of felony by day of the week
felony_by_day <- table(final_data$Day.of.Week)

# Eliminate first column (NA values)
felony_by_day <- felony_by_day[-1]

# Order the variable
ordered_felony_by_day <- c(felony_by_day[2], felony_by_day[6], felony_by_day[7], felony_by_day[5], felony_by_day[1], felony_by_day[3], felony_by_day[4])

# Create barplot (mar for setting margins)
par(mar=c(5,5,10,5))
barplot(ordered_felony_by_day, main="Number of Felonies by Day of the Month", yaxp=c(0, 200000, 20), cex.axis=.8, cex.names=.6, las=2)

# Find percentage of felony by day
ordered_felony_by_day_percentage <- ordered_felony_by_day/nrow(final_data)

# Create pie chart with percentage distributions
cols=rainbow(7)
pie(ordered_felony_by_day_percentage, main="Felony by Day of the Week, percentage", cex=.7, radius=.7, col=cols, labels=round(ordered_felony_by_day_percentage, digits=2))
legend("topright", names(ordered_felony_by_day_percentage), cex=.7, fill=cols)



################################################
#    EXPLORE FELONY BY DAY OF WEEK [NUMBER]    #
################################################

# Create temporary dataframe with rows ordered by number of day
temp <- final_data[order(final_data$Occurrence.Day), ]

# Create table with number of felonies by day7
felony_by_day_number <- table(temp$Occurrence.Day)

# Create barplot (mar for setting margins)
par(mar=c(5,5,10,5))
barplot(felony_by_day_number, main="Number of Felonies by Day of the Month", yaxp=c(0, 50000, 10), cex.axis=.8, cex.names=.6, las=2)

