########################################
#    EXPLORE FELONY BY JURISDICTION    #
########################################

# Find number of felonies by jurisdiction
felony_by_jurisdiction <- table(final_data$Jurisdiction)

# Remove NA
felony_by_jurisdiction <- felony_by_jurisdiction[-1]
felony_by_jurisdiction <- felony_by_jurisdiction[-1]

# Create barplot (mar for setting margins)
par(mar=c(10,5,5,5))
barplot(felony_by_jurisdiction, main="Number of Felonies by Jurisdiction", yaxp = c(0, 1100000, 20), cex.axis=.8, cex.names=.5, las=2)

