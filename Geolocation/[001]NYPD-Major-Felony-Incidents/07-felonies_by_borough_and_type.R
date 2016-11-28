############################################
#    EXPLORE FELONY BY BOROUGH AND TYPE    #
############################################

### MANHATTAN ###

# Create subset with only data from manhattan
manhattan <- subset(final_data, final_data$Borough == "MANHATTAN")

# Create table with number of felonies, Manhattan
manhattan_felony_by_type <- table(manhattan$Offense)

# Create barplot (mar for setting margins)
par(mar=c(10,5,5,5))
barplot(manhattan_felony_by_type, main="Number of Felonies by Type, Manhattan", yaxp = c(0, 180000, 20), cex.axis=.8, cex.names=.5, las=2)

# Find percentage of felony by borough
manhattan_felony_by_type_percentage <- manhattan_felony_by_type/nrow(manhattan)

# Create pie chart with percentage distributions
par(mar=c(3,3,3,3))
cols=rainbow(7)
pie(manhattan_felony_by_type_percentage, radius=.6, main="Felony by Type, Manhattan", cex=.7, col=cols, labels=round(manhattan_felony_by_type_percentage, digits=2))
legend("topright", names(manhattan_felony_by_type_percentage), cex=.6, fill=cols)

### BRONX ###

# Create subset with only data from bronx
bronx <- subset(final_data, final_data$Borough == "BRONX")

# Create table with number of felonies, Manhattan
bronx_felony_by_type <- table(bronx$Offense)

# Create barplot (mar for setting margins)
par(mar=c(10,5,5,5))
barplot(bronx_felony_by_type, main="Number of Felonies by Type, Bronx", yaxp = c(0, 60000, 20), cex.axis=.8, cex.names=.5, las=2)

# Find percentage of felony by borough
bronx_felony_by_type_percentage <- bronx_felony_by_type/nrow(bronx)

# Create pie chart with percentage distributions
par(mar=c(3,3,3,3))
cols=rainbow(7)
pie(bronx_felony_by_type_percentage, radius=.6, main="Felony by Type, Bronx", cex=.7, col=cols, labels=round(bronx_felony_by_type_percentage, digits=2))
legend("topright", names(felony_by_type_percentage), cex=.6, fill=cols)

### BROOKLYN ###

# Create subset with only data from bronx
brooklyn <- subset(final_data, final_data$Borough == "BROOKLYN")

# Create table with number of felonies, Manhattan
brooklyn_felony_by_type <- table(brooklyn$Offense)

# Create barplot (mar for setting margins)
par(mar=c(10,5,5,5))
barplot(brooklyn_felony_by_type, main="Number of Felonies by Type, Brooklin", yaxp = c(0, 120000, 20), cex.axis=.8, cex.names=.5, las=2)

# Find percentage of felony by borough
brooklyn_felony_by_type_percentage <- brooklyn_felony_by_type/nrow(brooklyn)

# Create pie chart with percentage distributions
par(mar=c(3,3,3,3))
cols=rainbow(7)
pie(brooklyn_felony_by_type_percentage, radius=.6, main="Felony by Type, Brooklyn", cex=.7, col=cols, labels=round(brooklyn_felony_by_type_percentage, digits=2))
legend("topright", names(felony_by_type_percentage), cex=.6, fill=cols)

