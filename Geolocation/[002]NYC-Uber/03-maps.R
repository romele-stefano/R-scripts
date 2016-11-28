###############
#   NYC MAP   #
###############

# New York City Boundaries
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

plot <- ggplot(data, aes(x = Lon, y = Lat, colour=Base))
plot <- plot + geom_point(size=0.06) +
scale_x_continuous(limits=c(min_long, max_long)) +
scale_y_continuous(limits=c(min_lat, max_lat))+
theme(legend.text=element_text(size=15), legend.title=element_text(size=15))+
guides(colour = guide_legend(override.aes = list(size=3)))+
ggtitle("NYC: Uber Pickups, April 2014")+
theme(panel.background = element_rect(fill="black", color="black"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.text = element_blank(), axis.line = element_blank(),
      axis.title = element_blank(), axis.ticks = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill="black", color="black"),
	plot.title = element_text(size = rel(2), colour = "white"))

png("nyc-uber-apr14.png", w=3500, h=3500, res=300)
par(mar=c(50, 50, 50, 50))
plot
dev.off()



##############################
#   NYC MAP WITH FREQUENCY   #
##############################

# Find all possible Lat/Lon combinations
comb <- table(final_data$Lat, final_data$Lon)
comb <- data.frame(comb)

# Extract unique locations with at least 1 pickup
comb <- subset(comb, Freq != 0)

# Change colnames
colnames(comb) <- c("Lat", "Lon", "Num_pickups")

# Change data type for plotting reasons
comb$Lat <- as.numeric(as.character(comb$Lat))
comb$Lon <- as.numeric(as.character(comb$Lon))

# New York City Boundaries
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

plot <- ggplot(comb, aes(x = Lon, y = Lat))
plot <- plot + geom_point(size=0.006, aes(colour = cut(comb$Num_pickups, c(0, 5, 10, Inf)))) +
scale_color_manual(name = "Number of pickups",
values = c("(0,5]" = "light blue","(5,10]" = "green", "(10,Inf]" = "red"),
                     labels = c("0-5", "6-10", "10-500"))+
scale_x_continuous(limits=c(min_long, max_long)) +
scale_y_continuous(limits=c(min_lat, max_lat))+
theme(legend.text=element_text(size=15), legend.title=element_text(size=15))+
guides(colour = guide_legend(override.aes = list(size=3)))+
ggtitle("NYC: Uber Pickups, April 2014")+
theme(panel.background = element_rect(fill="black", color="black"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.text = element_blank(), axis.line = element_blank(),
      axis.title = element_blank(), axis.ticks = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill="black", color="black"),
	plot.title = element_text(size = rel(2), colour = "white"))

png("nyc-uber-apr14-frequency.png", w=3500, h=3500, res=300)
par(mar=c(50, 50, 50, 50))
plot
dev.off()
