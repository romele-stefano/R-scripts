################
#   HEAT MAP   #
################

# BY DAY OF WEEK #

# Create dataframe with data for heatmap
h <- table(final_data[,c(2,3)])
h <- as.data.frame(h)

# Order factors for plotting
h$Day <- factor(h$Day, levels=c("Monday", "Tuesday", "Wednesday", 
"Thursday", "Friday", "Saturday", "Sunday"))

# Create heatmap
ggplot(h, aes(x = Day, y = Hour)) + geom_tile(aes(fill = Freq)) + 
scale_fill_gradient(name = 'Number pickups', low = '#e6ffe6',
high = "#008000", breaks=seq(0, 6000, by=500)) +
ggtitle("NYC: Uber pickups, April 2014")


# BY BASE #

h <- table(final_data[,c(2,8)])
h <- as.data.frame(h)

# Order factors for plotting
h$Day <- factor(h$Day, levels=c("Monday", "Tuesday", "Wednesday", 
"Thursday", "Friday", "Saturday", "Sunday"))

# Create heatmap
ggplot(h, aes(x = Day, y = Base)) + geom_tile(aes(fill = Freq)) + 
scale_fill_gradient(name = 'Number pickups', low = '#e6ffe6',
high = "#008000", breaks=seq(0, 28000, by=5000)) +
ggtitle("NYC: Uber pickups, April 2014")


# BY DAY OF MONTH #

heat_data <- final_data %>% separate(Date, c("Month", "Day", "Year"),
sep="/")

h <- table(heat_data[,c(2,5)])
h <- as.data.frame(h)

# Sequence numbers from 1 to 30 (days in april)
lv <- seq(1,30,by=1)

h$Day <- factor(h$Day, levels=lv)

# Create heatmap
ggplot(h, aes(x = Day, y = Hour)) + geom_tile(aes(fill = Freq)) + 
scale_fill_gradient(name = 'Number pickups', low = '#e6ffe6',
high = "#008000", breaks=seq(0, 3500, by=500)) +
ggtitle("NYC: Uber pickups, April 2014")
