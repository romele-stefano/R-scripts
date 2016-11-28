library(xlsx)
library(neuralnet)


set.seed(88)

# Read data from excel file
data <- read.xlsx("energy.xlsx", 1)

# clean data (last column are all NA)
data <- data[,-11]

# Eliminate rows with NA
final <- complete.cases(data)
final <- data[final==TRUE,]

# change colnames
colnames(final) <- c("Relative_compactness", "Surface_area", "Wall_area",
"Roof_area", "Overall_height", "Orientation", "Glazing_area",
"Glazing_area_distribution", "Heating_load", "Cooling_load")

# Scale values with min-max. Scale values in range [0,1]
maxs <- apply(final, 2, max) 
mins <- apply(final, 2, min)
scaled <- as.data.frame(scale(final, center = mins, scale = maxs - mins))

# Create train/test
s <- sample(nrow(scaled), 0.75*nrow(scaled))
train <- scaled[s,]
test <- scaled[-s,]
