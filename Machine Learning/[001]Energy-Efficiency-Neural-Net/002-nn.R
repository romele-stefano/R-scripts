# Create Neural Net
nn.fit <- neuralnet(Heating_load+Cooling_load~Relative_compactness+
Surface_area+Wall_area+Roof_area+Overall_height+Orientation+
Glazing_area+Glazing_area_distribution,
data=train, hidden=c(4), threshold = 0.01, stepmax = 1e+06, 
algorithm = "rprop+")

# Predict values for test
output <- compute(nn.fit, test[,-c(9,10)])

# Scale back values
output_scaled_heat <- output$net.result[,1]*(max(final$Heating_load)-min(final$Heating_load))+min(final$Heating_load)
output_scaled_cool <- output$net.result[,2]*(max(final$Cooling_load)-min(final$Cooling_load))+min(final$Cooling_load)

# Compare
t <- final[-s,]
compare <- data.frame(cbind(t$Heating_load, output_scaled_heat, t$Cooling_load, output_scaled_cool))
colnames(compare) <- c("heat_load", "heat_load_predicted", "cool_load", "cool_load_predicted") 

# Compute squared error
se_h <- 0
se_c <- 0
for (i in 1:nrow(compare)) {
	se_h[i] <- (compare$heat_load_predicted[i] - compare$heat_load[i])^2
	se_c[i] <- (compare$cool_load_predicted[i] - compare$cool_load[i])^2
}

# arrange columns
compare <- cbind(compare, se_h, se_c)
compare <- compare[,c(1,2,5,3,4,6)]

# Mean Squared Error
MSE_heat <- sum(compare$se_h)/nrow(compare)
MSE_cool <- sum(compare$se_c)/nrow(compare)

