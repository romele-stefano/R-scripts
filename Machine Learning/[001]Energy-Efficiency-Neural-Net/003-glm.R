# Create train/test 
lm.train <- final[s,]
lm.test <- final[-s,]

# Fit generalized linear model for Heating load
lm.fit_heat <- glm(Heating_load~.-Cooling_load, data=lm.train)
# Fit generalized linear model for Cooling load
lm.fit_cool <- glm(Cooling_load~.-Heating_load, data=lm.train)

# Predict values
pr.lm_heat <- predict(lm.fit_heat,lm.test)
pr.lm_cool <- predict(lm.fit_cool,lm.test)

compare_lm <- data.frame(cbind(lm.test$Heating_load, pr.lm_heat, lm.test$Cooling_load, pr.lm_cool))
colnames(compare_lm) <- c("heat_load", "heat_load_predicted", "cool_load", "cool_load_predicted") 

# Compute error
se_h <- 0
se_c <- 0
for (i in 1:nrow(compare_lm)) {
	se_h[i] <- (compare_lm$heat_load_predicted[i] - compare_lm$heat_load[i])^2
	se_c[i] <- (compare_lm$cool_load_predicted[i] - compare_lm$cool_load[i])^2
}

# arrange columns
compare_lm <- cbind(compare_lm, se_h, se_c)
compare_lm <- compare_lm[,c(1,2,5,3,4,6)]

# Mean Squared Error
MSE_heat_lm <- sum(compare_lm$se_h)/nrow(compare_lm)
MSE_cool_lm <- sum(compare_lm$se_c)/nrow(compare_lm)

