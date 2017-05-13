data <- data_hr[, -c(9,10)]
corrvalues <- cor(data)
corrplot(corrvalues, method="color", type="lower", diag=FALSE, tl.col="black")

#explore data
summary(data)

#lowest evaluation = 0.36, highest = 1.0
#divide obs into two groups: best(>avg)/worst(<avg) employees
best <- subset(data, last_evaluation>0.7161017)
worst <- subset(data, last_evaluation<=0.7161017)

#look at corrplot for best employees
cor_best <- cor(best)
corrplot(cor_best, method="color", type="lower", diag=FALSE, tl.col="black")
#look at corrplot for best employees
cor_worst <- cor(worst)
corrplot(cor_worst, method="color", type="lower", diag=FALSE, tl.col="black")
