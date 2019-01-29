library(ggplot2)
library(gridExtra)
library(plotly)


data <- read.csv("telcoChurnData.csv", head = TRUE)

# data cleaning
# find missing data
colSums(is.na(data))
# remove rows with NA values (11 obs. , 7043 total starting obs.)
dataClean <- data[complete.cases(data), ]

###############
#####     #####
##### EDA #####
#####     #####
###############

# churn rate by gender
bar.gender <- ggplot(data = dataClean, aes(x = gender, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by gender") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by senior citizen
# duplicate SeniorCitizen column
d <- as.factor(dataClean$SeniorCitizen)
# rename the levels
levels(d)[levels(d) == 0] <- "Non senior"
levels(d)[levels(d) == 1] <- "Senior"
bar.senior <- ggplot(data = dataClean, aes(x = d, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by senior citizen") +
  scale_y_continuous(labels = scales::percent) + labs(x = "Senior?",y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by multiple lines
bar.multipleLines <- ggplot(data = dataClean, aes(x = MultipleLines, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by multiple lines") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by internet service
bar.internetService <- ggplot(data = dataClean, aes(x = InternetService, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by internet service") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by tech support
bar.tech <- ggplot(data = dataClean, aes(x = TechSupport, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by tech support") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))


# churn rate by contract
bar.contract <- ggplot(data = dataClean, aes(x = Contract, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by contract") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))


# churn rate by tenure
bar.tenure <- ggplot(data = dataClean, aes(x = tenure, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by tenure") +
  scale_y_continuous(labels = scales::percent) + labs(x = "Number of months the customer has stayed with the company", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E"))

# churn rate by tenure (aggregate data)
# create cuts for aggragation
tenureCuts <- cut(dataClean$tenure, c(0, 12, 24, 36, 48, 60, 72))
tenureLabels <- as.factor(tenureCuts)
# rename the levels
levels(tenureLabels)[levels(tenureLabels) == "(0,12]"] <- "<= 12"
levels(tenureLabels)[levels(tenureLabels) == "(12,24]"] <- "> 12 & <= 24"
levels(tenureLabels)[levels(tenureLabels) == "(24,36]"] <- "> 24 & <= 36"
levels(tenureLabels)[levels(tenureLabels) == "(36,48]"] <- "> 36 & <= 48"
levels(tenureLabels)[levels(tenureLabels) == "(48,60]"] <- "> 48 & <= 60"
levels(tenureLabels)[levels(tenureLabels) == "(60,72]"] <- "> 60 & <= 72"
bar.tenureCuts <- ggplot(data = dataClean, aes(x = tenureLabels, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by tenure (aggregate)") +
  scale_y_continuous(labels = scales::percent) + labs(x = "Number of months the customer has stayed with the company", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by monthly charges (aggregate data)
# create cuts for aggragation
MonthlyChargesCuts <- cut(dataClean$MonthlyCharges, c(18.1, 38.3, 58.3, 78.3, 98.3, 119))
MonthlyChargesLabels <- as.factor(MonthlyChargesCuts)
# rename the levels
levels(MonthlyChargesLabels)[levels(MonthlyChargesLabels) == "(18.1,38.3]"] <- "> 18.1 & <= 38.3"
levels(MonthlyChargesLabels)[levels(MonthlyChargesLabels) == "(38.3,58.3]"] <- "> 38.3 & <= 58.3"
levels(MonthlyChargesLabels)[levels(MonthlyChargesLabels) == "(58.3,78.3]"] <- "> 58.3 & <= 78.3"
levels(MonthlyChargesLabels)[levels(MonthlyChargesLabels) == "(78.3,98.3]"] <- "> 78.3 & <= 98.3"
levels(MonthlyChargesLabels)[levels(MonthlyChargesLabels) == "(98.3,119]"] <- "> 98.3 & <= 119"
bar.monthlyChargesCuts <- ggplot(data = dataClean, aes(x = MonthlyChargesLabels, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by monthly charge (aggregate)") +
  scale_y_continuous(labels = scales::percent) + labs(x = "Monthly Charge (in US $)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by total charges (aggregate data)
# create cuts for aggragation
TotalChargesCuts <- cut(dataClean$TotalCharges, c(0, 1500, 3000, 4500, 6000, 7500, 9000))
TotalChargesLabels <- as.factor(TotalChargesCuts)
# rename the levels
levels(TotalChargesLabels)[levels(TotalChargesLabels) == "(0,1.5e+03]"] <- "> 0 & <= 1.5K"
levels(TotalChargesLabels)[levels(TotalChargesLabels) == "(1.5e+03,3e+03]"] <- "> 1.5K & <= 3K"
levels(TotalChargesLabels)[levels(TotalChargesLabels) == "(3e+03,4.5e+03]"] <- "> 3K & <= 4.5K"
levels(TotalChargesLabels)[levels(TotalChargesLabels) == "(4.5e+03,6e+03]"] <- "> 4.5K & <= 6K"
levels(TotalChargesLabels)[levels(TotalChargesLabels) == "(6e+03,7.5e+03]"] <- "> 6K & <= 7.5K"
levels(TotalChargesLabels)[levels(TotalChargesLabels) == "(7.5e+03,9e+03]"] <- "> 7.5K & <= 9K"
bar.TotalChargesCuts <- ggplot(data = dataClean, aes(x = TotalChargesLabels, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by total charge (aggregate)") +
  scale_y_continuous(labels = scales::percent) + labs(x = "Total Charge (in US $)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5)) 

# churn rate by partner
bar.partner <- ggplot(data = dataClean, aes(x = Partner, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by partner") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by dependents
bar.dependents <- ggplot(data = dataClean, aes(x = Dependents, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by dependents") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by phoneservice
bar.phoneService <- ggplot(data = dataClean, aes(x = PhoneService, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by phone service") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by online security
bar.onlineSecurity <- ggplot(data = dataClean, aes(x = OnlineSecurity, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by online security") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by online backup
bar.onlineBackup <- ggplot(data = dataClean, aes(x = OnlineBackup, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by online backup") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by device protection
bar.deviceProtection <- ggplot(data = dataClean, aes(x = DeviceProtection, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by device protection") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by streaming TV
bar.streamTV <- ggplot(data = dataClean, aes(x = StreamingTV, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by streaming tv") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by streaming movie
bar.streamMovies <- ggplot(data = dataClean, aes(x = StreamingMovies, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by streaming movies") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by paperless billing
bar.paperless <- ggplot(data = dataClean, aes(x = PaperlessBilling, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by paperless billing") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# churn rate by payment method
bar.payment <- ggplot(data = dataClean, aes(x = PaymentMethod, y = (..count..)/nrow(dataClean), fill = Churn)) + geom_bar(position = "fill") + ggtitle("Churn rate by payment method") +
  scale_y_continuous(labels = scales::percent) + labs(y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 6)) +
  scale_fill_manual(values=c("#59FF7C", "#FF596E")) + stat_count(geom = "text", aes(label = ..count..), position = position_fill(vjust = 0.5))

# save plot as png (1280 X fixed ratio) -> increase axis label size
grid.arrange(bar.gender, bar.senior, bar.partner, bar.dependents, nrow = 2)
grid.arrange(bar.tenureCuts, bar.monthlyChargesCuts, bar.TotalChargesCuts, bar.contract, bar.paperless, bar.payment, nrow = 3)
grid.arrange(bar.phoneService, bar.multipleLines, nrow = 2)
grid.arrange(bar.internetService, bar.onlineSecurity, bar.onlineBackup, bar.streamTV, bar.streamMovies, bar.deviceProtection, nrow = 3)
bar.tech


# scatterplot for monthly and total charges divided by number of months spent with te company
# creating new data
addedData <- dataClean
addedData$cuts <- tenureCuts
# subsetting the new dataset
g1 <- subset(addedData, cuts == "(0,12]")
g2 <- subset(addedData, cuts == "(12,24]")
g3 <- subset(addedData, cuts == "(24,36]")
g4 <- subset(addedData, cuts == "(36,48]")
g5 <- subset(addedData, cuts == "(48,60]")
g6 <- subset(addedData, cuts == "(60,72]")

# ADD  + geom_smooth(method = "lm", fill = NA)  for plotting linear regression line
scatter.g1 <- ggplot(g1, aes(x = MonthlyCharges, y = TotalCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for < 1 year customers")
scatter.g2 <- ggplot(g2, aes(x = MonthlyCharges, y = TotalCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for > 1 & <= 2 years customers")
scatter.g3 <- ggplot(g3, aes(x = MonthlyCharges, y = TotalCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for > 2 & <= 3 years customers")
scatter.g4 <- ggplot(g4, aes(x = MonthlyCharges, y = TotalCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for > 3 & <= 4 years customers")
scatter.g5 <- ggplot(g5, aes(x = MonthlyCharges, y = TotalCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for > 4 & <= 5 years customers")
scatter.g6 <- ggplot(g6, aes(x = MonthlyCharges, y = TotalCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for 5+ years customers") 

grid.arrange(scatter.g1, scatter.g2, scatter.g3, scatter.g4, scatter.g5, scatter.g6, nrow = 3)


# scatter plot for tenure and charges
scatter.tenureTotal <- ggplot(dataClean, aes(x = tenure, y = TotalCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for tenure and total charge") 
scatter.tenureMonthly <- ggplot(dataClean, aes(x = tenure, y = MonthlyCharges, colour = Churn)) + geom_point() + 
  ggtitle("Churn for tenure and monthly charge") 

grid.arrange(scatter.tenureTotal, scatter.tenureMonthly, nrow = 2)

# scatter charghes and churn
scatter.tenureChurn <- ggplot(dataClean, aes(x = MonthlyCharges, y = TotalCharges, colour = tenureCuts, shape = Churn)) + geom_point() + 
  scale_colour_discrete("Months with the company", labels = c("Less than 1y", "Between 1y and 2y", "Between 2y and 3y","Between 3y and 4y", "Between 4y and 5y", "Greater than 5y")) +
  ggtitle("Monthly Charges and Total Charges by Tenure group")

scatter.churnTenure <- ggplot(dataClean, aes(x = MonthlyCharges, y = TotalCharges, colour = Churn, shape = tenureCuts)) + geom_point() + 
  scale_shape_discrete("Months with the company", labels = c("Less than 1y", "Between 1y and 2y", "Between 2y and 3y","Between 3y and 4y", "Between 4y and 5y", "Greater than 5y")) +
  ggtitle("Monthly Charges and Total Charges by Tenure group")


grid.arrange(scatter.tenureChurn, scatter.churnTenure, nrow = 2)

# 3d scatter plot
plot_ly(x = dataClean$TotalCharges, y = dataClean$MonthlyCharges, z = dataClean$tenure, type = "scatter3d", mode = "markers", color = dataClean$Churn) 

  
  


##################
#####        #####
##### MODELS #####
#####        #####
##################

# for tree
library(rpart)
#confusion matrix (e1071 for SVM)
library(caret)
library(e1071)
# for plotting the tree
library(RColorBrewer)
library(rpart.plot)
library(rattle)
# area under the curve graph
library(pROC)
# random forest
library(randomForest)
library(randomForestExplainer)
# neural net
library(nnet)
# for plotting NN
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

set.seed(888)

# remove customer ID from the data set
dataModels <- dataClean[,-1]
# create sample for splitting data
s <- sample(nrow(dataModels), 0.8*nrow(dataModels))
# split into training and testing data
train <- dataModels[s,]
test <- dataModels[-s,]



# RANDOM GUESSING
# find % of Churn (26.57%)
table(dataClean$Churn)/nrow(dataClean)

# random guessing for test set
randomGuess[randomGuess == 1] <- "Yes"
randomGuess[randomGuess == 0] <- "No"

confusionMatrix(data = as.factor(randomGuess), reference = test$Churn, positive = "Yes")




# TREE

# model
tree.fit <- rpart(Churn ~ ., data = train, method = "class")

# plot tree
fancyRpartPlot(tree.fit)

# prediction
predicted_tree <- predict(tree.fit, test, type="class")

# accuracy - confusion matrix
confusionMatrix(data = predicted_tree, reference = test$Churn, positive = "Yes")

# manual confusion matrix
table(test$Churn, predicted_tree)
mean(predicted_tree == test$Churn)

# ROC tree
roc.tree <- roc(test$Churn, as.numeric(predicted_tree))
plot(roc.tree, main = "Receiver Operating Characteristic", print.auc = TRUE, col = "blue", ylim = c(0,1), legacy.axes = TRUE, xlab = "1 - False Positive Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")





# RANDOM FOREST

# model
rf.fit <- randomForest(Churn ~ ., data = train, ntree = 100, mtry = 4, localImp = TRUE)

# tune hyperparameters
tun <- tuneRF(train[,-20], train$Churn, stepFactor = 0.5, plot = TRUE, ntreeTry = 1000, trace = TRUE, startmtry = 1, mtry = 4)

# check RF with mtry = 2
rf.fit2 <- randomForest(Churn ~ ., data = train, ntree = 100, mtry = 2, localImp = TRUE)

# tune hyperparameters with e1071 library
# results: mtry = 2, ntree = 101, best_performance = 0.1920052
tuned_parameters_rf <- tune.randomForest(Churn ~ ., data = train, mtry = 1:5, ntree = 50:250)
summary(tuned_parameters_rf)

# check RF with mtry = 2, ntree = 101
rf.fit3 <- randomForest(Churn ~ ., data = train, ntree = 101, mtry = 2)

# plot variable importance
varImpPlot(rf.fit, main = "Variable Importance")

# rename columns for plotting legend
colnames(rf.fit$err.rate) <- c("OOB", "NO", "YES")

# plot random forest error
layout(matrix(c(1,2),nrow = 1), width = c(3.5,1)) 
par(mar=c(5,4,4,0.5)) #No margin on the right side
plot(rf.fit, main = "Random Forest - Error")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1), type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf.fit$err.rate),col = 1:3, cex = 0.8, fill = 1:3)

# prediction
predicted_rf <- predict(rf.fit, test)

# prediction with 2nd RF
predicted_rf2 <- predict(rf.fit2, test)

# prediction with 3rd RF
predicted_rf3 <- predict(rf.fit3, test)

# accuracy - confusion matrix
confusionMatrix(data = predicted_rf, reference = test$Churn, positive = "Yes")

# confusion matrix 2nd RF (increase in Accuracy, increase in Specificity, decrease in Sensitivity)
confusionMatrix(data = predicted_rf2, reference = test$Churn, positive = "Yes")

# confusion matrix 3rd RF
confusionMatrix(data = predicted_rf3, reference = test$Churn, positive = "Yes")

# ROC random forest (ROC 1st RF = 0.705, ROC 2nd RF = 0.704, ROC 3rd RF = 0.707)
roc.randomForest <- roc(test$Churn, as.numeric(predicted_rf))
plot(roc.randomForest, main = "Receiver Operating Characteristic", print.auc = TRUE, col = "blue", ylim = c(0,1), legacy.axes = TRUE, xlab = "1 - False Positive Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")

# explain RF
# explain_forest(rf.fit) # plot distribution of minimal depth and multi-way importance
min_depth_frame <- min_depth_distribution(rf.fit)
# k indicates the number of variables to plot
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 19)
rf.importance <- measure_importance(rf.fit, mean_sample = "relevant_trees") # data frame with importance values (gini_decrease, times_a_root, etc.)
mw1 <- plot_multi_way_importance(rf.importance, size_measure = "no_of_nodes", no_of_labels = 5)
mw2 <- plot_multi_way_importance(rf.importance, x_measure = "gini_decrease", y_measure = "accuracy_decrease", size_measure = "times_a_root", no_of_labels = 5)
mw3 <- plot_multi_way_importance(rf.importance, x_measure = "mean_min_depth", y_measure = "accuracy_decrease", size_measure = "no_of_nodes", no_of_labels = 5)
mw4 <- plot_multi_way_importance(rf.importance, x_measure = "no_of_nodes", y_measure = "accuracy_decrease", size_measure = "times_a_root", no_of_labels = 5)
mw5 <- plot_multi_way_importance(rf.importance, x_measure = "times_a_root", y_measure = "accuracy_decrease", size_measure = "no_of_nodes", no_of_labels = 5)
mw6 <- plot_multi_way_importance(rf.importance, x_measure = "times_a_root", y_measure = "no_of_nodes", size_measure = "gini_decrease", no_of_labels = 5)
grid.arrange(mw1, mw2, nrow = 2)
grid.arrange(mw3, mw4, nrow = 2)
grid.arrange(mw5, mw6, nrow = 2)
plot_importance_ggpairs(rf.importance)
plot_importance_rankings(rf.importance)
vars <- important_variables(rf.importance, k = 10, measures = c("mean_min_depth", "no_of_trees"))
rf.interactions <- min_depth_interactions(rf.fit, vars) 
plot_min_depth_interactions(rf.interactions) 





# GENERALIZED LINEAR MODEL
glm.fit <- glm(Churn ~ ., data = train, family = "binomial")
glm.prob <- predict(glm.fit, data = train, type = "response")
glm.probTest <- predict(glm.fit, newdata = test, type = "response")
glm.pred <- factor(ifelse(glm.probTest >= 0.5, "Yes", "No"))
confusionMatrix(data = glm.pred, reference = test$Churn, positive = "Yes")

roc.glm <- roc(test$Churn, as.numeric(glm.pred))
plot(roc.glm, main = "Receiver Operating Characteristic", print.auc = TRUE, col = "blue", ylim = c(0,1), legacy.axes = TRUE, xlab = "1 - False Positive Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")



# NERUAL NET

# model
nn.fit <- nnet(Churn ~ ., data = train, size = 10, decay = 0.001, maxit = 10000)
# prediction
predicted_nn <- predict(nn.fit, test, type = "class")
predicted_nn_int <- as.factor(predicted_nn)
# confusion matrix
confusionMatrix(data = as.factor(predicted_nn),  reference = test$Churn, positive = "Yes")
# plot NN
plot.nnet(nn.fit)

# tune hyperparameters with e1071 library
tuned_parameters_nn <- tune.nnet(Churn ~ ., data = train, size = 1:10)
summary(tuned_parameters_nn)

roc.nn <- roc(test$Churn, as.numeric(predicted_nn_int))
plot(roc.nn, main = "Receiver Operating Characteristic", print.auc = TRUE, col = "blue", ylim = c(0,1), legacy.axes = TRUE, xlab = "1 - False Positive Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")



# SUPPORT VECTOR MACHINES

# model
svm.fit <- svm(Churn ~ ., data = train, gamma = 0.50, cost = 10)

# predict values
predicted_svm <- predict(svm.fit, test)
# confusion matrix
confusionMatrix(data = as.factor(predicted_svm),  reference = test$Churn, positive = "Yes")

# tune hyperparameters with e1071 library
tuned_parameters_svm <- tune.svm(Churn ~ ., data = train, gamma = 10^(-5:-1), cost = 10^(-3:1))
summary(tuned_parameters_svm)

# model 2
svm.fit2 <- svm(Churn ~ ., data = train, gamma = 0.001, cost = 10)

# predict values
predicted_svm2 <- predict(svm.fit2, test)
# confusion matrix
confusionMatrix(data = as.factor(predicted_svm2),  reference = test$Churn, positive = "Yes")

# roc 1st model
roc.svm <- roc(test$Churn, as.numeric(predicted_svm))
plot(roc.svm, main = "Receiver Operating Characteristic", print.auc = TRUE, col = "blue", ylim = c(0,1), legacy.axes = TRUE, xlab = "1 - False Positive Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")

# roc 2nd model
roc.svm2 <- roc(test$Churn, as.numeric(predicted_svm2))
plot(roc.svm2, main = "Receiver Operating Characteristic", print.auc = TRUE, col = "blue", ylim = c(0,1), xlab = "1 - False Positive Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")



# ALL ROC

# add names for legend
legend_names <- c("TREE", "RF", "GLM", "NN", "SVM", "SVM - Tuned")

# create multi curve plot 
layout(matrix(c(1,2), nrow = 1), width = c(5,1)) 
par(mar = c(5,4,4,0.5)) #No margin on the right side 
roc_all <- plot(roc.tree, main = "Receiver Operating Characteristic", print.auc = TRUE, col = "orange", print.auc.y = .6, legacy.axes = TRUE, xlab = "True Negative Rate (Specificity)", ylab = "True Positive Rate (Sensitivity)")
roc_all <- plot(roc.randomForest, print.auc = TRUE, col = "dark green", print.auc.y = .5, add = TRUE, ylim = c(1,1))
roc_all <- plot(roc.glm, print.auc = TRUE, col = "light green", print.auc.y = .4, add = TRUE, ylim = c(1,1))
roc_all <- plot(roc.nn, print.auc = TRUE, col = "red", print.auc.y = .3, add = TRUE, ylim = c(1,1))
roc_all <- plot(roc.svm, print.auc = TRUE, col = "light blue", print.auc.y = .2, add = TRUE, ylim = c(1,1))
roc_all <- plot(roc.svm2, print.auc = TRUE, col = "blue", print.auc.y = .1, add = TRUE, ylim = c(1,1))
par(mar=c(5,0,4,0.5)) #No margin on the left side
plot(c(0,1), type = "n", axes = FALSE, xlab = "", ylab = "")
legend("top", legend_names, col = c("orange", "dark green", "light green", "red", "light blue", "blue"), 
       cex = 0.8, fill = c("orange", "dark green", "light green", "red", "light blue", "blue"))






### H2O Models
library(h2o)

# start h2o
h2o.init()

h2o_data <- h2o.importFile("C:/Users/romel/Documents/Dataset/telcoChurn/telcoChurnData.csv")
# remove customer_id 
h2o_data <- h2o_data[, -1]
response <- "Churn"
predictors <- colnames(data[,c(-1, -21)])
# create train and test data
splits <- h2o.splitFrame(h2o_data,c(0.8,0.1))
h2o_train <- splits[[1]]
h2o_valid <- splits[[2]]
h2o_test <- splits[[3]]

# RF model
rf.h2o <- h2o.randomForest(x = predictors, y = response, ntrees = 100, max_depth = 30, nbins_cats = 16, training_frame = h2o_train, 
          seed = 888)
# plot variable importance
h2o.varimp_plot(rf.h2o, num_of_features = 20)

h2o.auc(h2o.performance(rf.h2o, newdata = h2o_test))
# predictions
h2o_rf_predictions<-h2o.predict(object = rf.h2o, newdata = h2o_test)
# test set accuracy
mean(h2o_rf_predictions$predict == h2o_test$Churn)  
# classification error = 0.2054, accuracy = 0.7946, sensitivity (TPR) = 0.5555, specificity (TNR) = 0.8844
h2o.confusionMatrix(rf.h2o, h2o_test) 




# NN model RECTIFIER 2 HIDDEN LAYERS
nnR.h2o <- h2o.deeplearning(activation = "Rectifier", training_frame = h2o_train, validation_frame = h2o_valid, x = predictors, y = response, hidden=c(15, 7), 
        epochs = 10000, variable_importances = TRUE, stopping_rounds = 2, stopping_metric = "misclassification", stopping_tolerance = 0.01, 
        seed = 888)

nnR2.h2o <- h2o.deeplearning(activation = "Rectifier", training_frame = h2o_train, validation_frame = h2o_valid, x = predictors, y = response, hidden=c(15, 7, 3), 
                            epochs = 10000, variable_importances = TRUE, stopping_rounds = 2, stopping_metric = "misclassification", stopping_tolerance = 0.01, 
                            seed = 888)

# NN model MAXOUT WITH DROPOUT 2 HIDDEN LAYERS
nnMWD.h2o <- h2o.deeplearning(activation = "MaxoutWithDropout", training_frame = h2o_train, validation_frame = h2o_valid, x = predictors, y = response, hidden=c(15, 7), 
                            epochs = 10000, variable_importances = TRUE, stopping_rounds = 2, stopping_metric = "misclassification", stopping_tolerance = 0.01, 
                            seed = 888)

summary(nn.h2o)

# plot variable importance RECTIFIER NN
h2o.varimp_plot(nnR2.h2o, num_of_features = 30)
# plot variable importance MAXOUT WITH DROPOUT NN
h2o.varimp_plot(nnMWD.h2o, num_of_features = 30)

# predictions
h2o_nn_predictions <- h2o.predict(nnR2.h2o, h2o_test)
# test set accuracy
mean(h2o_nn_prediction$predict == h2o_test$Churn)
# MAXOUTWITHDROPOUT(2 HL -> 15, 7): classification error = 0.1771, accuracy = 0.8228, sensitivity (TPR) = 0.6378, specificity (TNR) = 0.9212 
# RECTIFIER (2 HL -> 15, 7): classification error = 0.2042, accuracy = 0.7952, sensitivity (TPR) = 0.5895, specificity (TNR) = 0.9236
# RECTIFIER (3 HL -> 15, 7, 3): classification error = 0.2042, accuracy = 0.7952, sensitivity (TPR) = 0.5975, specificity (TNR) = 0.9030 
h2o.confusionMatrix(nnR2.h2o, h2o_test) 






# shutdown 
h2o.shutdown(prompt = TRUE)

