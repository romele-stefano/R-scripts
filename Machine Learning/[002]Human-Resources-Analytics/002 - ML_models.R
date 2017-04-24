set.seed(888)



##### TREE #####

# create decision tree
tree.fit <- rpart(left ~ ., data = train, method="class")

# plot decision tree
fancyRpartPlot(tree.fit)

# predict values
predicted_tree <- predict(tree.fit, test, type="class")
table(test$left, predicted_tree)
mean(predicted_tree == test$left)



##### RANDOM FOREST #####

# create random forest
rf.fit <- randomForest(as.factor(left) ~ ., data = train, ntree = 250, mtry = 9)

# plot variable importance
varImpPlot(rf.fit, main = "Variable Importance")

# rename columns for plotting legend
colnames(rf.fit$err.rate) <- c("OOB", "Remain", "Left")

# plot random forest error
layout(matrix(c(1,2),nrow = 1), width = c(3.5,1)) 
par(mar=c(5,4,4,0.5)) #No margin on the right side
plot(rf.fit, main = "Random Forest - Error")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1), type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf.fit$err.rate),col = 1:3, cex = 0.8, fill = 1:3)

# predict values 
predicted_rf <- predict(rf.fit, test)
table(test$left, predicted_rf)
mean(predicted_rf == test$left)



##### NEURAL NET #####

# clean data. From RF and Tree we have seen that salary and sales variable are not
# foundamental. So we will drop them
train_nn <- train[,-c(9,10)]
test_nn <- test[,-c(9,10)]

# create neural net
nn.fit <- neuralnet(left ~ satisfaction_level + last_evaluation + number_project +
                    average_montly_hours + time_spend_company + Work_accident +
                    promotion_last_5years,
                    data = train_nn, hidden = 5, threshold = 0.05,        
                    stepmax = 1e+06, algorithm = "rprop+")

# predict values
predicted_nn <- compute(nn.fit, test_nn[,-7])
predicted_nn$net.result <- ifelse(predicted_nn$net.result > 0.5,1,0)
table(test_nn$left, predicted_nn$net.result)
mean(predicted_nn$net.result == test_nn$left)


##### SVM #####

# create support vector machines
svm.fit <- svm(left ~ ., data = train, gamma=0.50, cost=10)

# predict values
predicted_svm <- predict(svm.fit, test)
predicted_svm <- ifelse(predicted_svm > 0.5,1,0)
table(predicted_svm, test$left)
mean(predicted_svm == test$left)
