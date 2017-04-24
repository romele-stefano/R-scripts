# for tree
library(rpart)
# for plotting the tree
library(RColorBrewer)
library(rpart.plot)
library(rattle)
# random forest
library(randomForest)
# neural net
library(neuralnet)
# support vector machines
library(e1071)
# for ROC 
library(pROC)



data_hr <- read.csv("hr_data.csv", header=TRUE)

# create train and test set
s <- sample(nrow(data_hr), 0.75*nrow(data_hr))
train <- data_hr[s,]
test <- data_hr[-s,]