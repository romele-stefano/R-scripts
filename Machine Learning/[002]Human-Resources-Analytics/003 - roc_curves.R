# create receiver operating characteristic curve for each model
roc_tree <- roc(test$left, as.numeric(predicted_tree))
roc_rf <- roc(test$left, as.numeric(predicted_rf))
roc_nn <- roc(test$left, as.numeric(predicted_nn$net.result))
roc_svm <- roc(test$left, as.numeric(predicted_svm))

# add names for legend
legend_names <- c("Tree", "RF", "NN", "Svm")

# create multi curve plot 
layout(matrix(c(1,2),nrow=1), width = c(5,1)) 
par(mar=c(5,4,4,0.5)) #No margin on the right side 
roc_all <- plot(roc_tree, print.auc = TRUE, col = "blue", ylim = c(0,1),
                xlab = "False Positive Rate", ylab = "True Positive Rate")
roc_all <- plot(roc_rf, print.auc = TRUE, col = "green", print.auc.y = .4, add = TRUE,
                ylim = c(1,1))
roc_all <- plot(roc_nn, print.auc = TRUE, col = "red", print.auc.y = .3, add = TRUE,
                ylim = c(1,1))
roc_all <- plot(roc_svm, print.auc = TRUE, col = "orange", print.auc.y = .2, add = TRUE,
                ylim = c(1,1))
par(mar=c(5,0,4,0.5)) #No margin on the left side
plot(c(0,1), type="n", axes=F, xlab = "", ylab = "")
legend("top", legend_names, col = c("blue", "green", "red", "orange"), 
       cex = 0.8, fill = c("blue", "green", "red", "orange"))
