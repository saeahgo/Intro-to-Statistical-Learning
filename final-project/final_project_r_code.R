rm(list = ls())

#======================================================================#
### Library I used
library(MASS)
library(stats)
library(class)
library(pROC)
library(dplyr)

#======================================================================#
### Load the data
germancredit <- read.csv("C:/Users/Saeah Go/OneDrive/Desktop/Wi2022/STAT387/Final Project/germancredit.csv")

#======================================================================#
### Set the test data
set.seed(1)
train = sample(1000, 500) # selecting a random subset of 500 observations out of the original 1000 observations

test.german = germancredit[-train,]

#======================================================================#
### (a) LDA
# fit the model
lda.fit = lda(Default ~ ., data = germancredit, family = binomial) 
lda.pred = predict(lda.fit, test.german) 

# compute the confusion matrix
conf_mtx = table(lda.pred$class, test.german$Default, dnn = c("Predicted", "Actual")) 

# sensitivity and specificity
sensitivity = 308/(308+43) # TP/(TP+FN)
specificity = 83/(83+66) # TN/(FP+TN)

# overall misclassification rate
misClassRate = (66+43)/500 # (FP+FN)/total 

# ROC curve
test_roc = roc(test.german$Default, as.numeric(unlist(lda.pred$class)), plot = TRUE, print.auc = TRUE, main = "ROC Curve", legacy.axes = TRUE)




#======================================================================#
### (b) QDA
# fit the model
qda.fit <- qda(Default ~ ., data = germancredit)
qda.pred <- predict(qda.fit, test.german)$class

# compute the confusion matrix
conf_mtx_qda <- table(qda.pred, test.german$Default, dnn = c("Predicted", "Actual"))

# sensitivity and specificity
sensitivity = 299/(299+52) # TP/(TP+FN)
specificity = 115/(115+34) # TN/(FP+TN)

# overall misclassification rate
misClassRate = (34+52)/500 # (FP+FN)/total 

# plot the ROC curve
test_roc = roc(test.german$Default, as.numeric(unlist(qda.pred)), plot = TRUE, print.auc = TRUE, main = "ROC Curve", legacy.axes = TRUE)




#======================================================================#
### (c) Logistic Regression
# fit the model
log.reg = glm(Default ~ ., data = germancredit, family = binomial) 
log.probs = predict(log.reg, test.german, type = "response")

log.pred = rep(0, 500) 
log.pred[log.probs > 0.5] = 1

# compute the confusion matrix
conf_mtx_log <- table(log.pred, test.german$Default, dnn = c("Predicted", "Actual"))

# sensitivity and specificity
sensitivity = 313/(313+38) # TP/(TP+FN)
specificity = 83/(83+66) # TN/(FP+TN)

# overall misclassification rate
misClassRate = (66+38)/500 # (FP+FN)/total 

# plot the ROC curve
test_roc = roc(test.german$Default, as.numeric(unlist(log.pred)), plot = TRUE, print.auc = TRUE, main = "ROC Curve", legacy.axes = TRUE)




#======================================================================#
### (d) KNN 
train.X = germancredit %>% select(-Default)
train.X <- data.matrix(train.X) # convert from dataframe to matrix array

test.X <- germancredit %>% select(-Default)
test.X = test.X[-train,]
test.X <- data.matrix(test.X)

train.Default = germancredit$Default

# find the optimal k using test error rate
testError <- rep(0, 20)
for (i in 1:20) {
  knn.pred <- knn(train.X, test.X, train.Default, k = i)
  testError[i] <- mean(knn.pred != test.german$Default)
}

# plot the 1- test error (accuracy) rate and figure out the optimal k
plot(1:20, 1-testError, xlab = "K", ylab = "Accuracy rate", type = "b")

# fitting knn model
knn.pred = knn(train.X, test.X, train.Default, k = 9)
misClassError <- mean(knn.pred != test.german$Default)
print(paste('Accuracy =', 1-misClassError))

# report the error rate
testError <- mean(knn.pred != test.german$Default)
print(paste('Error rate =', testError))

# report the estimated error rate
expTestError <- mean(knn.pred != germancredit$Default)
print(paste('Estimated error rate =', expTestError))

# compute the confusion matrix
conf_mtx_knn <- table(knn.pred, test.german$Default, dnn = c("Predicted", "Actual"))

# sensitivity and specificity
sensitivity = 326/(326+25) # TP/(TP+FN)
specifisity = 45/(45+104) # TN/(FP+TN)

# plot the ROC curve
test_roc = roc(test.german$Default, as.numeric(unlist(knn.pred)), plot = TRUE, print.auc = TRUE, main = "ROC Curve", legacy.axes = TRUE)

# calculate AUC
auc(test.german$Default, as.numeric(knn.pred))