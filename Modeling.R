#import packages
if (!require("randomForest")) {
  install.packages("randomForest")
  library("randomForest")
}
if (!require("glmnet")) {
  install.packages("glmnet")
  library(glmnet)
}
if (!require("pROC")) {
  install.packages("pROC")
  library(pROC)
}
if (!require("caret")) {
  install.packages("caret")
  library(caret)
}

#-------------------------------------------
# read data
#-------------------------------------------

#restore R objects
data <- readRDS("data.RData")

#-------------------------------------------
# data partitioning
#-------------------------------------------

# split the dataset into predictors (x) and response (y)
x = subset(data, select = -ED_Y )
y = data$ED_Y

set.seed(1)

# store row index for 70% training set
train = sample(nrow(x), nrow(x)*.7)

# store row index for 30% testing set
test = (-train)

# create test set with response variable only
y.test = y[test]

#-------------------------------------------
# modeling
#-------------------------------------------

#-------------------------------------------
# logistic regression
#-------------------------------------------

#model 1 logistic regression

# fit a logistic regression model on training data
logistic.mod = glm(ED_Y ~ ., data = data[train,], family = binomial)

print(summary(logistic.mod))

# predict probabilities on test set
logistic.probs=predict(logistic.mod, x[test,], type = "response") 

contrasts(data$ED_Y)

# class predictions if predicted probability is greater than or less than 0.5
logistic.pred=logistic.pred=ifelse(logistic.probs>.5,"Costly","NonCostly")

# confusion matrix
logistic.pred.table=table(logistic.pred, y.test)
logistic.pred.table

# sensitivity, specificity, predictive values
sensitivity(logistic.pred.table)
specificity(logistic.pred.table)
posPredValue(logistic.pred.table)
negPredValue(logistic.pred.table)

# classification accuracy
mean(logistic.pred==y.test)

#roc curve
logistic.roc = roc(y.test, logistic.probs)

# auc
auc(logistic.roc)

#-------------------------------------------
# random forest
#-------------------------------------------

# model 2 random forest without tuning

set.seed(817)

# train the fit random forest on training data
rf = randomForest(ED_Y ~ . , data = data[train,], ntree=500,
                  importance =TRUE)
rf

# predict on the test set
rf.pred = predict(rf,newdata=data[-train ,],type="class")
rf.pred.prob = predict(rf,newdata=data[-train ,],type="prob")

# confusion matrix
rf.pred.table=table(rf.pred, y.test)
rf.pred.table

# sensitivity, specificity, predictive values
sensitivity(rf.pred.table)
specificity(rf.pred.table)
posPredValue(rf.pred.table)
negPredValue(rf.pred.table)

# classification accuracy
mean(rf.pred==y.test)

# ROC curve
rf.roc = roc(y.test, rf.pred.prob[,2])

#auc
auc(rf.roc)

# model 3 random forest with tuning for mtry

set.seed(1)              

# search for the optimal value of mtry 
OOB <- tuneRF(
  x          = x[train,],
  y          = y[train],
  ntreeTry   = 500,
  mtryStart  = 1,
  stepFactor = 1,
  improve    = 1e-5,
  trace      = FALSE       
)

OOB

# find the mtry value that minimizes OOB Error
mtry_opt <- OOB[,"mtry"][which.min(OOB[,"OOBError"])]

# train the random forest on training data
rf.optmtry = randomForest(ED_Y ~ . , data = data[train,], 
                          mtry=mtry_opt, ntree=500)

rf.optmtry

# predict on the test data
rf.optmtry.pred = predict(rf.optmtry,newdata=data[-train ,], type="class")
rf.optmtry.pred.prob = predict(rf.optmtry,newdata=data[-train ,], type="prob")

# confusion matrix
rf.optmtry.table=table(y.test, rf.optmtry.pred)
rf.optmtry.table

# sensitivity, specificity, predictive values
sensitivity(rf.optmtry.table)
specificity(rf.optmtry.table)
posPredValue(rf.optmtry.table)
negPredValue(rf.optmtry.table)

# classification accuracy
mean(rf.optmtry.pred==y.test)

#ROC curve
rf.optmtry.roc = roc(y.test, rf.optmtry.pred.prob[,2])

#auc
auc(rf.optmtry.roc)

#variable importance plot
varImpPlot(rf.optmtry, type=2)

#partial dependence plots
partialPlot(rf.optmtry, data, ED_X)
partialPlot(rf.optmtry, data., CLAIM_X)
data.rf$AREA_DEPRIV_INDEX[is.na(data.rf$AREA_DEPRIV_INDEX)] = mean(data.rf$AREA_DEPRIV_INDEX, na.rm = TRUE)
partialPlot(rf.optmtry, data.rf, AREA_DEPRIV_INDEX)
partialPlot(rf.optmtry, data, TEST_X)
partialPlot(rf.optmtry, data, IMAGE_X)
partialPlot(rf.optmtry, data., LOB)

#-------------------------------------------
# plot ROC curves for performance measurement
#-------------------------------------------

#ROC curves
plot(rf.roc, col = "green", main = "ROC Curves")
lines(rf.optmtry.roc, col = "blue")
lines(logistic.roc, col = "purple")
legend("bottomright",
       legend=c("Random Forest","Random Forest Optimized","Logistic Regression"),
       col=c("green","blue", "purple"),
       lty=1,
       cex=.5)


