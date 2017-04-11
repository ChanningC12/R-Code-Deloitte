############### caret package modeling methods testing #########################
library(caret)
library(ROCR)
library(lift)
# Data Preparation - Read in Data
getwd()
setwd("../Desktop/")
simple_model = read.csv("SIMPLE_MODEL.csv")
# Data Preparation - Create aggregated target variable and relevel it
simple_model$RESPONSE = ifelse(simple_model$NFO_IND==1,"NFO",ifelse(simple_model$RBO_IND==1,"RBO","FPO"))
simple_model$RESPONSE = as.factor(simple_model$RESPONSE)
table(simple_model$RESPONSE)
simple_model$NFO_IND = as.factor(ifelse(simple_model$NFO_IND==0,'non.NFO','NFO'))
simple_model$RBO_IND = as.factor(ifelse(simple_model$RBO_IND==0,'non.RBO','RBO'))
# Data Preparation - Train/Test/Validation
ind = createDataPartition(simple_model$RESPONSE,p=0.5,list=F)
simple_model_train = simple_model[ind,]
simple_model_val = simple_model[-ind,]

############### start model building process #####################
## Set up global trainControl
ctrl1 = trainControl(method="cv",number=5,classProbs = T, summaryFunction = twoClassSummary)
ctrl2 = trainControl(method="cv",number=5,classProbs = T)

######### AdaBoost Classification Tree ############
## multiple options model
system.time(AdaBoost <- train(RESPONSE~.-NFO_IND-RBO_IND,data=simple_model_train,method="adaboost",
                              metric="Accuracy"))
## Binary model - NFO
system.time(AdaBoost <- train(NFO_IND~.-RESPONSE-RBO_IND,data=simple_model_train,method="adaboost",
                              trControl=ctrl1,metric="ROC"))
## Binary model - RBO
system.time(AdaBoost <- train(RBO_IND~.-RESPONSE-NFO_IND,data=simple_model_train,method="adaboost",
                              trControl=ctrl1,metric="ROC"))

## Bagged CART
## Boosted Classification Trees

######### CART ############
## multiple options model
system.time(CART <- train(RESPONSE~.-NFO_IND-RBO_IND,data=simple_model_train,method="rpart",
                          trControl=ctrl2,tuneLength=10,metric="Kappa"))
CART
CART_pred = predict(CART,newdata=simple_model_val)
table(CART_pred)
confusionMatrix(CART_pred,simple_model_val$RESPONSE)
## Binary model - NFO
system.time(CART_NFO <- train(NFO_IND~.-RESPONSE-RBO_IND,data=simple_model_train,method="rpart",
                              trControl=ctrl1,metric="ROC"))
## Binary model - RBO
system.time(CART_RBO <- train(RBO_IND~.-RESPONSE-NFO_IND,data=simple_model_train,method="rpart",
                              trControl=ctrl1,metric="ROC"))


## eXtreme Gradient Boosting
## generalized linear model
## generalized linear model with stepwise feature selection

######### knn ############
## multiple options model
system.time(knn <- train(RESPONSE~.-NFO_IND-RBO_IND,data=simple_model_train,method="knn",
                          trControl=ctrl2,tuneLength=10,metric="Kappa"))
knn
knn_pred = predict(knn,newdata=simple_model_val)
table(knn_pred)
confusionMatrix(knn_pred,simple_model_val$RESPONSE)
## Binary model - NFO
system.time(knn_NFO <- train(NFO_IND~.-RESPONSE-RBO_IND,data=simple_model_train,method="knn",
                              trControl=ctrl1,metric="ROC"))
## Binary model - RBO
system.time(knn_RBO <- train(RBO_IND~.-RESPONSE-NFO_IND,data=simple_model_train,method="knn",
                              trControl=ctrl1,metric="ROC"))

############## Linear Discriminant Analysis ###################
system.time(lda <- train(RESPONSE~.-NFO_IND-RBO_IND,data=simple_model_train,method="lda",
                         trControl=ctrl2,tuneLength=10,metric="Kappa"))
lda
str(lda)
lda_pred = predict(lda,newdata=simple_model_val)
lda_pred_prob = predict(lda,newdata=simple_model_val,type="prob")
table(lda_pred)
confusionMatrix(lda_pred,simple_model_val$RESPONSE)

## Binary model - NFO
system.time(lda_NFO <- train(NFO_IND~.-RESPONSE-RBO_IND,data=simple_model_train,method="lda",
                             trControl=ctrl1,metric="ROC"))
lda_NFO
lda_NFO_pred = predict(lda_NFO,newdata=simple_model_val)
lda_NFO_pred_prob = predict(lda_NFO,newdata=simple_model_val,type="prob")
table(lda_NFO_pred)
lda_NFO_label = ifelse(simple_model_val$NFO_IND=="NFO",1,0)
plotLift(lda_NFO_pred_prob$NFO,lda_NFO_label,cumulative = T, n.buckets = 10)

## Binary model - RBO
system.time(lda_RBO <- train(RBO_IND~.-RESPONSE-NFO_IND,data=simple_model_train,method="lda",
                             trControl=ctrl1,metric="ROC"))

## Linear Discriminant Analysis with Stepwise feature selection
## Naive Bayes
## Neural Network
## Quadratic Discriminant Analysis

################ Random Forest ###################
system.time(rf <- train(RESPONSE~.-NFO_IND-RBO_IND,data=simple_model_train,method="rf",
                         trControl=ctrl2,tuneLength=10,metric="Kappa"))
rf
rf_pred = predict(rf,newdata=simple_model_val)
table(rf_pred)
confusionMatrix(rf_pred,simple_model_val$RESPONSE)
## Binary model - NFO
system.time(rf_NFO <- train(NFO_IND~.-RESPONSE-RBO_IND,data=simple_model_train,method="rf",
                             trControl=ctrl1,metric="ROC"))
## Binary model - RBO
system.time(rf_RBO <- train(RBO_IND~.-RESPONSE-NFO_IND,data=simple_model_train,method="rf",
                             trControl=ctrl1,metric="ROC"))

## Rule-Based Classifier
## Self-Organizing Map
## Support Vector Machines with Linear Kernel
## Support Vector Machines with Polynomial Kernel