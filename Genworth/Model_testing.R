getwd()
simple_model = read.csv("SIMPLE_MODEL.csv")
library(caret)
ctrl = trainControl(method="cv",number=5,summaryFunction = twoClassSummary,classProbs = T)
a = createDataPartition(simple_model$NFO_IND,p = 0.1,list=F)
simple_model_train = simple_model[a,]
simple_model_train$NFO_IND = as.factor(ifelse(simple_model_train$NFO_IND==0,'non.NFO','NFO'))
levels(simple_model_train$NFO_IND)


str(simple_model_train)
table(simple_model_train$NFO_IND)
system.time(knn <- train(NFO_IND~.-RBO_IND,data=simple_model_train,method="knn",trControl=ctrl,metric="ROC",tuneLength=20))
knn
summary(knn)
str(knn)
knn

knn_pred = predict(knn,newdata = simple_model_train)
table(knn_pred)
confusionMatrix(knn_pred,simple_model_train$NFO_IND,positive = "NFO")

########################################################
system.time(treebag <- train(NFO_IND~.-RBO_IND,data=simple_model_train,method="treebag",trControl=ctrl,metric="ROC"))
treebag
treebag_pred = predict(treebag,newdata=simple_model_train)
confusionMatrix(treebag_pred,simple_model_train$NFO_IND,positive = "NFO")
table(treebag_pred)

simple_model_val = simple_model[-a,]
simple_model_val$NFO_IND = as.factor(ifelse(simple_model_val$NFO_IND==0,'non.NFO','NFO'))
treebag_pred = predict(treebag, newdata=simple_model_val)
confusionMatrix(treebag_pred,simple_model_val$NFO_IND,positive = "NFO")
table(treebag_pred)


########################################################
system.time(rf <- train(NFO_IND~.-RBO_IND,data=simple_model_train,method="rf",trControl=ctrl,metric="ROC"))




