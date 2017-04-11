rm(list=ls())
getwd()
setwd("../Desktop/")

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(caret)
library(lift)

# read in data
simple_model = read.csv("SIMPLE_MODEL.csv")
str(simple_model)

# create response variable
simple_model$RESPONSE = ifelse(simple_model$NFO_IND==1,"NFO",ifelse(simple_model$RBO_IND==1,"RBO","FPO"))
simple_model$RESPONSE = as.factor(simple_model$RESPONSE)

# before running the model it is important to choose a reference group
simple_model$RESPONSE2 = relevel(simple_model$RESPONSE,ref="FPO")

# separate into train/test/validation
ind = createDataPartition(simple_model$RESPONSE,p=0.5,list=F)
simple_model_train = simple_model[ind,]
simple_model_val = simple_model[-ind,]

# build the model on trainset
train = multinom(RESPONSE2~.-RESPONSE-NFO_IND-RBO_IND,data=simple_model_train)
summary(train)

# the relative risk ratio for a one-unit increase in the variable networth is .793 for being in NFO vs. FPO
exp(coef(train))

# use predicted probability to understand the model
head(fitted(train))

# predict on validation set
pred = predict(train,newdata=simple_model_val,"probs")
pred = as.data.frame(pred)
head(pred)

# ROC
pred_roc = prediction(prediction = pred$NFO,labels = simple_model_val$NFO_IND)
perf_roc = performance(pred_roc,measure = "tpr",x.measure = "fpr")
plot(perf_roc, main = "ROC Curve - NFO")
perf_auc = performance(pred_roc, measure="auc")
unlist(perf_auc@y.values)

# lift curve
plotLift(pred$NFO,simple_model_val$NFO_IND,cumulative = T, n.buckets = 10)
TopDecileLift(pred$NFO,simple_model_val$NFO_IND) # top 10% lift







