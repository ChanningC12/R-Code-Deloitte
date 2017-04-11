rm(list=ls())
gc()
getwd()
setwd("../Desktop/")

library(caret)
library(rpart)
library(DMwR)
library(ROCR)
library(lift)
library(PRROC)
library(randomForest)
library(kernlab)
library(dplyr)
library(data.table)
library(ggplot2)
library(plyr)

# read in 4 scored dataset
nfo_glm = read.csv("VAL_SCORED_DECILED_NFO_12141_v2.csv")
rbo_glm = read.csv("VAL_SCORED_DECILED_RBO_22051_v2.csv")
nfo_rf = read.csv("RF_NFO.csv")
rbo_rf = read.csv("RF_RBO.csv")

# create centile score
nfo_glm = mutate(nfo_glm,centile=ntile(nfo_glm$Prob,100))
rbo_glm = mutate(rbo_glm,centile=ntile(rbo_glm$Prob,100))
nfo_rf = mutate(nfo_rf,centile=ntile(nfo_rf$Prob1,100))
rbo_rf = mutate(rbo_rf,centile=ntile(rbo_rf$Prob1,100))

# prediction, top 99%
nfo_glm = mutate(nfo_glm,pred99=ifelse(nfo_glm$score100==100,1,0))
rbo_glm = mutate(rbo_glm,pred99=ifelse(rbo_glm$score100==100,1,0))
nfo_rf = mutate(nfo_rf,pred99=ifelse(nfo_rf$centile==100,1,0))
rbo_rf = mutate(rbo_rf,pred99=ifelse(rbo_rf$centile==100,1,0))

# prediction, top 95%
nfo_glm = mutate(nfo_glm,pred95=ifelse(nfo_glm$score100>95,1,0))
rbo_glm = mutate(rbo_glm,pred95=ifelse(rbo_glm$score100>95,1,0))
nfo_rf = mutate(nfo_rf,pred95=ifelse(nfo_rf$centile>95,1,0))
rbo_rf = mutate(rbo_rf,pred95=ifelse(rbo_rf$centile>95,1,0))

# prediction, top 90%
nfo_glm = mutate(nfo_glm,pred90=ifelse(nfo_glm$score100>90,1,0))
rbo_glm = mutate(rbo_glm,pred90=ifelse(rbo_glm$score100>90,1,0))
nfo_rf = mutate(nfo_rf,pred90=ifelse(nfo_rf$centile>90,1,0))
rbo_rf = mutate(rbo_rf,pred90=ifelse(rbo_rf$centile>90,1,0))

# ROC curve - NFO GLM
tapply(nfo_glm$pred_NFO_12141,nfo_glm$NFO_IND,mean)
nfo_glm_roc = prediction(nfo_glm$pred_NFO_12141,nfo_glm$NFO_IND)
nfo_glm_roc_curve = performance(nfo_glm_roc,measure="tpr",x.measure="fpr")
plot(nfo_glm_roc_curve,main="ROC Curve - NFO - GLM",lwd=3)
abline(0,1,lty=2,col="red")
nfo_glm_auc = performance(nfo_glm_roc,measure="auc")
unlist(nfo_glm_auc@y.values) # 0.68

# ROC curve - RBO GLM
tapply(rbo_glm$Prob,rbo_glm$RBO_IND,mean)
rbo_glm_roc = prediction(rbo_glm$Prob,rbo_glm$RBO_IND)
rbo_glm_roc_curve = performance(rbo_glm_roc,measure="tpr",x.measure="fpr")
plot(rbo_glm_roc_curve,main="ROC Curve - RBO - GLM",lwd=3)
abline(0,1,lty=2,col="red")
rbo_glm_auc = performance(rbo_glm_roc,measure="auc")
unlist(rbo_glm_auc@y.values) # 0.74

# ROC curve - NFO RF
tapply(nfo_rf$Prob1,nfo_rf$Actual,mean)
nfo_rf_roc = prediction(nfo_rf$Prob1,nfo_rf$Actual)
nfo_rf_roc_curve = performance(nfo_rf_roc,measure="tpr",x.measure="fpr")
plot(nfo_rf_roc_curve,main="ROC Curve - NFO - Random Forest",lwd=3)
abline(0,1,lty=2,col="red")
nfo_rf_auc = performance(nfo_rf_roc,measure="auc")
unlist(nfo_rf_auc@y.values) # 0.90

# ROC curve - RBO RF
tapply(rbo_rf$Prob1,rbo_rf$Actual,mean)
rbo_rf_roc = prediction(rbo_rf$Prob1,rbo_rf$Actual)
rbo_rf_roc_curve = performance(rbo_rf_roc,measure="tpr",x.measure="fpr")
plot(rbo_rf_roc_curve,main="ROC Curve - RBO - Random Forest",lwd=3)
abline(0,1,lty=2,col="red")
rbo_rf_auc = performance(rbo_rf_roc,measure="auc")
unlist(rbo_rf_auc@y.values) # 0.83

# Confustion Matrix, top 99%
confusionMatrix(nfo_glm$pred99,nfo_glm$NFO_IND,positive="1")
confusionMatrix(rbo_glm$pred99,rbo_glm$RBO_IND,positive="1")
confusionMatrix(nfo_rf$pred99,nfo_rf$Actual,positive="1")
confusionMatrix(rbo_rf$pred99,rbo_rf$Actual,positive="1")

# Confusion Matrix, top 95%
confusionMatrix(nfo_glm$pred95,nfo_glm$NFO_IND,positive="1")
confusionMatrix(rbo_glm$pred95,rbo_glm$RBO_IND,positive="1")
confusionMatrix(nfo_rf$pred95,nfo_rf$Actual,positive="1")
confusionMatrix(rbo_rf$pred95,rbo_rf$Actual,positive="1")

# Confusion Matrix - top 90%
confusionMatrix(nfo_glm$pred90,nfo_glm$NFO_IND,positive="1")
confusionMatrix(rbo_glm$pred90,rbo_glm$RBO_IND,positive="1")
confusionMatrix(nfo_rf$pred90,nfo_rf$Actual,positive="1")
confusionMatrix(rbo_rf$pred90,rbo_rf$Actual,positive="1")

############## Dec. 22 #################
############## NFO:12251, RBO:22451 ###############
# read in 4 scored dataset
nfo_glm = read.csv("VAL_SCORED_DECILED_NFO_12251_v2.csv")
rbo_glm = read.csv("VAL_SCORED_DECILED_RBO_22451_v2.csv")
# prediction, top 99%
nfo_glm = mutate(nfo_glm,pred99=ifelse(nfo_glm$score100==100,1,0))
rbo_glm = mutate(rbo_glm,pred99=ifelse(rbo_glm$score100==100,1,0))
confusionMatrix(nfo_glm$pred99,nfo_glm$NFO_IND,positive="1")
confusionMatrix(rbo_glm$pred99,rbo_glm$RBO_IND,positive="1")

# prediction, top 95%
nfo_glm = mutate(nfo_glm,pred95=ifelse(nfo_glm$score100>95,1,0))
rbo_glm = mutate(rbo_glm,pred95=ifelse(rbo_glm$score100>95,1,0))
confusionMatrix(nfo_glm$pred95,nfo_glm$NFO_IND,positive="1")
confusionMatrix(rbo_glm$pred95,rbo_glm$RBO_IND,positive="1")

# prediction, top 90%
nfo_glm = mutate(nfo_glm,pred90=ifelse(nfo_glm$score100>90,1,0))
rbo_glm = mutate(rbo_glm,pred90=ifelse(rbo_glm$score100>90,1,0))
# Confusion Matrix - top 90%
confusionMatrix(nfo_glm$pred90,nfo_glm$NFO_IND,positive="1")
confusionMatrix(rbo_glm$pred90,rbo_glm$RBO_IND,positive="1")


# ROC curve - NFO GLM
tapply(nfo_glm$pred_NFO_12251,nfo_glm$NFO_IND,mean)
nfo_glm_roc = prediction(nfo_glm$pred_NFO_12251,nfo_glm$NFO_IND)
nfo_glm_roc_curve = performance(nfo_glm_roc,measure="tpr",x.measure="fpr")
plot(nfo_glm_roc_curve,main="ROC Curve - NFO - GLM",lwd=3)
abline(0,1,lty=2,col="red")
nfo_glm_auc = performance(nfo_glm_roc,measure="auc")
unlist(nfo_glm_auc@y.values) # 0.85

# ROC curve - RBO GLM
tapply(rbo_glm$pred_RBO_22451,rbo_glm$RBO_IND,mean)
rbo_glm_roc = prediction(rbo_glm$pred_RBO_22451,rbo_glm$RBO_IND)
rbo_glm_roc_curve = performance(rbo_glm_roc,measure="tpr",x.measure="fpr")
plot(rbo_glm_roc_curve,main="ROC Curve - RBO - GLM",lwd=3)
abline(0,1,lty=2,col="red")
rbo_glm_auc = performance(rbo_glm_roc,measure="auc")
unlist(rbo_glm_auc@y.values) # 0.74

# create probability for NFO and RBO
nfo_glm$prob = exp(nfo_glm$pred_NFO_12251)/(1+exp(nfo_glm$pred_NFO_12251))
quantile(nfo_glm$prob)

rbo_glm$prob = exp(rbo_glm$pred_RBO_22451)/(1+exp(rbo_glm$pred_RBO_22451))
quantile(rbo_glm$prob)

# prediction, NFO prob>2.7%, RBO prob>6%
nfo_glm = mutate(nfo_glm,pred_pop=ifelse(nfo_glm$prob>mean(nfo_glm$NFO_IND),1,0))
rbo_glm = mutate(rbo_glm,pred_pop=ifelse(rbo_glm$prob>mean(rbo_glm$RBO_IND),1,0))
confusionMatrix(nfo_glm$pred_pop,nfo_glm$NFO_IND,positive="1")
confusionMatrix(rbo_glm$pred_pop,rbo_glm$RBO_IND,positive="1")

# produce random forest confusion matrix on NFO prob>2.7%, RBO prob>6%
nfo_rf = read.csv("RF_NFO.csv")
rbo_rf = read.csv("RF_RBO.csv")

# prediction, NFO prob>2.7%, RBO prob>6%
nfo_rf = mutate(nfo_rf,pred_pop=ifelse(nfo_rf$Prob1>mean(nfo_rf$Actual),1,0))
rbo_rf = mutate(rbo_rf,pred_pop=ifelse(rbo_rf$Prob1>mean(rbo_rf$Actual),1,0))
confusionMatrix(nfo_rf$pred_pop,nfo_rf$Actual,positive="1")
confusionMatrix(rbo_rf$pred_pop,rbo_rf$Actual,positive="1")


##################### Dec. 27th ########################
# Top 2.7% cutoff, 2294 actual NFOs in the validation dataset and 5193 actual RBOs in the validation dataset
# create probability for NFO and RBO
nfo_glm$prob = exp(nfo_glm$pred_NFO_12251)/(1+exp(nfo_glm$pred_NFO_12251))
rbo_glm$prob = exp(rbo_glm$pred_RBO_22451)/(1+exp(rbo_glm$pred_RBO_22451))

# rank by probability
nfo_glm = nfo_glm[order(nfo_glm$prob,decreasing = T),]
rbo_glm = rbo_glm[order(rbo_glm$prob,decreasing = T),]
nfo_rf = nfo_rf[order(nfo_rf$Prob1,decreasing = T),]
rbo_rf = rbo_rf[order(rbo_rf$Prob1,decreasing = T),]

# Assign the top 2294 / 5193 as actual
nfo_glm$pred_overall = ifelse(nfo_glm$prob>=nfo_glm[2294,]$prob,1,0)
rbo_glm$pred_overall = ifelse(rbo_glm$prob>=rbo_glm[5193,]$prob,1,0)
nfo_rf$pred_overall = ifelse(nfo_rf$Prob1>=nfo_rf[2294,]$Prob1,1,0)
rbo_rf$pred_overall = ifelse(rbo_rf$Prob1>=rbo_rf[5193,]$Prob1,1,0)

# Produce confusion matrix
confusionMatrix(nfo_glm$pred_overall,nfo_glm$NFO_IND,positive = "1")
confusionMatrix(rbo_glm$pred_overall,rbo_glm$RBO_IND,positive = "1")
confusionMatrix(nfo_rf$pred_overall,nfo_rf$Actual,positive = "1")
confusionMatrix(rbo_rf$pred_overall,rbo_rf$Actual,positive = "1")

# Read in Genworth's scored data NFO
nfo_glm_Genworth = read.csv("NFO_model_scores_Genworth.csv")
summary(nfo_glm_Genworth)
count(nfo_glm_Genworth$NFO)

# Examine the ROC curve in their scored data
Genworth_pred = prediction(nfo_glm_Genworth$y_hat,nfo_glm_Genworth$NFO)
Genworth_roc = performance(Genworth_pred,measure="tpr",x.measure="fpr")
plot(Genworth_roc,main="Genworth Internal ROC Curve - GBM",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Genworth_auc = performance(Genworth_pred,measure="auc")
unlist(Genworth_auc@y.values)

plotLift(nfo_glm_Genworth$y_hat,nfo_glm_Genworth$NFO,cumulative=T,n.buckets=10)
TopDecileLift(nfo_glm_Genworth$y_hat,nfo_glm_Genworth$NFO) # 4.578

# Produce cutoff at top 90%, 95%, 99%
nfo_glm_Genworth = mutate(nfo_glm_Genworth,centile=ntile(nfo_glm_Genworth$y_hat,100))
nfo_glm_Genworth = mutate(nfo_glm_Genworth,pred90=ifelse(nfo_glm_Genworth$centile>90,1,0))
nfo_glm_Genworth = mutate(nfo_glm_Genworth,pred95=ifelse(nfo_glm_Genworth$centile>95,1,0))
nfo_glm_Genworth = mutate(nfo_glm_Genworth,pred99=ifelse(nfo_glm_Genworth$centile>99,1,0))

# Produce confusion matrix
confusionMatrix(nfo_glm_Genworth$pred95,nfo_glm_Genworth$NFO,positive = "1")
confusionMatrix(nfo_glm_Genworth$pred99,nfo_glm_Genworth$NFO,positive = "1")

############### Look at the top 1.5% for NFO #################
nfo_glm = mutate(nfo_glm,quantile1000=ntile(nfo_glm$prob,1000))
nfo_rf = mutate(nfo_rf,quantile1000=ntile(nfo_rf$Prob1,1000))

# Assign top 1.5% as NFOs
nfo_glm = mutate(nfo_glm,pred1000=ifelse(nfo_glm$quantile1000>985,1,0))
nfo_rf = mutate(nfo_rf,pred1000=ifelse(nfo_rf$quantile1000>985,1,0))

# Produce confusion matrix
confusionMatrix(nfo_glm$pred1000,nfo_glm$NFO_IND,positive = "1")
confusionMatrix(nfo_rf$pred1000,nfo_rf$Actual,positive = "1")

############### Look at the top 0.4% for NFO #################
rbo_glm = mutate(rbo_glm,quantile1000=ntile(rbo_glm$prob,1000))
rbo_rf = mutate(rbo_rf,quantile1000=ntile(rbo_rf$Prob1,1000))

# Assign top 0.4% as NFOs
rbo_glm = mutate(rbo_glm,pred1000=ifelse(rbo_glm$quantile1000>996,1,0))
rbo_rf = mutate(rbo_rf,pred1000=ifelse(rbo_rf$quantile1000>996,1,0))

# Produce confusion matrix
confusionMatrix(rbo_glm$pred1000,rbo_glm$RBO_IND,positive = "1")
confusionMatrix(rbo_rf$pred1000,rbo_rf$Actual,positive = "1")

                
################# Dec. 28th ######################
# Genworth NFO confusion matrix, match the # predicted with # actual
# Read in Genworth's scored data NFO
nfo_glm_Genworth = read.csv("NFO_model_scores_Genworth.csv")
summary(nfo_glm_Genworth)
count(nfo_glm_Genworth$NFO)

nfo_glm_Genworth = nfo_glm_Genworth[order(nfo_glm_Genworth$y_hat,decreasing = T),]
nfo_glm_Genworth$pred_overall = ifelse(nfo_glm_Genworth$y_hat>=nfo_glm_Genworth[2193,]$y_hat,1,0)

confusionMatrix(nfo_glm_Genworth$pred_overall,nfo_glm_Genworth$NFO,positive = "1")


################ Model Finalization ##################
# NFO: 11281
# RBO: 22531

# Generate finalized ROC curve
# Read in scored datasets
nfo_glm_final = read.csv("VAL_SCORED_DECILED_NFO_12281.csv")
rbo_glm_final = read.csv("VAL_SCORED_DECILED_RBO_22531.csv")
nfo_rf = read.csv("RF_NFO.csv")
rbo_rf = read.csv("RF_RBO.csv")

# ROC curve - NFO GLM
nfo_pred = prediction(nfo_glm_final$pred_NFO_12281,nfo_glm_final$NFO_IND)
nfo_roc_final = performance(nfo_pred,measure="tpr",x.measure="fpr")
plot(nfo_roc_final,main="Deloitte ROC Curve - GLM Model",lwd=3,col="light green")
abline(0,1,lty=2,col="red")
nfo_auc_final = performance(nfo_pred,measure="auc")
unlist(nfo_auc_final@y.values) # 0.848

# ROC curve - NFO RF
nfo_pred = prediction(nfo_rf$Prob1,nfo_rf$Actual)
nfo_roc_final = performance(nfo_pred,measure="tpr",x.measure="fpr")
plot(nfo_roc_final,main="Deloitte ROC Curve - Random Forest Model",lwd=3,col="light green")
abline(0,1,lty=2,col="red")
nfo_auc_final = performance(nfo_pred,measure="auc")
unlist(nfo_auc_final@y.values) # 0.904

# ROC curve - RBO GLM
rbo_pred = prediction(rbo_glm_final$pred_RBO_22531,rbo_glm_final$RBO_IND)
rbo_roc_final = performance(rbo_pred,measure="tpr",x.measure="fpr")
plot(rbo_roc_final,main="Deloitte ROC Curve - GLM Model",lwd=3,col="light green")
abline(0,1,lty=2,col="red")
rbo_auc_final = performance(rbo_pred,measure="auc")
unlist(rbo_auc_final@y.values) # 0.754

# ROC curve - RBO RF
rbo_pred = prediction(rbo_rf$Prob1,rbo_rf$Actual)
rbo_roc_final = performance(rbo_pred,measure="tpr",x.measure="fpr")
plot(rbo_roc_final,main="Deloitte ROC Curve - Random Forest Model",lwd=3,col="light green")
abline(0,1,lty=2,col="red")
rbo_auc_final = performance(rbo_pred,measure="auc")
unlist(rbo_auc_final@y.values) # 0.824

## Update the confusion matrix based on the finalized model
# Create probability
nfo_glm_final$prob = exp(nfo_glm_final$pred_NFO_12281)/(1+exp(nfo_glm_final$pred_NFO_12281))
rbo_glm_final$prob = exp(rbo_glm_final$pred_RBO_22531)/(1+exp(rbo_glm_final$pred_RBO_22531))

# Order by prob
nfo_glm_final = nfo_glm_final[order(nfo_glm_final$prob,decreasing = T),]
rbo_glm_final = rbo_glm_final[order(rbo_glm_final$prob,decreasing = T),]
nfo_rf = nfo_rf[order(nfo_rf$Prob1,decreasing = T),]
rbo_rf = rbo_rf[order(rbo_rf$Prob1,decreasing = T),]

# Assign the top 2294 / 5193 as actual
nfo_glm_final$pred_overall = ifelse(nfo_glm_final$prob>=nfo_glm_final[2294,]$prob,1,0)
rbo_glm_final$pred_overall = ifelse(rbo_glm_final$prob>=rbo_glm_final[5193,]$prob,1,0)
nfo_rf$pred_overall = ifelse(nfo_rf$Prob1>=nfo_rf[2294,]$Prob1,1,0)
rbo_rf$pred_overall = ifelse(rbo_rf$Prob1>=rbo_rf[5193,]$Prob1,1,0)

# Produce confusion matrix
confusionMatrix(nfo_glm_final$pred_overall,nfo_glm_final$NFO_IND,positive = "1")
confusionMatrix(rbo_glm_final$pred_overall,rbo_glm_final$RBO_IND,positive = "1")
confusionMatrix(nfo_rf$pred_overall,nfo_rf$Actual,positive = "1")
confusionMatrix(rbo_rf$pred_overall,rbo_rf$Actual,positive = "1")


################### Jan 2nd #######################
# Genworth RBO scored data analysis
# Read in Genworth's scored data NFO
rbo_glm_Genworth = read.csv("rbo_scored_1.csv")
summary(rbo_glm_Genworth)
count(rbo_glm_Genworth$RBO)

# Examine the ROC curve in their scored data
Genworth_pred = prediction(rbo_glm_Genworth$y_hat,rbo_glm_Genworth$RBO)
Genworth_roc = performance(Genworth_pred,measure="tpr",x.measure="fpr")
plot(Genworth_roc,main="Genworth Internal ROC Curve - GBM",lwd=3,col="light blue")
abline(0,1,lty=2,col="red")
Genworth_auc = performance(Genworth_pred,measure="auc")
unlist(Genworth_auc@y.values) # 0.725

plotLift(rbo_glm_Genworth$y_hat,rbo_glm_Genworth$RBO,cumulative=T,n.buckets=10)
TopDecileLift(rbo_glm_Genworth$y_hat,rbo_glm_Genworth$RBO) # 3.34

# Produce threshold 8.1%
rbo_glm_Genworth = rbo_glm_Genworth[order(rbo_glm_Genworth$y_hat_RBO,decreasing=TRUE),]
rbo_glm_Genworth = mutate(rbo_glm_Genworth,
                          RBO_pred=ifelse(rbo_glm_Genworth$y_hat_RBO>=rbo_glm_Genworth[3024,]$y_hat_RBO,1,0))
rbo_glm_Genworth[c(1:3024),"RBO_pred"]=1
rbo_glm_Genworth[c(3025:nrow(rbo_glm_Genworth)),"RBO_pred"]=0
count(rbo_glm_Genworth$RBO_pred)
# Produce confusion matrix
confusionMatrix(rbo_glm_Genworth$RBO_pred,rbo_glm_Genworth$RBO,positive = "1")

# Assign decile number to RBO model
rbo_glm_Genworth = mutate(rbo_glm_Genworth,decile = ntile(rbo_glm_Genworth$y_hat_RBO,10))
rbo_glm_Genworth$decile = 11 - rbo_glm_Genworth$decile
count(rbo_glm_Genworth$decile)
library(data.table)
rbo_glm_Genworth = as.data.table(rbo_glm_Genworth)
rbo_glm_Genworth[,.(pred = sum(RBO)),by=decile]


############### Jan 3 #####################
####### Scored datasets without Summarized Credit variables ########
nfo_glm = read.csv("VAL_SCORED_DECILED_NFO_12281_rmSC.csv")
rbo_glm = read.csv("VAL_SCORED_DECILED_RBO_22531_rmSC.csv")

count(nfo_glm$NFO_IND)
count(rbo_glm$RBO_IND)

nfo_glm = as.data.table(nfo_glm)
rbo_glm = as.data.table(rbo_glm)

nfo_glm[order(score10,decreasing = T),.(.N, pred = sum(NFO_IND)),by=score10]
rbo_glm[order(score10,decreasing = T),.(.N, pred = sum(RBO_IND)),by=score10]

# Update the confusion matrix
nfo_glm = nfo_glm[order(nfo_glm$pred_NFO_12281,decreasing = T),]
rbo_glm = rbo_glm[order(rbo_glm$pred_RBO_22531,decreasing = T),]

nfo_glm$NFO_pred = NA
nfo_glm[c(1:2294),"NFO_pred"]=1
nfo_glm[c(2295:nrow(nfo_glm)),"NFO_pred"]=0
confusionMatrix(nfo_glm$NFO_pred,nfo_glm$NFO_IND)

rbo_glm$RBO_pred = NA
rbo_glm[c(1:5193),"RBO_pred"]=1
rbo_glm[c(5194:nrow(rbo_glm)),"RBO_pred"]=0
confusionMatrix(rbo_glm$RBO_pred,rbo_glm$RBO_IND)



##################### Draw ROC curve on the same chart ######################
nfo_glm = read.csv("VAL_SCORED_DECILED_NFO_12281.csv")
rbo_glm = read.csv("VAL_SCORED_DECILED_RBO_22531.csv")
nfo_rf = read.csv("RF_NFO.csv")
rbo_rf = read.csv("RF_RBO.csv")
nfo_glm_Genworth = read.csv("NFO_model_scores_Genworth.csv")
rbo_glm_Genworth = read.csv("rbo_scored_1.csv")

# ROC curve - NFO GLM
nfo_glm_roc = prediction(nfo_glm$pred_NFO_12281,nfo_glm$NFO_IND)
nfo_glm_roc_curve = performance(nfo_glm_roc,measure="tpr",x.measure="fpr")

nfo_rf_roc = prediction(nfo_rf$Prob1,nfo_rf$Actual)
nfo_rf_roc_curve = performance(nfo_rf_roc,measure="tpr",x.measure="fpr")

Genworth_pred = prediction(nfo_glm_Genworth$y_hat,nfo_glm_Genworth$NFO)
Genworth_roc = performance(Genworth_pred,measure="tpr",x.measure="fpr")

plot(nfo_rf_roc_curve,main="ROC Curve - NFO model",lwd=2,col="dark green")
plot(nfo_glm_roc_curve,lwd=2,col="green",add=TRUE)
plot(Genworth_roc,lwd=2,col="light blue",add=TRUE)
abline(0,1,lty=2,col="red")

# Add legend
legend("bottomright", legend=c("Random Forest - Deloitte", "GLM - Deloitte", "GBM - Genworth"),
       col=c("dark green", "green","light blue"), lty=1,cex=0.8,
       box.lty=1)


# ROC curve - RBO GLM
rbo_glm_roc = prediction(rbo_glm$pred_RBO_22531,rbo_glm$RBO_IND)
rbo_glm_roc_curve = performance(rbo_glm_roc,measure="tpr",x.measure="fpr")

rbo_rf_roc = prediction(rbo_rf$Prob1,rbo_rf$Actual)
rbo_rf_roc_curve = performance(rbo_rf_roc,measure="tpr",x.measure="fpr")

Genworth_pred = prediction(rbo_glm_Genworth$y_hat_RBO,rbo_glm_Genworth$RBO)
Genworth_roc = performance(Genworth_pred,measure="tpr",x.measure="fpr")

plot(rbo_rf_roc_curve,main="ROC Curve - RBO model",lwd=2,col="dark green")
plot(rbo_glm_roc_curve,lwd=2,col="green",add=TRUE)
plot(Genworth_roc,lwd=2,col="light blue",add=TRUE)
abline(0,1,lty=2,col="red")

# Add legend
legend("bottomright", legend=c("Random Forest - Deloitte", "GLM - Deloitte", "GBM - Genworth"),
       col=c("dark green", "green","light blue"), lty=1,cex=0.8,
       box.lty=1)


################ Jan 8th ###################
######### NFO model GLM / RF ###########
# Generate finalized ROC curve
# Read in scored datasets
nfo_glm_final = read.csv("VAL_SCORED_GLM_No_NumDec_NFO.csv")
nfo_rf = read.csv("VAL_RF_SCORED_CENTILE_No_NumDec_NFO.csv")

# ROC curve - NFO GLM
nfo_pred_glm = prediction(nfo_glm_final$prob_NFO,nfo_glm_final$NFO_IND)
nfo_roc_final_glm = performance(nfo_pred_glm,measure="tpr",x.measure="fpr")
plot(nfo_roc_final_glm,main="Deloitte ROC Curve - GLM / RF",lwd=3,col="light green")
abline(0,1,lty=2,col="red")
nfo_auc_final_glm = performance(nfo_pred,measure="auc")
unlist(nfo_auc_final_glm@y.values) # 0.732

# ROC curve - NFO RF
nfo_pred_rf = prediction(nfo_rf$P_NFO_IND1,nfo_rf$NFO_IND)
nfo_roc_final_rf = performance(nfo_pred_rf,measure="tpr",x.measure="fpr")
plot(nfo_roc_final_rf,lwd=3,col="dark green",add=TRUE)
abline(0,1,lty=2,col="red")
nfo_auc_final_rf = performance(nfo_pred_rf,measure="auc")
unlist(nfo_auc_final_rf@y.values) # 0.81
abline(0,1,lty=2,col="red")

# Add legend
legend("bottomright", legend=c("Random Forest - Deloitte", "GLM - Deloitte"),
       col=c("dark green", "green"), lty=1,cex=0.8,
       box.lty=1)

# Order by prob
nfo_glm_final = nfo_glm_final[order(nfo_glm_final$prob_NFO,decreasing = T),]
nfo_rf = nfo_rf[order(nfo_rf$P_NFO_IND1,decreasing = T),]

# Assign the top 2294 / 5193 as actual
nfo_glm_final$pred_overall = ifelse(nfo_glm_final$prob_NFO>=nfo_glm_final[2294,]$prob_NFO,1,0)
nfo_rf$pred_overall = ifelse(nfo_rf$P_NFO_IND1>=nfo_rf[2294,]$P_NFO_IND1,1,0)

nfo_glm_final$pred_overall10 = ifelse(nfo_glm_final$score10>=10,1,0)
nfo_rf$pred_overall10 = ifelse(nfo_rf$score10>=10,1,0)

# Produce confusion matrix
confusionMatrix(nfo_glm_final$pred_overall,nfo_glm_final$NFO_IND,positive = "1")
confusionMatrix(nfo_rf$pred_overall,nfo_rf$NFO_IND,positive = "1")

confusionMatrix(nfo_glm_final$pred_overall10,nfo_glm_final$NFO_IND,positive = "1")
confusionMatrix(nfo_rf$pred_overall10,nfo_rf$NFO_IND,positive = "1")


################ Jan 9th ###################
nfo_glm_old = read.csv("VAL_SCORED_DECILED_NFO_12281.csv")
nfo_rf_old = read.csv("RF_NFO.csv")
nfo_glm_new = read.csv("VAL_SCORED_GLM_No_NumDec_NFO.csv")
nfo_new = read.csv("VAL_RF_SCORED_CENTILE_No_NumDec_NFO.csv")

# ROC curve (compare old to new)
# ROC curve - NFO GLM
nfo_pred_glm_old = prediction(nfo_glm_old$pred_NFO_12281,nfo_glm_old$NFO_IND)
nfo_roc_glm_old = performance(nfo_pred_glm_old,measure="tpr",x.measure="fpr")
plot(nfo_roc_glm_old,main="Deloitte ROC Curve - GLM",lwd=3,col="light green",lty=2)

nfo_pred_glm_new = prediction(nfo_glm_new$prob_NFO,nfo_glm_new$NFO_IND)
nfo_roc_glm_new = performance(nfo_pred_glm_new,measure="tpr",x.measure="fpr")
plot(nfo_roc_glm_new,col="light green",add=T,lwd=3)
abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("GLM - w/  NUM_DECISIONS", "GLM - w/o NUM_DECISIONS"),
       col=c("light green", "light green"), lty=c(2,1),cex=0.8,
       box.lty=1)

# ROC curve - NFO RF
nfo_pred_rf_old = prediction(nfo_rf_old$Prob1,nfo_rf_old$Actual)
nfo_roc_rf_old = performance(nfo_pred_rf_old,measure="tpr",x.measure="fpr")
plot(nfo_roc_rf_old,main="Deloitte ROC Curve - Random Forest",lwd=3,col="dark green",lty=2)

nfo_pred_rf_new = prediction(nfo_new$P_NFO_IND1,nfo_new$NFO_IND)
nfo_roc_rf_new = performance(nfo_pred_rf_new,measure="tpr",x.measure="fpr")
plot(nfo_roc_rf_new,col="dark green",add=T,lwd=3)
abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("RF - w/  NUM_DECISIONS", "RF - w/o NUM_DECISIONS"),
       col=c("dark green", "dark green"), lty=c(2,1),cex=0.8,
       box.lty=1)


############### RBO updated ROC / Confusion Matrix ###################
######### RBO model GLM / RF ###########
# Generate finalized ROC curve
# Read in scored datasets
rbo_glm_new = read.csv("VAL_SCORED_DECILED_RBO_22621_rsplit1.csv")
rbo_rf_new = read.csv("VAL_RF_RBO_SCORED_CENTILE_0106.csv")

# ROC curve - RBO GLM
rbo_pred_glm_new = prediction(rbo_glm_new$pred_RBO_22621,rbo_glm_new$RBO_IND)
rbo_roc_glm_new = performance(rbo_pred_glm_new,measure="tpr",x.measure="fpr")
plot(rbo_roc_glm_new,main="Deloitte ROC Curve - GLM / RF",lwd=3,col="light green")
rbo_auc_glm_new = performance(rbo_pred_glm_new,measure="auc")
unlist(rbo_auc_glm_new@y.values) # 0.73

# ROC curve - RBO RF
rbo_pred_rf_new = prediction(rbo_rf_new$P_RBO_IND1,rbo_rf_new$RBO_IND)
rbo_roc_rf_new = performance(rbo_pred_rf_new,measure="tpr",x.measure="fpr")
plot(rbo_roc_rf_new,lwd=3,col="dark green",add=TRUE)
abline(0,1,lty=2,col="red")
rbo_auc_rf_new = performance(rbo_pred_rf_new,measure="auc")
unlist(rbo_auc_rf_new@y.values) # 
abline(0,1,lty=2,col="red")

# Add legend
legend("bottomright", legend=c("Random Forest - Deloitte", "GLM - Deloitte"),
       col=c("dark green", "green"), lty=1,cex=0.8,
       box.lty=1)

# Order by prob
rbo_glm_new = rbo_glm_new[order(rbo_glm_new$pred_RBO_22621,decreasing = T),]
rbo_rf_new = rbo_rf_new[order(rbo_rf_new$P_RBO_IND1,decreasing = T),]

# Assign the top 5193 as actual
rbo_glm_new$pred_overall = ifelse(rbo_glm_new$pred_RBO_22621>=rbo_glm_new[5193,]$pred_RBO_22621,1,0)
rbo_rf_new$pred_overall = ifelse(rbo_rf_new$P_RBO_IND1>=rbo_rf_new[5193,]$P_RBO_IND1,1,0)

rbo_glm_new$pred_overall10 = ifelse(rbo_glm_new$score10>=10,1,0)
rbo_rf_new$pred_overall10 = ifelse(rbo_rf_new$score10>=10,1,0)

# Produce confusion matrix
confusionMatrix(rbo_glm_new$pred_overall,rbo_glm_new$RBO_IND,positive = "1")
confusionMatrix(rbo_rf_new$pred_overall,rbo_rf_new$RBO_IND,positive = "1")

confusionMatrix(rbo_glm_new$pred_overall10,rbo_glm_new$RBO_IND,positive = "1")
confusionMatrix(rbo_rf_new$pred_overall10,rbo_rf_new$RBO_IND,positive = "1")


rbo_glm_old = read.csv("VAL_SCORED_DECILED_RBO_22531.csv")
rbo_rf_old = read.csv("RF_RBO.csv")
rbo_glm_new = read.csv("VAL_SCORED_DECILED_RBO_22621_copy.csv")
rbo_rf_new = read.csv("VAL_RF_SCORED_CENTILE_No_NumDec_RBO.csv")

# ROC curve (compare old to new)
# ROC curve - RBO GLM
rbo_pred_glm_old = prediction(rbo_glm_old$pred_RBO_22531,rbo_glm_old$RBO_IND)
rbo_roc_glm_old = performance(rbo_pred_glm_old,measure="tpr",x.measure="fpr")
plot(rbo_roc_glm_old,main="Deloitte ROC Curve - GLM",lwd=1,col="light green",lty=2)

rbo_pred_glm_new = prediction(rbo_glm_new$prob_RBO,rbo_glm_new$RBO_IND)
rbo_roc_glm_new = performance(rbo_pred_glm_new,measure="tpr",x.measure="fpr")
plot(rbo_roc_glm_new,col="light green",add=T,lwd=1)
abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("GLM - w/  NUM_DECISIONS", "GLM - w/o NUM_DECISIONS"),
       col=c("light green", "light green"), lty=c(2,1),cex=0.8,
       box.lty=1)

# ROC curve - RBO RF
rbo_pred_rf_old = prediction(rbo_rf_old$Prob1,rbo_rf_old$Actual)
rbo_roc_rf_old = performance(rbo_pred_rf_old,measure="tpr",x.measure="fpr")
plot(rbo_roc_rf_old,main="Deloitte ROC Curve - Random Forest",lwd=1,col="dark green",lty=2)

rbo_pred_rf_new = prediction(rbo_rf_new$P_RBO_IND1,rbo_rf_new$RBO_IND)
rbo_roc_rf_new = performance(rbo_pred_rf_new,measure="tpr",x.measure="fpr")
plot(rbo_roc_rf_new,col="dark green",add=T,lwd=1)
abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("RF - w/  NUM_DECISIONS", "RF - w/o NUM_DECISIONS"),
       col=c("dark green", "dark green"), lty=c(2,1),cex=0.8,
       box.lty=1)



####################### Jan 10 ##################################
# Zeroing out SC variables and External variables
nfo_glm_ext = read.csv("VAL_SCORED_DECILED_NFO_12631_ext.csv")
nfo_glm_wcr = read.csv("VAL_SCORED_DECILED_NFO_12631_wcr.csv")
rbo_glm_ext = read.csv("VAL_SCORED_DECILED_RBO_22631_ext.csv")
rbo_glm_wcr = read.csv("VAL_SCORED_DECILED_RBO_22631_wcr.csv")


# NFO
nfo_pred_glm_wcr = prediction(nfo_glm_wcr$pred_NFO_12631,nfo_glm_wcr$NFO_IND)
nfo_roc_glm_wcr = performance(nfo_pred_glm_wcr,measure="tpr",x.measure="fpr")
plot(nfo_roc_glm_wcr,main="Deloitte ROC Curve - GLM",lwd=1,col="chartreuse3")

nfo_pred_glm_ext = prediction(nfo_glm_ext$pred_NFO_12631,nfo_glm_ext$NFO_IND)
nfo_roc_glm_ext = performance(nfo_pred_glm_ext,measure="tpr",x.measure="fpr")
plot(nfo_roc_glm_ext,lwd=1,col="chartreuse4",add=TRUE)

abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("GLM - w/o  Summarized Credit", "GLM - w/o External"),
       col=c("chartreuse3", "chartreuse4"), lty=c(1,1),cex=0.8,
       box.lty=1)

# RBO
rbo_pred_glm_wcr = prediction(rbo_glm_wcr$pred_RBO_22631,rbo_glm_wcr$RBO_IND)
rbo_roc_glm_wcr = performance(rbo_pred_glm_wcr,measure="tpr",x.measure="fpr")
plot(rbo_roc_glm_wcr,main="Deloitte ROC Curve - GLM",lwd=1,col="chartreuse3")

rbo_pred_glm_ext = prediction(rbo_glm_ext$pred_RBO_22631,rbo_glm_ext$RBO_IND)
rbo_roc_glm_ext = performance(rbo_pred_glm_ext,measure="tpr",x.measure="fpr")
plot(rbo_roc_glm_ext,lwd=1,col="chartreuse4",add=TRUE)

abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("GLM - w/o  Summarized Credit", "GLM - w/o External"),
       col=c("chartreuse3", "chartreuse4"), lty=c(1,1),cex=0.8,
       box.lty=1)


####### Exclude External Variables ########
nfo_rf = read.csv("RF_NFO.csv")
nfo_rf_ext = read.csv("RF_NFO_ext.csv")
rbo_rf = read.csv("RF_RBO.csv")
rbo_rf_ext = read.csv("RF_RBO_ext.csv")

# NFO
nfo_pred_rf = prediction(nfo_rf$P_NFO_IND1,nfo_rf$NFO_IND)
nfo_roc_rf = performance(nfo_pred_rf,measure="tpr",x.measure="fpr")
plot(nfo_roc_rf,main="Deloitte ROC Curve - Random Forest",lwd=1,col="dark green",lty=2)
nfo_auc_rf = performance(nfo_pred_rf,measure="auc")
unlist(nfo_auc_rf@y.values)

nfo_pred_rf_ext = prediction(nfo_rf_ext$P_NFO_IND1,nfo_rf_ext$NFO_IND)
nfo_roc_rf_ext = performance(nfo_pred_rf_ext,measure="tpr",x.measure="fpr")
plot(nfo_roc_rf_ext,lwd=1,col="dark green",add=T)
nfo_auc_rf_ext = performance(nfo_pred_rf_ext,measure="auc")
unlist(nfo_auc_rf_ext@y.values)

abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("RF", "RF - w/o External"),
       col=c("dark green", "dark green"), lty=c(2,1),cex=0.8,
       box.lty=1)

# RBO
rbo_pred_rf = prediction(rbo_rf$P_RBO_IND1,rbo_rf$RBO_IND)
rbo_roc_rf = performance(rbo_pred_rf,measure="tpr",x.measure="fpr")
plot(rbo_roc_rf,main="Deloitte ROC Curve - Random Forest",lwd=1,col="dark green",lty=2)
rbo_auc_rf = performance(rbo_pred_rf,measure="auc")
unlist(rbo_auc_rf@y.values)

rbo_pred_rf_ext = prediction(rbo_rf_ext$P_RBO_IND1,rbo_rf_ext$RBO_IND)
rbo_roc_rf_ext = performance(rbo_pred_rf_ext,measure="tpr",x.measure="fpr")
plot(rbo_roc_rf_ext,lwd=1,col="dark green",add=T)
rbo_auc_rf_ext = performance(rbo_pred_rf_ext,measure="auc")
unlist(rbo_auc_rf_ext@y.values)

abline(0,1,lty=2,col="red")

legend("bottomright", legend=c("RF", "RF - w/o External"),
       col=c("dark green", "dark green"), lty=c(2,1),cex=0.8,
       box.lty=1)

################### Jan 13rd ########################
# Updated NFO model for documentatin 12821
nfo_glm_new = read.csv("../Desktop/VAL_SCORED_GLM_12821.csv")

nfo_pred_glm_new = prediction(nfo_glm_new$pred_NFO_12821,nfo_glm_new$NFO_IND)
nfo_roc_glm_new = performance(nfo_pred_glm_new,measure="tpr",x.measure="fpr")
plot(nfo_roc_glm_new,main="Deloitte ROC Curve - GLM / RF",lwd=3,col="light green")
nfo_auc_glm_new = performance(nfo_pred_glm_new,measure="auc")
unlist(nfo_auc_glm_new@y.values) # 0.73

nfo_glm_new = nfo_glm_new[order(nfo_glm_new$pred_NFO_12821,decreasing = TRUE),]
nfo_glm_new$pred_overall = ifelse(nfo_glm_new$pred_NFO_12821>=nfo_glm_new[2294,]$pred_NFO_12821,1,0)
confusionMatrix(nfo_glm_new$pred_overall,nfo_glm_new$NFO_IND)






