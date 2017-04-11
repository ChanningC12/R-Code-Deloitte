rm(list=ls())
gc()
getwd()
setwd("../Desktop/")

library(caret)
library(ROCR)

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
nfo_glm_pred = prediction(nfo_glm_final$pred_NFO_12281,nfo_glm_final$NFO_IND)
nfo_roc_glm_final = performance(nfo_glm_pred,measure="tpr",x.measure="fpr")
plot(nfo_roc_glm_final,main="Deloitte ROC Curve - GLM Model",lwd=3,col="light green")
abline(0,1,lty=2,col="red")
# AUC for NFO GLM model
nfo_auc_glm_final = performance(nfo_glm_pred,measure="auc")
unlist(nfo_auc_glm_final@y.values)

# ROC curve - NFO RF
nfo_rf_pred = prediction(nfo_rf$Prob1,nfo_rf$Actual)
nfo_roc_rf_final = performance(nfo_rf_pred,measure="tpr",x.measure="fpr")
plot(nfo_roc_rf_final,main="Deloitte ROC Curve - Random Forest Model",lwd=3,col="dark green")
abline(0,1,lty=2,col="red")
# AUC for NFO RF model
nfo_auc_rf_final = performance(nfo_rf_pred,measure="auc")
unlist(nfo_auc_rf_final@y.values)

# ROC curve - RBO GLM
rbo_glm_pred = prediction(rbo_glm_final$pred_RBO_22531,rbo_glm_final$RBO_IND)
rbo_roc_glm_final = performance(rbo_glm_pred,measure="tpr",x.measure="fpr")
plot(rbo_roc_glm_final,main="Deloitte ROC Curve - GLM Model",lwd=3,col="light green")
abline(0,1,lty=2,col="red")
# AUC for RBO GLM model
rbo_auc_glm_final = performance(rbo_glm_pred,measure="auc")
unlist(rbo_auc_glm_final@y.values) 

# ROC curve - RBO RF
rbo_rf_pred = prediction(rbo_rf$Prob1,rbo_rf$Actual)
rbo_roc_rf_final = performance(rbo_rf_pred,measure="tpr",x.measure="fpr")
plot(rbo_roc_rf_final,main="Deloitte ROC Curve - Random Forest Model",lwd=3,col="dark green")
abline(0,1,lty=2,col="red")
# AUC for RBO RF model
rbo_auc_rf_final = performance(rbo_rf_pred,measure="auc")
unlist(rbo_auc_rf_final@y.values) 

#################### Confusion Matrix ######################
# Create probability
nfo_glm_final$prob = exp(nfo_glm_final$pred_NFO_12281)/(1+exp(nfo_glm_final$pred_NFO_12281))
rbo_glm_final$prob = exp(rbo_glm_final$pred_RBO_22531)/(1+exp(rbo_glm_final$pred_RBO_22531))

# Order by decreasing probability
nfo_glm_final = nfo_glm_final[order(nfo_glm_final$prob,decreasing = T),]
rbo_glm_final = rbo_glm_final[order(rbo_glm_final$prob,decreasing = T),]
nfo_rf = nfo_rf[order(nfo_rf$Prob1,decreasing = T),]
rbo_rf = rbo_rf[order(rbo_rf$Prob1,decreasing = T),]

# Assign the top 2294 (NFO) / 5193 (RBO) as actual
nfo_glm_final$pred_overall = ifelse(nfo_glm_final$prob>=nfo_glm_final[2294,]$prob,1,0)
rbo_glm_final$pred_overall = ifelse(rbo_glm_final$prob>=rbo_glm_final[5193,]$prob,1,0)
nfo_rf$pred_overall = ifelse(nfo_rf$Prob1>=nfo_rf[2294,]$Prob1,1,0)
rbo_rf$pred_overall = ifelse(rbo_rf$Prob1>=rbo_rf[5193,]$Prob1,1,0)

# Produce confusion matrix
confusionMatrix(nfo_glm_final$pred_overall,nfo_glm_final$NFO_IND,positive = "1")
confusionMatrix(rbo_glm_final$pred_overall,rbo_glm_final$RBO_IND,positive = "1")
confusionMatrix(nfo_rf$pred_overall,nfo_rf$Actual,positive = "1")
confusionMatrix(rbo_rf$pred_overall,rbo_rf$Actual,positive = "1")
