############### RBO updated ROC / Confusion Matrix ###################
# RBO model for Random Forest 
rm(list=ls())
gc()
getwd()
setwd("../Desktop/")

library(caret)
library(ROCR)

# Generate finalized ROC curve
# Read in scored datasets
rbo_rf_0120_06 = read.csv("RF_RBO_0120_06.csv") # scored data for min size 100
rbo_rf_0120_07 = read.csv("RF_RBO_0120_07.csv") # scored data for min size 20
rbo_rf_0120_08 = read.csv("RF_RBO_0120_08.csv") # scored data for min size 50

# ROC curve - RBO RF 0120_06
# Get the prediction based on predicted probability and actual RBO
rbo_pred_0120_06 = prediction(rbo_rf_0120_06$Prob_RBO,rbo_rf_0120_06$Actual)
# Get performance metrics of ROC based on trp (true positive rate) and fpr (false positive rate)
rbo_roc_0120_06 = performance(rbo_pred_0120_06,measure="tpr",x.measure="fpr")
# Plot ROC curve
plot(rbo_roc_0120_06,main="Deloitte ROC Curve - Random Forest",lwd=1,col="dark green")
# Get AUC value
rbo_auc_0120_06 = performance(rbo_pred_0120_06,measure="auc")
unlist(rbo_auc_0120_06@y.values) # 0.781

# ROC curve - RBO RF 0120_07
# Get the prediction based on predicted probability and actual RBO
rbo_pred_0120_07 = prediction(rbo_rf_0120_07$Prob_RBO,rbo_rf_0120_07$Actual)
# Get performance metrics of ROC based on trp (true positive rate) and fpr (false positive rate)
rbo_roc_0120_07 = performance(rbo_pred_0120_07,measure="tpr",x.measure="fpr")
# Plot ROC curve, add=T will add the curve on the current plot
plot(rbo_roc_0120_07,lwd=1,col="light green",add=TRUE)
# Get AUC value
rbo_auc_0120_07 = performance(rbo_pred_0120_07,measure="auc")
unlist(rbo_auc_0120_07@y.values) # 0.811

# ROC curve - RBO RF 0120_08
# Get the prediction based on predicted probability and actual RBO
rbo_pred_0120_08 = prediction(rbo_rf_0120_08$Prob_RBO,rbo_rf_0120_08$Actual)
# Get performance metrics of ROC based on trp (true positive rate) and fpr (false positive rate)
rbo_roc_0120_08 = performance(rbo_pred_0120_08,measure="tpr",x.measure="fpr")
# Plot ROC curve, add=T will add the curve on the current plot
plot(rbo_roc_0120_08,lwd=1,col="green",add=TRUE)
# Get AUC value
rbo_auc_0120_08 = performance(rbo_pred_0120_08,measure="auc")
unlist(rbo_auc_0120_08@y.values) # 0.796

# Add the reference line
abline(0,1,lty=2,col="red")

# Add legend
legend("bottomright", legend=c("Random Forest - min size 20",
                               "Random Forest - min size 50",
                               "Random Forest - min size 100"),
       col=c("light green","green", "dark green"), lty=1,cex=0.8,
       box.lty=1)



######### Confusion Matrix ###########
# Order by RBO prob
rbo_rf_0120_06 = rbo_rf_0120_06[order(rbo_rf_0120_06$Prob_RBO,decreasing = T),]
rbo_rf_0120_07 = rbo_rf_0120_07[order(rbo_rf_0120_07$Prob_RBO,decreasing = T),]
rbo_rf_0120_08 = rbo_rf_0120_08[order(rbo_rf_0120_08$Prob_RBO,decreasing = T),]


# Assign the top 5193 as actual
rbo_glm_new$pred_overall = ifelse(rbo_glm_new$pred_RBO_22621>=rbo_glm_new[5193,]$pred_RBO_22621,1,0)
rbo_rf_new$pred_overall = ifelse(rbo_rf_new$P_RBO_IND1>=rbo_rf_new[5193,]$P_RBO_IND1,1,0)

rbo_rf_0120_06$pred = ifelse(rbo_rf_0120_06$Prob_RBO>=rbo_rf_0120_06[5193,]$Prob_RBO,1,0)
rbo_rf_0120_07$pred = ifelse(rbo_rf_0120_07$Prob_RBO>=rbo_rf_0120_07[5193,]$Prob_RBO,1,0)
rbo_rf_0120_08$pred = ifelse(rbo_rf_0120_08$Prob_RBO>=rbo_rf_0120_08[5193,]$Prob_RBO,1,0)

# Produce confusion matrix
confusionMatrix(rbo_rf_0120_06$pred,rbo_rf_0120_06$Actual,positive = "1")
confusionMatrix(rbo_rf_0120_07$pred,rbo_rf_0120_07$Actual,positive = "1")
confusionMatrix(rbo_rf_0120_08$pred,rbo_rf_0120_08$Actual,positive = "1")


################# Jan 25th ######################
# Finalized model for NFO and RBO (0125_01)
rbo_rf_0125_01 = read.csv("RBO_0125_01.csv")

# ROC curve - RBO RF 0125_01
# Get the prediction based on predicted probability and actual RBO
rbo_pred_0125_01 = prediction(rbo_rf_0125_01$Prob_RBO,rbo_rf_0125_01$Actual)
# Get performance metrics of ROC based on trp (true positive rate) and fpr (false positive rate)
rbo_roc_0125_01 = performance(rbo_pred_0125_01,measure="tpr",x.measure="fpr")
# Plot ROC curve
plot(rbo_roc_0125_01,main="Deloitte ROC Curve - Random Forest",lwd=1,col="dark green")
# Add the reference line
abline(0,1,lty=2,col="red")
# Get AUC value
rbo_auc_0125_01 = performance(rbo_pred_0125_01,measure="auc")
unlist(rbo_auc_0125_01@y.values) # 0.813

######### Confusion Matrix ###########
# Order by RBO prob
rbo_rf_0125_01 = rbo_rf_0125_01[order(rbo_rf_0125_01$Prob_RBO,decreasing = T),]
rbo_rf_0125_01$pred = ifelse(rbo_rf_0125_01$Prob_RBO>=rbo_rf_0125_01[5193,]$Prob_RBO,1,0)
confusionMatrix(rbo_rf_0125_01$pred,rbo_rf_0125_01$Actual,positive = "1")

################# Jan 26th ######################
# Finalized model for NFO and RBO (0126_02,0126_03)
rbo_rf_0126_03 = read.csv("../../../RBO_0126_03.csv")

# ROC curve - RBO RF 0126_03
# Get the prediction based on predicted probability and actual RBO
rbo_pred_0126_03 = prediction(rbo_rf_0126_03$Prob_RBO,rbo_rf_0126_03$Actual)
# Get performance metrics of ROC based on trp (true positive rate) and fpr (false positive rate)
rbo_roc_0126_03 = performance(rbo_pred_0126_03,measure="tpr",x.measure="fpr")
# Plot ROC curve
plot(rbo_roc_0126_03,main="Deloitte ROC Curve - Random Forest",lwd=1,col="dark green")
# Add the reference line
abline(0,1,lty=2,col="red")
# Get AUC value
rbo_auc_0126_03 = performance(rbo_pred_0126_03,measure="auc")
unlist(rbo_auc_0126_03@y.values) # 0.812

######### Confusion Matrix ###########
# Order by RBO prob
rbo_rf_0126_03 = rbo_rf_0126_03[order(rbo_rf_0126_03$Prob_RBO,decreasing = T),]
rbo_rf_0126_03$pred = ifelse(rbo_rf_0126_03$Prob_RBO>=rbo_rf_0126_03[5193,]$Prob_RBO,1,0)
confusionMatrix(rbo_rf_0126_03$pred,rbo_rf_0126_03$Actual,positive = "1")

####### Extract TP/FP/FN #########
rbo_rf_0126_03_TP = rbo_rf_0126_03[rbo_rf_0126_03$pred==1 & rbo_rf_0126_03$Actual==1,]
rbo_rf_0126_03_FP = rbo_rf_0126_03[rbo_rf_0126_03$pred==1 & rbo_rf_0126_03$Actual==0,]
rbo_rf_0126_03_FN = rbo_rf_0126_03[rbo_rf_0126_03$pred==0 & rbo_rf_0126_03$Actual==1,]

rbo_rf_0126_03$RBO_CM = ifelse(rbo_rf_0126_03$pred==1 & rbo_rf_0126_03$Actual==1,"TP",
                               ifelse(rbo_rf_0126_03$pred==1 & rbo_rf_0126_03$Actual==0,"FP",
                                      ifelse(rbo_rf_0126_03$pred==0 & rbo_rf_0126_03$Actual==1,"FN","TN")))
table(rbo_rf_0126_03$RBO_CM)

write.csv(rbo_rf_0126_03_TP,"../../../rbo_rf_0126_03_TP.csv")
write.csv(rbo_rf_0126_03_FP,"../../../rbo_rf_0126_03_FP.csv")
write.csv(rbo_rf_0126_03_FN,"../../../rbo_rf_0126_03_FN.csv")

# NFO 0126_02
nfo_rf_0126_02 = read.csv("../../../NFO_0126_02.csv")

# ROC curve - NFO RF 0126_02
# Get the prediction based on predicted probability and actual NFO
nfo_pred_0126_02 = prediction(nfo_rf_0126_02$Prob_NFO,nfo_rf_0126_02$Actual)
# Get performance metrics of ROC based on trp (true positive rate) and fpr (false positive rate)
nfo_roc_0126_02 = performance(nfo_pred_0126_02,measure="tpr",x.measure="fpr")
# Plot ROC curve
plot(nfo_roc_0126_02,main="Deloitte ROC Curve - Random Forest",lwd=1,col="dark green")
# Add the reference line
abline(0,1,lty=2,col="red")
# Get AUC value
nfo_auc_0126_02 = performance(nfo_pred_0126_02,measure="auc")
unlist(nfo_auc_0126_02@y.values) # 0.792

######### Confusion Matrix ###########
# Order by NFO prob
nfo_rf_0126_02 = nfo_rf_0126_02[order(nfo_rf_0126_02$Prob_NFO,decreasing = T),]
nfo_rf_0126_02$pred = ifelse(nfo_rf_0126_02$Prob_NFO>=nfo_rf_0126_02[2294,]$Prob_NFO,1,0)
confusionMatrix(nfo_rf_0126_02$pred,nfo_rf_0126_02$Actual,positive = "1")

nfo_rf_0126_02_TP = nfo_rf_0126_02[nfo_rf_0126_02$pred==1 & nfo_rf_0126_02$Actual==1,]
nfo_rf_0126_02_FP = nfo_rf_0126_02[nfo_rf_0126_02$pred==1 & nfo_rf_0126_02$Actual==0,]
nfo_rf_0126_02_FN = nfo_rf_0126_02[nfo_rf_0126_02$pred==0 & nfo_rf_0126_02$Actual==1,]

nfo_rf_0126_02$NFO_CM = ifelse(nfo_rf_0126_02$pred==1 & nfo_rf_0126_02$Actual==1,"TP",
                               ifelse(nfo_rf_0126_02$pred==1 & nfo_rf_0126_02$Actual==0,"FP",
                                      ifelse(nfo_rf_0126_02$pred==0 & nfo_rf_0126_02$Actual==1,"FN","TN")))
table(nfo_rf_0126_02$NFO_CM)

write.csv(nfo_rf_0126_02_TP,"../../../nfo_rf_0126_02_TP.csv")
write.csv(nfo_rf_0126_02_FP,"../../../nfo_rf_0126_02_FP.csv")
write.csv(nfo_rf_0126_02_FN,"../../../nfo_rf_0126_02_FN.csv")

cm = merge(rbo_rf_0126_03[,c(1,2,9)],nfo_rf_0126_02[,c(1,2,9)],by=c("plcy_ref","eff_dt"),all=T)
str(cm)

