##################### Allvar Missed Dataset #######################
rm(list=ls())
getwd()
setwd("../Desktop/")

library(caret)
library(DMwR)
library(ROCR)
library(lift)
library(PRROC)
library(randomForest)
library(kernlab)
library(dplyr)
library(data.table)
library(ggplot2)
# read in data
allvars = read.csv("allvars_capped_miss1213.csv")

# Check missing values
for (Var in names(allvars)) {
  missing <- sum(is.na(allvars[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

for (x in 1:ncol(allvars)) {
  allvars[is.na(allvars[,x]),x] = median(allvars[,x],na.rm=T)}

set.seed(111)
ind_rbo = createDataPartition(allvars$RBO_IND,p=0.1,list=F)
allvars_train_rbo = allvars[ind_rbo,]
allvars_val_rbo = allvars[-ind_rbo,]
prop.table(table(allvars_train_rbo$RBO_IND))
prop.table(table(allvars_val_rbo$RBO_IND))
prop.table(table(allvars_train_rbo$RATE_INCR))
prop.table(table(allvars_val_rbo$RATE_INCR))

set.seed(112)
ind_nfo = createDataPartition(allvars$NFO_IND,p=0.1,list=F)
allvars_train_nfo = allvars[ind_nfo,]
allvars_val_nfo = allvars[-ind_nfo,]
prop.table(table(allvars_train_nfo$NFO_IND))
prop.table(table(allvars_val_nfo$NFO_IND))
prop.table(table(allvars_train_nfo$RATE_INCR))
prop.table(table(allvars_val_nfo$RATE_INCR))

# NFO
allvars_train_nfo$NFO_IND = as.factor(ifelse(allvars_train_nfo$NFO_IND==0,'non.NFO','NFO'))
ctrl = trainControl(method="cv",number=3,summaryFunction = twoClassSummary,classProbs = T)
set.seed(2223)
system.time(glm_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT-FULL_PAY_IND-CUST_REF,data=allvars_train_nfo,
                             method="gbm",trControl=ctrl,metric="ROC")) #1214s

a = varImp(glm_nfo)
gbm_nfo_imp = data.frame(a$importance)
gbm_nfo_imp = data.frame(variable=rownames(gbm_nfo_imp),gbm_nfo_imp)
str(gbm_nfo_imp)
gbm_nfo_imp = gbm_nfo_imp[order(-gbm_nfo_imp$Overall),]
gbm_nfo_imp_pos = gbm_nfo_imp[gbm_nfo_imp$Overall>0,]
write.csv(gbm_nfo_imp,"gbm_nfo_imp.csv")

# RBO
allvars_train_rbo$RBO_IND = as.factor(ifelse(allvars_train_rbo$RBO_IND==0,'non.RBO','RBO'))
ctrl = trainControl(method="cv",number=3,summaryFunction = twoClassSummary,classProbs = T)
set.seed(2223)
system.time(glm_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT-FULL_PAY_IND-CUST_REF,data=allvars_train_rbo,
                             method="gbm",trControl=ctrl,metric="ROC")) #1214s

a = varImp(glm_rbo)
gbm_rbo_imp = data.frame(a$importance)
gbm_rbo_imp = data.frame(variable=rownames(gbm_rbo_imp),gbm_rbo_imp)
str(gbm_rbo_imp)
gbm_rbo_imp = gbm_rbo_imp[order(-gbm_rbo_imp$Overall),]
gbm_rbo_imp_pos = gbm_rbo_imp[gbm_rbo_imp$Overall>0,]

# output the varImp
write.csv(gbm_imp,"gbm_nfo_imp.csv")
write.csv(gbm_rbo_imp,"gbm_rbo_imp.csv")

# select variables in varImp
b = unique(gbm_rbo_imp_pos$variable)
rbo_SMOTE_select = rbo_SMOTE[,colnames(rbo_SMOTE) %in% b | colnames(rbo_SMOTE) %in% "RBO_IND"]

rbo_SMOTE_select$RBO_IND = as.factor(ifelse(rbo_SMOTE_select$RBO_IND==0,'non.RBO','RBO'))
ctrl = trainControl(method="cv",number=3,summaryFunction = twoClassSummary,classProbs = T)

######################## Filter SMOTE_select #########################
smote_rm = c("ORIG_DBA_AFTR",
             "BIO_AFTER_NO_BIO",
             "FREQ_AFTR_M",
             "STATE_UT",
             "STATE_AK")

rbo_SMOTE_select = rbo_SMOTE_select[,!colnames(rbo_SMOTE_select) %in% smote_rm]

######################## SMOTE filtered data ############################
allvars = allvars_capped_miss1213[,colnames(allvars_capped_miss1213) %in% b | colnames(allvars_capped_miss1213) %in% "RBO_IND"]
allvars = allvars[,!colnames(allvars) %in% smote_rm]

for (Var in names(allvars)) {
  missing <- sum(is.na(allvars[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

# Impute with median
for (x in 1:ncol(allvars)) {
  allvars[is.na(allvars[,x]),x] = median(allvars[,x],na.rm=T)
}

# createDataPartition
set.seed(1218)
ind = createDataPartition(allvars$RBO_IND,p=0.6,list=F)
allvars_train_rbo = allvars[ind,]
allvars_val_rbo = allvars[-ind,]

# SMOTE the data
allvars_train_rbo$RBO_IND = as.factor(allvars_train_rbo$RBO_IND)
set.seed(2221)
system.time(rbo_SMOTE_select <- SMOTE(RBO_IND~.,data=allvars_train_rbo)) #926s
prop.table(table(rbo_SMOTE_select$RBO_IND))

rbo_SMOTE_select$RBO_IND = as.factor(ifelse(rbo_SMOTE_select$RBO_IND==0,'non.RBO','RBO'))

# Add decision tree synthetic variable
allvars_val_rbo$tree_var1 = ifelse(allvars_val_rbo$RATE_INCR<5.5 & 
                                     allvars_val_rbo$CNT_RBO_PREV1<0.5 &
                                     allvars_val_rbo$TOTL_COMMUNICATED_INCR_RATE>=3.5 &
                                     allvars_val_rbo$TOTL_COMMUNICATED_INCR_RATE<5.5 &
                                     allvars_val_rbo$BENE_TRM_BFOR<0.5,1,0)

allvars_val_rbo$tree_var2 = ifelse(allvars_val_rbo$RATE_INCR<5.5 & 
                                     allvars_val_rbo$CNT_RBO_PREV1>=0.5,1,0)

allvars_val_rbo$tree_var3 = ifelse(allvars_val_rbo$RATE_INCR>=5.5 & 
                                     allvars_val_rbo$BENEFIT_POOL_ASSET_RATIO <5.5 &
                                     allvars_val_rbo$BENE_TRM_BFOR<0.5 &
                                     allvars_val_rbo$BENEFIT_POOL_ASSET_RATIO<4.5,1,0)

allvars_val_rbo$tree_var4 = ifelse(allvars_val_rbo$RATE_INCR>=5.5 & 
                                     allvars_val_rbo$BENEFIT_POOL_ASSET_RATIO <5.5 &
                                     allvars_val_rbo$BENE_TRM_BFOR>=0.5 &
                                     allvars_val_rbo$ANNL_PREM_BFOR<7.5 &
                                     allvars_val_rbo$ANNL_PREM_BFOR>=3.5,1,0)

rbo_SMOTE_select$tree_var1 = ifelse(rbo_SMOTE_select$RATE_INCR<5.5 & 
                                      rbo_SMOTE_select$CNT_RBO_PREV1<0.5 &
                                      rbo_SMOTE_select$TOTL_COMMUNICATED_INCR_RATE>=3.5 &
                                      rbo_SMOTE_select$TOTL_COMMUNICATED_INCR_RATE<5.5 &
                                      rbo_SMOTE_select$BENE_TRM_BFOR<0.5,1,0)

rbo_SMOTE_select$tree_var2 = ifelse(rbo_SMOTE_select$RATE_INCR<5.5 & 
                                      rbo_SMOTE_select$CNT_RBO_PREV1>=0.5,1,0)

rbo_SMOTE_select$tree_var3 = ifelse(rbo_SMOTE_select$RATE_INCR>=5.5 & 
                                      rbo_SMOTE_select$BENEFIT_POOL_ASSET_RATIO <5.5 &
                                      rbo_SMOTE_select$BENE_TRM_BFOR<0.5 &
                                      rbo_SMOTE_select$BENEFIT_POOL_ASSET_RATIO<4.5,1,0)

rbo_SMOTE_select$tree_var4 = ifelse(rbo_SMOTE_select$RATE_INCR>=5.5 & 
                                      rbo_SMOTE_select$BENEFIT_POOL_ASSET_RATIO <5.5 &
                                      rbo_SMOTE_select$BENE_TRM_BFOR>=0.5 &
                                      rbo_SMOTE_select$ANNL_PREM_BFOR<7.5 &
                                      rbo_SMOTE_select$ANNL_PREM_BFOR>=3.5,1,0)


#################### GLM ############################
# RBO model
system.time(glm_rbo <- train(as.factor(RBO_IND)~.,data=rbo_SMOTE_select,
                             method="glm",family="binomial",trControl=ctrl,metric="ROC")) # 38s
summary(glm_rbo)
glm_rbo_pred = predict(glm_rbo,newdata=allvars_val_rbo,type="prob")
head(glm_rbo_pred)
tapply(glm_rbo_pred$RBO,allvars_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(glm_rbo_pred$RBO,allvars_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - RBO - GLM", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.75

plotLift(glm_rbo_pred$RBO,allvars_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(glm_rbo_pred$RBO,allvars_val_rbo$RBO_IND) # 3.50

################### CART ############################
system.time(rpart_rbo <- train(as.factor(RBO_IND)~.,data=rbo_SMOTE_select,
                               method="rpart",trControl=ctrl,metric="ROC",tuneLength=25)) # 111s
rpart_rbo
rpart_rbo_pred = predict(rpart_rbo,newdata=allvars_val_rbo,type="prob")
head(rpart_rbo_pred)
tapply(rpart_rbo_pred$RBO,allvars_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(rpart_rbo_pred$RBO,allvars_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - RBO - rpart", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.72

plotLift(rpart_rbo_pred$RBO,allvars_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(rpart_rbo_pred$RBO,allvars_val_rbo$RBO_IND) # 3.45

################# NNET ########################
system.time(nnet_rbo <- train(as.factor(RBO_IND)~.,data=rbo_SMOTE_select,
                              method="nnet",trControl=ctrl,metric="ROC")) # 493s 
nnet_rbo
nnet_rbo_pred = predict(nnet_rbo,newdata=allvars_val_rbo,type="prob")
head(nnet_rbo_pred)
tapply(nnet_rbo_pred$RBO,allvars_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(nnet_rbo_pred$RBO,allvars_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - RBO - nnet", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.70

plotLift(nnet_rbo_pred$RBO,allvars_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(nnet_rbo_pred$RBO,allvars_val_rbo$RBO_IND) # 2.12

########################## GBM #################################
system.time(gbm_rbo <- train(as.factor(RBO_IND)~.,data=rbo_SMOTE_select,
                             method="gbm",trControl=ctrl,metric="ROC")) # 454s
gbm_rbo
gbm_rbo_pred = predict(gbm_rbo,newdata=allvars_val_rbo,type="prob")
head(gbm_rbo_pred)
tapply(gbm_rbo_pred$RBO,allvars_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(gbm_rbo_pred$RBO,allvars_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - RBO - gbm", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.77

plotLift(gbm_rbo_pred$RBO,allvars_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(gbm_rbo_pred$RBO,allvars_val_rbo$RBO_IND) # 3.76


######################### cTree ##########################
# RBO
system.time(ctree_rbo <- train(as.factor(RBO_IND)~.,data=rbo_SMOTE_select,
                               method="ctree",trControl=ctrl,metric="ROC")) # 
ctree_rbo
ctree_rbo_pred = predict(ctree_rbo,newdata=allvars_val_rbo,type="prob")
head(ctree_rbo_pred)
tapply(ctree_rbo_pred$RBO,allvars_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(ctree_rbo_pred$RBO,allvars_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 

plotLift(ctree_rbo_pred$RBO,allvars_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(ctree_rbo_pred$RBO,allvars_val_rbo$RBO_IND) # 
