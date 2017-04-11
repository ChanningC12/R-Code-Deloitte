################## Overlapping Prediction in NFO / RBO GLM and RF Model ##################
rm(list=ls())
gc()
getwd()
setwd("../Desktop/SCORED DATA_NUM_DECISIONS/")

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

# Read in data
nfo_glm = read.csv("GLM_NFO.csv")
nfo_rf = read.csv("RF_NFO.csv")
rbo_glm = read.csv("GLM_RBO_22621.csv")
rbo_rf = read.csv("RF_RBO.csv")

# Merge the datasets
nfo_all = merge(nfo_glm,nfo_rf,by=c("PLCY_REF","EFF_DT"),all = T)
# Examine if the merging process is correct
sum(nfo_all$NFO_IND.x-nfo_all$NFO_IND.y)

rbo_all = merge(rbo_glm,rbo_rf,by=c("PLCY_REF","EFF_DT"),all = T)
sum(rbo_all$RBO_IND.x-rbo_all$RBO_IND.y)


# Assign prediction indicator to the model
# Order by decreasing probability
nfo_all = nfo_all[order(nfo_all$prob_NFO,decreasing = T),]
nfo_all$NFO_pred_glm = ifelse(nfo_all$prob_NFO>=nfo_all[2294,]$prob_NFO,1,0)
nfo_all = nfo_all[order(nfo_all$P_NFO_IND1,decreasing = T),]
nfo_all$NFO_pred_rf = ifelse(nfo_all$P_NFO_IND1>=nfo_all[2294,]$P_NFO_IND1,1,0)

# Cross tab
table(nfo_all$NFO_pred_glm,nfo_all$NFO_IND.x)
table(nfo_all$NFO_pred_rf,nfo_all$NFO_IND.y)
table(nfo_all$NFO_pred_glm,nfo_all$NFO_pred_rf)

# create overlapped indicator
nfo_all$NFO_pred_overlap = ifelse(nfo_all$NFO_pred_glm+nfo_all$NFO_pred_rf>1,1,0)
count(nfo_all$NFO_pred_overlap)
table(nfo_all$NFO_pred_overlap,nfo_all$NFO_IND.x)


# Same process for RBO
# Order by decreasing probability
rbo_all = rbo_all[order(rbo_all$pred_RBO_22621,decreasing = T),]
rbo_all$RBO_pred_glm = ifelse(rbo_all$pred_RBO_22621>=rbo_all[5193,]$pred_RBO_22621,1,0)
count(rbo_all$RBO_pred_glm)
rbo_all = rbo_all[order(rbo_all$P_RBO_IND1,decreasing = T),]
rbo_all$RBO_pred_rf = ifelse(rbo_all$P_RBO_IND1>=rbo_all[5193,]$P_RBO_IND1,1,0)
count(rbo_all$RBO_pred_rf)

# Cross tab
table(rbo_all$RBO_pred_glm,rbo_all$RBO_IND.x)
table(rbo_all$RBO_pred_rf,rbo_all$RBO_IND.y)
table(rbo_all$RBO_pred_glm,rbo_all$RBO_pred_rf)

# create overlapped indicator
rbo_all$RBO_pred_overlap = ifelse(rbo_all$RBO_pred_glm+rbo_all$RBO_pred_rf>1,1,0)
count(rbo_all$RBO_pred_overlap)
table(rbo_all$RBO_pred_overlap,rbo_all$RBO_IND.x)











