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

# read in data
allvars = read.csv("modeling_data_final_0106.csv")
allvars$PC_DS_NEO1_RBO = NULL

allvars_NFO_train = allvars[allvars$RSPLIT1=="TRN"| allvars$RSPLIT1=="TST",]
prop.table(table(allvars_NFO_train$NFO_IND))

## A small example with a data set created artificially from the IRIS
## data 
data(iris)
iris <- iris[, c(1, 2, 5)]
table(iris$Species)
iris$Species <- factor(ifelse(iris$Species == "setosa","rare","common")) 
iris$Species2 <- NA
iris[1:146,]$Species2="common"
iris[147:150,]$Species2="rare"
iris$Species2=as.factor(iris$Species2)
## checking the class distribution of this artificial data set
prop.table(table(iris$Species2))
iris$Species=NULL
newData <- SMOTE(Species2 ~ ., iris, perc.over = 50,perc.under=1500)
table(newData$Species2)


############### SMOTE for NFO (automated) ###############
allvars$NFO_IND = as.factor(ifelse(allvars$NFO_IND==0,'non.NFO','NFO'))
allvars_NFO_train$NFO_IND = as.factor(ifelse(allvars_NFO_train$NFO_IND==0,'non.NFO','NFO'))
table(allvars$NFO_IND)
table(allvars_NFO_train$NFO_IND)

set.seed(12191)
system.time(nfo_SMOTE <- SMOTE(NFO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                               -RESPONSE_TARGET,data=allvars))
prop.table(table(nfo_SMOTE$NFO_IND))
write.csv(nfo_SMOTE,"nfo_SMOTE.csv")

############## SMOTE for NFO (50%-50%) ###############
set.seed(12192)
# system.time: 5762s
system.time(nfo_SMOTE <- SMOTE(NFO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                               -RESPONSE_TARGET,data=allvars,perc.over = 124,perc.under = 200))
prop.table(table(nfo_SMOTE$NFO_IND))

############## SMOTE for NFO (~10%) ###############
set.seed(12193)
# system.time: 2779s
system.time(nfo_SMOTE <- SMOTE(NFO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                               -RESPONSE_TARGET,data=allvars_NFO_train,perc.over = 100,perc.under = 1800))
prop.table(table(nfo_SMOTE$NFO_IND))
nfo_SMOTE$NFO_IND = ifelse(nfo_SMOTE=="NFO",1,0)
write.csv(nfo_SMOTE,"nfo_SMOTE_10.csv",row.names = F)

############## SMOTE for NFO (~6%) ###############
set.seed(12194)
# system.time: 2954s
system.time(nfo_SMOTE <- SMOTE(NFO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                               -RESPONSE_TARGET,data=allvars_NFO_train,perc.over = 100,perc.under = 3000))
prop.table(table(nfo_SMOTE$NFO_IND))
nfo_SMOTE$NFO_IND = ifelse(nfo_SMOTE$NFO_IND=="NFO",1,0)
write.csv(nfo_SMOTE,"nfo_SMOTE_6.csv",row.names = F)

############## SMOTE for NFO (~50%) ###############
set.seed(12195)
# system.time: 3307s
system.time(nfo_SMOTE <- SMOTE(NFO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                               -RESPONSE_TARGET,data=allvars_NFO_train,perc.over = 100,perc.under = 200))
prop.table(table(nfo_SMOTE$NFO_IND))
nfo_SMOTE$NFO_IND = ifelse(nfo_SMOTE$NFO_IND=="NFO",1,0)
write.csv(nfo_SMOTE,"nfo_SMOTE_50.csv",row.names = F)

############## SMOTE for NFO (~12%) ###############
set.seed(12196)
# system.time: 3370s
system.time(nfo_SMOTE <- SMOTE(NFO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                               -RESPONSE_TARGET,data=allvars_NFO_train,perc.over = 100,perc.under = 1400))
prop.table(table(nfo_SMOTE$NFO_IND))
nfo_SMOTE$NFO_IND = ifelse(nfo_SMOTE$NFO_IND=="NFO",1,0)
write.csv(nfo_SMOTE,"nfo_SMOTE_12.csv",row.names = F)


############# SMOTE for RBO ###############
prop.table(table(allvars_NFO_train$RBO_IND))
set.seed(12197)
# system.time: 3370s
system.time(rbo_SMOTE_50 <- SMOTE(RBO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                               -RESPONSE_TARGET,data=allvars_NFO_train,perc.over = 100,perc.under = 200))
prop.table(table(rbo_SMOTE_50$RBO_IND))

set.seed(12198)
# system.time: 3370s
system.time(rbo_SMOTE_15 <- SMOTE(RBO_IND~.-RSPLIT1-RSPLIT2-RSPLIT3-RSPLIT4-RSPLIT5
                                  -RESPONSE_TARGET,data=allvars_NFO_train,perc.over = 100,perc.under = 1000))
prop.table(table(rbo_SMOTE_15$RBO_IND))














