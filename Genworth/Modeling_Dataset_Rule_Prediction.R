rm(list=ls())
gc()
getwd()
setwd("../Desktop/")

library(caret)
library(dplyr)
################# Genworth's Random Forest Rule ##################
NFO_0126_02 = read.csv("NFO_0126_02.csv")
RBO_0126_03 = read.csv("RBO_0126_03.csv")

quantile(NFO_0126_02$Prob_NFO)
quantile(RBO_0126_03$Prob_RBO)

# merge the two scored datasets into one
RF_merge = merge(NFO_0126_02,RBO_0126_03,by=c("plcy_ref","eff_dt"),all=T)

# Assign top 2.68% to be NFOs based on NFO prob
RF_merge = RF_merge[order(RF_merge$Prob_NFO,decreasing = T),]
RF_merge = mutate(RF_merge,centile_NFO = ntile(RF_merge$Prob_NFO,100))
RF_merge$NFO_pred_class = ifelse(RF_merge$centile_NFO>97,"NFO","non.NFO")

# Assign top 6.02% to be RBOs based on RBO prob
RF_merge = RF_merge[order(RF_merge$Prob_RBO,decreasing = T),]
RF_merge = mutate(RF_merge,centile_RBO = ntile(RF_merge$Prob_RBO,100))
RF_merge$RBO_pred_class = ifelse(RF_merge$centile_RBO>93,"RBO","non.RBO")

# Assign the "rule"
RF_merge$pred_class = ifelse(RF_merge$NFO_pred_class == "NFO" & RF_merge$RBO_pred_class != "RBO","NFO",
                             ifelse(RF_merge$NFO_pred_class != "NFO" & RF_merge$RBO_pred_class == "RBO","RBO",
                                    ifelse(RF_merge$NFO_pred_class == "NFO" & RF_merge$RBO_pred_class == "RBO","Overlap","Full Pay")))
prop.table(table(RF_merge$pred_class))

RF_merge$pred_class_1 = ifelse(RF_merge$pred_class %in% c("NFO","RBO","Full Pay"),RF_merge$pred_class,
                               ifelse(RF_merge$pred_class=="Overlap" & RF_merge$Prob_NFO<0.0268 & RF_merge$Prob_RBO<0.0602,"Full Pay",
                                      ifelse(RF_merge$pred_class=="Overlap" & RF_merge$Prob_NFO<0.0268 & RF_merge$Prob_RBO>=0.0602,"RBO",
                                             ifelse(RF_merge$pred_class=="Overlap" & RF_merge$Prob_NFO>=0.0268 & RF_merge$Prob_RBO<0.0602,"NFO",
                                                    ifelse(RF_merge$Prob_NFO>=0.0268 & RF_merge$Prob_RBO>=0.0602 & 
                                                             RF_merge$Prob_NFO/0.03 - 1 > RF_merge$Prob_RBO/0.08-1
                                                           , "NFO","RBO")))))
prop.table(table(RF_merge$pred_class_1))

# Create actuall Label
RF_merge$response = ifelse(RF_merge$Actual.x==1,"NFO",ifelse(RF_merge$Actual.y==1,"RBO","Full Pay"))
prop.table(table(RF_merge$response))
prop.table(table(RF_merge$pred_class_1))


# confusion matrix
confusionMatrix(RF_merge$pred_class_1,RF_merge$response)