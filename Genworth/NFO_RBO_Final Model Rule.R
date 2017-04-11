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

# read in scored data
nfo_glm_final = read.csv("VAL_SCORED_GLM_12821.csv")
rbo_glm_final = read.csv("SCORED DATA_NUM_DECISIONS/GLM_RBO_22621.csv")

# create probability for NFO and RBO
nfo_glm_final$prob = exp(nfo_glm_final$pred_NFO_12821)/(1+exp(nfo_glm_final$pred_NFO_12821))
quantile(nfo_glm_final$prob)

rbo_glm_final$prob = exp(rbo_glm_final$pred_RBO_22621)/(1+exp(rbo_glm_final$pred_RBO_22621))
quantile(rbo_glm_final$prob)

# merge the two scored datasets into one
rbo_glm_final$EFF_DT = toupper(rbo_glm_final$EFF_DT)
glm_merge = merge(nfo_glm_final,rbo_glm_final,by=c("PLCY_REF","EFF_DT"),all=T)

# Assign top 2.68% to be NFOs based on NFO prob
glm_merge = glm_merge[order(glm_merge$prob.x,decreasing = T),]
glm_merge = mutate(glm_merge,centile_NFO = ntile(glm_merge$prob.x,100))
glm_merge$NFO_pred_class = ifelse(glm_merge$centile_NFO>97,"NFO","non.NFO")

# Assign top 6.02% to be RBOs based on RBO prob
glm_merge = glm_merge[order(glm_merge$prob.y,decreasing = T),]
glm_merge = mutate(glm_merge,centile_RBO = ntile(glm_merge$prob.y,100))
glm_merge$RBO_pred_class = ifelse(glm_merge$centile_RBO>93,"RBO","non.RBO")

# Assign the rule
glm_merge$pred_class = ifelse(glm_merge$NFO_pred_class=="NFO" & glm_merge$RBO_pred_class=="non.RBO","NFO",
                              ifelse(glm_merge$NFO_pred_class=="non.NFO" & glm_merge$RBO_pred_class=="RBO","RBO",
                              ifelse(glm_merge$NFO_pred_class=="non.NFO" & glm_merge$RBO_pred_class=="non.RBO","Full Pay",
                              ifelse(glm_merge$NFO_pred_class=="NFO" & glm_merge$RBO_pred_class=="RBO" &
                                       glm_merge$prob.x/0.03 - 1 > glm_merge$prob.y/0.07-1,"NFO","RBO"))))

# Assign the "rule"
glm_merge$pred_class = ifelse(glm_merge$prob.x<0.0268 & glm_merge$prob.y<0.0602,"Full Pay",
                              ifelse(glm_merge$prob.x<0.0268 & glm_merge>=0.0602,"RBO",
                                     ifelse(glm_merge$prob.x>=0.0268 & glm_merge$prob.y<0.0602,"NFO",
                                            ifelse(glm_merge$prob.x>=0.0268 & glm_merge$prob.y>=0.0602 & 
                                                     glm_merge$prob.x/0.0268 - 1 > glm_merge$prob.y/0.0602-1
                                                   , "NFO","RBO"))))
# Create actuall Label
glm_merge$response = ifelse(glm_merge$NFO_IND==1,"NFO",ifelse(glm_merge$RBO_IND==1,"RBO","Full Pay"))
prop.table(table(glm_merge$response))
prop.table(table(glm_merge$pred_class))


# confusion matrix
confusionMatrix(glm_merge$pred_class,glm_merge$response)




################# Genworth's 3*3 model based on Deloitte's rule #################

# merge the two scored datasets into one
glm_Genworth_merge = merge(nfo_glm_Genworth,rbo_glm_Genworth,by=c("PLCY_REF","EFF_DT"),all=T)

# Assign top 2.68% to be NFOs based on NFO prob
glm_merge = glm_merge[order(glm_merge$prob.x,decreasing = T),]
glm_merge = mutate(glm_merge,centile_NFO = ntile(glm_merge$prob.x,100))
glm_merge$NFO_pred_class = ifelse(glm_merge$centile_NFO>97,"NFO","non.NFO")

# Assign top 6.02% to be RBOs based on RBO prob162+
glm_merge = glm_merge[order(glm_merge$prob.y,decreasing = T),]
glm_merge = mutate(glm_merge,centile_RBO = ntile(glm_merge$prob.y,100))
glm_merge$RBO_pred_class = ifelse(glm_merge$centile_RBO>93,"RBO","non.RBO")



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



