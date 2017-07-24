rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)
library(rpart.plot)

allvars <- fread("allvars_0619.csv")
colSums(is.na(allvars))
str(allvars)

# Fill NAs with 0: 
allvars[is.na(allvars)] <- 0
colSums(is.na(allvars))

allvars <- as.data.frame.matrix(allvars)

################################## Iteration: 061901 Linear Regression Model ######################################
vars_061901 <- c(
  "CID",
  "Tiv_Des_NBRx",
  "ECL_IND",
  "Des_Flag",
# "VIIV_NBRx",
  "TRIUMEQ_NBRx_pre",
  "Perc_Tiv_Tru_Pre",
  "TRUVADA_NBRx_loyalty",
  "TRIUMEQ_NBRX_growth",
  "HVMW_IND",
  "GENVOYA_NBRx_pre",
  "TIVICAY_NBRx_pre",
# "DTG_NBRx_share_pre",
  "triumeq_to_odefsey_rx",
  "TRUVADA_SWITCH_NBRX_growth",
  "total_details",
  "total_reps",
  "Mkt_NBRx"
)

allvars_061901 <- allvars[,vars_061901]

################################## Iteration: 06190101 Classification Model ######################################
table(allvars_061901$Des_Flag)
allvars_06190101 <- allvars_061901[allvars_061901$Des_Flag %in% c("High","Low","Med"),]

control_tree = trainControl(method="cv",number=5,classProbs = T)

set.seed(06190101) # System time: 
system.time(mod_rpart_06190101 <- train(as.factor(Des_Flag)~.-CID-ECL_IND-Tiv_Des_NBRx,
                                    data=allvars_06190101,method="rpart",metric="Kappa",
                                    trControl = control_tree,tuneLength=5))
mod_rpart_06190101
mod_rpart_06190101$finalModel

# Plot the decision tree
fancyRpartPlot(mod_rpart_06190101$finalModel,palettes=c("Greys", "Oranges"))
rpart.plot(mod_rpart_06190101$finalModel,cex=0.7)
varImp(mod_rpart_06190101)

mod_rpart_06190101_varImp <- varImp(mod_rpart_06190101)
mod_rpart_06190101_varImp <- as.data.frame(mod_rpart_06190101_varImp$importance)
mod_rpart_06190101_varImp <- data.frame(Variable = row.names(mod_rpart_06190101_varImp), Importance = mod_rpart_06190101_varImp$Overall)
fwrite(mod_rpart_06190101_varImp,"../../Model Output/mod_rpart_06190101_varImp.csv")


################################## Iteration: 06190102 Classification Model ######################################
allvars_06190102 <- allvars_06190101[allvars_06190101$ECL_IND==1,]

set.seed(06190102) # System time: 
system.time(mod_rpart_06190102 <- train(as.factor(Des_Flag)~.-CID-ECL_IND-Tiv_Des_NBRx,
                                    data=allvars_06190102,method="rpart",metric="Kappa",
                                    trControl = control_tree,tuneLength=5))
mod_rpart_06190102
mod_rpart_06190102$finalModel
# Plot the decision tree
rpart.plot(mod_rpart_06190102$finalModel,cex=0.5)
mod_rpart_06190102_varImp <- varImp(mod_rpart_06190102)
mod_rpart_06190102_varImp <- as.data.frame(mod_rpart_06190102_varImp$importance)
mod_rpart_06190102_varImp <- data.frame(Variable = row.names(mod_rpart_06190102_varImp), Importance = mod_rpart_06190102_varImp$Overall)
fwrite(mod_rpart_06190102_varImp,"../../Model Output/mod_rpart_06190102_varImp.csv")









  