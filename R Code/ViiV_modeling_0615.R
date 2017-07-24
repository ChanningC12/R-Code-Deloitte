rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)
library(rpart.plot)

allvars <- fread("allvars_0614.csv")
colSums(is.na(allvars))
str(allvars)

# Fill NAs with 0: 
allvars[is.na(allvars)] <- 0
colSums(is.na(allvars))

allvars <- as.data.frame.matrix(allvars)

# Keep preliminary variables
vars <- c(
  "CID",
  "Tiv_Des_NBRx",
  "Des_Flag",
  "ECL_IND",
  "Mkt_NBRx",
  "VIIV_NBRx",
  "GILEAD_NBRx",
  "GENVOYA_NBRx_pre",
  "ODEFSEY_NBRx_pre",
  "Mkt_NBRx_pre",
  "TIVICAY_NBRx_pre",
  "TRIUMEQ_NBRx_pre",
  "NRTI_NBRx_pre",
  "DBR_NBRx_pre",
  "PI_NBRx_pre",
  "TRUVADA_NBRx_pre",
  "ATRIPLA_NBRx_pre",
  "DTG_NBRx_share_pre",
  "TRIUMEQ_NBRx_share_pre",
  "TRUVADA_NBRx_share_pre",
  "TRIUMEQ_NAIVE_NBRX_growth",
  "TIVICAY_NAIVE_NBRX_growth",
  "DBR_NAIVE_NBRX_growth",
  "TRUVADA_NAIVE_NBRX_growth",
  "PI_NAIVE_NBRX_growth",
  "ATRIPLA_NAIVE_NBRX_growth",
  "TRIUMEQ_NBRX_growth",
  "TIVICAY_NBRX_growth",
  "DBR_NBRX_growth",
  "TRUVADA_NBRX_growth",
  "PI_NBRX_growth",
  "ATRIPLA_NBRX_growth",
  "TRIUMEQ_SWITCH_NBRX_growth",
  "TIVICAY_SWITCH_NBRX_growth",
  "DBR_SWITCH_NBRX_growth",
  "TRUVADA_SWITCH_NBRX_growth",
  "PI_SWITCH_NBRX_growth",
  "ATRIPLA_SWITCH_NBRX_growth",
  "Gilead_NBRx_loyalty",
  "STR_NBRx_loyalty",
  "TRUVADA_NBRx_loyalty",
  "DBR_NBRx_loyalty",
  "triumeq_to_genvoya_rx",
  "triumeq_to_odefsey_rx",
  "triumeq_to_other_rx",
  "ECL_IND",
  "HVMW_IND",
  "total_details",
  "total_reps",
  "Region",
  "test_treat_ind",
  "Genvoya_Adopter",
  "Triumeq_Adopter",
  "Ttl_Pts_Pre",
  "Perc_Trii_Pre",
  "Perc_Tiv_Tru_Pre"
)

allvars <- allvars[,vars]

##################### Correlation between target variable and predictors #########################
# get numeric variables
num <- sapply(allvars,is.numeric)
allvars_num <- allvars[,num]
allvars_cor = cor(subset(allvars_num, select=-c(Tiv_Des_NBRx)),allvars$Tiv_Des_NBRx)
# transform correlation matrix to three column data frame
cor_summary = as.data.frame(as.table(allvars_cor))
colnames(cor_summary)[3] = "correlation"
cor_summary$Var2 <- "Tiv_Des_NBRx"
# create absolute correlation
cor_summary$abs_cor = abs(cor_summary$correlation)
# sort by absolute correlation
cor_summary = cor_summary[order(cor_summary$abs_cor,decreasing = T),]
fwrite(cor_summary,"../../Model Output/mod_061501_cor.csv",row.names = F)

###################################### Decision tree to see variable importance ######################################
control = trainControl(method="cv",number=5)
# Interation 061501
set.seed(061501) # System time: 
system.time(mod_rpart_01 <- train(Tiv_Des_NBRx~.-CID-ECL_IND-Des_Flag,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=20))
mod_rpart_01
mod_rpart_01$finalModel
fancyRpartPlot(mod_rpart_01$finalModel)
mod_rpart_01_varImp <- varImp(mod_rpart_01)
mod_rpart_01_varImp <- as.data.frame(mod_rpart_01_varImp$importance)
mod_rpart_01_varImp <- data.frame(Variable = row.names(mod_rpart_01_varImp), Importance = mod_rpart_01_varImp$Overall)
fwrite(mod_rpart_01_varImp,"../../Model Output/mod_rpart_01_varImp.csv")

###################################### Correlation between predictors ######################################
mod_rpart_01_varImp <- mod_rpart_01_varImp %>% 
  dplyr::mutate(
    rank = rank(-Importance,ties.method = "min")
  )

allvars_lm <- allvars[,names(allvars) %in% c("CID","Tiv_Des_NBRx","ECL_IND","Des_Flag") | names(allvars) %in% mod_rpart_01_varImp[mod_rpart_01_varImp$Importance>0,]$Variable]

## Check the correlation between predictors
cor = cor(subset(allvars_lm,select=-c(CID,Tiv_Des_NBRx,ECL_IND,Des_Flag)))
# transform correlation matrix to three column data frame
cor_summary_pred = as.data.frame(as.table(cor))
colnames(cor_summary_pred)[3] = "correlation"
# exclude if var1 is same as var2
cor_summary_pred = cor_summary_pred[cor_summary_pred$Var1!=cor_summary_pred$Var2,]
# create absolute correlation
cor_summary_pred$abs_cor = abs(cor_summary_pred$correlation)
# sort by absolute correlation
cor_summary_pred = cor_summary_pred[order(cor_summary_pred$abs_cor,decreasing = T),]

cor_summary_pred <- cor_summary_pred[seq(1,nrow(cor_summary_pred),2),]
fwrite(cor_summary_pred,"../../Model Output/mod_lm_01_cor.csv",row.names = F)

################################## Iteration: 061501 Linear Regression Model ######################################
# https://stats.stackexchange.com/questions/5135/interpretation-of-rs-lm-output
mod_lm_01 <- train(Tiv_Des_NBRx~.-CID-DBR_NBRx_pre-ECL_IND-Des_Flag,
                     data = allvars_lm, method="lm", metric="RMSE",trControl=control)
summary(mod_lm_01)
mod_lm_01_summary <- as.data.frame(summary(mod_lm_01$finalModel)$coefficients)
mod_lm_01_summary <- data.frame(Variable = row.names(mod_lm_01_summary),mod_lm_01_summary)
fwrite(mod_lm_01_summary,"../../Model Output/mod_lm_01_summary.csv")



################################## Iteration: 061502 Linear Regression Model ######################################
vars_061502 <- c(
  "CID",
  "Tiv_Des_NBRx",
  "ECL_IND",
  "Des_Flag",
  "VIIV_NBRx",
  "TRIUMEQ_NBRx_pre",
  "Perc_Tiv_Tru_Pre",
  "TRUVADA_NBRx_loyalty",
  "TRIUMEQ_NBRX_growth",
  "HVMW_IND",
  "GENVOYA_NBRx_pre",
  "TIVICAY_NBRx_pre",
  "DTG_NBRx_share_pre",
  "triumeq_to_odefsey_rx",
  "TRUVADA_SWITCH_NBRX_growth",
  "total_details",
  "total_reps"
)

allvars_061502 <- allvars[,vars_061502]

mod_lm_02 <- train(Tiv_Des_NBRx~.-CID-ECL_IND-Des_Flag,
                   data = allvars_061502, method="lm", metric="RMSE",trControl=control)
summary(mod_lm_02)
mod_lm_02_summary <- as.data.frame(summary(mod_lm_02$finalModel)$coefficients)
mod_lm_02_summary <- data.frame(Variable = row.names(mod_lm_02_summary),mod_lm_02_summary)
fwrite(mod_lm_02_summary,"../../Model Output/mod_lm_02_summary.csv")


# Decision Tree for Variable Importance
set.seed(061502) # System time: 
system.time(mod_rpart_02 <- train(Tiv_Des_NBRx~.-CID-ECL_IND-Des_Flag,
                                  data=allvars_061502,method="rpart",
                                  trControl = control,tuneLength=30))
mod_rpart_02
mod_rpart_02$finalModel
# Plot the decision tree
fancyRpartPlot(mod_rpart_02$finalModel,palettes=c("Greys", "Oranges"))
rpart.plot(mod_rpart_02$finalModel)

mod_rpart_02_varImp <- varImp(mod_rpart_02)
mod_rpart_02_varImp <- as.data.frame(mod_rpart_02_varImp$importance)
mod_rpart_02_varImp <- data.frame(Variable = row.names(mod_rpart_02_varImp), Importance = mod_rpart_02_varImp$Overall)
fwrite(mod_rpart_02_varImp,"../../Model Output/mod_rpart_02_varImp.csv")


################################## Iteration: 06150201 Classification Model ######################################
table(allvars_061502$Des_Flag)
allvars_06150201 <- allvars_061502[allvars_061502$Des_Flag %in% c("High","Low","Med"),]

control_tree = trainControl(method="cv",number=5,classProbs = T)

set.seed(06150201) # System time: 
system.time(mod_rpart_0201 <- train(as.factor(Des_Flag)~.-CID-ECL_IND-Tiv_Des_NBRx,
                                  data=allvars_06150201,method="rpart",metric="Kappa",
                                  trControl = control_tree,tuneLength=10))
mod_rpart_0201
mod_rpart_0201$finalModel
# Plot the decision tree
fancyRpartPlot(mod_rpart_0201$finalModel,palettes=c("Greys", "Oranges"))
rpart.plot(mod_rpart_0201$finalModel)
varImp(mod_rpart_0201)

mod_rpart_0201_varImp <- varImp(mod_rpart_0201)
mod_rpart_0201_varImp <- as.data.frame(mod_rpart_0201_varImp$importance)
mod_rpart_0201_varImp <- data.frame(Variable = row.names(mod_rpart_0201_varImp), Importance = mod_rpart_0201_varImp$Overall)
fwrite(mod_rpart_0201_varImp,"../../Model Output/mod_rpart_0201_varImp.csv")


################################## Iteration: 06150202 Classification Model ######################################
table(allvars_061502$Des_Flag)
allvars_06150202 <- allvars_06150201[allvars_06150201$ECL_IND==1,]

set.seed(06150202) # System time: 
system.time(mod_rpart_0202 <- train(as.factor(Des_Flag)~.-CID-ECL_IND-Tiv_Des_NBRx,
                                    data=allvars_06150202,method="rpart",metric="Kappa",
                                    trControl = control_tree,tuneLength=10))
mod_rpart_0202
mod_rpart_0202$finalModel
# Plot the decision tree
rpart.plot(mod_rpart_0202$finalModel,cex=0.55)
mod_rpart_0202_varImp <- varImp(mod_rpart_0202)
mod_rpart_0202_varImp <- as.data.frame(mod_rpart_0202_varImp$importance)
mod_rpart_0202_varImp <- data.frame(Variable = row.names(mod_rpart_0202_varImp), Importance = mod_rpart_0202_varImp$Overall)
fwrite(mod_rpart_0202_varImp,"../../Model Output/mod_rpart_0202_varImp.csv")


################################## Iteration: 061601 Linear Regression Model ######################################
vars_061601 <- c(
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

allvars_061601 <- allvars[,vars_061601]

mod_lm_03 <- train(Tiv_Des_NBRx~.-CID-ECL_IND-Des_Flag,
                   data = allvars_061601, method="lm", metric="RMSE",trControl=control)
summary(mod_lm_03)
mod_lm_03_summary <- as.data.frame(summary(mod_lm_03$finalModel)$coefficients)
mod_lm_03_summary <- data.frame(Variable = row.names(mod_lm_03_summary),mod_lm_03_summary)
fwrite(mod_lm_03_summary,"../../Model Output/mod_lm_03_summary.csv")


# Decision Tree for Variable Importance
set.seed(061601) # System time: 
system.time(mod_rpart_03 <- train(Tiv_Des_NBRx~.-CID-ECL_IND-Des_Flag,
                                  data=allvars_061601,method="rpart",
                                  trControl = control,tuneLength=30))
mod_rpart_03
mod_rpart_03$finalModel
# Plot the decision tree
fancyRpartPlot(mod_rpart_03$finalModel,palettes=c("Greys", "Oranges"))
rpart.plot(mod_rpart_03$finalModel)

mod_rpart_03_varImp <- varImp(mod_rpart_03)
mod_rpart_03_varImp <- as.data.frame(mod_rpart_03_varImp$importance)
mod_rpart_03_varImp <- data.frame(Variable = row.names(mod_rpart_03_varImp), Importance = mod_rpart_03_varImp$Overall)
fwrite(mod_rpart_03_varImp,"../../Model Output/mod_rpart_03_varImp.csv")


################################## Iteration: 06160101 Classification Model ######################################
table(allvars_061601$Des_Flag)
allvars_06160101 <- allvars_061601[allvars_061601$Des_Flag %in% c("High","Low","Med"),]

control_tree = trainControl(method="cv",number=5,classProbs = T)

set.seed(06160101) # System time: 
system.time(mod_rpart_0301 <- train(as.factor(Des_Flag)~.-CID-ECL_IND-Tiv_Des_NBRx,
                                    data=allvars_06160101,method="rpart",metric="Kappa",
                                    trControl = control_tree,tuneLength=5))
mod_rpart_0301
mod_rpart_0301$finalModel
# Plot the decision tree
fancyRpartPlot(mod_rpart_0301$finalModel,palettes=c("Greys", "Oranges"))
rpart.plot(mod_rpart_0301$finalModel,cex=0.7)
varImp(mod_rpart_0301)

mod_rpart_0301_varImp <- varImp(mod_rpart_0301)
mod_rpart_0301_varImp <- as.data.frame(mod_rpart_0301_varImp$importance)
mod_rpart_0301_varImp <- data.frame(Variable = row.names(mod_rpart_0301_varImp), Importance = mod_rpart_0301_varImp$Overall)
fwrite(mod_rpart_0301_varImp,"../../Model Output/mod_rpart_0301_varImp.csv")


################################## Iteration: 06160102 Classification Model ######################################
allvars_06160102 <- allvars_06160101[allvars_06160101$ECL_IND==1,]

set.seed(06160102) # System time: 
system.time(mod_rpart_0302 <- train(as.factor(Des_Flag)~.-CID-ECL_IND-Tiv_Des_NBRx,
                                    data=allvars_06160102,method="rpart",metric="Kappa",
                                    trControl = control_tree,tuneLength=5))
mod_rpart_0302
mod_rpart_0302$finalModel
# Plot the decision tree
rpart.plot(mod_rpart_0202$finalModel,cex=0.5)
mod_rpart_0302_varImp <- varImp(mod_rpart_0302)
mod_rpart_0302_varImp <- as.data.frame(mod_rpart_0302_varImp$importance)
mod_rpart_0302_varImp <- data.frame(Variable = row.names(mod_rpart_0302_varImp), Importance = mod_rpart_0302_varImp$Overall)
fwrite(mod_rpart_0302_varImp,"../../Model Output/mod_rpart_0302_varImp.csv")









  