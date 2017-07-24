rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)

allvars <- fread("allvars_0614.csv")
colSums(is.na(allvars))
str(allvars)

# Fill NAs with 0: 
allvars[is.na(allvars)] <- 0
colSums(is.na(allvars))

ggplot(allvars, aes(Tiv_Des_NBRx)) + geom_histogram()

##################### Correlation between target variable and predictors #########################
# get numeric variables
num <- sapply(allvars,is.numeric)
allvars <- as.data.frame.matrix(allvars)
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


###################################### Decision tree to see variable importance ######################################
control = trainControl(method="cv",number=5)
# Interation 061401
set.seed(061401) # System time: 
system.time(mod_rpart_01 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_01

varImp(mod_rpart_01)
plot(varImp(mod_rpart_01))

# Iteration 061402
set.seed(061402) # System time: 
system.time(mod_rpart_02 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_02
varImp(mod_rpart_02)
plot(varImp(mod_rpart_02))

# Iteration 061403
set.seed(061403) # System time: 
system.time(mod_rpart_03 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx # -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_03
varImp(mod_rpart_03)
plot(varImp(mod_rpart_03))

# Iteration 061404
set.seed(061404) # System time: 
system.time(mod_rpart_04 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx # -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_04
varImp(mod_rpart_04)
plot(varImp(mod_rpart_04))


# Iteration 061405
set.seed(061405) # System time: 
system.time(mod_rpart_05 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post # -NRTI_Switch
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_05
varImp(mod_rpart_05)
plot(varImp(mod_rpart_05))

# Iteration 061406
set.seed(061406) # System time: 
system.time(mod_rpart_06 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post # -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj # -DBR_TRx_recent-GILEAD_Switch
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_06
varImp(mod_rpart_06)
plot(varImp(mod_rpart_06))


# Iteration 061407
set.seed(061407) # System time: 
system.time(mod_rpart_07 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post  -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj  -DBR_TRx_recent-GILEAD_Switch
                                  -abc_to_descovy_rx  -DBR_TRx-Tiv_Tru_Pts_Pre-TIVICAY_Naive_NBRx-DBR_NBRx-DBR_Switch_NBRx
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_07
varImp(mod_rpart_07)
plot(varImp(mod_rpart_07))


# Iteration 061408
set.seed(061408) # System time: 
system.time(mod_rpart_08 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post  -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj  -DBR_TRx_recent-GILEAD_Switch
                                  -abc_to_descovy_rx  -DBR_TRx-Tiv_Tru_Pts_Pre-TIVICAY_Naive_NBRx-DBR_NBRx-DBR_Switch_NBRx
                                  -gilead_to_descovy_rx-triumeq_to_descovy_rx # -VIIV_Switch-VIIV_NBRx-VIIV_TRx-NRTI_NBRx-NRTI_NBRx_recent
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_08
varImp(mod_rpart_08)
plot(varImp(mod_rpart_08))


# Iteration 061409
set.seed(061409) # System time: 
system.time(mod_rpart_09 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post  -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj  -DBR_TRx_recent-GILEAD_Switch
                                  -abc_to_descovy_rx  -DBR_TRx-Tiv_Tru_Pts_Pre-TIVICAY_Naive_NBRx-DBR_NBRx-DBR_Switch_NBRx
                                  -gilead_to_descovy_rx-triumeq_to_descovy_rx  -VIIV_Switch-VIIV_NBRx-VIIV_TRx-NRTI_NBRx-NRTI_NBRx_recent
                                  -Tiv_Des_TRx_pre  -TIVICAY_TRx_pre-Perc_Tiv_Tru_Pre-Tiv_Tru_Pts_Post,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_09
varImp(mod_rpart_09)
plot(varImp(mod_rpart_09))


# Iteration 061410
set.seed(061410) # System time: 
system.time(mod_rpart_10 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post  -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj  -DBR_TRx_recent-GILEAD_Switch
                                  -abc_to_descovy_rx  -DBR_TRx-Tiv_Tru_Pts_Pre-TIVICAY_Naive_NBRx-DBR_NBRx-DBR_Switch_NBRx
                                  -gilead_to_descovy_rx-triumeq_to_descovy_rx  -VIIV_Switch-VIIV_NBRx-VIIV_TRx-NRTI_NBRx-NRTI_NBRx_recent
                                  -Tiv_Des_TRx_pre  -TIVICAY_TRx_pre-Perc_Tiv_Tru_Pre-Tiv_Tru_Pts_Post
                                  -Tiv_Des_NBRx_pre  -DBR_TRx_pre-Mkt_Switch-DBR_NBRx_recent-Mkt_NBRx-TIVICAY_NBRx_pre-GILEAD_NBRx
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_10
varImp(mod_rpart_10)
plot(varImp(mod_rpart_10))



# Iteration 061411
set.seed(061411) # System time: 
system.time(mod_rpart_11 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post  -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj  -DBR_TRx_recent-GILEAD_Switch
                                  -abc_to_descovy_rx  -DBR_TRx-Tiv_Tru_Pts_Pre-TIVICAY_Naive_NBRx-DBR_NBRx-DBR_Switch_NBRx
                                  -gilead_to_descovy_rx-triumeq_to_descovy_rx  -VIIV_Switch-VIIV_NBRx-VIIV_TRx-NRTI_NBRx-NRTI_NBRx_recent
                                  -Tiv_Des_TRx_pre  -TIVICAY_TRx_pre-Perc_Tiv_Tru_Pre-Tiv_Tru_Pts_Post
                                  -Tiv_Des_NBRx_pre  -DBR_TRx_pre-Mkt_Switch-DBR_NBRx_recent-Mkt_NBRx-TIVICAY_NBRx_pre-GILEAD_NBRx
                                     -VIIV_Naive_NBRx-DBR_Naive_NBRx-Mkt_TRx_recent-DBR_NBRx_pre-DBR_NBRx_loyalty-NRTI_TRx_recent-TRUVADA_Switch_NBRx-NRTI_TRx-Mkt_TRx-ODEFSEY_Switch_NBRx
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_11
varImp(mod_rpart_11)
plot(varImp(mod_rpart_11))


# Iteration 061412
set.seed(061412) # System time: 
system.time(mod_rpart_12 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post  -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj  -DBR_TRx_recent-GILEAD_Switch
                                  -abc_to_descovy_rx  -DBR_TRx-Tiv_Tru_Pts_Pre-TIVICAY_Naive_NBRx-DBR_NBRx-DBR_Switch_NBRx
                                  -gilead_to_descovy_rx-triumeq_to_descovy_rx  -VIIV_Switch-VIIV_NBRx-VIIV_TRx-NRTI_NBRx-NRTI_NBRx_recent
                                  -Tiv_Des_TRx_pre  -TIVICAY_TRx_pre-Perc_Tiv_Tru_Pre-Tiv_Tru_Pts_Post
                                  -Tiv_Des_NBRx_pre  -DBR_TRx_pre-Mkt_Switch-DBR_NBRx_recent-Mkt_NBRx-TIVICAY_NBRx_pre-GILEAD_NBRx
                                    -VIIV_Naive_NBRx-DBR_Naive_NBRx-Mkt_TRx_recent-DBR_NBRx_pre-DBR_NBRx_loyalty-NRTI_TRx_recent-TRUVADA_Switch_NBRx-NRTI_TRx-Mkt_TRx-ODEFSEY_Switch_NBRx
                                    -Mkt_NBRx_pre-Mkt_NBRx_recent-Ttl_Pts_Post-ECL_Status-HVMW_IND-Perc_Tiv_Tru_Post-DTG_TRx_share_pre-triumeq_to_other_rx,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_12
varImp(mod_rpart_12)
plot(varImp(mod_rpart_12))


# Iteration 061412
set.seed(061413) # System time: 
system.time(mod_rpart_13 <- train(Tiv_Des_NBRx~.-CID-Segment-Street_Address-CITY-State-Zip_Code-Terr_ID-Des_Flag-Tiv_Des_NBRx99Dec
                                  -Tiv_Des_Pts-Tiv_Des_Pts_Post-Tiv_Des_TRx_recent-Des_NBRx-DESCOVY_NBRx
                                  -Ttl_Des_Pts-DESCOVY_Switch_NBRx-switch_to_descovy_rx-DESCOVY_TRx-truvada_to_descovy_rx  -TIVICAY_Switch_NBRx-TIVICAY_NBRx-TIVICAY_TRx_recent-Tiv_TRx
                                  -Tiv_Des_NBRx_recent-DESCOVY_Naive_NBRx-Perc_Tiv_Des_Post  -NRTI_Switch
                                  -TIVICAY_TRx-TIVICAY_NBRx_recent-Perc_Tiv-Perc_Tiv_Adj  -DBR_TRx_recent-GILEAD_Switch
                                  -abc_to_descovy_rx  -DBR_TRx-Tiv_Tru_Pts_Pre-TIVICAY_Naive_NBRx-DBR_NBRx-DBR_Switch_NBRx
                                  -gilead_to_descovy_rx-triumeq_to_descovy_rx  -VIIV_Switch-VIIV_NBRx-VIIV_TRx-NRTI_NBRx-NRTI_NBRx_recent
                                  -Tiv_Des_TRx_pre  -TIVICAY_TRx_pre-Perc_Tiv_Tru_Pre-Tiv_Tru_Pts_Post
                                  -Tiv_Des_NBRx_pre  -DBR_TRx_pre-Mkt_Switch-DBR_NBRx_recent-Mkt_NBRx-TIVICAY_NBRx_pre-GILEAD_NBRx
                                    -VIIV_Naive_NBRx-DBR_Naive_NBRx-Mkt_TRx_recent-DBR_NBRx_pre-DBR_NBRx_loyalty-NRTI_TRx_recent-TRUVADA_Switch_NBRx-NRTI_TRx-Mkt_TRx-ODEFSEY_Switch_NBRx
                                    -Mkt_NBRx_pre-Mkt_NBRx_recent-Ttl_Pts_Post-ECL_Status-HVMW_IND-Perc_Tiv_Tru_Post-DTG_TRx_share_pre-triumeq_to_other_rx
                                    -TRUVADA_TRx_pre-Mkt_TRx_pre-STR_Switch-ODEFSEY_TRx-GILEAD_TRx-NRTI_TRx_pre-TRUVADA_TRx-Ttl_Pts_Pre
                                  # -PI_NBRx-ODEFSEY_TRx_recent-STR_NBRx-ODEFSEY_NBRx-TRIUMEQ_TRx-PI_TRx-PI_TRx_pre-Mkt_Naive_NBRx-PI_TRx_recent
                                  ,
                                  data=allvars,method="rpart",
                                  trControl = control,tuneLength=3))
mod_rpart_13
varImp(mod_rpart_13)
plot(varImp(mod_rpart_13))


###################################### Correlation between predictors ######################################
## Check the correlation between predictors
cor = cor(subset(allvars,select=c(Tiv_Tru_Pts_Pre, TIVICAY_TRx_pre, DBR_TRx_pre, 
                                        Mkt_NBRx_pre,TRUVADA_TRx_pre,NRTI_TRx_pre,PI_TRx_pre)))
# transform correlation matrix to three column data frame
cor_summary_pred = as.data.frame(as.table(cor))
colnames(cor_summary_pred)[3] = "correlation"
# exclude if var1 is same as var2
cor_summary_pred = cor_summary_pred[cor_summary_pred$Var1!=cor_summary_pred$Var2,]
# create absolute correlation
cor_summary_pred$abs_cor = abs(cor_summary_pred$correlation)
# sort by absolute correlation
cor_summary_pred = cor_summary_pred[order(cor_summary_pred$abs_cor,decreasing = T),]


###################################### Linear Regression Model ######################################
system.time(
  mod_lm_01 <- train(Tiv_Des_NBRx~Tiv_Tru_Pts_Pre + TIVICAY_TRx_pre + DBR_TRx_pre + 
    Mkt_NBRx_pre + TRUVADA_TRx_pre + NRTI_TRx_pre + PI_TRx_pre,
  data = allvars, method="lm", metric="RMSE",trControl=control)
)

summary(mod_lm_01)



















