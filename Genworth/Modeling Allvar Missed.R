##################### Allvar Missed Dataset #######################
rm(list=ls())
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
allvars = read.csv("allvars_capped_miss1213.csv")

# list of variables to keep for modeling purpose
varlist = c(
      "PLCY_REF",
      "EFF_DT",
      "NFO_IND",
      "RBO_IND",
      "ISSUE_AGE", # Issue_age
      "IF_SPOUSE_DISC", # Spousal Discount
      "SHARED_POLICY", # Shared Policy
      "POLICYHOLDER_AGE", # Policyholder age
      "CLAIM_COUNT_CONTACT_DYNAMIC", # Count of any contact of claim
      "CLAIM_COUNT_CLOSED_DYNAMIC", # Count of any contact of claim
      "TOTAL_AMOUNT_PAID", # Total amount paid
      "CLAIM_COUNT_DYNAMIC", # Total Dynamic count of claims
      "RATE_INCR", # % rate increase
      "RATE_INCR_AFFORD", # Rate increase/total income
      "IFA_GROUP_STATE_PROD_FA", # Rate Action State/Product Approval Group - Full Approval
      "IFA_GROUP_STATE_PROD_FAWP", # Rate Action State/Product Approval Group - Full Approval W Phases
      "IFA_GROUP_STATE_PROD_OTFA", # NA
      "POLICY_AGE_2", # Policy age
      "ANNL_PREM_BFOR", # The Annual Premium being paid by the policyholder prior to the Round
      "RESPONSE_PRE", # NA
      "CNT_FPO_PREV1", # Count of previous FPO response
      "CNT_RBO_PREV1", # Count of previous RBO response
      "BENEFIT_POOL_ASSET_RATIO", # Total Pool of benfits/Total assets
      "DECISION_STAGE", # Stage of Decision
      "BINARY_CARDIO_TRANS", # Propenisty Cardio-Transformed
      "BINARY_DIAB_TRANS", # Propenisty Diabetes-Transformed
      "BINARY_NF_TRANS", # Propenisty Neoplasm Female-Transformed
      "BINARY_HYPT_TRANS", # Propenisty Hypertension-Transformed
      "HH_INCOME", # Household income range mid point
      "NETW", # Total assets
      "BANK", # Bankruptcy Household Indicator
      "NOC19", # Number of Children
      "BAL_NONAGNFIRSTMTG", # Sum of balances for mortgages underwritten or guaranteed by organization other than Freddie, Fannie, FHA, or VA
      "HIGHCRD_1STMTGCREDIT", # Total high credit for all 1st Mortgage accounts from mortgage trades with a mortgage or real estate company/bank, credit union or finance company exclusive of HELOC and HELoan
      "HIGHCRD_TOTALALLCREDIT", # Total high credit of all credit accounts
      "BAL_BANKCARD_90TO119DPD", # Total balance of Bank Card accounts that are 90 to 119 days past due
      "ZAGG_CCW", # Concealed Weapons
      "ZAGG_TUOC_B", # Individual Occupation - Professional
      "ZAGG_ILOR_15", # Length of Residence - Average
      "BENE_TRM_BFOR", # Benefit Period (in years) Before
      "BIO_BEFORE_NO_BIO", # Benefit Inflation Option Before-No Bio
      "ORIG_DBA_BFOR", # Daily Benefit Amount Before
      "FMLY_PRSNC", # Presence of family members nearby
      "HHCOMP_A", # Married with children
      "NUM_DECISIONS", # Decision Count
      "TOTL_COMMUNICATED_INCR_RATE", # Communicated Rate Increase
      "PREM_PAID_TOT_2", # Total premium paid to date
      "BIO_BEFORE_5_COMPOUND", # BIO_BEFORE_5_COMPOUND
      "FREQ_BFOR_M", # FREQ_BFOR_M
      "FREQ_BFOR_Q", # FREQ_BFOR_Q
      "RATE_INCR_PRE", # Previous rate increase
      "BAVGONDL", # Buyer - Catalog - Average Online Dollars
      "CA00", # Children Present 00-02 Years of Age
      "FAMP_M", # Family position code-MALE HOH
      "HHMARST", # Marital Status - Household
      "STATE_PARTNERSHIP", # STATE_PARTNERSHIP
      "CLAIM_COUNT_NP_CLOSED_DYNAMIC", # No pay claim closed claim count
      "CLAIM_COUNT_OPEN_DYNAMIC", # Dynamic counts of open claims
      "BINARY_PREG_TRANS", # Propenisty Pregnancy-Transformed
      "AALZ", # Alzheimers disease ailment
      "APT_N", # Dwelling_type-NURSING HOME
      "HHCOMP_D", # ONE MALE, ONE FEMALE MARRITAL STAT UNK W/ NO CHILDREN 
      "HHCOMP_J", # MALE HOH ONLY W/O CHILDREN
      "INIRA", # investment in a individual retirement account
      "INMEDI", # investment in Medicare coverage
      "IORE", # indicating someone is retired
      "MEDSUP", # Medicare Supplement Insurance Responder Index
      "NAH19", # Number of Adults in Household
      "PALLERGY", # indicating allergy medications treatment2 is represented in the household
      "TUOC_G", # Individual Occupation -HEALTH SERVICES
      "TUOCS_M", # Individual Spouse Occupation -NURSE
      "BAL_AUTOFINANCE_60DPD", # Sum of balances for auto loans opened through a Dealer or Auto Finance company that are 60+ days past due
      "BAL_TOTALALLCREDIT", # Sum of balances for all credit accounts 
      "HH_BANKCARD_SEVEREDEROG", # Percentage of households with at least one Bank Card account in severe derogatory status
      "HIGHCRD_MTGCREDIT", # Total high credit of all mortgage accounts
      "HIGHCRD_NONMTGCREDIT", # Total high credit for Non-Mortgage accounts 
      "ZAGG_OHMCARE", # Buyer - Catalog - Home Care
      "ZAGG_RHEALTH" # Buyer - Retail - Health
      )

# Keep above variables
allvars_filter = allvars[,colnames(allvars) %in% varlist]
varlist %in% colnames(allvars_filter)

# Check missing values
for (Var in names(allvars_filter)) {
  missing <- sum(is.na(allvars_filter[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}
# Still missing few disease scores
# Impute with median
for (x in 1:ncol(allvars_filter)) {
    allvars_filter[is.na(allvars_filter[,x]),x] = median(allvars_filter[,x],na.rm=T)
  }


# Check high correlation
allvars_filter = allvars_filter[,c(1:2,8:9,3:7,10:ncol(allvars_filter))] # reorder the column
allvars_cor = cor(allvars_filter[,c(5:ncol(allvars_filter))])
highlyCor = findCorrelation(allvars_cor,cutoff=0.6)
allvars_predictors = allvars_filter[,c(5:ncol(allvars_filter))]
names(allvars_predictors[,highlyCor])

varlist_rm = c("HIGHCRD_MTGCREDIT",
               "HIGHCRD_1STMTGCREDIT",
               "BAL_NONAGNFIRSTMTG",
               "HIGHCRD_NONMTGCREDIT",
               "ISSUE_AGE",
               "PREM_PAID_TOT_2",
               "HHCOMP_J",
               "CLAIM_COUNT_CLOSED_DYNAMIC",
               "HIGHCRD_TOTALALLCREDIT",
               "RATE_INCR_PRE")

allvars_filter_cor = allvars_filter[,!colnames(allvars_filter) %in% varlist_rm]

# Check the correlation again
allvars_cor2 = cor(allvars_filter_cor[,c(5:ncol(allvars_filter_cor))])
highlyCor2 = findCorrelation(allvars_cor2,cutoff=0.75)
allvars_final = allvars_filter_cor

############ Train/Test/Validation and Sampling Techniques ###############
set.seed(111)
ind_nfo = createDataPartition(allvars_final$NFO_IND,p=0.6,list=F)
allvars_final_train_nfo = allvars_final[ind_nfo,]
allvars_final_val_nfo = allvars_final[-ind_nfo,]
prop.table(table(allvars_final_train_nfo$NFO_IND))
prop.table(table(allvars_final_val_nfo$NFO_IND))

set.seed(222)
ind_rbo = createDataPartition(allvars_final$RBO_IND,p=0.6,list=F)
allvars_final_train_rbo = allvars_final[ind_rbo,]
allvars_final_val_rbo = allvars_final[-ind_rbo,]
prop.table(table(allvars_final_train_rbo$RBO_IND))
prop.table(table(allvars_final_val_rbo$RBO_IND))

# SMOTE sampling
allvars_final_train_nfo$NFO_IND = as.factor(allvars_final_train_nfo$NFO_IND)
allvars_final_train_rbo$RBO_IND = as.factor(allvars_final_train_rbo$RBO_IND)

set.seed(1112)
nfo_SMOTE = SMOTE(NFO_IND~.,data=allvars_final_train_nfo)
prop.table(table(nfo_SMOTE$NFO_IND))

set.seed(2221)
system.time(rbo_SMOTE <- SMOTE(RBO_IND~.,data=allvars_final_train_rbo)) # 1261s
prop.table(table(rbo_SMOTE$RBO_IND))

############ Decision Tree Synthetic Variables ###########
allvars_final_val_rbo$tree_var1 = ifelse(allvars_final_val_rbo$RATE_INCR<5.5 & 
                                           allvars_final_val_rbo$CNT_RBO_PREV1<0.5 &
                                           allvars_final_val_rbo$TOTL_COMMUNICATED_INCR_RATE>=3.5 &
                                           allvars_final_val_rbo$TOTL_COMMUNICATED_INCR_RATE<5.5 &
                                           allvars_final_val_rbo$BENE_TRM_BFOR<0.5,1,0)

allvars_final_val_rbo$tree_var2 = ifelse(allvars_final_val_rbo$RATE_INCR<5.5 & 
                                           allvars_final_val_rbo$CNT_RBO_PREV1>=0.5,1,0)

allvars_final_val_rbo$tree_var3 = ifelse(allvars_final_val_rbo$RATE_INCR>=5.5 & 
                                           allvars_final_val_rbo$BENEFIT_POOL_ASSET_RATIO <5.5 &
                                           allvars_final_val_rbo$BENE_TRM_BFOR<0.5 &
                                           allvars_final_val_rbo$BENEFIT_POOL_ASSET_RATIO<4.5,1,0)

allvars_final_val_rbo$tree_var4 = ifelse(allvars_final_val_rbo$RATE_INCR>=5.5 & 
                                           allvars_final_val_rbo$BENEFIT_POOL_ASSET_RATIO <5.5 &
                                           allvars_final_val_rbo$BENE_TRM_BFOR>=0.5 &
                                           allvars_final_val_rbo$ANNL_PREM_BFOR<7.5 &
                                           allvars_final_val_rbo$ANNL_PREM_BFOR>=3.5,1,0)


rbo_SMOTE$tree_var1 = ifelse(rbo_SMOTE$RATE_INCR<5.5 & 
                               rbo_SMOTE$CNT_RBO_PREV1<0.5 &
                               rbo_SMOTE$TOTL_COMMUNICATED_INCR_RATE>=3.5 &
                               rbo_SMOTE$TOTL_COMMUNICATED_INCR_RATE<5.5 &
                               rbo_SMOTE$BENE_TRM_BFOR<0.5,1,0)

rbo_SMOTE$tree_var2 = ifelse(rbo_SMOTE$RATE_INCR<5.5 & 
                               rbo_SMOTE$CNT_RBO_PREV1>=0.5,1,0)

rbo_SMOTE$tree_var3 = ifelse(rbo_SMOTE$RATE_INCR>=5.5 & 
                               rbo_SMOTE$BENEFIT_POOL_ASSET_RATIO <5.5 &
                               rbo_SMOTE$BENE_TRM_BFOR<0.5 &
                               rbo_SMOTE$BENEFIT_POOL_ASSET_RATIO<4.5,1,0)

rbo_SMOTE$tree_var4 = ifelse(rbo_SMOTE$RATE_INCR>=5.5 & 
                               rbo_SMOTE$BENEFIT_POOL_ASSET_RATIO <5.5 &
                               rbo_SMOTE$BENE_TRM_BFOR>=0.5 &
                               rbo_SMOTE$ANNL_PREM_BFOR<7.5 &
                               rbo_SMOTE$ANNL_PREM_BFOR>=3.5,1,0)




############ Modeling ##############
nfo_SMOTE$NFO_IND = as.factor(ifelse(nfo_SMOTE$NFO_IND==0,'non.NFO','NFO'))
rbo_SMOTE$RBO_IND = as.factor(ifelse(rbo_SMOTE$RBO_IND==0,'non.RBO','RBO'))
ctrl = trainControl(method="cv",number=3,summaryFunction = twoClassSummary,classProbs = T)

############ Logistic Regression #############
# NFO model
system.time(glm_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                method="glm",family="binomial",trControl=ctrl,metric="ROC"))
summary(glm_nfo)
glm_nfo_pred = predict(glm_nfo,newdata=allvars_final_val_nfo,type="prob")
head(glm_nfo_pred)
tapply(glm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(glm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.80

plotLift(glm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(glm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 3.61

# precision-recall curve
nfo_PR_perf = performance(nfo_ROC,measure="prec",x.measure="rec")
plot(nfo_PR_perf,main="PR Curve - NFO - SMOTE", col="blue", lwd=3)

# use PRROC to draw ROC and PR curve
nfo_1 = glm_nfo_pred$NFO[allvars_final_val_nfo$NFO_IND==1]
nfo_2 = glm_nfo_pred$NFO[allvars_final_val_nfo$NFO_IND==0]

nfo_ROC_new = roc.curve(scores.class0 = nfo_1, scores.class1 = nfo_2, curve=T)
plot(nfo_ROC_new)
nfo_PR = pr.curve(scores.class0 = nfo_1, scores.class1 = nfo_2, curve=T)
plot(nfo_PR)


# RBO model
system.time(glm_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                             method="glm",family="binomial",trControl=ctrl,metric="ROC"))
summary(glm_rbo)
glm_rbo_pred = predict(glm_rbo,newdata=allvars_final_val_rbo,type="prob")
head(glm_rbo_pred)
tapply(glm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(glm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.72

plotLift(glm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(glm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 3.07

# precision-recall curve
rbo_PR_perf = performance(rbo_ROC,measure="prec",x.measure="rec")
plot(rbo_PR_perf,main="PR Curve - rbo - SMOTE", col="blue", lwd=3)

# use PRROC to draw ROC and PR curve
rbo_1 = glm_rbo_pred$rbo[allvars_final_val_rbo$rbo_IND==1]
rbo_2 = glm_rbo_pred$rbo[allvars_final_val_rbo$rbo_IND==0]

rbo_ROC_new = roc.curve(scores.class0 = rbo_1, scores.class1 = rbo_2, curve=T)
plot(rbo_ROC_new)
rbo_PR = pr.curve(scores.class0 = rbo_1, scores.class1 = rbo_2, curve=T)
plot(rbo_PR)


################ CART #####################
# NFO
set.seed(1113)
system.time(cart_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                              method="rpart",trControl=ctrl,metric="ROC",tuneLength=25))
cart_nfo
cart_nfo_pred = predict(cart_nfo,newdata=allvars_final_val_nfo,type="prob")
head(cart_nfo_pred)
tapply(cart_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(cart_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.84

plotLift(cart_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(cart_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 4.90

# RBO
set.seed(2223)
system.time(cart_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                              method="rpart",trControl=ctrl,metric="ROC",tuneLength=25)) # 46s
cart_rbo
cart_rbo_pred = predict(cart_rbo,newdata=allvars_final_val_rbo,type="prob")
head(cart_rbo_pred)
tapply(cart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(cart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.75

plotLift(cart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(cart_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 3.49


################# Linear Discriminant Analysis ###################
# NFO
system.time(lda_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                             method="lda",trControl=ctrl,metric="ROC"))
lda_nfo
lda_nfo_pred = predict(lda_nfo,newdata=allvars_final_val_nfo,type="prob")
head(lda_nfo_pred)
tapply(lda_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(lda_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.79

plotLift(lda_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(lda_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 3.70

# RBO
system.time(lda_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                             method="lda",trControl=ctrl,metric="ROC"))
lda_rbo
lda_rbo_pred = predict(lda_rbo,newdata=allvars_final_val_rbo,type="prob")
head(lda_rbo_pred)
tapply(lda_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(lda_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.71

plotLift(lda_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(lda_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 2.61


############## KNN ################
# NFO
system.time(knn_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                             method="knn",trControl=ctrl,metric="ROC",tuneLength=5)) # 1170s
knn_nfo
knn_nfo_pred = predict(knn_nfo,newdata=allvars_final_val_nfo,type="prob")
head(knn_nfo_pred)
tapply(knn_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(knn_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.68

plotLift(knn_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(knn_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 2.19

# RBO
system.time(knn_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                             method="knn",trControl=ctrl,metric="ROC",tuneLength=5))
knn_rbo
knn_rbo_pred = predict(knn_rbo,newdata=allvars_final_val_rbo,type="prob")
head(knn_rbo_pred)
tapply(knn_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(knn_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) 

plotLift(knn_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(knn_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) 

############ Random Forest ##################
# NFO
set.seed(1115)
system.time(rf_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                            method="rf",trControl=ctrl,metric="ROC",tuneLength=5)) # 1732.9s, mtry=32
rf_nfo
rf_nfo_pred = predict(rf_nfo,newdata=allvars_final_val_nfo,type="prob")
head(rf_nfo_pred)
tapply(rf_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(rf_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.88

plotLift(rf_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(rf_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 5.87

# use PRROC to draw ROC and PR curve
nfo_1 = rf_nfo_pred$NFO[allvars_final_val_nfo$NFO_IND==1]
nfo_2 = rf_nfo_pred$NFO[allvars_final_val_nfo$NFO_IND==0]

nfo_ROC_new = roc.curve(scores.class0 = nfo_1, scores.class1 = nfo_2, curve=T)
plot(nfo_ROC_new)
nfo_PR = pr.curve(scores.class0 = nfo_1, scores.class1 = nfo_2, curve=T)
plot(nfo_PR) # PRAUC 0.32

# Confusion matrix
rf_nfo_pred_class = predict(rf_nfo,newdata=allvars_final_val_nfo,type="raw")
head(rf_nfo_pred_class)
confusionMatrix(rf_nfo_pred_class,allvars_final_val_nfo$NFO_IND,positive="NFO")

# RBO
set.seed(2225)
system.time(rf_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                            method="rf",trControl=ctrl,metric="ROC")) # 3946.7s
rf_rbo
rf_rbo_pred = predict(rf_rbo,newdata=allvars_final_val_rbo,type="prob")
head(rf_rbo_pred)
tapply(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.81

plotLift(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(rf_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 4.80

# use PRROC to draw ROC and PR curve
rbo_1 = rf_rbo_pred$RBO[allvars_final_val_rbo$RBO_IND==1]
rbo_2 = rf_rbo_pred$RBO[allvars_final_val_rbo$RBO_IND==0]

rbo_ROC_new = roc.curve(scores.class0 = rbo_1, scores.class1 = rbo_2, curve=T)
plot(rbo_ROC_new)
rbo_PR = pr.curve(scores.class0 = rbo_1, scores.class1 = rbo_2, curve=T)
plot(rbo_PR) # PRAUC 0.40

# Confusion matrix
rf_rbo_pred_class = predict(rf_rbo,newdata=allvars_final_val_rbo,type="raw")
head(rf_rbo_pred_class)
confusionMatrix(rf_rbo_pred_class,allvars_final_val_rbo$RBO_IND,positive="RBO")

# generate gain table
rf_rbo_gain = data.frame(RBO_IND = allvars_final_val_rbo$RBO_IND,RBO_prob = rf_rbo_pred$RBO)
rf_rbo_gain = rf_rbo_gain[order(-rf_rbo_gain$RBO_prob),]
rf_rbo_gain = mutate(rf_rbo_gain,quantile=ntile(rf_rbo_gain$RBO_prob,10))
rf_rbo_gain$quantile = 11-rf_rbo_gain$quantile
rf_rbo_gain = as.data.table(rf_rbo_gain)
rf_rbo_gain_table = rf_rbo_gain[,.N,by=c("quantile","RBO_IND")]
rf_rbo_gain_table = rf_rbo_gain_table[RBO_IND==1,]
rf_rbo_gain_table$RBO_cum = cumsum(rf_rbo_gain_table$N)
rf_rbo_gain_table$gain_index = rf_rbo_gain_table$RBO_cum/(rf_rbo_gain_table$quantile*max(rf_rbo_gain_table$RBO_cum)/10)

# unqiue list of variable
rf_rbo_gain[,.N,by=quantile]
rf_rbo_list = data.frame(unique(colnames(allvars_final_val_rbo)))

rf_plot = ggplot(rf_rbo_gain_table,aes(x=quantile,y=gain_index))
rf_plot + geom_bar(stat = "identity",fill="orange") + geom_text(aes(label=round(gain_index,2)))

# output the result
write.csv(rf_rbo_list,"rf_rbo_list.csv")
write.csv(rf_rbo_gain_table,"rf_rbo_gain_table.csv")





################### GBM ########################
# NFO
system.time(gbm_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                             method="gbm",trControl=ctrl,metric="ROC")) # 106.6s
gbm_nfo
gbm_nfo_pred = predict(gbm_nfo,newdata=allvars_final_val_nfo,type="prob")
head(gbm_nfo_pred)
tapply(gbm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(gbm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.864

plotLift(gbm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(gbm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 5.20

# RBO
system.time(gbm_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                             method="gbm",trControl=ctrl,metric="ROC")) # 298.5s
gbm_rbo
gbm_rbo_pred = predict(gbm_rbo,newdata=allvars_final_val_rbo,type="prob")
head(gbm_rbo_pred)
tapply(gbm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(gbm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.77

plotLift(gbm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(gbm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 3.80

############################ Neural Network #############################
# NFO
system.time(nnet_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                              method="nnet",trControl=ctrl,metric="ROC")) # 208.2s
nnet_nfo
nnet_nfo_pred = predict(nnet_nfo,newdata=allvars_final_val_nfo,type="prob")
head(nnet_nfo_pred)
tapply(nnet_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(nnet_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.83

plotLift(nnet_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(nnet_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 4.54

# RBO
system.time(nnet_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                              method="nnet",trControl=ctrl,metric="ROC")) # 480.1s
nnet_rbo
nnet_rbo_pred = predict(nnet_rbo,newdata=allvars_final_val_rbo,type="prob")
head(nnet_rbo_pred)
tapply(nnet_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(nnet_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.72

plotLift(nnet_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(nnet_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 2.54


############################ SVM #################################
# NFO
system.time(svm_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                             method="svmLinear",trControl=ctrl,metric="ROC")) # 5782.4s
svm_nfo
svm_nfo_pred = predict(svm_nfo,newdata=allvars_final_val_nfo,type="prob")
head(svm_nfo_pred)
tapply(svm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(svm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.80

plotLift(svm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(svm_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 3.78


# RBO
system.time(svm_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                             method="svmLinear",trControl=ctrl,metric="ROC")) # heavy computation
svm_rbo
svm_rbo_pred = predict(svm_rbo,newdata=allvars_final_val_rbo,type="prob")
head(svm_rbo_pred)
tapply(svm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(svm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) #

plotLift(svm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(svm_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)

############################ Naive Bayes #################################
# NFO
system.time(nb_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                            method="nb",trControl=ctrl,metric="ROC")) # 541.2s
nb_nfo
nb_nfo_pred = predict(nb_nfo,newdata=allvars_final_val_nfo,type="prob")
head(nb_nfo_pred)
tapply(nb_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(nb_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.68

plotLift(nb_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(nb_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 2.54


# RBO
system.time(nb_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                            method="nb",trControl=ctrl,metric="ROC")) # 1117.06s
nb_rbo
nb_rbo_pred = predict(nb_rbo,newdata=allvars_final_val_rbo,type="prob")
head(nb_rbo_pred)
tapply(nb_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(nb_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) #

plotLift(nb_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(nb_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)


############################ treebag #################################
# NFO
system.time(treebag_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                                 method="treebag",trControl=ctrl,metric="ROC")) # 91.3s
treebag_nfo
treebag_nfo_pred = predict(treebag_nfo,newdata=allvars_final_val_nfo,type="prob")
head(treebag_nfo_pred)
tapply(treebag_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(treebag_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.86

plotLift(treebag_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(treebag_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 5.25


# RBO
system.time(treebag_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                                 method="treebag",trControl=ctrl,metric="ROC")) # 327s
treebag_rbo
treebag_rbo_pred = predict(treebag_rbo,newdata=allvars_final_val_rbo,type="prob")
head(treebag_rbo_pred)
tapply(treebag_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(treebag_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.78

plotLift(treebag_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(treebag_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 4.39



############################ glmboost #################################
# NFO
system.time(glmboost_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                                  method="glmboost",trControl=ctrl,metric="ROC",tuneLength=10)) # 
glmboost_nfo
glmboost_nfo_pred = predict(glmboost_nfo,newdata=allvars_final_val_nfo,type="prob")
head(glmboost_nfo_pred)
tapply(glmboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(glmboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 

plotLift(glmboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(glmboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 


# RBO
system.time(glmboost_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                                  method="glmboost",trControl=ctrl,metric="ROC")) #
glmboost_rbo
glmboost_rbo_pred = predict(glmboost_rbo,newdata=allvars_final_val_rbo,type="prob")
head(glmboost_rbo_pred)
tapply(glmboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(glmboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 

plotLift(glmboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(glmboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 


############################ adaboost #################################
# NFO
system.time(adaboost_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                                  method="adaboost",trControl=ctrl,metric="ROC")) 
adaboost_nfo
adaboost_nfo_pred = predict(adaboost_nfo,newdata=allvars_final_val_nfo,type="prob")
head(adaboost_nfo_pred)
tapply(adaboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(adaboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 

plotLift(adaboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(adaboost_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 


# RBO
system.time(adaboost_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                                  method="adaboost",trControl=ctrl,metric="ROC")) # 
adaboost_rbo
adaboost_rbo_pred = predict(adaboost_rbo,newdata=allvars_final_val_rbo,type="prob")
head(adaboost_rbo_pred)
tapply(adaboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(adaboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 

plotLift(adaboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(adaboost_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 


############################ conditional inference tree #################################
# NFO
# NFO
system.time(ctree_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                               method="ctree",trControl=ctrl,metric="ROC")) # 
ctree_nfo
ctree_nfo_pred = predict(ctree_nfo,newdata=allvars_final_val_nfo,type="prob")
head(ctree_nfo_pred)
tapply(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 

plotLift(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 


# RBO
system.time(ctree_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                               method="ctree",trControl=ctrl,metric="ROC")) # 
ctree_rbo
ctree_rbo_pred = predict(ctree_rbo,newdata=allvars_final_val_rbo,type="prob")
head(ctree_rbo_pred)
tapply(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 

plotLift(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 

