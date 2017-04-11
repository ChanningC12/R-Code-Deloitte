################### Random Forest and GLM with only internal variables ####################
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

varList_internal = c(
  "CLAIM_COUNT_CLOSED_DYNAMIC",
  "TOTAL_AMOUNT_PAID",
  "CLAIM_COUNT_DYNAMIC",
  "RATE_INCR",
  "RATE_INCR_AFFORD",
  "IFA_GROUP_STATE_PROD_FA",
  "IFA_GROUP_STATE_PROD_FAWP",
  "IFA_GROUP_STATE_PROD_OTFA",
  "POLICY_AGE_2",
  "ANNL_PREM_BFOR",
  "CNT_FPO_PREV1",
  "CNT_RBO_PREV1",
  "RESPONSE_PRE",
  "BENEFIT_POOL_ASSET_RATIO",
  "DECISION_STAGE",
  "BENE_TRM_BFOR",
  "BIO_BEFORE_NO_BIO",
  "ORIG_DBA_BFOR",
  "BIO_AFTER_NO_BIO",
  "BIO_BEFORE_5_COMPOUND",
  "FREQ_AFTR_M",
  "FREQ_AFTR_Q",
  "FREQ_BFOR_M",
  "FREQ_BFOR_Q",
  "NUM_DECISIONS",
  "ORIG_DBA_AFTR",
  "PREM_PAID_TOT",
  "RATE_INCR_PRE",
  "STATE_AK",
  "STATE_AR",
  "STATE_AZ",
  "STATE_CT",
  "STATE_IA",
  "STATE_IN",
  "STATE_MD",
  "STATE_MI",
  "STATE_MN",
  "STATE_OR",
  "STATE_PARTNERSHIP",
  "STATE_TX",
  "STATE_UT",
  "STATE_VA",
  "STATE_WA",
  "STATE_WI",
  "STATE_WV",
  "STATE_WY",
  "TOTL_COMMUNICATED_INCR_RATE",
  "CLAIM_COUNT_NP_CLOSED_DYNAMIC",
  "CLAIM_COUNT_OPEN_DYNAMIC",
  "ANNL_PREM_AFTR",
  "BENE_TRM_AFTR",
  "BIO_AFTER_5_COMPOUND",
  "BIO_AFTER_SIMPLE",
  "BIO_BEFORE_5_SIMPLE",
  "ELIM_PRD_AFTR",
  "ELIM_PRD_BFOR",
  "FREQ_AFTR_H",
  "FREQ_AFTR_Y",
  "FREQ_BFOR_H",
  "FREQ_BFOR_Y",
  "NY_COMPARE",
  "POLICY_AGE_1",
  "POLICY_AGE_2",
  "PREM_PAID_TOT_1",
  "PREM_PAID_TOT_2",
  "PRODUCT_DESC",
  "RATE_INCR_MA",
  "RESPONSE",
  "STATE",
  "STATE_AL",
  "STATE_CA",
  "STATE_CO",
  "STATE_DC",
  "STATE_DE",
  "STATE_FL",
  "STATE_GA",
  "STATE_ID",
  "STATE_IL",
  "STATE_KS",
  "STATE_KY",
  "STATE_LA",
  "STATE_ME",
  "STATE_MO",
  "STATE_MS",
  "STATE_MT",
  "STATE_NC",
  "STATE_ND",
  "STATE_NE",
  "STATE_NJ",
  "STATE_NM",
  "STATE_NV",
  "STATE_OH",
  "STATE_OK",
  "STATE_PA",
  "STATE_RI",
  "STATE_SC",
  "STATE_SD",
  "STATE_TN",
  "ALGT_FLAG",
  "GENDER_F",
  "GENDER_M",
  "GENDER_S",
  "LIMITED_PAY",
  "PAY_TERM_10",
  "PAY_TERM_65",
  "PAY_TERM_L",
  "PREF_DISC_F",
  "PRODUCT_PLCY_LIVES"
)


allvars_trn_internal = allvars_trn[,colnames(allvars_trn) %in% varList_internal]













