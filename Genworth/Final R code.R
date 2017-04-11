setwd("C:\\Users\\sakssethi\\Documents\\Life_Insurance_Competition\\data")
getwd()
data <- read.csv("Final_ds.csv",header=T)
test <- data[3176:7149,]
train_data <- data[1:3175,]
set.seed(12345)
train_data$BIN_SEV_CARDIO<-train_data$BINARY_CARDIO_TRANS*train_data$SEV_CARDIO_RAWSCR
train_data$BIN_SEV_CERV<-train_data$BINARY_CERV_TRANS*train_data$SEV_CERV_RAWSCR
train_data$BIN_SEV_DIAB<-train_data$BINARY_DIAB_TRANS*train_data$SEV_DIAB_RAWSCR
train_data$BIN_SEV_DRUG<-train_data$BINARY_DRUG_TRANS*train_data$SEV_DRUG_RAWSCR
train_data$BIN_SEV_HERNIA<-train_data$BINARY_HERNIA_TRANS*train_data$SEV_HERNIA_RAWSCR
train_data$BIN_SEV_HYPT<-train_data$BINARY_HYPT_TRANS*train_data$SEV_HYPT_RAWSCR
train_data$BIN_SEV_MENTAL<-train_data$BINARY_MENTAL_TRANS*train_data$SEV_MENTAL_RAWSCR
train_data$BIN_SEV_MUSC<-train_data$BINARY_MUSC_TRANS*train_data$SEV_MUSC_RAWSCR
train_data$BIN_SEV_NF<-train_data$BINARY_NF_TRANS*train_data$SEV_NF_RAWSCR
train_data$BIN_SEV_NM<-train_data$BINARY_NM_TRANS*train_data$SEV_NM_RAWSCR
train_data$BIN_SEV_NO<-train_data$BINARY_NO_TRANS*train_data$SEV_NO_RAWSCR
train_data$BIN_SEV_AC<-train_data$BINARY_AC_TRANS*train_data$SEV_AC_RAWSCR
train_data$BIN_SEV_NT<-train_data$BINARY_NT_TRANS*train_data$SEV_NT_RAWSCR
train_data$BIN_SEV_PREG<-train_data$BINARY_PREG_TRANS*train_data$SEV_PREG_RAWSCR
train_data$BIN_SEV_RESP<-train_data$BINARY_RESP_TRANS*train_data$SEV_RESP_RAWSCR

test$BIN_SEV_AC<-test$BINARY_AC_TRANS*test$SEV_AC_RAWSCR
test$BIN_SEV_CARDIO<-test$BINARY_CARDIO_TRANS*test$SEV_CARDIO_RAWSCR
test$BIN_SEV_CERV<-test$BINARY_CERV_TRANS*test$SEV_CERV_RAWSCR
test$BIN_SEV_DIAB<-test$BINARY_DIAB_TRANS*test$SEV_DIAB_RAWSCR
test$BIN_SEV_DRUG<-test$BINARY_DRUG_TRANS*test$SEV_DRUG_RAWSCR
test$BIN_SEV_HERNIA<-test$BINARY_HERNIA_TRANS*test$SEV_HERNIA_RAWSCR
test$BIN_SEV_HYPT<-test$BINARY_HYPT_TRANS*test$SEV_HYPT_RAWSCR
test$BIN_SEV_MENTAL<-test$BINARY_MENTAL_TRANS*test$SEV_MENTAL_RAWSCR
test$BIN_SEV_MUSC<-test$BINARY_MUSC_TRANS*test$SEV_MUSC_RAWSCR
test$BIN_SEV_NF<-test$BINARY_NF_TRANS*test$SEV_NF_RAWSCR
test$BIN_SEV_NM<-test$BINARY_NM_TRANS*test$SEV_NM_RAWSCR
test$BIN_SEV_NO<-test$BINARY_NO_TRANS*test$SEV_NO_RAWSCR
test$BIN_SEV_NT<-test$BINARY_NT_TRANS*test$SEV_NT_RAWSCR
test$BIN_SEV_PREG<-test$BINARY_PREG_TRANS*test$SEV_PREG_RAWSCR
test$BIN_SEV_RESP<-test$BINARY_RESP_TRANS*test$SEV_RESP_RAWSCR




train_data$Body_Mass_Index <- ifelse(train_data$Body_Mass_Index <= 18,18,ifelse(train_data$Body_Mass_Index >= 65,65,train_data$Body_Mass_Index
))
test$Body_Mass_Index <- ifelse(test$Body_Mass_Index <= 18,18,ifelse(test$Body_Mass_Index >= 65,65,test$Body_Mass_Index
))
train_data$MVR <- ifelse(train_data$MultipleVehichleViolations %in% 'Yes',1,ifelse(train_data$MultipleVehichleViolations %in% 'No',2,3))
test$MVR <- ifelse(test$MultipleVehichleViolations %in% 'Yes',1,ifelse(test$MultipleVehichleViolations %in% 'No',2,3))
train_data$smoker_bmi<- train_data$Smoker*train_data$Body_Mass_Index
test$smoker_bmi<- test$Smoker*test$Body_Mass_Index

train_data$smoker_medical_bmi<- train_data$Smoker*train_data$Body_Mass_Index*train_data$Medical
test$smoker_medical_bmi<- test$Smoker*test$Body_Mass_Index*test$Medical


train_data$smoker_MVR<- train_data$Smoker*train_data$MVR
test$smoker_MVR<- test$Smoker*test$MVR

train_data$NT_MVR<- train_data$SEV_NT_RAWSCR*train_data$MVR
test$NT_MVR<- test$SEV_NT_RAWSCR*test$MVR

train_data$NT_MVR_smoker<- train_data$SEV_NT_RAWSCR*train_data$MVR*train_data$Smoker
test$NT_MVR_smoker<- test$SEV_NT_RAWSCR*test$MVR*test$Smoker

train_data$NT_resp_smoker<- train_data$SEV_NT_RAWSCR*train_data$SEV_RESP_RAWSCR
test$NT_resp_smoker<- test$SEV_NT_RAWSCR*test$SEV_RESP_RAWSCR





train <- subset(train_data,select = -c(UNIQUE_ID,
                                       Stnd_UW_Class,
                                       Smoker,
                                       POEP,
                                       APT,
                                       MOBPLUS,
                                       RC,
                                       VETERAN,
                                       CA00,
                                       ONLA,
                                       IPVM,
                                       IRVM,
                                       IDC,
                                       IHCR,
                                       IHCI,
                                       IHCO,
                                       IHGA,
                                       IHHI,
                                       IHLO,
                                       IHSW,
                                       IHWI,
                                       IOCE,
                                       IOLP,
                                       IMBO,
                                       IMCO,
                                       IMFO,
                                       IMGI,
                                       IMPS,
                                       IMDV,
                                       INVE,
                                       INLI,
                                       INMU,
                                       INSB,
                                       IMCU,
                                       IMSO,
                                       IOTR,
                                       IREA,
                                       IRBF,
                                       IRCK,
                                       IRPE,
                                       IDCH,
                                       IDHE,
                                       IDRE,
                                       ISOT,
                                       ISBB,
                                       ISBK,
                                       ISHU,
                                       ISWA,
                                       ISRU,
                                       ISPO,
                                       ITPI,
                                       IVCR,
                                       IDVIT,
                                       PRESCA,
                                       SPEA,
                                       FINB,
                                       CSR,
                                       HOMO,
                                       PDPE,
                                       SGDI,
                                       SGFA,
                                       SGLL,
                                       CONSHOP,
                                       SOHOHH,
                                       N5059,
                                       N4049,
                                       N3039,
                                       N2029,
                                       IMBBND,
                                       IMALT,
                                       IHEX,
                                       IHCLF,
                                       BONDOLLR,
                                       OAGEN,
                                       OAWOMEN,
                                       ONOVEL,
                                       OCHILDPR,
                                       OCRAFTS,
                                       OGIFT,
                                       OSTATION,
                                       OHMFURN,
                                       OFURNITR,
                                       OHSWARE,
                                       OLINENS,
                                       OGARDEN,
                                       OJEWELRY,
                                       OOTHER,
                                       OPETS,
                                       BPAMEX,
                                       BPCREDIT,
                                       BPDISCVR,
                                       BUYER,
                                       POC19,
                                       IODIGITL,
                                       IHDIY,
                                       IHGREEN,
                                       IOATV,
                                       IHIGHSCL,
                                       PRESHI,
                                       PRESMI,
                                       PRESRI,
                                       PRESTI,
                                       HIR,
                                       CE,
                                       OILC,
                                       RORDERS,
                                       RAGEN,
                                       RACHILD,
                                       RATEEN,
                                       RAWOMEN,
                                       RCHILDPR,
                                       RHMCARE,
                                       RHMFURN,
                                       RHSWARE,
                                       RJEWELRY,
                                       RETAIL,
                                       AACD,
                                       AALZ,
                                       AART,
                                       AASM,
                                       AAST,
                                       ABED,
                                       ABLD,
                                       ABCE,
                                       ABRC,
                                       ACLD,
                                       ADNT,
                                       ADBT,
                                       ADT2,
                                       AFTA,
                                       AGRD,
                                       AHAR,
                                       APAN,
                                       ARES,
                                       PALLERG,
                                       PARTHITS,
                                       PBACKPN,
                                       PDEPRESS,
                                       PDIABETE,
                                       PECZEMA,
                                       PHRTACID,
                                       PHGCHOLS,
                                       PNSLALRG,
                                       POBESITY,
                                       PSINUSIT,
                                       PWEIGHT,
                                       AILMENTS,
                                       PCORRECT,
                                       PHEARTRX,
                                       AGVSN,
                                       C210MAH,
                                       C210HMI,
                                       C210CIP,
                                       C210PSU,
                                       C210WHT,
                                       C210EBI,
                                       HH_TOTALALLCREDIT_60DPD,
                                       NUM_TOTALALLCREDIT,
                                       NUM_TOTALALLCREDIT_60DPD,
                                       BAL_TOTALALLCREDIT_NEW,
                                       PRCNT_TOTALALLCREDIT,
                                       HH_TOTALALLCREDIT_30TO59DPD,
                                       HH_TOTALALLCREDIT_60TO89DPD,
                                       HH_TOTALALLCREDIT_COLLECTIONS,
                                       HH_TOTALALLCREDIT_SEVEREDEROG,
                                       HH_TOTALALLCREDIT_BANKRUPTCY,
                                       NUM_TOTALALLCREDIT_30TO59DPD,
                                       NUM_TOTALALLCREDIT_60TO89DPD,
                                       NUM_TOTALALLCREDIT_BANKRUPTCY,
                                       BAL_TOTALALLCREDIT_60TO89DPD,
                                       BAL_TOTALALLCREDIT_SEVEREDEROG,
                                       BAL_TOTALALLCREDIT_BANKRUPTCY,
                                       HH_MTGCREDIT,
                                       HH_MTGCREDIT_NEW,
                                       HH_MTG_60TO89DPD,
                                       NUM_MTG_30TO59DPD,
                                       NUM_MTG_60TO89DPD,
                                       BAL_MTG_60TO89DPD,
                                       BAL_MTG_FORECLOSURE,
                                       BAL_MTG_BANKRUPTCY,
                                       HH_1STMTGCREDIT,
                                       HH_1STMTG_30TO59DPD,
                                       HH_1STMTG_60TO89DPD,
                                       HH_1STMTG_FORECLOSURE,
                                       NUM_1STMTG_30TO59DPD,
                                       NUM_1STMTG_BANKRUPTCY,
                                       BAL_1STMTG_30TO59DPD,
                                       BAL_1STMTG_60TO89DPD,
                                       BAL_1STMTG_COLLECTIONS,
                                       BAL_1STMTG_BANKRUPTCY,
                                       HH_AGENCYFIRSTMTG_60DPD,
                                       NUM_AGENCYFIRSTMTG_60DPD,
                                       HH_AGENCY1STMORG_30TO59DPD,
                                       NUM_AGENCY1STMORG_30TO59DPD,
                                       BAL_AGENCY1STMORG_30TO59DPD,
                                       BAL_AGENCY1STMORG_90TO119DP,
                                       BAL_AGENCY1STMORG_COLLECTIO,
                                       BAL_AGENCY1STMORG_FORECLOSU,
                                       BAL_AGENCY1STMORG_BANKRUPTC,
                                       HH_NONAGNFIRSTMTG,
                                       HH_NONAGNFIRSTMTG_NEW,
                                       HH_NONAGNFIRSTMTG_60DPD,
                                       NUM_NONAGNFIRSTMTG_NEW,
                                       NUM_NONAGNFIRSTMTG_60DPD,
                                       PRCNT_NONAGNFIRSTMTG,
                                       HIGHCRD_NONAGNFIRSTMORT_NEW,
                                       HH_HELOC,
                                       NUM_HELOC,
                                       NUM_HELOC_NEW,
                                       BAL_HELOC_NEW,
                                       HIGHCRD_HELOC,
                                       HIGHCRD_HELOC_NEW,
                                       HH_HOMEEQUITYLOANS,
                                       HH_HOMEEQUITYLOANS_NEW,
                                       HH_HOMEEQUITYLOANS_60DPD,
                                       NUM_HOMEEQUITYLOANS,
                                       MINMOB_HOMEEQUITYLOANS,
                                       HIGHCRD_HOMEEQUITYLOANS_NEW,
                                       BAL_NONMTGCREDIT,
                                       HIGHCRD_BANKCARDCREDIT_NEW,
                                       HH_BANKCARD_60TO89DPD,
                                       HH_BANKCARD_COLLECTIONS,
                                       NUM_BANKCARD_30TO59DPD,
                                       NUM_BANKCARD_60TO89DPD,
                                       NUM_BANKCARD_COLLECTIONS,
                                       BAL_BANKCARD_30TO59DPD,
                                       BAL_BANKCARD_60TO89DPD,
                                       BAL_BANKCARD_90TO119DPD,
                                       BAL_BANKCARD_BANKRUPTCY,
                                       HH_RETAILCREDIT,
                                       HH_RETAILCREDIT_60DPD,
                                       NUM_RETAILCREDIT_60DPD,
                                       HIGHCRD_RETAILCREDIT,
                                       PRCNT_RETAILCREDIT,
                                       MINMOB_RETAILCREDIT,
                                       HH_AUTOFINANCE_60DPD,
                                       NUM_AUTOFINANCE_60DPD,
                                       BAL_AUTOFINANCE_60DPD,
                                       NUM_STUDENTLOAN_60DPD,
                                       HH_AUTOBANK_60DPD,
                                       NUM_AUTOBANK_60DPD,
                                       HH_CONSUMERFINANCE_60DPD,
                                       NUM_CONSUMERFINANCE_60DPD,
                                       BAL_CONSUMERFINANCE_60DPD,
                                       HH_OTHERNONMTG_60DPD,
                                       NUM_OTHERNONMTG,
                                       NUM_OTHERNONMTG_60DPD,
                                       INQUIRY_AUTO,
                                       INQUIRY_CONSUMERFINANCE,
                                       PRCNT_NEWMTGTRADE,
                                       ZAGG_C210WHT,
                                       ZAGG_C210BLU,
                                       ZAGG_C210PMR,
                                       ZAGG_C210EBI,
                                       ZAGG_C210HVA,
                                       ZAGG_C210HMI,
                                       ZAGG_CA11,
                                       ZAGG_VAC,
                                       ZAGG_APT_T,
                                       ZAGG_ESTINC30_007,
                                       ZAGG_ESTINC30_067,
                                       ZAGG_ESTINC30_174,
                                       ZAGG_ESTINC30_224,
                                       ZAGG_HHCOMP_G,
                                       ZAGG_HHCOMP_F,
                                       ZAGG_HHCOMP_B,
                                       ZAGG_HHCOMP_J,
                                       ZAGG_MR_M,
                                       ZAGG_RC_M,
                                       ZAGG_TUOC_A,
                                       ZAGG_TUOC_O,
                                       ZAGG_TUOC_C,
                                       ZAGG_TUOC_D,
                                       ZAGG_TUOC_B,
                                       ZAGG_TUOC_L,
                                       ZAGG_PUBH,
                                       ZAGG_MR_Y,
                                       ZAGG_ATV,
                                       ZAGG_BPAMEXY,
                                       ZAGG_BPCREDIT,
                                       ZAGG_BPMASTER,
                                       ZAGG_BPVISA,
                                       ZAGG_OATEEN,
                                       ZAGG_OARTS,
                                       ZAGG_OSPORTS,
                                       ZAGG_CO,
                                       ZAGG_CCW,
                                       ZAGG_FINB,
                                       ZAGG_GROC,
                                       ZAGG_SRET,
                                       ZAGG_TRAD,
                                       ZAGG_IDVIT,
                                       ZAGG_IDWGHT,
                                       ZAGG_INMEDI,
                                       ZAGG_ICCO,
                                       ZAGG_IDC,
                                       ZAGG_IDEN,
                                       ZAGG_IDHE,
                                       ZAGG_IDVE,
                                       ZAGG_IGC,
                                       ZAGG_IHCR,
                                       ZAGG_IHCO,
                                       ZAGG_IHCF,
                                       ZAGG_IHGA,
                                       ZAGG_IHKN,
                                       ZAGG_IHQU,
                                       ZAGG_IHSE,
                                       ZAGG_IHWO,
                                       ZAGG_INOT,
                                       ZAGG_IMBO,
                                       ZAGG_IMBM,
                                       ZAGG_IMCH,
                                       ZAGG_IMCL,
                                       ZAGG_IMHO,
                                       ZAGG_IMOT,
                                       ZAGG_IMOR,
                                       ZAGG_IOOW,
                                       ZAGG_IORE,
                                       ZAGG_IDOG,
                                       ZAGG_ISBB,
                                       ZAGG_ISHO,
                                       ZAGG_SGFA,
                                       ZAGG_SGHL,
                                       ZAGG_SGOE,
                                       ZAGG_SGPA,
                                       ZAGG_SGSE,
                                       ZAGG_SGTC,
                                       ZAGG_SGUP,
                                       ZAGG_BANK,
                                       ZAGG_SOHOHH,
                                       ZAGG_SOHOIN,
                                       ZAGG_HOMSTAT_Y,
                                       ZAGG_HOMSTAT_R,
                                       ZAGG_HOMSTAT_U,
                                       ZAGG_NOC19_0,
                                       ZAGG_NOC19_1,
                                       ZAGG_NOC19_2,
                                       ZAGG_NOC19_4,
                                       ZAGG_IHBEAD,
                                       ZAGG_IHDIY,
                                       ZAGG_IHGREEN,
                                       ZAGG_IRBKCLUB,
                                       ZAGG_ISROLLER,
                                       ZAGG_PRESHI,
                                       ZAGG_IHGF,
                                       ZAGG_IHGV,
                                       ZAGG_PRESMI,
                                       ZAGG_PRESTI,
                                       ZAGG_FAMP_C,
                                       ZAGG_FAMP_W,
                                       ZAGG_ESTDII30_0,
                                       ZAGG_ESTDII30_9,
                                       ZAGG_PRESCA,
                                       ZAGG_CALAGE,
                                       ZAGG_TUSR,
                                       ZAGG_SPENDPAT_10,
                                       ZAGG_SPENDPAT_11,
                                       ZAGG_SPENDPAT_14,
                                       ZAGG_SPENDPAT_15,
                                       ZAGG_RDOLLARS,
                                       ZAGG_RABIGTLL,
                                       ZAGG_RATEEN,
                                       ZAGG_RARTS,
                                       ZAGG_RBEAUTY,
                                       ZAGG_RCOLLECT,
                                       ZAGG_RCOMPHMO,
                                       ZAGG_RCRAFTS,
                                       ZAGG_RELECTNC,
                                       ZAGG_RGARDEN,
                                       ZAGG_RHOLIDAY,
                                       ZAGG_RHSWARE,
                                       ZAGG_ROTHER,
                                       ZAGG_RPHOTO,
                                       ZAGG_RSPECFD,
                                       ZAGG_RSTATION,
                                       ZAGG_BUYER,
                                       ZAGG_RETAIL,
                                       ZAGG_AACD,
                                       ZAGG_ACNC,
                                       ZAGG_ACNK,
                                       ZAGG_ACFS,
                                       ZAGG_ACRN,
                                       ZAGG_ADTU,
                                       ZAGG_AFTA,
                                       ZAGG_AKID,
                                       ZAGG_ALNG,
                                       ZAGG_AMLS,
                                       ZAGG_ANOS,
                                       ZAGG_ARES,
                                       ZAGG_ASTK,
                                       ZAGG_AGRESP,
                                       ZAGG_PARTRSCS,
                                       ZAGG_PBACKPN,
                                       ZAGG_PBRSTCAN,
                                       ZAGG_PCLDSORE,
                                       ZAGG_PDIATYP1,
                                       ZAGG_PNLFUNGS,
                                       ZAGG_PPROSCAN,
                                       ZAGG_PSNORING,
                                       ZAGG_ABBA,
                                       ZAGG_ADIS,
                                       ZAGG_ACDE,
                                       ZAGG_PHBPMED,
                                       ZAGG_POSTMEDS,
                                       ZAGG_PSCAR,
                                       ZAGG_PVIAGRA,
                                       ZAGG_RACE_B,
                                       ZAGG_RACE_H,
                                       ZAGG_RACE_N,
                                       ZAGG_RACE_J,
                                       ZAGG_RACE_P,
                                       Driving_Record,
                                       BINARY_AC_TRANS,
                                       BINARY_DRUG_TRANS,
                                       BINARY_HERNIA_TRANS,
                                       BINARY_MUSC_TRANS,
                                       BINARY_NF_TRANS,
                                       BINARY_NM_TRANS,
                                       BINARY_PREG_TRANS,
                                       BINARY_RESP_TRANS,
                                       BINARY_ASTHMA_TRANS,
                                       SEV_HERNIA_RAWSCR,
                                       SEV_MUSC_RAWSCR,
                                       SEV_NO_RAWSCR,
                                       SEV_NT_RAWSCR,
                                       SEV_RESP_RAWSCR,
                                       SEV_AC_RAWSCR,
                                       SEV_DIAB_RAWSCR,
                                       SEV_PREG_RAWSCR,
                                       TUOCS_FD,
                                       BIN_SEV_DRUG,
                                       BIN_SEV_MUSC,
                                       BIN_SEV_AC,
                                       BIN_SEV_NT,
                                       BIN_SEV_PREG,
                                       BIN_SEV_RESP,
                                       MVR,
                                       #CORRELATED VARS
                                       BIN_SEV_HYPT,
                                       BIN_SEV_MENTAL,
                                       BIN_SEV_DIAB,
                                       BIN_SEV_NO,
                                       BIN_SEV_CERV,
                                       BIN_SEV_CARDIO,
                                       ASNR,
                                       
                                       ACFS,ABRT,ACNS
                                       
                                       
))

install.packages("gbm")
library(gbm)
set.seed(12345)
#Uploaded this : Submission 5
gbm1 <-
  gbm(risk_class_num~. , # formula
      data=train, # dataset
      distribution="gaussian", # see the help for other choices
      n.trees=4000, # number of trees
      shrinkage=0.001, # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
      train.fraction = 1,
      n.minobsinnode = 8, # minimum total weight needed in each node
      cv.folds = 5, # do 3-fold cross-validation
      keep.data=TRUE, # keep a copy of the dataset with the object
      verbose=FALSE, # don't print out progress
      n.cores=1)
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
predbst <- predict(gbm1,n.trees=best.iter,newdata=train,type = 'response')
#p.predBST <- apply(predbst, 1, which.max)
head(predbst)
install.packages("Metrics")
library(Metrics)
p.pred <- round(predbst)
head(p.pred)
rmse(p.pred,as.numeric(train$risk_class_num))
predbst1 <- predict(gbm1,n.trees=best.iter,newdata=test,type = 'response')
p.pred1 <- round(predbst1)
rmse(p.pred1,as.numeric(test$risk_class_num))
write.csv(as.matrix(p.pred1),"pred207.csv")

set.seed(12345)
#Uploaded this : Submission 5
gbm1 <-
  gbm(risk_class_num~. , # formula
      data=train, # dataset
      distribution="gaussian", # see the help for other choices
      n.trees=4000, # number of trees
      shrinkage=0.001, # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
      train.fraction = 1,
      n.minobsinnode = 10, # minimum total weight needed in each node
      cv.folds = 5, # do 3-fold cross-validation
      keep.data=TRUE, # keep a copy of the dataset with the object
      verbose=FALSE, # don't print out progress
      n.cores=1)
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
predbst <- predict(gbm1,n.trees=best.iter,newdata=train,type = 'response')
#p.predBST <- apply(predbst, 1, which.max)
head(predbst)
install.packages("Metrics")
library(Metrics)
p.pred <- round(predbst)
head(p.pred)
rmse(p.pred,as.numeric(train$risk_class_num))
predbst1 <- predict(gbm1,n.trees=best.iter,newdata=test,type = 'response')
p.pred1 <- round(predbst1)
rmse(p.pred1,as.numeric(test$risk_class_num))
write.csv(as.matrix(p.pred1),"pred208.csv")

set.seed(12345)
#Uploaded this : Submission 5
gbm1 <-
  gbm(risk_class_num~. , # formula
      data=train, # dataset
      distribution="gaussian", # see the help for other choices
      n.trees=5000, # number of trees
      shrinkage=0.001, # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
      train.fraction = 1,
      n.minobsinnode = 8, # minimum total weight needed in each node
      cv.folds = 5, # do 3-fold cross-validation
      keep.data=TRUE, # keep a copy of the dataset with the object
      verbose=FALSE, # don't print out progress
      n.cores=1)
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
predbst <- predict(gbm1,n.trees=best.iter,newdata=train,type = 'response')
#p.predBST <- apply(predbst, 1, which.max)
head(predbst)
install.packages("Metrics")
library(Metrics)
p.pred <- round(predbst)
head(p.pred)
rmse(p.pred,as.numeric(train$risk_class_num))
predbst1 <- predict(gbm1,n.trees=best.iter,newdata=test,type = 'response')
p.pred1 <- round(predbst1)
rmse(p.pred1,as.numeric(test$risk_class_num))
write.csv(as.matrix(p.pred1),"pred209.csv")

set.seed(12345)
#Uploaded this : Submission 5
gbm1 <-
  gbm(risk_class_num~. , # formula
      data=train, # dataset
      distribution="gaussian", # see the help for other choices
      n.trees=4000, # number of trees
      shrinkage=0.01, # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=6, # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
      train.fraction = 1,
      n.minobsinnode = 8, # minimum total weight needed in each node
      cv.folds = 5, # do 3-fold cross-validation
      keep.data=TRUE, # keep a copy of the dataset with the object
      verbose=FALSE, # don't print out progress
      n.cores=1)
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
predbst <- predict(gbm1,n.trees=best.iter,newdata=train,type = 'response')
#p.predBST <- apply(predbst, 1, which.max)
head(predbst)
install.packages("Metrics")
library(Metrics)
p.pred <- round(predbst)
head(p.pred)
rmse(p.pred,as.numeric(train$risk_class_num))
predbst1 <- predict(gbm1,n.trees=best.iter,newdata=test,type = 'response')
p.pred1 <- round(predbst1)
rmse(p.pred1,as.numeric(test$risk_class_num))
write.csv(as.matrix(p.pred1),"pred210.csv")

set.seed(12345)
#Uploaded this : Submission 5
gbm1 <-
  gbm(risk_class_num~. , # formula
      data=train, # dataset
      distribution="gaussian", # see the help for other choices
      n.trees=20000, # number of trees
      shrinkage=0.001, # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
      train.fraction = 1,
      n.minobsinnode = 10, # minimum total weight needed in each node
      cv.folds = 5, # do 3-fold cross-validation
      keep.data=TRUE, # keep a copy of the dataset with the object
      verbose=FALSE, # don't print out progress
      n.cores=1)
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
predbst <- predict(gbm1,n.trees=best.iter,newdata=train,type = 'response')
#p.predBST <- apply(predbst, 1, which.max)
head(predbst)
install.packages("Metrics")
library(Metrics)
p.pred <- round(predbst)
head(p.pred)
rmse(p.pred,as.numeric(train$risk_class_num))
predbst1 <- predict(gbm1,n.trees=best.iter,newdata=test,type = 'response')
p.pred1 <- round(predbst1)
rmse(p.pred1,as.numeric(test$risk_class_num))
write.csv(as.matrix(p.pred1),"pred211.csv")

install.packages("gbm")
library(gbm)
set.seed(12345)
#Uploaded this : Submission 5
gbm1 <-
  gbm(risk_class_num~. , # formula
      data=train, # dataset
      distribution="gaussian", # see the help for other choices
      n.trees=4000, # number of trees
      shrinkage=0.01, # shrinkage or learning rate,
      # 0.001 to 0.1 usually work
      interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
      train.fraction = 1,
      n.minobsinnode = 10, # minimum total weight needed in each node
      cv.folds = 5, # do 3-fold cross-validation
      keep.data=TRUE, # keep a copy of the dataset with the object
      verbose=FALSE, # don't print out progress
      n.cores=1)
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
predbst <- predict(gbm1,n.trees=best.iter,newdata=train,type = 'response')
#p.predBST <- apply(predbst, 1, which.max)
head(predbst)
install.packages("Metrics")
library(Metrics)
p.pred <- round(predbst)
head(p.pred)
rmse(p.pred,as.numeric(train$risk_class_num))
predbst1 <- predict(gbm1,n.trees=best.iter,newdata=test,type = 'response')
p.pred1 <- round(predbst1)
rmse(p.pred1,as.numeric(test$risk_class_num))
write.csv(as.matrix(p.pred1),"pred212.csv")
