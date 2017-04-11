# set up directory
getwd()
setwd("D:/Channing/")

# read in allvars_capped dataset
allvars_capped = read.csv("allvars_capped.csv",na.strings = c("","NA"))
str(allvars_capped)

# Load in packages
library(randomForest)
library(caret)
library(gbm)
library(rpart)

######### gbm - NFO
inTrain_gbm = createDataPartition(allvars_capped$NFO_IND,p=0.25,list=F)
allvars_capped_train = allvars_capped[inTrain_gbm,]

fit_gbm = gbm(NFO_IND~.-PLCY_REF-GENDER_S-IF_RED_BENPERIOD-IF_RED_BIO-IF_RED_DBA-IF_RED_ELIM
              -NY_COMPARE-PHASE-POLICY_AGE_1-PREM_PAID_TOT_1-PRODUCT_DESC
              -RBO_NUM_CHANGES-ROUND_PHASE_IND-ROUND-RESPONSE-RBO_IND-FILLER-MISSING_RESPONSE_IND-CLIENT_ZIP
              -DECISION_SPLIT-MISSING_KBM_IND-LANDING_SPOT-IF_COUPLE_ISS_DISC,
              data=allvars_capped_train,
              distribution="gaussian", # see the help for other choices
              n.trees=4000, # number of trees
              shrinkage=0.01, # shrinkage or learning rate,
              # 0.001 to 0.1 usually work
              #              interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
              bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
              train.fraction = 1,
              n.minobsinnode = 10, # minimum total weight needed in each node
              #              cv.folds = 5, # do 3-fold cross-validation
              #              keep.data=TRUE, # keep a copy of the dataset with the object
              verbose=FALSE, # don't print out progress
              n.cores=1)

best.iter = gbm.perf(fit_gbm,method="cv")
print(best.iter)
test = summary(fit_gbm)
print(summary(fit_gbm))
pretty.gbm.tree(fit_gbm)



######## CART and RF
# for randomForest, impute missing values: numerical to median, character to mode
allvars_capped_rf = allvars_capped

Mode = function (x, na.rm) {
  xtab = table(x)
  xmode = names(which(xtab == max(xtab)))
  if (length(xmode) > 1)
    xmode = ">1 mode"
  return(xmode)
}

for (x in 1:ncol(allvars_capped_rf)) {
  if(class(allvars_capped_rf[,x]) %in% c("numeric","integer")) {
    allvars_capped_rf[is.na(allvars_capped_rf[,x]),x] = median(allvars_capped_rf[,x],na.rm=T)
  }
  else if(class(allvars_capped_rf[,x]) == "factor") {
    allvars_capped_rf[is.na(allvars_capped_rf[,x]),x] = Mode(allvars_capped_rf[,x],na.rm=T)
  }
}

# subset the data, filter out irrelevant columns
allvars_capped_rf = subset(allvars_capped_rf,select=-c(PLCY_REF,GENDER_S,IF_RED_BENPERIOD,IF_RED_BIO,IF_RED_DBA,IF_RED_ELIM
                                                ,NY_COMPARE,PHASE,POLICY_AGE_1,PREM_PAID_TOT_1,PRODUCT_DESC
                                                ,RBO_NUM_CHANGES,ROUND_PHASE_IND,ROUND,RESPONSE,RBO_IND,FILLER,MISSING_RESPONSE_IND,CLIENT_ZIP
                                                ,DECISION_SPLIT,MISSING_KBM_IND,LANDING_SPOT,IF_COUPLE_ISS_DISC))

# CART
fit_cart = rpart(as.factor(NFO_IND)~.,data=allvars_capped_rf)
var_imp = data.frame(varImp(fit_cart))
write.csv(var_imp,"var_imp_cart.csv")


# random forest - NFO
fit_rf = randomForest(NFO_IND~.,
                      data=allvars_capped_rf,
                      mtry=floor(sqrt(ncol(allvars_capped_rf))),
                      ntree=1000)
imp = importance(fit_rf)
imp = data.frame(imp)
imp[order(-imp$IncNodePurity),]
varImpPlot(fit_rf)
















