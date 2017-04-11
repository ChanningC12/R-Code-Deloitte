# set up directory
getwd()
setwd("../Desktop/")

# read in pre_cap_2_rf dataset
pre_cap_2_rf = read.csv("pre_cap_2_rf.csv",na.strings = c("","NA"))
str(pre_cap_2_rf)
pre_cap_2_rf$X = NULL
pre_cap_2_rf$NFO_IND = ifelse(pre_cap_2_rf$RESPONSE=="NFO",1,0)
pre_cap_2_rf$RBO_IND = ifelse(pre_cap_2_rf$RESPONSE=="RBO",1,0)

# randomForest
library(randomForest)
library(caret)

inTrain = createDataPartition(y=pre_cap_2_rf$NFO_IND,p=0.1,list=F)
pre_cap_2_rf_train = pre_cap_2_rf[inTrain,]

fit = randomForest(NFO_IND~.-RESPONSE-RBO_IND,mtry=3,data=pre_cap_2_rf_train)
imp = importance(fit)
imp = data.frame(imp)
imp[order(-imp$IncNodePurity),]

varImpPlot(fit)

# gbm
library(gbm)
fit_gbm = gbm(NFO_IND~.-RESPONSE-RBO_IND,
              data=pre_cap_2_rf_train,
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
best.iter = gbm.perf(fit_gbm,method="cv")
print(best.iter)
test = summary(fit_gbm)
print(summary(fit_gbm))
pretty.gbm.tree(fit_gbm)

predbst = predict(fit_gbm,n.trees=best.iter,newdata=pre_cap_2_rf_train,type = 'response')
head(predbst)
summary(predbst)

library(gbm)
fit_gbm_2 = gbm(NFO_IND~.-RESPONSE-RBO_IND,
              data=pre_cap_2_rf_train,
              distribution="gaussian", # see the help for other choices
              n.trees=best.iter, # number of trees
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
print(gbm.perf(fit_gbm_2,method="cv"))
print(summary(fit_gbm_2))















