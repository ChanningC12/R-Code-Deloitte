getwd()
setwd("../Desktop/")
# read in data
model_simple = read.csv("SIMPLE_MODEL.csv")
str(model_simple)
table(model_simple$NFO_IND)
table(model_simple$RBO_IND)

# detect correlation
library(corrplot)
corrplot(cor(model_simple),method="number")

# separate into train/test/validation
trntst_ind = sample(1:nrow(model_simple),floor(0.7*nrow(model_simple)))
model_simple_trntst = model_simple[trntst_ind,]
model_simple_val = model_simple[-trntst_ind,]

# build logistic regression on trntst
model_logit = glm(NFO_IND~.-RBO_IND,data=model_simple_trntst,family="binomial")
summary(model_logit)

# append probability to the val dataset
model_simple_val$NFO_PROB = predict(model_logit,newdata = model_simple_val, type="response")
tapply(model_simple_val$NFO_PROB,model_simple_val$NFO_IND,mean)

# Confusion matrix - cutoff 0.3
library(plyr)
library(dplyr)
library(caret)
# Pipe operator: The rules are simple: the object on the left hand side 
# is passed as the first argument to the function on the right hand side
model_simple_val = mutate(model_simple_val, NFO_decile = ntile(model_simple_val$NFO_PROB,10))
model_simple_val$NFO_PRED_TOP30 = ifelse(model_simple_val$NFO_decile>7,1,0)
confusionMatrix(model_simple_val$NFO_PRED_TOP30,model_simple_val$NFO_IND,positive="1")

# ROC curve
library(ROCR)
nfo_pred = prediction(prediction = model_simple_val$NFO_PROB,labels = model_simple_val$NFO_IND)
nfo_perf = performance(nfo_pred,measure = "tpr",x.measure = "fpr")
plot(nfo_perf, main = "ROC Curve - NFO")
nfo_perf_auc = performance(nfo_pred, measure="auc")
unlist(nfo_perf_auc@y.values)

# Lift curve
library(lift)
plotLift(model_simple_val$NFO_PROB, model_simple_val$NFO_IND,cumulative = T, n.buckets = 100)
TopDecileLift(model_simple_val$NFO_PROB, model_simple_val$NFO_IND) # top 10% lift

# Do the same for RBO
# build logistic regression on trntst
model_logit_rbo = glm(RBO_IND~.-NFO_IND,data=model_simple_trntst,family="binomial")
summary(model_logit_rbo)

# append probability to the val dataset
model_simple_val$RBO_PROB = predict(model_logit_rbo,newdata = model_simple_val, type="response")
tapply(model_simple_val$RBO_PROB,model_simple_val$RBO_IND,mean)

# Confusion matrix - cutoff 0.3
model_simple_val = mutate(model_simple_val, RBO_decile = ntile(model_simple_val$RBO_PROB,10))
model_simple_val$RBO_PRED_TOP30 = ifelse(model_simple_val$RBO_decile>7,1,0)
confusionMatrix(model_simple_val$RBO_PRED_TOP30,model_simple_val$RBO_IND,positive="1")

# ROC curve
rbo_pred = prediction(prediction = model_simple_val$RBO_PROB,labels = model_simple_val$RBO_IND)
rbo_perf = performance(rbo_pred,measure = "tpr",x.measure = "fpr")
plot(rbo_perf, main = "ROC Curve - RBO")
rbo_perf_auc = performance(rbo_pred, measure="auc")
unlist(rbo_perf_auc@y.values)

# Lift curve
plotLift(model_simple_val$RBO_PROB, model_simple_val$RBO_IND,cumulative = T, n.buckets = 100)
TopDecileLift(model_simple_val$RBO_PROB, model_simple_val$RBO_IND) # top 10% lift














