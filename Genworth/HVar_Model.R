################################ Simple Model with all "H" Variables ##################################
library(plyr)
library(ROCR)
library(lift)
library(caret)
library(OptimalCutpoints)
library(MASS)
library(DMwR)
## Clean up and set directory
rm(list=ls())
getwd()
setwd("../Desktop/")

## Read in data
H_var = read.csv("HVar.csv")
str(H_var)

## Check missing values for each column
for (Var in names(H_var)) {
  missing <- sum(is.na(H_var[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

# Impute missing with 0 for AVG_AMOUNT_PAID and RATE_INCR_PRE
H_var_filter = H_var
H_var_filter[is.na(H_var_filter$AVG_AMOUNT_PAID),"AVG_AMOUNT_PAID"]=0
count(H_var_filter$AVG_AMOUNT_PAID)

H_var_filter[is.na(H_var_filter$RATE_INCR_PRE),"RATE_INCR_PRE"]=0
count(H_var_filter$RATE_INCR_PRE)

# keep completed cases
H_var_filter = H_var_filter[complete.cases(H_var_filter),]

# Randomly divide into traintest and validation
ind = sample(1:nrow(H_var_filter),floor(0.75*nrow(H_var_filter)))
H_var_train = H_var_filter[ind,] 
H_var_val = H_var_filter[-ind,]

system.time(glm_train <- glm(NFO_IND~.-RBO_IND-PLCY_REF-EFF_DT-ISSUE_AGE,data=H_var_train,family="binomial"))

summary(glm_train)
glm_pred = predict(glm_train,newdata=H_var_val,type="response")

# ROC curve
glm_pred_roc = prediction(prediction=glm_pred, labels=H_var_val$NFO_IND)
glm_perf = performance(glm_pred_roc,measure="tpr",x.measure="fpr")
plot(glm_perf, main="ROC Curve - RBO - Simple Model", col="blue", lwd=3)
abline(a=0,b=1,lwd=2,lty=2)
glm_perf_auc = performance(glm_pred_roc,measure="auc")
unlist(glm_perf_auc@y.values)

plotLift(glm_pred,H_var_val$NFO_IND,cumulative = T, n.buckets = 10)

# Optimal sensitivity /specificity
perf = function(cut, mod, y)
{
  yhat = (mod>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( d[1]^2 + d[2]^2 ) 
  OUT = t(as.matrix(c(cut,sensitivity, specificity, c.rate,d)))
  colnames(OUT) = c("cut","sensitivity", "specificity", "c.rate", "distance")
  return(OUT)
}

s_nfo = seq(0,1,length=1000)
OUT_nfo = matrix(0,1000,5)
for(i in 1:1000) OUT_nfo[i,]=perf(s_nfo[i],glm_pred,H_var_val$NFO_IND)
OUT_nfo=as.data.frame(OUT_nfo)
colnames(OUT_nfo)=c("cut","sensitivity", "specificity", "c.rate", "distance")

H_var_nfo = data.frame(NFO_IND = H_var_val$NFO_IND,glm_pred)
H_var_nfo$pred_outcome = ifelse(H_var_nfo$pred>0.04,1,0)
confusionMatrix(H_var_nfo$pred_outcome,H_var_nfo$NFO_IND,positive="1")
library(dplyr)
H_var_nfo = mutate(H_var_nfo,quantile=ntile(H_var_nfo$pred,10))
table(H_var_nfo$quantile,H_var_nfo$pred_outcome)
table(H_var_nfo$quantile,H_var_nfo$NFO_IND)
H_var_nfo$Y = ifelse(H_var_nfo$NFO_IND==H_var_nfo$pred_outcome & H_var_nfo$NFO_IND==1,1,0)
table(H_var_nfo$quantile,H_var_nfo$Y)
table(H_var_nfo$NFO_IND)


train_rbo = glm(RBO_IND~.-NFO_IND-PLCY_REF-EFF_DT-ISSUE_AGE,data=H_var_train,family="binomial")
summary(train_rbo)
pred_rbo = predict(train_rbo,newdata=H_var_varl,type="response")
# ROC curve
pred_roc_rbo = prediction(prediction=pred_rbo, labels=H_var_varl$RBO_IND)
perf_rbo = performance(pred_roc_rbo,measure="tpr",x.measure="fpr")
plot(perf_rbo, main="ROC Curve - RBO - Simple Model", col="blue", lwd=3)
abline(a=0,b=1,lwd=2,lty=2)
perf_auc_rbo = performance(pred_roc_rbo,measure="auc")
unlist(perf_auc_rbo@y.values)

plotLift(pred_rbo,H_var_varl$RBO_IND,cumulative = T, n.buckets = 10)
H_var_rbo = data.frame(RBO_IND = H_var_varl$RBO_IND,pred_rbo)
H_var_rbo = mutate(H_var_rbo,quantile=ntile(H_var_rbo$pred,10))
table(H_var_rbo$quantile,H_var_rbo$RBO_IND)
table(H_var_rbo$RBO_IND)

############### Dec. 7th ##################
H_var_nfo$pred_outcome = ifelse(H_var_nfo$quantile==10,1,0)
H_var_rbo$pred_outcome = ifelse(H_var_rbo$quantile==10,1,0)
confusionMatrix(H_var_nfo$pred_outcome,H_var_nfo$NFO_IND,positive="1")
confusionMatrix(H_var_rbo$pred_outcome,H_var_rbo$RBO_IND,positive="1")


############### Dec.12th ##################
## determine the optimal cutoff based on cost
# control.cutpoints = control.cutpoints(CFP = 10,CFN = 1)
# optimal.cutpoints(X="glm_pred",status="NFO_IND",data=H_var_nfo,
#                  methods="MCT",
#                  tag.healthy = "1",
#                  control=control.cutpoints)

## Stepwise regression on glm
varImp(glm_train)
vif = data.frame(vif = vif(glm_train))
vif = data.frame(Variable=as.character(row.names(vif)),vif)

# system.time(glm_train_step <- stepAIC(glm_train,direction = "both"))
# glm_train_step
H_var_train$NFO_IND = as.factor(ifelse(H_var_train$NFO_IND==0,'non.NFO','NFO'))
H_var_val$NFO_IND = as.factor(ifelse(H_var_val$NFO_IND==0,'non.NFO','NFO'))
# SMOTE sampling
set.seed(9560)
H_var_train_SMOTE = SMOTE(NFO_IND~.,data=H_var_train)
table(H_var_train_SMOTE$NFO_IND)

ctrl = trainControl(method="cv",number=5,summaryFunction = twoClassSummary,classProbs = T)
system.time(glm_train_caret<-train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT-ISSUE_AGE
                                    -ZAGG_C210EBI-ZAGG_C210HMI-C210HMI-C210EBI-C210KSES
                                      -C210CIP-ZAGG_C210WHT-ZAGG_C210WHT-ZAGG_C210CIP
                                    -ZAGG_C210HVA-C210HVA-ZAGG_C210BLU-C210WHT-C210BLU,
                                    data=H_var_train_SMOTE,
                        method="glm",family="binomial",trControl=ctrl,metric="ROC"))
glm_train_caret
summary(glm_train_caret)
vif = data.frame(vif = vif(glm_train_caret))
# predict
glm_pred_caret = predict(glm_train_caret,newdata=H_var_val,type="prob")
H_var_val_roc = data.frame(H_var_val,nfo_prob=glm_pred_caret)
tapply(H_var_val_roc$nfo_prob.NFO,H_var_val_roc$NFO_IND,mean)

glm_pred_caret_class = predict(glm_train_caret,newdata=H_var_val)
confusionMatrix(glm_pred_caret_class,H_var_val$NFO_IND,positive="NFO")

# ROC
glm_pred_caret_roc = prediction(prediction=H_var_val_roc$nfo_prob.NFO, labels=H_var_val_roc$NFO_IND,
                                label.ordering = c("non.NFO","NFO"))
glm_perf = performance(glm_pred_caret_roc,measure="tpr",x.measure="fpr")
plot(glm_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
abline(a=0,b=1,lwd=2,lty=2)
glm_perf_auc = performance(glm_pred_caret_roc,measure="auc")
unlist(glm_perf_auc@y.values)


########### rpart #############
system.time(rpart_train_caret<-train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT-ISSUE_AGE
                                   -ZAGG_C210EBI-ZAGG_C210HMI-C210HMI-C210EBI-C210KSES
                                   -C210CIP-ZAGG_C210WHT-ZAGG_C210WHT-ZAGG_C210CIP
                                   -ZAGG_C210HVA-C210HVA-ZAGG_C210BLU-C210WHT-C210BLU,
                                   data=H_var_train_SMOTE,
                                   method="rpart",trControl=ctrl,metric="ROC",tuneLength=50))
rpart_train_caret
varImp(rpart_train_caret)
rpart_pred_caret = predict(rpart_train_caret,newdata=H_var_val,type="prob")
rpart_pred_caret_class = predict(rpart_train_caret,newdata=H_var_val)
tapply(rpart_pred_caret$NFO,H_var_val$NFO_IND,mean)
confusionMatrix(rpart_pred_caret_class,H_var_val$NFO_IND,positive="NFO")

# ROC
rpart_pred_caret_roc = prediction(prediction=rpart_pred_caret$non.NFO, labels=H_var_val$NFO_IND)
rpart_perf = performance(rpart_pred_caret_roc,measure="tpr",x.measure="fpr")
plot(rpart_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
abline(a=0,b=1,lwd=2,lty=2)
rpart_perf_auc = performance(rpart_pred_caret_roc,measure="auc")
round(unlist(rpart_perf_auc@y.values),2)







