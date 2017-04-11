getwd()
setwd("../Desktop/")
library(caret)
library(ROCR)
library(lift)

# read in data
Val_Score = read.csv("VAL_SCORED_DECILED_NFO.csv")
str(Val_Score)
table(Val_Score$nfo_ind_old)

# Transform the raw score into probability
Val_Score$nfo_prob = exp(Val_Score$pred_NFO_12011)/(1+exp(Val_Score$pred_NFO_12011))
summary(Val_Score$nfo_prob)
tapply(Val_Score$nfo_prob,Val_Score$nfo_ind_old,mean)

# top 30% cut-off
Val_Score$cutoff_top_30 = ifelse(Val_Score$score10>7,1,0)
confusionMatrix(Val_Score$cutoff_top_30,Val_Score$nfo_ind_old,positive="1")

# ROC curve
nfo_pred = prediction(prediction=Val_Score$pred_NFO_12011, labels=Val_Score$nfo_ind_old)
nfo_perf = performance(nfo_pred,measure="tpr",x.measure="fpr")
plot(nfo_perf, main="ROC Curve - NFO - Simple Model", col="blue", lwd=3)
abline(a=0,b=1,lwd=2,lty=2)
nfo_perf_auc = performance(nfo_pred,measure="auc")
unlist(nfo_perf_auc@y.values)

# Lift curve
nfo_perf_lift = performance(nfo_pred,measure="lift",x.measure="rpp")
plot(nfo_perf_lift,main="Lift Curve - NFO - Simple Model")

plotLift(Val_Score$pred_NFO_12011,Val_Score$nfo_ind_old,cumulative = T, n.buckets = 10)


# Do the same for RBO
Val_Score_RBO = read.csv("VAL_SCORED_DECILED_RBO_FINAL.csv")
str(Val_Score_RBO)
table(Val_Score_RBO$rbo_ind_old)

# Transform the raw score into probability
Val_Score_RBO$rbo_prob = exp(Val_Score_RBO$pred_RBO_22011)/(1+exp(Val_Score_RBO$pred_RBO_22011))
summary(Val_Score_RBO$rbo_prob)
tapply(Val_Score_RBO$rbo_prob,Val_Score_RBO$rbo_ind_old,mean)

# top 30% cut-off
Val_Score_RBO$cutoff_top_30 = ifelse(Val_Score_RBO$score10>7,1,0)
confusionMatrix(Val_Score_RBO$cutoff_top_30,Val_Score_RBO$rbo_ind_old,positive="1")

# ROC curve
rbo_pred = prediction(prediction=Val_Score_RBO$pred_RBO_22011, labels=Val_Score_RBO$rbo_ind_old)
rbo_perf = performance(rbo_pred,measure="tpr",x.measure="fpr")
plot(rbo_perf, main="ROC Curve - RBO - Simple Model", col="blue", lwd=3)
abline(a=0,b=1,lwd=2,lty=2)
rbo_perf_auc = performance(rbo_pred,measure="auc")
unlist(rbo_perf_auc@y.values)

# Lift curve
rbo_perf_lift = performance(rbo_pred,measure="lift",x.measure="rpp")
plot(rbo_perf_lift,main="Lift Curve - RBO - Simple Model")

plotLift(Val_Score_RBO$pred_RBO_22011,Val_Score_RBO$rbo_ind_old,cumulative = T, n.buckets = 10)

# Optimal Cutpoints in R (memory size issue)
library(OptimalCutpoints)
optimal.cutpoints(X = "pred_NFO_12011",tag.healthy = 1,status="nfo_ind_old",data=Val_Score,methods = "MaxSe")

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

# Determine the optimal cut-off for NFO
s_nfo = seq(0,1,length=1000)
OUT_nfo = matrix(0,1000,5)
for(i in 1:1000) OUT_nfo[i,]=perf(s_nfo[i],Val_Score$nfo_prob,Val_Score$nfo_ind_old)
plot(s_nfo,OUT_nfo[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_nfo,OUT_nfo[,3],col="darkgreen",lwd=2)
lines(s_nfo,OUT_nfo[,4],col=4,lwd=2)
lines(s_nfo,OUT_nfo[,5],col="darkred",lwd=2)
box()
legend(0.6,.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))

OUT_nfo=as.data.frame(OUT_nfo)
colnames(OUT_nfo)=c("cut","sensitivity", "specificity", "c.rate", "distance")

# Determine the optimal cut-off for RBO
s_rbo = seq(0,1,length=1000)
OUT_rbo = matrix(0,1000,5)
for(i in 1:1000) OUT_rbo[i,]=perf(s_rbo[i],Val_Score_RBO$rbo_prob,Val_Score_RBO$rbo_ind_old)
plot(s_rbo,OUT_rbo[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_rbo,OUT_rbo[,3],col="darkgreen",lwd=2)
lines(s_rbo,OUT_rbo[,4],col=4,lwd=2)
lines(s_rbo,OUT_rbo[,5],col="darkred",lwd=2)
box()
legend(0.6,.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))

OUT_rbo=as.data.frame(OUT_rbo)
colnames(OUT_rbo)=c("cut","sensitivity", "specificity", "c.rate", "distance")


# NFO - optimal cut off
OUT_nfo = data.frame(OUT_nfo)
colnames(OUT_nfo) = c("cut","sensitivity", "specificity", "c.rate", "distance")
OUT_nfo[which.min(abs(OUT_nfo$sensitivity - OUT_nfo$specificity)),]
# NFO - confusion matrix at optimal cut off
Val_Score$cutoff_best_nfo = ifelse(Val_Score$nfo_prob>0.02202202,1,0)
confusionMatrix(Val_Score$cutoff_best_nfo,Val_Score$nfo_ind_old,positive="1")

# RBO - optimal cut off
OUT_rbo = data.frame(OUT_rbo)
colnames(OUT_rbo) = c("cut","sensitivity", "specificity", "c.rate", "distance")
OUT_rbo[which.min(abs(OUT_rbo$sensitivity - OUT_rbo$specificity)),]
# RBO - confusion matrix at optimal cut off
Val_Score_RBO$cutoff_best_rbo = ifelse(Val_Score_RBO$rbo_prob>0.05005005,1,0)
confusionMatrix(Val_Score_RBO$cutoff_best_rbo,Val_Score_RBO$rbo_ind_old,positive="1")

############### Dec. 5th ################
# merge two original datasets
Val_Score_all = merge(Val_Score,Val_Score_RBO,by=c("PLCY_REF","DECISION_STAGE"))
# Transform the probability
Val_Score_all$nfo_prob = exp(Val_Score_all$pred_NFO_12011)/(1+exp(Val_Score_all$pred_NFO_12011))
Val_Score_all$rbo_prob = exp(Val_Score_all$pred_RBO_22011)/(1+exp(Val_Score_all$pred_RBO_22011))
# Predicted NFO and RBO on best cut-off
Val_Score_all$nfo_pred = ifelse(Val_Score_all$nfo_prob>0.02202202,1,0)
Val_Score_all$rbo_pred = ifelse(Val_Score_all$rbo_prob>0.05005005,1,0)
# See how many are predicted to be both NFO and RBO
table(Val_Score_all$nfo_pred,Val_Score_all$rbo_pred)
# Keep the useful columns only
Val_Score_all_output = Val_Score_all[,c(1:4,22:23,9:21,37:42,45:48)]
write.csv(Val_Score_all_output,"Simple_Model_Dataset.csv")

############### Dec. 6th #######################
summary(Val_Score_all$nfo_prob)
summary(Val_Score_all$rbo_prob)
Val_Score_all$nfo_pred = ifelse(Val_Score_all$nfo_prob>0.04,1,0)
Val_Score_all$rbo_pred = ifelse(Val_Score_all$rbo_prob>0.09,1,0)
confusionMatrix(Val_Score_all$nfo_pred,Val_Score_all$nfo_ind_old.x,positive="1")
confusionMatrix(Val_Score_all$rbo_pred,Val_Score_all$rbo_ind_old.x,positive="1")



