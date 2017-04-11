############################ ctree #################################
# NFO
system.time(ctree_nfo <- train(as.factor(NFO_IND)~.-RBO_IND-PLCY_REF-EFF_DT,data=nfo_SMOTE,
                                 method="ctree",trControl=ctrl,metric="ROC")) # 91.3s
ctree_nfo
ctree_nfo_pred = predict(ctree_nfo,newdata=allvars_final_val_nfo,type="prob")
head(ctree_nfo_pred)
tapply(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,mean)
nfo_ROC = prediction(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND)
nfo_ROC_perf = performance(nfo_ROC,measure="tpr",x.measure="fpr")
plot(nfo_ROC_perf, main="ROC Curve - NFO - SMOTE", col="blue", lwd=3)
nfo_AUC = performance(nfo_ROC,measure="auc")
unlist(nfo_AUC@y.values) # 0.86

plotLift(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(ctree_nfo_pred$NFO,allvars_final_val_nfo$NFO_IND) # 5.25


# RBO
system.time(ctree_rbo <- train(as.factor(RBO_IND)~.-NFO_IND-PLCY_REF-EFF_DT,data=rbo_SMOTE,
                                 method="ctree",trControl=ctrl,metric="ROC")) # 327s
ctree_rbo
ctree_rbo_pred = predict(ctree_rbo,newdata=allvars_final_val_rbo,type="prob")
head(ctree_rbo_pred)
tapply(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,mean)
rbo_ROC = prediction(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND)
rbo_ROC_perf = performance(rbo_ROC,measure="tpr",x.measure="fpr")
plot(rbo_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_AUC = performance(rbo_ROC,measure="auc")
unlist(rbo_AUC@y.values) # 0.78

plotLift(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND,cumulative=T,n.buckets=10) 
TopDecileLift(ctree_rbo_pred$RBO,allvars_final_val_rbo$RBO_IND) # 4.39