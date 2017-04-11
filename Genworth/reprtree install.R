options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

# Random Forest using randomForest package, to specify the parameters
system.time(rf_rbo_raw <- randomForest(as.factor(RBO_IND)~.-RSPLIT1,
                                       data=allvars_RBO_train,
                                       importance=T,ntree=100,mtry=20,
                                       do.trace=100)) # 625s
# Check the model, draw ROC and see the lift
rf_rbo_raw
rf_rbo_raw_pred = predict(rf_rbo_raw,newdata=allvars_RBO_val,type="prob")
head(rf_rbo_raw_pred)
rf_rbo_raw_pred=data.frame(rf_rbo_raw_pred)
tapply(rf_rbo_raw_pred$RBO,allvars_RBO_val$RBO_IND,mean)
rbo_simple_ROC = prediction(rf_rbo_raw_pred$RBO,allvars_RBO_val$RBO_IND)
rbo_simple_ROC_perf = performance(rbo_simple_ROC,measure="tpr",x.measure="fpr")
plot(rbo_simple_ROC_perf, main="ROC Curve - rbo - SMOTE", col="blue", lwd=3)
rbo_simple_AUC = performance(rbo_simple_ROC,measure="auc")
unlist(rbo_simple_AUC@y.values) # 0.72

plotLift(rf_rbo_raw_pred$RBO,allvars_RBO_val$RBO_IND,cumulative=T,n.buckets=10)
TopDecileLift(rf_rbo_raw_pred$RBO,allvars_RBO_val$RBO_IND) # 2.21


# getTree, k means kth tree you want to visualize
tree<-getTree(rf_rbo_raw, k=80, labelVar=TRUE)
tree
# rename the name of the column
colnames(tree)<-sapply(colnames(tree),collapse)
rules<-getConds(tree)
print(rules)

# plot.tree
reprtree:::plot.getTree(rf_rbo_raw)

# output the rules
lapply(rules,write,"RF_tree_raw_30.txt",append=T)


# tree bucket testing
test=allvars_RBO_val[
  BINARY_DIAB_TRANS > 0.39301059185  &  ANNL_PREM_BFOR < 7.5  &  CNT_FPO_PREV1 < 0.5,]
prop.table(table(test$RBO_IND))

test=allvars_RBO_val[
  BINARY_NS_TRANS < 0.24326717585  &  RATE_INCR > 6.5  &  BINARY_NS_TRANS > 0.24295434825  &  DECISION_STAGE < 2.5  &  DECISION_STAGE > 1.5  &  RATE_INCR_AFFORD > 4.5,]
prop.table(table(test$RBO_IND))