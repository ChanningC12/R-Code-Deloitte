library(plyr)
getwd()
setwd("../Desktop/")
# read in data
model_simple = read.csv("SIMPLE_MODEL.csv")
# K-means
set.seed(11)
# run k-means, k=3, nstart=1000
model_simple_km = kmeans(model_simple[,3:9],centers=3,nstart=1000)
# append grouping to the data
model_simple_kmeans = data.frame(model_simple,model_simple_km$cluster)
colnames(model_simple_kmeans)[10] = "CLUSTER"
# explore the data
plyr:: count(model_simple_kmeans$CLUSTER) # specifiy the package
tapply(model_simple_kmeans$NFO_IND,model_simple_kmeans$CLUSTER,mean)
tapply(model_simple_kmeans$RBO_IND,model_simple_kmeans$CLUSTER,mean)

# Use ANOVA to test the significance of the difference among three groups
km_aov = aov(model_simple_kmeans$NFO_IND~as.factor(model_simple_kmeans$CLUSTER))
summary(km_aov)
TukeyHSD(km_aov)

# separate model based on group
model_simple_kmeans_seg1 = model_simple_kmeans[model_simple_kmeans$CLUSTER==1,]
model_simple_kmeans_seg2 = model_simple_kmeans[model_simple_kmeans$CLUSTER==2,]
model_simple_kmeans_seg3 = model_simple_kmeans[model_simple_kmeans$CLUSTER==3,]

# seg 1 model building
trntst_ind = sample(1:nrow(model_simple_kmeans_seg1),floor(0.7*nrow(model_simple_kmeans_seg1)))
model_simple_kmeans_seg1_trntst = model_simple_kmeans_seg1[trntst_ind,]
model_simple_kmeans_seg1_val = model_simple_kmeans_seg1[-trntst_ind,]

# build logistic regression on trntst
model_logit_km_rbo = glm(RBO_IND~.-NFO_IND-CLUSTER,data=model_simple_kmeans_seg1_trntst,family="binomial")
summary(model_logit_km_rbo)

# append probability to the val dataset
model_simple_kmeans_seg1_val$RBO_PROB = predict(model_logit_km_rbo,newdata = model_simple_kmeans_seg1_val, type="response")
tapply(model_simple_kmeans_seg1_val$RBO_PROB,model_simple_kmeans_seg1_val$RBO_IND,mean)

# Confusion matrix - cutoff 0.3
model_simple_kmeans_seg1_val = mutate(model_simple_kmeans_seg1_val, RBO_decile = ntile(model_simple_kmeans_seg1_val$RBO_PROB,10))
model_simple_kmeans_seg1_val$RBO_PRED_TOP30 = ifelse(model_simple_kmeans_seg1_val$RBO_decile>7,1,0)
confusionMatrix(model_simple_kmeans_seg1_val$RBO_PRED_TOP30,model_simple_kmeans_seg1_val$RBO_IND,positive="1")

# ROC curve
rbo_pred = prediction(prediction = model_simple_kmeans_seg1_val$RBO_PROB,labels = model_simple_kmeans_seg1_val$RBO_IND)
rbo_perf = performance(rbo_pred,measure = "tpr",x.measure = "fpr")
plot(rbo_perf, main = "ROC Curve - RBO")
rbo_perf_auc = performance(rbo_pred, measure="auc")
unlist(rbo_perf_auc@y.values)

# Lift curve
plotLift(model_simple_kmeans_seg1_val$RBO_PROB, model_simple_kmeans_seg1_val$RBO_IND,cumulative = T, n.buckets = 100)
TopDecileLift(model_simple_kmeans_seg1_val$RBO_PROB, model_simple_kmeans_seg1_val$RBO_IND) # top 10% lift

### Seg 2 - RBO ###
trntst_ind = sample(1:nrow(model_simple_kmeans_seg2),floor(0.7*nrow(model_simple_kmeans_seg2)))
model_simple_kmeans_seg2_trntst = model_simple_kmeans_seg2[trntst_ind,]
model_simple_kmeans_seg2_val = model_simple_kmeans_seg2[-trntst_ind,]

# build logistic regression on trntst
model_logit_km_rbo = glm(RBO_IND~.-NFO_IND-CLUSTER,data=model_simple_kmeans_seg2_trntst,family="binomial")
summary(model_logit_km_rbo)

# append probability to the val dataset
model_simple_kmeans_seg2_val$RBO_PROB = predict(model_logit_km_rbo,newdata = model_simple_kmeans_seg2_val, type="response")
tapply(model_simple_kmeans_seg2_val$RBO_PROB,model_simple_kmeans_seg2_val$RBO_IND,mean)

# Confusion matrix - cutoff 0.3
model_simple_kmeans_seg2_val = mutate(model_simple_kmeans_seg2_val, RBO_decile = ntile(model_simple_kmeans_seg2_val$RBO_PROB,10))
model_simple_kmeans_seg2_val$RBO_PRED_TOP30 = ifelse(model_simple_kmeans_seg2_val$RBO_decile>7,1,0)
confusionMatrix(model_simple_kmeans_seg2_val$RBO_PRED_TOP30,model_simple_kmeans_seg2_val$RBO_IND,positive="1")

# ROC curve
rbo_pred = prediction(prediction = model_simple_kmeans_seg2_val$RBO_PROB,labels = model_simple_kmeans_seg2_val$RBO_IND)
rbo_perf = performance(rbo_pred,measure = "tpr",x.measure = "fpr")
plot(rbo_perf, main = "ROC Curve - RBO")
rbo_perf_auc = performance(rbo_pred, measure="auc")
unlist(rbo_perf_auc@y.values)

# Lift curve
plotLift(model_simple_kmeans_seg2_val$RBO_PROB, model_simple_kmeans_seg2_val$RBO_IND,cumulative = T, n.buckets = 100)
TopDecileLift(model_simple_kmeans_seg2_val$RBO_PROB, model_simple_kmeans_seg2_val$RBO_IND) # top 10% lift

### Seg 3 - RBO ###
trntst_ind = sample(1:nrow(model_simple_kmeans_seg3),floor(0.7*nrow(model_simple_kmeans_seg3)))
model_simple_kmeans_seg3_trntst = model_simple_kmeans_seg3[trntst_ind,]
model_simple_kmeans_seg3_val = model_simple_kmeans_seg3[-trntst_ind,]

# build logistic regression on trntst
model_logit_km_rbo = glm(RBO_IND~.-NFO_IND-CLUSTER,data=model_simple_kmeans_seg3_trntst,family="binomial")
summary(model_logit_km_rbo)

# append probability to the val dataset
model_simple_kmeans_seg3_val$RBO_PROB = predict(model_logit_km_rbo,newdata = model_simple_kmeans_seg3_val, type="response")
tapply(model_simple_kmeans_seg3_val$RBO_PROB,model_simple_kmeans_seg3_val$RBO_IND,mean)

# Confusion matrix - cutoff 0.3
model_simple_kmeans_seg3_val = mutate(model_simple_kmeans_seg3_val, RBO_decile = ntile(model_simple_kmeans_seg3_val$RBO_PROB,10))
model_simple_kmeans_seg3_val$RBO_PRED_TOP30 = ifelse(model_simple_kmeans_seg3_val$RBO_decile>7,1,0)
confusionMatrix(model_simple_kmeans_seg3_val$RBO_PRED_TOP30,model_simple_kmeans_seg3_val$RBO_IND,positive="1")

# ROC curve
rbo_pred = prediction(prediction = model_simple_kmeans_seg3_val$RBO_PROB,labels = model_simple_kmeans_seg3_val$RBO_IND)
rbo_perf = performance(rbo_pred,measure = "tpr",x.measure = "fpr")
plot(rbo_perf, main = "ROC Curve - RBO")
rbo_perf_auc = performance(rbo_pred, measure="auc")
unlist(rbo_perf_auc@y.values)

# Lift curve
plotLift(model_simple_kmeans_seg3_val$RBO_PROB, model_simple_kmeans_seg3_val$RBO_IND,cumulative = T, n.buckets = 100)
TopDecileLift(model_simple_kmeans_seg3_val$RBO_PROB, model_simple_kmeans_seg3_val$RBO_IND) # top 10% lift



####################### Hierarchical Clustering ##############################
model_simple_hclust = hclust(dist(model_simple[,3:9])) # memory issue
clusterCut = cutree(model_simple_hclust)









