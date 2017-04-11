# Model Based Clustering
library(mclust)
fit <- Mclust(mydata[,c(1:4)])
fit # view solution summary

fit$BIC # lookup all the options attempted
classif = fit$classification # classifn vector
mydata1 = cbind(mydata.orig, classif) # append to dataset
mydata1[1:10,] #view top 10 rows
table(mydata1$classif)

fit1=cbind(classif)
rownames(fit1)=rownames(mydata)
library(cluster)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)

# get cluster means to help characterize the clusters
cmeans=aggregate(mydata.orig,by=list(classif),FUN=mean); cmeans 
