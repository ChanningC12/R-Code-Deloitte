library(readxl)    # free data from excel hades
library(dplyr)     # sane data manipulation
library(tidyr)     # sane data munging
library(viridis)   # sane colors
library(ggplot2)   # needs no introduction
library(ggfortify) 
rm(list=ls())
gc()
###pre-loaded data
mydata = USArrests
mydata <- na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save orig data copy
mydata <- scale(mydata) # standardize variables
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram

k1 = 3 # eyeball the no. of clusters
# cut tree into k1 clusters
groups <- cutree(fit, k=k1)
# draw dendogram with red borders around the k1 clusters
rect.hclust(fit, k=k1, border="red")

# Determine number of clusters #
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Look for an "elbow" in the scree plot #
#Look for an "elbow" in the scree plot. The interior node at which the angle formed by the 'arms' is the smallest. This scree-plot is not unlike the one we saw in factor-analysis. Again, as with the dendogram, we get either 2 or 4 as the options available. Suppose we go with 2.
# Use optimal no. of clusters in k-means #
k1=2
# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution
# get cluster means
aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata1 <- data.frame(mydata.orig, fit$cluster)
