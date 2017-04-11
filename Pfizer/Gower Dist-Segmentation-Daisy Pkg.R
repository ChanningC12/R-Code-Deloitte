rm(list=ls())
gc()
###pre-loaded data
library(datasets)
head(iris3)
mydata = iris
library(cluster)
mydata <- na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save orig data copy
mydata<-data.frame(length=mydata.orig$Sepal.Length,width=mydata.orig$Sepal.Width,l1=mydata.orig$Petal.Length,w1=mydata.orig$Petal.Width)
#mydata <- scale(mydata) # standardize variables
mydata<-mydata.orig

#No need to scale data if using Gower Distance; Euclidean distance that is used in Kmeans can only be used for continuous variables
gower_dist <- daisy(mydata,
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)
mydata[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),arr.ind = TRUE)[1, ], ]
mydata[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),arr.ind = TRUE)[1, ], ]

# choosing a clustering algorithm in this example (PAM) & selecting # of clusters using Silhouette width
sil_width <- c(NA)
for(i in 2:10){  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(1:10, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width") 
lines(1:10, sil_width)
#finalizing the # of clusters based on above chart
pam_fit <- pam(gower_dist, diss = TRUE, k = 5)
summary(pam_fit)
mydata1 = cbind(mydata.orig, cluster=pam_fit$clustering) # append to dataset
mydata1$Species<-NULL
cmeans=aggregate(mydata1,by=list(mydata1$cluster),mean)
saveRDS(mydata1, "C:/Users/smusti/Downloads/Work/R/mydata1.Rds")
library(ggfortify)
library(ggplot2)
mydata1[pam_fit$medoids, ]
library(Rtsne)
tsne_obj <- Rtsne(as.matrix(gower_dist), is_distance = TRUE)

library(dplyr)
tsne_data <- tsne_obj$Y %>% data.frame() %>%  setNames(c("X", "Y")) %>%  mutate(cluster = factor(pam_fit$clustering),name = mydata.orig$Species)
#https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



# calculate mean
cluster0_mean <- apply(mydata.orig, 2, mean)
# calculate sd
cluster0_sd <- apply(mydata.orig, 2, sd)
# run kmeans cluster
kmeans.result <- kmeans(mydata,3)

# calculate cluster center before clustering
new_center <- data.frame(Murder = kmeans.result$centers[,c("Murder")]*cluster0_sd[1]+cluster0_mean[1],
                         Assault = kmeans.result$centers[,c("Assault")]*cluster0_sd[2]+cluster0_mean[2],
                         UrbanPop = kmeans.result$centers[,c("UrbanPop")]*cluster0_sd[3]+cluster0_mean[3],
                         Rape = kmeans.result$centers[,c("Rape")]*cluster0_sd[4]+cluster0_mean[4])
# plot graph
plot(mydata.orig[c("Murder","Assault")], 
     col = kmeans.result$cluster+1,pch = 2,cex = 1, 
     main = "Three Cluster of Members", 
     xlab = "Murder", ylab = "Assault",
     xlim = c(0,20), ylim = c(40,360)
)
summary(mydata.orig)
cluster_score <- function(new_member){
kmeans_center <- data.frame(new_center)
new_member_5 <- data.frame(Murder = new_member[,"Murder"],
                           Assault = new_member[,"Assault"],
                           UrbanPop = new_member[,"UrbanPop"],
                           Rape = new_member[,"Rape"])
# calculate normalized table
new_member_5_nom0 <- sweep(new_member_5,2,cluster0_mean)
new_member_5_nom <- sweep(new_member_5,2,cluster0_sd,"/")
# find out which cluster new member belongs to
closest.cluster <- function(x) {
  cluster.dist <- apply(kmeans_center, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
score <- apply(new_member_5_nom, 1, closest.cluster)
return(score)
}
member_score <- closest.cluster(mydata.orig[c(1:50),])

library(cluster)
daisy.mat <- as.matrix(daisy(mydata, metric="gower"))

library(StatMatch)
gower.mat <- gower.dist(mydata)
head(daisy.mat, 3)
head(gower.mat, 3)

identical(daisy.mat, gower.mat)
max(abs(daisy.mat - gower.mat))
obs <- as.integer(c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
                    0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1))
mydata<-cbind(mydata,obs)
str(mydata)
mydata[,5] <- sapply(mydata[,5], FUN=function(x) ifelse(x==1, TRUE, FALSE))