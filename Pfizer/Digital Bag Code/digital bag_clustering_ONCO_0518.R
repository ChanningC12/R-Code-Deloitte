######################################################################################################################################
###                                                Digital Bag Output ONCO Cluster                                                 ###
######################################################################################################################################

# Clear up the space for the job
rm(list=ls()) # remove all objects in the memory
gc() # garbage collection

# Set up working directory
setwd("D:\\")

# Read in libraries
library(data.table)
library(reshape)
library(reshape2)
library(Rtsne)
library(dplyr)

# Read in digital bag output
digital_bag_email_10 <- fread("Chi Cheng/Master Digital Bag/Output Data/digital_bag_email_distribution_top10_0517.csv")

# List of brands that is predicted to send at least an email OR the brand is XTANDI
brand_km <- unique(digital_bag_email_10[digital_bag_email_10$emails_pred_final>0 | digital_bag_email_10$brand=="XTANDI",]$brand)

# ONCO pre-kmeans dataset
digital_bag_email_ONCO_km <- digital_bag_email_10[digital_bag_email_10$spec_grp_max == "ONCO" & 
                                                   digital_bag_email_10$brand %in% brand_km, 
                                                 c("pfz_cust_id","brand","emails_pred_final")]
digital_bag_email_ONCO_km <- digital_bag_email_ONCO_km[!duplicated(digital_bag_email_ONCO_km[,c(1:2)]),]
summary(digital_bag_email_ONCO_km)

# Reshape ONCO output to wide table
ONCO_km <- cast(digital_bag_email_ONCO_km,pfz_cust_id~brand)
fwrite(ONCO_km,"Chi Cheng/Processed Data/ONCO_km_0517.csv",row.names = F)

# Impute missing as 0
ONCO_km <- fread("Chi Cheng/Processed Data/ONCO_km_0517.csv")
ONCO_km[is.na(ONCO_km)] <- 0
# Keep HCPs that are predicted to receive at least one email
ONCO_km_filter <- ONCO_km[rowSums(ONCO_km)>0,]

# Determine the number of clusters
# WSS: with-in-sum-of-squares, total distance of data points from their respective centroids
set.seed(051703)
k.max <- 20
wss <- sapply(1:k.max,function(k){
  kmeans(as.matrix(ONCO_km_filter[,2:ncol(ONCO_km_filter)]),k,nstart=50,iter.max=15)$tot.withinss})
plot(1:k.max,wss,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

ONCO_wss <- data.frame(cluster=1:k.max, wss=wss)
fwrite(ONCO_wss,"Chi Cheng/Output/ONCO_wss_0517.csv",row.names = F)

# Assign k=4 for clustering analysis
k1=4

# K-Means Cluster Analysis
set.seed(051704)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter[,2:ncol(ONCO_km_filter)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,"Chi Cheng/Output/ONCO_all_cluster_0517.csv",row.names = F)

# t-SNE plot for kmean clusters
system.time(
  tsne <- Rtsne(as.matrix(ONCO_km_filter[,2:ncol(ONCO_km_filter)]),check_duplicates = F)
)

tsne <- tsne$Y %>% data.frame() %>%
  setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(fit_ONCO$cluster))

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))

# get cluster means and count
clust_mean_ONCO <- aggregate(ONCO_km_filter[,2:ncol(ONCO_km_filter)],by=list(fit_ONCO$cluster),FUN=mean)
ONCO_all_cluster_count <- ONCO_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_ONCO <- merge(clust_mean_ONCO,ONCO_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_ONCO,"Chi Cheng/Output/cluster_mean_ONCO_0517.csv",row.names = F)


# Read in ONCO cluster data and cluster mean data
detach("package:reshape",unload=TRUE)
detach("package:reshape2",unload=TRUE)
ONCO_all_cluster <- fread("Chi Cheng/Output/ONCO_all_cluster_0517.csv")
clust_mean_ONCO <- fread("Chi Cheng/Output/cluster_mean_ONCO_0517.csv")

# Shape ONCO cluster dataset to the optimization format
names(clust_mean_ONCO)[1] = "cluster"
clust_mean_ONCO <- as.data.frame.matrix(clust_mean_ONCO)
clust_mean_ONCO_reshape <- melt(clust_mean_ONCO[,c(1:ncol(clust_mean_ONCO)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_ONCO)-1))

names(clust_mean_ONCO_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_ONCO_reshape = clust_mean_ONCO_reshape[clust_mean_ONCO_reshape$brand != "cluster",]

clust_mean_ONCO_reshape <- clust_mean_ONCO_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

ONCO_top_10_brand_clust <- clust_mean_ONCO_reshape %>%
  filter(rank<=10) %>% arrange(cluster,rank)

ONCO_top_10_brand_clust$brand_format <- gsub(" ",".",ONCO_top_10_brand_clust$brand)

# Format the 4 clusters
# cluster 1
ONCO_clust_1 <- ONCO_all_cluster[ONCO_all_cluster$cluster==1,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_1,paste("Chi Cheng/Processed Data/digital bag optimization/ONCO_clust_1_",Sys.Date(),".csv",sep = ""),row.names = F)
# cluster 2
ONCO_clust_2 <- ONCO_all_cluster[ONCO_all_cluster$cluster==2,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_2,paste("Chi Cheng/Processed Data/digital bag optimization/ONCO_clust_2_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 3
ONCO_clust_3 <- ONCO_all_cluster[ONCO_all_cluster$cluster==3,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_3,paste("Chi Cheng/Processed Data/digital bag optimization/ONCO_clust_3_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 4
ONCO_clust_4 <- ONCO_all_cluster[ONCO_all_cluster$cluster==4,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==4,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_4,paste("Chi Cheng/Processed Data/digital bag optimization/ONCO_clust_4_",Sys.Date(),".csv",sep = ""),row.names = F)



###### Apply round-up rule to distribute emails in the bags
clust_mean_ONCO <- fread("Chi Cheng/Output/cluster_mean_ONCO_0517.csv")

# Shape ONCO cluster dataset to the optimization format
names(clust_mean_ONCO)[1] = "cluster"
clust_mean_ONCO <- as.data.frame.matrix(clust_mean_ONCO)
clust_mean_ONCO_reshape <- melt(clust_mean_ONCO[,c(1:ncol(clust_mean_ONCO)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_ONCO)-1))

names(clust_mean_ONCO_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_ONCO_reshape = clust_mean_ONCO_reshape[clust_mean_ONCO_reshape$brand != "cluster",]

clust_mean_ONCO_reshape <- clust_mean_ONCO_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

ONCO_top_10_brand_clust <- clust_mean_ONCO_reshape %>%
  filter(rank<=10) %>% arrange(cluster,rank)

# distribute emails
ONCO_top_10_brand_clust$emails_pred_adj <- with(ONCO_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
      ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")],
      ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,c("brand","emails_pred_adj")],
      ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==4,c("brand","emails_pred_adj")])
fwrite(ONCO_bag,"Chi Cheng/Output/ONCO_bag_0517.csv",row.names = F)



################## Apply 10 clusters #######################
k3=10

# K-Means Cluster Analysis
set.seed(051709)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter[,2:ncol(ONCO_km_filter)]),centers = k3,nstart=50,iter.max=15)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,"Chi Cheng/Output/ONCO_all_cluster_10_0517.csv",row.names = F)

# t-SNE plot for kmean clusters
system.time(
  tsne <- Rtsne(as.matrix(ONCO_km_filter[,2:ncol(ONCO_km_filter)]),check_duplicates = F)
)

tsne$cluster <- as.factor(fit_ONCO$cluster)

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))

# get cluster means and count
clust_mean_ONCO <- aggregate(ONCO_km_filter[,2:ncol(ONCO_km_filter)],by=list(fit_ONCO$cluster),FUN=mean)
ONCO_all_cluster_count <- ONCO_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_ONCO <- merge(clust_mean_ONCO,ONCO_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_ONCO,"Chi Cheng/Output/cluster_mean_ONCO_10_0517.csv",row.names = F)


# Read in ONCO cluster data and cluster mean data
detach("package:reshape",unload=TRUE)
detach("package:reshape2",unload=TRUE)
ONCO_all_cluster <- fread("Chi Cheng/Output/ONCO_all_cluster_10_0517.csv")
clust_mean_ONCO <- fread("Chi Cheng/Output/cluster_mean_ONCO_10_0517.csv")

# Shape ONCO cluster dataset to the optimization format
names(clust_mean_ONCO)[1] = "cluster"
clust_mean_ONCO <- as.data.frame.matrix(clust_mean_ONCO)
clust_mean_ONCO_reshape <- melt(clust_mean_ONCO[,c(1:ncol(clust_mean_ONCO)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_ONCO)-1))

names(clust_mean_ONCO_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_ONCO_reshape = clust_mean_ONCO_reshape[clust_mean_ONCO_reshape$brand != "cluster",]

clust_mean_ONCO_reshape <- clust_mean_ONCO_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "first")
  )

ONCO_top_10_brand_clust <- clust_mean_ONCO_reshape %>%
  filter(rank<=10) %>% arrange(cluster,rank)

ONCO_top_10_brand_clust$brand_format <- gsub(" ",".",ONCO_top_10_brand_clust$brand)

# Format the 10 clusters
ONCO_all_cluster <- as.data.frame.matrix(ONCO_all_cluster)
# cluster 1
ONCO_clust_1 <- ONCO_all_cluster[ONCO_all_cluster$cluster==1,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_1,paste("D:/Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_1_",Sys.Date(),".csv",sep = ""),row.names = F)
# cluster 2
ONCO_clust_2 <- ONCO_all_cluster[ONCO_all_cluster$cluster==2,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_2,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_2_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 3
ONCO_clust_3 <- ONCO_all_cluster[ONCO_all_cluster$cluster==3,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_3,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_3_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 4
ONCO_clust_4 <- ONCO_all_cluster[ONCO_all_cluster$cluster==4,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==4,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_4,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_4_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 5
ONCO_clust_5 <- ONCO_all_cluster[ONCO_all_cluster$cluster==5,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==5,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_5,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_5_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 6
ONCO_clust_6 <- ONCO_all_cluster[ONCO_all_cluster$cluster==6,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==6,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_6,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_6_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 7
ONCO_clust_7 <- ONCO_all_cluster[ONCO_all_cluster$cluster==7,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==7,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_7,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_7_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 8
ONCO_clust_8 <- ONCO_all_cluster[ONCO_all_cluster$cluster==8,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==8,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_8,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_8_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 9
ONCO_clust_9 <- ONCO_all_cluster[ONCO_all_cluster$cluster==9,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==9,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_9,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_9_",Sys.Date(),".csv",sep = ""),row.names = F)

# cluster 10
ONCO_clust_10 <- ONCO_all_cluster[ONCO_all_cluster$cluster==10,names(ONCO_all_cluster) %in% 
                                    unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==10,]$brand_format) |
                                    names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_10,paste("Chi Cheng/Processed Data/digital bag optimization/chunks_0517/ONCO_clust_10_",Sys.Date(),".csv",sep = ""),row.names = F)

# distribute emails
ONCO_top_10_brand_clust$emails_pred_adj <- with(ONCO_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==4,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==5,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==6,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==7,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==8,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==9,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==10,c("brand","emails_pred_adj")])
fwrite(ONCO_bag,"Chi Cheng/Output/ONCO_bag_10_0517.csv",row.names = F)





