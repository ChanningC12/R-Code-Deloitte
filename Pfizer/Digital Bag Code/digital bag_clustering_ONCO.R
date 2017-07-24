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
library(Rtsne)
library(dplyr)

# Read in digital bag output
digital_bag_email_10 <- fread("Chi Cheng/Master Digital Bag/Output Data/digital_bag_email_distribution_top10_0525.csv")
digital_bag_all_spec_summary <- fread(paste("D:/Chi Cheng/Output/digital_bag_all_spec_seg_summary_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))

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

# Append segment
segment <- fread("Chi Cheng/Master Digital Bag/Data Loading/segment_all.csv")
names(segment)[1] <- "pfz_cust_id"
ONCO_km <- merge(ONCO_km,segment,by="pfz_cust_id",all.x=T)
colSums(is.na(ONCO_km))
table(ONCO_km$Segment)
fwrite(ONCO_km,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_km_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Impute missing as 0
ONCO_km <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_km_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
ONCO_km[is.na(ONCO_km)] <- 0
# Keep HCPs that are predicted to receive at least one email
ONCO_km_filter <- ONCO_km[rowSums(ONCO_km[,c(2:57)])>0,]

# Split ONCO data by segment
ONCO_km_filter_A <- ONCO_km_filter[ONCO_km_filter$Segment=="A",c(1:57)]
ONCO_km_filter_B <- ONCO_km_filter[ONCO_km_filter$Segment=="B",c(1:57)]
ONCO_km_filter_C <- ONCO_km_filter[ONCO_km_filter$Segment=="C",c(1:57)]
ONCO_km_filter_D <- ONCO_km_filter[ONCO_km_filter$Segment=="D",c(1:57)]
ONCO_km_filter_E <- ONCO_km_filter[ONCO_km_filter$Segment=="E",c(1:57)]
ONCO_km_filter_F <- ONCO_km_filter[ONCO_km_filter$Segment=="F",c(1:57)]

detach("package:reshape",unload=TRUE)
######################################### Segment A ####################################################
# Determine the number of clusters
# WSS: with-in-sum-of-squares, total distance of data points from their respective centroids
set.seed(052501)
k.max <- 20
wss <- sapply(1:k.max,function(k){
  kmeans(as.matrix(ONCO_km_filter_A[,2:ncol(ONCO_km_filter_A)]),k,nstart=50,iter.max=15)$tot.withinss})
plot(1:k.max,wss,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

ONCO_wss <- data.frame(cluster=1:k.max, wss=wss)
fwrite(ONCO_wss,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_wss_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Assign k=4 for clustering analysis
k1=3

# K-Means Cluster Analysis
set.seed(052502)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter_A[,2:ncol(ONCO_km_filter_A)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter_A, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# t-SNE plot for kmean clusters
set.seed(052503)
tsne <- Rtsne(as.matrix(ONCO_km_filter_A[,2:ncol(ONCO_km_filter_A)]),check_duplicates = F)

tsne <- tsne$Y %>% data.frame() %>%
  setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(fit_ONCO$cluster))

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))

fwrite(tsne,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/tsne_ONCO_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_ONCO <- aggregate(ONCO_km_filter_A[,2:ncol(ONCO_km_filter_A)],by=list(fit_ONCO$cluster),FUN=mean)
ONCO_all_cluster_count <- ONCO_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_ONCO <- merge(clust_mean_ONCO,ONCO_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_ONCO,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/cluster_mean_ONCO_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

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

# Format the 3 clusters
# cluster 1
ONCO_clust_1 <- ONCO_all_cluster[ONCO_all_cluster$cluster==1,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_1,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_A_1_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)
# cluster 2
ONCO_clust_2 <- ONCO_all_cluster[ONCO_all_cluster$cluster==2,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_2,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_A_2_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# cluster 3
ONCO_clust_3 <- ONCO_all_cluster[ONCO_all_cluster$cluster==3,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_3,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_A_3_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)



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

ONCO_A_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="ONCO" & digital_bag_all_spec_summary$emails_pred>0 &
                                                      digital_bag_all_spec_summary$Segment=="A" & digital_bag_all_spec_summary$mrkt_rel_avg>=0.05,]$brand)
ONCO_A_brand <- unique(c(ONCO_A_brand,"XALKORI"))

ONCO_top_10_brand_clust[!(ONCO_top_10_brand_clust$brand %in% ONCO_A_brand),]$emails_pred_adj <- 0

fwrite(ONCO_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
      ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")],
      ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,c("brand","emails_pred_adj")])
names(ONCO_bag) <- c("brand_1","emails_pred_adj_1","brand_2","emails_pred_adj_2","brand_3","emails_pred_adj_3")
fwrite(ONCO_bag,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_bag_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################## Apply 10 clusters #######################
k2=10

# K-Means Cluster Analysis
set.seed(052504)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter_A[,2:ncol(ONCO_km_filter_A)]),centers = k2,nstart=50,iter.max=15)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter_A, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_10_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# t-SNE plot for kmean clusters
tsne$cluster <- as.factor(fit_ONCO$cluster)

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))



################################## Segment B ###############################################
# K-Means Cluster Analysis
set.seed(052505)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter_B[,2:ncol(ONCO_km_filter_B)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter_B, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_ONCO <- aggregate(ONCO_km_filter_B[,2:ncol(ONCO_km_filter_B)],by=list(fit_ONCO$cluster),FUN=mean)
ONCO_all_cluster_count <- ONCO_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_ONCO <- merge(clust_mean_ONCO,ONCO_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_ONCO,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/cluster_mean_ONCO_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


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

# Format the 3 clusters
# cluster 1
ONCO_clust_1 <- ONCO_all_cluster[ONCO_all_cluster$cluster==1,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_1,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_B_1_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)
# cluster 2
ONCO_clust_2 <- ONCO_all_cluster[ONCO_all_cluster$cluster==2,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_2,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_B_2_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# cluster 3
ONCO_clust_3 <- ONCO_all_cluster[ONCO_all_cluster$cluster==3,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_3,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_B_3_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


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

ONCO_B_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="ONCO" & digital_bag_all_spec_summary$emails_pred>0 &
                                                      digital_bag_all_spec_summary$Segment=="B" & digital_bag_all_spec_summary$mrkt_rel_avg>=0.05,]$brand)

ONCO_B_brand <- unique(c(ONCO_B_brand,"XALKORI"))

ONCO_top_10_brand_clust[!(ONCO_top_10_brand_clust$brand %in% ONCO_B_brand),]$emails_pred_adj <- 0

fwrite(ONCO_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==3,c("brand","emails_pred_adj")])
names(ONCO_bag) <- c("brand_1","emails_pred_adj_1","brand_2","emails_pred_adj_2","brand_3","emails_pred_adj_3")
fwrite(ONCO_bag,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_bag_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment C ###############################################
# K-Means Cluster Analysis
set.seed(052506)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter_C[,2:ncol(ONCO_km_filter_C)]),centers = k3,nstart=50,iter.max=15)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter_C, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_ONCO <- aggregate(ONCO_km_filter_C[,2:ncol(ONCO_km_filter_C)],by=list(fit_ONCO$cluster),FUN=mean)
ONCO_all_cluster_count <- ONCO_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_ONCO <- merge(clust_mean_ONCO,ONCO_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_ONCO,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/cluster_mean_ONCO_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

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

# Format the 2 clusters
# cluster 1
ONCO_clust_1 <- ONCO_all_cluster[ONCO_all_cluster$cluster==1,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_1,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_C_1_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)
# cluster 2
ONCO_clust_2 <- ONCO_all_cluster[ONCO_all_cluster$cluster==2,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_2,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_C_2_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)



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

ONCO_C_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="ONCO" & digital_bag_all_spec_summary$emails_pred>0 &
                                                      digital_bag_all_spec_summary$Segment=="C" & digital_bag_all_spec_summary$mrkt_rel_avg>=0.05,]$brand)

ONCO_C_brand <- unique(c(ONCO_C_brand,"XALKORI"))

ONCO_top_10_brand_clust[!(ONCO_top_10_brand_clust$brand %in% ONCO_C_brand),]$emails_pred_adj <- 0

fwrite(ONCO_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")])
names(ONCO_bag) <- c("brand_1","emails_pred_adj_1","brand_2","emails_pred_adj_2")
fwrite(ONCO_bag,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_bag_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)



################################## Segment D ###############################################
# K-Means Cluster Analysis
set.seed(052507)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter_D[,2:ncol(ONCO_km_filter_D)]),centers = k3,nstart=50,iter.max=15)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter_D, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_ONCO <- aggregate(ONCO_km_filter_D[,2:ncol(ONCO_km_filter_D)],by=list(fit_ONCO$cluster),FUN=mean)
ONCO_all_cluster_count <- ONCO_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_ONCO <- merge(clust_mean_ONCO,ONCO_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_ONCO,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/cluster_mean_ONCO_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

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

# Format the 2 clusters
# cluster 1
ONCO_clust_1 <- ONCO_all_cluster[ONCO_all_cluster$cluster==1,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_1,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_D_1_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)
# cluster 2
ONCO_clust_2 <- ONCO_all_cluster[ONCO_all_cluster$cluster==2,names(ONCO_all_cluster) %in% 
                                   unique(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(ONCO_all_cluster) %in% c("pfz_cust_id")]
fwrite(ONCO_clust_2,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_clust_D_2_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)



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

ONCO_D_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="ONCO" & digital_bag_all_spec_summary$emails_pred>0 &
                                                      digital_bag_all_spec_summary$Segment=="D" & digital_bag_all_spec_summary$mrkt_rel_avg>=0.05,]$brand)

ONCO_D_brand <- unique(c(ONCO_D_brand,"XALKORI"))

ONCO_top_10_brand_clust[!(ONCO_top_10_brand_clust$brand %in% ONCO_D_brand),]$emails_pred_adj <- 0

fwrite(ONCO_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
        ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")])
names(ONCO_bag) <- c("brand_1","emails_pred_adj_1","brand_2","emails_pred_adj_2")
fwrite(ONCO_bag,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_bag_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)



################################## Segment E ###############################################
ONCO_km_filter_E$cluster <- 1
fwrite(ONCO_km_filter_E,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_ONCO <- aggregate(ONCO_km_filter_E[,2:(ncol(ONCO_km_filter_E)-1)],by=list(ONCO_km_filter_E$cluster),FUN=mean)
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

ONCO_top_10_brand_clust$emails_pred_adj <- with(ONCO_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

ONCO_E_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="ONCO" & digital_bag_all_spec_summary$emails_pred>0 &
                                                      digital_bag_all_spec_summary$Segment=="E" & digital_bag_all_spec_summary$mrkt_rel_avg>=0.05,]$brand)

ONCO_E_brand <- unique(c(ONCO_E_brand,"XALKORI"))

ONCO_top_10_brand_clust[!(ONCO_top_10_brand_clust$brand %in% ONCO_E_brand),]$emails_pred_adj <- 0

fwrite(ONCO_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")])
names(ONCO_bag) <- c("brand_1","emails_pred_adj_1")
fwrite(ONCO_bag,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_bag_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment F ###############################################
ONCO_km_filter_F$cluster <- 1
fwrite(ONCO_km_filter_F,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_ONCO <- aggregate(ONCO_km_filter_F[,2:(ncol(ONCO_km_filter_F)-1)],by=list(ONCO_km_filter_F$cluster),FUN=mean)
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

ONCO_top_10_brand_clust$emails_pred_adj <- with(ONCO_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

ONCO_F_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="ONCO" & digital_bag_all_spec_summary$emails_pred>0 &
                                                      digital_bag_all_spec_summary$Segment=="F" & digital_bag_all_spec_summary$mrkt_rel_avg>=0.05,]$brand)

ONCO_A_brand <- unique(c(ONCO_F_brand,"XALKORI"))

ONCO_top_10_brand_clust[!(ONCO_top_10_brand_clust$brand %in% ONCO_F_brand),]$emails_pred_adj <- 0

fwrite(ONCO_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

ONCO_bag <- 
  cbind(ONCO_top_10_brand_clust[ONCO_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")])
names(ONCO_bag) <- c("brand_1","emails_pred_adj_1")
fwrite(ONCO_bag,paste("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_bag_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


