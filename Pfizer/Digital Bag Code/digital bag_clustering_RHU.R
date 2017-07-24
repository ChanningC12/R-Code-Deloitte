######################################################################################################################################
###                                                Digital Bag Output RHU Cluster                                                 ###
######################################################################################################################################

# Kick out the lowest score brand in each bag

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
digital_bag_all_spec_summary <- fread("D:/Chi Cheng/Output/digital_bag_all_spec_seg_summary_0525.csv")

# List of brands that is predicted to send at least an email OR the brand is XTANDI
brand_km <- unique(digital_bag_email_10[digital_bag_email_10$emails_pred_final>0 | digital_bag_email_10$brand=="XTANDI",]$brand)

# RHU pre-kmeans dataset
digital_bag_email_RHU_km <- digital_bag_email_10[digital_bag_email_10$spec_grp_max == "RHU" & 
                                                   digital_bag_email_10$brand %in% brand_km, 
                                                 c("pfz_cust_id","brand","emails_pred_final")]
digital_bag_email_RHU_km <- digital_bag_email_RHU_km[!duplicated(digital_bag_email_RHU_km[,c(1:2)]),]
summary(digital_bag_email_RHU_km)

# Reshape RHU output to wide table
RHU_km <- cast(digital_bag_email_RHU_km,pfz_cust_id~brand)

# Append segment
segment <- fread("Chi Cheng/Master Digital Bag/Data Loading/segment_all.csv")
names(segment)[1] <- "pfz_cust_id"
RHU_km <- merge(RHU_km,segment,by="pfz_cust_id",all.x=T)
colSums(is.na(RHU_km))
table(RHU_km$Segment)
fwrite(RHU_km,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_km_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Impute missing as 0
RHU_km <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_km_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
RHU_km[is.na(RHU_km)] <- 0
# Keep HCPs that are predicted to receive at least one email
RHU_km_filter <- RHU_km[rowSums(RHU_km[,c(2:54)])>0,]

# Split RHU data by segment
RHU_km_filter_A <- RHU_km_filter[RHU_km_filter$Segment=="A",c(1:54)]
RHU_km_filter_B <- RHU_km_filter[RHU_km_filter$Segment=="B",c(1:54)]
RHU_km_filter_C <- RHU_km_filter[RHU_km_filter$Segment=="C",c(1:54)]
RHU_km_filter_D <- RHU_km_filter[RHU_km_filter$Segment=="D",c(1:54)]
RHU_km_filter_E <- RHU_km_filter[RHU_km_filter$Segment=="E",c(1:54)]
RHU_km_filter_F <- RHU_km_filter[RHU_km_filter$Segment=="F",c(1:54)]

detach("package:reshape",unload=TRUE)
######################################### Segment A ####################################################
# Determine the number of clusters
# WSS: with-in-sum-of-squares, total distance of data points from their respective centroids
set.seed(052601)
k.max <- 20
wss <- sapply(1:k.max,function(k){
  kmeans(as.matrix(RHU_km_filter_A[,2:ncol(RHU_km_filter_A)]),k,nstart=50,iter.max=15)$tot.withinss})
plot(1:k.max,wss,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

RHU_wss <- data.frame(cluster=1:k.max, wss=wss)
fwrite(RHU_wss,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_wss_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Assign k=4 for clustering analysis
k1=2

# K-Means Cluster Analysis
set.seed(052602)
fit_RHU <- kmeans(as.matrix(RHU_km_filter_A[,2:ncol(RHU_km_filter_A)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
RHU_all_cluster <- data.frame(RHU_km_filter_A, cluster = fit_RHU$cluster)
table(RHU_all_cluster$cluster)
fwrite(RHU_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# t-SNE plot for kmean clusters
set.seed(052603)
tsne <- Rtsne(as.matrix(RHU_km_filter_A[,2:ncol(RHU_km_filter_A)]),check_duplicates = F)

tsne <- tsne$Y %>% data.frame() %>%
  setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(fit_RHU$cluster))

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))

fwrite(tsne,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/tsne_RHU_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_RHU <- aggregate(RHU_km_filter_A[,2:ncol(RHU_km_filter_A)],by=list(fit_RHU$cluster),FUN=mean)
RHU_all_cluster_count <- RHU_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_RHU <- merge(clust_mean_RHU,RHU_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_RHU,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/cluster_mean_RHU_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

RHU_top_10_brand_clust$brand_format <- gsub(" ",".",RHU_top_10_brand_clust$brand)

# Format the 2 clusters
# cluster 1
RHU_clust_1 <- RHU_all_cluster[RHU_all_cluster$cluster==1,names(RHU_all_cluster) %in% 
                                   unique(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(RHU_all_cluster) %in% c("pfz_cust_id")]
fwrite(RHU_clust_1,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_clust_A_1_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)
# cluster 2
RHU_clust_2 <- RHU_all_cluster[RHU_all_cluster$cluster==2,names(RHU_all_cluster) %in% 
                                   unique(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(RHU_all_cluster) %in% c("pfz_cust_id")]
fwrite(RHU_clust_2,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_clust_A_2_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

# distribute emails
RHU_top_10_brand_clust$emails_pred_adj <- with(RHU_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

RHU_brand <- digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="RHU" & digital_bag_all_spec_summary$emails_pred>0,]
RHU_mrkt_rel <- RHU_brand %>% group_by(Segment) %>%
  dplyr::summarize(
    mrkt_rel_cutoff = min(mrkt_rel_avg)
  )

RHU_mrkt_rel <- merge(RHU_mrkt_rel,RHU_brand[,c("Segment","mrkt_rel_avg","brand")],by.x=c("Segment","mrkt_rel_cutoff"),by.y = c("Segment","mrkt_rel_avg"),all.x = T)

RHU_A_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="RHU" & digital_bag_all_spec_summary$emails_pred>0 &
                                                   digital_bag_all_spec_summary$Segment=="A" & 
                                                   digital_bag_all_spec_summary$mrkt_rel_avg>RHU_mrkt_rel[RHU_mrkt_rel$Segment=="A",]$mrkt_rel_cutoff,]$brand)

RHU_top_10_brand_clust[!(RHU_top_10_brand_clust$brand %in% RHU_A_brand),]$emails_pred_adj <- 0

fwrite(RHU_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

RHU_bag <- 
  cbind(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
      RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")])
names(RHU_bag) <- c("brand_1","emails_pred_adj_1","brand_2","emails_pred_adj_2")
fwrite(RHU_bag,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_bag_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################## Apply 10 clusters #######################
k2=10

# K-Means Cluster Analysis
set.seed(052604)
fit_RHU <- kmeans(as.matrix(RHU_km_filter_A[,2:ncol(RHU_km_filter_A)]),centers = k2,nstart=50,iter.max=15)

# append cluster assignment
RHU_all_cluster <- data.frame(RHU_km_filter_A, cluster = fit_RHU$cluster)
table(RHU_all_cluster$cluster)
fwrite(RHU_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_10_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# t-SNE plot for kmean clusters
tsne$cluster <- as.factor(fit_RHU$cluster)

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))



################################## Segment B ###############################################
# K-Means Cluster Analysis
set.seed(052605)
fit_RHU <- kmeans(as.matrix(RHU_km_filter_B[,2:ncol(RHU_km_filter_B)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
RHU_all_cluster <- data.frame(RHU_km_filter_B, cluster = fit_RHU$cluster)
table(RHU_all_cluster$cluster)
fwrite(RHU_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_RHU <- aggregate(RHU_km_filter_B[,2:ncol(RHU_km_filter_B)],by=list(fit_RHU$cluster),FUN=mean)
RHU_all_cluster_count <- RHU_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_RHU <- merge(clust_mean_RHU,RHU_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_RHU,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/cluster_mean_RHU_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

RHU_top_10_brand_clust$brand_format <- gsub(" ",".",RHU_top_10_brand_clust$brand)

# Format the 3 clusters
# cluster 1
RHU_clust_1 <- RHU_all_cluster[RHU_all_cluster$cluster==1,names(RHU_all_cluster) %in% 
                                   unique(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,]$brand_format) |
                                   names(RHU_all_cluster) %in% c("pfz_cust_id")]
fwrite(RHU_clust_1,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_clust_B_1_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)
# cluster 2
RHU_clust_2 <- RHU_all_cluster[RHU_all_cluster$cluster==2,names(RHU_all_cluster) %in% 
                                   unique(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==2,]$brand_format) |
                                   names(RHU_all_cluster) %in% c("pfz_cust_id")]
fwrite(RHU_clust_2,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_clust_B_2_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

# distribute emails
RHU_top_10_brand_clust$emails_pred_adj <- with(RHU_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

RHU_B_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="RHU" & digital_bag_all_spec_summary$emails_pred>0 &
                                                     digital_bag_all_spec_summary$Segment=="B" & 
                                                     digital_bag_all_spec_summary$mrkt_rel_avg>RHU_mrkt_rel[RHU_mrkt_rel$Segment=="B",]$mrkt_rel_cutoff,]$brand)

RHU_top_10_brand_clust[!(RHU_top_10_brand_clust$brand %in% RHU_B_brand),]$emails_pred_adj <- 0

fwrite(RHU_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

RHU_bag <- 
  cbind(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")],
        RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==2,c("brand","emails_pred_adj")])
names(RHU_bag) <- c("brand_1","emails_pred_adj_1","brand_2","emails_pred_adj_2")
fwrite(RHU_bag,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_bag_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment C ###############################################
RHU_km_filter_C$cluster <- 1
fwrite(RHU_km_filter_C,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_RHU <- aggregate(RHU_km_filter_C[,2:(ncol(RHU_km_filter_C)-1)],by=list(RHU_km_filter_C$cluster),FUN=mean)
# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

RHU_top_10_brand_clust$brand_format <- gsub(" ",".",RHU_top_10_brand_clust$brand)

RHU_top_10_brand_clust$emails_pred_adj <- with(RHU_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                             ifelse(emails_pred_avg<3,3,
                                                                                    ifelse(emails_pred_avg<6,6,
                                                                                           ifelse(emails_pred_avg<18,12,24)))))

RHU_C_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="RHU" & digital_bag_all_spec_summary$emails_pred>0 &
                                                     digital_bag_all_spec_summary$Segment=="C" & 
                                                     digital_bag_all_spec_summary$mrkt_rel_avg>RHU_mrkt_rel[RHU_mrkt_rel$Segment=="C",]$mrkt_rel_cutoff,]$brand)

RHU_top_10_brand_clust[!(RHU_top_10_brand_clust$brand %in% RHU_C_brand),]$emails_pred_adj <- 0

fwrite(RHU_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


RHU_bag <- 
  cbind(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")])
names(RHU_bag) <- c("brand_1","emails_pred_adj_1")
fwrite(RHU_bag,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_bag_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment D ###############################################
RHU_km_filter_D$cluster <- 1
fwrite(RHU_km_filter_D,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_RHU <- aggregate(RHU_km_filter_D[,2:(ncol(RHU_km_filter_D)-1)],by=list(RHU_km_filter_D$cluster),FUN=mean)
# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

RHU_top_10_brand_clust$brand_format <- gsub(" ",".",RHU_top_10_brand_clust$brand)

RHU_top_10_brand_clust$emails_pred_adj <- with(RHU_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                             ifelse(emails_pred_avg<3,3,
                                                                                    ifelse(emails_pred_avg<6,6,
                                                                                           ifelse(emails_pred_avg<18,12,24)))))

RHU_D_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="RHU" & digital_bag_all_spec_summary$emails_pred>0 &
                                                     digital_bag_all_spec_summary$Segment=="D" & 
                                                     digital_bag_all_spec_summary$mrkt_rel_avg>RHU_mrkt_rel[RHU_mrkt_rel$Segment=="D",]$mrkt_rel_cutoff,]$brand)

RHU_top_10_brand_clust[!(RHU_top_10_brand_clust$brand %in% RHU_D_brand),]$emails_pred_adj <- 0

fwrite(RHU_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


RHU_bag <- 
  cbind(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")])
names(RHU_bag) <- c("brand_1","emails_pred_adj_1")
fwrite(RHU_bag,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_bag_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment E ###############################################
RHU_km_filter_E$cluster <- 1
fwrite(RHU_km_filter_E,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_RHU <- aggregate(RHU_km_filter_E[,2:(ncol(RHU_km_filter_E)-1)],by=list(RHU_km_filter_E$cluster),FUN=mean)
# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

RHU_top_10_brand_clust$brand_format <- gsub(" ",".",RHU_top_10_brand_clust$brand)

RHU_top_10_brand_clust$emails_pred_adj <- with(RHU_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

RHU_E_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="RHU" & digital_bag_all_spec_summary$emails_pred>0 &
                                                     digital_bag_all_spec_summary$Segment=="E" & 
                                                     digital_bag_all_spec_summary$mrkt_rel_avg>RHU_mrkt_rel[RHU_mrkt_rel$Segment=="E",]$mrkt_rel_cutoff,]$brand)

RHU_top_10_brand_clust[!(RHU_top_10_brand_clust$brand %in% RHU_E_brand),]$emails_pred_adj <- 0

fwrite(RHU_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


RHU_bag <- 
  cbind(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")])
names(RHU_bag) <- c("brand_1","emails_pred_adj_1")
fwrite(RHU_bag,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_bag_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment F ###############################################
RHU_km_filter_F$cluster <- 1
fwrite(RHU_km_filter_F,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_RHU <- aggregate(RHU_km_filter_F[,2:(ncol(RHU_km_filter_F)-1)],by=list(RHU_km_filter_F$cluster),FUN=mean)
# Shape RHU cluster dataset to the optimization format
names(clust_mean_RHU)[1] = "cluster"
clust_mean_RHU <- as.data.frame.matrix(clust_mean_RHU)
clust_mean_RHU_reshape <- melt(clust_mean_RHU[,c(1:ncol(clust_mean_RHU)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_RHU)-1))

names(clust_mean_RHU_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_RHU_reshape = clust_mean_RHU_reshape[clust_mean_RHU_reshape$brand != "cluster",]

clust_mean_RHU_reshape <- clust_mean_RHU_reshape %>% group_by(cluster) %>%
  dplyr::mutate(
    rank = rank(-emails_pred_avg,ties.method = "min")
  )

RHU_top_10_brand_clust <- clust_mean_RHU_reshape %>%
  filter(rank<=9) %>% arrange(cluster,rank)

RHU_top_10_brand_clust$brand_format <- gsub(" ",".",RHU_top_10_brand_clust$brand)

RHU_top_10_brand_clust$emails_pred_adj <- with(RHU_top_10_brand_clust,ifelse(emails_pred_avg==0,0,
                                                                               ifelse(emails_pred_avg<3,3,
                                                                                      ifelse(emails_pred_avg<6,6,
                                                                                             ifelse(emails_pred_avg<18,12,24)))))

RHU_F_brand <- unique(digital_bag_all_spec_summary[digital_bag_all_spec_summary$spec_grp_max=="RHU" & digital_bag_all_spec_summary$emails_pred>0 &
                                                     digital_bag_all_spec_summary$Segment=="F" & 
                                                     digital_bag_all_spec_summary$mrkt_rel_avg>RHU_mrkt_rel[RHU_mrkt_rel$Segment=="F",]$mrkt_rel_cutoff,]$brand)

RHU_top_10_brand_clust[!(RHU_top_10_brand_clust$brand %in% RHU_F_brand),]$emails_pred_adj <- 0

fwrite(RHU_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

RHU_bag <- 
  cbind(RHU_top_10_brand_clust[RHU_top_10_brand_clust$cluster==1,c("brand","emails_pred_adj")])
names(RHU_bag) <- c("brand_1","emails_pred_adj_1")
fwrite(RHU_bag,paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_bag_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


