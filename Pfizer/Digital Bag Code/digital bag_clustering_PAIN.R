######################################################################################################################################
###                                                Digital Bag Output PAIN Cluster                                                 ###
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
# Read in digital bag summary output to get brand scores / market relevance
PAIN_score_mrkt_rel <- fread("Chi Cheng/Output/PAIN_avg_score_mrkt_rel_0601.csv")
names(PAIN_score_mrkt_rel)[2] <- "Segment"
names(PAIN_score_mrkt_rel)[3] <- "cluster"

# List of brands that is predicted to send at least an email OR the brand is XTANDI
brand_km <- unique(digital_bag_email_10[digital_bag_email_10$emails_pred_final>0 | digital_bag_email_10$brand=="XTANDI",]$brand)

# PAIN pre-kmeans dataset
digital_bag_email_PAIN_km <- digital_bag_email_10[digital_bag_email_10$spec_grp_max == "PAIN" & 
                                                   digital_bag_email_10$brand %in% brand_km, 
                                                 c("pfz_cust_id","brand","emails_pred_final")]
digital_bag_email_PAIN_km <- digital_bag_email_PAIN_km[!duplicated(digital_bag_email_PAIN_km[,c(1:2)]),]
summary(digital_bag_email_PAIN_km)

# Reshape PAIN output to wide table
PAIN_km <- cast(digital_bag_email_PAIN_km,pfz_cust_id~brand)

# Append segment
segment <- fread("Chi Cheng/Master Digital Bag/Data Loading/segment_all.csv")
names(segment)[1] <- "pfz_cust_id"
PAIN_km <- merge(PAIN_km,segment,by="pfz_cust_id",all.x=T)
colSums(is.na(PAIN_km))
table(PAIN_km$Segment)
fwrite(PAIN_km,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_km_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Impute missing as 0
PAIN_km <- fread("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_km_0526.csv")
PAIN_km[is.na(PAIN_km)] <- 0
# Keep HCPs that are predicted to receive at least one email
PAIN_km_filter <- PAIN_km[rowSums(PAIN_km[,c(2:56)])>0,]

# Split PAIN data by segment
PAIN_km_filter_A <- PAIN_km_filter[PAIN_km_filter$Segment=="A",c(1:56)]
PAIN_km_filter_B <- PAIN_km_filter[PAIN_km_filter$Segment=="B",c(1:56)]
PAIN_km_filter_C <- PAIN_km_filter[PAIN_km_filter$Segment=="C",c(1:56)]
PAIN_km_filter_D <- PAIN_km_filter[PAIN_km_filter$Segment=="D",c(1:56)]
PAIN_km_filter_E <- PAIN_km_filter[PAIN_km_filter$Segment=="E",c(1:56)]
PAIN_km_filter_F <- PAIN_km_filter[PAIN_km_filter$Segment=="F",c(1:56)]

detach("package:reshape",unload=TRUE)

# set average score and market relevance threshold
PAIN_score_mrkt_rel$ind <- ifelse(PAIN_score_mrkt_rel$brand 
                                 %in% c("LYRICA","XELJANZ","FLECTOR","EMBEDA","CELEBREX","ARTHROTEC","RARE DISEASES",
                                        "CHANTIX","PROTONIX","PREVNAR 13","PRISTIQ","RELPAX"),1,
                                        ifelse(PAIN_score_mrkt_rel$overall_score_final>=0.1 & 
                                                 PAIN_score_mrkt_rel$mrkt_rel_avg>=0.005,1,0))
table(PAIN_score_mrkt_rel$ind)

######################################### Segment C ####################################################
# Determine the number of clusters
# WSS: with-in-sum-of-squares, total distance of data points from their respective centroids
set.seed(052606)
k.max <- 20
wss <- sapply(1:k.max,function(k){
  kmeans(as.matrix(PAIN_km_filter_C[,2:ncol(PAIN_km_filter_C)]),k,nstart=50,iter.max=15)$tot.withinss})
plot(1:k.max,wss,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

PAIN_wss <- data.frame(cluster=1:k.max, wss=wss)
fwrite(PAIN_wss,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_wss_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Assign k=2 for clustering analysis
k1=2

# K-Means Cluster Analysis
set.seed(052607)
fit_PAIN <- kmeans(as.matrix(PAIN_km_filter_C[,2:ncol(PAIN_km_filter_C)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
PAIN_all_cluster <- data.frame(PAIN_km_filter_C, cluster = fit_PAIN$cluster)
table(PAIN_all_cluster$cluster)
fwrite(PAIN_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_all_cluster_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# t-SNE plot for kmean clusters
set.seed(052608)
tsne <- Rtsne(as.matrix(PAIN_km_filter_C[,2:ncol(PAIN_km_filter_C)]),check_duplicates = F)

tsne <- tsne$Y %>% data.frame() %>%
  setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(fit_PAIN$cluster))

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))

fwrite(tsne,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/tsne_PAIN_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PAIN <- aggregate(PAIN_km_filter_C[,2:ncol(PAIN_km_filter_C)],by=list(fit_PAIN$cluster),FUN=mean)
PAIN_all_cluster_count <- PAIN_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PAIN <- merge(clust_mean_PAIN,PAIN_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PAIN,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/cluster_mean_PAIN_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Shape PAIN cluster dataset to the optimization format
names(clust_mean_PAIN)[1] = "cluster"
clust_mean_PAIN <- as.data.frame.matrix(clust_mean_PAIN)
clust_mean_PAIN_reshape <- melt(clust_mean_PAIN[,c(1:ncol(clust_mean_PAIN)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_PAIN)-1))

names(clust_mean_PAIN_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PAIN_reshape = clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$brand != "cluster",]

PAIN_brand <- PAIN_score_mrkt_rel[PAIN_score_mrkt_rel$Segment=="C",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PAIN_reshape <- merge(clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$emails_pred_avg>0,],PAIN_brand,
                                by=c("brand","cluster"),all.x=T)

# core brands in PAIN bags
clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape %>% 
  filter(brand %in% c("LYRICA","XELJANZ","FLECTOR","EMBEDA","CELEBREX","ARTHROTEC","RARE DISEASES"))

clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PAIN_reshape_bag_top <- clust_mean_PAIN_reshape_bag[clust_mean_PAIN_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)

# 2 OTHER brand slots outside the list
brand_extra <- clust_mean_PAIN_reshape %>%
  filter(brand %in% c("CHANTIX","PROTONIX","PREVNAR 13")) %>% arrange(-overall_score_final)

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 2,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PAIN_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PAIN_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
PAIN_top_10_brand_clust <- PAIN_top_10_brand_clust[!(PAIN_top_10_brand_clust$brand %in% c("PREVNAR 13","RARE DISEASES","PROTONIX")),]

fwrite(PAIN_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_pre_bag_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################## Apply 10 clusters #######################
k2=10

# K-Means Cluster Analysis
set.seed(052609)
fit_PAIN <- kmeans(as.matrix(PAIN_km_filter_C[,2:ncol(PAIN_km_filter_C)]),centers = k2,nstart=50,iter.max=15)

# append cluster assignment
PAIN_all_cluster <- data.frame(PAIN_km_filter_C, cluster = fit_PAIN$cluster)
table(PAIN_all_cluster$cluster)
fwrite(PAIN_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_all_cluster_10_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# t-SNE plot for kmean clusters
tsne$cluster <- as.factor(fit_PAIN$cluster)

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))



################################## Segment D ###############################################
# K-Means Cluster Analysis
set.seed(052610)
fit_PAIN <- kmeans(as.matrix(PAIN_km_filter_D[,2:ncol(PAIN_km_filter_D)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
PAIN_all_cluster <- data.frame(PAIN_km_filter_D, cluster = fit_PAIN$cluster)
table(PAIN_all_cluster$cluster)
fwrite(PAIN_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_all_cluster_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PAIN <- aggregate(PAIN_km_filter_D[,2:ncol(PAIN_km_filter_D)],by=list(fit_PAIN$cluster),FUN=mean)
PAIN_all_cluster_count <- PAIN_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PAIN <- merge(clust_mean_PAIN,PAIN_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PAIN,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/cluster_mean_PAIN_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape PAIN cluster dataset to the optimization format
names(clust_mean_PAIN)[1] = "cluster"
clust_mean_PAIN <- as.data.frame.matrix(clust_mean_PAIN)
clust_mean_PAIN_reshape <- melt(clust_mean_PAIN[,c(1:ncol(clust_mean_PAIN)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_PAIN)-1))

names(clust_mean_PAIN_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PAIN_reshape = clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$brand != "cluster",]

PAIN_brand <- PAIN_score_mrkt_rel[PAIN_score_mrkt_rel$Segment=="D",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PAIN_reshape <- merge(clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$emails_pred_avg>0,],PAIN_brand,
                                 by=c("brand","cluster"),all.x=T)

# core brands in PAIN bags
clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape %>% 
  filter(brand %in% c("LYRICA","XELJANZ","FLECTOR","EMBEDA","CELEBREX","ARTHROTEC","RARE DISEASES"))

clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PAIN_reshape_bag_top <- clust_mean_PAIN_reshape_bag[clust_mean_PAIN_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)

# 2 OTHER brand slots outside the list
brand_extra <- clust_mean_PAIN_reshape %>%
  filter(brand %in% c("CHANTIX","PROTONIX","PREVNAR 13")) %>% arrange(-overall_score_final)

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 2,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PAIN_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PAIN_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
PAIN_top_10_brand_clust <- PAIN_top_10_brand_clust[!(PAIN_top_10_brand_clust$brand %in% c("PREVNAR 13","RARE DISEASES","PROTONIX")),]

fwrite(PAIN_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_pre_bag_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment A ###############################################
PAIN_km_filter_A$cluster <- 1
fwrite(PAIN_km_filter_A,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_all_cluster_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_PAIN <- aggregate(PAIN_km_filter_A[,2:(ncol(PAIN_km_filter_A)-1)],by=list(PAIN_km_filter_A$cluster),FUN=mean)

# Shape PAIN cluster dataset to the optimization format
names(clust_mean_PAIN)[1] = "cluster"
clust_mean_PAIN <- as.data.frame.matrix(clust_mean_PAIN)
clust_mean_PAIN_reshape <- melt(clust_mean_PAIN[,c(1:ncol(clust_mean_PAIN)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_PAIN)-1))

names(clust_mean_PAIN_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PAIN_reshape = clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$brand != "cluster",]

PAIN_brand <- PAIN_score_mrkt_rel[PAIN_score_mrkt_rel$Segment=="A",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PAIN_reshape <- merge(clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$emails_pred_avg>0,],PAIN_brand,
                                 by=c("brand","cluster"),all.x=T)

# core brands in PAIN bags
clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape %>% 
  filter(brand %in% c("LYRICA","XELJANZ","FLECTOR","EMBEDA","CELEBREX","PRISTIQ","ARTHROTEC","RARE DISEASES"))

clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PAIN_reshape_bag_top <- clust_mean_PAIN_reshape_bag[clust_mean_PAIN_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)

# 2 OTHER brand slots outside the list
brand_extra <- clust_mean_PAIN_reshape %>%
  filter(brand %in% c("CHANTIX","PROTONIX","PREVNAR 13")) %>% arrange(-overall_score_final)

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 2,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PAIN_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PAIN_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
PAIN_top_10_brand_clust <- PAIN_top_10_brand_clust[!(PAIN_top_10_brand_clust$brand %in% c("PREVNAR 13","CHANTIX","PROTONIX")),]
fwrite(PAIN_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_pre_bag_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment B ###############################################
PAIN_km_filter_B$cluster <- 1
fwrite(PAIN_km_filter_B,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_all_cluster_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_PAIN <- aggregate(PAIN_km_filter_B[,2:(ncol(PAIN_km_filter_B)-1)],by=list(PAIN_km_filter_B$cluster),FUN=mean)

# Shape PAIN cluster dataset to the optimization format
names(clust_mean_PAIN)[1] = "cluster"
clust_mean_PAIN <- as.data.frame.matrix(clust_mean_PAIN)
clust_mean_PAIN_reshape <- melt(clust_mean_PAIN[,c(1:ncol(clust_mean_PAIN)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_PAIN)-1))

names(clust_mean_PAIN_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PAIN_reshape = clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$brand != "cluster",]

PAIN_brand <- PAIN_score_mrkt_rel[PAIN_score_mrkt_rel$Segment=="B",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PAIN_reshape <- merge(clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$emails_pred_avg>0,],PAIN_brand,
                                 by=c("brand","cluster"),all.x=T)

# core brands in PAIN bags
clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape %>% 
  filter(brand %in% c("LYRICA","XELJANZ","FLECTOR","EMBEDA","CELEBREX","PRISTIQ","ARTHROTEC","RARE DISEASES"))

clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PAIN_reshape_bag_top <- clust_mean_PAIN_reshape_bag[clust_mean_PAIN_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)

# 2 OTHER brand slots outside the list
brand_extra <- clust_mean_PAIN_reshape %>%
  filter(brand %in% c("CHANTIX","PROTONIX","PREVNAR 13")) %>% arrange(-overall_score_final)

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 2,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PAIN_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PAIN_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
PAIN_top_10_brand_clust <- PAIN_top_10_brand_clust[!(PAIN_top_10_brand_clust$brand %in% c("PREVNAR 13","CHANTIX","PROTONIX")),]
fwrite(PAIN_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_pre_bag_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment E ###############################################
PAIN_km_filter_E$cluster <- 1
fwrite(PAIN_km_filter_E,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_all_cluster_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_PAIN <- aggregate(PAIN_km_filter_E[,2:(ncol(PAIN_km_filter_E)-1)],by=list(PAIN_km_filter_E$cluster),FUN=mean)

# Shape PAIN cluster dataset to the optimization format
names(clust_mean_PAIN)[1] = "cluster"
clust_mean_PAIN <- as.data.frame.matrix(clust_mean_PAIN)
clust_mean_PAIN_reshape <- melt(clust_mean_PAIN[,c(1:ncol(clust_mean_PAIN)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_PAIN)-1))

names(clust_mean_PAIN_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PAIN_reshape = clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$brand != "cluster",]

PAIN_brand <- PAIN_score_mrkt_rel[PAIN_score_mrkt_rel$Segment=="E",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PAIN_reshape <- merge(clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$emails_pred_avg>0,],PAIN_brand,
                                 by=c("brand","cluster"),all.x=T)

# core brands in PAIN bags
clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape %>% 
  filter(brand %in% c("LYRICA","XELJANZ","FLECTOR","EMBEDA","CELEBREX","PRISTIQ","RELPAX","RARE DISEASES"))

clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PAIN_reshape_bag_top <- clust_mean_PAIN_reshape_bag[clust_mean_PAIN_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)

# 2 OTHER brand slots outside the list
brand_extra <- clust_mean_PAIN_reshape %>%
  filter(brand %in% c("CHANTIX","PROTONIX","PREVNAR 13")) %>% arrange(-overall_score_final)

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 2,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PAIN_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PAIN_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
PAIN_top_10_brand_clust <- PAIN_top_10_brand_clust[!(PAIN_top_10_brand_clust$brand %in% c("RARE DISEASES","PREVNAR 13","CHANTIX")),]

fwrite(PAIN_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_pre_bag_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment F ###############################################
PAIN_km_filter_F$cluster <- 1
fwrite(PAIN_km_filter_F,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_all_cluster_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

clust_mean_PAIN <- aggregate(PAIN_km_filter_F[,2:(ncol(PAIN_km_filter_F)-1)],by=list(PAIN_km_filter_F$cluster),FUN=mean)

# Shape PAIN cluster dataset to the optimization format
names(clust_mean_PAIN)[1] = "cluster"
clust_mean_PAIN <- as.data.frame.matrix(clust_mean_PAIN)
clust_mean_PAIN_reshape <- melt(clust_mean_PAIN[,c(1:ncol(clust_mean_PAIN)-1)],
                                id.vars = "cluster",
                                measure.vars = c(2:ncol(clust_mean_PAIN)-1))

names(clust_mean_PAIN_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PAIN_reshape = clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$brand != "cluster",]

PAIN_brand <- PAIN_score_mrkt_rel[PAIN_score_mrkt_rel$Segment=="F",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PAIN_reshape <- merge(clust_mean_PAIN_reshape[clust_mean_PAIN_reshape$emails_pred_avg>0,],PAIN_brand,
                                 by=c("brand","cluster"),all.x=T)

# core brands in PAIN bags
clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape %>% 
  filter(brand %in% c("LYRICA","XELJANZ","FLECTOR","EMBEDA","CELEBREX","PRISTIQ","RELPAX","RARE DISEASES"))

clust_mean_PAIN_reshape_bag <- clust_mean_PAIN_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PAIN_reshape_bag_top <- clust_mean_PAIN_reshape_bag[clust_mean_PAIN_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)

# 2 OTHER brand slots outside the list
brand_extra <- clust_mean_PAIN_reshape %>%
  filter(brand %in% c("CHANTIX","PROTONIX","PREVNAR 13")) %>% arrange(-overall_score_final)

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 2,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PAIN_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PAIN_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
PAIN_top_10_brand_clust <- PAIN_top_10_brand_clust[!(PAIN_top_10_brand_clust$brand %in% c("PREVNAR 13","CHANTIX")),]

fwrite(PAIN_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PAIN/PAIN_pre_bag_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

