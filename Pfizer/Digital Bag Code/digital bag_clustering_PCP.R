######################################################################################################################################
###                                                Digital Bag Output PCP Cluster                                                 ###
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
# Read in digital bag summary output to get brand scores / market relevance
PCP_score_mrkt_rel <- fread("Chi Cheng/Output/PCP_avg_score_mrkt_rel_0601.csv")
names(PCP_score_mrkt_rel)[2] <- "Segment"
names(PCP_score_mrkt_rel)[3] <- "cluster"

# List of brands that is predicted to send at least an email OR the brand is XTANDI
brand_km <- unique(digital_bag_email_10[digital_bag_email_10$emails_pred_final>0 | digital_bag_email_10$brand=="XTANDI",]$brand)

# PCP pre-kmeans dataset
digital_bag_email_PCP_km <- digital_bag_email_10[digital_bag_email_10$spec_grp_max == "PCP" & 
                                                   digital_bag_email_10$brand %in% brand_km, 
                                                 c("pfz_cust_id","brand","emails_pred_final")]
digital_bag_email_PCP_km <- digital_bag_email_PCP_km[!duplicated(digital_bag_email_PCP_km[,c(1:2)]),]
summary(digital_bag_email_PCP_km)

# Reshape PCP output to wide table
PCP_km <- cast(digital_bag_email_PCP_km,pfz_cust_id~brand)

# Append segment
segment <- fread("Chi Cheng/Master Digital Bag/Data Loading/segment_all.csv")
names(segment)[1] <- "pfz_cust_id"
PCP_km <- merge(PCP_km,segment,by="pfz_cust_id",all.x=T)
colSums(is.na(PCP_km))
table(PCP_km$Segment)
fwrite(PCP_km,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_km_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Impute missing as 0
PCP_km <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_km_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
PCP_km <- as.data.frame.matrix(PCP_km)
PCP_km[is.na(PCP_km)] <- 0
# Keep HCPs that are predicted to receive at least one email
PCP_km_filter <- PCP_km[rowSums(PCP_km[,c(2:57)])>0,]

# Split PCP data by segment
PCP_km_filter_A <- PCP_km_filter[PCP_km_filter$Segment=="A",c(1:57)]
PCP_km_filter_B <- PCP_km_filter[PCP_km_filter$Segment=="B",c(1:57)]
PCP_km_filter_C <- PCP_km_filter[PCP_km_filter$Segment=="C",c(1:57)]
PCP_km_filter_D <- PCP_km_filter[PCP_km_filter$Segment=="D",c(1:57)]
PCP_km_filter_E <- PCP_km_filter[PCP_km_filter$Segment=="E",c(1:57)]
PCP_km_filter_F <- PCP_km_filter[PCP_km_filter$Segment=="F",c(1:57)]

detach("package:reshape",unload=TRUE)

# set average score and market relevance threshold
PCP_score_mrkt_rel$ind <- ifelse(PCP_score_mrkt_rel$brand 
                                 %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13",
                                        "PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                                        "QUILLIVANT XR","PROTONIX"),1,
                                 ifelse(PCP_score_mrkt_rel$brand %in% c("ADVIL","NEXIUM","PREPARATION H","RARE DISEASES") & 
                                          PCP_score_mrkt_rel$overall_score_final>=0.1,1,
                                        ifelse(PCP_score_mrkt_rel$overall_score_final>=0.1 & 
                                                 PCP_score_mrkt_rel$mrkt_rel_avg>=0.005,1,0)))
table(PCP_score_mrkt_rel$ind)

######################################### Segment C ####################################################
# Determine the number of clusters
# WSS: with-in-sum-of-squares, total distance of data points from their respective centroids
set.seed(053101)
k.max <- 20
wss <- sapply(1:k.max,function(k){
  kmeans(as.matrix(PCP_km_filter_C[,2:ncol(PCP_km_filter_C)]),k,nstart=50,iter.max=15)$tot.withinss})
plot(1:k.max,wss,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

PCP_wss <- data.frame(cluster=1:k.max, wss=wss)
fwrite(PCP_wss,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_wss_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Assign k=2 for clustering analysis
k1=4

# K-Means Cluster Analysis
set.seed(053102)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_C[,2:ncol(PCP_km_filter_C)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
PCP_all_cluster <- data.frame(PCP_km_filter_C, cluster = fit_PCP$cluster)
table(PCP_all_cluster$cluster)
fwrite(PCP_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_all_cluster_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# t-SNE plot for kmean clusters
set.seed(053103)
tsne <- Rtsne(as.matrix(PCP_km_filter_C[,2:ncol(PCP_km_filter_C)]),check_duplicates = F)

tsne <- tsne$Y %>% data.frame() %>%
  setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(fit_PCP$cluster))

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))

fwrite(tsne,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/tsne_PCP_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PCP <- aggregate(PCP_km_filter_C[,2:ncol(PCP_km_filter_C)],by=list(fit_PCP$cluster),FUN=mean)
PCP_all_cluster_count <- PCP_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PCP <- merge(clust_mean_PCP,PCP_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PCP,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/cluster_mean_PCP_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Shape PCP cluster dataset to the optimization format
names(clust_mean_PCP)[1] = "cluster"
clust_mean_PCP <- as.data.frame.matrix(clust_mean_PCP)
clust_mean_PCP_reshape <- melt(clust_mean_PCP[,c(1:ncol(clust_mean_PCP)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_PCP)-1))

names(clust_mean_PCP_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PCP_reshape = clust_mean_PCP_reshape[clust_mean_PCP_reshape$brand != "cluster",]

PCP_brand <- PCP_score_mrkt_rel[PCP_score_mrkt_rel$Segment=="C",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PCP_reshape <- merge(clust_mean_PCP_reshape[clust_mean_PCP_reshape$emails_pred_avg>0,],PCP_brand,
                                by=c("brand","cluster"),all.x=T)

# 13 brands in PCP bags
clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape %>% 
  filter(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                      "QUILLIVANT XR","PROTONIX"))

clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PCP_reshape_bag_top <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)
brand_extra_core <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$rank_score>8,]

# 4 OTHER brand slots outside the list
brand_extra <- clust_mean_PCP_reshape %>%
  filter(!(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                        "XTANDI","QUILLIVANT XR","PROTONIX"))) %>% arrange(-overall_score_final)

brand_extra <- rbind(as.data.frame(brand_extra_core[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 4,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PCP_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PCP_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)

fwrite(PCP_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_pre_bag_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)



################################## Segment D ###############################################
# K-Means Cluster Analysis
set.seed(053105)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_D[,2:ncol(PCP_km_filter_D)]),centers = k1,nstart=50,iter.max=15)

# append cluster assignment
PCP_all_cluster <- data.frame(PCP_km_filter_D, cluster = fit_PCP$cluster)
table(PCP_all_cluster$cluster)
fwrite(PCP_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_all_cluster_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PCP <- aggregate(PCP_km_filter_D[,2:ncol(PCP_km_filter_D)],by=list(fit_PCP$cluster),FUN=mean)
PCP_all_cluster_count <- PCP_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PCP <- merge(clust_mean_PCP,PCP_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PCP,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/cluster_mean_PCP_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape PCP cluster dataset to the optimization format
names(clust_mean_PCP)[1] = "cluster"
clust_mean_PCP <- as.data.frame.matrix(clust_mean_PCP)
clust_mean_PCP_reshape <- melt(clust_mean_PCP[,c(1:ncol(clust_mean_PCP)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_PCP)-1))

names(clust_mean_PCP_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PCP_reshape = clust_mean_PCP_reshape[clust_mean_PCP_reshape$brand != "cluster",]

PCP_brand <- PCP_score_mrkt_rel[PCP_score_mrkt_rel$Segment=="D",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PCP_reshape <- merge(clust_mean_PCP_reshape[clust_mean_PCP_reshape$emails_pred_avg>0,],PCP_brand,
                                by=c("brand","cluster"),all.x=T)

# 13 brands in PCP bags
clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape %>% 
  filter(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                      "QUILLIVANT XR","PROTONIX"))

clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PCP_reshape_bag_top <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)
brand_extra_core <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$rank_score>8,]

# 4 OTHER brand slots outside the list
brand_extra <- clust_mean_PCP_reshape %>%
  filter(!(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                        "XTANDI","QUILLIVANT XR","PROTONIX"))) %>% arrange(-overall_score_final)

brand_extra <- rbind(as.data.frame(brand_extra_core[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 4,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PCP_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PCP_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)


fwrite(PCP_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_pre_bag_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

################################## Segment A ###############################################
# K-Means Cluster Analysis
k2=10
set.seed(053106)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_A[,2:ncol(PCP_km_filter_A)]),centers = k2,nstart=50,iter.max=15)

# append cluster assignment
PCP_all_cluster <- data.frame(PCP_km_filter_A, cluster = fit_PCP$cluster)
table(PCP_all_cluster$cluster)
fwrite(PCP_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_all_cluster_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PCP <- aggregate(PCP_km_filter_A[,2:ncol(PCP_km_filter_A)],by=list(fit_PCP$cluster),FUN=mean)
PCP_all_cluster_count <- PCP_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PCP <- merge(clust_mean_PCP,PCP_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PCP,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/cluster_mean_PCP_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape PCP cluster dataset to the optimization format
names(clust_mean_PCP)[1] = "cluster"
clust_mean_PCP <- as.data.frame.matrix(clust_mean_PCP)
clust_mean_PCP_reshape <- melt(clust_mean_PCP[,c(1:ncol(clust_mean_PCP)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_PCP)-1))

names(clust_mean_PCP_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PCP_reshape = clust_mean_PCP_reshape[clust_mean_PCP_reshape$brand != "cluster",]

PCP_brand <- PCP_score_mrkt_rel[PCP_score_mrkt_rel$Segment=="A",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PCP_reshape <- merge(clust_mean_PCP_reshape[clust_mean_PCP_reshape$emails_pred_avg>0,],PCP_brand,
                                by=c("brand","cluster"),all.x=T)

# 13 brands in PCP bags
clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape %>% 
  filter(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                      "QUILLIVANT XR","PROTONIX"))

clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PCP_reshape_bag_top <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)
brand_extra_core <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$rank_score>8,]

# 4 OTHER brand slots outside the list
brand_extra <- clust_mean_PCP_reshape %>%
  filter(!(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                        "XTANDI","QUILLIVANT XR","PROTONIX"))) %>% arrange(-overall_score_final)

brand_extra <- rbind(as.data.frame(brand_extra_core[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 4,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PCP_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PCP_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)


fwrite(PCP_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_pre_bag_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# Generate t-SNE plot for 4 cluster and 10 cluster
# t-SNE plot for kmean clusters
set.seed(053110)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_A[,2:ncol(PCP_km_filter_C)]),centers = k1,nstart=50,iter.max=15)
tsne <- Rtsne(as.matrix(PCP_km_filter_A[,2:ncol(PCP_km_filter_A)]),check_duplicates = F)

tsne <- tsne$Y %>% data.frame() %>%
  setNames(c("X", "Y")) %>% 
  mutate(cluster = factor(fit_PCP$cluster))

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))

fwrite(tsne,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/tsne_PCP_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

k2=10
# K-Means Cluster Analysis
set.seed(053111)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_A[,2:ncol(PCP_km_filter_A)]),centers = k2,nstart=50,iter.max=15)

# t-SNE plot for kmean clusters
tsne$cluster <- as.factor(fit_PCP$cluster)

ggplot(aes(x = X, y = Y), data = tsne) + geom_point(aes(color = cluster))



################################## Segment B ###############################################
# K-Means Cluster Analysis
k2=10
set.seed(053107)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_B[,2:ncol(PCP_km_filter_B)]),centers = k2,nstart=50,iter.max=15)

# append cluster assignment
PCP_all_cluster <- data.frame(PCP_km_filter_B, cluster = fit_PCP$cluster)
table(PCP_all_cluster$cluster)
fwrite(PCP_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_all_cluster_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PCP <- aggregate(PCP_km_filter_B[,2:ncol(PCP_km_filter_B)],by=list(fit_PCP$cluster),FUN=mean)
PCP_all_cluster_count <- PCP_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PCP <- merge(clust_mean_PCP,PCP_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PCP,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/cluster_mean_PCP_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape PCP cluster dataset to the optimization format
names(clust_mean_PCP)[1] = "cluster"
clust_mean_PCP <- as.data.frame.matrix(clust_mean_PCP)
clust_mean_PCP_reshape <- melt(clust_mean_PCP[,c(1:ncol(clust_mean_PCP)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_PCP)-1))

names(clust_mean_PCP_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PCP_reshape = clust_mean_PCP_reshape[clust_mean_PCP_reshape$brand != "cluster",]

PCP_brand <- PCP_score_mrkt_rel[PCP_score_mrkt_rel$Segment=="B",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PCP_reshape <- merge(clust_mean_PCP_reshape[clust_mean_PCP_reshape$emails_pred_avg>0,],PCP_brand,
                                by=c("brand","cluster"),all.x=T)

# 13 brands in PCP bags
clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape %>% 
  filter(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                      "QUILLIVANT XR","PROTONIX"))

clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PCP_reshape_bag_top <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)
brand_extra_core <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$rank_score>8,]

# 4 OTHER brand slots outside the list
brand_extra <- clust_mean_PCP_reshape %>%
  filter(!(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                        "XTANDI","QUILLIVANT XR","PROTONIX"))) %>% arrange(-overall_score_final)

brand_extra <- rbind(as.data.frame(brand_extra_core[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 4,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PCP_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PCP_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)

fwrite(PCP_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_pre_bag_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

################################## Segment E ###############################################
# K-Means Cluster Analysis
k3=2
set.seed(053108)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_E[,2:ncol(PCP_km_filter_E)]),centers = k3,nstart=50,iter.max=15)

# append cluster assignment
PCP_all_cluster <- data.frame(PCP_km_filter_E, cluster = fit_PCP$cluster)
table(PCP_all_cluster$cluster)
fwrite(PCP_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_all_cluster_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PCP <- aggregate(PCP_km_filter_E[,2:ncol(PCP_km_filter_E)],by=list(fit_PCP$cluster),FUN=mean)
PCP_all_cluster_count <- PCP_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PCP <- merge(clust_mean_PCP,PCP_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PCP,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/cluster_mean_PCP_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape PCP cluster dataset to the optimization format
names(clust_mean_PCP)[1] = "cluster"
clust_mean_PCP <- as.data.frame.matrix(clust_mean_PCP)
clust_mean_PCP_reshape <- melt(clust_mean_PCP[,c(1:ncol(clust_mean_PCP)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_PCP)-1))

names(clust_mean_PCP_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PCP_reshape = clust_mean_PCP_reshape[clust_mean_PCP_reshape$brand != "cluster",]

PCP_brand <- PCP_score_mrkt_rel[PCP_score_mrkt_rel$Segment=="E",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PCP_reshape <- merge(clust_mean_PCP_reshape[clust_mean_PCP_reshape$emails_pred_avg>0,],PCP_brand,
                                by=c("brand","cluster"),all.x=T)

# 13 brands in PCP bags
clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape %>% 
  filter(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                      "QUILLIVANT XR","PROTONIX"))

clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PCP_reshape_bag_top <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)
brand_extra_core <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$rank_score>8,]

# 4 OTHER brand slots outside the list
brand_extra <- clust_mean_PCP_reshape %>%
  filter(!(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                        "XTANDI","QUILLIVANT XR","PROTONIX"))) %>% arrange(-overall_score_final)

brand_extra <- rbind(as.data.frame(brand_extra_core[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 4,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PCP_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PCP_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
fwrite(PCP_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_pre_bag_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


################################## Segment F ###############################################
# K-Means Cluster Analysis
k3=2
set.seed(053109)
fit_PCP <- kmeans(as.matrix(PCP_km_filter_F[,2:ncol(PCP_km_filter_F)]),centers = k3,nstart=50,iter.max=15)

# append cluster assignment
PCP_all_cluster <- data.frame(PCP_km_filter_F, cluster = fit_PCP$cluster)
table(PCP_all_cluster$cluster)
fwrite(PCP_all_cluster,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_all_cluster_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)

# get cluster means and count
clust_mean_PCP <- aggregate(PCP_km_filter_F[,2:ncol(PCP_km_filter_F)],by=list(fit_PCP$cluster),FUN=mean)
PCP_all_cluster_count <- PCP_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_PCP <- merge(clust_mean_PCP,PCP_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_PCP,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/cluster_mean_PCP_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)


# Shape PCP cluster dataset to the optimization format
names(clust_mean_PCP)[1] = "cluster"
clust_mean_PCP <- as.data.frame.matrix(clust_mean_PCP)
clust_mean_PCP_reshape <- melt(clust_mean_PCP[,c(1:ncol(clust_mean_PCP)-1)],
                               id.vars = "cluster",
                               measure.vars = c(2:ncol(clust_mean_PCP)-1))

names(clust_mean_PCP_reshape) = c("cluster","brand","emails_pred_avg")
clust_mean_PCP_reshape = clust_mean_PCP_reshape[clust_mean_PCP_reshape$brand != "cluster",]

PCP_brand <- PCP_score_mrkt_rel[PCP_score_mrkt_rel$Segment=="F",c("Segment","overall_score_final","brand","cluster","ind")]

# merge in the score avg
clust_mean_PCP_reshape <- merge(clust_mean_PCP_reshape[clust_mean_PCP_reshape$emails_pred_avg>0,],PCP_brand,
                                by=c("brand","cluster"),all.x=T)

# 13 brands in PCP bags
clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape %>% 
  filter(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                      "QUILLIVANT XR","PROTONIX"))

clust_mean_PCP_reshape_bag <- clust_mean_PCP_reshape_bag %>% 
  dplyr::arrange(cluster,-overall_score_final) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(
    rank_score = rank(-overall_score_final,ties.method = "min")
  )

clust_mean_PCP_reshape_bag_top <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$ind==1,] %>% group_by(cluster) %>% top_n(n = 8,wt = overall_score_final)
brand_extra_core <- clust_mean_PCP_reshape_bag[clust_mean_PCP_reshape_bag$rank_score>8,]

# 4 OTHER brand slots outside the list
brand_extra <- clust_mean_PCP_reshape %>%
  filter(!(brand %in% c("LYRICA","VIAGRA","ELIQUIS","CHANTIX","PREVNAR 13","PRISTIQ","PREMARIN","PREMARIN VAGINAL","TOVIAZ","IBRANCE","CELEBREX",
                        "XTANDI","QUILLIVANT XR","PROTONIX"))) %>% arrange(-overall_score_final)

brand_extra <- rbind(as.data.frame(brand_extra_core[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra <- brand_extra[brand_extra$ind==1,] %>% group_by(cluster) %>% top_n(n = 4,wt = overall_score_final)

brand_extra <- rbind(as.data.frame(clust_mean_PCP_reshape_bag_top[,c(1:6)]),
                     as.data.frame(brand_extra[,c(1:6)]))

brand_extra$emails_pred_adj <- with(brand_extra,ifelse(emails_pred_avg==0,0,
                                                       ifelse(emails_pred_avg<3,3,
                                                              ifelse(emails_pred_avg<6,6,
                                                                     ifelse(emails_pred_avg<18,12,24)))))

PCP_top_10_brand_clust <- brand_extra %>% arrange(cluster,-emails_pred_adj)
fwrite(PCP_top_10_brand_clust,paste("Chi Cheng/Output/Digital bag/Clustering/PCP/PCP_pre_bag_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)




