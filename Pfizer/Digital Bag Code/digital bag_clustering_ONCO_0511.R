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
library(dplyr)

# Read in digital bag output
digital_bag_email_10 <- fread("Chi Cheng/Master Digital Bag/Output Data/digital_bag_email_distribution_top10_0511.csv")

# List of brands that is predicted to send at least an email or brand is XTANDI
brand_km <- unique(digital_bag_email_10[digital_bag_email_10$emails_pred_final>0 | digital_bag_email_10$brand=="XTANDI",]$brand)

# ONCO pre-kmeans dataset
digital_bag_email_ONCO_km <- digital_bag_email_10[digital_bag_email_10$spec_grp_max == "ONCO" & 
                                                   digital_bag_email_10$brand %in% brand_km, 
                                                 c("pfz_cust_id","brand","emails_pred_final")]
digital_bag_email_ONCO_km <- digital_bag_email_ONCO_km[!duplicated(digital_bag_email_ONCO_km[,c(1:2)]),]
summary(digital_bag_email_ONCO_km)

# Reshape ONCO output to wide table
ONCO_km <- cast(digital_bag_email_ONCO_km,pfz_cust_id~brand)
fwrite(ONCO_km,"Chi Cheng/Processed Data/ONCO_km_0511.csv",row.names = F)

# Impute missing as 0
ONCO_km <- fread("Chi Cheng/Processed Data/ONCO_km_0511.csv")
ONCO_km[is.na(ONCO_km)] <- 0
# Keep HCPs that are predicted to receive at least one email
ONCO_km_filter <- ONCO_km[rowSums(ONCO_km)>0,]

# Assign k=5 for clustering analysis
k1=5

# K-Means Cluster Analysis
set.seed(051102)
fit_ONCO <- kmeans(as.matrix(ONCO_km_filter[,c(2:ncol(ONCO_km_filter))]),k1)

# append cluster assignment
ONCO_all_cluster <- data.frame(ONCO_km_filter, cluster = fit_ONCO$cluster)
table(ONCO_all_cluster$cluster)
fwrite(ONCO_all_cluster,"Chi Cheng/Output/ONCO_all_cluster_0511.csv",row.names = F)

# get cluster means and count
clust_mean_ONCO <- aggregate(ONCO_km_filter[,c(2:ncol(ONCO_km_filter))],by=list(fit_ONCO$cluster),FUN=mean)
ONCO_all_cluster_count <- ONCO_all_cluster %>% group_by(cluster) %>%
  dplyr::summarize(
    count = n())

clust_mean_ONCO <- merge(clust_mean_ONCO,ONCO_all_cluster_count,by.x="Group.1",by.y="cluster")
fwrite(clust_mean_ONCO,"Chi Cheng/Output/cluster_mean_ONCO_0511.csv",row.names = F)