################################# Digital Bag Chart Preparation - ONCO #############################################
bag_A <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_A_0525.csv")
bag_B <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_B_0525.csv")
bag_C <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_C_0525.csv")
bag_D <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_D_0525.csv")
bag_E <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_E_0525.csv")
bag_F <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_pre_bag_F_0525.csv")

bag_A$Segment <- "A"
bag_B$Segment <- "B"
bag_C$Segment <- "C"
bag_D$Segment <- "D"
bag_E$Segment <- "E"
bag_F$Segment <- "F"

bag_A <- bag_A[,c("Segment","cluster","brand","emails_pred_adj")]
bag_B <- bag_B[,c("Segment","cluster","brand","emails_pred_adj")]
bag_C <- bag_C[,c("Segment","cluster","brand","emails_pred_adj")]
bag_D <- bag_D[,c("Segment","cluster","brand","emails_pred_adj")]
bag_E <- bag_E[,c("Segment","cluster","brand","emails_pred_adj")]
bag_F <- bag_F[,c("Segment","cluster","brand","emails_pred_adj")]

bag <- rbind(bag_A,bag_B,bag_C,bag_D,bag_E,bag_F)
names(bag)[4] <- "emails_bag"

#################### cluster by pfz_cust_id #########################
ONCO_all_cluster_A <- fread("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_A_0525.csv")
ONCO_all_cluster_B <- fread("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_B_0525.csv")
ONCO_all_cluster_C <- fread("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_C_0525.csv")
ONCO_all_cluster_D <- fread("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_D_0525.csv")
ONCO_all_cluster_E <- fread("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_E_0525.csv")
ONCO_all_cluster_F <- fread("Chi Cheng/Output/Digital bag/Clustering/ONCO/ONCO_all_cluster_F_0525.csv")

ONCO_all_cluster_A$Segment <- "A"
ONCO_all_cluster_B$Segment <- "B"
ONCO_all_cluster_C$Segment <- "C"
ONCO_all_cluster_D$Segment <- "D"
ONCO_all_cluster_E$Segment <- "E"
ONCO_all_cluster_F$Segment <- "F"

ONCO_all_cluster_A <- ONCO_all_cluster_A[,c("pfz_cust_id","cluster","Segment")]
ONCO_all_cluster_B <- ONCO_all_cluster_B[,c("pfz_cust_id","cluster","Segment")]
ONCO_all_cluster_C <- ONCO_all_cluster_C[,c("pfz_cust_id","cluster","Segment")]
ONCO_all_cluster_D <- ONCO_all_cluster_D[,c("pfz_cust_id","cluster","Segment")]
ONCO_all_cluster_E <- ONCO_all_cluster_E[,c("pfz_cust_id","cluster","Segment")]
ONCO_all_cluster_F <- ONCO_all_cluster_F[,c("pfz_cust_id","cluster","Segment")]

cluster <- rbind(ONCO_all_cluster_A,ONCO_all_cluster_B,ONCO_all_cluster_C,ONCO_all_cluster_D,ONCO_all_cluster_E,ONCO_all_cluster_F)

bag_master <- merge(cluster,bag,by=c("Segment","cluster"),allow.cartesian=TRUE)


############################# Summary Report ###################################
ONCO_master <- digital_bag_email_10[digital_bag_email_10$spec_grp_max=="ONCO",]
colSums(is.na(ONCO_master))

ONCO_master <- merge(ONCO_master,bag_master,by=c("pfz_cust_id","brand","Segment"),all=T)
colSums(is.na(ONCO_master))
table(ONCO_master$cluster)

ONCO_master$brand_trx_dol_agg <- with(ONCO_master,
                                      ifelse(brand %in% c("ADVIL","NEXIUM","PREPARATION H"),brand_trx_dol_add,
                                             brand_trx_dol))

# Add indicator on whether a HCP received an email for a specific brand
ONCO_master <- ONCO_master %>%
  mutate(
    hcp_count_2016 = ifelse(emails_dlvd_2016>0,1,0),
    hcp_count_pred = ifelse(emails_pred_final>0,1,0),
    hcp_count_pred_bag = ifelse(emails_bag>0,1,0)
    )

# Brand Summary 1: count of emails delivered in 2016 and emails prediction by brand
digital_bag_ALL_summary_1 <- ONCO_master %>%
  group_by(brand) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016,na.rm = T),
    emails_pred = sum(emails_pred_final,na.rm = T),
    emails_pred_bag = sum(emails_bag,na.rm=T))

# Add an indicator on brand_trx_dol and market_trx_dol for HCPs who received emails in 2016 / in prediction
ONCO_master$brand_trx_dol_2016 = ifelse(ONCO_master$hcp_count_2016==1,ONCO_master$brand_trx_dol_agg,0)
ONCO_master$brand_trx_dol_pred = ifelse(ONCO_master$hcp_count_pred==1,ONCO_master$brand_trx_dol_agg,0)
ONCO_master$brand_trx_dol_pred_bag = ifelse(ONCO_master$hcp_count_pred_bag==1,ONCO_master$brand_trx_dol_agg,0)

ONCO_master$mrkt_trx_dol_2016 = ifelse(ONCO_master$hcp_count_2016==1,ONCO_master$mrkt_trx_dol,0)
ONCO_master$mrkt_trx_dol_pred = ifelse(ONCO_master$hcp_count_pred==1,ONCO_master$mrkt_trx_dol,0)
ONCO_master$mrkt_trx_dol_pred_bag = ifelse(ONCO_master$hcp_count_pred_bag==1,ONCO_master$mrkt_trx_dol,0)


# Brand Summary 2: count of HCPs received emails in 2016 / in prediction, total brand_trx_dol and market_trx_dol
digital_bag_ALL_summary_2 <- ONCO_master %>%
  group_by(brand) %>%
  dplyr::summarize(
    hcp_count_2016 = sum(hcp_count_2016,na.rm = T),
    hcp_count_pred = sum(hcp_count_pred,na.rm = T),
    hcp_count_pred_bag = sum(hcp_count_pred_bag,na.rm = T),
    brand_trx_dol_2016 = sum(brand_trx_dol_2016,na.rm = T),
    brand_trx_dol_pred = sum(brand_trx_dol_pred,na.rm = T),
    brand_trx_dol_pred_bag = sum(brand_trx_dol_pred_bag,na.rm = T),
    mrkt_trx_dol_2016 = sum(mrkt_trx_dol_2016,na.rm = T),
    mrkt_trx_dol_pred = sum(mrkt_trx_dol_pred,na.rm = T),
    mrkt_trx_dol_pred_bag = sum(mrkt_trx_dol_pred_bag,na.rm = T))

# Merge the two summaries
digital_bag_ALL_summary <- merge(digital_bag_ALL_summary_1,digital_bag_ALL_summary_2,by="brand")
fwrite(digital_bag_ALL_summary,paste("D:/Chi Cheng/Output/ONCO_summary_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)











