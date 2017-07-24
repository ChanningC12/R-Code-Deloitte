################################# Digital Bag Chart Preparation - RHU #############################################
bag_A <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_A_0526.csv")
bag_B <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_B_0526.csv")
bag_C <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_C_0526.csv")
bag_D <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_D_0526.csv")
bag_E <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_E_0526.csv")
bag_F <- read.csv("D:/Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_pre_bag_F_0526.csv")

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
RHU_all_cluster_A <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_A_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
RHU_all_cluster_B <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_B_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
RHU_all_cluster_C <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_C_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
RHU_all_cluster_D <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_D_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
RHU_all_cluster_E <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_E_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))
RHU_all_cluster_F <- fread(paste("Chi Cheng/Output/Digital bag/Clustering/RHU/RHU_all_cluster_F_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""))

RHU_all_cluster_A$Segment <- "A"
RHU_all_cluster_B$Segment <- "B"
RHU_all_cluster_C$Segment <- "C"
RHU_all_cluster_D$Segment <- "D"
RHU_all_cluster_E$Segment <- "E"
RHU_all_cluster_F$Segment <- "F"

RHU_all_cluster_A <- RHU_all_cluster_A[,c("pfz_cust_id","cluster","Segment")]
RHU_all_cluster_B <- RHU_all_cluster_B[,c("pfz_cust_id","cluster","Segment")]
RHU_all_cluster_C <- RHU_all_cluster_C[,c("pfz_cust_id","cluster","Segment")]
RHU_all_cluster_D <- RHU_all_cluster_D[,c("pfz_cust_id","cluster","Segment")]
RHU_all_cluster_E <- RHU_all_cluster_E[,c("pfz_cust_id","cluster","Segment")]
RHU_all_cluster_F <- RHU_all_cluster_F[,c("pfz_cust_id","cluster","Segment")]

cluster <- rbind(RHU_all_cluster_A,RHU_all_cluster_B,RHU_all_cluster_C,RHU_all_cluster_D,RHU_all_cluster_E,RHU_all_cluster_F)

bag_master <- merge(cluster,bag,by=c("Segment","cluster"),allow.cartesian=TRUE)


############################# Summary Report ###################################
RHU_master <- digital_bag_email_10[digital_bag_email_10$spec_grp_max=="RHU",]
colSums(is.na(RHU_master))

RHU_master <- merge(RHU_master,bag_master,by=c("pfz_cust_id","brand","Segment"),all=T)
colSums(is.na(RHU_master))
table(RHU_master$cluster)


RHU_master$brand_trx_dol_agg <- with(RHU_master,
                                      ifelse(brand %in% c("ADVIL","NEXIUM","PREPARATION H"),brand_trx_dol_add,
                                             brand_trx_dol))

# Add indicator on whether a HCP received an email for a specific brand
RHU_master <- RHU_master %>%
  mutate(
    hcp_count_2016 = ifelse(emails_dlvd_2016>0,1,0),
    hcp_count_pred = ifelse(emails_pred_final>0,1,0),
    hcp_count_pred_bag = ifelse(emails_bag>0,1,0)
    )

# Brand Summary 1: count of emails delivered in 2016 and emails prediction by brand
digital_bag_ALL_summary_1 <- RHU_master %>%
  group_by(brand) %>%
  dplyr::summarize(
    emails_dlvd_2016 = sum(emails_dlvd_2016,na.rm = T),
    emails_pred = sum(emails_pred_final,na.rm = T),
    emails_pred_bag = sum(emails_bag,na.rm=T))

# Add an indicator on brand_trx_dol and market_trx_dol for HCPs who received emails in 2016 / in prediction
RHU_master$brand_trx_dol_2016 = ifelse(RHU_master$hcp_count_2016==1,RHU_master$brand_trx_dol_agg,0)
RHU_master$brand_trx_dol_pred = ifelse(RHU_master$hcp_count_pred==1,RHU_master$brand_trx_dol_agg,0)
RHU_master$brand_trx_dol_pred_bag = ifelse(RHU_master$hcp_count_pred_bag==1,RHU_master$brand_trx_dol_agg,0)

RHU_master$mrkt_trx_dol_2016 = ifelse(RHU_master$hcp_count_2016==1,RHU_master$mrkt_trx_dol,0)
RHU_master$mrkt_trx_dol_pred = ifelse(RHU_master$hcp_count_pred==1,RHU_master$mrkt_trx_dol,0)
RHU_master$mrkt_trx_dol_pred_bag = ifelse(RHU_master$hcp_count_pred_bag==1,RHU_master$mrkt_trx_dol,0)


# Brand Summary 2: count of HCPs received emails in 2016 / in prediction, total brand_trx_dol and market_trx_dol
digital_bag_ALL_summary_2 <- RHU_master %>%
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
fwrite(digital_bag_ALL_summary,paste("D:/Chi Cheng/Output/RHU_summary_",substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),".csv",sep = ""),row.names = F)











