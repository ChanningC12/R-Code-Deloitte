library(dplyr)
library(cluster)

# load analysis data
setwd("D:\\Processed Data\\Headroom\\")
mrkt_rx_cust <- readRDS("2016_customer_data.rds")
summary(mrkt_rx_cust)

# load missing data
missing_cust <- readRDS("2016_missing_customer_data.rds")
summary(missing_cust)

# load duplicate spec ids
setwd("D:\\Soumyajit\\Output")
dup_cust <- readRDS("dup_cust.rds")

# define list of attributes for clustering
clust_att <- c("age","tenure", "perc_medicare_rx", "perc_medicaid_rx", "gender_max",
               "overall_website_sum","mdcl_grp_size","overall_details_sum","pract_typ_size",
               "pract_typ_spec_cnt","pract_typ_site_cnt","site_type_max")

mrkt_rx_spec <- mrkt_rx_cust[mrkt_rx_cust$spec_grp==spec,]
missing_dat_spec <- missing_cust[missing_cust$spec_grp==spec,]

# remove data missing cutomer id
nrow(missing_dat_spec[is.na(missing_dat_spec$pfz_cust_id)==T,])
missing_dat_spec <- missing_dat_spec[is.na(missing_dat_spec$pfz_cust_id)==F,]

# select data for clustering
dat_spec <- rbind(mrkt_rx_spec,missing_dat_spec)
dup_cust <- readRDS("dup_cust.RDS")
dat_spec <- dat_spec[dat_spec$pfz_cust_id %in% dup_cust$pfz_cust_id ==F,] # data with customer with unique spec
rm(dup_cust)
dat_spec$tenure <- ifelse(dat_spec$tenure <=0, median(dat_spec[dat_spec$tenure>0,"tenure"]$tenure,na.rm=T),
                              dat_spec$tenure)
dat_spec$age <- ifelse(dat_spec$age <=0, median(dat_spec[dat_spec$age>0,"age"]$age,na.rm=T),
                          dat_spec$age)
dat_spec$age <- ifelse(is.na(dat_spec$age), median(dat_spec[dat_spec$age>0,"age"]$age,na.rm=T),
                       dat_spec$age)

dat_spec$perc_medicare_rx<- ifelse(dat_spec$perc_medicare_rx< 0, 0, dat_spec$perc_medicare_rx)
dat_spec$perc_medicare_rx<- ifelse(is.na(dat_spec$perc_medicare_rx), median(dat_spec$perc_medicare_rx,na.rm=T), dat_spec$perc_medicare_rx)

dat_spec$perc_medicaid_rx<- ifelse(dat_spec$perc_medicaid_rx< 0, 0, dat_spec$perc_medicaid_rx)
dat_spec$perc_medicaid_rx<- ifelse(is.na(dat_spec$perc_medicaid_rx), median(dat_spec$perc_medicaid_rx,na.rm=T), dat_spec$perc_medicaid_rx)

dat_spec$gender <- ifelse(as.character(dat_spec$gender_max)=="","U",as.character(dat_spec$gender_max))
dat_spec$gender <- as.factor(dat_spec$gender)
dat_spec$gender_max <- dat_spec$gender
dat_spec$site_type <- ifelse(as.character(dat_spec$site_type_max)=="","Unknown",
                             as.character(dat_spec$site_type_max))
dat_spec$site_type <- as.factor(dat_spec$site_type)
dat_spec$site_type_max <- dat_spec$site_type

clust_dat <- dat_spec[,clust_att]

saveRDS(dat_spec,"dat_spec.rds")
saveRDS(missing_dat_spec,"missing_dat_spec.rds")
saveRDS(mrkt_rx_spec,"mrkt_rx_spec.rds")
rm(dat_spec,missing_dat_spec,mrkt_rx_spec,mrkt_rx_cust,missing_cust)
gc()

# get clusters
daisy.mat <- as.matrix(daisy(clust_dat, metric="gower"))
clusters <- pam(x=daisy.mat,k=4,diss=T)
dat_spec <- readRDS("dat_spec.rds")
dat_spec$cluster_id <- paste(spec,clusters$clustering,sep="")

# get summaries for cohorts
mrkt_rx_coh_sum = dat_spec %>%
  group_by_(.dots=c("cluster_id","gender_max","site_type_max")) %>%
  summarise(avg_perc_medicare_rx=mean(perc_medicare_rx,na.rm=T),
            avg_perc_medicaid_rx=mean(perc_medicaid_rx,na.rm=T),
            avg_age=mean(age,na.rm=T),
            avg_tenure=mean(tenure, na.rm=T),
            avg_mdcl_grp_size=mean(mdcl_grp_size, na.rm=T),
            avg_pract_typ_size=mean(pract_typ_size,na.rm=T),
            avg_pract_typ_spec=mean(pract_typ_spec_cnt,na.rm=T),
            avg_pract_typ_site=mean(pract_typ_site_cnt, na.rm=T),
            avg_overall_details=mean(overall_details_sum, na.rm=T),
            avg_brand_trx_dol_agg = mean(brand_trx_dol_agg,na.rm=T),
            avg_yr_yr_mkrt=mean(yr_yr_mkrt,na.rm=T),
            avg_emails_dlvrd=mean(emails_dlvd_sum, na.rm=T),
            avg_hqem_open=mean(hqem_open_sum, na.rm=T),
            avg_rte_dlvrd=mean(rte_dlvd_sum, na.rm=T),
            avg_overall_web=mean(overall_website_sum, na.rm=T),
            count_hcp = n()
  )

write.csv(mrkt_rx_coh_sum,paste("D:\\Soumyajit\\Output\\Cohort_Summary_",spec,".csv",sep="")) #output for step3

# append missing and non missing master data set (cust-brand level)
setwd("D:\\Processed Data\\Headroom\\")
mrkt_rx_dat2 <- readRDS("mrkt_rx_dat_cust.rds")
missing_dat3 <- readRDS("missing_dat_cust.rds")
miss_cust_brnd <- missing_dat3[missing_dat3$spec_grp==spec,]
cust_brnd <- mrkt_rx_dat2[mrkt_rx_dat2$spec_grp==spec,]
cust_brnd_agg <- rbind(cust_brnd,miss_cust_brnd)

# get cohorts/cluster_id to customer id - brand level data
coh_cust_brnd <- merge(cust_brnd_agg,dat_spec[,c("pfz_cust_id","cluster_id")],
                       by=c("pfz_cust_id"),all.x=T)

# trim mrk trx
coh_cust_brnd$mrkt_trx_dol1 <- trimws(coh_cust_brnd$mrkt_trx_dol)

# aggregate brand trx
spec_cust_brnd_agg = coh_cust_brnd %>%
  group_by_(.dots=c("pfz_cust_id","mrkt_trx_dol1","cluster_id")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol,na.rm=T),yr_yr_mkrt=max(yr_mrkt_trx_dol,na.rm=T))
summary(spec_cust_brnd_agg)

# convert mrkt trx to numeric
spec_cust_brnd_agg$mrkt_trx_dol2 <- as.numeric(spec_cust_brnd_agg$mrkt_trx_dol1)

# Calculate market share
spec_cust_brnd_agg = spec_cust_brnd_agg %>%
  mutate(mrkt_share = brand_trx_dol_agg/mrkt_trx_dol2)
summary(spec_cust_brnd_agg$mrkt_share)

# get the market shares back to disaggregated data
spec_cust_brnd <- merge(coh_cust_brnd, spec_cust_brnd_agg, by=c("pfz_cust_id","mrkt_trx_dol1","cluster_id"))

# summarise avg mrkt share by brand and cohort
spec_coh_brnd_sum = spec_cust_brnd %>%
  group_by_(.dots=c("cluster_id","brand")) %>%
  summarise(avg_mrkt_share = mean(mrkt_share,na.rm=T))

# Mrkt share, adjust the NaN to 0
spec_coh_brnd_sum$avg_mrkt_share = ifelse(is.na(spec_coh_brnd_sum$avg_mrkt_share),0,spec_coh_brnd_sum$avg_mrkt_share)
write.csv(spec_coh_brnd_sum,paste("D:\\Soumyajit\\Output\\Cohort_Brand_Summary_",spec,".csv",sep="")) #output for step6


# merge back avergare market share to customer level data
mrkt_rx_spec_avg <- merge(coh_cust_brnd, spec_coh_brnd_sum, by=c("cluster_id","brand"), all.x=T)
mrkt_rx_spec_avg <- mrkt_rx_spec_avg[,!colnames(mrkt_rx_spec_avg)=="mrkt_trx_dol2"]

# get market share
mrkt_rx_spec_avg <- merge(mrkt_rx_spec_avg, spec_cust_brnd_agg[,c("pfz_cust_id","mrkt_trx_dol1",
                                                                  "mrkt_share")], 
                          by=c("pfz_cust_id","mrkt_trx_dol1"),all.x=T)
mrkt_rx_spec_avg$mrkt_share = ifelse(is.na(mrkt_rx_spec_avg$mrkt_share),0,mrkt_rx_spec_avg$mrkt_share)

# # calculate headroom
# mrkt_rx_spec_avg$potential <- ifelse(mrkt_rx_spec_avg$mrkt_share>=mrkt_rx_spec_avg$avg_mrkt_share,0,
#                                     mrkt_rx_spec_avg$avg_mrkt_share - mrkt_rx_spec_avg$mrkt_share)

summary(mrkt_rx_spec_avg)
write.csv(mrkt_rx_spec_avg,paste("D:\\Soumyajit\\Output\\Cohort_Brand_AvgMktShare_",spec,".csv",sep="")) #output for step7
cust_16 <- readRDS("cust_16.rds")
write.csv(cust_16[cust_16$spec_grp_max==spec,c("pfz_cust_id","tenure","gender_max","emails_dlvd_sum", "hqem_open_sum",
                                                 "rte_dlvd_sum", "overall_website_sum","mdcl_grp_size","overall_details_sum",
                                                 "pract_typ_size","pract_typ_spec_cnt","pract_typ_site_cnt","pract_typ_avg_size",
                                                 "city_max","site_type_max","site_city_max","mdcl_grp_full_nm_max","grad_yr_max",
                                               "age_max","site_address_max","site_type_max")],
          paste("D:\\Soumyajit\\Output\\Customer_16_",spec,".csv",sep=""))
rm(list=ls())
gc()

#####################################################################################################

