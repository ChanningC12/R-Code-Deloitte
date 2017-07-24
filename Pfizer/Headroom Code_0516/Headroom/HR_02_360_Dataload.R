rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(ggplot2)

# read header data
setwd("D:\\Headers")
hd_15_16 <- read.csv("201516_360_by_id_header.csv",header=T)

# read data
cust_15_16 <- rbind(hd_15_16, read.csv("D:\\raw\\xfer_March21_2017\\201516_360_by_id.csv",header=F))
names(cust_15_16) <- names(hd_15_16)

# filter on 2016 data
cust_16 <- cust_15_16[cust_15_16$year==2016,]
saveRDS(cust_16,"cust_16.rds")
rm(cust_16)
# read data
cust_dat_16 <- readRDS("cust_16.rds")
cust_16 <- cust_dat_16[!is.na(cust_dat_16$pfz_cust_id),]
rm(cust_dat_16)
cust_16 <- cust_16[cust_16$brand_trx_sum >0,]

# assign gender
# cust_16$gender <- ifelse(cust_16$gender_max %in% c("M","F")==F,"U",cust_16$gender_max)

# calculate tenure, difference btwn tenure and age and impute missing tenure
cust_16$tenure = 2016 - cust_16$grad_yr_max
summary(cust_16$tenure)

cust_16$tenure = ifelse(is.na(cust_16$grad_yr_max), cust_16$age - mean(cust_16$age_max - cust_16$tenure,
                                                                 na.rm=T),cust_16$tenure)
# cust_16$tenure = ifelse(cust_16$tenure > cust_16$age_max, cust_16$age_max, cust_16$tenure)
# summary(cust_16$tenure)
# cust_16$tenure = ifelse(cust_16$tenure <0,0, cust_16$tenure)
# summary(cust_16$tenure)
# cust_16$tenure = ifelse(cust_16$tenure <=0, median(cust_16$tenure), cust_16$tenure)

# calculate medical group size
mdcl_grp_size = cust_16[cust_16$spec_grp_max %in% c("NP/PA")==F,] %>%
  group_by_(.dots=c("mdcl_grp_full_nm_max")) %>%
  summarise(mdcl_grp_size = n()
  )
mdcl_grp_size$mdcl_grp_size <- ifelse(mdcl_grp_size$mdcl_grp_full_nm_max=="",1,mdcl_grp_size$mdcl_grp_size)
cust_16 <- merge(cust_16,mdcl_grp_size,by="mdcl_grp_full_nm_max",all.x=T)
cust_16$mdcl_grp_size <- ifelse(is.na(cust_16$mdcl_grp_size),1,cust_16$mdcl_grp_size)

# # calculate practice type size and specialty count
pract_typ_size = cust_16[!cust_16$spec_grp_max==c("NP/PA") & !cust_16$site_address_max =="",] %>%
  group_by_(.dots=c("site_type_max")) %>%
  summarise(pract_typ_size = n(),
            pract_typ_site_cnt = length(unique(site_address_max)),
            pract_typ_spec_cnt = length(unique(spec_grp_max))
  )
pract_typ_size$pract_typ_avg_size <- round(pract_typ_size$pract_typ_size/pract_typ_size$pract_typ_site_cnt)
cust_16 <- merge(cust_16,pract_typ_size,by="site_type_max",all.x=T)
cust_16$pract_typ_avg_size <- ifelse(is.na(cust_16$pract_typ_avg_size),median(pract_typ_size$pract_typ_avg_size),
                                     cust_16$pract_typ_avg_size)
cust_16$pract_typ_site_cnt <- ifelse(is.na(cust_16$pract_typ_site_cnt),median(pract_typ_size$pract_typ_site_cnt),
                                     cust_16$pract_typ_site_cnt)
cust_16$pract_typ_size <- ifelse(is.na(cust_16$pract_typ_size),median(pract_typ_size$pract_typ_size,na.rm=T),
                                     cust_16$pract_typ_size)
cust_16$pract_typ_spec_cnt <- ifelse(is.na(cust_16$pract_typ_spec_cnt),median(pract_typ_size$pract_typ_spec_cnt),
                                     cust_16$pract_typ_spec_cnt)

# # calculate location size and specialty count
# site_size = cust_16[cust_16$spec_grp_max %in% c("NP/PA") ==F,] %>%
#   group_by_(.dots=c("site_address_max")) %>%
#   summarise(site_size = n(),
#             site_spec_cnt = length(unique(spec_grp_max))
#   )
# site_size$site_size <- ifelse(site_size$site_address_max=="",
#                                       1,
#                                       site_size$site_size)
# site_size$site_spec_cnt <- ifelse(site_size$site_address_max=="",1,
#                                           site_size$site_spec_cnt)
# cust_16 <- merge(cust_16,site_size,by="site_address_max",all.x=T)
# cust_16$site_size <- ifelse(is.na(cust_16$site_size),1,cust_16$site_size)
# cust_16$site_spec_cnt <- ifelse(is.na(cust_16$site_spec_cnt),1,cust_16$site_spec_cnt)

# # get avg specialty count for each practice 
# pract_type_spec <-  cust_16[cust_16$spec_grp_max %in% c("NP/PA") ==F,] %>%
#   group_by_(.dots=c("site_type_max")) %>%
#   summarise(pract_typ_site_avg_spec = mean(site_spec_cnt,na.rm=T)
#   )
# cust_16 <- merge(cust_16,pract_type_spec,by="site_type_max",all.x=T)

setwd("D:\\Processed Data\\Headroom\\")
saveRDS(cust_16,"cust_16.rds")
rm(cust_16)
cust_16 <- readRDS("cust_16.rds")
# select relevant columns
cust_16_dat <- cust_16[,c("pfz_cust_id","tenure","gender_max","emails_dlvd_sum", "hqem_open_sum",
                          "rte_dlvd_sum", "overall_website_sum","mdcl_grp_size","overall_details_sum",
                        "pract_typ_size","pract_typ_spec_cnt","pract_typ_site_cnt","pract_typ_avg_size",
                        "city_max","site_type_max","site_city_max")]

########################################################################################################
# load brand level data for 2016
########################################################################################################

# load analysis data
setwd("D:\\Processed Data\\Headroom")
mrkt_rx_dat2 <- readRDS("mrkt_rx_dat_cust.rds")
summary(mrkt_rx_dat2)

# load missing data
missing_dat3 <- readRDS("missing_dat_cust.rds")

# # dup check
# dup_spec <-rbind(mrkt_rx_dat2[,c("pfz_cust_id","spec_grp")],
#                  missing_dat3[,c("pfz_cust_id","spec_grp")])
# dup_spec$id <- paste(dup_spec$pfz_cust_id,dup_spec$spec_grp,sep="_")
# dup_spec1 <- dup_spec[!duplicated(dup_spec),]
# dup_cust <- dup_spec1[duplicated(dup_spec1$pfz_cust_id),]
# saveRDS(dup_cust,"dup_cust.rds")

# dup_cust <- readRDS("D:\\Soumyajit\\Output\\dup_cust.rds")
# mrkt_rx_dat2 <- mrkt_rx_dat2[mrkt_rx_dat2$pfz_cust_id %in% dup_cust$pfz_cust_id ==F,] # data with customer with unique spec
# missing_dat3 <- missing_dat3[missing_dat3$pfz_cust_id %in% dup_cust$pfz_cust_id ==F,] # data with customer with unique spec
mrkt_rx_dat3 <- mrkt_rx_dat2[mrkt_rx_dat2$pfz_cust_id %in% cust_16_dat$pfz_cust_id ==T,]
missing_dat3 <- missing_dat3[missing_dat3$pfz_cust_id %in% cust_16_dat$pfz_cust_id ==T,]
# rm(dup_cust)

# roll up data to customer id level
mrkt_rx_cust = mrkt_rx_dat2 %>%
  group_by_(.dots=c("pfz_cust_id","spec_grp")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol,na.rm=T),yr_yr_mkrt=max(yr_mrkt_trx_dol,na.rm=T),
            perc_medicare_rx=max(perc_medicare_rx,na.rm=T),perc_medicaid_rx=max(perc_medicaid_rx,na.rm=T),
            age=max(age,na.rm=T))

missing_cust = missing_dat3 %>%
  group_by_(.dots=c("pfz_cust_id","spec_grp")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol,na.rm=T),yr_yr_mkrt=max(yr_mrkt_trx_dol,na.rm=T),
            perc_medicare_rx=max(perc_medicare_rx,na.rm=T),perc_medicaid_rx=max(perc_medicaid_rx,na.rm=T),
            age=max(age,na.rm=T))

# get aditional attributes from cust_16 data
mrkt_rx_cust1 <- inner_join(mrkt_rx_cust,cust_16_dat,by="pfz_cust_id")
missing_cust1 <- inner_join(missing_cust,cust_16_dat,by="pfz_cust_id")

saveRDS(mrkt_rx_cust1,"2016_customer_data.rds")
saveRDS(missing_cust1,"2016_missing_customer_data.rds")
