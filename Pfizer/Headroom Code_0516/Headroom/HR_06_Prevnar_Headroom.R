rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(ggplot2)
  
  #read data
  prevnar <- readRDS("D://raw//prev_us_hcp_brand_year_month.rds")
  setwd("D:\\Processed Data\\Headroom\\")
  
  # read processed 360 id data
  cust_16 <- readRDS("cust_16.rds") 
  
  # filter on year and merge to 360 data to get specialty
  prevnar1 <- prevnar[prevnar$year==2016,]
  prevnar1 <- unique(prevnar1)
  prevnar1 <- merge(prevnar1,cust_16[,c("pfz_cust_id","spec_grp_max")],by="pfz_cust_id",all.x=T)
  
  # aggregate trx and dol values for the year 
  prevnar_yr = prevnar1 %>%
    group_by_(.dots=c("pfz_cust_id","spec_grp_max")) %>%
    summarise(brand_trx_yr = sum(brand_trx,na.rm=T),
              mrkt_trx_yr = sum(mrkt_trx,na.rm=T),
              brand_trx_dol_yr = sum(brand_trx_dol,na.rm=T),
              mrkt_trx_dol_yr = sum(mrkt_trx_dol,na.rm=T)
    )
  
  # read aggregated headroom data
  hcplist <- readRDS("hcplist.rds")
  
  # merge with aggregated headroom data to get cohort id
  brand_dat <- prevnar_yr
  brand_dat <- merge(brand_dat, unique(hcplist[,c("pfz_cust_id","cluster_id")]), 
                     by="pfz_cust_id", all.x=T)
  sum(is.na(brand_dat$cluster_id))  
  brand_dat <- brand_dat[is.na(brand_dat$cluster_id)==F,]
  # brand_dat <- spec_dat[spec_dat$brand==brand,names(spec_dat) %in% c("potential")==F]

  # calculate mrkt share for each cohort
  coh_sum = brand_dat %>%
    group_by_(.dots=c("cluster_id")) %>%
    summarise(coh_brand_trx = mean(brand_trx_yr,na.rm=T),
              coh_mrkt_trx = mean(mrkt_trx_yr,na.rm=T)
    )
  coh_sum$brand_mrkt_share = coh_sum$coh_brand_trx/coh_sum$coh_mrkt_trx            
  
  # calculate market share for each HCP on basis of Trx
  brand_dat$mrkt_share_trx = brand_dat$brand_trx_yr/brand_dat$mrkt_trx_yr           
  brand_dat <- merge(brand_dat,coh_sum,by="cluster_id",all.x=T)
  
  # calculate headroom and adjusted headroom
  brand_dat$headroom <- (brand_dat$brand_mrkt_share * brand_dat$mrkt_trx_yr) - brand_dat$brand_trx_yr
  brand_dat$headroom_adj <- ifelse(brand_dat$headroom<0,0,brand_dat$headroom)
  brand_dat$headroom_adj <- ifelse(brand_dat$headroom_adj > brand_dat$mrkt_trx_yr,brand_dat$mrkt_trx_yr,
                                   brand_dat$headroom_adj)
  
  brand_dat <- brand_dat[order(brand_dat$headroom),]
  brand_dat$spec_grp <- brand_dat$spec_grp_max
  brand_dat$brand_trx <- brand_dat$brand_trx_yr
  brand_dat$mrkt_trx <- brand_dat$mrkt_trx_yr
  brand_dat$brand_trx_dol <- brand_dat$brand_trx_dol_yr
  brand_dat$mrkt_trx_dol <- brand_dat$mrkt_trx_dol_yr
  brand_dat$brand <- "PREVNAR"
  brand_dat$year <- 2016
  
  hcplist1 <- hcplist[,c("cluster_id","pfz_cust_id","brand","mrkt_trx",
                         "mrkt_trx_dol","brand_trx","brand_trx_dol","spec_grp",
                         "mrkt_share_trx" ,"coh_brand_trx","coh_mrkt_trx",
                         "brand_mrkt_share","headroom","headroom_adj" )]
  brand_dat <- brand_dat[,names(hcplist1)]
  
  # top5 <-head(brand_dat,5L)
  # bot5 <- tail(brand_dat,5L)
  
  setwd("D:\\Soumyajit\\Output\\Brand Reports\\Spec_Headroom")
  # write.csv(rbind(top5,bot5),paste("HCP_performance_Prevnar.csv",sep=""),row.names = F)
  write.csv(brand_dat,paste("HCP_list_Prevnar.csv",sep=""),row.names = F)
  
  hcplist2 <- rbind(hcplist1,brand_dat)
  hcplist3 <- hcplist2[is.na(hcplist2$mrkt_share_trx)==F,]
  hcplist3 <- hcplist3[is.na(hcplist3$brand_mrkt_share)==F,]
  
  # save final headroom data
  write.csv(hcplist2,"hcplist_prevnar_0414_v1.csv",row.names = F)  
  saveRDS(hcplist2,"hcplist_prevnar_0414_v1.rds")
  