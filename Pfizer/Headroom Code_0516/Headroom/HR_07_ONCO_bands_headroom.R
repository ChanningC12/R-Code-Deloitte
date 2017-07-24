rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(ggplot2)
  
  #read data
  vaccine_raw <- read.csv("D:\\sunil\\headroom\\data\\onco_brnd_pull_v2.csv")
  setwd("D:\\Processed Data\\Headroom\\")
  
  vaccine <- vaccine_raw
  vaccine$mrkt_trx[vaccine_raw$brand=="IBRANCE"] <- vaccine_raw$mrkt_trx[vaccine_raw$brand=="IBRANCE"] + vaccine_raw$brand_trx[vaccine_raw$brand=="IBRANCE"]
  vaccine$mrkt_trx_dol[vaccine_raw$brand=="IBRANCE"] <- vaccine_raw$mrkt_trx_dol[vaccine_raw$brand=="IBRANCE"] + vaccine_raw$brand_trx_dol[vaccine_raw$brand=="IBRANCE"]
  
  # read processed 360 id data
  cust_16 <- readRDS("cust_16.rds") 
  
  # filter on year and merge to 360 data to get specialty
  vaccine1 <- vaccine[vaccine$year==2016,]
  vaccine1 <- unique(vaccine1)
  vaccine1 <- merge(vaccine1,cust_16[,c("pfz_cust_id","spec_grp_max")],by="pfz_cust_id",
                    all.x=T)
  
  # # aggregate trx and dol values for the year 
  # vaccine = vaccine1 %>%
  #   group_by_(.dots=c("pfz_cust_id","spec_grp_max")) %>%
  #   summarise(brand_trx = sum(brand_trx,na.rm=T),
  #             mrkt_trx = sum(mrkt_trx,na.rm=T),
  #             brand_trx_dol = sum(brand_trx_dol,na.rm=T),
  #             mrkt_trx_dol = sum(mrkt_trx_dol,na.rm=T)
  #   )
   
  # read aggregated headroom data
  hcplist <- readRDS("hcplist.rds")
  
  # merge with aggregated headroom data to get cohort id
  brand_dat <-vaccine1
  brand_dat <- merge(brand_dat, unique(hcplist[,c("pfz_cust_id","cluster_id")]), 
                     by="pfz_cust_id", all.x=T)
  sum(is.na(brand_dat$cluster_id))  
  brand_dat <- brand_dat[is.na(brand_dat$cluster_id)==F,]
  # brand_dat <- spec_dat[spec_dat$brand==brand,names(spec_dat) %in% c("potential")==F]

  # calculate mrkt share for each cohort
  coh_sum = brand_dat %>%
    group_by_(.dots=c("cluster_id","brand")) %>%
    summarise(coh_brand_trx = mean(brand_trx,na.rm=T),
              coh_mrkt_trx = mean(mrkt_trx,na.rm=T)
    )
  coh_sum$brand_mrkt_share = coh_sum$coh_brand_trx/coh_sum$coh_mrkt_trx            
  
  # calculate market share for each HCP on basis of Trx
  brand_dat$mrkt_share_trx = brand_dat$brand_trx/brand_dat$mrkt_trx           
  brand_dat <- merge(brand_dat,coh_sum,by=intersect(names(brand_dat),names(coh_sum)),all.x=T)
  
  # calculate headroom and adjusted headroom
  brand_dat$headroom <- (brand_dat$brand_mrkt_share * brand_dat$mrkt_trx) - brand_dat$brand_trx
  brand_dat$headroom_adj <- ifelse(brand_dat$headroom<0,0,brand_dat$headroom)
  brand_dat$headroom_adj <- ifelse(brand_dat$headroom_adj > brand_dat$mrkt_trx,brand_dat$mrkt_trx,
                                   brand_dat$headroom_adj)
  
  brand_dat <- brand_dat[order(brand_dat$headroom),]
  brand_dat$spec_grp <- brand_dat$spec_grp_max
  
  hcplist1 <- hcplist[,c("cluster_id","pfz_cust_id","brand","mrkt_trx",
                         "mrkt_trx_dol","brand_trx","brand_trx_dol","spec_grp",
                         "mrkt_share_trx" ,"coh_brand_trx","coh_mrkt_trx",
                         "brand_mrkt_share","headroom","headroom_adj" )]
  brand_dat <- brand_dat[,names(hcplist1)]
  
  # top5 <-head(brand_dat,5L)
  # bot5 <- tail(brand_dat,5L)
  setwd("D:\\Soumyajit\\Output\\Brand_Reports\\Spec_Headroom")
  hcplist_prevnar <- readRDS("hcplist_prevnar_trumenba.rds")
  
  
  setwd("D:/sunil/headroom/outputs/onco")
  # write.csv(rbind(top5,bot5),paste("HCP_performance_Prevnar.csv",sep=""),row.names = F)
  write.csv(brand_dat,paste("Headroom_ONCO_Brands_v0517.csv",sep=""),row.names = F)
  
  onco_brand <- unique(brand_dat$brand)
  
  hcplist1_wo_onco = subset(hcplist_prevnar, !hcplist_prevnar$brand %in% onco_brand)
  hcplist2 <- rbind(hcplist1_wo_onco,brand_dat)
  hcplist3 <- hcplist2[is.na(hcplist2$mrkt_share_trx)==F,]
  hcplist3 <- hcplist3[is.na(hcplist3$brand_mrkt_share)==F,]
  
  # save final headroom data
  write.csv(hcplist3,"hcplist_updated_onco_0517.csv",row.names = F)  
  saveRDS(hcplist3,"hcplist_updated_onco_0517.rds")
  