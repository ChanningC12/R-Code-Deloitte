rm(list=ls())
gc()
library(dplyr)
########################################################################################################
spec_headroom <- data.frame()
 for (spec in c("ALLG",
                "ANE",
                "CARD",
                "CHP",
                "DDS",
                "DERM",
                "DIAB",
                "ELCT",
                "ENDO",
                "IND",
                "NEPH",
                "NEUR",
                "NP/PA",
                "NSU",
                "OBG",
                "ONCO",
                "OPTH",
                "ORTH",
                "OTH",
                "PAIN",
                "PCP",
                "PEDS",
                "PODS",
                "PSY",
                "PULM",
                "RHU",
                "URO",
                "VMD",
                "OPT",
                "OPD"))
 {
  spec_headroom <- data.frame()
  
  setwd("D:\\Soumyajit\\Output")
  spec_dat <- read.csv(paste("Cohort_Brand_AvgMktShare_",spec,".csv",sep=""))
  spec_dat$brand1=gsub("\\.|/|\\-|\"|\\s","_",spec_dat$brand)
  
  for (brand1 in unique(spec_dat$brand1)){
  
  brand_dat <- spec_dat[spec_dat$brand1==brand1,names(spec_dat) %in% c("potential")==F]

  # calculate mrkt share for each cohort
  coh_sum = brand_dat %>%
  group_by_(.dots=c("cluster_id")) %>%
  summarise(coh_brand_trx = mean(brand_trx,na.rm=T),
            coh_mrkt_trx = mean(mrkt_trx,na.rm=T)
  )
  coh_sum$brand_mrkt_share = coh_sum$coh_brand_trx/coh_sum$coh_mrkt_trx            
  
  # calculate market share for each HCP on basis of Trx
  brand_dat$mrkt_share_trx = brand_dat$brand_trx/brand_dat$mrkt_trx            
  brand_dat <- merge(brand_dat,coh_sum,by="cluster_id",all.x=T)
  
  # calculate headroom
  brand_dat$headroom <- (brand_dat$brand_mrkt_share * brand_dat$mrkt_trx) - brand_dat$brand_trx
  brand_dat$headroom_adj <- ifelse(brand_dat$headroom<0,0,brand_dat$headroom)
  brand_dat$headroom_adj <- ifelse(brand_dat$headroom_adj > brand_dat$mrkt_trx,brand_dat$mrkt_trx,
                                   brand_dat$headroom_adj)
  
  brand_dat <- brand_dat[order(brand_dat$headroom),]
  
  top5 <-head(brand_dat,5L)
  bot5 <- tail(brand_dat,5L)
  
  # setwd("D:\\Soumyajit\\Output\\Brand Reports")
  # write.csv(rbind(top5,bot5),paste("HCP_performance_",spec,"_",brand1,".csv",sep=""),row.names = F)
  # write.csv(brand_dat,paste("HCP_list_",spec,"_",brand1,".csv",sep=""),row.names = F)
  spec_headroom <- rbind(brand_dat,spec_headroom)  
  }
    spec_headroom <- spec_headroom[, c("cluster_id","pfz_cust_id","brand","mrkt_trx","mrkt_trx_dol","yr_mrkt_trx",
                                       "yr_mrkt_trx_dol","brand_trx","brand_trx_dol","spec_grp",
                                       "mrkt_share_trx","coh_brand_trx","coh_mrkt_trx",
                                       "brand_mrkt_share","headroom","headroom_adj")] 
  # write.csv(spec_headroom,paste("D:\\Soumyajit\\Output\\Brand Reports\\Spec_Headroom\\HCP_list_",spec,".csv",sep=""),row.names = F)
  spec_headroom1 <- spec_headroom[!is.na(spec_headroom$brand_mrkt_share),]
  spec_headroom1 <- spec_headroom1[!is.na(spec_headroom1$mrkt_share_trx),]
  write.csv(spec_headroom1,paste("D:\\Soumyajit\\Output\\Brand Reports\\Spec_Headroom\\HCP_list_",spec,"_nonmiss.csv",sep=""),row.names = F)
  }