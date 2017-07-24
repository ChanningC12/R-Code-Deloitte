rm(list=ls())
gc()
library(dplyr)

########################################################################################
#append reports
########################################################################################
filenames <- list.files("D:\\Soumyajit\\Output\\Brand Reports\\Spec_Headroom",pattern="*.csv")
hcplist <- data.frame()
setwd("D:\\Soumyajit\\Output\\Brand Reports\\Spec_Headroom")

for( i in (1:31)){
  temp = read.csv(filenames[i]);
          temp = temp[is.na(temp$cluster_id)==F,!names(temp) %in% c('X')]
          temp=temp[,names(hcplist2)]
 hcplist2 = rbind(temp,hcplist2)
}

saveRDS(hcplist,"hcplist.rds")
write.csv(hcplist,"hcplist_spec_agg.csv",row.names = F)

# hcplist <- readRDS("hcplist.rds")
hcplist <- unique(hcplist)
