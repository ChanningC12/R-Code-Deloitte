rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)

des_sob <- read.csv("descovy_sob_0626.csv")
des_sob$switch_to_rx_adj <- (des_sob$switch_to+des_sob$restart_diff)*des_sob$percentage
summary(des_sob)

# naive rx for Descovy
des_naive <- des_sob[,c("BRAND","END_DATE","new_therapy")]
des_naive <- distinct(des_naive)
names(des_naive) <- c("BRAND","END_DATE","Rx")

# nbrx adj for all other brands
des_switch <- des_sob[,c("BRAND_PROD_GRP_SF","END_DATE","switch_to_rx_adj")]
names(des_switch) <- c("BRAND","END_DATE","Rx")

des_sob_final <- rbind(des_naive,des_switch)
des_sob_final$week <- as.Date(des_sob_final$END_DATE,"%d%b%Y")
des_sob_final <- des_sob_final %>% select(week,BRAND,Rx) %>% arrange(week,BRAND)

library(reshape)
des_sob_final_reshape <- cast(des_sob_final,week~BRAND)
des_sob_final_reshape$V2 <- NULL
des_sob_final_reshape[is.na(des_sob_final_reshape)] <- 0
fwrite(des_sob_final_reshape,"../../Summary/descovy_sob_0627.csv",row.names = F)


############ High writers naive trend ################
naive_H <- read.csv("H_writer_naive.csv")
str(naive_H)
naive_H$week <- as.Date(naive_H$END_DATE,"%d%b%Y")
naive_H <- naive_H[,c("CID","week","new_therapy")]
naive_H_reshape <- cast(naive_H,CID~week)
naive_H_reshape[is.na(naive_H_reshape)] <- 0
names(naive_H_reshape)
rowSums(naive_H_reshape[,(ncol(naive_H_reshape)-11):ncol(naive_H_reshape)])
naive_H_reshape_ttl <- data.frame(naive_H_reshape,
                              ttl_6_3_mnth = rowSums(naive_H_reshape[,(ncol(naive_H_reshape)-23):(ncol(naive_H_reshape)-12)]),
                              ttl_3_mnth = rowSums(naive_H_reshape[,(ncol(naive_H_reshape)-11):ncol(naive_H_reshape)]))
naive_H_reshape_ttl$growth <- (naive_H_reshape_ttl$ttl_3_mnth - naive_H_reshape_ttl$ttl_6_3_mnth)/naive_H_reshape_ttl$ttl_6_3_mnth
naive_H_reshape_ttl$growth_cat <- ifelse(naive_H_reshape_ttl$growth<=0 | is.nan(naive_H_reshape_ttl$growth),
                                         "Negative","Positive")
table(naive_H_reshape_ttl$growth_cat)
fwrite(naive_H_reshape_ttl[,c("CID","ttl_6_3_mnth","ttl_3_mnth","growth_cat")],
       "H_writer_naive_Descovy.csv",row.names = F)

# merge growth category to the naive_H
naive_H <- read.csv("H_writer_naive.csv")
naive_H$week <- as.Date(naive_H$END_DATE,"%d%b%Y")

naive_H_summary <- merge(naive_H_reshape_ttl[,c("CID","growth_cat")],
                         naive_H[,c("CID","week","new_therapy")],
                         by="CID",all.x=T)
table(naive_H_summary$growth_cat)

naive_H_summary <- naive_H_summary %>% group_by_(.dots = c("week","growth_cat")) %>%
  dplyr::summarize(
    new_therapy_avg = mean(new_therapy,na.rm=T)
  )
naive_H_summary_reshape <- cast(naive_H_summary,week~growth_cat)
fwrite(naive_H_summary_reshape,"../../Summary/naive_growth_category.csv",row.names = F)


################### naive data for Triumeq ######################
naive_H_triumeq <- read.csv("H_writer_naive_triumeq.csv")


