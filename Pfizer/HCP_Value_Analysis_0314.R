rm(list=ls())
gc()
getwd()
setwd("../Desktop/")

library(data.table)
library(dplyr)
library(ggplot2)

# Read in mkrt_rx data
system.time(mkrt_rx_sample <- read.csv("deloitte_mrkt_rx_sample.csv")) # 551.55s
str(mkrt_rx_sample)
colSums(is.na(mkrt_rx_sample))

# Random sample 10000 observations for testing purpose
mkrt_rx_sub = mkrt_rx_sample[sample(1:nrow(mkrt_rx_sample),100000, replace = F),]
write.csv(mkrt_rx_sub,"mkrt_rx_sub.csv",row.names = F)

# check if pfz_cust_id corresponds to unique age, spec_group, % medicare and % medicaid
nrow(unique(mkrt_rx_sub[c("pfz_cust_id")]))
nrow(unique(mkrt_rx_sub[c("pfz_cust_id","age")]))
nrow(unique(mkrt_rx_sub[c("pfz_cust_id","spec_grp")]))
nrow(unique(mkrt_rx_sub[c("pfz_cust_id","perc_medicare_rx")]))
nrow(unique(mkrt_rx_sub[c("pfz_cust_id","perc_medicaid_rx")]))
nrow(unique(mkrt_rx_sub[c("pfz_cust_id","perc_medicare_rx","perc_medicaid_rx","age","spec_grp")]))

# Group by HCP and mkrt - the dataset is on HCP & mrkt level
mkrt_rx_sub_agg = mkrt_rx_sub %>%
  group_by_(.dots=c("pfz_cust_id","mrkt_trx_dol","perc_medicare_rx","perc_medicaid_rx","age","spec_grp")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol))

# Calculate market share
mkrt_rx_sub_agg = mkrt_rx_sub_agg %>%
  mutate(mrkt_share = brand_trx_dol_agg/mrkt_trx_dol)

summary(mkrt_rx_sub_agg$mrkt_share)

########### Frequency Chart by Age, Specialty group and Payer Mix #############
# Frequency charts
hist(mkrt_rx_sub_agg$age) # lot of 0s in age
hist(mkrt_rx_sub_agg$perc_medicare_rx)
hist(mkrt_rx_sub_agg$perc_medicaid_rx)
ggplot(data.frame(mkrt_rx_sub_agg),aes(spec_grp)) + geom_bar()

############ Total Brand Rx and Mrkt Rx by Age, Specialty group and Payer Mix ###############
# By age
# brand TRx and Mrkt TRx
mkrt_rx_sub_age = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("age")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol_agg),
            mrkt_trx_dol_agg = sum(mrkt_trx_dol))
qplot(x=age,y=brand_trx_dol_agg,data=mkrt_rx_sub_age,geom="line")
qplot(x=age,y=mrkt_trx_dol_agg,data=mkrt_rx_sub_age,geom="line")

# By perc_medicare
length(unique(mkrt_rx_sub_agg$perc_medicare_rx))
summary(mkrt_rx_sub_agg$perc_medicare_rx)
# Add percent cut
mkrt_rx_sub_agg$medicare_group = cut(mkrt_rx_sub_agg$perc_medicare_rx,seq(0,1,by=0.05))

mkrt_rx_sub_medicare = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("medicare_group")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol_agg),
            mrkt_trx_dol_agg = sum(mrkt_trx_dol))
ggplot(mkrt_rx_sub_medicare, aes(x = medicare_group, y = brand_trx_dol_agg)) + geom_bar(stat = "identity")
ggplot(mkrt_rx_sub_medicare, aes(x = medicare_group, y = mrkt_trx_dol_agg)) + geom_bar(stat = "identity")

# By perc_medicaid
length(unique(mkrt_rx_sub_agg$perc_medicaid_rx))
summary(mkrt_rx_sub_agg$perc_medicaid_rx)
# Add percent cut
mkrt_rx_sub_agg$medicaid_group = cut(mkrt_rx_sub_agg$perc_medicaid_rx,seq(0,1,by=0.05))

mkrt_rx_sub_medicaid = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("medicaid_group")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol_agg),
            mrkt_trx_dol_agg = sum(mrkt_trx_dol))
ggplot(mkrt_rx_sub_medicaid, aes(x = medicaid_group, y = brand_trx_dol_agg)) + geom_bar(stat = "identity")
ggplot(mkrt_rx_sub_medicaid, aes(x = medicaid_group, y = mrkt_trx_dol_agg)) + geom_bar(stat = "identity")

# By specialty group
mkrt_rx_sub_spec = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("spec_grp")) %>%
  summarise(brand_trx_dol_agg = sum(brand_trx_dol_agg),
            mrkt_trx_dol_agg = sum(mrkt_trx_dol))
ggplot(mkrt_rx_sub_spec, aes(x = spec_grp, y = brand_trx_dol_agg)) + geom_bar(stat = "identity")
ggplot(mkrt_rx_sub_spec, aes(x = spec_grp, y = mrkt_trx_dol_agg)) + geom_bar(stat = "identity")

############ Average Brand Rx and Mrkt Rx by Age, Specialty group and Payer Mix ###############
# By age
# brand TRx and Mrkt TRx
mkrt_rx_sub_age_avg = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("age")) %>%
  summarise(brand_trx_dol_agg = mean(brand_trx_dol_agg,na.rm=T),
            mrkt_trx_dol_agg = mean(mrkt_trx_dol,na.rm=T))
qplot(x=age,y=brand_trx_dol_agg,data=mkrt_rx_sub_age_avg,geom="line")
qplot(x=age,y=mrkt_trx_dol_agg,data=mkrt_rx_sub_age_avg,geom="line")

# By perc_medicare
mkrt_rx_sub_medicare_avg = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("medicare_group")) %>%
  summarise(brand_trx_dol_agg = mean(brand_trx_dol_agg,na.rm=T),
            mrkt_trx_dol_agg = mean(mrkt_trx_dol,na.rm=T))
ggplot(mkrt_rx_sub_medicare_avg, aes(x = medicare_group, y = brand_trx_dol_agg)) + geom_bar(stat = "identity")
ggplot(mkrt_rx_sub_medicare_avg, aes(x = medicare_group, y = mrkt_trx_dol_agg)) + geom_bar(stat = "identity")

# By perc_medicaid
mkrt_rx_sub_medicaid_avg = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("medicaid_group")) %>%
  summarise(brand_trx_dol_agg = mean(brand_trx_dol_agg,na.rm=T),
            mrkt_trx_dol_agg = mean(mrkt_trx_dol,na.rm=T))
ggplot(mkrt_rx_sub_medicaid_avg, aes(x = medicaid_group, y = brand_trx_dol_agg)) + geom_bar(stat = "identity")
ggplot(mkrt_rx_sub_medicaid_avg, aes(x = medicaid_group, y = mrkt_trx_dol_agg)) + geom_bar(stat = "identity")

# By specialty group
mkrt_rx_sub_spec_avg = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("spec_grp")) %>%
  summarise(brand_trx_dol_agg = mean(brand_trx_dol_agg,na.rm=T),
            mrkt_trx_dol_agg = mean(mrkt_trx_dol,na.rm=T))
ggplot(mkrt_rx_sub_spec_avg, aes(x = spec_grp, y = brand_trx_dol_agg)) + geom_bar(stat = "identity")
ggplot(mkrt_rx_sub_spec_avg, aes(x = spec_grp, y = mrkt_trx_dol_agg)) + geom_bar(stat = "identity")



############### Aggregated Table by Specialty group, Age, Medicare and Medicaid ################
# Mrkt share, adjust the NaN to 0
mkrt_rx_sub_agg$mrkt_share_2 = ifelse(mkrt_rx_sub_agg$mrkt_trx_dol==0,0,mkrt_rx_sub_agg$mrkt_share)

# Age bins: <20, 20-35, 35-50, 50-65, 65-80, 80-95, >=95
mkrt_rx_sub_agg$age_bin = cut(mkrt_rx_sub_agg$age,c(-Inf,seq(20,95,by=15),Inf))

# Medicare and medicaid bins, by 10%
mkrt_rx_sub_agg$medicare_bin = cut(mkrt_rx_sub_agg$perc_medicare_rx,seq(0,1,by=0.1))
mkrt_rx_sub_agg$medicaid_bin = cut(mkrt_rx_sub_agg$perc_medicaid_rx,seq(0,1,by=0.1))

mkrt_rx_sub_agg_bin = mkrt_rx_sub_agg %>%
  group_by_(.dots=c("spec_grp","medicare_bin","medicaid_bin","age_bin")) %>%
  summarise(count = n(),
            brand_trx_dol_tot = sum(brand_trx_dol_agg),
            mrkt_trx_dol_tot = sum(mrkt_trx_dol),
            mrkt_share_avg = mean(mrkt_share_2,na.rm=T),
            cust_dist_count = length(unique(pfz_cust_id)))

write.csv(mkrt_rx_sub_agg_bin,"mkrt_rx_agg_table.csv",row.names = F)











