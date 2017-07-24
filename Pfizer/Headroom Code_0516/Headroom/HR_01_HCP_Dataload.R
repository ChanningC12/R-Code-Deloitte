rm(list=ls())
gc()
setwd("D:\\raw")
library(data.table)
library(dplyr)
library(ggplot2)

# Read in mkrt_rx data
mkrt_rx1 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.001")
mkrt_rx2 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.002",header=F,col.names=names(mkrt_rx1))
mkrt_rx3 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.003",header=F,col.names=names(mkrt_rx1))
mkrt_rx4 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.004",header=F, col.names=names(mkrt_rx1))
mkrt_rx5 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.005",header=F, col.names=names(mkrt_rx1))
mkrt_rx6 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.006",header=F, col.names=names(mkrt_rx1))
mkrt_rx7 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.007",header=F, col.names=names(mkrt_rx1))
mkrt_rx8 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.008",header=F, col.names=names(mkrt_rx1))
mkrt_rx9 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.009",header=F, col.names=names(mkrt_rx1))
mkrt_rx10 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.010",header=F, col.names=names(mkrt_rx1))
mkrt_rx11 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.011",header=F, col.names=names(mkrt_rx1))
mkrt_rx12 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.012",header=F, col.names=names(mkrt_rx1))
mkrt_rx13 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.013",header=F, col.names=names(mkrt_rx1))
mkrt_rx14 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.014",header=F, col.names=names(mkrt_rx1))
mkrt_rx15 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.015",header=F, col.names=names(mkrt_rx1))
mkrt_rx16 <- read.csv("D:\\raw\\deloitte_mrkt_rx_sample.csv.016",header=F, col.names=names(mkrt_rx1))

# assign dataset id
mkrt_rx1$dat_id <- 1
mkrt_rx2$dat_id <- 2 
mkrt_rx3$dat_id <- 3
mkrt_rx4$dat_id <- 4
mkrt_rx5$dat_id <- 5
mkrt_rx6$dat_id <- 6 
mkrt_rx7$dat_id <- 7 
mkrt_rx8$dat_id <- 8 
mkrt_rx9$dat_id <- 9 
mkrt_rx10$dat_id <- 10
mkrt_rx11$dat_id <- 11
mkrt_rx12$dat_id <- 12
mkrt_rx13$dat_id <- 13
mkrt_rx14$dat_id <- 14
mkrt_rx15$dat_id <- 15
mkrt_rx16$dat_id <- 16

# store 1st row in missing data as they are not usable
missing_dat <-  rbind(mkrt_rx2[1,],mkrt_rx3[1,],mkrt_rx4[1,],mkrt_rx5[1,] ,mkrt_rx6[1,],mkrt_rx7[1,], 
                      mkrt_rx8[1,],mkrt_rx9[1,],mkrt_rx10[1,],mkrt_rx11[1,],mkrt_rx12[1,],mkrt_rx13[1,],mkrt_rx14[1,], 
                      mkrt_rx15[1,],mkrt_rx16[1,] )

# removing 1st rows as they are not usable
mkrt_rx2 <- mkrt_rx2[-1,] 
mkrt_rx3 <- mkrt_rx3[-1,] 
mkrt_rx4 <- mkrt_rx4[-1,] 
mkrt_rx5 <- mkrt_rx5[-1,] 
mkrt_rx6 <- mkrt_rx6[-1,] 
mkrt_rx7 <- mkrt_rx7[-1,] 
mkrt_rx8 <- mkrt_rx8[-1,] 
mkrt_rx9 <- mkrt_rx9[-1,] 
mkrt_rx10<- mkrt_rx10[-1,] 
mkrt_rx11<- mkrt_rx11[-1,] 
mkrt_rx12<- mkrt_rx12[-1,] 
mkrt_rx13<- mkrt_rx13[-1,] 
mkrt_rx14<- mkrt_rx14[-1,] 
mkrt_rx15<- mkrt_rx15[-1,] 
mkrt_rx16<- mkrt_rx16[-1,] 

# append datasets
system.time(mkrt_rx <- rbind(mkrt_rx1,mkrt_rx2,mkrt_rx3,mkrt_rx4,mkrt_rx5,mkrt_rx6,mkrt_rx7,mkrt_rx8,mkrt_rx9,mkrt_rx10,
                             mkrt_rx11,mkrt_rx12,mkrt_rx13,mkrt_rx14,mkrt_rx15,mkrt_rx16)) #318.39

# convert market rx and brand rx to numeric
mkrt_rx$mrkt_trx_dol <- as.numeric(mkrt_rx$mrkt_trx_dol)
mkrt_rx$yr_mrkt_trx <- as.numeric(mkrt_rx$yr_mrkt_trx)
mkrt_rx$brand_trx_dol <- as.numeric(mkrt_rx$brand_trx_dol)

# Generate QC Report
summary(mkrt_rx)
mkrt_rx$rownum <- as.numeric(row.names(mkrt_rx))

# save non missing data for analysis 
mrkt_rx_dat <- mkrt_rx[rowSums(is.na(mkrt_rx[,c("pfz_cust_id","perc_medicare_rx","perc_medicaid_rx",
                                                "age","spec_grp")]))==0,]
mrkt_rx_dat1 <- mrkt_rx_dat[mrkt_rx_dat$age>0,]
mrkt_rx_dat1 <- mrkt_rx_dat1[mrkt_rx_dat1$mrkt_trx_dol>=0,]
mrkt_rx_dat1 <- mrkt_rx_dat1[mrkt_rx_dat1$brand_trx_dol>=0,]
mrkt_rx_dat1 <- mrkt_rx_dat1[mrkt_rx_dat1$yr_mrkt_trx>=0,]

summary(mrkt_rx_dat1)

# save remaining data in missing dat
missing_dat$rownum <- -1
missing_dat1 <- rbind(missing_dat, mkrt_rx[!(mkrt_rx$rownum %in% mrkt_rx_dat1$rownum),])
summary(missing_dat1)
sum(nrow(missing_dat1),nrow(mrkt_rx_dat1))

# get missing %
miss.medicare <- sum(is.na(missing_dat1$perc_medicare_rx))/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1))
miss.medicaid <- sum(is.na(missing_dat1$perc_medicaid_rx))/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1))
miss.age <- sum(is.na(missing_dat1$age),missing_dat1$age<=0,na.rm=T)/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1))
miss.mrkt_trx_dol <- sum(is.na(missing_dat1$mrkt_trx_dol),missing_dat1$mrkt_trx_dol<0,na.rm=T)/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1))
miss.brand_trx_dol <- sum(is.na(missing_dat1$brand_trx_dol),missing_dat1$brand_trx_dol<0,na.rm=T)/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1))
miss.yr_mrkt_trx <- sum(is.na(missing_dat1$yr_mrkt_trx),missing_dat1$yr_mrkt_trx<0,na.rm=T)/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1))
miss.perc.report <- as.data.frame(cbind(miss.medicaid,miss.medicare,miss.age,
                                        miss.yr_mrkt_trx,miss.brand_trx_dol,miss.mrkt_trx_dol))

# get missing rate of rows
missing_dat2 <- missing_dat1

missing_dat2$miss.medicare <- as.numeric(is.na(missing_dat1$perc_medicare_rx))

missing_dat2$miss.medicaid <- as.numeric(is.na(missing_dat1$perc_medicaid_rx))

missing_dat2$miss.age <- as.numeric(is.na(missing_dat1$age),1,0)
missing_dat2$miss.age <- ifelse(missing_dat1$age<=0,1,missing_dat2$miss.age)

#missing_dat2$miss.mrkt_trx_dol <- as.numeric(is.na(missing_dat1$mrkt_trx_dol),1,0)
#missing_dat2$miss.mrkt_trx_dol <- ifelse(missing_dat1$mrkt_trx_dol<0,1,missing_dat2$miss.mrkt_trx_dol)

missing_dat2$miss.brand_trx_dol <- as.numeric(is.na(missing_dat1$brand_trx_dol),1,0)
missing_dat2$miss.brand_trx_dol <- ifelse(missing_dat1$brand_trx_dol<0,1,missing_dat2$miss.brand_trx_dol)

missing_dat2$miss.yr_mrkt_trx <- as.numeric(is.na(missing_dat1$yr_mrkt_trx),1,0)
missing_dat2$miss.yr_mrkt_trx <- ifelse(missing_dat1$yr_mrkt_trx<0,1,missing_dat2$miss.yr_mrkt_trx)

missing_dat2$missing.id <- as.numeric(is.na(missing_dat1$pfz_cust_id))
missing_dat2$missing.spec <- as.numeric(is.na(missing_dat1$spec_grp))

missing_dat2$miss.attribute <- rowSums(missing_dat2[,c("miss.yr_mrkt_trx","miss.brand_trx_dol","miss.medicare",
                                                       "miss.age","miss.medicare","miss.medicaid")],
                                       na.rm=T)

summary(missing_dat2$miss.attribute)
miss_att = missing_dat2 %>%
  group_by_(.dots=("miss.attribute")) %>%
  summarise(count=n())

miss_att$perc = round(100*miss_att$count/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1)),2)
miss_att <- miss_att[miss_att$miss.attribute>0,]

missing_dat2$miss.id_spec <- rowSums(missing_dat2[,c("missing.id","missing.spec")],
                                       na.rm=T)
miss_id_Spec = missing_dat2 %>%
  group_by_(.dots=("miss.id_spec")) %>%
  summarise(count=n())

miss_id_Spec$perc = round(100*miss_id_Spec$count/sum(nrow(missing_dat1),nrow(mrkt_rx_dat1)),2)
miss_id_Spec <- miss_id_Spec[miss_id_Spec$miss.id_spec >0,]

# save datasets
setwd("D:\\Processed Data\\Headroom")
saveRDS(missing_dat,"missing_dat_1strows.rds") #1st rows
saveRDS(mkrt_rx,"mrkt_rx.rds") #appended data
saveRDS(missing_dat2,"missing_dat.rds") #missing data
saveRDS(mrkt_rx_dat1,"mrkt_rx_dat.rds") #analysis data
saveRDS(miss_att,"missing_attributes.rds") #missing_report
saveRDS(miss.perc.report,"missing_perc_variables.rds") #missing_report
saveRDS(miss_id_Spec,"missing_id_spec.rds") #missing_report

write.csv(miss_att,"missing_attributes.csv")
write.csv(miss_id_Spec,"missing_id_spec.csv")
write.csv(miss.perc.report,"missing_perc_variables.csv")

# seggregate customers with missing data from that with complete data
missing_dat3 <- rbind(missing_dat, mkrt_rx[(mkrt_rx$pfz_cust_id %in% missing_dat1$pfz_cust_id)==T,])
mrkt_rx_dat2 <- mkrt_rx[(mkrt_rx$pfz_cust_id %in% missing_dat1$pfz_cust_id)==F,]

saveRDS(missing_dat3,"missing_dat_cust.rds") # all customers with missing data
saveRDS(mrkt_rx_dat2,"mrkt_rx_dat_cust.rds") # all customers with complete data data


