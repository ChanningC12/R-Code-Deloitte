rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)

patient_des <- read.csv("patient_rx_tiv_des.csv")
patient_des$X_NAME_ <- NULL
str(patient_des)
colSums(is.na(patient_des))
# fill na as 0
patient_des[is.na(patient_des)] <- 0
# sort by patient_id and mo_id
patient_des <- patient_des %>% arrange(PATIENT_ID,MO_ID)
n_distinct(patient_des$PATIENT_ID) # 20,438 patients prescribed Tivicay+Descovy at the same month

# month since Descovy launch
des_launch <- as.data.frame(unique(patient_des$MO_ID))
names(des_launch) <- "MO_ID"
des_launch <- des_launch %>% 
  dplyr::mutate(month_since_launch = rank(MO_ID)-13)

# merge the month since launch
patient_des <- merge(patient_des,des_launch,by="MO_ID",all.x=T)
table(patient_des$month_since_launch)

# Add the indicator to indicate the month a patient prescribe Tiv+Des
patient_des <- patient_des %>% group_by(PATIENT_ID) %>%
  dplyr::mutate(
    month_since_launch_rank = rank(month_since_launch,ties.method = "min")
  )

# calculate the first month a patient prescribed Tiv+Des
patient_adopt_first <- patient_des %>% 
  dplyr::mutate(
    tiv_des_ind = ifelse(DESCOVY>0 & TIVICAY>0,1,0)
  ) %>%
  filter(tiv_des_ind==1) %>%
  group_by(PATIENT_ID) %>%
  dplyr::summarize(
    month_since_launch = min(month_since_launch)
  )

# merge in the rank of the month for the first month adoption
patient_adopt_first <- merge(patient_adopt_first,patient_des[,c("PATIENT_ID","month_since_launch","month_since_launch_rank")],
                             by=c("PATIENT_ID","month_since_launch"),all.x = T)
patient_adopt_first$month_since_launch_rank_last <- patient_adopt_first$month_since_launch_rank-1

# merge back with month_since_launch_rank_last to get the portfolio before the month adopting Tiv+Des
patient_adopt_last <- merge(patient_adopt_first,patient_des[,!(names(patient_des) %in% "month_since_launch")],
                            by.x=c("PATIENT_ID","month_since_launch_rank_last"),
                            by.y=c("PATIENT_ID","month_since_launch_rank"),
                            all.x=T)
View(patient_des[patient_des$PATIENT_ID==277450434,]) # for QC
sum(is.na(patient_adopt_last$MO_ID))

# Categorize the brands into tiers
patient_adopt_last <- patient_adopt_last %>% 
  dplyr::mutate(
    source = ifelse(TIVICAY>0 & TRUVADA>0,"TIVICAY+TRUVADA",
                    ifelse(ISENTRESS>0 & TRUVADA>0,"ISENTRESS+TRUVADA",
                           ifelse(PREZCOBIX+NORVIR+PREZISTA+REYATAZ+EVOTAZ+KALETRA+LEXIVA>0 & TRUVADA>0,"PI+TRUVADA",
                                  ifelse(INTELENCE+NEVIRAPINE+SUSTIVA+NEVIRAPINE_ER+EDURANT>0 & TRUVADA>0,"NNRTI+TRUVADA",
                                         ifelse(EPZICOM>0 & TIVICAY>0,"TIVICAY+EPZICOM",
                                                ifelse(TRIUMEQ>0,"TRIUMEQ",ifelse(
                                                  ATRIPLA+GENVOYA+STRIBILD+COMPLERA+ODEFSEY>0,"OTHER STR","OTHER"
                                                ))))))))
table(patient_adopt_last$source)

switch_source <- patient_adopt_last %>% group_by(source) %>%
  dplyr::summarize(
    count = n()
  )

fwrite(switch_source,"../../Summary/tivicay_descovy_switch_source.csv",row.names = T)


# 0622 update
patient_adopt_last_bin <- patient_adopt_last[,6:56]
patient_adopt_last_bin[patient_adopt_last_bin>1] <- 1
patient_adopt_last_bin <- data.frame(patient_adopt_last[,1:5],patient_adopt_last_bin)
summary(patient_adopt_last_bin)

patient_adopt_last_bin <- patient_adopt_last_bin %>% 
  dplyr::mutate(
    source = ifelse(TIVICAY>0 & TRUVADA>0,"TIVICAY+TRUVADA",
                           ifelse(PREZCOBIX+NORVIR+PREZISTA+REYATAZ+EVOTAZ+KALETRA+LEXIVA>0 & TRUVADA>0,"PI+TRUVADA",
                                  ifelse(TRUVADA>0,"TRUVADA+OTHER",
                                         ifelse(EPZICOM>0 & TIVICAY>0,"TIVICAY+EPZICOM",
                                                ifelse(TIVICAY>0,"TIVICAY+OTHER",
                                                      ifelse(TRIUMEQ>0,"TRIUMEQ",
                                                            ifelse(ATRIPLA>0,"ATRIPLA",
                                                                    ifelse(GENVOYA+STRIBILD+COMPLERA+ODEFSEY>0,"GILEAD STR",
                                                                          ifelse(NORVIR>0,"NORVIR","OTHER"
                                                ))))))))))
table(patient_adopt_last_bin$source)
prop.table(table(patient_adopt_last_bin$source))

OTHER <- patient_adopt_last_bin[patient_adopt_last_bin$source %in% c("OTHER"),]
summary(OTHER)
OTHER_NORVIR <- OTHER[OTHER$NORVIR==1,]
summary(OTHER_NORVIR)
OTHER_ISENTRESS <- OTHER[OTHER$ISENTRESS==1,]
summary(OTHER_ISENTRESS)


switch_source <- patient_adopt_last_bin %>% group_by(source) %>%
  dplyr::summarize(
    count = n()
  )

fwrite(switch_source,"../../Summary/tivicay_descovy_switch_source_0622.csv",row.names = T)

# Reshape from wide to long
# library(reshape2)
# patient_adopt_last_reshape <- melt(patient_adopt_last[,!(names(patient_adopt_last) %in% 
#                                                              c("month_since_launch_rank_last","month_since_launch",
#                                                                "month_since_launch_rank"))], 
#                                      id.vars = 1:2)
# names(patient_adopt_last_reshape) <- c("PATIENT_ID","MO_ID","Brand","Count")
# patient_regimen <- patient_adopt_last_reshape[patient_adopt_last_reshape$Count>0 
#                                              & !(is.na(patient_adopt_last_reshape$Count))
#                                              & !(patient_adopt_last_reshape$Brand %in% c("TIVICAY","DESCOVY")),]
# fwrite(patient_regimen,"patient_regimen.csv",row.names = F)

