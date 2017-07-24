rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)

patient_des <- read.csv("patient_des.csv")
patient <- fread("patient.csv")
str(patient_des)
patient_des$FIRST_RX_IN_MONTH_NEW <- as.Date(as.character(patient_des$FIRST_RX_IN_MONTH,"%Y/%b/%d"))
patient_des$LAST_RX_IN_MONTH_NEW <- as.Date(as.character(patient_des$LAST_RX_IN_MONTH,"%Y/%b/%d"))

# read in Tiv+Des regimen dataset to indicate Tiv+Des adopters
patient_tiv_des <- read.csv("patient_rx_tiv_des.csv")
patient_tiv_des <- unique(patient_tiv_des$PATIENT_ID)
patient_tiv_des <- data.frame(PATIENT_ID = patient_tiv_des, tiv_des_ind = 1)

############################# Age and Gender Distribution ##################################
# calculate patient age
str(patient)
n_distinct(patient$PATIENT_ID) # 872,218 unique patients in patient dataset
patient$age <- year(Sys.Date()) - patient$PTNT_YOB
summary(patient$age)
# patient <- patient[patient$age>=0,]
ggplot(patient,aes(age)) + geom_histogram()

# merge the age to the patient_desvocy dataset
n_distinct(patient_des$PATIENT_ID) # 45,280 unique patients in patient_descovy dataset
table(unique(patient_des$PATIENT_ID) %in% unique(patient$PATIENT_ID))

des_pt_id <- unique(patient_des$PATIENT_ID)
des_pt_id <- data.frame(PATIENT_ID = des_pt_id)
des_pt_id <- merge(des_pt_id,patient,by="PATIENT_ID",all.x=T)

des_pt_id <- merge(des_pt_id,patient_tiv_des,by="PATIENT_ID",all.x = T)
table(des_pt_id$tiv_des_ind)

# age distribution summary
des_pt_age <- des_pt_id %>% group_by(age) %>%
  dplyr::summarize(count = n())
# gender distribution summary
des_pt_gender <- des_pt_id %>% group_by(PTNT_GENDER) %>%
  dplyr::summarize(count = n())
fwrite(des_pt_age,"../../Summary/patient_des_age.csv",row.names = F)
fwrite(des_pt_gender,"../../Summary/patient_des_gender.csv",row.names = F)

# Age and gender distribution for TIV+DES patients
# age distribution summary
des_pt_age_tiv <- des_pt_id[des_pt_id$tiv_des_ind==1,] %>% group_by(age) %>%
  dplyr::summarize(count = n())
# gender distribution summary
des_pt_gender_tiv <- des_pt_id[des_pt_id$tiv_des_ind==1,] %>% group_by(PTNT_GENDER) %>%
  dplyr::summarize(count = n())

fwrite(des_pt_age_tiv,"../../Summary/patient_des_age_tiv.csv",row.names = F)
fwrite(des_pt_gender_tiv,"../../Summary/patient_des_gender_tiv.csv",row.names = F)

# for QC
View(patient_des[patient_des$FIRST_RX_IN_MONTH!=patient_des$LAST_RX_IN_MONTH,])
patient_des$MO_ID_new <- as.numeric(paste(substr(as.character(patient_des$FIRST_RX_IN_MONTH),1,4),
                                          substr(as.character(patient_des$FIRST_RX_IN_MONTH),6,7),sep = ""))
View(patient_des[patient_des$MO_ID!=patient_des$MO_ID_new,])
table(patient_des$MO_ID-patient_des$MO_ID_new)


############################ State Distribution ###############################
allvars_state <- fread("allvars_CID_state.csv")
table(allvars_state$State)
n_distinct(patient_des$PRACTITIONER_KEY) # 6,190 unique CIDs in patient descovy dataset
table(unique(patient_des$PRACTITIONER_KEY) %in% unique(allvars_state$CID)) # 6,004 CIDs are in allvars

patient_des <- merge(patient_des,patient_tiv_des,by="PATIENT_ID",all.x = T)
table(patient_des$tiv_des_ind)
patient_des <- patient_des[!is.na(patient_des$tiv_des_ind) & patient_des$tiv_des_ind==1,]
patient_des <- merge(patient_des,allvars_state,by.x="PRACTITIONER_KEY",by.y="CID",all.x=T)

patient_state <- patient_des[,c("PATIENT_ID","State","FIRST_RX_IN_MONTH_NEW")]
patient_state <- patient_state %>% filter(State!="",!(is.na(State))) %>% group_by(PATIENT_ID) %>%
  dplyr::mutate(rank = rank(FIRST_RX_IN_MONTH_NEW,ties.method = "min")) %>% group_by(PATIENT_ID) %>%
  dplyr::mutate(rank_rev = rank(-rank,ties.method = "min")) %>% filter(rank_rev==1)

patient_state_unique <- as.data.frame(unique(patient_state[,c("PATIENT_ID","State","FIRST_RX_IN_MONTH_NEW")]))

patient_state_unique <- patient_state_unique %>% group_by_(.dots = c("PATIENT_ID")) %>%
  dplyr::mutate(count = n())
n_distinct(patient_state_unique$PATIENT_ID)
View(patient_state_unique[patient_state_unique$count>1,])

patient_state_unique <- patient_state_unique %>% group_by_(.dots = c("PATIENT_ID","FIRST_RX_IN_MONTH_NEW")) %>%
  dplyr::summarize(
    State = max(State)
  )

fwrite(patient_state_unique,"../../Summary/patient_des_state_0621.csv",row.names = F)

######################### Distribution of patient adoption since Descovy launch ###########################
des_launch <- as.data.frame(unique(patient_des$MO_ID))
names(des_launch) <- "MO_ID"
des_launch <- des_launch %>% 
  dplyr::mutate(month_since_launch = rank(MO_ID)-1)

patient_adopt <- patient_des[,c("PATIENT_ID","MO_ID")]
patient_adopt <- patient_adopt %>% group_by(PATIENT_ID) %>%
  dplyr::summarize(
    adopt_first = min(MO_ID)
  )

patient_adopt <- merge(patient_adopt,des_launch,by.x="adopt_first",by.y="MO_ID",all.x=T)
table(patient_adopt$month_since_launch)

des_pt_adopt <- patient_adopt %>% group_by(month_since_launch) %>%
  dplyr::summarize(count = n())
fwrite(des_pt_adopt,"../../Summary/patient_des_adopt.csv",row.names = F)
fwrite(patient_adopt,"../../Summary/patient_des_adopt_full.csv",row.names = F)













