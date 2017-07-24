dim(patient_adopt_last_bin)
names(patient_adopt_last_bin) # DESCOVY 17
rowsum <- rowSums(patient_adopt_last_bin[,6:56])
sum(is.na(rowsum))

patient_adopt_last_bin <- data.frame(patient_adopt_last_bin, all=rowsum)
summary(patient_adopt_last_bin$all)

patient_adopt_last_bin <- as.data.table(patient_adopt_last_bin)
des_only <- patient_adopt_last_bin[patient_adopt_last_bin$all==1 & patient_adopt_last_bin$DESCOVY==1,] # 737 patients

patient_des <- read.csv("patient_rx_tiv_des.csv")
patient_des$X_NAME_ <- NULL
patient_des[is.na(patient_des)] <- 0

des_only <- merge(des_only[,"PATIENT_ID"],patient_des,by="PATIENT_ID",all.x=T)
des_only <- des_only %>% arrange(PATIENT_ID,MO_ID)


des_only <- des_only %>% group_by(PATIENT_ID) %>%
  dplyr::mutate(
    MO_ID_rank = rank(MO_ID,ties.method = "min")
  )

des_only_first <- des_only %>% 
  dplyr::mutate(
    tiv_des_ind = ifelse(DESCOVY>0 & TIVICAY>0,1,0)
  ) %>%
  filter(tiv_des_ind==1) %>%
  group_by(PATIENT_ID) %>%
  dplyr::summarize(
    MO_ID_rank = min(MO_ID_rank)
  )

des_only_first$MO_ID_rank_new <- des_only_first$MO_ID_rank-2

des_only_prior_two <- merge(des_only_first[,c("PATIENT_ID","MO_ID_rank_new")],des_only,
                            by.x=c("PATIENT_ID","MO_ID_rank_new"),
                            by.y=c("PATIENT_ID","MO_ID_rank"),
                            all.x=T)
summary(des_only_prior_two) # 99 are missing

patient_id <- des_only_prior_two[is.na(des_only_prior_two$RETROVIR),]$PATIENT_ID
patient_portfolio <- patient_des[patient_des$PATIENT_ID %in% patient_id,]
n_distinct(patient_portfolio$PATIENT_ID)







