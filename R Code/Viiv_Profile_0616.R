rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)

allvars <- fread("allvars_0619.csv")
colSums(is.na(allvars))
str(allvars)

# Fill NAs with 0: 
allvars[is.na(allvars)] <- 0

allvars <- as.data.frame.matrix(allvars)

############ Who is a Tivicay + Descovy adopter? What is the profile of a high writer? ###############
allvars$Tiv_Des_NBRx_ind <- with(allvars,ifelse(Tiv_Des_NBRx>0,1,0))
table(allvars$Tiv_Des_NBRx_ind) # 3,292 out of 7,615 CIDs
table(allvars$Tiv_Des_NBRx_ind,allvars$Des_Flag)


writer_summary <- allvars %>% group_by(Tiv_Des_NBRx_ind) %>%
  dplyr::summarize(
    count = n(),
    HVMW_IND = mean(HVMW_IND,na.rm=T),
    Mkt_NBRx = mean(Mkt_NBRx,na.rm=T),
    TIVICAY_NBRx_pre = mean(TIVICAY_NBRx_pre,na.rm=T),
    TRIUMEQ_NBRx_pre = mean(TRIUMEQ_NBRx_pre,na.rm=T),
    GENVOYA_NBRx_pre = mean(GENVOYA_NBRx_pre,na.rm=T),
    TRIUMEQ_NBRX_growth = mean(TRIUMEQ_NBRX_growth,na.rm=T),
    TRUVADA_SWITCH_NBRX_growth = mean(TRUVADA_SWITCH_NBRX_growth,na.rm=T),
    triumeq_to_odefsey_rx = mean(triumeq_to_odefsey_rx,na.rm=T),
    TRUVADA_NBRx_loyalty = mean(TRUVADA_NBRx_loyalty,na.rm=T),
    Perc_Tiv_Tru_Pre = mean(Perc_Tiv_Tru_Pre,na.rm=T),
    total_details = mean(total_details,na.rm=T),
    total_reps = mean(total_reps,na.rm=T)
  )

fwrite(writer_summary,"../../Summary/writer_summary_0619.csv",row.names = F)


writer_summary_HVMW <- allvars %>% group_by(Des_Flag) %>%
  dplyr::summarize(
    count = n(),
    ECL_count = sum(ECL_IND,na.rm=T),
    HVMW_count = sum(HVMW_IND,na.rm=T),
    HVMW_IND = mean(HVMW_IND,na.rm=T),
    Mkt_NBRx = mean(Mkt_NBRx,na.rm=T),
    TIVICAY_NBRx_pre = mean(TIVICAY_NBRx_pre,na.rm=T),
    TRIUMEQ_NBRx_pre = mean(TRIUMEQ_NBRx_pre,na.rm=T),
    GENVOYA_NBRx_pre = mean(GENVOYA_NBRx_pre,na.rm=T),
    TRIUMEQ_NBRX_growth = mean(TRIUMEQ_NBRX_growth,na.rm=T),
    TRUVADA_SWITCH_NBRX_growth = mean(TRUVADA_SWITCH_NBRX_growth,na.rm=T),
    triumeq_to_odefsey_rx = mean(triumeq_to_odefsey_rx,na.rm=T),
    TRUVADA_NBRx_loyalty = mean(TRUVADA_NBRx_loyalty,na.rm=T),
    Perc_Tiv_Tru_Pre = mean(Perc_Tiv_Tru_Pre,na.rm=T),
    total_details = mean(total_details,na.rm=T),
    total_reps = mean(total_reps,na.rm=T)
  )

fwrite(writer_summary_HVMW,"../../Summary/writer_summary_HVMW_0619.csv",row.names = F)


###################### Writers Profile ########################
writer_summary_DesFlag <- aggregate(allvars, by=list(allvars$Des_Flag), FUN=mean, na.rm=TRUE)
writer_summary_nonWriter <- aggregate(allvars, by=list(allvars$Tiv_Des_NBRx_ind), FUN=mean, na.rm=TRUE)
writer_summary_all <- rbind(writer_summary_DesFlag, writer_summary_nonWriter)

fwrite(writer_summary_all,"../../Summary/writer_summary_all_0619.csv",row.names = F)

