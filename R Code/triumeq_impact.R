rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)

triumeq <- fread("triumeq_impact.csv")
colSums(is.na(triumeq))
str(triumeq)

triumeq_sub <- triumeq[triumeq$Des_Flag %in% c("High","Med","Low"),]
triumeq_sub$week <- as.Date(triumeq_sub$END_DATE,"%d%b%Y")

triumeq_impact <- triumeq_sub %>% group_by_(.dots = c("week","Des_Flag")) %>%
  dplyr::summarize(
    TRIUMEQ_NBRx = sum(TRIUMEQ_NBRx,na.rm=T),
    count = n(),
    TRIUMEQ_NBRx_avg = sum(TRIUMEQ_NBRx,na.rm=T) / n()
  )

fwrite(triumeq_impact,"triumeq_impact_Tableau.csv",row.names = F)


library(reshape)
triumeq_impact_reshape <- cast(triumeq_impact[,c("week","Des_Flag","TRIUMEQ_NBRx_avg")],week~Des_Flag)
fwrite(triumeq_impact_reshape,"triumeq_impact_Excel.csv",row.names = F)


# Triumeq to all, triumeq_to_all_rx, triumeq_to_descovy_rx
triumeq_impact_switch <- triumeq_sub %>% group_by_(.dots = c("week","Des_Flag")) %>%
  dplyr::summarize(
    triumeq_to_all_rx = sum(triumeq_to_all_rx,na.rm=T),
    count = n(),
    triumeq_to_all_rx_avg = sum(triumeq_to_all_rx,na.rm=T) / n()
  )

triumeq_impact_switch_reshape <- cast(triumeq_impact_switch[,c("week","Des_Flag","triumeq_to_all_rx_avg")],week~Des_Flag)
triumeq_impact_switch_total <- cast(triumeq_impact_switch[,c("week","Des_Flag","triumeq_to_all_rx")],week~Des_Flag)

fwrite(triumeq_impact_switch_reshape,"triumeq_impact_switch_Excel.csv",row.names = F)
fwrite(triumeq_impact_switch_total,"triumeq_impact_switch_total.csv",row.names = F)




# Aggregate for all
triumeq_impact <- triumeq_sub %>% group_by_(.dots = c("week","Des_Flag")) %>%
  dplyr::summarize(
    count = n(),
    TRIUMEQ_NBRx = sum(TRIUMEQ_NBRx,na.rm=T),
    TRIUMEQ_NBRx_avg = sum(TRIUMEQ_NBRx,na.rm=T) / n(),
    triumeq_to_all_rx = sum(triumeq_to_all_rx,na.rm=T),
    triumeq_to_all_rx_avg = sum(triumeq_to_all_rx,na.rm=T) / n(),
    triumeq_to_descovy_rx = sum(triumeq_to_descovy_rx,na.rm=T),
    triumeq_to_descovy_rx_avg = sum(triumeq_to_descovy_rx,na.rm=T) / n()
  )
fwrite(triumeq_impact,"triumeq_impact_all.csv",row.names = F)












