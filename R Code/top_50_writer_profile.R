rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)

NBRx_top_50 <- fread("NBRx_top_50.csv")
NBRx_top_50$Region <- substr(NBRx_top_50$Terr_ID,1,4)
n_distinct(NBRx_top_50$Region)

region_map <- fread("region_map.csv")
NBRx_top_50 <- merge(NBRx_top_50,region_map,by="Region",all.x=T)


NBRx_top_50 <- NBRx_top_50[NBRx_top_50$Des_Flag=="High" & NBRx_top_50$ECL_IND==1,
                             c("CID","Region","Region_nm","DESCOVY_NBRx_recent",
                               "TIVICAY_NBRx_recent","TRIUMEQ_NBRx_recent","Tiv_Des_Pts","Tiv_Des_NBRx"
                               )] %>% 
  arrange(-Tiv_Des_NBRx) %>% 
  filter(Region_nm!="") %>%
  dplyr::mutate(
    rank = rank(-Tiv_Des_NBRx)) %>% filter(rank<=50)

H_writer <- NBRx_top_50 %>% group_by(Region_nm) %>%
  dplyr::summarize(
    count = n(),
    Descovy_NBRx_recent = mean(DESCOVY_NBRx_recent,na.rm=T),
    TIVICAY_NBRx_recent = mean(TIVICAY_NBRx_recent,na.rm=T),
    TRIUMEQ_NBRx_recent = mean(TRIUMEQ_NBRx_recent,na.rm=T),
    Tiv_Des_Pts = mean(Tiv_Des_Pts,na.rm=T)
  ) %>% arrange(-count)

fwrite(H_writer,"High_writer_top_50_0620_v2.csv",row.names = F)

summary(H_writer)

