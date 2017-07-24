rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)

zip <- fread("../Zip2Terr.csv")
zip$Region <- substr(zip$Terr_ID,1,4)
region_map <- fread("region_map.csv")
region <- merge(zip,region_map,by="Region",all.x=T)

zip_avg <- region %>% group_by(Region) %>%
  dplyr::summarize(
    zip_avg = ceiling(median(Zip_Code,na.rm=T))
  )

allvars_map <- fread("allvars_0619_map.csv")
allvars_map <- merge(allvars_map,zip_avg,by="Region",all.x=T)
fwrite(allvars_map,"allvars_0620_map.csv")

View(zip[zip$Zip_Code>=33200 & zip$Zip_Code<=33300,]) # 33246 - 33247
View(zip[zip$Zip_Code>=77900 & zip$Zip_Code<=78000,]) # 77928 - 77901
# 96818 - 94002

fwrite(zip_avg,"zip_region_match.csv",row.names = F)

# QC high writers
allvars_map_H <- allvars_map[allvars_map$Des_Flag=="High",]
table(allvars_map_H$Region_nm)
View(allvars_map_H[allvars_map_H$Region_nm=="NW, NO. CAL",])









