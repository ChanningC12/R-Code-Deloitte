rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)

# Read in core_details dataset for TRIUMEQ, GENVOYA and DISCOVY
core <- fread("CORE_DETAILS_B_D_FIRST_SEG_0623.CSV")
colSums(is.na(core))
str(core)

# Read in unique region match-up dataset (a subset from allvars)
region <- fread("allvars_region.csv")
str(region)
# Read in region name mapping
region_map <- fread("region_map.csv")
# merge region name with region
region <- merge(region,region_map,by="Region",all.x=T)
table(region$Region_nm)
colSums(is.na(region))
View(region[is.na(region$Region_nm),])

# merge region name to core details
core <- merge(core,region,by="CID",all.x=T)
table(core$Region_nm)
colSums(is.na(core))

# convert week to date format
core$week <- as.Date(core$END_DATE,"%d%b%Y")
str(core)


# indicator of ECL/HVMW first adpoter
core$ECL_IND_FIRST <- with(core,ifelse(CID_FIRST_ADOPT==1 & ECL_IND==1,1,0))
core$HVMW_IND_FIRST <- with(core,ifelse(CID_FIRST_ADOPT==1 & HVMW_IND==1,1,0))

# ECL/HVMW TRx
core$NBRX_ECL <- with(core,ifelse(ECL_IND==1,NBRX,0))
core$NBRX_HVMW <- with(core,ifelse(HVMW_IND==1,NBRX,0))

core$TOTAL_RX_ECL <- with(core,ifelse(ECL_IND==1,TOTAL_RX,0))
core$TOTAL_RX_HVMW <- with(core,ifelse(HVMW_IND==1,TOTAL_RX,0))

# calculate week/brand level total Rx, new adopters
core_week <- core %>% group_by_(.dots = c("week","BRAND","Region_nm")) %>%
  dplyr::summarize(
    TOTAL_RX = sum(TOTAL_RX),
    TOTAL_RX_ECL = sum(TOTAL_RX_ECL),
    TOTAL_RX_HVMW = sum(TOTAL_RX_HVMW),
    NBRX = sum(NBRX),
    NBRX_ECL = sum(NBRX_ECL),
    NBRX_HVMW = sum(NBRX_HVMW),
    NEW_ADOPT_COUNT = sum(CID_FIRST_ADOPT),
    NEW_ADOPT_COUNT_ECL = sum(ECL_IND_FIRST),
    NEW_ADOPT_COUNT_HVMW = sum(HVMW_IND_FIRST)
  )

# calculate cumulative Rx and cumulative adopters
core_week <- core_week %>% arrange(week, BRAND) %>%
  group_by_(.dots = c("BRAND","Region_nm")) %>%
  dplyr::mutate(
    CUM_TOTAL_RX = cumsum(TOTAL_RX),
    CUM_TOTAL_RX_ECL = cumsum(TOTAL_RX_ECL),
    CUM_TOTAL_RX_HVMW = cumsum(TOTAL_RX_HVMW),
    CUM_NBRX = cumsum(NBRX),
    CUM_NBRX_ECL = cumsum(NBRX_ECL),
    CUM_NBRX_HVMW = cumsum(NBRX_HVMW),
    CUM_ADOPT_COUNT = cumsum(NEW_ADOPT_COUNT),
    CUM_ADOPT_COUNT_ECL = cumsum(NEW_ADOPT_COUNT_ECL),
    CUM_ADOPT_COUNT_HVMW = cumsum(NEW_ADOPT_COUNT_HVMW)
    )

# read in week+brand level summary to merge in week_since_launch
week_since_launch <- fread("breadth_depth_0623.csv")
core_week <- merge(core_week,week_since_launch[,c("week","BRAND","week_since_launch")],
                   by=c("week","BRAND"),all.x = T)

core_week <- core_week[,c("week","BRAND","Region_nm","week_since_launch",
                          "TOTAL_RX","CUM_TOTAL_RX",
                          "TOTAL_RX_ECL","CUM_TOTAL_RX_ECL",
                          "TOTAL_RX_HVMW","CUM_TOTAL_RX_HVMW",
                          "NBRX","CUM_NBRX","NBRX_ECL","CUM_NBRX_ECL",
                          "NBRX_HVMW","CUM_NBRX_HVMW",
                          "NEW_ADOPT_COUNT","CUM_ADOPT_COUNT",
                          "NEW_ADOPT_COUNT_ECL","CUM_ADOPT_COUNT_ECL",
                          "NEW_ADOPT_COUNT_HVMW","CUM_ADOPT_COUNT_HVMW")]

# Add count of CIDs by region
terr <- fread("../S2_ECL_Terr.csv")
terr$region <- substr(terr$Terr_ID,1,4)
region_ECL <- terr[,c("CID","region")]
region_ECL <- merge(region_ECL,region_map,by.x="region",by.y="Region",all.x = T)

region_ECL <- distinct(region_ECL)

region_ECL_count <- region_ECL %>% group_by(Region_nm) %>%
  dplyr::summarize(
    HCP_count = n()
  )

core_week <- merge(core_week,region_ECL_count,by="Region_nm",all.x=T)
core_week <- core_week %>% arrange(week,BRAND)
core_week$adopt_perc_ECL <- core_week$CUM_ADOPT_COUNT_ECL / core_week$HCP_count
core_week$adopt_TRx_ECL <- core_week$CUM_TOTAL_RX_ECL / core_week$CUM_ADOPT_COUNT_ECL
core_week$adopt_NBRx_ECL <- core_week$CUM_NBRX_ECL / core_week$CUM_ADOPT_COUNT_ECL
summary(core_week$adopt_perc_ECL)

fwrite(core_week,"breadth_depth_0623_region.csv",row.names = F)

