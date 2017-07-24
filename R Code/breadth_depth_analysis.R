rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)

core <- fread("CORE_DETAILS_B_D_FIRST_SEG_0623.CSV")
colSums(is.na(core))
str(core)

# adjust the week format
core$week <- as.Date(core$END_DATE,"%d%b%Y")
str(core)

core <- core %>% arrange(CID,BRAND,week)

# indicator of ECL/HVMW first adpoter
core$ECL_IND_FIRST <- with(core,ifelse(CID_FIRST_ADOPT==1 & ECL_IND==1,1,0))
core$HVMW_IND_FIRST <- with(core,ifelse(CID_FIRST_ADOPT==1 & HVMW_IND==1,1,0))

# ECL/HVMW TRx
core$NBRX_ECL <- with(core,ifelse(ECL_IND==1,NBRX,0))
core$NBRX_HVMW <- with(core,ifelse(HVMW_IND==1,NBRX,0))

core$TOTAL_RX_ECL <- with(core,ifelse(ECL_IND==1,TOTAL_RX,0))
core$TOTAL_RX_HVMW <- with(core,ifelse(HVMW_IND==1,TOTAL_RX,0))

# calculate week/brand level total Rx, new adopters
core_week <- core %>% group_by_(.dots = c("week","BRAND")) %>%
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
  group_by_(.dots = c("BRAND")) %>%
  dplyr::mutate(
    CUM_TOTAL_RX = cumsum(TOTAL_RX),
    CUM_TOTAL_RX_ECL = cumsum(TOTAL_RX_ECL),
    CUM_TOTAL_RX_HVMW = cumsum(TOTAL_RX_HVMW),
    CUM_NBRX = cumsum(NBRX),
    CUM_NBRX_ECL = cumsum(NBRX_ECL),
    CUM_NBRX_HVMW = cumsum(NBRX_HVMW),
    CUM_ADOPT_COUNT = cumsum(NEW_ADOPT_COUNT),
    CUM_ADOPT_COUNT_ECL = cumsum(NEW_ADOPT_COUNT_ECL),
    CUM_ADOPT_COUNT_HVMW = cumsum(NEW_ADOPT_COUNT_HVMW),
    week_since_launch = row_number()
  )

core_week <- core_week[,c("week","BRAND","week_since_launch",
                          "TOTAL_RX","CUM_TOTAL_RX",
                          "TOTAL_RX_ECL","CUM_TOTAL_RX_ECL",
                          "TOTAL_RX_HVMW","CUM_TOTAL_RX_HVMW",
                          "NBRX","CUM_NBRX","NBRX_ECL","CUM_NBRX_ECL",
                          "NBRX_HVMW","CUM_NBRX_HVMW",
                          "NEW_ADOPT_COUNT","CUM_ADOPT_COUNT",
                          "NEW_ADOPT_COUNT_ECL","CUM_ADOPT_COUNT_ECL",
                          "NEW_ADOPT_COUNT_HVMW","CUM_ADOPT_COUNT_HVMW")]
fwrite(core_week,"breadth_depth_0623.csv",row.names = F)








