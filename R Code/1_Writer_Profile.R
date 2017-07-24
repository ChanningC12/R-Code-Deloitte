rm(list=ls())
gc()

setwd("~/../Desktop/ViiV/Data and Data Dictionary/Raw/")

library(data.table)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)

allvars <- fread("allvars.csv")
colSums(is.na(allvars))
str(allvars)

# Fill NAs with 0: 
allvars[is.na(allvars)] <- 0

################## 1a. How many total vs. "meaningful" adopters compared to other brands at similar point in launch ###############
##### 1) Count of HCPs, Average Writing Volume, % adopted Triumeq/Genvoya
# Count of total vs meaningful adopters
allvars$Tiv_Des_NBRx_ind <- with(allvars,ifelse(Tiv_Des_NBRx>0,1,0))
table(allvars$Tiv_Des_NBRx_ind) # 4,753 out of 139,280 CIDs
table(allvars$Des_Flag) # 253 High, 353 Medium, 2703 Low
table(allvars$ECL_IND) # 4,503 ECLs
table(allvars$ECL_Status) # 816 HVMW, 764 Rep Added, 964 No See, 1959 Long term wins

# Average writing volume
allvars %>% group_by_(.dots = c("Tiv_Des_NBRx_ind","Des_Flag","ECL_Status")) %>%
  dplyr::summarize(
    count = n(),
    Tiv_Des_NBRx = mean(Tiv_Des_NBRx,na.rm=T)
  )

# Trimuqe / Genvoya adopter
table(allvars$Genvoya_Adopter) # 1,823 Genvoya early adopter (3 months post brand launch)
table(allvars$Triumeq_Adopter) # 1,619 Genvoya early adopter (3 months post brand launch)

# % Adopted Triumeq/Genvoya
allvars %>% group_by_(.dots = c("Tiv_Des_NBRx_ind","Des_Flag","ECL_Status")) %>%
  dplyr::summarize(
    count = n(),
    Genvoya_Adopter_perc = mean(Genvoya_Adopter, na.rm=T),
    Triumeq_Adopter_perc = mean(Triumeq_Adopter, na.rm=T)
)


################## 1b. What is the intensity / depth per HCP ##################
# Population: ECL Only

# 1) # of adopters by week
# Need CID+Week level adopter indicator, ask for definition and how to roll up to week level

# 2) average volume per writer by week
# Roll up CID+week level to week level and calculate the average NBRx by CID on week level

# 3) add as data label avg Triumeq NBRx
# Same above, this is to detect the impact on Triumeq NBRx

# 4) Cut by National and Regional
table(allvars$Region,allvars$ECL_Status)



################## 1c. What is the "profile" of high Descovy adopter compare to the non-adopter? ##################
# Are they "Gilead Loyalist"? ABC "hater"?

# What metrics to use for "profile"?




################## 1d. Where are the meaningful adopters located geographically? Test & Treat area ##################
# Population: ECL only
allvars_ECL <- allvars %>% filter(ECL_IND==1)

geo_summary <- allvars_ECL %>%
  group_by_(.dots = c("State","Des_Flag")) %>%
  dplyr::summarize(
    count = n()
  )

geo_summary <- geo_summary %>% group_by(State) %>%
  dplyr::mutate(
    count_state = sum(count)
  )
geo_summary$count_perc <- geo_summary$count/geo_summary$count_state

# Test and Treat indicator
geo_summary_2 <- allvars_ECL %>%
  group_by_(.dots = c("test_treat_ind","Des_Flag")) %>%
  dplyr::summarize(
    count = n()
  )

geo_summary_2 <- geo_summary_2 %>% group_by(test_treat_ind) %>%
  dplyr::mutate(
    count_test_treat = sum(count)
  )

geo_summary_2$count_perc <- geo_summary_2$count/geo_summary_2$count_test_treat


################## 1e. Who are the top 50-100 writers LTD (What is their Triumeq use) ##################
# Population: ECL only
allvars_ECL$Tiv_Des_NBRx_rank = rank(-allvars_ECL$Tiv_Des_NBRx,ties.method = "min")
View(allvars_ECL[,c("CID","Tiv_Des_NBRx","Tiv_Des_NBRx_rank")])


################## 1f. Are there any high volume HCPs showing a recent shift in usage ##################




